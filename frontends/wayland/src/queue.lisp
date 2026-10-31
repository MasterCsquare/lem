(in-package #:lem-wayland)

;;;; Calls from Lem's editor thread into the wlroots event-loop thread.
;;;
;;;; wlroots' scene graph is only touched from the thread that runs the
;;;; Wayland event loop.  Lem calls frontend methods from its editor thread,
;;;; so every scene update is marshalled through a TASK-QUEUE: the editor
;;;; thread enqueues a thunk, wakes the event loop through an eventfd and
;;;; waits for the result.

(defconstant +wl-event-readable+ #x01)
(defconstant +efd-cloexec+ #x80000)
(defconstant +efd-nonblock+ #x800)
(defconstant +eagain+ 11)

(defvar *task-queues* (make-hash-table :test #'eql))

(defstruct pending-task
  function
  (done-p nil)
  values
  error)

(defclass task-queue ()
  ((lock :initform (bt2:make-lock :name "lem-wayland task queue")
         :reader task-queue-lock)
   (condition :initform (bt2:make-condition-variable)
              :reader task-queue-condition)
   (pending :initform '() :accessor task-queue-pending)
   (event-fd :initform -1 :accessor task-queue-event-fd)
   (event-source :initform nil :accessor task-queue-event-source)
   (thread :initform nil :accessor task-queue-thread)
   (closed-p :initform nil :accessor task-queue-closed-p)))

(cffi:defcallback %task-queue-ready :int
    ((fd :int) (mask :uint32) (data :pointer))
  (declare (ignore mask data))
  (let ((queue (gethash fd *task-queues*)))
    (when queue
      (task-queue-drain queue))
    0))

(defun task-queue-drain (queue)
  (let ((buffer (cffi:foreign-alloc :uint64 :count 1)))
    (unwind-protect
         (loop :while (plusp (%unix-read (task-queue-event-fd queue)
                                         buffer
                                         8)))
      (cffi:foreign-free buffer)))
  (let ((tasks (bt2:with-lock-held ((task-queue-lock queue))
                 (prog1 (nreverse (task-queue-pending queue))
                   (setf (task-queue-pending queue) '())))))
    (dolist (task tasks)
      (handler-case
          (let ((values (multiple-value-list (funcall (pending-task-function task)))))
            (bt2:with-lock-held ((task-queue-lock queue))
              (setf (pending-task-values task) values
                    (pending-task-done-p task) t))
            (bt2:condition-broadcast (task-queue-condition queue)))
        (error (condition)
          (bt2:with-lock-held ((task-queue-lock queue))
            (setf (pending-task-error task) condition
                  (pending-task-done-p task) t))
          (bt2:condition-broadcast (task-queue-condition queue)))))))

(defun make-task-queue (event-loop)
  (let* ((queue (make-instance 'task-queue))
         (fd (%eventfd 0 (logior +efd-cloexec+ +efd-nonblock+))))
    (when (minusp fd)
      (error "Could not create an eventfd for the compositor queue"))
    (setf (task-queue-event-fd queue) fd
          (gethash fd *task-queues*) queue)
    (setf (task-queue-event-source queue)
          (%wl-event-loop-add-fd event-loop
                                 fd
                                 +wl-event-readable+
                                 (cffi:callback %task-queue-ready)
                                 (cffi:null-pointer)))
    (when (cffi:null-pointer-p (task-queue-event-source queue))
      (remhash fd *task-queues*)
      (error "Could not register the compositor task queue with wlroots"))
    queue))

(defun task-queue-wake (queue)
  (let ((buffer (cffi:foreign-alloc :uint64 :count 1)))
    (unwind-protect
         (progn
           (setf (cffi:mem-ref buffer :uint64) 1)
           (%unix-write (task-queue-event-fd queue) buffer 8))
      (cffi:foreign-free buffer))))

(defun call-on-task-queue (queue function)
  "Run FUNCTION on QUEUE's event-loop thread and return its values.
When called from the event-loop thread itself, call FUNCTION directly."
  (if (eq (bt2:current-thread) (task-queue-thread queue))
      (funcall function)
      (let ((task (make-pending-task :function function)))
        (bt2:with-lock-held ((task-queue-lock queue))
          (when (task-queue-closed-p queue)
            (error "The Wayland compositor is not running"))
          (push task (task-queue-pending queue)))
        (task-queue-wake queue)
        (bt2:with-lock-held ((task-queue-lock queue))
          (loop :until (pending-task-done-p task)
                :do (bt2:condition-wait (task-queue-condition queue)
                                        (task-queue-lock queue))))
        (if (pending-task-error task)
            (error (pending-task-error task))
            (values-list (pending-task-values task))))))

(defun close-task-queue (queue)
  (when queue
    (bt2:with-lock-held ((task-queue-lock queue))
      (setf (task-queue-closed-p queue) t)
      (dolist (task (prog1 (task-queue-pending queue)
                      (setf (task-queue-pending queue) '())))
        (setf (pending-task-error task)
              (make-condition 'simple-error
                              :format-control
                              "The Wayland compositor has stopped")
              (pending-task-done-p task) t)))
    (bt2:condition-broadcast (task-queue-condition queue))
    (let ((source (task-queue-event-source queue))
          (fd (task-queue-event-fd queue)))
      (when (and source (not (cffi:null-pointer-p source)))
        (%wl-event-source-remove source)
        (setf (task-queue-event-source queue) nil))
      (when (and fd (not (minusp fd)))
        (cffi:foreign-funcall "close" :int fd)))))
