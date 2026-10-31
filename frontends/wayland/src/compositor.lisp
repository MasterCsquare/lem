(in-package #:lem-wayland)

;;;; Constants from wayland-server-protocol / wlroots

(defconstant +wlr-input-device-keyboard+ 0)
(defconstant +wlr-input-device-pointer+ 1)

(defconstant +wl-seat-capability-pointer+ 1)
(defconstant +wl-seat-capability-keyboard+ 2)

(defconstant +wl-keyboard-key-state-released+ 0)
(defconstant +wl-keyboard-key-state-pressed+ 1)

(defconstant +wl-pointer-button-state-released+ 0)
(defconstant +wl-pointer-button-state-pressed+ 1)

;;; Linux evdev button codes delivered in wlr_pointer_button_event.button.

(defconstant +btn-left+ #x110)
(defconstant +btn-right+ #x111)
(defconstant +btn-middle+ #x112)

(defconstant +wl-pointer-axis-vertical-scroll+ 0)
(defconstant +wl-pointer-axis-horizontal-scroll+ 1)
(defconstant +wl-pointer-axis-source-wheel+ 0)

(defconstant +wl-pointer-axis-relative-direction-identical+ 0)

(defconstant +wlr-edge-top+ 1)
(defconstant +wlr-edge-bottom+ 2)
(defconstant +wlr-edge-left+ 4)
(defconstant +wlr-edge-right+ 8)
(defconstant +wlr-edge-all+ 15)

(defconstant +clock-monotonic+ 1)
(defconstant +wlr-log-info+ 2)

(defconstant +xkb-context-no-flags+ 0)
(defconstant +xkb-keymap-compile-no-flags+ 0)
(defconstant +xkb-state-mods-depressed+ 1)
(defconstant +xkb-state-mods-latched+ 2)
(defconstant +xkb-state-mods-locked+ 4)
(defconstant +xkb-state-mods-effective+ 8)
(defconstant +xkb-state-mods-all+ 7)

(defconstant +drm-format-argb8888+ #x34325241)
(defconstant +drm-format-mod-linear+ 0)
(defconstant +wlr-buffer-data-ptr-access-write+ 2)

(defconstant +default-output-width+ 1280)
(defconstant +default-output-height+ 720)

;;;; State

(defvar *compositor* nil)

;;; Callbacks installed by the Lem frontend.  They are invoked on the
;;; compositor thread and are responsible for marshalling to the editor
;;; thread with LEM:SEND-EVENT.

(defvar *toplevel-mapped-function* nil)
(defvar *toplevel-destroyed-function* nil)
(defvar *toplevel-title-changed-function* nil)
(defvar *focus-view-function* nil)
(defvar *focus-client-function* nil)
(defvar *output-changed-function* nil)

;;; Pointer events that belong to Lem rather than to a client surface.
;;; Coordinates are global layout pixels.

(defvar *pointer-position-function* nil)
(defvar *pointer-motion-function* nil)
(defvar *pointer-button-function* nil)
(defvar *pointer-axis-function* nil)

(defun notify-output-changed (compositor)
  (when *output-changed-function*
    (funcall *output-changed-function*
             (compositor-output-width compositor)
             (compositor-output-height compositor))))

(defstruct output-info
  output
  layout-output
  scene-output
  frame-listener
  destroy-listener
  (width 0 :type fixnum)
  (height 0 :type fixnum))

(defstruct scene-xdg-entry
  base
  tree
  new-popup-listener
  destroy-listener)

(defstruct lem-view-rect
  id
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (width 0 :type fixnum)
  (height 0 :type fixnum))

;;;; Scene representation of Lem's own windows.
;;;
;;;; Each view has a scene tree containing one scene buffer node.  The buffer
;;;; is a wlroots shared-memory buffer whose pixels are uploaded from the
;;;; Cairo image the frontend renders the window into.  Two buffers are kept
;;;; per view and alternated: wlr_scene_buffer ignores a set_buffer() call
;;;; with the same buffer twice, so alternating guarantees damage is emitted
;;;; even when consecutive frames have identical pixels.

(defstruct view-scene
  id
  tree
  image-node
  (buffers '())
  (current-buffer nil)
  (buffer-width 0 :type fixnum)
  (buffer-height 0 :type fixnum))

(defstruct view-frame
  id
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defstruct client-frame
  id
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (floating-p nil)
  (visible-p t))

(defstruct (wayland-toplevel (:constructor %make-wayland-toplevel))
  (id 0 :type fixnum)
  toplevel
  xdg-surface
  surface
  scene-node
  map-listener
  unmap-listener
  destroy-listener
  surface-destroy-listener
  commit-listener
  request-maximize-listener
  request-fullscreen-listener
  request-minimize-listener
  request-move-listener
  request-resize-listener
  request-show-window-menu-listener
  set-title-listener
  set-app-id-listener
  set-parent-listener
  (title "" :type string)
  (app-id "" :type string)
  (parent-id nil)
  (floating-p nil)
  (configured-p nil)
  (mapped-p nil)
  (registered-p nil)
  (visible-p nil)
  (activated-p nil)
  (tiled-p nil)
  (requested-width 0 :type fixnum)
  (requested-height 0 :type fixnum)
  (requested-x 0 :type fixnum)
  (requested-y 0 :type fixnum)
  (last-configured-width -1 :type fixnum)
  (last-configured-height -1 :type fixnum))

(defclass compositor ()
  ((display :initform nil :accessor compositor-display)
   (event-loop :initform nil :accessor compositor-event-loop)
   (backend :initform nil :accessor compositor-backend)
   (renderer :initform nil :accessor compositor-renderer)
   (allocator :initform nil :accessor compositor-allocator)
   (shm-allocator :initform nil :accessor compositor-shm-allocator)
   (task-queue :initform nil :accessor compositor-task-queue)
   (thread :initform nil :accessor compositor-thread)
   (socket-name :initform nil :accessor compositor-socket-name)
   (scene :initform nil :accessor compositor-scene)
   (scene-output-layout :initform nil :accessor compositor-scene-output-layout)
   (output-layout :initform nil :accessor compositor-output-layout)
   (backend-new-output-listener :initform nil :accessor compositor-backend-new-output-listener)
   (backend-new-input-listener :initform nil :accessor compositor-backend-new-input-listener)
   (xdg-shell :initform nil :accessor compositor-xdg-shell)
   (xdg-shell-new-toplevel-listener :initform nil
                                    :accessor compositor-xdg-shell-new-toplevel-listener)
   (seat :initform nil :accessor compositor-seat)
   (seat-request-set-selection-listener :initform nil
                                        :accessor compositor-seat-request-set-selection-listener)
   (seat-request-set-primary-selection-listener :initform nil
                                                :accessor compositor-seat-request-set-primary-selection-listener)
   (seat-request-set-cursor-listener :initform nil
                                     :accessor compositor-seat-request-set-cursor-listener)
   (cursor :initform nil :accessor compositor-cursor)
   (xcursor-manager :initform nil :accessor compositor-xcursor-manager)
   (keyboard :initform nil :accessor compositor-keyboard)
   (pointer :initform nil :accessor compositor-pointer)
   ;; Listeners attached to wlr_keyboard/wlr_pointer.  Both structs assert
   ;; that their signal lists are empty when they are finished, so they have
   ;; to be removed before the backend is destroyed.
   (input-listeners :initform '() :accessor compositor-input-listeners)
   (focused-surface :initform nil :accessor compositor-focused-surface)
   (pointer-focused-surface :initform nil :accessor compositor-pointer-focused-surface)
   (pointer-surface-x :initform 0.0d0 :accessor compositor-pointer-surface-x)
   (pointer-surface-y :initform 0.0d0 :accessor compositor-pointer-surface-y)
   ;; Layout-space origin of POINTER-FOCUSED-SURFACE, used to keep sending
   ;; motion to a client while a button drags the pointer outside of it.
   (pointer-focus-origin-x :initform 0.0d0 :accessor compositor-pointer-focus-origin-x)
   (pointer-focus-origin-y :initform 0.0d0 :accessor compositor-pointer-focus-origin-y)
   ;; :LEM or :CLIENT while a pointer button is held.  A grab keeps every
   ;; event of the drag on the side that received the initial press.
   (pointer-grab :initform nil :accessor compositor-pointer-grab)
   (pointer-grab-buttons :initform '() :accessor compositor-pointer-grab-buttons)
   (primary-output :initform nil :accessor compositor-primary-output)
   (output-width :initform 0 :type fixnum :accessor compositor-output-width)
   (output-height :initform 0 :type fixnum :accessor compositor-output-height)
   (outputs :initform (make-hash-table :test #'eql) :reader compositor-outputs)
   (toplevels :initform (make-hash-table :test #'eql) :reader compositor-toplevels)
   (toplevels-by-id :initform (make-hash-table :test #'eql)
                    :reader compositor-toplevels-by-id)
   (surface-toplevels :initform (make-hash-table :test #'eql)
                      :reader compositor-surface-toplevels)
   (xdg-surface-entries :initform (make-hash-table :test #'eql)
                        :reader compositor-xdg-surface-entries)
   (next-toplevel-id :initform 0 :type fixnum
                     :accessor compositor-next-toplevel-id)
   (background-rect :initform nil :accessor compositor-background-rect)
   (lem-views :initform '() :accessor compositor-lem-views)
   (view-scenes :initform (make-hash-table :test #'eql)
                :reader compositor-view-scenes)
   (keyboard-routing :initform (make-hash-table :test #'eql)
                     :reader compositor-keyboard-routing)
   (lifecycle-lock :initform (bt2:make-lock :name "lem-wayland lifecycle")
                   :reader compositor-lifecycle-lock)
   (lifecycle-condition :initform (bt2:make-condition-variable)
                        :reader compositor-lifecycle-condition)
   (started-p :initform nil :accessor compositor-started-p)
   (startup-error :initform nil :accessor compositor-startup-error)
   (stopped-p :initform nil :accessor compositor-stopped-p)))

;;;; Small helpers

(defun pointer-key (pointer)
  (cffi:pointer-address pointer))

(defun null-pointer-p (pointer)
  (or (null pointer) (cffi:null-pointer-p pointer)))

(defun output-resolution (output)
  (cffi:with-foreign-object (width :int)
    (cffi:with-foreign-object (height :int)
      (%wlr-output-effective-resolution output width height)
      (values (cffi:mem-ref width :int)
              (cffi:mem-ref height :int)))))

(defun output-fallback-resolution (output)
  (let ((width (i32-at output +wlr-output-width+))
        (height (i32-at output +wlr-output-height+)))
    (if (and (plusp width) (plusp height))
        (values width height)
        (values +default-output-width+ +default-output-height+))))

(defun send-frame-done (scene-output)
  (cffi:with-foreign-object (now :char +timespec-size+)
    (when (zerop (%clock-gettime +clock-monotonic+ now))
      (%wlr-scene-output-send-frame-done scene-output now))))

(defun update-seat-capabilities (compositor)
  (let ((capabilities 0))
    (when (compositor-keyboard compositor)
      (setf capabilities (logior capabilities +wl-seat-capability-keyboard+)))
    (when (compositor-pointer compositor)
      (setf capabilities (logior capabilities +wl-seat-capability-pointer+)))
    (when (compositor-seat compositor)
      (%wlr-seat-set-capabilities (compositor-seat compositor) capabilities))))

;;;; Keyboard

(defun install-keymap (keyboard)
  (let ((context (%xkb-context-new +xkb-context-no-flags+)))
    (unless (null-pointer-p context)
      (unwind-protect
           (let ((keymap (%xkb-keymap-new-from-names
                          context
                          (cffi:null-pointer)
                          +xkb-keymap-compile-no-flags+)))
             (unless (null-pointer-p keymap)
               (unwind-protect
                    (unless (%wlr-keyboard-set-keymap keyboard keymap)
                      (format *error-output* "lem-wayland: failed to set keymap~%"))
                 (%xkb-keymap-unref keymap))))
        (%xkb-context-unref context)))))

(defun key-event-time (event)
  (u32-at event +wlr-keyboard-key-event-time+))

(defun key-event-keycode (event)
  (u32-at event +wlr-keyboard-key-event-keycode+))

(defun key-event-state (event)
  (u32-at event +wlr-keyboard-key-event-state+))

(defun keyboard-xkb-state (compositor)
  (let ((keyboard (compositor-keyboard compositor)))
    (when keyboard
      (let ((state (ptr-at keyboard +wlr-keyboard-xkb-state+)))
        (unless (null-pointer-p state)
          state)))))

(defun modifier-active-p (compositor name)
  (let ((state (keyboard-xkb-state compositor)))
    (and state
         (plusp (%xkb-state-mod-name-is-active
                 state name +xkb-state-mods-all+)))))

(defun super-active-p (compositor)
  (modifier-active-p compositor "Mod4"))

(defun xkb-keycode (keycode)
  "wlr_keyboard key events carry evdev keycodes; xkb wants evdev + 8."
  (+ keycode 8))

(defun keycode-unshifted-sym (compositor keycode)
  (let* ((keyboard (compositor-keyboard compositor))
         (keymap (and keyboard (ptr-at keyboard +wlr-keyboard-keymap+)))
         (state (keyboard-xkb-state compositor)))
    (when (and keymap state
               (not (null-pointer-p keymap))
               (not (null-pointer-p state)))
      (let ((layout (%xkb-state-key-get-layout state (xkb-keycode keycode))))
        (cffi:with-foreign-object (syms :pointer)
          (when (plusp (%xkb-keymap-key-get-syms-by-level
                        keymap (xkb-keycode keycode) layout 0 syms))
            (let ((array (cffi:mem-ref syms :pointer)))
              (unless (null-pointer-p array)
                (let ((sym (cffi:mem-ref array :uint32)))
                  (unless (zerop sym)
                    sym))))))))))

(defun key-modifier-keysym-p (keysym)
  (or (<= #xffe1 keysym #xffee)
      (= keysym #xffe5)
      (= keysym #xff7f)))

(defun keysym->lem-sym (keysym)
  (cond
    ((= keysym #x20) "Space")
    ((= keysym #xff08) "Backspace")
    ((= keysym #xff09) "Tab")
    ((= keysym #xff0d) "Return")
    ((= keysym #xff1b) "Escape")
    ((= keysym #xffff) "Delete")
    ((= keysym #xff63) "Insert")
    ((= keysym #xff50) "Home")
    ((= keysym #xff57) "End")
    ((= keysym #xff55) "PageUp")
    ((= keysym #xff56) "PageDown")
    ((= keysym #xff51) "Left")
    ((= keysym #xff52) "Up")
    ((= keysym #xff53) "Right")
    ((= keysym #xff54) "Down")
    ((<= #xffbe keysym #xffc9)
     (format nil "F~D" (- keysym #xffbe -1)))
    ((<= #x21 keysym #x7e)
     (string (code-char keysym)))
    ((<= #x01000000 keysym #x0110ffff)
     (string (code-char (- keysym #x01000000))))
    (t (format nil "Key~X" keysym))))

(defun lem-key-args (compositor keycode)
  "Return MAKE-KEY arguments for KEYCODE, or NIL for a bare modifier."
  (let* ((state (keyboard-xkb-state compositor))
         (sym (and state (%xkb-state-key-get-one-sym state (xkb-keycode keycode))))
         (utf32 (and state (%xkb-state-key-get-utf32 state (xkb-keycode keycode))))
         (shift (modifier-active-p compositor "Shift"))
         (ctrl (modifier-active-p compositor "Control"))
         (meta (modifier-active-p compositor "Mod1"))
         (super (modifier-active-p compositor "Mod4"))
         (hyper (modifier-active-p compositor "Mod3")))
    (cond
      ((null state) nil)
      ((and (not (or ctrl meta super hyper))
            (integerp utf32)
            (>= utf32 #x20)
            (not (<= #x7f utf32 #x9f)))
       (list :sym (if (= utf32 #x20)
                      "Space"
                      (string (code-char utf32)))
             :shift nil :ctrl nil :meta nil :super nil :hyper nil))
      ((key-modifier-keysym-p sym) nil)
      (t
       (let ((base (or (keycode-unshifted-sym compositor keycode) sym)))
         (list :sym (keysym->lem-sym base)
               :ctrl ctrl :meta meta :super super :hyper hyper
               :shift shift))))))

(defun send-key-event-to-client (compositor event)
  (let ((seat (compositor-seat compositor))
        (surface (compositor-focused-surface compositor)))
    (when (and seat surface)
      (%wlr-seat-keyboard-notify-key seat
                                     (key-event-time event)
                                     (key-event-keycode event)
                                     (key-event-state event))
      t)))

(defun send-key-event-to-lem (compositor keycode)
  (let ((args (lem-key-args compositor keycode)))
    (when args
      (lem:send-event
       (lambda ()
         ;; MAKE-KEY caches keys in a global hash table; construct the key on
         ;; the editor thread where every other MAKE-KEY call happens.
         (lem:send-event (apply #'lem:make-key args)))))
    t))

(defun handle-key-event (compositor event)
  "Route key events: Super combinations always belong to Lem.

While a client surface has keyboard focus, ordinary keys are forwarded to
it (and modifiers are mirrored by HANDLE-MODIFIERS-EVENT).  Everything else
is translated to a Lem key event.  Key releases are matched against the
route chosen for the press so the client never sees a release it did not
see being pressed."
  (let* ((keycode (key-event-keycode event))
         (pressed (= (key-event-state event) +wl-keyboard-key-state-pressed+))
         (routing (compositor-keyboard-routing compositor))
         (route (gethash keycode routing)))
    (cond
      ((not pressed)
       (remhash keycode routing)
       (when (eq route :client)
         (send-key-event-to-client compositor event)))
      ((eq route :client)
       (send-key-event-to-client compositor event))
      ((eq route :lem)
       (send-key-event-to-lem compositor keycode))
      ((and (not (super-active-p compositor))
            (compositor-focused-surface compositor))
       (setf (gethash keycode routing) :client)
       (send-key-event-to-client compositor event))
      (t
       (setf (gethash keycode routing) :lem)
       (send-key-event-to-lem compositor keycode)))))

(defun handle-modifiers-event (compositor)
  (let ((seat (compositor-seat compositor))
        (keyboard (compositor-keyboard compositor)))
    (when (and seat keyboard
               (not (super-active-p compositor)))
      (%wlr-seat-keyboard-notify-modifiers
       seat
       (cffi:inc-pointer keyboard +wlr-keyboard-modifiers+)))))

(defun focus-surface (compositor surface)
  (let ((old (compositor-focused-surface compositor)))
    (unless (and (not (null-pointer-p old))
                 (not (null-pointer-p surface))
                 (cffi:pointer-eq old surface))
      (clrhash (compositor-keyboard-routing compositor))))
  (let ((seat (compositor-seat compositor))
        (keyboard (compositor-keyboard compositor)))
    (cond
      ((null-pointer-p surface)
       (when seat
         (%wlr-seat-keyboard-notify-clear-focus seat)))
      ((and seat keyboard)
       (%wlr-seat-keyboard-notify-enter
        seat
        surface
        (cffi:inc-pointer keyboard +wlr-keyboard-keycodes+)
        (size-at keyboard +wlr-keyboard-num-keycodes+)
        (cffi:inc-pointer keyboard +wlr-keyboard-modifiers+))))
    (setf (compositor-focused-surface compositor) surface)))

(defun focus-next-mapped-toplevel (compositor)
  (block nil
    (maphash (lambda (key toplevel)
               (declare (ignore key))
               (when (and (wayland-toplevel-mapped-p toplevel)
                          (not (null-pointer-p (wayland-toplevel-surface toplevel))))
                 (focus-surface compositor (wayland-toplevel-surface toplevel))
                 (return t)))
             (compositor-toplevels compositor))
    (focus-surface compositor nil)))

(defun handle-new-keyboard (compositor device)
  (let ((keyboard (%wlr-keyboard-from-input-device device)))
    (unless (null-pointer-p keyboard)
      (install-keymap keyboard)
      (setf (compositor-keyboard compositor) keyboard)
      (when (compositor-seat compositor)
        (%wlr-seat-set-keyboard (compositor-seat compositor) keyboard))
      (push (add-signal-listener
             keyboard +wlr-keyboard-events-key+
             (lambda (event)
               (handle-key-event compositor event)))
            (compositor-input-listeners compositor))
      (push (add-signal-listener
             keyboard +wlr-keyboard-events-modifiers+
             (lambda (event)
               (declare (ignore event))
               (handle-modifiers-event compositor)))
            (compositor-input-listeners compositor))
      (update-seat-capabilities compositor)
      (when (compositor-focused-surface compositor)
        (focus-surface compositor (compositor-focused-surface compositor)))
      (format *error-output* "lem-wayland: keyboard ready~%")
      (finish-output *error-output*))))

;;;; Pointer

(defun scene-surface-at (compositor x y)
  (cffi:with-foreign-object (nx :double)
    (cffi:with-foreign-object (ny :double)
      (let ((node (%wlr-scene-node-at (compositor-scene compositor)
                                      (coerce x 'double-float)
                                      (coerce y 'double-float)
                                      nx ny)))
        (unless (null-pointer-p node)
          ;; WLR_SCENE_NODE_AT also returns rect and tree nodes; only buffer
          ;; nodes can be converted with wlr_scene_buffer_from_node.
          (when (= (u32-at node +wlr-scene-node-type+)
                   +wlr-scene-node-type-buffer+)
            (let* ((buffer (%wlr-scene-buffer-from-node node))
                   (scene-surface (unless (null-pointer-p buffer)
                                    (%wlr-scene-surface-try-from-buffer buffer))))
              (unless (null-pointer-p scene-surface)
                (values (ptr-at scene-surface +wlr-scene-surface-surface+)
                        (cffi:mem-ref nx :double)
                        (cffi:mem-ref ny :double))))))))))

(defun restore-default-cursor (compositor)
  (let ((cursor (compositor-cursor compositor))
        (manager (compositor-xcursor-manager compositor)))
    (when cursor
      (%wlr-cursor-set-surface cursor (cffi:null-pointer) 0 0)
      (when manager
        (%wlr-cursor-set-xcursor cursor manager "left_ptr")))))

(defun clear-pointer-focus (compositor)
  (when (compositor-pointer-focused-surface compositor)
    (when (compositor-seat compositor)
      (%wlr-seat-pointer-notify-clear-focus (compositor-seat compositor)))
    (setf (compositor-pointer-focused-surface compositor) nil)
    (restore-default-cursor compositor)))

(defun notify-lem-pointer-position (compositor x y)
  (declare (ignore compositor))
  (when *pointer-position-function*
    (funcall *pointer-position-function* x y)))

(defun notify-lem-pointer-motion (compositor x y)
  (declare (ignore compositor))
  (when *pointer-motion-function*
    (funcall *pointer-motion-function* x y)))

(defun notify-lem-pointer-button (compositor x y button pressed)
  (declare (ignore compositor))
  (when *pointer-button-function*
    (funcall *pointer-button-function* x y button pressed)))

(defun notify-lem-pointer-axis (compositor x y wheel-y)
  (declare (ignore compositor))
  (when *pointer-axis-function*
    (funcall *pointer-axis-function* x y wheel-y)))

(defun evdev-button->lem-button (button)
  "Translate a Linux evdev button code to Lem's mouse button keyword."
  (cond ((= button +btn-left+) :button-1)
        ((= button +btn-right+) :button-3)
        ((= button +btn-middle+) :button-2)
        (t nil)))

(defun pointer-grab-active-p (compositor)
  (not (null (compositor-pointer-grab compositor))))

(defun pointer-grab-owns-surface-p (compositor)
  "True when pointer motion belongs to the focused client surface."
  (let ((surface (compositor-pointer-focused-surface compositor)))
    (and (eq (compositor-pointer-grab compositor) :client)
         surface
         (not (null-pointer-p surface)))))

(defun track-pointer-grab-button (compositor button pressed)
  (if pressed
      (pushnew button (compositor-pointer-grab-buttons compositor))
      (setf (compositor-pointer-grab-buttons compositor)
            (delete button (compositor-pointer-grab-buttons compositor))))
  (when (and (not pressed)
             (null (compositor-pointer-grab-buttons compositor)))
    (setf (compositor-pointer-grab compositor) nil)))

(defun pointer-local-coordinates (compositor x y)
  "Convert global layout coordinates to coordinates inside the grabbed surface."
  (values (- x (compositor-pointer-focus-origin-x compositor))
          (- y (compositor-pointer-focus-origin-y compositor))))

(defun toplevel-for-surface (compositor surface)
  (when (and surface (not (null-pointer-p surface)))
    (or (gethash (pointer-key surface)
                 (compositor-surface-toplevels compositor))
        (gethash (pointer-key surface)
                 (compositor-toplevels compositor)))))

(defun lem-view-at (compositor x y)
  "Return the id of the topmost Lem view containing the point X, Y."
  (loop :for rect :in (reverse (compositor-lem-views compositor))
        :when (and (<= (lem-view-rect-x rect) x)
                   (< x (+ (lem-view-rect-x rect) (lem-view-rect-width rect)))
                   (<= (lem-view-rect-y rect) y)
                   (< y (+ (lem-view-rect-y rect) (lem-view-rect-height rect))))
        :return (lem-view-rect-id rect)))

(defun focus-lem-content (compositor)
  "Give the Lem view under the cursor its window's focus."
  (let* ((cursor (compositor-cursor compositor))
         (id (and cursor
                  (lem-view-at compositor
                               (double-at cursor +wlr-cursor-x+)
                               (double-at cursor +wlr-cursor-y+)))))
    (when (and id *focus-view-function*)
      (funcall *focus-view-function* id)
      t)))

(defun ensure-pointer-focus (compositor surface sx sy)
  (unless (or (null surface) (null-pointer-p surface))
    (let ((old (compositor-pointer-focused-surface compositor)))
      (unless (and old (cffi:pointer-eq surface old))
        (when old
          ;; A surface may have installed its own cursor image; go back to
          ;; the compositor's default before the next client takes over.
          (restore-default-cursor compositor))
        (when (compositor-seat compositor)
          (%wlr-seat-pointer-notify-enter
           (compositor-seat compositor) surface sx sy))
        (setf (compositor-pointer-focused-surface compositor) surface)))
    (setf (compositor-pointer-surface-x compositor) sx
          (compositor-pointer-surface-y compositor) sy)
    (let ((cursor (compositor-cursor compositor)))
      (when cursor
        (setf (compositor-pointer-focus-origin-x compositor)
              (- (double-at cursor +wlr-cursor-x+) sx)
              (compositor-pointer-focus-origin-y compositor)
              (- (double-at cursor +wlr-cursor-y+) sy))))))

(defun notify-client-pointer-axis (compositor event)
  (let ((seat (compositor-seat compositor)))
    (when seat
      (%wlr-seat-pointer-notify-axis
       seat
       (u32-at event +wlr-pointer-axis-time+)
       (u32-at event +wlr-pointer-axis-orientation+)
       (double-at event +wlr-pointer-axis-delta+)
       (i32-at event +wlr-pointer-axis-delta-discrete+)
       (u32-at event +wlr-pointer-axis-source+)
       (i32-at event +wlr-pointer-axis-relative-direction+)))))

(defun pointer-axis-steps (event)
  "Return the wheel amount in Lem units (positive means up)."
  (let ((discrete (i32-at event +wlr-pointer-axis-delta-discrete+))
        (delta (double-at event +wlr-pointer-axis-delta+)))
    ;; Wayland's positive axis delta means scrolling down, while Lem's
    ;; positive wheel-y means scrolling up.
    (- (if (/= discrete 0) discrete delta))))

(defun notify-lem-wheel (compositor event x y)
  (when (= (u32-at event +wlr-pointer-axis-orientation+) 0)
    (let ((steps (pointer-axis-steps event)))
      (unless (zerop steps)
        (notify-lem-pointer-axis compositor x y steps)))))

(defun resume-pointer-hover (compositor surface sx sy)
  "Update pointer focus after a grab ends; SURFACE is under the cursor."
  (if (or (null surface) (null-pointer-p surface))
      (clear-pointer-focus compositor)
      (ensure-pointer-focus compositor surface sx sy)))

(defun handle-pointer-motion-at (compositor time x y)
  (notify-lem-pointer-position compositor x y)
  (cond
    ((eq (compositor-pointer-grab compositor) :lem)
     ;; A Lem drag keeps receiving the global position even over clients.
     (clear-pointer-focus compositor)
     (notify-lem-pointer-motion compositor x y))
    ((pointer-grab-owns-surface-p compositor)
     (multiple-value-bind (lx ly) (pointer-local-coordinates compositor x y)
       (when (compositor-seat compositor)
         (%wlr-seat-pointer-notify-motion
          (compositor-seat compositor) time lx ly))))
    (t
     (multiple-value-bind (surface sx sy) (scene-surface-at compositor x y)
       (if (or (null surface) (null-pointer-p surface))
           (progn
             (clear-pointer-focus compositor)
             (notify-lem-pointer-motion compositor x y))
           (progn
             (ensure-pointer-focus compositor surface sx sy)
             (when (compositor-seat compositor)
               (%wlr-seat-pointer-notify-motion
                (compositor-seat compositor) time sx sy))))))))

(defun handle-pointer-motion (compositor event)
  (let ((cursor (compositor-cursor compositor))
        (device (ptr-at event 0))
        (time (u32-at event +wlr-pointer-motion-time+)))
    (when cursor
      (%wlr-cursor-move cursor
                        device
                        (double-at event +wlr-pointer-motion-delta-x+)
                        (double-at event +wlr-pointer-motion-delta-y+))
      (handle-pointer-motion-at compositor
                                time
                                (double-at cursor +wlr-cursor-x+)
                                (double-at cursor +wlr-cursor-y+)))))

(defun handle-pointer-motion-absolute (compositor event)
  (let ((cursor (compositor-cursor compositor))
        (device (ptr-at event 0))
        (time (u32-at event +wlr-pointer-motion-absolute-time+)))
    (when cursor
      (%wlr-cursor-warp-absolute cursor
                                 device
                                 (double-at event +wlr-pointer-motion-absolute-x+)
                                 (double-at event +wlr-pointer-motion-absolute-y+))
      (handle-pointer-motion-at compositor
                                time
                                (double-at cursor +wlr-cursor-x+)
                                (double-at cursor +wlr-cursor-y+)))))

(defun handle-pointer-button (compositor event)
  (let* ((cursor (compositor-cursor compositor))
         (seat (compositor-seat compositor))
         (time (u32-at event +wlr-pointer-button-time+))
         (button (u32-at event +wlr-pointer-button-button+))
         (state (u32-at event +wlr-pointer-button-state+))
         (pressed (= state +wl-pointer-button-state-pressed+))
         (lem-button (evdev-button->lem-button button)))
    (when cursor
      (let ((x (double-at cursor +wlr-cursor-x+))
            (y (double-at cursor +wlr-cursor-y+)))
        (notify-lem-pointer-position compositor x y)
        (multiple-value-bind (surface sx sy) (scene-surface-at compositor x y)
          (let ((surface (unless (null-pointer-p surface) surface)))
            (cond
              ;; A grab owns every button until its last press is released.
              ((pointer-grab-active-p compositor)
               (let ((grab (compositor-pointer-grab compositor)))
                 (track-pointer-grab-button compositor button pressed)
                 (ecase grab
                   (:lem
                    (when lem-button
                      (notify-lem-pointer-button
                       compositor x y lem-button pressed)))
                   (:client
                    (when (and seat
                               (pointer-grab-owns-surface-p compositor))
                      (%wlr-seat-pointer-notify-button
                       seat time button state))))
                 (unless (pointer-grab-active-p compositor)
                   (resume-pointer-hover compositor surface sx sy))))
              (pressed
               (if surface
                   (progn
                     (setf (compositor-pointer-grab compositor) :client)
                     (pushnew button
                              (compositor-pointer-grab-buttons compositor))
                     (ensure-pointer-focus compositor surface sx sy)
                     (when *focus-client-function*
                       (let ((toplevel
                               (toplevel-for-surface compositor surface)))
                         (when toplevel
                           (funcall *focus-client-function*
                                    (wayland-toplevel-id toplevel)))))
                     (when seat
                       (%wlr-seat-pointer-notify-button
                        seat time button state)))
                   (progn
                     ;; Press on Lem content, a modeline, or a separator.
                     (setf (compositor-pointer-grab compositor) :lem)
                     (pushnew button
                              (compositor-pointer-grab-buttons compositor))
                     (focus-lem-content compositor)
                     (when lem-button
                       (notify-lem-pointer-button
                        compositor x y lem-button t)))))
              (t
               ;; A release whose press we did not see.  Keep the client in
               ;; sync if the pointer is over one; never start a grab here.
               (when surface
                 (ensure-pointer-focus compositor surface sx sy)
                 (when seat
                   (%wlr-seat-pointer-notify-button
                    seat time button state)))))))))))

(defun handle-pointer-axis (compositor event)
  (let ((cursor (compositor-cursor compositor)))
    (when cursor
      (let ((x (double-at cursor +wlr-cursor-x+))
            (y (double-at cursor +wlr-cursor-y+)))
        (notify-lem-pointer-position compositor x y)
        (cond
          ((eq (compositor-pointer-grab compositor) :lem)
           (notify-lem-wheel compositor event x y))
          ((pointer-grab-owns-surface-p compositor)
           (notify-client-pointer-axis compositor event))
          (t
           (multiple-value-bind (surface sx sy)
               (scene-surface-at compositor x y)
             (if (or (null surface) (null-pointer-p surface))
                 (notify-lem-wheel compositor event x y)
                 (progn
                   (ensure-pointer-focus compositor surface sx sy)
                   (notify-client-pointer-axis compositor event))))))))))

(defun handle-pointer-frame (compositor)
  (when (compositor-seat compositor)
    (%wlr-seat-pointer-notify-frame (compositor-seat compositor))))

(defun handle-new-pointer (compositor device)
  (let ((pointer (%wlr-pointer-from-input-device device)))
    (unless (null-pointer-p pointer)
      (setf (compositor-pointer compositor) pointer)
      (when (compositor-cursor compositor)
        (%wlr-cursor-attach-input-device (compositor-cursor compositor) device))
      (push (add-signal-listener
             pointer +wlr-pointer-events-motion+
             (lambda (event) (handle-pointer-motion compositor event)))
            (compositor-input-listeners compositor))
      (push (add-signal-listener
             pointer +wlr-pointer-events-motion-absolute+
             (lambda (event) (handle-pointer-motion-absolute compositor event)))
            (compositor-input-listeners compositor))
      (push (add-signal-listener
             pointer +wlr-pointer-events-button+
             (lambda (event) (handle-pointer-button compositor event)))
            (compositor-input-listeners compositor))
      (push (add-signal-listener
             pointer +wlr-pointer-events-axis+
             (lambda (event) (handle-pointer-axis compositor event)))
            (compositor-input-listeners compositor))
      (push (add-signal-listener
             pointer +wlr-pointer-events-frame+
             (lambda (event)
               (declare (ignore event))
               (handle-pointer-frame compositor)))
            (compositor-input-listeners compositor))
      (update-seat-capabilities compositor)
      (format *error-output* "lem-wayland: pointer ready~%")
      (finish-output *error-output*))))

(defun handle-new-input (compositor device)
  (let ((type (i32-at device +wlr-input-device-type+)))
    (cond
      ((= type +wlr-input-device-keyboard+)
       (handle-new-keyboard compositor device))
      ((= type +wlr-input-device-pointer+)
       (handle-new-pointer compositor device))
      (t nil))))

;;;; Clipboard

(defconstant +pollin+ #x001)
(defconstant +clipboard-poll-timeout-ms+ 1000)

(defparameter *clipboard-mime-types*
  '("text/plain;charset=utf-8" "text/plain")
  "MIME types advertised by the compositor's selection source.")

(defparameter *clipboard-preferred-mime-types*
  '("text/plain;charset=utf-8" "text/plain" "UTF8_STRING" "STRING" "TEXT")
  "MIME types we understand when reading a client selection, most preferred first.")

(defvar *lem-data-source* nil
  "Foreign pointer to the wlr_data_source serving Lem's clipboard.")

(defvar *lem-data-source-impl* nil
  "Foreign pointer to the wlr_data_source_impl used by *LEM-DATA-SOURCE*.")

(defvar *lem-selection-text* ""
  "Text currently offered by the compositor's own selection source.")

(cffi:defcallback %lem-data-source-send :void
    ((source :pointer) (mime-type :string) (fd :int))
  (declare (ignore source mime-type))
  (unwind-protect
       (cffi:with-foreign-string ((buffer size) *lem-selection-text*
                                  :encoding :utf-8)
         ;; SIZE includes the trailing NUL that must not reach the client.
         (let ((length (max 0 (1- size))))
           (loop :with offset = 0
                 :while (< offset length)
                 :do (let ((written (%unix-write
                                     fd
                                     (cffi:inc-pointer buffer offset)
                                     (- length offset))))
                       (unless (plusp written)
                         (return))
                       (incf offset written)))))
    (%unix-close fd)))

(cffi:defcallback %lem-data-source-accept :void
    ((source :pointer) (serial :uint32) (mime-type :string))
  (declare (ignore source serial mime-type)))

(cffi:defcallback %lem-data-source-destroy :void ((source :pointer))
  (when (and *lem-data-source*
             (cffi:pointer-eq source *lem-data-source*))
    (let ((impl *lem-data-source-impl*))
      (setf *lem-data-source* nil
            *lem-data-source-impl* nil)
      (when impl
        (cffi:foreign-free impl)))
    (cffi:foreign-free source)))

(defun initialize-data-source-mime-types (source mime-types)
  (let* ((pointer-size (cffi:foreign-type-size :pointer))
         (count (length mime-types))
         (size (* count pointer-size))
         (array (cffi:foreign-alloc :pointer :count (max count 1))))
    (loop :for type :in mime-types
          :for index :from 0
          :do (setf (cffi:mem-ref array :pointer (* index pointer-size))
                    (cffi:foreign-string-alloc type)))
    (setf (cffi:mem-ref source :size
                        (+ +wlr-data-source-mime-types+ +wl-array-size-field+))
          size
          (cffi:mem-ref source :size
                        (+ +wlr-data-source-mime-types+ +wl-array-alloc+))
          size
          (cffi:mem-ref source :pointer
                        (+ +wlr-data-source-mime-types+ +wl-array-data+))
          array)))

(defun ensure-data-source ()
  "Return the compositor's wlr_data_source, creating it on first use."
  (or *lem-data-source*
      (let* ((impl (cffi:foreign-alloc :char
                                       :count +wlr-data-source-impl-size+
                                       :initial-element 0))
             (source (cffi:foreign-alloc :char
                                         :count +wlr-data-source-size+
                                         :initial-element 0)))
        (setf (cffi:mem-ref impl :pointer +wlr-data-source-impl-send+)
              (cffi:callback %lem-data-source-send)
              (cffi:mem-ref impl :pointer +wlr-data-source-impl-accept+)
              (cffi:callback %lem-data-source-accept)
              (cffi:mem-ref impl :pointer +wlr-data-source-impl-destroy+)
              (cffi:callback %lem-data-source-destroy))
        (%wlr-data-source-init source impl)
        (initialize-data-source-mime-types source *clipboard-mime-types*)
        (setf *lem-data-source* source
              *lem-data-source-impl* impl)
        source)))

(defun data-source-mime-types (source)
  (let* ((pointer-size (cffi:foreign-type-size :pointer))
         (size (cffi:mem-ref source :size
                             (+ +wlr-data-source-mime-types+
                                +wl-array-size-field+)))
         (data (cffi:mem-ref source :pointer
                             (+ +wlr-data-source-mime-types+
                                +wl-array-data+))))
    (unless (null-pointer-p data)
      (loop :for index :from 0 :below (floor size pointer-size)
            :for type = (cffi:mem-ref data :pointer (* index pointer-size))
            :unless (null-pointer-p type)
            :collect (cffi:foreign-string-to-lisp type)))))

(defun select-clipboard-mime-type (source)
  (let ((types (data-source-mime-types source)))
    (loop :for type :in *clipboard-preferred-mime-types*
          :when (member type types :test #'string=)
          :return type)))

(defun decode-clipboard-bytes (bytes)
  (let* ((length (length bytes))
         (buffer (cffi:foreign-alloc :char :count (1+ length))))
    (unwind-protect
         (progn
           (loop :for index :from 0 :below length
                 :do (setf (cffi:mem-ref buffer :uchar index)
                           (aref bytes index)))
           (setf (cffi:mem-ref buffer :uchar length) 0)
           (handler-case
               (cffi:foreign-string-to-lisp buffer :encoding :utf-8)
             (error () nil)))
      (cffi:foreign-free buffer))))

(defun read-clipboard-fd (fd)
  (let ((buffer (cffi:foreign-alloc :char :count 4096))
        (pollfd (cffi:foreign-alloc :char
                                    :count +pollfd-size+
                                    :initial-element 0))
        (bytes (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t
                             :fill-pointer 0)))
    (unwind-protect
         (progn
           (setf (cffi:mem-ref pollfd :int +pollfd-fd+) fd
                 (cffi:mem-ref pollfd :short +pollfd-events+) +pollin+)
           (loop
             (when (not (plusp (%unix-poll pollfd 1
                                           +clipboard-poll-timeout-ms+)))
               (return))
             (let ((count (%unix-read fd buffer 4096)))
               (if (plusp count)
                   (loop :for index :from 0 :below count
                         :do (vector-push-extend
                              (cffi:mem-ref buffer :uchar index)
                              bytes))
                   (return))))
           (decode-clipboard-bytes bytes))
      (cffi:foreign-free pollfd)
      (cffi:foreign-free buffer))))

(defun read-selection-text (source mime-type)
  (let ((fds (cffi:foreign-alloc :int :count 2)))
    (unwind-protect
         (when (zerop (%unix-pipe fds))
           (let ((read-fd (cffi:mem-ref fds :int 0))
                 (write-fd (cffi:mem-ref fds :int 1)))
             (unwind-protect
                  (progn
                    (%wlr-data-source-send source mime-type write-fd)
                    (%unix-close write-fd)
                    (setf write-fd -1)
                    (read-clipboard-fd read-fd))
               (unless (minusp write-fd)
                 (%unix-close write-fd))
               (%unix-close read-fd))))
      (cffi:foreign-free fds))))

(defun set-seat-clipboard-text (compositor text)
  "Offer TEXT as the seat selection on behalf of the compositor."
  (let ((seat (compositor-seat compositor)))
    (when seat
      (setf *lem-selection-text* (or text ""))
      (%wlr-seat-set-selection
       seat
       (ensure-data-source)
       (%wl-display-next-serial (compositor-display compositor)))
      t)))

(defun seat-clipboard-text (compositor)
  "Return the text currently selected on COMPOSITOR's seat, or NIL."
  (let* ((seat (compositor-seat compositor))
         (source (and seat (ptr-at seat +wlr-seat-selection-source+))))
    (cond
      ((null-pointer-p source) nil)
      ((and *lem-data-source*
            (cffi:pointer-eq source *lem-data-source*))
       (copy-seq *lem-selection-text*))
      (t
       (let ((mime-type (select-clipboard-mime-type source)))
         (when mime-type
           (read-selection-text source mime-type)))))))

(defun destroy-lem-data-source ()
  (let ((source *lem-data-source*))
    (when source
      (%wlr-data-source-destroy source))))

(defun handle-request-set-selection (compositor event)
  (when (compositor-seat compositor)
    (%wlr-seat-set-selection
     (compositor-seat compositor)
     (ptr-at event +wlr-seat-request-set-selection-source+)
     (u32-at event +wlr-seat-request-set-selection-serial+))))

(defun handle-request-set-primary-selection (compositor event)
  (when (compositor-seat compositor)
    (%wlr-seat-set-primary-selection
     (compositor-seat compositor)
     (ptr-at event +wlr-seat-request-set-primary-selection-source+)
     (u32-at event +wlr-seat-request-set-primary-selection-serial+))))

(defun handle-request-set-cursor (compositor event)
  (let ((cursor (compositor-cursor compositor)))
    (when cursor
      (let ((surface (ptr-at event +wlr-seat-request-set-cursor-surface+)))
        (if (null-pointer-p surface)
            (restore-default-cursor compositor)
            (%wlr-cursor-set-surface
             cursor
             surface
             (i32-at event +wlr-seat-request-set-cursor-hotspot-x+)
             (i32-at event +wlr-seat-request-set-cursor-hotspot-y+)))))))

;;;; xdg-shell clients

(defun toplevel-target-size (compositor toplevel)
  (if (and (plusp (wayland-toplevel-requested-width toplevel))
           (plusp (wayland-toplevel-requested-height toplevel)))
      (values (wayland-toplevel-requested-width toplevel)
              (wayland-toplevel-requested-height toplevel))
      (if (and (plusp (compositor-output-width compositor))
               (plusp (compositor-output-height compositor)))
          (values (compositor-output-width compositor)
                  (compositor-output-height compositor))
          (values +default-output-width+ +default-output-height+))))

(defun configure-toplevel (compositor toplevel)
  (let ((xdg (wayland-toplevel-toplevel toplevel)))
    (unless (null-pointer-p xdg)
      (multiple-value-bind (width height) (toplevel-target-size compositor toplevel)
        (unless (and (= width (wayland-toplevel-last-configured-width toplevel))
                     (= height (wayland-toplevel-last-configured-height toplevel)))
          (setf (wayland-toplevel-last-configured-width toplevel) width
                (wayland-toplevel-last-configured-height toplevel) height)
          (%wlr-xdg-toplevel-set-size xdg width height)))
      (let ((edges (if (wayland-toplevel-floating-p toplevel)
                       0
                       +wlr-edge-all+)))
        (unless (or (and (null (wayland-toplevel-tiled-p toplevel))
                         (zerop edges))
                    (and (wayland-toplevel-tiled-p toplevel)
                         (= edges +wlr-edge-all+)))
          (setf (wayland-toplevel-tiled-p toplevel)
                (= edges +wlr-edge-all+))
          (%wlr-xdg-toplevel-set-tiled xdg edges))))))

(defun maybe-configure-toplevel (compositor toplevel)
  (unless (wayland-toplevel-configured-p toplevel)
    (setf (wayland-toplevel-configured-p toplevel) t)
    (unless (null-pointer-p (wayland-toplevel-toplevel toplevel))
      (configure-toplevel compositor toplevel))))

(defun acknowledge-toplevel-request (compositor toplevel)
  "Send a configure event in reply to an xdg-toplevel state request.

Lem decides every geometry itself, so maximize/fullscreen/minimize/move
requests are acknowledged (the xdg-shell protocol requires a configure
event) without changing the layout.  Resizing and moving stay available
through Lem's window commands."
  (when (and toplevel
             (wayland-toplevel-configured-p toplevel)
             (not (null-pointer-p (wayland-toplevel-toplevel toplevel))))
    ;; Force configure-toplevel to emit a new event even when the size did
    ;; not change, so the client's pending request is acknowledged.
    (setf (wayland-toplevel-last-configured-width toplevel) 0
          (wayland-toplevel-last-configured-height toplevel) 0)
    (configure-toplevel compositor toplevel)))

(defun handle-toplevel-state-request (compositor toplevel)
  (acknowledge-toplevel-request compositor toplevel))

(defun update-toplevel-sizes (compositor)
  (maphash (lambda (key toplevel)
             (declare (ignore key))
             (when (and (wayland-toplevel-configured-p toplevel)
                        (not (null-pointer-p (wayland-toplevel-toplevel toplevel))))
               (configure-toplevel compositor toplevel)))
           (compositor-toplevels compositor)))

(defun set-toplevel-activated (toplevel active)
  (unless (eq (not (null (wayland-toplevel-activated-p toplevel)))
              (not (null active)))
    (setf (wayland-toplevel-activated-p toplevel) active)
    (unless (null-pointer-p (wayland-toplevel-toplevel toplevel))
      (%wlr-xdg-toplevel-set-activated
       (wayland-toplevel-toplevel toplevel)
       active))))

(defun toplevel-parent-id (compositor toplevel)
  (let* ((xdg (wayland-toplevel-toplevel toplevel))
         (parent-xdg (and (not (null-pointer-p xdg))
                          (ptr-at xdg +wlr-xdg-toplevel-parent+))))
    (when (and parent-xdg (not (null-pointer-p parent-xdg)))
      (let* ((base (ptr-at parent-xdg +wlr-xdg-toplevel-base+))
             (surface (and base (not (null-pointer-p base))
                           (ptr-at base +wlr-xdg-surface-surface+)))
             (parent (and surface
                          (gethash (pointer-key surface)
                                   (compositor-toplevels compositor)))))
        (when parent
          (wayland-toplevel-id parent))))))

(defun forget-toplevel-surface (compositor surface)
  (remhash (pointer-key surface) (compositor-surface-toplevels compositor)))

(defun notify-toplevel-title-changed (toplevel)
  (when (and (wayland-toplevel-registered-p toplevel)
             *toplevel-title-changed-function*)
    (funcall *toplevel-title-changed-function*
             (wayland-toplevel-id toplevel)
             (wayland-toplevel-title toplevel)
             (wayland-toplevel-app-id toplevel))))

(defun handle-toplevel-title (compositor toplevel)
  (declare (ignore compositor))
  (setf (wayland-toplevel-title toplevel)
        (or (cstring-at (wayland-toplevel-toplevel toplevel)
                        +wlr-xdg-toplevel-title+)
            ""))
  (notify-toplevel-title-changed toplevel))

(defun handle-toplevel-app-id (compositor toplevel)
  (declare (ignore compositor))
  (setf (wayland-toplevel-app-id toplevel)
        (or (cstring-at (wayland-toplevel-toplevel toplevel)
                        +wlr-xdg-toplevel-app-id+)
            ""))
  (notify-toplevel-title-changed toplevel))

(defun handle-toplevel-map (compositor toplevel)
  (maybe-configure-toplevel compositor toplevel)
  (setf (wayland-toplevel-mapped-p toplevel) t)
  (when (wayland-toplevel-scene-node toplevel)
    (%wlr-scene-node-set-enabled
     (wayland-toplevel-scene-node toplevel) nil))
  (unless (wayland-toplevel-registered-p toplevel)
    (setf (wayland-toplevel-registered-p toplevel) t)
    (let ((parent-id (toplevel-parent-id compositor toplevel)))
      (setf (wayland-toplevel-parent-id toplevel) parent-id
            (wayland-toplevel-floating-p toplevel) (not (null parent-id)))
      (when *toplevel-mapped-function*
        (funcall *toplevel-mapped-function*
                 (wayland-toplevel-id toplevel)
                 (wayland-toplevel-title toplevel)
                 (wayland-toplevel-app-id toplevel)
                 parent-id
                 (wayland-toplevel-floating-p toplevel)))))
  (format *error-output*
          "lem-wayland: client ~D mapped title=~S app_id=~S~%"
          (wayland-toplevel-id toplevel)
          (wayland-toplevel-title toplevel)
          (wayland-toplevel-app-id toplevel))
  (finish-output *error-output*))

(defun handle-toplevel-unmap (compositor toplevel)
  (setf (wayland-toplevel-mapped-p toplevel) nil
        (wayland-toplevel-visible-p toplevel) nil)
  (when (wayland-toplevel-scene-node toplevel)
    (%wlr-scene-node-set-enabled
     (wayland-toplevel-scene-node toplevel) nil))
  (set-toplevel-activated toplevel nil)
  (when (and (compositor-focused-surface compositor)
             (cffi:pointer-eq (compositor-focused-surface compositor)
                              (wayland-toplevel-surface toplevel)))
    (focus-surface compositor nil))
  (when (and (compositor-pointer-focused-surface compositor)
             (cffi:pointer-eq (compositor-pointer-focused-surface compositor)
                              (wayland-toplevel-surface toplevel)))
    (clear-pointer-focus compositor)))

(defun handle-toplevel-destroy (compositor toplevel)
  ;; Both the xdg-toplevel destroy signal and the wlr-surface destroy signal
  ;; may call this; only the first call does the work.
  (unless (gethash (wayland-toplevel-id toplevel)
                   (compositor-toplevels-by-id compositor))
    (return-from handle-toplevel-destroy nil))
  (let ((surface (wayland-toplevel-surface toplevel)))
    (remove-signal-listener (wayland-toplevel-map-listener toplevel))
    (remove-signal-listener (wayland-toplevel-unmap-listener toplevel))
    (remove-signal-listener (wayland-toplevel-destroy-listener toplevel))
    (remove-signal-listener (wayland-toplevel-surface-destroy-listener toplevel))
    (remove-signal-listener (wayland-toplevel-commit-listener toplevel))
    (remove-signal-listener (wayland-toplevel-set-title-listener toplevel))
    (remove-signal-listener (wayland-toplevel-set-app-id-listener toplevel))
    (remove-signal-listener (wayland-toplevel-set-parent-listener toplevel))
    (remove-signal-listener
     (wayland-toplevel-request-maximize-listener toplevel))
    (remove-signal-listener
     (wayland-toplevel-request-fullscreen-listener toplevel))
    (remove-signal-listener
     (wayland-toplevel-request-minimize-listener toplevel))
    (remove-signal-listener (wayland-toplevel-request-move-listener toplevel))
    (remove-signal-listener
     (wayland-toplevel-request-resize-listener toplevel))
    (remove-signal-listener
     (wayland-toplevel-request-show-window-menu-listener toplevel))
    (remhash (pointer-key surface) (compositor-toplevels compositor))
    (remhash (wayland-toplevel-id toplevel)
             (compositor-toplevels-by-id compositor))
    (when surface
      (forget-toplevel-surface compositor surface))
    (maphash (lambda (key entry)
               (when (eq entry toplevel)
                 (remhash key (compositor-surface-toplevels compositor))))
             (compositor-surface-toplevels compositor))
    (let ((entry (gethash (pointer-key (wayland-toplevel-xdg-surface toplevel))
                          (compositor-xdg-surface-entries compositor))))
      (when entry
        (remove-signal-listener (scene-xdg-entry-new-popup-listener entry))
        (remove-signal-listener (scene-xdg-entry-destroy-listener entry))
        (remhash (pointer-key (wayland-toplevel-xdg-surface toplevel))
                 (compositor-xdg-surface-entries compositor))))
    (when (and (compositor-focused-surface compositor)
               (cffi:pointer-eq (compositor-focused-surface compositor) surface))
      (focus-surface compositor nil))
    (when (and (compositor-pointer-focused-surface compositor)
               (cffi:pointer-eq (compositor-pointer-focused-surface compositor)
                                surface))
      ;; A button may still be held when its window disappears.
      (setf (compositor-pointer-grab compositor) nil
            (compositor-pointer-grab-buttons compositor) '())
      (clear-pointer-focus compositor))
    (when *toplevel-destroyed-function*
      (funcall *toplevel-destroyed-function*
               (wayland-toplevel-id toplevel)))))

(defun remove-xdg-surface-entry (compositor base)
  (let* ((key (pointer-key base))
         (entry (gethash key (compositor-xdg-surface-entries compositor))))
    (when entry
      (remhash key (compositor-xdg-surface-entries compositor))
      ;; Removing the listener that is currently running is supported by
      ;; wl_signal_emit, which iterates the list with a safe iterator.
      (remove-signal-listener (scene-xdg-entry-new-popup-listener entry))
      (remove-signal-listener (scene-xdg-entry-destroy-listener entry))))
  (values))

(defun handle-new-popup (compositor parent-base popup)
  (let* ((base (ptr-at popup +wlr-xdg-popup-base+))
         (surface (ptr-at base +wlr-xdg-surface-surface+))
         (parent-entry (gethash (pointer-key parent-base)
                                (compositor-xdg-surface-entries compositor)))
         (parent-toplevel (gethash (pointer-key parent-base)
                                   (compositor-surface-toplevels compositor)))
         (tree (if parent-entry
                   (%wlr-scene-xdg-surface-create
                    (scene-xdg-entry-tree parent-entry) base)
                   (%wlr-scene-xdg-surface-create
                    (compositor-scene compositor) base))))
    (when (and tree parent-toplevel surface (not (null-pointer-p surface)))
      (setf (gethash (pointer-key surface)
                     (compositor-surface-toplevels compositor))
            parent-toplevel))
    (when tree
      (let ((entry (make-scene-xdg-entry :base base :tree tree)))
        (setf (gethash (pointer-key base)
                       (compositor-xdg-surface-entries compositor))
              entry)
        (let ((new-popup-listener
                (add-signal-listener
                 base +wlr-xdg-surface-events-new-popup+
                 (lambda (new-popup)
                   (handle-new-popup compositor base new-popup)))))
          (setf (scene-xdg-entry-new-popup-listener entry) new-popup-listener
                (scene-xdg-entry-destroy-listener entry)
                (add-signal-listener
                 base +wlr-xdg-surface-events-destroy+
                 (lambda (event)
                   (declare (ignore event))
                   (when (and surface (not (null-pointer-p surface)))
                     (remhash (pointer-key surface)
                              (compositor-surface-toplevels compositor)))
                   (remove-xdg-surface-entry compositor base)))))))))

(defun handle-new-toplevel (compositor xdg-toplevel)
  (let* ((base (ptr-at xdg-toplevel +wlr-xdg-toplevel-base+))
         (surface (ptr-at base +wlr-xdg-surface-surface+))
         (scene-node (%wlr-scene-xdg-surface-create
                      (compositor-scene compositor) base))
         (id (incf (compositor-next-toplevel-id compositor)))
         (toplevel (%make-wayland-toplevel
                    :id id
                    :toplevel xdg-toplevel
                    :xdg-surface base
                    :surface surface
                    :scene-node scene-node
                    :title (or (cstring-at xdg-toplevel
                                           +wlr-xdg-toplevel-title+)
                               "")
                    :app-id (or (cstring-at xdg-toplevel
                                            +wlr-xdg-toplevel-app-id+)
                                ""))))
    (when scene-node
      (%wlr-scene-node-set-position scene-node 0 0)
      (%wlr-scene-node-set-enabled scene-node nil))
    (setf (gethash (pointer-key surface) (compositor-toplevels compositor))
          toplevel
          (gethash (pointer-key surface)
                   (compositor-surface-toplevels compositor))
          toplevel
          (gethash id (compositor-toplevels-by-id compositor))
          toplevel)
    (let ((entry (make-scene-xdg-entry :base base :tree scene-node)))
      (setf (gethash (pointer-key base)
                     (compositor-xdg-surface-entries compositor))
            entry)
      (setf (scene-xdg-entry-new-popup-listener entry)
            (add-signal-listener
             base +wlr-xdg-surface-events-new-popup+
             (lambda (popup)
               (handle-new-popup compositor base popup)))
            (scene-xdg-entry-destroy-listener entry)
            (add-signal-listener
             base +wlr-xdg-surface-events-destroy+
             (lambda (event)
               (declare (ignore event))
               (remove-xdg-surface-entry compositor base)))))
    (setf (wayland-toplevel-map-listener toplevel)
          (add-signal-listener
           surface +wlr-surface-events-map+
           (lambda (event)
             (declare (ignore event))
             (handle-toplevel-map compositor toplevel)))
          (wayland-toplevel-unmap-listener toplevel)
          (add-signal-listener
           surface +wlr-surface-events-unmap+
           (lambda (event)
             (declare (ignore event))
             (handle-toplevel-unmap compositor toplevel)))
          ;; wlroots requires every listener attached to the xdg-toplevel
          ;; signals to be gone by the time its destroy signal is emitted.
          (wayland-toplevel-destroy-listener toplevel)
          (add-signal-listener
           xdg-toplevel +wlr-xdg-toplevel-events-destroy+
           (lambda (event)
             (declare (ignore event))
             (handle-toplevel-destroy compositor toplevel)))
          (wayland-toplevel-surface-destroy-listener toplevel)
          (add-signal-listener
           surface +wlr-surface-events-destroy+
           (lambda (event)
             (declare (ignore event))
             (handle-toplevel-destroy compositor toplevel)))
          (wayland-toplevel-commit-listener toplevel)
          (add-signal-listener
           surface +wlr-surface-events-commit+
           (lambda (event)
             (declare (ignore event))
             (maybe-configure-toplevel compositor toplevel)))
          (wayland-toplevel-set-title-listener toplevel)
          (add-signal-listener
           xdg-toplevel +wlr-xdg-toplevel-events-set-title+
           (lambda (event)
             (declare (ignore event))
             (handle-toplevel-title compositor toplevel)))
          (wayland-toplevel-set-app-id-listener toplevel)
          (add-signal-listener
           xdg-toplevel +wlr-xdg-toplevel-events-set-app-id+
           (lambda (event)
             (declare (ignore event))
             (handle-toplevel-app-id compositor toplevel)))
          (wayland-toplevel-set-parent-listener toplevel)
          (add-signal-listener
           xdg-toplevel +wlr-xdg-toplevel-events-set-parent+
           (lambda (event)
             (declare (ignore event))
             (let ((parent-id (toplevel-parent-id compositor toplevel)))
               (setf (wayland-toplevel-parent-id toplevel) parent-id
                     (wayland-toplevel-floating-p toplevel)
                     (not (null parent-id)))))))
    ;; xdg-shell requires compositors to answer state requests with a
    ;; configure event.  Lem owns the layout, so the request itself is not
    ;; applied by the compositor; the frontend exposes equivalent commands.
    (setf (wayland-toplevel-request-maximize-listener toplevel)
          (add-signal-listener
           xdg-toplevel +wlr-xdg-toplevel-events-request-maximize+
           (lambda (event)
             (declare (ignore event))
             (handle-toplevel-state-request compositor toplevel)))
          (wayland-toplevel-request-fullscreen-listener toplevel)
          (add-signal-listener
           xdg-toplevel +wlr-xdg-toplevel-events-request-fullscreen+
           (lambda (event)
             (declare (ignore event))
             (handle-toplevel-state-request compositor toplevel)))
          (wayland-toplevel-request-minimize-listener toplevel)
          (add-signal-listener
           xdg-toplevel +wlr-xdg-toplevel-events-request-minimize+
           (lambda (event)
             (declare (ignore event))
             (handle-toplevel-state-request compositor toplevel)))
          (wayland-toplevel-request-move-listener toplevel)
          (add-signal-listener
           xdg-toplevel +wlr-xdg-toplevel-events-request-move+
           (lambda (event)
             (declare (ignore event))
             (handle-toplevel-state-request compositor toplevel)))
          (wayland-toplevel-request-resize-listener toplevel)
          (add-signal-listener
           xdg-toplevel +wlr-xdg-toplevel-events-request-resize+
           (lambda (event)
             (declare (ignore event))
             (handle-toplevel-state-request compositor toplevel)))
          (wayland-toplevel-request-show-window-menu-listener toplevel)
          (add-signal-listener
           xdg-toplevel +wlr-xdg-toplevel-events-request-show-window-menu+
           (lambda (event)
             (declare (ignore event))
             (handle-toplevel-state-request compositor toplevel))))
    (format *error-output*
            "lem-wayland: new toplevel ~D title=~S app_id=~S~%"
            id
            (wayland-toplevel-title toplevel)
            (wayland-toplevel-app-id toplevel))
    (finish-output *error-output*)))

;;;; Lem views
;;;
;;; Lem renders each window into a Cairo image.  The frontend uploads those
;;; pixels here, and SYNC-FRAME places every view and every client surface in
;;; the order given by Lem's window tree.

(defun create-shm-argb8888-buffer (compositor width height)
  "Allocate a wlr_buffer backed by shared memory with ARGB8888 pixels."
  (let* ((modifiers (cffi:foreign-alloc :uint64 :count 2 :initial-element 0))
         (format (cffi:foreign-alloc :char :count 32 :initial-element 0)))
    (unwind-protect
         (progn
           (setf (cffi:mem-ref modifiers :uint64) +drm-format-mod-linear+)
           (setf (cffi:mem-ref format :uint32 0) +drm-format-argb8888+
                 (cffi:mem-ref format :size 8) 1
                 (cffi:mem-ref format :size 16) 1
                 (cffi:mem-ref format :pointer 24) modifiers)
           (%wlr-allocator-create-buffer (compositor-shm-allocator compositor)
                                         width
                                         height
                                         format))
      (cffi:foreign-free format)
      (cffi:foreign-free modifiers))))

(defun upload-pixels-to-buffer (buffer data width height stride)
  "Copy pixels into BUFFER.  Returns NIL when the buffer is busy."
  (cffi:with-foreign-objects ((ptr :pointer)
                              (format :uint32)
                              (buffer-stride :size))
    (unless (%wlr-buffer-begin-data-ptr-access
             buffer +wlr-buffer-data-ptr-access-write+
             ptr format buffer-stride)
      (return-from upload-pixels-to-buffer nil))
    (unwind-protect
         (let ((destination (cffi:mem-ref ptr :pointer))
               (destination-stride (cffi:mem-ref buffer-stride :size))
               (row-bytes (* width 4)))
           (unless (= (cffi:mem-ref format :uint32) +drm-format-argb8888+)
             (error "Unexpected buffer format ~X from the shared-memory allocator"
                    (cffi:mem-ref format :uint32)))
           (loop :for row :below height
                 :do (%memcpy
                      (cffi:inc-pointer destination (* row destination-stride))
                      (cffi:inc-pointer data (* row stride))
                      (min row-bytes destination-stride))))
      (%wlr-buffer-end-data-ptr-access buffer))
    t))

(defun destroy-view-scene (compositor scene)
  (declare (ignore compositor))
  (let ((tree (view-scene-tree scene)))
    (when (and tree (not (null-pointer-p tree)))
      (%wlr-scene-node-destroy tree))
    (dolist (buffer (view-scene-buffers scene))
      (%wlr-buffer-drop buffer))
    (setf (view-scene-tree scene) nil
          (view-scene-image-node scene) nil
          (view-scene-buffers scene) '()
          (view-scene-current-buffer scene) nil)))

(defun delete-view-scene (compositor id)
  (let ((scene (gethash id (compositor-view-scenes compositor))))
    (when scene
      (remhash id (compositor-view-scenes compositor))
      (destroy-view-scene compositor scene))))

(defun ensure-view-scene (compositor id)
  (or (gethash id (compositor-view-scenes compositor))
      (let ((scene (make-view-scene :id id)))
        (setf (gethash id (compositor-view-scenes compositor)) scene)
        scene)))

(defun ensure-view-tree (compositor scene)
  (or (view-scene-tree scene)
      (let ((tree (%wlr-scene-tree-create (compositor-scene compositor))))
        (unless (null-pointer-p tree)
          (%wlr-scene-node-set-position tree 0 0))
        (setf (view-scene-tree scene) tree))))

(defun update-view-image (compositor id width height data stride)
  "Upload WIDTH x HEIGHT ARGB pixels from DATA (row stride STRIDE) into ID's view."
  (when (and (plusp width) (plusp height))
    (let* ((scene (ensure-view-scene compositor id))
           (tree (ensure-view-tree compositor scene))
           (same-size-p (and (= width (view-scene-buffer-width scene))
                             (= height (view-scene-buffer-height scene))))
           (old-buffers (view-scene-buffers scene))
           (current (view-scene-current-buffer scene))
           (buffer (and same-size-p
                        (find-if (lambda (candidate) (not (eq candidate current)))
                                 old-buffers))))
      (when (null-pointer-p tree)
        (error "Could not create a scene tree for Lem view ~D" id))
      ;; Prefer the pooled buffer that is not currently attached to the scene
      ;; node.  If it is still busy, drop it and allocate a fresh one.
      (when (and buffer
                 (not (upload-pixels-to-buffer buffer data width height stride)))
        (%wlr-buffer-drop buffer)
        (setf old-buffers (remove buffer old-buffers)
              buffer nil))
      (unless buffer
        (setf buffer (create-shm-argb8888-buffer compositor width height))
        (when (null-pointer-p buffer)
          (error "Could not allocate an ARGB buffer for Lem view ~D" id))
        (unless (upload-pixels-to-buffer buffer data width height stride)
          (error "Could not upload pixels into a fresh buffer of view ~D" id)))
      (let ((node (view-scene-image-node scene)))
        (if node
            (%wlr-scene-buffer-set-buffer node buffer)
            (let ((new-node (%wlr-scene-buffer-create tree buffer)))
              (when (null-pointer-p new-node)
                (error "Could not create a scene buffer for Lem view ~D" id))
              (%wlr-scene-node-set-position new-node 0 0)
              (setf (view-scene-image-node scene) new-node))))
      (cond
        (same-size-p
         (setf (view-scene-buffers scene)
               (cons buffer (remove buffer old-buffers))
               (view-scene-current-buffer scene) buffer))
        (t
         ;; A size change invalidates every buffer of the old size.  The scene
         ;; node has already been switched to the new buffer above.
         (dolist (old old-buffers)
           (%wlr-buffer-drop old))
         (setf (view-scene-buffers scene) (list buffer)
               (view-scene-current-buffer scene) buffer)))
      (setf (view-scene-buffer-width scene) width
            (view-scene-buffer-height scene) height))))

(defun close-toplevel (compositor id)
  (let ((toplevel (gethash id (compositor-toplevels-by-id compositor))))
    (when (and toplevel
               (not (null-pointer-p (wayland-toplevel-toplevel toplevel))))
      (%wlr-xdg-toplevel-send-close
       (wayland-toplevel-toplevel toplevel))
      t)))

(defun disable-toplevel-scene (toplevel)
  (setf (wayland-toplevel-visible-p toplevel) nil)
  (let ((node (wayland-toplevel-scene-node toplevel)))
    (when (and node (not (null-pointer-p node)))
      (%wlr-scene-node-set-enabled node nil))))

(defun apply-client-frame (compositor frame)
  (let ((toplevel (gethash (client-frame-id frame)
                           (compositor-toplevels-by-id compositor))))
    (when toplevel
      (setf (wayland-toplevel-requested-x toplevel) (client-frame-x frame)
            (wayland-toplevel-requested-y toplevel) (client-frame-y frame)
            (wayland-toplevel-requested-width toplevel) (client-frame-width frame)
            (wayland-toplevel-requested-height toplevel) (client-frame-height frame)
            (wayland-toplevel-floating-p toplevel) (client-frame-floating-p frame))
      (when (and (client-frame-visible-p frame)
                 (wayland-toplevel-mapped-p toplevel)
                 (plusp (client-frame-width frame))
                 (plusp (client-frame-height frame)))
        (setf (wayland-toplevel-visible-p toplevel) t)
        (configure-toplevel compositor toplevel)
        (let ((node (wayland-toplevel-scene-node toplevel)))
          (when (and node (not (null-pointer-p node)))
            (%wlr-scene-node-set-position
             node
             (client-frame-x frame)
             (client-frame-y frame))
            (%wlr-scene-node-set-enabled node t)
            (%wlr-scene-node-raise-to-top node))))
      (unless (and (client-frame-visible-p frame)
                   (wayland-toplevel-mapped-p toplevel)
                   (plusp (client-frame-width frame))
                   (plusp (client-frame-height frame)))
        (disable-toplevel-scene toplevel)))))

(defun update-focus-and-activation (compositor focused-client-id)
  (let* ((focused (and focused-client-id
                       (gethash focused-client-id
                                (compositor-toplevels-by-id compositor))))
         (target (when (and focused
                            (wayland-toplevel-mapped-p focused)
                            (wayland-toplevel-visible-p focused)
                            (not (null-pointer-p
                                  (wayland-toplevel-surface focused))))
                   (wayland-toplevel-surface focused))))
    (maphash
     (lambda (id toplevel)
       (declare (ignore id))
       (set-toplevel-activated
        toplevel
        (and (eq toplevel focused)
             (wayland-toplevel-visible-p toplevel)
             (wayland-toplevel-mapped-p toplevel))))
     (compositor-toplevels-by-id compositor))
    (let ((current (compositor-focused-surface compositor)))
      (unless (or (and (null-pointer-p target)
                       (null-pointer-p current))
                  (and (not (null-pointer-p target))
                       (not (null-pointer-p current))
                       (cffi:pointer-eq target current)))
        (focus-surface compositor target)))))

(defun sync-frame (compositor items focused-client-id)
  "Lay out one frame: ITEMS is Lem's window order, bottom to top.

Each item is a VIEW-FRAME or CLIENT-FRAME.  Client surfaces are only shown
where Lem's window tree displays them; every other mapped client is hidden."
  (maphash (lambda (id toplevel)
             (declare (ignore id))
             (disable-toplevel-scene toplevel))
           (compositor-toplevels-by-id compositor))
  (let ((rects '()))
    (dolist (item items)
      (typecase item
        (view-frame
         (let ((scene (gethash (view-frame-id item)
                               (compositor-view-scenes compositor))))
           (when scene
             (let ((tree (view-scene-tree scene)))
               (when (and tree (not (null-pointer-p tree)))
                 (%wlr-scene-node-set-position
                  tree (view-frame-x item) (view-frame-y item))
                 (%wlr-scene-node-set-enabled tree t)
                 (%wlr-scene-node-raise-to-top tree))
               (when (plusp (view-scene-buffer-width scene))
                 (push (make-lem-view-rect
                        :id (view-frame-id item)
                        :x (view-frame-x item)
                        :y (view-frame-y item)
                        :width (view-scene-buffer-width scene)
                        :height (view-scene-buffer-height scene))
                       rects))))))
        (client-frame
         (apply-client-frame compositor item))))
    (setf (compositor-lem-views compositor) (nreverse rects)))
  (update-focus-and-activation compositor focused-client-id))

(defun update-background (compositor width height)
  (let ((rect (compositor-background-rect compositor)))
    (cond
      (rect
       (%wlr-scene-rect-set-size rect width height))
      (t
       (cffi:with-foreign-object (color :float 4)
         (setf (cffi:mem-ref color :float 0) 0.06
               (cffi:mem-ref color :float 1) 0.07
               (cffi:mem-ref color :float 2) 0.09
               (cffi:mem-ref color :float 3) 1.0)
         (let ((node (%wlr-scene-rect-create
                      (compositor-scene compositor) width height color)))
           (unless (null-pointer-p node)
             (%wlr-scene-node-lower-to-bottom node)
             (setf (compositor-background-rect compositor) node))))))))

;;;; Outputs

(defun enable-output (compositor output width height)
  (declare (ignore compositor width height))
  (let ((state (cffi:foreign-alloc :char
                                   :count +wlr-output-state-size+
                                   :initial-element 0)))
    (unwind-protect
         (progn
           (%wlr-output-state-init state)
           (%wlr-output-state-set-enabled state t)
           (unless (%wlr-output-commit-state output state)
             (format *error-output* "lem-wayland: failed to commit output~%")
             (finish-output *error-output*)))
      (%wlr-output-state-finish state)
      (cffi:foreign-free state))))

(defun handle-output-frame (compositor scene-output)
  (declare (ignore compositor))
  (when (%wlr-scene-output-commit scene-output (cffi:null-pointer))
    (send-frame-done scene-output)))

(defun handle-output-destroy (compositor output)
  (let* ((key (pointer-key output))
         (info (gethash key (compositor-outputs compositor))))
    (when info
      (remove-signal-listener (output-info-frame-listener info))
      (remove-signal-listener (output-info-destroy-listener info))
      (remhash key (compositor-outputs compositor))
      (when (and (compositor-primary-output compositor)
                 (cffi:pointer-eq (compositor-primary-output compositor) output))
        (setf (compositor-primary-output compositor) nil
              (compositor-output-width compositor) 0
              (compositor-output-height compositor) 0)
        (notify-output-changed compositor)))))

(defun handle-new-output (compositor output)
  (unless (%wlr-output-init-render
           output
           (compositor-allocator compositor)
           (compositor-renderer compositor))
    (format *error-output* "lem-wayland: wlr_output_init_render failed~%"))
  (let ((layout-output (%wlr-output-layout-add-auto
                        (compositor-output-layout compositor) output))
        (scene-output (%wlr-scene-output-create
                       (compositor-scene compositor) output)))
    (%wlr-scene-output-layout-add-output
     (compositor-scene-output-layout compositor)
     layout-output
     scene-output)
    (multiple-value-bind (width height) (output-resolution output)
      (when (or (<= width 0) (<= height 0))
        (multiple-value-setq (width height) (output-fallback-resolution output)))
      (let ((info (make-output-info
                   :output output
                   :layout-output layout-output
                   :scene-output scene-output
                   :width width
                   :height height)))
        (setf (gethash (pointer-key output) (compositor-outputs compositor)) info)
        (setf (output-info-frame-listener info)
              (add-signal-listener
               output +wlr-output-events-frame+
               (lambda (event)
                 (declare (ignore event))
                 (handle-output-frame compositor scene-output)))
              (output-info-destroy-listener info)
              (add-signal-listener
               output +wlr-output-events-destroy+
               (lambda (event)
                 (declare (ignore event))
                 (handle-output-destroy compositor output))))
        (enable-output compositor output width height)
        (multiple-value-bind (new-width new-height) (output-resolution output)
          (when (and (plusp new-width) (plusp new-height))
            (setf (output-info-width info) new-width
                  (output-info-height info) new-height)))
        (when (or (null (compositor-primary-output compositor))
                  (cffi:pointer-eq (compositor-primary-output compositor) output))
          (setf (compositor-primary-output compositor) output
                (compositor-output-width compositor) (output-info-width info)
                (compositor-output-height compositor) (output-info-height info))
          (notify-output-changed compositor))
        (update-background compositor
                           (compositor-output-width compositor)
                           (compositor-output-height compositor))
        (update-toplevel-sizes compositor)
        (format *error-output*
                "lem-wayland: output ~Dx~D~%"
                (output-info-width info)
                (output-info-height info))
        (finish-output *error-output*)))))

;;;; Lifecycle

(defvar *compositor-signal-sources* '())

(cffi:defcallback %compositor-signal :int
    ((signal-number :int) (data :pointer))
  (declare (ignore data))
  (when (member signal-number '(2 15))
    (when *compositor*
      (lem:send-event
       (lambda ()
         (handler-case
             (lem:exit-editor)
           (lem:exit-editor () nil)
           (error () nil))))))
  0)

(defun install-signal-handlers (compositor)
  (dolist (signal-number '(2 15))
    (let ((source (%wl-event-loop-add-signal
                   (compositor-event-loop compositor)
                   signal-number
                   (cffi:callback %compositor-signal)
                   (cffi:null-pointer))))
      (when source
        (push source *compositor-signal-sources*)))))

(defun remove-signal-handlers ()
  (dolist (source *compositor-signal-sources*)
    (unless (null-pointer-p source)
      (%wl-event-source-remove source)))
  (setf *compositor-signal-sources* '()))

(defun signal-lifecycle (compositor)
  (bt2:with-lock-held ((compositor-lifecycle-lock compositor))
    (bt2:condition-broadcast (compositor-lifecycle-condition compositor))))

(defun start-compositor (compositor)
  "Create the Wayland socket, start the backend, and install signals.
Must be called on the compositor event-loop thread."
  (let ((socket-name-pointer
          (%wl-display-add-socket-auto (compositor-display compositor))))
    (when (null-pointer-p socket-name-pointer)
      (error "Could not add a Wayland socket (is XDG_RUNTIME_DIR set?)"))
    (setf (compositor-socket-name compositor)
          (cffi:foreign-string-to-lisp socket-name-pointer))
    (unless (%wlr-backend-start (compositor-backend compositor))
      (error "Could not start the wlroots backend"))
    (setf *compositor* compositor)
    (install-signal-handlers compositor)
    (format *error-output* "~&lem-wayland: WAYLAND_DISPLAY=~A~%"
            (compositor-socket-name compositor))
    (finish-output *error-output*)))

(defun stop-compositor (compositor)
  "Ask the compositor event loop to exit.  Safe to call from any thread."
  (when compositor
    (bt2:with-lock-held ((compositor-lifecycle-lock compositor))
      (let ((display (compositor-display compositor)))
        (when (and display
                   (not (null-pointer-p display))
                   (not (compositor-stopped-p compositor)))
          (%wl-display-terminate display))))))

(defun run-compositor-event-loop (compositor)
  (setf (task-queue-thread (compositor-task-queue compositor))
        (bt2:current-thread))
  (unwind-protect
       (progn
         (start-compositor compositor)
         (setf (compositor-started-p compositor) t)
         (signal-lifecycle compositor)
         (%wl-display-run (compositor-display compositor)))
    (when (compositor-task-queue compositor)
      (setf (task-queue-thread (compositor-task-queue compositor)) nil))
    (setf (compositor-stopped-p compositor) t)
    (signal-lifecycle compositor)))

(defun destroy-compositor (compositor)
  (when compositor
    (when (compositor-task-queue compositor)
      (close-task-queue (compositor-task-queue compositor)))
    (remove-signal-handlers)
    (when (compositor-view-scenes compositor)
      (maphash (lambda (id scene)
                 (declare (ignore id))
                 (destroy-view-scene compositor scene))
               (compositor-view-scenes compositor))
      (clrhash (compositor-view-scenes compositor)))
    (bt2:with-lock-held ((compositor-lifecycle-lock compositor))
      (when (and (compositor-display compositor)
                 (not (null-pointer-p (compositor-display compositor))))
        (%wl-display-destroy-clients (compositor-display compositor))
        (destroy-lem-data-source)
        (when (and (compositor-scene compositor)
                   (not (null-pointer-p (compositor-scene compositor))))
          (%wlr-scene-node-destroy (compositor-scene compositor))
          (setf (compositor-scene compositor) nil))
        ;; wlr_xdg_shell asserts that no NEW-TOPLEVEL listener is left when
        ;; the display is destroyed.  Drop our listeners on the backend too
        ;; so teardown does not call back into a half-destroyed compositor.
        (remove-signal-listener
         (compositor-xdg-shell-new-toplevel-listener compositor))
        (setf (compositor-xdg-shell-new-toplevel-listener compositor) nil)
        (remove-signal-listener
         (compositor-backend-new-output-listener compositor))
        (remove-signal-listener
         (compositor-backend-new-input-listener compositor))
        (setf (compositor-backend-new-output-listener compositor) nil
              (compositor-backend-new-input-listener compositor) nil)
        ;; The seat asserts that its signal lists are empty when destroyed as
        ;; part of wl_display_destroy.
        (remove-signal-listener
         (compositor-seat-request-set-cursor-listener compositor))
        (remove-signal-listener
         (compositor-seat-request-set-selection-listener compositor))
        (remove-signal-listener
         (compositor-seat-request-set-primary-selection-listener compositor))
        (setf (compositor-seat-request-set-cursor-listener compositor) nil
              (compositor-seat-request-set-selection-listener compositor) nil
              (compositor-seat-request-set-primary-selection-listener compositor) nil)
        ;; wlr_keyboard_finish/wlr_pointer_finish assert that no listener is
        ;; attached to their signals when the backend destroys the devices.
        (dolist (listener (compositor-input-listeners compositor))
          (remove-signal-listener listener))
        ;; wlr_seat_set_keyboard attached the seat's own listeners to the
        ;; keyboard; detach them before the backend destroys the device,
        ;; otherwise wlr_keyboard_finish asserts on a non-empty signal list.
        (when (and (compositor-seat compositor)
                   (compositor-keyboard compositor))
          (%wlr-seat-set-keyboard (compositor-seat compositor)
                                  (cffi:null-pointer)))
        (setf (compositor-input-listeners compositor) '()
              (compositor-keyboard compositor) nil
              (compositor-pointer compositor) nil)
        (when (and (compositor-backend compositor)
                   (not (null-pointer-p (compositor-backend compositor))))
          (%wlr-backend-destroy (compositor-backend compositor))
          (setf (compositor-backend compositor) nil))
        (%wl-display-destroy (compositor-display compositor))
        (setf (compositor-display compositor) nil)))
    (setf *compositor* nil)))

(defun initialize-compositor (compositor)
  (setf (compositor-display compositor) (%wl-display-create))
  (unless (compositor-display compositor)
    (error "Could not create wl_display"))
  (let ((event-loop (%wl-display-get-event-loop (compositor-display compositor))))
    (setf (compositor-event-loop compositor) event-loop
          (compositor-task-queue compositor) (make-task-queue event-loop)
          (compositor-backend compositor)
          (%wlr-backend-autocreate event-loop (cffi:null-pointer)))
    (unless (compositor-backend compositor)
      (error "Could not create a wlroots backend (check WLR_BACKENDS)")))
  (setf (compositor-renderer compositor)
        (%wlr-renderer-autocreate (compositor-backend compositor)))
  (unless (compositor-renderer compositor)
    (error "Could not create a wlroots renderer"))
  (setf (compositor-allocator compositor)
        (%wlr-allocator-autocreate (compositor-backend compositor)
                                   (compositor-renderer compositor)))
  (unless (compositor-allocator compositor)
    (error "Could not create a wlroots allocator"))
  (setf (compositor-shm-allocator compositor) (%wlr-shm-allocator-create))
  (unless (compositor-shm-allocator compositor)
    (error "Could not create a wlroots shared-memory allocator"))
  (%wlr-compositor-create (compositor-display compositor)
                          6
                          (compositor-renderer compositor))
  (%wlr-subcompositor-create (compositor-display compositor))
  (%wlr-data-device-manager-create (compositor-display compositor))
  (%wlr-primary-selection-v1-device-manager-create
   (compositor-display compositor))
  (%wlr-shm-create-with-renderer (compositor-display compositor)
                                 1
                                 (compositor-renderer compositor))
  (%wlr-linux-dmabuf-v1-create-with-renderer (compositor-display compositor)
                                             5
                                             (compositor-renderer compositor))
  (setf (compositor-scene compositor) (%wlr-scene-create)
        (compositor-output-layout compositor)
        (%wlr-output-layout-create (compositor-display compositor))
        (compositor-xdg-shell compositor)
        (%wlr-xdg-shell-create (compositor-display compositor) 5)
        (compositor-seat compositor)
        (%wlr-seat-create (compositor-display compositor) "seat0")
        (compositor-cursor compositor)
        (%wlr-cursor-create))
  (setf (compositor-scene-output-layout compositor)
        (%wlr-scene-attach-output-layout
         (compositor-scene compositor)
         (compositor-output-layout compositor)))
  (%wlr-cursor-attach-output-layout
   (compositor-cursor compositor)
   (compositor-output-layout compositor))
  (let ((manager (%wlr-xcursor-manager-create (cffi:null-pointer) 24)))
    (setf (compositor-xcursor-manager compositor) manager)
    (when manager
      (%wlr-xcursor-manager-load manager 1.0)
      (%wlr-cursor-set-xcursor (compositor-cursor compositor)
                               manager "left_ptr")))
  (setf (compositor-backend-new-output-listener compositor)
        (add-signal-listener
         (compositor-backend compositor) +wlr-backend-events-new-output+
         (lambda (output)
           (handle-new-output compositor output)))
        (compositor-backend-new-input-listener compositor)
        (add-signal-listener
         (compositor-backend compositor) +wlr-backend-events-new-input+
         (lambda (device)
           (handle-new-input compositor device)))
        (compositor-xdg-shell-new-toplevel-listener compositor)
        (add-signal-listener
         (compositor-xdg-shell compositor) +wlr-xdg-shell-events-new-toplevel+
         (lambda (toplevel)
           (handle-new-toplevel compositor toplevel)))
        (compositor-seat-request-set-cursor-listener compositor)
        (add-signal-listener
         (compositor-seat compositor) +wlr-seat-events-request-set-cursor+
         (lambda (event)
           (handle-request-set-cursor compositor event)))
        (compositor-seat-request-set-selection-listener compositor)
        (add-signal-listener
         (compositor-seat compositor) +wlr-seat-events-request-set-selection+
         (lambda (event)
           (handle-request-set-selection compositor event)))
        (compositor-seat-request-set-primary-selection-listener compositor)
        (add-signal-listener
         (compositor-seat compositor)
         +wlr-seat-events-request-set-primary-selection+
         (lambda (event)
           (handle-request-set-primary-selection compositor event))))
  compositor)

(defun make-compositor ()
  (initialize-compositor (make-instance 'compositor)))

(defun run-compositor ()
  "Create and run the Wayland compositor.  Returns when the display exits."
  (let ((compositor (make-compositor)))
    (unwind-protect
         (run-compositor-event-loop compositor)
      (destroy-compositor compositor))))

(defun run-compositor-thread (compositor)
  (unwind-protect
       (handler-case
           (progn
             (initialize-compositor compositor)
             (run-compositor-event-loop compositor))
         (error (condition)
           (setf (compositor-startup-error compositor) condition)
           (format *error-output* "~&lem-wayland: ~A~%" condition)
           (finish-output *error-output*)))
    (destroy-compositor compositor)
    (setf (compositor-stopped-p compositor) t)
    (signal-lifecycle compositor)))

(defun wait-for-compositor-thread (compositor timeout)
  "Wait up to TIMEOUT seconds for the compositor thread to exit.
Returns true when the thread has finished."
  (let ((thread (compositor-thread compositor))
        (deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (unless (and thread (bt2:thread-alive-p thread))
        (return t))
      (when (>= (get-internal-real-time) deadline)
        (return nil))
      (sleep 0.05))))

(defun launch-compositor ()
  "Create and start a compositor on a dedicated thread, returning it once the
Wayland socket is accepting clients."
  (let ((compositor (make-instance 'compositor)))
    (setf (compositor-thread compositor)
          (bt2:make-thread (lambda () (run-compositor-thread compositor))
                           :name "lem-wayland compositor"))
    (bt2:with-lock-held ((compositor-lifecycle-lock compositor))
      (loop :until (or (compositor-started-p compositor)
                       (compositor-startup-error compositor)
                       (compositor-stopped-p compositor))
            :do (bt2:condition-wait
                 (compositor-lifecycle-condition compositor)
                 (compositor-lifecycle-lock compositor))))
    (when (compositor-startup-error compositor)
      (let ((condition (compositor-startup-error compositor)))
        (wait-for-compositor-thread compositor 5)
        (setf (compositor-thread compositor) nil)
        (error "Could not start the Wayland compositor: ~A" condition)))
    (unless (compositor-started-p compositor)
      (error "The Wayland compositor stopped during startup"))
    compositor))

(defun stop-compositor-thread (compositor)
  "Terminate the compositor event loop and wait for its thread to finish."
  (when compositor
    (stop-compositor compositor)
    (when (compositor-thread compositor)
      (unless (wait-for-compositor-thread compositor 5)
        (format *error-output*
                "~&lem-wayland: the compositor thread did not stop~%")
        (finish-output *error-output*)))
    (setf (compositor-thread compositor) nil)
    compositor))
