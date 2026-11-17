(defpackage :lem-darwin/appearance
  (:use :cl)
  #+(and darwin arm64)
  (:import-from :cl-objc
                :invoke)
  (:export :set-window-appearance
           :dispatch-set-window-appearance))
(in-package :lem-darwin/appearance)

;;; The appearance (dark or light title bar and frame) of a Cocoa window.
;;;
;;; Every GUI frontend on macOS draws into an NSWindow, so a frontend
;;; implements LEM-IF:SET-FRAME-COLOR by passing its own NSWindow here.
;;; Taking the window explicitly, rather than [NSApp keyWindow], changes
;;; the right window even when another one has focus, or none does yet.
;;;
;;; cl-objc supports macOS on arm64 only; elsewhere these are no-ops.

#+(and darwin arm64)
(progn

(cffi:define-foreign-library core-foundation
  (:darwin (:framework "CoreFoundation")))

(cffi:use-foreign-library core-foundation)

;; NSObject is toll-free bridged with CFType, so CFRetain and CFRelease
;; keep a window alive while a change to it waits for the main thread.
(cffi:defcfun ("CFRetain" cf-retain) :pointer
  (object :pointer))

(cffi:defcfun ("CFRelease" cf-release) :void
  (object :pointer))

;; libdispatch is plain C, not Objective-C, so it is called through CFFI.
(cffi:defcfun ("dispatch_async_f" dispatch-async-f) :void
  (queue :pointer)
  (context :pointer)
  (work :pointer))

(defun main-dispatch-queue ()
  "Return the main dispatch queue (dispatch_get_main_queue)."
  (cffi:foreign-symbol-pointer "_dispatch_main_q"))

(defun appearance-name (mode)
  "Return the NSAppearance name for MODE (:dark or :light)."
  (ecase mode
    (:dark "NSAppearanceNameDarkAqua")
    (:light "NSAppearanceNameAqua")))

(defun null-window-p (nswindow)
  "Return true if NSWINDOW is NIL or a null pointer."
  (or (null nswindow) (cffi:null-pointer-p nswindow)))

(defun set-window-appearance (nswindow mode)
  "Set the appearance of NSWINDOW, a pointer to an NSWindow, to MODE
(:dark or :light). Must be called on the main thread.
Returns MODE, or NIL if NSWINDOW is null or the change fails."
  (unless (null-window-p nswindow)
    ;; Errors are logged, not signaled: this usually runs in a GCD
    ;; callback on the main thread, where an unhandled Lisp error would
    ;; stop the native event loop.
    (handler-case
        (let ((window (cffi:convert-from-foreign nswindow 'objc-cffi:objc-id))
              (appearance (invoke 'ns-appearance
                                  :appearance-named
                                  (invoke 'ns-string
                                          :string-with-utf8-string
                                          (appearance-name mode)))))
          (unless (objc-cffi:objc-nil-object-p appearance)
            (invoke window :set-appearance appearance)
            mode))
      (error (e)
        (log:error "Failed to set window appearance to ~S: ~A" mode e)
        nil))))

;; dispatch_async_f work functions: void (*)(void *context). The context
;; is the window, retained by DISPATCH-SET-WINDOW-APPEARANCE.
(cffi:defcallback set-dark-appearance-on-main-thread :void ((nswindow :pointer))
  (unwind-protect (set-window-appearance nswindow :dark)
    (cf-release nswindow)))

(cffi:defcallback set-light-appearance-on-main-thread :void ((nswindow :pointer))
  (unwind-protect (set-window-appearance nswindow :light)
    (cf-release nswindow)))

(defun dispatch-set-window-appearance (nswindow mode)
  "Queue a change of NSWINDOW's appearance to MODE (:dark or :light) on
the main thread. Safe to call from any thread; NSWINDOW is retained until
the change runs. Returns MODE, or NIL if NSWINDOW is null."
  (unless (null-window-p nswindow)
    (let ((work (ecase mode
                  (:dark (cffi:callback set-dark-appearance-on-main-thread))
                  (:light (cffi:callback set-light-appearance-on-main-thread)))))
      (cf-retain nswindow)
      (dispatch-async-f (main-dispatch-queue) nswindow work)
      mode)))

) ; end #+(and darwin arm64) progn

#-(and darwin arm64)
(progn
(defun set-window-appearance (nswindow mode)
  "No-op: window appearance is only supported on macOS (arm64)."
  (declare (ignore nswindow mode))
  nil)
(defun dispatch-set-window-appearance (nswindow mode)
  "No-op: window appearance is only supported on macOS (arm64)."
  (declare (ignore nswindow mode))
  nil)
)
