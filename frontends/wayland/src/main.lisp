(in-package #:lem-wayland)

(defun main (&rest arguments)
  "Start Lem as a Wayland compositor.

The compositor backend is selected by wlroots' usual environment variables
(WLR_BACKENDS, WAYLAND_DISPLAY, XDG_RUNTIME_DIR, ...).  ARGUMENTS are parsed
like the other Lem frontends; when no interface is given, :WAYLAND is used."
  (let ((arguments (cond ((null arguments)
                          (uiop:command-line-arguments))
                         ((and (= 1 (length arguments))
                               (listp (first arguments)))
                          (first arguments))
                         (t arguments))))
    (let ((args (lem:parse-args arguments)))
      (unless (lem:command-line-arguments-interface args)
        (setf (lem:command-line-arguments-interface args) :wayland))
      (lem:launch args))))
