(in-package #:lem-wayland)

;;;; EXWM-style commands for the Wayland frontend.
;;;
;;;; A client toplevel owns a Lem window, so every command that changes the
;;;; window tree (split, delete, focus, float) is also a window-management
;;;; command for the external Wayland client shown in that window.

(defun wayland-implementation ()
  (lem:implementation))

(defun ensure-lem-editing-window (implementation
                                  &optional (window (lem:current-window)))
  "Return a window that does not display a client, splitting one off if needed."
  (cond
    ((null (client-for-window implementation window))
     window)
    ((lem:floating-window-p window)
     (ensure-lem-editing-window
      implementation
      (or (find-if-not (lambda (candidate)
                         (client-for-window implementation candidate))
                       (lem:window-list))
          (current-tiling-window))))
    (t
     (ensure-tiling-current-window)
     (lem:pop-to-buffer (new-scratch-buffer implementation)
                        :split-action :sensibly))))

(defun split-tiling-window (implementation direction)
  (let ((window (ensure-tiling-current-window)))
    (ecase direction
      (:below (lem:split-window-vertically window))
      (:right (lem:split-window-horizontally window)))
    (let ((new (lem:get-next-window window)))
      (lem:switch-to-window new)
      (lem:switch-to-buffer (new-scratch-buffer implementation))
      (ignore-errors (lem:balance-windows))
      (lem:redraw-display))))

(defun cycle-client (implementation step)
  (let ((clients (client-window-list implementation)))
    (unless clients
      (lem:editor-error "No Wayland clients are visible"))
    (let* ((current (client-for-window implementation (lem:current-window)))
           (position (or (position current clients :test #'eq) -1))
           (next (nth (mod (+ position step) (length clients)) clients)))
      (focus-client-window implementation next))))

;;;; Windows for Lem itself

(lem:define-command wayland-execute-command () ()
  "Run M-x in a Lem window, creating one if the active window is a client."
  (ensure-lem-editing-window (wayland-implementation))
  (lem:execute-command nil))

(lem:define-command wayland-find-file (arg) (:universal)
  "Open a file in a Lem window, creating one if necessary."
  (ensure-lem-editing-window (wayland-implementation))
  (lem:find-file arg))

(lem:define-command wayland-select-buffer (name) ((:other-buffer "Use Buffer: "))
  "Switch a Lem window to NAME, creating one if necessary."
  (ensure-lem-editing-window (wayland-implementation))
  (lem:select-buffer name))

(lem:define-command wayland-kill-buffer (buffer) ((:buffer "Kill buffer: "))
  "Kill BUFFER from a Lem window."
  (ensure-lem-editing-window (wayland-implementation))
  (lem:kill-buffer buffer))

(lem:define-command new-lem-window () ()
  "Split the tiling layout and show a fresh Lem scratch buffer."
  (let ((implementation (wayland-implementation)))
    (ensure-tiling-current-window)
    (lem:pop-to-buffer (new-scratch-buffer implementation)
                       :split-action :sensibly)
    (lem:redraw-display)))

(lem:define-command wayland-split-below () ()
  "Split the active window into two stacked windows."
  (split-tiling-window (wayland-implementation) :below))

(lem:define-command wayland-split-right () ()
  "Split the active window into two side-by-side windows."
  (split-tiling-window (wayland-implementation) :right))

(lem:define-command wayland-delete-active-window () ()
  "Delete the active window without leaving Lem with no window at all."
  (let* ((implementation (wayland-implementation))
         (window (lem:current-window)))
    (cond
      ((lem:floating-window-p window)
       (let ((tiled (current-tiling-window)))
         (unless (eq tiled window)
           (lem:switch-to-window tiled)))
       (lem:delete-window window))
      ((lem:one-window-p)
       (if (client-for-window implementation window)
           (lem:switch-to-buffer (new-scratch-buffer implementation))
           (lem:editor-error "Can not delete the only window")))
      (t
       (lem:delete-window window)
       (ignore-errors (lem:balance-windows))))
    (lem:redraw-display)))

(lem:define-command wayland-delete-other-windows () ()
  "Delete every window except the active tiled one."
  (ensure-tiling-current-window)
  (lem:delete-other-windows)
  (ignore-errors (lem:balance-windows))
  (lem:redraw-display))

;;;; Commands for Wayland clients

(lem:define-command close-active-client () ()
  "Ask the Wayland client displayed in the active window to close."
  (let* ((implementation (wayland-implementation))
         (client (client-for-window implementation (lem:current-window))))
    (unless client
      (lem:editor-error "The active window is not a Wayland client"))
    (lem:message "Closing ~A" (client-display-name client))
    (close-client-surface implementation client)))

(lem:define-command select-next-client () ()
  "Focus the next visible Wayland client window."
  (cycle-client (wayland-implementation) 1))

(lem:define-command select-previous-client () ()
  "Focus the previous visible Wayland client window."
  (cycle-client (wayland-implementation) -1))

(lem:define-command select-wayland-client (buffer)
    ((:buffer "Wayland client: "))
  "Focus the window of the Wayland client displayed by BUFFER."
  (let* ((implementation (wayland-implementation))
         (client (client-for-any-buffer implementation buffer)))
    (unless client
      (lem:editor-error "~A is not a Wayland client buffer"
                        (lem:buffer-name buffer)))
    (focus-client-window implementation client)))

(lem:define-command toggle-active-window-floating () ()
  "Move the active Wayland client between the tiling layout and a float."
  (let* ((implementation (wayland-implementation))
         (window (lem:current-window))
         (client (client-for-window implementation window)))
    (unless client
      (lem:editor-error "The active window is not a Wayland client"))
    (if (lem:floating-window-p window)
        ;; Floating -> tiled: remove the float, then split the layout for it.
        (let ((buffer (client-buffer client)))
          (let ((tiled (current-tiling-window)))
            (unless (eq tiled window)
              (lem:switch-to-window tiled)))
          (ignore-errors (lem:delete-window window))
          (let ((new (lem:pop-to-buffer buffer :split-action :sensibly)))
            (select-mapped-client-window new)
            (setf (client-window client) new
                  (client-floating-p client) nil))
          (ignore-errors (lem:balance-windows)))
        ;; Tiled -> floating: keep the tiled slot alive with a Lem buffer.
        (let* ((buffer (client-buffer client))
               (x (lem:window-x window))
               (y (lem:window-y window))
               (width (lem:window-width window))
               (height (lem:window-height window))
               (floating (lem:make-floating-window
                          :buffer buffer
                          :x x
                          :y y
                          :width (max 10 width)
                          :height (max 3 height)
                          :use-modeline-p t)))
          (setf (lem:floating-window-focusable-p floating) t)
          (lem:switch-to-buffer (new-scratch-buffer implementation))
          (lem:switch-to-window floating)
          (setf (client-window client) floating
                (client-floating-p client) t)))
    (lem:redraw-display)))

(lem:define-command run-wayland-program (command)
    ((:string "Run Wayland program: "))
  "Launch an external program inside this compositor."
  (when (string= command "")
    (lem:editor-error "No command given"))
  (handler-case
      (progn
        (uiop:launch-program (list "/bin/sh" "-c" command))
        (lem:message "Started: ~A" command))
    (error (condition)
      (lem:editor-error "Could not run ~A: ~A" command condition))))
