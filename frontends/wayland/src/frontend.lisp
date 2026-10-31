(in-package #:lem-wayland)

;;;; Lem frontend on top of the Lem-owned Wayland compositor.
;;;
;;;; The Wayland side is deliberately small: it provides the socket,
;;;; surfaces, seat input and an output, while every layout decision is
;;;; delegated to Lem's window tree.  A client toplevel becomes an ordinary
;;;; tiled Lem window; dialogs with a parent become floating Lem windows.
;;;;
;;;; Lem draws into Cairo ARGB32 surfaces owned by each Lem view.  The
;;;; editor thread is the only thread that draws; before a frame is handed
;;;; to the compositor the surface is flushed and uploaded through the
;;;; compositor task queue, so the wlroots scene graph is only touched from
;;;; its event-loop thread.

(defparameter *default-font-name* "monospace")
(defparameter *default-font-size* 16)
(defparameter *default-foreground-color* (lem:make-color 221 221 221))
(defparameter *default-background-color* (lem:make-color 30 30 30))

(defparameter *super-keybindings*
  '(("s-x" . lem:execute-command)
    ("s-f" . lem:find-file)
    ("s-b" . lem:select-buffer)
    ("s-k" . lem:kill-buffer)
    ("s-1" . lem:delete-other-windows)
    ("s-2" . lem:split-active-window-vertically)
    ("s-3" . lem:split-active-window-horizontally)
    ("s-0" . lem:delete-active-window)
    ("s-o" . lem:next-window)
    ("s-q" . lem:exit-lem)))

(defclass wayland (lem:implementation)
  ((compositor
    :initform nil
    :accessor wayland-compositor)
   (output-width
    :initform 0
    :accessor wayland-output-width)
   (output-height
    :initform 0
    :accessor wayland-output-height)
   (char-width
    :initform 1
    :accessor wayland-char-width)
   (char-height
    :initform 1
    :accessor wayland-char-height)
   (font-ascent
    :initform 0.0d0
    :accessor wayland-font-ascent)
   (font-descent
    :initform 0.0d0
    :accessor wayland-font-descent)
   (font-name
    :initform *default-font-name*
    :accessor wayland-font-name)
   (font-size
    :initform *default-font-size*
    :accessor wayland-font-size)
   (font-descriptions
    :initform (make-hash-table :test #'equal)
    :accessor wayland-font-descriptions)
   (foreground-color
    :initform *default-foreground-color*
    :accessor wayland-foreground-color)
   (background-color
    :initform *default-background-color*
    :accessor wayland-background-color)
   (cursor-type
    :initform :box
    :accessor wayland-cursor-type)
   (display-title
    :initform "Lem"
    :accessor wayland-display-title)
   (display-fullscreen-p
    :initform nil
    :accessor wayland-display-fullscreen-p)
   (interface-ready-p
    :initform nil
    :accessor wayland-interface-ready-p)
   (views
    :initform (make-hash-table :test #'eql)
    :accessor wayland-views)
   (next-view-id
    :initform 1
    :accessor wayland-next-view-id)
   (clients
    :initform (make-hash-table :test #'eql)
    :accessor wayland-clients)
   (clients-by-buffer
    :initform (make-hash-table :test #'eq)
    :accessor wayland-clients-by-buffer))
  (:default-initargs
   :name :wayland
   :support-floating-window t
   :redraw-after-modifying-floating-window t
   :underline-color-support t
   :window-left-margin 0
   :window-bottom-margin 0))

;;;; Per-view Cairo state and per-client bookkeeping

(defstruct (view (:constructor %make-view))
  id
  window
  (x 0)
  (y 0)
  (columns 1)
  (body-rows 1)
  use-modeline-p
  surface
  data
  stride
  (surface-width 1)
  (surface-height 1)
  cairo
  layout
  (dirty-p nil)
  (cursor-x 0)
  (cursor-y 0))

(defstruct (client (:constructor %make-client))
  id
  buffer
  placeholder
  title
  app-id
  window
  floating-p
  closing-p)

(defun d (x)
  (coerce x 'double-float))

;;;; Small helpers

(cffi:defcfun ("setenv" %setenv) :int
  (name :string)
  (value :string)
  (overwrite :int))

(defun set-environment-variable (name value)
  (%setenv name value 1))

(defun clamp-int (value minimum maximum)
  (max minimum (min maximum value)))

(defun color-rgba (color &optional (alpha 1.0d0))
  (values (d (/ (lem:color-red color) 255))
          (d (/ (lem:color-green color) 255))
          (d (/ (lem:color-blue color) 255))
          (d alpha)))

(defun set-cairo-color (cairo color &optional (alpha 1.0d0))
  (multiple-value-bind (red green blue alpha)
      (color-rgba color alpha)
    (%cairo-set-source-rgba cairo red green blue alpha)))

(defun fill-rect (cairo color x y width height &optional (alpha 1.0d0))
  (when (and (plusp width) (plusp height))
    (set-cairo-color cairo color alpha)
    ;; Start from an empty path: a clip rectangle left over from a previous
    ;; text draw would otherwise be filled again as well.
    (%cairo-new-path cairo)
    (%cairo-rectangle cairo (d x) (d y) (d width) (d height))
    (%cairo-fill cairo)))

(defun draw-horizontal-line (cairo color x y width)
  (fill-rect cairo color x y width 1))

(defun sanitize-client-name (string)
  (let* ((string (or string ""))
         (clean (string-trim
                 '(#\Space #\Tab #\Newline #\Return)
                 (remove-if (lambda (char)
                              (< (char-code char) #x20))
                            string))))
    (if (plusp (length clean))
        (subseq clean 0 (min 80 (length clean)))
        "")))

(defun client-display-name (client)
  (let ((title (sanitize-client-name (client-title client)))
        (app-id (sanitize-client-name (client-app-id client))))
    (cond ((plusp (length title)) title)
          ((plusp (length app-id)) app-id)
          (t (format nil "Wayland client ~D" (client-id client))))))

(defun client-for-buffer (implementation buffer)
  (and buffer
       (gethash buffer (wayland-clients-by-buffer implementation))))

(defun live-buffer-p (buffer)
  (and buffer (not (lem:deleted-buffer-p buffer))))

(defun view-window-live-p (view)
  (let ((window (view-window view)))
    (and window (not (lem:deleted-window-p window)))))

;;;; Fonts

(defun font-description (implementation bold italic)
  (let* ((key (list (not (null bold)) (not (null italic))))
         (table (wayland-font-descriptions implementation))
         (description (gethash key table)))
    (unless description
      (setf description (%pango-font-description-new))
      (%pango-font-description-set-family
       description
       (or (wayland-font-name implementation) "monospace"))
      (%pango-font-description-set-absolute-size
       description
       (d (* (wayland-font-size implementation) +pango-scale+)))
      (%pango-font-description-set-weight
       description
       (if bold +pango-weight-bold+ 400))
      (%pango-font-description-set-style
       description
       (if italic +pango-style-italic+ +pango-style-normal+))
      (setf (gethash key table) description))
    description))

(defun clear-font-descriptions (implementation)
  (let ((table (wayland-font-descriptions implementation)))
    (maphash (lambda (key description)
               (declare (ignore key))
               (when description
                 (%pango-font-description-free description)))
             table)
    (clrhash table)))

(defun measure-font (implementation)
  (let* ((description (font-description implementation nil nil))
         (surface (%cairo-image-surface-create +cairo-format-argb32+ 1 1))
         (cairo (%cairo-create surface))
         (layout (%pango-cairo-create-layout cairo)))
    (unless (or (cffi:null-pointer-p surface)
                (cffi:null-pointer-p cairo)
                (cffi:null-pointer-p layout))
      (unwind-protect
           (progn
             (%pango-layout-set-font-description layout description)
             (%pango-layout-set-text layout "M" -1)
             (cffi:with-foreign-objects ((width :int)
                                         (height :int))
               (%pango-layout-get-pixel-size layout width height)
               (setf (wayland-char-width implementation)
                     (max 1 (cffi:mem-ref width :int))))
             (let ((metrics (%pango-context-get-metrics
                             (%pango-layout-get-context layout)
                             description
                             (cffi:null-pointer))))
               (when (and metrics (not (cffi:null-pointer-p metrics)))
                 (unwind-protect
                      (progn
                        (setf (wayland-font-ascent implementation)
                              (/ (d (%pango-font-metrics-get-ascent metrics))
                                 +pango-scale+)
                              (wayland-font-descent implementation)
                              (/ (d (%pango-font-metrics-get-descent metrics))
                                 +pango-scale+)))
                   (%pango-font-metrics-unref metrics)))))
        (when (and layout (not (cffi:null-pointer-p layout)))
          (%g-object-unref layout))
        (when (and cairo (not (cffi:null-pointer-p cairo)))
          (%cairo-destroy cairo))
        (when (and surface (not (cffi:null-pointer-p surface)))
          (%cairo-surface-destroy surface)))))
  (let ((ascent (wayland-font-ascent implementation))
        (descent (wayland-font-descent implementation)))
    (when (or (zerop ascent) (zerop descent))
      (setf ascent 0.8d0
            descent 0.2d0))
    (setf (wayland-font-ascent implementation) ascent
          (wayland-font-descent implementation) descent
          (wayland-char-height implementation)
          (max 1 (ceiling (+ ascent descent))))
    (values)))

;;;; Views and their Cairo surfaces

(defun view-total-rows (view)
  (+ (view-body-rows view) (if (view-use-modeline-p view) 1 0)))

(defun view-pixel-width (implementation view)
  (* (max 1 (view-columns view))
     (wayland-char-width implementation)))

(defun view-pixel-height (implementation view)
  (* (max 1 (view-total-rows view))
     (wayland-char-height implementation)))

(defun view-body-pixel-height (implementation view)
  (* (max 1 (view-body-rows view))
     (wayland-char-height implementation)))

(defun destroy-view-surface (view)
  (when (view-layout view)
    (%g-object-unref (view-layout view)))
  (when (view-cairo view)
    (%cairo-destroy (view-cairo view)))
  (when (view-surface view)
    (%cairo-surface-destroy (view-surface view)))
  (setf (view-layout view) nil
        (view-cairo view) nil
        (view-surface view) nil
        (view-data view) nil
        (view-stride view) 0))

(defun paint-view-background (implementation view)
  (let ((cairo (view-cairo view)))
    (when cairo
      (set-cairo-color cairo (wayland-background-color implementation))
      (%cairo-paint cairo))))

(defun reset-view-surface (implementation view)
  (destroy-view-surface view)
  (let ((width (view-pixel-width implementation view))
        (height (view-pixel-height implementation view)))
    (multiple-value-bind (surface data stride)
        (cairo-argb32-image width height)
      (setf (view-surface view) surface
            (view-data view) data
            (view-stride view) stride
            (view-surface-width view) width
            (view-surface-height view) height
            (view-cairo view) (%cairo-create surface))
      (unless (cffi:null-pointer-p (view-cairo view))
        (setf (view-layout view)
              (%pango-cairo-create-layout (view-cairo view))))
      (paint-view-background implementation view)
      (setf (view-dirty-p view) t))))

(defun ensure-view-surface (implementation view)
  (unless (view-surface view)
    (reset-view-surface implementation view)))

(defun clear-view (implementation view)
  (ensure-view-surface implementation view)
  (when (view-cairo view)
    (paint-view-background implementation view)
    (setf (view-dirty-p view) t)))

(defun refresh-view-surfaces (implementation)
  (maphash (lambda (id view)
             (declare (ignore id))
             (reset-view-surface implementation view))
           (wayland-views implementation)))

(defun reset-interface-font (implementation)
  (clear-font-descriptions implementation)
  (measure-font implementation))

(defun refresh-interface-after-font-change (implementation)
  (reset-interface-font implementation)
  (refresh-view-surfaces implementation)
  (when (and (wayland-interface-ready-p implementation)
             (lem:current-frame))
    (lem:update-on-display-resized)))

;;;; Drawing objects

(defun glyph-color-for-cell (implementation attribute)
  (if (and attribute (lem:cursor-attribute-p attribute))
      (if (eq (wayland-cursor-type implementation) :box)
          (lem:attribute-background-color nil)
          (lem:attribute-foreground-with-reverse attribute))
      (lem:attribute-foreground-with-reverse attribute)))

(defun underline-color-for-cell (implementation attribute foreground)
  (declare (ignore implementation))
  (let ((underline (and attribute (lem:attribute-underline attribute))))
    (cond ((null underline) nil)
          ((eq underline t) foreground)
          ((stringp underline)
           (or (lem:parse-color underline) foreground))
          (t underline))))

(defun draw-pango-string (implementation view string attribute
                          x baseline &key (alpha 1.0d0) clip)
  (let ((cairo (view-cairo view))
        (layout (view-layout view)))
    (when (and cairo layout string (plusp (length string)))
      (let ((description
              (font-description implementation
                                 (and attribute
                                      (lem:attribute-bold attribute))
                                 (and attribute
                                      (lem:attribute-italic attribute)))))
        (%pango-layout-set-text layout string -1)
        (%pango-layout-set-font-description layout description)
        ;; Sync the layout with the Cairo context before positioning the text:
        ;; the move below must be the current point seen by show-layout.
        (%pango-cairo-update-layout cairo layout)
        (when clip
          (%cairo-save cairo)
          (destructuring-bind (clip-x clip-y clip-width clip-height) clip
            (%cairo-rectangle cairo (d clip-x) (d clip-y)
                              (d clip-width) (d clip-height))
            (%cairo-clip cairo)
            ;; The rectangle must not stay in the path and get filled by a
            ;; later fill-rect.
            (%cairo-new-path cairo)))
        (unwind-protect
             (progn
               (set-cairo-color cairo
                                (glyph-color-for-cell implementation attribute)
                                alpha)
               (%cairo-move-to cairo (d x) (d baseline))
               (%pango-cairo-show-layout cairo layout))
          (when clip
            (%cairo-restore cairo)))))))

(defun draw-cell (implementation view string attribute x top width height)
  (let* ((cairo (view-cairo view))
         (cursor-p (and attribute (lem:cursor-attribute-p attribute)))
         (cursor-type (wayland-cursor-type implementation))
         (background (lem:attribute-background-with-reverse attribute))
         (foreground (glyph-color-for-cell implementation attribute))
         (baseline (- (+ top height)
                      (wayland-font-descent implementation)))
         (alpha (if (and attribute (lem:attribute-dim attribute))
                    0.55d0
                    1.0d0))
         (draw-glyph-p t))
    ;; Cell background and the cursor shape.
    (cond
      ((and cursor-p (eq cursor-type :box))
       (fill-rect cairo background x top width height))
      (cursor-p
       ;; Bar and underline cursors leave the cell background alone.
       (ecase cursor-type
         (:bar
          (fill-rect cairo background x top 1 height))
         (:underline
          (fill-rect cairo background x (- (+ top height) 1) width 1))))
      (t
       (fill-rect cairo background x top width height)))
    ;; A box cursor draws its glyph in the editor background colour so that
    ;; the shape stays visible (the SDL2 frontend does the same).
    (when draw-glyph-p
      (draw-pango-string implementation view string attribute
                         x baseline
                         :alpha alpha
                         :clip (list x top width height)))
    (let ((underline-color
            (underline-color-for-cell implementation attribute foreground)))
      (when underline-color
        (draw-horizontal-line cairo underline-color
                              x (- (+ top height) 1) width)))
    (when (and attribute (lem:attribute-strikethrough attribute))
      (draw-horizontal-line cairo foreground
                            x (+ top (floor height 2)) width))
    (when cursor-p
      (setf (view-cursor-x view) x
            (view-cursor-y view) top))))

(defun draw-atomic-object (implementation view string attribute
                           x top width height)
  (let ((cairo (view-cairo view))
        (cursor-p (and attribute (lem:cursor-attribute-p attribute)))
        (background (lem:attribute-background-with-reverse attribute))
        (foreground (glyph-color-for-cell implementation attribute))
        (baseline (- (+ top height)
                     (wayland-font-descent implementation)))
        (alpha (if (and attribute (lem:attribute-dim attribute))
                   0.55d0
                   1.0d0)))
    (if (and cursor-p
             (not (eq (wayland-cursor-type implementation) :box)))
        (ecase (wayland-cursor-type implementation)
          (:bar
           (fill-rect cairo background x top 1 height))
          (:underline
           (fill-rect cairo background x (- (+ top height) 1) width 1)))
        (fill-rect cairo background x top width height))
    (draw-pango-string implementation view string attribute
                       x baseline
                       :alpha alpha
                       :clip (list x top width height))
    (let ((underline-color
            (underline-color-for-cell implementation attribute foreground)))
      (when underline-color
        (draw-horizontal-line cairo underline-color
                              x (- (+ top height) 1) width)))
    (when (and attribute (lem:attribute-strikethrough attribute))
      (draw-horizontal-line cairo foreground
                            x (+ top (floor height 2)) width))
    (when cursor-p
      (setf (view-cursor-x view) x
            (view-cursor-y view) top))))

(defun draw-drawing-object (implementation view object x bottom-y)
  (let* ((cairo (view-cairo view))
         (char-width (wayland-char-width implementation))
         (char-height (wayland-char-height implementation))
         (object-width (lem-if:object-width implementation object))
         (object-height (lem-if:object-height implementation object))
         (top (- bottom-y (max 1 object-height))))
    (typecase object
      (void-object
       nil)
      (eol-cursor-object
       (let ((color (or (eol-cursor-object-color object)
                        (wayland-background-color implementation)))
             (cursor-type (wayland-cursor-type implementation)))
         (setf (view-cursor-x view) x
               (view-cursor-y view) top)
         (ecase cursor-type
           (:box
            (fill-rect cairo color x top char-width char-height))
           (:bar
            (fill-rect cairo color x top 1 char-height))
           (:underline
            (fill-rect cairo color x
                        (- (+ top char-height) 1)
                        char-width 1)))))
      (extend-to-eol-object
       (let ((color (or (extend-to-eol-object-color object)
                        (wayland-background-color implementation)))
             (width (- (lem-if:view-width implementation view) x)))
         (when (plusp width)
           (fill-rect cairo color x top width char-height))))
      (image-object
       ;; Images are not decoded by this frontend yet; leave the line
       ;; background in place and keep the correct advance.
       nil)
      ((or control-character-object icon-object folder-object emoji-object)
       (draw-atomic-object implementation view
                            (text-object-string object)
                            (text-object-attribute object)
                            x top (max 1 object-width) object-height))
      (line-end-object
       (let ((offset (line-end-object-offset object)))
         (draw-atomic-object implementation view
                              (text-object-string object)
                              (text-object-attribute object)
                              (+ x (* offset char-width))
                              top
                              (max 1 object-width)
                              object-height)))
      (text-object
       (let ((string (text-object-string object))
             (attribute (text-object-attribute object))
             (cell-x x))
         (loop :for char :across string
               :while (< cell-x (lem-if:view-width implementation view))
               :for cell-width := (max 0
                                       (* (lem:string-width (string char))
                                          char-width))
               :do (when (plusp cell-width)
                     (draw-cell implementation view (string char) attribute
                                cell-x top cell-width object-height))
                   (incf cell-x cell-width))))
      (t nil))))

;;;; Lem interface: display properties

(defmethod lem-if:display-width ((implementation wayland))
  (max 1 (floor (wayland-output-width implementation)
                (max 1 (wayland-char-width implementation)))))

(defmethod lem-if:display-height ((implementation wayland))
  (max 1 (floor (wayland-output-height implementation)
                (max 1 (wayland-char-height implementation)))))

(defmethod lem-if:display-title ((implementation wayland))
  (wayland-display-title implementation))

(defmethod lem-if:set-display-title ((implementation wayland) title)
  (setf (wayland-display-title implementation) title))

(defmethod lem-if:display-fullscreen-p ((implementation wayland))
  (wayland-display-fullscreen-p implementation))

(defmethod lem-if:set-display-fullscreen-p ((implementation wayland) fullscreen-p)
  (setf (wayland-display-fullscreen-p implementation) fullscreen-p))

(defmethod lem-if:get-background-color ((implementation wayland))
  (or (let ((color lem-if:*background-color-of-drawing-window*))
        (cond ((null color) nil)
              ((stringp color) (lem:parse-color color))
              (t color)))
      (wayland-background-color implementation)))

(defmethod lem-if:get-foreground-color ((implementation wayland))
  (wayland-foreground-color implementation))

(defmethod lem-if:update-foreground ((implementation wayland) color-name)
  (let ((color (and (stringp color-name) (lem:parse-color color-name))))
    (when color
      (setf (wayland-foreground-color implementation) color))))

(defmethod lem-if:update-background ((implementation wayland) color-name)
  (let ((color (and (stringp color-name) (lem:parse-color color-name))))
    (when color
      (setf (wayland-background-color implementation) color))))

(defmethod lem-if:update-cursor-shape ((implementation wayland) cursor-type)
  (setf (wayland-cursor-type implementation) cursor-type))

(defmethod lem-if:get-char-width ((implementation wayland))
  (wayland-char-width implementation))

(defmethod lem-if:get-char-height ((implementation wayland))
  (wayland-char-height implementation))

;;;; Lem interface: views

(defmethod lem-if:make-view ((implementation wayland)
                             window x y width height use-modeline)
  (let* ((id (prog1 (wayland-next-view-id implementation)
               (incf (wayland-next-view-id implementation))))
         (view (%make-view
                :id id
                :window window
                :x x
                :y y
                :columns (max 1 width)
                :body-rows (max 1 height)
                :use-modeline-p use-modeline)))
    (setf (gethash id (wayland-views implementation)) view)
    (reset-view-surface implementation view)
    view))

(defmethod lem-if:delete-view ((implementation wayland) view)
  (when (gethash (view-id view) (wayland-views implementation))
    (remhash (view-id view) (wayland-views implementation))
    (destroy-view-surface view)
    (let ((compositor (wayland-compositor implementation)))
      (when (and compositor
                 (not (compositor-stopped-p compositor)))
        (call-on-task-queue
         (compositor-task-queue compositor)
         (lambda ()
           (delete-view-scene compositor (view-id view))))))))

(defmethod lem-if:clear ((implementation wayland) view)
  (clear-view implementation view))

(defmethod lem-if:set-view-size ((implementation wayland) view width height)
  (setf (view-columns view) (max 1 width)
        (view-body-rows view) (max 1 height))
  (unless (and (= (view-surface-width view)
                  (view-pixel-width implementation view))
               (= (view-surface-height view)
                  (view-pixel-height implementation view)))
    (reset-view-surface implementation view))
  (setf (view-dirty-p view) t))

(defmethod lem-if:set-view-pos ((implementation wayland) view x y)
  (declare (ignore implementation))
  (setf (view-x view) x
        (view-y view) y))

(defmethod lem-if:view-width ((implementation wayland) view)
  (view-pixel-width implementation view))

(defmethod lem-if:view-height ((implementation wayland) view)
  (view-pixel-height implementation view))

(defmethod lem-if:render-line ((implementation wayland)
                               view x y objects height)
  (ensure-view-surface implementation view)
  (when (view-cairo view)
    (let* ((view-width (lem-if:view-width implementation view))
           (background (lem:attribute-background-color nil))
           (bottom-y (+ y height))
           (current-x x))
      (fill-rect (view-cairo view) background
                 x y (- view-width x) height)
      (dolist (object objects)
        (when (>= current-x view-width)
          (return))
        (draw-drawing-object implementation view object current-x bottom-y)
        (incf current-x (max 0 (lem-if:object-width implementation object))))
      (setf (view-dirty-p view) t))))

(defmethod lem-if:render-line-on-modeline ((implementation wayland)
                                           view
                                           left-objects
                                           right-objects
                                           default-attribute
                                           height)
  (ensure-view-surface implementation view)
  (when (view-cairo view)
    (let* ((view-width (lem-if:view-width implementation view))
           (view-height (lem-if:view-height implementation view))
           (char-height (wayland-char-height implementation))
           (background (lem:attribute-background-color default-attribute))
           (left-bottom-y (+ (- view-height char-height) height))
           (right-bottom-y view-height)
           (current-x 0))
      (fill-rect (view-cairo view) background
                 0 (- view-height height) view-width height)
      (dolist (object left-objects)
        (when (>= current-x view-width)
          (return))
        (draw-drawing-object implementation view object
                             current-x left-bottom-y)
        (incf current-x (max 0 (lem-if:object-width implementation object))))
      (setf current-x view-width)
      (dolist (object right-objects)
        (decf current-x (max 0 (lem-if:object-width implementation object)))
        (draw-drawing-object implementation view object
                             current-x right-bottom-y))
      (setf (view-dirty-p view) t))))

(defmethod lem-if:clear-to-end-of-window ((implementation wayland) view y)
  (ensure-view-surface implementation view)
  (when (view-cairo view)
    (fill-rect (view-cairo view)
               (lem:attribute-background-color nil)
               0 y
               (lem-if:view-width implementation view)
               (- (lem-if:view-height implementation view) y))
    (setf (view-dirty-p view) t)))

(defmethod lem-if:object-width ((implementation wayland) object)
  (let ((char-width (wayland-char-width implementation)))
    (typecase object
      (void-object 0)
      (control-character-object (* 2 char-width))
      (icon-object
       (* (lem:string-width (text-object-string object)) char-width))
      (folder-object (* 2 char-width))
      (emoji-object
       (* char-width 2 (length (text-object-string object))))
      (eol-cursor-object 0)
      (extend-to-eol-object 0)
      (line-end-object
       (* (lem:string-width (text-object-string object)) char-width))
      (text-object
       (* (lem:string-width (text-object-string object)) char-width))
      (image-object
       (or (image-object-width object) (* 2 char-width)))
      (t 0))))

(defmethod lem-if:object-height ((implementation wayland) object)
  (typecase object
    (image-object
     (or (image-object-height object)
         (wayland-char-height implementation)))
    (t
     (wayland-char-height implementation))))

;;;; Lem interface: fonts

(defmethod lem-if:set-font-name ((implementation wayland) font-name)
  (setf (wayland-font-name implementation) font-name))

(defmethod lem-if:set-font-size ((implementation wayland) size)
  (setf (wayland-font-size implementation) size))

(defmethod lem-if:update-screen-size ((implementation wayland))
  (refresh-interface-after-font-change implementation))

(defmethod lem-if:increase-font-size ((implementation wayland))
  (setf (wayland-font-size implementation)
        (max 6 (1+ (wayland-font-size implementation))))
  (refresh-interface-after-font-change implementation))

(defmethod lem-if:decrease-font-size ((implementation wayland))
  (setf (wayland-font-size implementation)
        (max 6 (1- (wayland-font-size implementation))))
  (refresh-interface-after-font-change implementation))

(defmethod lem-if:get-font ((implementation wayland))
  (values (wayland-font-name implementation)
          (wayland-font-size implementation)))

(defmethod lem-if:get-font-list ((implementation wayland))
  (declare (ignore implementation))
  '())

;;;; Lem interface: frame operations that have no native window

(defmethod lem-if:set-frame-color ((implementation wayland) mode)
  (declare (ignore implementation mode))
  nil)

;;;; Frame construction

(defun frame-items-for-window (implementation window items)
  (let* ((view (lem:window-view window))
         (char-width (wayland-char-width implementation))
         (char-height (wayland-char-height implementation)))
    (when (and view (view-window-live-p view))
      (push (make-view-frame
             :id (view-id view)
             :x (* (view-x view) char-width)
             :y (* (view-y view) char-height))
            items)
      (let ((client (client-for-buffer implementation (lem:window-buffer window))))
        (when (and client
                   (live-buffer-p (lem:window-buffer window))
                   (not (client-closing-p client)))
          (push (make-client-frame
                 :id (client-id client)
                 :x (* (view-x view) char-width)
                 :y (* (view-y view) char-height)
                 :width (lem-if:view-width implementation view)
                 :height (view-body-pixel-height implementation view)
                 :floating-p (lem:floating-window-p window)
                 :visible-p t)
                items))))))

(defun build-frame-items (implementation)
  (let* ((frame (lem:current-frame))
         (current (lem:current-window))
         (tiled (and frame (lem:window-list frame)))
         (floating (and frame (lem:frame-floating-windows frame)))
         (headers (and frame (lem:frame-header-windows frame)))
         (items '()))
    (dolist (window headers)
      (frame-items-for-window implementation window items))
    (dolist (window tiled)
      (unless (eq window current)
        (frame-items-for-window implementation window items)))
    (when (and current (member current tiled :test #'eq))
      (frame-items-for-window implementation current items))
    (dolist (window floating)
      (frame-items-for-window implementation window items))
    (nreverse items)))

(defun dump-view-snapshot (view directory)
  "Write VIEW's Cairo surface to an image in DIRECTORY (debugging aid)."
  (let ((path (format nil "~A/lem-view-~D.png" directory (view-id view))))
    (ignore-errors (%cairo-surface-write-to-png (view-surface view) path))))

(defun collect-dirty-views (implementation)
  (let ((uploads '())
        (dump-directory (uiop:getenv "LEM_WAYLAND_DUMP")))
    (maphash
     (lambda (id view)
       (declare (ignore id))
       (when (and (view-dirty-p view)
                  (view-surface view))
         (%cairo-surface-flush (view-surface view))
         (when dump-directory
           (dump-view-snapshot view dump-directory))
         (push view uploads)))
     (wayland-views implementation))
    (nreverse uploads)))

(defun current-focused-client-id (implementation)
  (let ((client (client-for-buffer implementation
                                   (lem:window-buffer
                                    (lem:current-window)))))
    (and client (not (client-closing-p client))
         (client-id client))))

(defmethod lem-if:update-display ((implementation wayland))
  (let ((compositor (wayland-compositor implementation)))
    (when (and compositor
               (not (compositor-stopped-p compositor)))
      (let ((uploads (collect-dirty-views implementation))
            (items (build-frame-items implementation))
            (focused-client-id (current-focused-client-id implementation)))
        (call-on-task-queue
         (compositor-task-queue compositor)
         (lambda ()
           (dolist (view uploads)
             (update-view-image compositor
                                (view-id view)
                                (view-surface-width view)
                                (view-surface-height view)
                                (view-data view)
                                (view-stride view)))
           (sync-frame compositor items focused-client-id)))
        (dolist (view uploads)
          (setf (view-dirty-p view) nil))))))

(defmethod lem-if:clipboard-copy ((implementation wayland) text)
  (let ((compositor (wayland-compositor implementation)))
    (when (and compositor
               (not (compositor-stopped-p compositor)))
      (call-on-task-queue
       (compositor-task-queue compositor)
       (lambda () (set-seat-clipboard-text compositor text))))))

(defmethod lem-if:clipboard-paste ((implementation wayland))
  (let ((compositor (wayland-compositor implementation)))
    (when (and compositor
               (not (compositor-stopped-p compositor)))
      (call-on-task-queue
       (compositor-task-queue compositor)
       (lambda () (seat-clipboard-text compositor))))))

;;;; Duplicate client buffers and placeholders

(defun client-placeholder-buffer (implementation client)
  (declare (ignore implementation))
  (let ((placeholder (client-placeholder client)))
    (cond ((live-buffer-p placeholder) placeholder)
          (t
           (let ((buffer (lem:make-buffer
                          (format nil " *wayland-client-~D*" (client-id client))
                          :temporary t
                          :enable-undo-p nil)))
             (lem:insert-string
              (lem:buffer-point buffer)
              "This Wayland client is displayed in another window.\n")
             (lem:buffer-start (lem:buffer-point buffer))
             (setf (lem:buffer-read-only-p buffer) t)
             (setf (client-placeholder client) buffer)
             buffer)))))

(defun normalize-client-buffers (implementation)
  (maphash
   (lambda (id client)
     (declare (ignore id))
     (let ((buffer (client-buffer client)))
       (when (and (live-buffer-p buffer)
                  (not (client-closing-p client)))
         (let ((windows
                 (remove-duplicates
                  (lem:get-buffer-windows buffer :include-floating-windows t)
                  :test #'eq))
               (current (lem:current-window)))
           (when (< 1 (length windows))
             (let* ((keeper (or (find current windows :test #'eq)
                                (first windows)))
                    (placeholder (client-placeholder-buffer
                                  implementation client)))
               (dolist (window windows)
                 (unless (eq window keeper)
                   (when (and (not (lem:deleted-window-p window))
                              (not (lem:deleted-buffer-p placeholder)))
                     (lem:with-current-window window
                       (lem:switch-to-buffer placeholder)))))))))))
   (wayland-clients implementation)))

(defmethod lem-if:will-update-display ((implementation wayland))
  (normalize-client-buffers implementation))

;;;; Output changes

(defun handle-output-changed (implementation width height)
  (when (and (plusp width) (plusp height))
    (unless (and (= width (wayland-output-width implementation))
                 (= height (wayland-output-height implementation)))
      (setf (wayland-output-width implementation) width
            (wayland-output-height implementation) height)
      (when (and (wayland-interface-ready-p implementation)
                 (lem:current-frame))
        (lem:update-on-display-resized)))))

(defun wait-for-output (implementation timeout)
  (let ((compositor (wayland-compositor implementation))
        (deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (let ((width (and compositor
                        (compositor-output-width compositor)))
            (height (and compositor
                         (compositor-output-height compositor))))
        (when (and width height (plusp width) (plusp height))
          (return (values width height)))
        (when (>= (get-internal-real-time) deadline)
          (return (values 0 0)))
        (sleep 0.02)))))

;;;; Client surfaces as Lem buffers and windows

(defun populate-client-buffer (client buffer)
  (lem:insert-string
   (lem:buffer-point buffer)
   (format nil "Wayland client #~D~%title: ~A~%app-id: ~A~%~
               ~%This buffer stands for a Wayland client. Killing it asks ~
               the client to close.~%"
           (client-id client)
           (or (client-title client) "")
           (or (client-app-id client) "")))
  (lem:buffer-start (lem:buffer-point buffer))
  (setf (lem:buffer-read-only-p buffer) t)
  buffer)

(defun close-client-surface (implementation client)
  (let ((compositor (wayland-compositor implementation)))
    (when (and compositor
               (not (compositor-stopped-p compositor)))
      (handler-case
          (call-on-task-queue
           (compositor-task-queue compositor)
           (lambda ()
             (close-toplevel compositor (client-id client))))
        (error (condition)
          (format *error-output*
                  "~&lem-wayland: could not close client ~D: ~A~%"
                  (client-id client) condition)
          (finish-output *error-output*))))))

(defun handle-client-buffer-killed (implementation buffer)
  (let ((client (gethash buffer (wayland-clients-by-buffer implementation))))
    (when (and client (not (client-closing-p client)))
      (setf (client-closing-p client) t
            (client-buffer client) nil)
      (remhash buffer (wayland-clients-by-buffer implementation))
      (close-client-surface implementation client))))

(defun select-mapped-client-window (window)
  ;; Never steal focus from an active prompt (minibuffer).
  (unless (lem:active-prompt-window)
    (lem:switch-to-window window)))

(defun map-client-tiled (implementation client buffer)
  (declare (ignore implementation))
  (let ((window (lem:pop-to-buffer buffer)))
    (select-mapped-client-window window)
    (setf (client-window client) window)))

(defun map-client-floating (implementation client buffer)
  (let* ((columns (lem-if:display-width implementation))
         (rows (lem-if:display-height implementation))
         (width (clamp-int (min 60 (max 1 (- columns 4))) 1 columns))
         (height (clamp-int (min 20 (max 1 (- rows 4))) 1 rows))
         (x (max 0 (floor (- columns width) 2)))
         (y (max 0 (floor (- rows height) 2)))
         (window (lem:make-floating-window
                  :buffer buffer
                  :x x
                  :y y
                  :width width
                  :height height
                  :use-modeline-p t)))
    (setf (lem:floating-window-focusable-p window) t)
    (select-mapped-client-window window)
    (setf (client-window client) window)))

(defun handle-toplevel-mapped (implementation id title app-id parent-id floating-p)
  (declare (ignore parent-id))
  (unless (gethash id (wayland-clients implementation))
    (let* ((client (%make-client
                    :id id
                    :title (or title "")
                    :app-id (or app-id "")
                    :floating-p floating-p))
           (buffer (populate-client-buffer
                    client
                    (lem:make-buffer
                     (lem:unique-buffer-name (client-display-name client))
                     :enable-undo-p nil))))
      (setf (client-buffer client) buffer)
      (setf (gethash id (wayland-clients implementation)) client)
      (setf (gethash buffer (wayland-clients-by-buffer implementation)) client)
      (lem:add-hook
       (lem:variable-value 'lem:kill-buffer-hook :buffer buffer)
       (lambda (buffer)
         (handle-client-buffer-killed implementation buffer)))
      (if floating-p
          (map-client-floating implementation client buffer)
          (map-client-tiled implementation client buffer))
      (lem:redraw-display))))

(defun switch-to-tiled-window-if-possible ()
  (when (lem:floating-window-p (lem:current-window))
    (let ((window (find-if-not #'lem:deleted-window-p
                               (lem:window-list))))
      (when window
        (lem:switch-to-window window)))))

(defun handle-toplevel-destroyed (implementation id)
  (let ((client (gethash id (wayland-clients implementation))))
    (when client
      (remhash id (wayland-clients implementation))
      (setf (client-closing-p client) t)
      (let ((buffer (client-buffer client))
            (placeholder (client-placeholder client))
            (client-window (client-window client)))
        (when (live-buffer-p buffer)
          (remhash buffer (wayland-clients-by-buffer implementation)))
        (let ((windows
                (remove-duplicates
                 (append
                  (when (live-buffer-p buffer)
                    (lem:get-buffer-windows buffer
                                            :include-floating-windows t))
                  (when (live-buffer-p placeholder)
                    (lem:get-buffer-windows placeholder
                                            :include-floating-windows t))
                  (when (and client-window
                             (not (lem:deleted-window-p client-window)))
                    (list client-window)))
                 :test #'eq)))
          ;; Floating windows belonging to the client disappear with it.
          (dolist (window windows)
            (when (and (not (lem:deleted-window-p window))
                       (lem:floating-window-p window))
              (when (eq window (lem:current-window))
                (switch-to-tiled-window-if-possible))
              (ignore-errors (lem:delete-window window)))))
        (when (live-buffer-p placeholder)
          (lem:delete-buffer placeholder))
        (when (live-buffer-p buffer)
          (lem:delete-buffer buffer))
        (lem:redraw-display)))))

(defun handle-toplevel-title-changed (implementation id title app-id)
  (let ((client (gethash id (wayland-clients implementation))))
    (when client
      (setf (client-title client) (or title "")
            (client-app-id client) (or app-id ""))
      (let ((buffer (client-buffer client)))
        (when (and (live-buffer-p buffer)
                   (not (client-closing-p client)))
          ;; Renaming to the bare display name keeps this idempotent: if the
          ;; title did not change, the names already match.  If another
          ;; buffer owns the name, leave the current one alone.
          (let ((name (client-display-name client)))
            (unless (string= name (lem:buffer-name buffer))
              (handler-case
                  (lem:buffer-rename buffer name)
                (error (condition)
                  (declare (ignore condition))))))
          (lem:redraw-display))))))

;;;; Focus from the compositor

(defun handle-focus-view (implementation id)
  (let ((view (gethash id (wayland-views implementation))))
    (when (and view (view-window-live-p view))
      (unless (eq (view-window view) (lem:current-window))
        (lem:switch-to-window (view-window view)))
      (lem:redraw-display))))

(defun handle-focus-client (implementation id)
  (let* ((client (gethash id (wayland-clients implementation)))
         (buffer (and client (client-buffer client))))
    (when (live-buffer-p buffer)
      (let ((windows
              (remove-duplicates
               (lem:get-buffer-windows buffer :include-floating-windows t)
               :test #'eq)))
        (when windows
          (let ((window (or (find (lem:current-window) windows :test #'eq)
                            (first windows))))
            (unless (eq window (lem:current-window))
              (lem:switch-to-window window))
            (lem:redraw-display)))))))

;;;; Callbacks and lifecycle

(defun install-frontend-callbacks (implementation)
  (setf *toplevel-mapped-function*
        (lambda (id title app-id parent-id floating-p)
          (lem:send-event
           (lambda ()
             (handle-toplevel-mapped implementation id title app-id
                                     parent-id floating-p))))
        *toplevel-destroyed-function*
        (lambda (id)
          (lem:send-event
           (lambda ()
             (handle-toplevel-destroyed implementation id))))
        *toplevel-title-changed-function*
        (lambda (id title app-id)
          (lem:send-event
           (lambda ()
             (handle-toplevel-title-changed implementation id title app-id))))
        *focus-view-function*
        (lambda (id)
          (lem:send-event
           (lambda ()
             (handle-focus-view implementation id))))
        *focus-client-function*
        (lambda (id)
          (lem:send-event
           (lambda ()
             (handle-focus-client implementation id))))
        *output-changed-function*
        (lambda (width height)
          (lem:send-event
           (lambda ()
             (handle-output-changed implementation width height))))))

(defun uninstall-frontend-callbacks ()
  (setf *toplevel-mapped-function* nil
        *toplevel-destroyed-function* nil
        *toplevel-title-changed-function* nil
        *focus-view-function* nil
        *focus-client-function* nil
        *output-changed-function* nil))

(defun install-super-keybindings ()
  (dolist (binding *super-keybindings*)
    (lem:define-key lem:*global-keymap*
      (car binding)
      (cdr binding))))

(defun initialize-interface (implementation)
  (multiple-value-bind (width height)
      (wait-for-output implementation 2)
    (when (and (plusp width) (plusp height))
      (setf (wayland-output-width implementation) width
            (wayland-output-height implementation) height)))
  (when (or (zerop (wayland-output-width implementation))
            (zerop (wayland-output-height implementation)))
    (setf (wayland-output-width implementation) 1280
          (wayland-output-height implementation) 720))
  (reset-interface-font implementation)
  (let* ((compositor (wayland-compositor implementation))
         (socket-name (and compositor (compositor-socket-name compositor))))
    (when socket-name
      (set-environment-variable "WAYLAND_DISPLAY" socket-name))))

(defun destroy-all-views (implementation)
  (maphash (lambda (id view)
             (declare (ignore id))
             (destroy-view-surface view))
           (wayland-views implementation))
  (clrhash (wayland-views implementation)))

(defmethod lem-if:invoke ((implementation wayland) function)
  (install-frontend-callbacks implementation)
  (unwind-protect
       (progn
         (setf (wayland-compositor implementation) (launch-compositor))
         (initialize-interface implementation)
         (install-super-keybindings)
         (let ((editor-thread (funcall function)))
           (setf (wayland-interface-ready-p implementation) t)
           (when (and editor-thread
                      (bt2:thread-alive-p editor-thread))
             (bt2:join-thread editor-thread))))
    (setf (wayland-interface-ready-p implementation) nil)
    (uninstall-frontend-callbacks)
    (let ((compositor (wayland-compositor implementation)))
      (setf (wayland-compositor implementation) nil)
      (when compositor
        (stop-compositor-thread compositor)))
    (destroy-all-views implementation)
    (clrhash (wayland-clients implementation))
    (clrhash (wayland-clients-by-buffer implementation))
    (when (wayland-font-descriptions implementation)
      (clear-font-descriptions implementation))))
