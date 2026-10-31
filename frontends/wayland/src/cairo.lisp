(in-package #:lem-wayland)

;;;; Cairo / Pango bindings used for Lem's character-cell rendering.
;;;
;;;; Text is rasterized with Pango into Cairo ARGB32 image surfaces.  The
;;;; compositor copies those surfaces into wlroots shared-memory buffers.

(cffi:define-foreign-library libcairo
  (:unix (:or "libcairo.so.2" "libcairo.so")))

(cffi:define-foreign-library libpango
  (:unix (:or "libpango-1.0.so.0" "libpango-1.0.so")))

(cffi:define-foreign-library libpangocairo
  (:unix (:or "libpangocairo-1.0.so.0" "libpangocairo-1.0.so")))

(cffi:define-foreign-library libgobject
  (:unix (:or "libgobject-2.0.so.0" "libgobject-2.0.so")))

(cffi:use-foreign-library libcairo)
(cffi:use-foreign-library libpango)
(cffi:use-foreign-library libpangocairo)
(cffi:use-foreign-library libgobject)

(defconstant +cairo-format-argb32+ 0)
(defconstant +pango-scale+ 1024)
(defconstant +pango-weight-bold+ 700)
(defconstant +pango-style-normal+ 0)
(defconstant +pango-style-italic+ 2)
(defconstant +pango-underline-single+ 1)
(defconstant +pango-underline-double+ 2)
(defconstant +pango-underline-low+ 4)
(defconstant +pango-underline-error+ 8)
(defconstant +pango-underline-single-line+ 16)
(defconstant +pango-underline-double-line+ 32)

;;;; Cairo

(cffi:defcfun ("cairo_image_surface_create" %cairo-image-surface-create) :pointer
  (format :int)
  (width :int)
  (height :int))

(cffi:defcfun ("cairo_image_surface_get_data" %cairo-image-surface-get-data) :pointer
  (surface :pointer))

(cffi:defcfun ("cairo_image_surface_get_stride" %cairo-image-surface-get-stride) :int
  (surface :pointer))

(cffi:defcfun ("cairo_surface_flush" %cairo-surface-flush) :void
  (surface :pointer))

(cffi:defcfun ("cairo_surface_mark_dirty" %cairo-surface-mark-dirty) :void
  (surface :pointer))

(cffi:defcfun ("cairo_surface_destroy" %cairo-surface-destroy) :void
  (surface :pointer))

(cffi:defcfun ("cairo_surface_write_to_png" %cairo-surface-write-to-png) :int
  (surface :pointer)
  (filename :string))

(cffi:defcfun ("cairo_create" %cairo-create) :pointer
  (surface :pointer))

(cffi:defcfun ("cairo_destroy" %cairo-destroy) :void
  (context :pointer))

(cffi:defcfun ("cairo_save" %cairo-save) :void
  (context :pointer))

(cffi:defcfun ("cairo_restore" %cairo-restore) :void
  (context :pointer))

(cffi:defcfun ("cairo_set_source_rgba" %cairo-set-source-rgba) :void
  (context :pointer)
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(cffi:defcfun ("cairo_set_source_rgb" %cairo-set-source-rgb) :void
  (context :pointer)
  (red :double)
  (green :double)
  (blue :double))

(cffi:defcfun ("cairo_rectangle" %cairo-rectangle) :void
  (context :pointer)
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(cffi:defcfun ("cairo_fill" %cairo-fill) :void
  (context :pointer))

(cffi:defcfun ("cairo_paint" %cairo-paint) :void
  (context :pointer))

(cffi:defcfun ("cairo_clip" %cairo-clip) :void
  (context :pointer))

(cffi:defcfun ("cairo_reset_clip" %cairo-reset-clip) :void
  (context :pointer))

(cffi:defcfun ("cairo_new_path" %cairo-new-path) :void
  (context :pointer))

(cffi:defcfun ("cairo_move_to" %cairo-move-to) :void
  (context :pointer)
  (x :double)
  (y :double))

(cffi:defcfun ("cairo_translate" %cairo-translate) :void
  (context :pointer)
  (tx :double)
  (ty :double))

(cffi:defcfun ("cairo_scale" %cairo-scale) :void
  (context :pointer)
  (sx :double)
  (sy :double))

(cffi:defcfun ("cairo_line_to" %cairo-line-to) :void
  (context :pointer)
  (x :double)
  (y :double))

(cffi:defcfun ("cairo_set_line_width" %cairo-set-line-width) :void
  (context :pointer)
  (width :double))

(cffi:defcfun ("cairo_stroke" %cairo-stroke) :void
  (context :pointer))

;;;; Pango

(cffi:defcfun ("pango_font_description_new" %pango-font-description-new) :pointer)

(cffi:defcfun ("pango_font_description_free" %pango-font-description-free) :void
  (description :pointer))

(cffi:defcfun ("pango_font_description_copy" %pango-font-description-copy) :pointer
  (description :pointer))

(cffi:defcfun ("pango_font_description_set_family" %pango-font-description-set-family) :void
  (description :pointer)
  (family :string))

(cffi:defcfun ("pango_font_description_set_absolute_size"
               %pango-font-description-set-absolute-size) :void
  (description :pointer)
  (size :double))

(cffi:defcfun ("pango_font_description_set_weight"
               %pango-font-description-set-weight) :void
  (description :pointer)
  (weight :int))

(cffi:defcfun ("pango_font_description_set_style"
               %pango-font-description-set-style) :void
  (description :pointer)
  (style :int))

(cffi:defcfun ("pango_cairo_create_layout" %pango-cairo-create-layout) :pointer
  (context :pointer))

(cffi:defcfun ("pango_layout_set_font_description"
               %pango-layout-set-font-description) :void
  (layout :pointer)
  (description :pointer))

(cffi:defcfun ("pango_layout_set_text" %pango-layout-set-text) :void
  (layout :pointer)
  (text :string)
  (length :int))

(cffi:defcfun ("pango_layout_set_width" %pango-layout-set-width) :void
  (layout :pointer)
  (width :int))

(cffi:defcfun ("pango_layout_get_pixel_size" %pango-layout-get-pixel-size) :void
  (layout :pointer)
  (width :pointer)
  (height :pointer))

(cffi:defcfun ("pango_layout_get_context" %pango-layout-get-context) :pointer
  (layout :pointer))

(cffi:defcfun ("pango_cairo_update_layout" %pango-cairo-update-layout) :void
  (context :pointer)
  (layout :pointer))

(cffi:defcfun ("pango_cairo_show_layout" %pango-cairo-show-layout) :void
  (context :pointer)
  (layout :pointer))

(cffi:defcfun ("pango_layout_set_attributes" %pango-layout-set-attributes) :void
  (layout :pointer)
  (attributes :pointer))

(cffi:defcfun ("pango_context_get_metrics" %pango-context-get-metrics) :pointer
  (context :pointer)
  (description :pointer)
  (language :pointer))

(cffi:defcfun ("pango_font_metrics_get_ascent" %pango-font-metrics-get-ascent) :int
  (metrics :pointer))

(cffi:defcfun ("pango_font_metrics_get_descent" %pango-font-metrics-get-descent) :int
  (metrics :pointer))

(cffi:defcfun ("pango_font_metrics_unref" %pango-font-metrics-unref) :void
  (metrics :pointer))

(cffi:defcfun ("g_object_unref" %g-object-unref) :void
  (object :pointer))

;;;; Helpers

(defun cairo-argb32-image (width height)
  "Create a Cairo image surface and return (values surface data stride)."
  (let ((surface (%cairo-image-surface-create +cairo-format-argb32+
                                             (max 1 width)
                                             (max 1 height))))
    (when (cffi:null-pointer-p surface)
      (error "Could not create a Cairo image surface (~Dx~D)" width height))
    (values surface
            (%cairo-image-surface-get-data surface)
            (%cairo-image-surface-get-stride surface))))
