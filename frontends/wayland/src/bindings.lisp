(in-package #:lem-wayland)

;;;; Foreign libraries

(cffi:define-foreign-library libwayland-server
  (:unix (:or "libwayland-server.so.0" "libwayland-server.so")))

(cffi:define-foreign-library libwlroots-0.19
  (:unix (:or "libwlroots-0.19.so.0" "libwlroots-0.19.so")))

(cffi:define-foreign-library libxkbcommon
  (:unix (:or "libxkbcommon.so.0" "libxkbcommon.so")))

(cffi:use-foreign-library libwayland-server)
(cffi:use-foreign-library libwlroots-0.19)
(cffi:use-foreign-library libxkbcommon)

;;;; Signal/listener plumbing

(defvar *signal-listeners* (make-hash-table :test #'eql))
(defvar *next-signal-listener-id* 0)

(defstruct (signal-listener (:constructor %make-signal-listener (id record callback)))
  id
  record
  callback)

(cffi:defcfun ("wl_list_insert" %wl-list-insert) :void
  (list :pointer)
  (element :pointer))

(cffi:defcfun ("wl_list_remove" %wl-list-remove) :void
  (element :pointer))

(cffi:defcallback %signal-listener-callback :void ((listener :pointer) (data :pointer))
  (let ((entry (gethash (cffi:mem-ref listener :uint64 +wl-listener-size+)
                        *signal-listeners*)))
    (when entry
      (funcall (signal-listener-callback entry) data))))

(defun add-signal-listener (owner signal-offset callback)
  "Add CALLBACK to the wl_signal at OWNER + SIGNAL-OFFSET.

OWNER is a foreign pointer to the object containing the signal.  The returned
object owns a foreign allocation and must be passed to
REMOVE-SIGNAL-LISTENER when the owner is destroyed."
  (let* ((record (cffi:foreign-alloc :char
                                     :count (+ +wl-listener-size+ 8)
                                     :initial-element 0))
         (id (incf *next-signal-listener-id*))
         (entry (%make-signal-listener id record callback)))
    (setf (cffi:mem-ref record :pointer +wl-listener-notify+)
          (cffi:callback %signal-listener-callback))
    (setf (cffi:mem-ref record :uint64 +wl-listener-size+) id)
    (setf (gethash id *signal-listeners*) entry)
    (%wl-list-insert (cffi:inc-pointer owner signal-offset) record)
    entry))

(defun remove-signal-listener (entry)
  (when (and entry (signal-listener-record entry))
    (%wl-list-remove (signal-listener-record entry))
    (remhash (signal-listener-id entry) *signal-listeners*)
    (cffi:foreign-free (signal-listener-record entry))
    (setf (signal-listener-record entry) nil))
  (values))

;;;; Raw field accessors
;;;
;;; wlroots publishes opaque pointers only.  The offsets in OFFSETS.LISP are
;;; produced from the installed headers by scripts/gen-offsets.sh.

(declaim (inline ptr-at u8-at u32-at i32-at u64-at size-at double-at))

(defun ptr-at (pointer offset)
  (cffi:mem-ref pointer :pointer offset))

(defun u8-at (pointer offset)
  (cffi:mem-ref pointer :uint8 offset))

(defun u32-at (pointer offset)
  (cffi:mem-ref pointer :uint32 offset))

(defun i32-at (pointer offset)
  (cffi:mem-ref pointer :int32 offset))

(defun u64-at (pointer offset)
  (cffi:mem-ref pointer :uint64 offset))

(defun size-at (pointer offset)
  (cffi:mem-ref pointer :size offset))

(defun double-at (pointer offset)
  (cffi:mem-ref pointer :double offset))

(defun cstring-at (pointer offset)
  (let ((value (ptr-at pointer offset)))
    (unless (cffi:null-pointer-p value)
      (cffi:foreign-string-to-lisp value))))

;;;; libwayland-server

(cffi:defcfun ("wl_display_create" %wl-display-create) :pointer)

(cffi:defcfun ("wl_display_destroy" %wl-display-destroy) :void
  (display :pointer))

(cffi:defcfun ("wl_display_get_event_loop" %wl-display-get-event-loop) :pointer
  (display :pointer))

(cffi:defcfun ("wl_display_add_socket_auto" %wl-display-add-socket-auto) :pointer
  (display :pointer))

(cffi:defcfun ("wl_display_run" %wl-display-run) :void
  (display :pointer))

(cffi:defcfun ("wl_display_terminate" %wl-display-terminate) :void
  (display :pointer))

(cffi:defcfun ("wl_display_destroy_clients" %wl-display-destroy-clients) :void
  (display :pointer))

(cffi:defcfun ("wl_display_next_serial" %wl-display-next-serial) :uint32
  (display :pointer))

(cffi:defcfun ("wl_event_loop_add_fd" %wl-event-loop-add-fd) :pointer
  (event-loop :pointer)
  (fd :int)
  (mask :uint32)
  (callback :pointer)
  (data :pointer))

(cffi:defcfun ("wl_event_loop_add_signal" %wl-event-loop-add-signal) :pointer
  (event-loop :pointer)
  (signal-number :int)
  (callback :pointer)
  (data :pointer))

(cffi:defcfun ("wl_event_loop_dispatch" %wl-event-loop-dispatch) :int
  (event-loop :pointer)
  (timeout :int))

(cffi:defcfun ("wl_event_source_remove" %wl-event-source-remove) :int
  (source :pointer))

(cffi:defcfun ("eventfd" %eventfd) :int
  (initval :uint32)
  (flags :int))

(cffi:defcfun ("read" %unix-read) :long
  (fd :int)
  (buffer :pointer)
  (count :size))

(cffi:defcfun ("write" %unix-write) :long
  (fd :int)
  (buffer :pointer)
  (count :size))

(cffi:defcfun ("close" %unix-close) :int
  (fd :int))

(cffi:defcfun ("pipe" %unix-pipe) :int
  (fds :pointer))

(cffi:defcfun ("poll" %unix-poll) :int
  (fds :pointer)
  (nfds :ulong)
  (timeout :int))

;;;; wlroots: logging / backend / renderer

(cffi:defcfun ("wlr_log_init" %wlr-log-init) :void
  (verbosity :int)
  (callback :pointer))

(cffi:defcfun ("wlr_backend_autocreate" %wlr-backend-autocreate) :pointer
  (event-loop :pointer)
  (session-pointer :pointer))

(cffi:defcfun ("wlr_backend_start" %wlr-backend-start) :boolean
  (backend :pointer))

(cffi:defcfun ("wlr_backend_destroy" %wlr-backend-destroy) :void
  (backend :pointer))

(cffi:defcfun ("wlr_renderer_autocreate" %wlr-renderer-autocreate) :pointer
  (backend :pointer))

(cffi:defcfun ("wlr_allocator_autocreate" %wlr-allocator-autocreate) :pointer
  (backend :pointer)
  (renderer :pointer))

(cffi:defcfun ("wlr_shm_allocator_create" %wlr-shm-allocator-create) :pointer)

(cffi:defcfun ("wlr_allocator_create_buffer" %wlr-allocator-create-buffer) :pointer
  (allocator :pointer)
  (width :int)
  (height :int)
  (format :pointer))

(cffi:defcfun ("wlr_renderer_get_texture_formats"
               %wlr-renderer-get-texture-formats) :pointer
  (renderer :pointer)
  (buffer-caps :uint32))

(cffi:defcfun ("wlr_drm_format_set_get" %wlr-drm-format-set-get) :pointer
  (format-set :pointer)
  (format :uint32))

(cffi:defcfun ("wlr_buffer_begin_data_ptr_access"
               %wlr-buffer-begin-data-ptr-access) :boolean
  (buffer :pointer)
  (flags :uint32)
  (data :pointer)
  (format :pointer)
  (stride :pointer))

(cffi:defcfun ("wlr_buffer_end_data_ptr_access" %wlr-buffer-end-data-ptr-access) :void
  (buffer :pointer))

(cffi:defcfun ("wlr_buffer_drop" %wlr-buffer-drop) :void
  (buffer :pointer))

;;;; wlroots: globals

(cffi:defcfun ("wlr_compositor_create" %wlr-compositor-create) :pointer
  (display :pointer)
  (version :uint32)
  (renderer :pointer))

(cffi:defcfun ("wlr_subcompositor_create" %wlr-subcompositor-create) :pointer
  (display :pointer))

(cffi:defcfun ("wlr_data_device_manager_create" %wlr-data-device-manager-create) :pointer
  (display :pointer))

(cffi:defcfun ("wlr_primary_selection_v1_device_manager_create"
               %wlr-primary-selection-v1-device-manager-create)
    :pointer
  (display :pointer))

(cffi:defcfun ("wlr_xdg_shell_create" %wlr-xdg-shell-create) :pointer
  (display :pointer)
  (version :uint32))

(cffi:defcfun ("wlr_shm_create_with_renderer" %wlr-shm-create-with-renderer) :pointer
  (display :pointer)
  (version :uint32)
  (renderer :pointer))

(cffi:defcfun ("wlr_linux_dmabuf_v1_create_with_renderer"
               %wlr-linux-dmabuf-v1-create-with-renderer) :pointer
  (display :pointer)
  (version :uint32)
  (renderer :pointer))

(cffi:defcfun ("wlr_xdg_toplevel_set_size" %wlr-xdg-toplevel-set-size) :uint32
  (toplevel :pointer)
  (width :uint32)
  (height :uint32))

(cffi:defcfun ("wlr_xdg_toplevel_set_activated"
               %wlr-xdg-toplevel-set-activated) :void
  (toplevel :pointer)
  (activated :boolean))

(cffi:defcfun ("wlr_xdg_toplevel_set_tiled" %wlr-xdg-toplevel-set-tiled) :void
  (toplevel :pointer)
  (edges :uint32))

(cffi:defcfun ("wlr_xdg_toplevel_send_close" %wlr-xdg-toplevel-send-close) :void
  (toplevel :pointer))

;;;; wlroots: scene

(cffi:defcfun ("wlr_scene_create" %wlr-scene-create) :pointer)

(cffi:defcfun ("wlr_scene_attach_output_layout" %wlr-scene-attach-output-layout) :pointer
  (scene :pointer)
  (output-layout :pointer))

(cffi:defcfun ("wlr_scene_output_create" %wlr-scene-output-create) :pointer
  (scene :pointer)
  (output :pointer))

(cffi:defcfun ("wlr_scene_output_layout_add_output" %wlr-scene-output-layout-add-output) :void
  (scene-output-layout :pointer)
  (output-layout-output :pointer)
  (scene-output :pointer))

(cffi:defcfun ("wlr_scene_get_scene_output" %wlr-scene-get-scene-output) :pointer
  (scene :pointer)
  (output :pointer))

(cffi:defcfun ("wlr_scene_xdg_surface_create" %wlr-scene-xdg-surface-create) :pointer
  (parent :pointer)
  (xdg-surface :pointer))

(cffi:defcfun ("wlr_scene_tree_create" %wlr-scene-tree-create) :pointer
  (parent :pointer))

(cffi:defcfun ("wlr_scene_buffer_create" %wlr-scene-buffer-create) :pointer
  (parent :pointer)
  (buffer :pointer))

(cffi:defcfun ("wlr_scene_buffer_set_buffer" %wlr-scene-buffer-set-buffer) :void
  (scene-buffer :pointer)
  (buffer :pointer))

(cffi:defcfun ("wlr_scene_buffer_set_dest_size" %wlr-scene-buffer-set-dest-size) :void
  (scene-buffer :pointer)
  (width :int)
  (height :int))

(cffi:defcfun ("wlr_scene_rect_create" %wlr-scene-rect-create) :pointer
  (parent :pointer)
  (width :int)
  (height :int)
  (color :pointer))

(cffi:defcfun ("wlr_scene_rect_set_size" %wlr-scene-rect-set-size) :void
  (rect :pointer)
  (width :int)
  (height :int))

(cffi:defcfun ("wlr_scene_node_destroy" %wlr-scene-node-destroy) :void
  (node :pointer))

(cffi:defcfun ("wlr_scene_node_set_enabled" %wlr-scene-node-set-enabled) :void
  (node :pointer)
  (enabled :boolean))

(cffi:defcfun ("wlr_scene_node_raise_to_top" %wlr-scene-node-raise-to-top) :void
  (node :pointer))

(cffi:defcfun ("wlr_scene_node_lower_to_bottom" %wlr-scene-node-lower-to-bottom) :void
  (node :pointer))

(cffi:defcfun ("wlr_scene_node_reparent" %wlr-scene-node-reparent) :void
  (node :pointer)
  (new-parent :pointer))

(cffi:defcfun ("wlr_scene_node_set_position" %wlr-scene-node-set-position) :void
  (node :pointer)
  (x :int)
  (y :int))

(cffi:defcfun ("wlr_scene_node_at" %wlr-scene-node-at) :pointer
  (node :pointer)
  (lx :double)
  (ly :double)
  (nx :pointer)
  (ny :pointer))

(cffi:defcfun ("wlr_scene_buffer_from_node" %wlr-scene-buffer-from-node) :pointer
  (node :pointer))

(cffi:defcfun ("wlr_scene_surface_try_from_buffer" %wlr-scene-surface-try-from-buffer) :pointer
  (buffer :pointer))

(cffi:defcfun ("wlr_scene_output_needs_frame" %wlr-scene-output-needs-frame) :boolean
  (scene-output :pointer))

(cffi:defcfun ("wlr_scene_output_commit" %wlr-scene-output-commit) :boolean
  (scene-output :pointer)
  (options :pointer))

(cffi:defcfun ("wlr_scene_output_send_frame_done" %wlr-scene-output-send-frame-done) :void
  (scene-output :pointer)
  (now :pointer))

;;;; wlroots: output / output layout

(cffi:defcfun ("wlr_output_layout_create" %wlr-output-layout-create) :pointer
  (display :pointer))

(cffi:defcfun ("wlr_output_layout_add_auto" %wlr-output-layout-add-auto) :pointer
  (layout :pointer)
  (output :pointer))

(cffi:defcfun ("wlr_output_init_render" %wlr-output-init-render) :boolean
  (output :pointer)
  (allocator :pointer)
  (renderer :pointer))

(cffi:defcfun ("wlr_output_effective_resolution" %wlr-output-effective-resolution) :void
  (output :pointer)
  (width :pointer)
  (height :pointer))

(cffi:defcfun ("wlr_output_state_init" %wlr-output-state-init) :void
  (state :pointer))

(cffi:defcfun ("wlr_output_state_finish" %wlr-output-state-finish) :void
  (state :pointer))

(cffi:defcfun ("wlr_output_state_set_enabled" %wlr-output-state-set-enabled) :void
  (state :pointer)
  (enabled :boolean))

(cffi:defcfun ("wlr_output_state_set_custom_mode" %wlr-output-state-set-custom-mode) :void
  (state :pointer)
  (width :int32)
  (height :int32)
  (refresh :int32))

(cffi:defcfun ("wlr_output_commit_state" %wlr-output-commit-state) :boolean
  (output :pointer)
  (state :pointer))

(cffi:defcfun ("wlr_output_schedule_frame" %wlr-output-schedule-frame) :void
  (output :pointer))

;;;; wlroots: seat

(cffi:defcfun ("wlr_seat_create" %wlr-seat-create) :pointer
  (display :pointer)
  (name :string))

(cffi:defcfun ("wlr_seat_set_capabilities" %wlr-seat-set-capabilities) :void
  (seat :pointer)
  (capabilities :uint32))

(cffi:defcfun ("wlr_seat_set_keyboard" %wlr-seat-set-keyboard) :void
  (seat :pointer)
  (keyboard :pointer))

(cffi:defcfun ("wlr_seat_keyboard_notify_enter" %wlr-seat-keyboard-notify-enter) :void
  (seat :pointer)
  (surface :pointer)
  (keycodes :pointer)
  (num-keycodes :size)
  (modifiers :pointer))

(cffi:defcfun ("wlr_seat_keyboard_notify_clear_focus" %wlr-seat-keyboard-notify-clear-focus) :void
  (seat :pointer))

(cffi:defcfun ("wlr_seat_keyboard_notify_key" %wlr-seat-keyboard-notify-key) :void
  (seat :pointer)
  (time-msec :uint32)
  (key :uint32)
  (state :uint32))

(cffi:defcfun ("wlr_seat_keyboard_notify_modifiers" %wlr-seat-keyboard-notify-modifiers) :void
  (seat :pointer)
  (modifiers :pointer))

(cffi:defcfun ("wlr_seat_pointer_notify_enter" %wlr-seat-pointer-notify-enter) :void
  (seat :pointer)
  (surface :pointer)
  (sx :double)
  (sy :double))

(cffi:defcfun ("wlr_seat_pointer_notify_clear_focus" %wlr-seat-pointer-notify-clear-focus) :void
  (seat :pointer))

(cffi:defcfun ("wlr_seat_pointer_notify_motion" %wlr-seat-pointer-notify-motion) :void
  (seat :pointer)
  (time-msec :uint32)
  (sx :double)
  (sy :double))

(cffi:defcfun ("wlr_seat_pointer_notify_button" %wlr-seat-pointer-notify-button) :uint32
  (seat :pointer)
  (time-msec :uint32)
  (button :uint32)
  (state :uint32))

(cffi:defcfun ("wlr_seat_pointer_notify_axis" %wlr-seat-pointer-notify-axis) :void
  (seat :pointer)
  (time-msec :uint32)
  (orientation :uint32)
  (value :double)
  (value-discrete :int32)
  (source :uint32)
  (relative-direction :uint32))

(cffi:defcfun ("wlr_seat_pointer_notify_frame" %wlr-seat-pointer-notify-frame) :void
  (seat :pointer))

(cffi:defcfun ("wlr_seat_set_selection" %wlr-seat-set-selection) :void
  (seat :pointer)
  (source :pointer)
  (serial :uint32))

(cffi:defcfun ("wlr_seat_set_primary_selection" %wlr-seat-set-primary-selection) :void
  (seat :pointer)
  (source :pointer)
  (serial :uint32))

;;;; wlroots: clipboard data sources

(cffi:defcfun ("wlr_data_source_init" %wlr-data-source-init) :void
  (source :pointer)
  (impl :pointer))

(cffi:defcfun ("wlr_data_source_send" %wlr-data-source-send) :void
  (source :pointer)
  (mime-type :string)
  (fd :int))

(cffi:defcfun ("wlr_data_source_destroy" %wlr-data-source-destroy) :void
  (source :pointer))

;;;; wlroots: cursor / input devices / keyboard

(cffi:defcfun ("wlr_cursor_create" %wlr-cursor-create) :pointer)

(cffi:defcfun ("wlr_cursor_attach_output_layout" %wlr-cursor-attach-output-layout) :void
  (cursor :pointer)
  (layout :pointer))

(cffi:defcfun ("wlr_cursor_attach_input_device" %wlr-cursor-attach-input-device) :void
  (cursor :pointer)
  (device :pointer))

(cffi:defcfun ("wlr_cursor_move" %wlr-cursor-move) :void
  (cursor :pointer)
  (device :pointer)
  (delta-x :double)
  (delta-y :double))

(cffi:defcfun ("wlr_cursor_warp_absolute" %wlr-cursor-warp-absolute) :void
  (cursor :pointer)
  (device :pointer)
  (x :double)
  (y :double))

(cffi:defcfun ("wlr_cursor_set_xcursor" %wlr-cursor-set-xcursor) :void
  (cursor :pointer)
  (manager :pointer)
  (name :string))

(cffi:defcfun ("wlr_cursor_set_surface" %wlr-cursor-set-surface) :void
  (cursor :pointer)
  (surface :pointer)
  (hotspot-x :int)
  (hotspot-y :int))

(cffi:defcfun ("wlr_xcursor_manager_create" %wlr-xcursor-manager-create) :pointer
  (name :pointer)
  (size :uint32))

(cffi:defcfun ("wlr_xcursor_manager_load" %wlr-xcursor-manager-load) :boolean
  (manager :pointer)
  (scale :float))

(cffi:defcfun ("wlr_keyboard_from_input_device" %wlr-keyboard-from-input-device) :pointer
  (device :pointer))

(cffi:defcfun ("wlr_pointer_from_input_device" %wlr-pointer-from-input-device) :pointer
  (device :pointer))

(cffi:defcfun ("wlr_keyboard_set_keymap" %wlr-keyboard-set-keymap) :boolean
  (keyboard :pointer)
  (keymap :pointer))

;;;; libxkbcommon

(cffi:defcfun ("xkb_context_new" %xkb-context-new) :pointer
  (flags :uint32))

(cffi:defcfun ("xkb_context_unref" %xkb-context-unref) :void
  (context :pointer))

(cffi:defcfun ("xkb_keymap_new_from_names" %xkb-keymap-new-from-names) :pointer
  (context :pointer)
  (names :pointer)
  (flags :uint32))

(cffi:defcfun ("xkb_keymap_unref" %xkb-keymap-unref) :void
  (keymap :pointer))

(cffi:defcfun ("xkb_state_key_get_one_sym" %xkb-state-key-get-one-sym) :uint32
  (state :pointer)
  (keycode :uint32))

(cffi:defcfun ("xkb_state_key_get_utf32" %xkb-state-key-get-utf32) :uint32
  (state :pointer)
  (keycode :uint32))

(cffi:defcfun ("xkb_state_mod_name_is_active" %xkb-state-mod-name-is-active) :boolean
  (state :pointer)
  (name :string)
  (type :uint32))

(cffi:defcfun ("xkb_state_key_get_layout" %xkb-state-key-get-layout) :uint32
  (state :pointer)
  (keycode :uint32))

(cffi:defcfun ("xkb_keymap_key_get_syms_by_level"
               %xkb-keymap-key-get-syms-by-level) :int
  (keymap :pointer)
  (keycode :uint32)
  (layout :uint32)
  (level :uint32)
  (syms :pointer))

;;;; libc

(cffi:defcfun ("clock_gettime" %clock-gettime) :int
  (clock-id :int)
  (timespec :pointer))

(cffi:defcfun ("memcpy" %memcpy) :pointer
  (destination :pointer)
  (source :pointer)
  (size :size))
