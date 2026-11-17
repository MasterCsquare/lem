(defpackage :lem-sdl2/wm
  (:use :cl)
  (:export :set-x11-wm-class
           :cocoa-window))
(in-package :lem-sdl2/wm)

(defparameter +lem-x11-wm-class+ "Lem SDL2")

;; Layout of SDL2's SDL_SysWMinfo: an SDL_version (3 bytes) at offset 0,
;; the SDL_SYSWM_TYPE subsystem (an int) at 4, and a union of
;; per-platform structs at 8, padded to 64 bytes. On Cocoa the union
;; starts with the NSWindow pointer.
(defconstant +syswm-info-size+ 72)
(defconstant +syswm-subsystem-offset+ 4)
(defconstant +syswm-info-offset+ 8)
(defconstant +syswm-cocoa+ 4)

;; this is SDL2 way
;; if the stable version of SDL is 3, set WM_CLASS is set via hint SDL_HINT_APP_ID
;;
;; cf.
;; - how SDL3 gets WM_CLASS:
;;     - https://github.com/libsdl-org/SDL/blob/d3f2de7f297d761a7dc5b0dda3c7b5d7bd49eac9/src/video/x11/SDL_x11window.c#L633C40-L633C40
;; - how to set WM_CLASS in here:
;;     - SDL_SetHint() function with key SDL_HINT_APP_ID
;;     - https://wiki.libsdl.org/SDL2/SDL_SetHint
;;     - https://github.com/libsdl-org/SDL/blob/d3f2de7f297d761a7dc5b0dda3c7b5d7bd49eac9/src/core/unix/SDL_appid.c#L63C45-L63C45
(defun set-x11-wm-class ()
  (setf (uiop:getenv "SDL_VIDEO_X11_WMCLASS") +lem-x11-wm-class+))

(defun cocoa-window (window)
  "Return the NSWindow behind the SDL WINDOW, as a foreign pointer, or
NIL if WINDOW is not a Cocoa window."
  (cffi:with-foreign-object (info :uint8 +syswm-info-size+)
    (loop :for i :below +syswm-info-size+
          :do (setf (cffi:mem-aref info :uint8 i) 0))
    ;; SDL_GetWindowWMInfo reads the caller's SDL version from the struct
    ;; to know which layout to fill in.
    (cffi:foreign-funcall "SDL_GetVersion" :pointer info :void)
    (when (and (/= 0 (cffi:foreign-funcall "SDL_GetWindowWMInfo"
                                           :pointer (autowrap:ptr window)
                                           :pointer info
                                           :int))
               (= +syswm-cocoa+
                  (cffi:mem-ref info :int +syswm-subsystem-offset+)))
      (let ((nswindow (cffi:mem-ref info :pointer +syswm-info-offset+)))
        (unless (cffi:null-pointer-p nswindow)
          nswindow)))))
