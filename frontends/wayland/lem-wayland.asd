(defsystem "lem-wayland"
  :description "A Wayland compositor frontend for Lem (wlroots 0.19)."
  :depends-on ("cffi"
               "bordeaux-threads"
               "uiop"
               "lem/core")
  :serial t
  :components ((:file "src/package")
               (:file "src/offsets")
               (:file "src/bindings")
               (:file "src/cairo")
               (:file "src/queue")
               (:file "src/compositor")
               (:file "src/frontend")
               (:file "src/main")))

(defsystem "lem-wayland/executable"
  :build-operation program-op
  :build-pathname "lem-wayland"
  :entry-point "lem-wayland:main"
  :depends-on ("lem-wayland"))
