(asdf:load-asd (merge-pathnames "frontends/wayland/lem-wayland.asd"
                                (uiop:getcwd)))

(ql:quickload :lem-wayland)

(lem:init-at-build-time)

(sb-ext:save-lisp-and-die "lem-wayland"
                          :toplevel #'lem-wayland:main
                          :executable t)
