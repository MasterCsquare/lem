(defsystem "lem-webview"
  :depends-on ("webview"
               "float-features"
               "lem-server"
               "lem-darwin")
  :serial t
  :components ((:file "main")
               (:file "browser")))
