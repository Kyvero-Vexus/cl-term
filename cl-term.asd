(defsystem #:cl-term
  :name "cl-term"
  :description "Terminal emulator with xterm feature parity"
  :version "0.2.0"
  :author "Gensym"
  :license "GPL-3.0"
  :depends-on (#:cffi #:bordeaux-threads #:babel)
  :serial t
  :components ((:file "src/package")
               (:file "src/config")
               (:file "src/pty")
               (:file "src/ansi")
               (:file "src/screen")
               (:file "src/terminal")
               (:file "src/renderer")))
