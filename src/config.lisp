;;;; cl-term/src/config.lisp — Runtime configuration

(in-package #:cl-term.config)

(defstruct (terminal-config (:constructor %make-terminal-config))
  (scrollback-limit 2000 :type (integer 100 200000))
  (cursor-blink nil :type boolean)
  (bell-style :none :type keyword)
  (shell "/bin/bash" :type string)
  (keymap-name :default :type keyword))

(defun normalize-config (&key (scrollback-limit 2000) (cursor-blink nil)
                           (bell-style :none) (shell "/bin/bash")
                           (keymap-name :default))
  (%make-terminal-config
   :scrollback-limit (max 100 scrollback-limit)
   :cursor-blink (not (null cursor-blink))
   :bell-style bell-style
   :shell shell
   :keymap-name keymap-name))

(defun merge-config (&optional base overrides)
  (let* ((b (or base (normalize-config)))
         (o (or overrides b)))
    (%make-terminal-config
     :scrollback-limit (terminal-config-scrollback-limit o)
     :cursor-blink (terminal-config-cursor-blink o)
     :bell-style (terminal-config-bell-style o)
     :shell (terminal-config-shell o)
     :keymap-name (terminal-config-keymap-name o))))

(defun make-terminal-config (&rest keys)
  (apply #'normalize-config keys))
