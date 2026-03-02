;;;; cl-term/src/package.lisp — Package definitions for cl-term

(defpackage #:cl-term.pty
  (:use #:cl #:cffi)
  (:export
   ;; PTY struct accessors
   #:pty
   #:pty-p
   #:pty-master-fd
   #:pty-slave-fd
   #:pty-pid
   ;; Lifecycle
   #:make-pty
   #:pty-close
   #:with-pty
   ;; I/O
   #:spawn-shell
   #:read-from-pty
   #:write-to-pty
   #:pty-resize))

(defpackage #:cl-term.ansi
  (:use #:cl)
  (:export
   ;; Sequence struct
   #:ansi-sequence
   #:ansi-sequence-type
   #:ansi-sequence-params
   #:ansi-sequence-intermediate
   #:ansi-sequence-final
   #:ansi-sequence-data
   ;; Parser
   #:ansi-parser
   #:make-ansi-parser
   #:ansi-parser-result
   #:ansi-parser-state
   #:parser-feed
   #:parse-ansi-sequences
   ;; SGR helpers
   #:sgr-bold
   #:sgr-underline
   #:sgr-blink
   #:sgr-reverse
   #:sgr-fg-color
   #:sgr-bg-color
   #:sgr-reset))

(defpackage #:cl-term.config
  (:use #:cl)
  (:export
   #:terminal-config
   #:make-terminal-config
   #:terminal-config-scrollback-limit
   #:terminal-config-cursor-blink
   #:terminal-config-bell-style
   #:terminal-config-shell
   #:terminal-config-keymap-name
   #:normalize-config
   #:merge-config))

(defpackage #:cl-term.screen
  (:use #:cl)
  (:export
   ;; Screen struct
   #:screen
   #:make-screen
   #:screen-width
   #:screen-height
   #:screen-cursor-row
   #:screen-cursor-col
   #:screen-scroll-top
   #:screen-scroll-bot
   #:screen-current-attrs
   #:screen-tab-stops
   ;; Cell access
   #:screen-cell-char
   #:screen-cell-attrs
   #:screen-ref
   ;; Cell attrs
   #:cell-attrs
   #:make-cell-attrs
   #:cell-attrs-bold
   #:cell-attrs-underline
   #:cell-attrs-blink
   #:cell-attrs-reverse
   #:cell-attrs-fg-color
   #:cell-attrs-bg-color
   ;; Operations
   #:screen-put-char
   #:screen-move-cursor
   #:screen-scroll-up
   #:screen-scroll-down
   #:screen-clear
   #:screen-clear-line
   #:screen-erase-to-eol
   #:screen-erase-to-sol
   #:screen-insert-line
   #:screen-delete-line
   #:char-display-width
   #:screen-switch-alt
   #:screen-switch-normal
   #:screen-scrollback
   #:screen-search-scrollback
   #:screen-set-max-scrollback
   #:screen-line-string))

(defpackage #:cl-term.terminal
  (:use #:cl #:cl-term.pty #:cl-term.ansi #:cl-term.screen #:cl-term.config)
  (:export
   #:terminal
   #:make-terminal
   #:terminal-screen
   #:terminal-pty
   #:terminal-running-p
   #:start-terminal
   #:stop-terminal
   #:send-input
   #:receive-output
   #:process-output
   #:send-key
   #:terminal-bind-key
   #:terminal-key-bindings
   #:terminal-list-tabs
   #:terminal-new-tab
   #:terminal-switch-tab
   #:terminal-close-tab
   #:terminal-rename-tab
   #:terminal-search-scrollback))

(defpackage #:cl-term
  (:use #:cl)
  (:import-from #:cl-term.terminal
   #:make-terminal
   #:start-terminal
   #:stop-terminal
   #:send-input
   #:receive-output)
  (:import-from #:cl-term.screen
   #:screen-width
   #:screen-height
   #:screen-cursor-row
   #:screen-cursor-col)
  (:export
   #:make-terminal
   #:start-terminal
   #:stop-terminal
   #:send-input
   #:receive-output
   #:screen-width
   #:screen-height
   #:screen-cursor-row
   #:screen-cursor-col))

(defpackage #:cl-term.renderer
  (:use #:common-lisp)
  (:import-from #:cl-term.screen
   #:screen #:screen-width #:screen-height
   #:screen-cursor-row #:screen-cursor-col
   #:screen-cell-char #:screen-cell-attrs
   #:cell-attrs-bold #:cell-attrs-underline #:cell-attrs-blink
   #:cell-attrs-reverse #:cell-attrs-fg-color #:cell-attrs-bg-color)
  (:export
   #:color-256-to-rgb
   #:render-screen-plain
   #:render-screen-ansi
   #:dump-screen))
