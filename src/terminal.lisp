;;;; cl-term/src/terminal.lisp — Terminal state machine
;;;;
;;;; Combines PTY I/O, ANSI parsing, and screen buffer management
;;;; into a unified terminal emulator state machine.

(in-package #:cl-term.terminal)

;;; ── Terminal struct ───────────────────────────────────────────────────────────

(defstruct terminal
  "Full terminal emulator state."
  (pty    nil)   ; CL-TERM.PTY:PTY
  (screen nil)   ; CL-TERM.SCREEN:SCREEN
  (parser nil)   ; CL-TERM.ANSI:ANSI-PARSER
  (thread nil)   ; bordeaux-threads thread for I/O
  (running-p nil :type boolean)
  (on-update nil) ; callback: (lambda (terminal)) called after screen update
  (lock     nil)) ; bt:lock protecting screen

(defun make-terminal (&key (width 80) (height 24) on-update)
  "Create a new terminal emulator with a WIDTHxHEIGHT screen."
  (make-terminal%
   :screen    (cl-term.screen:make-screen :width width :height height)
   :parser    (cl-term.ansi:make-ansi-parser)
   :running-p nil
   :on-update on-update
   :lock      (bt:make-lock "terminal-lock")))

;;; ── SGR attribute application ────────────────────────────────────────────────

(defun apply-sgr (terminal seq)
  "Apply SGR (Select Graphic Rendition) sequence to current screen attributes."
  (let ((screen (terminal-screen terminal))
        (attrs  (cl-term.screen:screen-current-attrs
                 (terminal-screen terminal))))
    (when (cl-term.ansi:sgr-reset seq)
      (setf attrs (cl-term.screen:make-cell-attrs)))
    (when (cl-term.ansi:sgr-bold seq)
      (setf (cl-term.screen:cell-attrs-bold attrs) t))
    (when (cl-term.ansi:sgr-underline seq)
      (setf (cl-term.screen:cell-attrs-underline attrs) t))
    (when (cl-term.ansi:sgr-blink seq)
      (setf (cl-term.screen:cell-attrs-blink attrs) t))
    (when (cl-term.ansi:sgr-reverse seq)
      (setf (cl-term.screen:cell-attrs-reverse attrs) t))
    (let ((fg (cl-term.ansi:sgr-fg-color seq))
          (bg (cl-term.ansi:sgr-bg-color seq)))
      (when fg (setf (cl-term.screen:cell-attrs-fg-color attrs) fg))
      (when bg (setf (cl-term.screen:cell-attrs-bg-color attrs) bg)))
    (setf (cl-term.screen:screen-current-attrs screen) attrs)))

;;; ── CSI sequence dispatch ────────────────────────────────────────────────────

(defun dispatch-csi (terminal seq)
  "Dispatch a parsed CSI sequence to the appropriate screen operation."
  (let* ((screen (terminal-screen terminal))
         (params (cl-term.ansi:ansi-sequence-params seq))
         (p1     (or (first params) 0))
         (p2     (or (second params) 0)))
    (case (cl-term.ansi:ansi-sequence-final seq)

      ;; Cursor movement
      (#\A (cl-term.screen:screen-move-cursor screen
             (max 0 (- (cl-term.screen:screen-cursor-row screen) (max 1 p1)))
             (cl-term.screen:screen-cursor-col screen)))

      (#\B (cl-term.screen:screen-move-cursor screen
             (min (1- (cl-term.screen:screen-height screen))
                  (+ (cl-term.screen:screen-cursor-row screen) (max 1 p1)))
             (cl-term.screen:screen-cursor-col screen)))

      (#\C (cl-term.screen:screen-move-cursor screen
             (cl-term.screen:screen-cursor-row screen)
             (min (1- (cl-term.screen:screen-width screen))
                  (+ (cl-term.screen:screen-cursor-col screen) (max 1 p1)))))

      (#\D (cl-term.screen:screen-move-cursor screen
             (cl-term.screen:screen-cursor-row screen)
             (max 0 (- (cl-term.screen:screen-cursor-col screen) (max 1 p1)))))

      (#\H ; Cursor Position (CUP): row p1, col p2 (1-indexed)
       (#\f)
       (cl-term.screen:screen-move-cursor screen
         (max 0 (1- (max 1 p1)))
         (max 0 (1- (max 1 p2)))))

      (#\J ; Erase Display
       (case p1
         (0 ; from cursor to end
          (cl-term.screen:screen-erase-to-eol screen)
          (loop for row from (1+ (cl-term.screen:screen-cursor-row screen))
                below (cl-term.screen:screen-height screen)
                do (cl-term.screen:screen-clear-line screen row)))
         (1 ; from beginning to cursor
          (loop for row from 0
                below (cl-term.screen:screen-cursor-row screen)
                do (cl-term.screen:screen-clear-line screen row))
          (cl-term.screen:screen-erase-to-sol screen))
         (2 (cl-term.screen:screen-clear screen))))

      (#\K ; Erase Line
       (case p1
         (0 (cl-term.screen:screen-erase-to-eol screen))
         (1 (cl-term.screen:screen-erase-to-sol screen))
         (2 (cl-term.screen:screen-clear-line screen
              (cl-term.screen:screen-cursor-row screen)))))

      (#\L ; Insert Lines
       (cl-term.screen:screen-insert-line screen (max 1 p1)))

      (#\M ; Delete Lines
       (cl-term.screen:screen-delete-line screen (max 1 p1)))

      (#\S ; Scroll Up
       (cl-term.screen:screen-scroll-up screen (max 1 p1)))

      (#\T ; Scroll Down
       (cl-term.screen:screen-scroll-down screen (max 1 p1)))

      (#\m ; SGR
       (apply-sgr terminal seq))

      (#\r ; Set Scroll Region
       (let ((top (max 0 (1- (max 1 p1))))
             (bot (min (1- (cl-term.screen:screen-height screen))
                       (1- (if (zerop p2) (cl-term.screen:screen-height screen) p2)))))
         (when (< top bot)
           (setf (cl-term.screen:screen-scroll-top (terminal-screen terminal)) top
                 (cl-term.screen:screen-scroll-bot (terminal-screen terminal)) bot)
           (cl-term.screen:screen-move-cursor screen 0 0))))

      (otherwise nil)))) ; Ignore unrecognized sequences

;;; ── Output processing ────────────────────────────────────────────────────────

(defun process-output (terminal data)
  "Process raw string DATA from PTY: parse ANSI sequences, update screen."
  (let ((screen (terminal-screen terminal))
        (parser (terminal-parser terminal)))
    (loop for char across data do
      (cl-term.ansi:parser-feed parser char))
    ;; Process all accumulated sequences
    (let ((seqs (nreverse (cl-term.ansi:ansi-parser-result parser))))
      (setf (cl-term.ansi:ansi-parser-result parser) nil)
      (loop for seq in seqs do
        (case (cl-term.ansi:ansi-sequence-type seq)

          (:print
           ;; Printable character
           (cl-term.screen:screen-put-char screen
             (cl-term.ansi:ansi-sequence-final seq)))

          (:ctrl
           ;; Control characters
           (case (cl-term.ansi:ansi-sequence-final seq)
             (#\Return (setf (cl-term.screen:screen-cursor-col screen) 0))
             (#\Linefeed
              (incf (cl-term.screen:screen-cursor-row screen))
              (when (> (cl-term.screen:screen-cursor-row screen)
                       (cl-term.screen:screen-scroll-bot screen))
                (cl-term.screen:screen-scroll-up screen 1)
                (setf (cl-term.screen:screen-cursor-row screen)
                      (cl-term.screen:screen-scroll-bot screen))))
             (#\Backspace
              (when (> (cl-term.screen:screen-cursor-col screen) 0)
                (decf (cl-term.screen:screen-cursor-col screen))))
             (#\Tab
              ;; Advance to next tab stop
              (let* ((col     (cl-term.screen:screen-cursor-col screen))
                     (stops   (cl-term.screen:screen-tab-stops screen))
                     (next    (find-if (lambda (s) (> s col)) stops)))
                (if next
                    (setf (cl-term.screen:screen-cursor-col screen) next)
                    (setf (cl-term.screen:screen-cursor-col screen)
                          (1- (cl-term.screen:screen-width screen))))))))

          (:csi
           (dispatch-csi terminal seq))

          ;; :esc :osc :dcs — TODO
          (otherwise nil))))))

;;; ── I/O loop ─────────────────────────────────────────────────────────────────

(defun %io-loop (terminal)
  "Background thread: read from PTY and update screen."
  (loop while (terminal-running-p terminal) do
    (let ((data (cl-term.pty:read-from-pty (terminal-pty terminal))))
      (when data
        (bt:with-lock-held ((terminal-lock terminal))
          (process-output terminal data)
          (when (terminal-on-update terminal)
            (funcall (terminal-on-update terminal) terminal)))))
    (sleep 0.01))) ; 10ms polling interval

(defun start-terminal (terminal &key (shell "/bin/bash"))
  "Spawn shell and start the I/O background thread."
  (when (terminal-running-p terminal)
    (error "Terminal is already running"))
  (let ((pty (cl-term.pty:spawn-shell :command shell)))
    (setf (terminal-pty terminal)       pty
          (terminal-running-p terminal) t
          (terminal-thread terminal)
          (bt:make-thread (lambda () (%io-loop terminal))
                          :name "cl-term-io")))
  terminal)

(defun stop-terminal (terminal)
  "Stop the terminal I/O thread and close the PTY."
  (setf (terminal-running-p terminal) nil)
  (when (terminal-thread terminal)
    (bt:join-thread (terminal-thread terminal) :timeout 2)
    (setf (terminal-thread terminal) nil))
  (when (terminal-pty terminal)
    (cl-term.pty:pty-close (terminal-pty terminal))
    (setf (terminal-pty terminal) nil)))

(defun send-input (terminal text)
  "Send TEXT as keyboard input to the running shell."
  (when (terminal-pty terminal)
    (cl-term.pty:write-to-pty (terminal-pty terminal) text)))

(defun receive-output (terminal)
  "Read and process any pending PTY output (for polling mode). Returns NIL."
  (when (terminal-pty terminal)
    (let ((data (cl-term.pty:read-from-pty (terminal-pty terminal))))
      (when data
        (bt:with-lock-held ((terminal-lock terminal))
          (process-output terminal data))))))
