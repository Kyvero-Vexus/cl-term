;;;; cl-term/src/terminal.lisp — Terminal state machine

(in-package #:cl-term.terminal)

(defstruct terminal-tab
  (id 0 :type fixnum)
  (title "shell" :type string)
  (screen nil)
  (parser nil))

(defun %make-default-keymap ()
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "Enter" ht) (string #\Return)
          (gethash "Tab" ht) (string #\Tab)
          (gethash "Backspace" ht) (string #\Backspace)
          (gethash "C-c" ht) (string (code-char 3))
          (gethash "C-d" ht) (string (code-char 4))
          (gethash "C-z" ht) (string (code-char 26))
          (gethash "Up" ht) (format nil "~C[A" #\Escape)
          (gethash "Down" ht) (format nil "~C[B" #\Escape)
          (gethash "Right" ht) (format nil "~C[C" #\Escape)
          (gethash "Left" ht) (format nil "~C[D" #\Escape))
    ht))

(defstruct (terminal (:constructor %make-terminal))
  (pty    nil)
  (screen nil)
  (parser nil)
  (thread nil)
  (running-p nil :type boolean)
  (on-update nil)
  (lock     nil)
  (config (cl-term.config:make-terminal-config))
  (key-bindings (%make-default-keymap))
  (tabs nil :type list)
  (active-tab-id 0 :type fixnum)
  (next-tab-id 1 :type fixnum)
  (panes '(:single) :type list))

(defun make-terminal (&key (width 80) (height 24) on-update config)
  (let* ((cfg (or config (cl-term.config:make-terminal-config)))
         (screen (cl-term.screen:make-screen
                  :width width :height height
                  :scrollback-limit (cl-term.config:terminal-config-scrollback-limit cfg)))
         (parser (cl-term.ansi:make-ansi-parser))
         (tab (make-terminal-tab :id 0 :screen screen :parser parser :title "shell")))
    (%make-terminal
     :screen screen
     :parser parser
     :running-p nil
     :on-update on-update
     :lock (bt:make-lock "terminal-lock")
     :config cfg
     :tabs (list tab)
     :active-tab-id 0
     :next-tab-id 1)))

(defun terminal-list-tabs (terminal)
  (mapcar (lambda (tab)
            (list :id (terminal-tab-id tab)
                  :title (terminal-tab-title tab)
                  :active (= (terminal-tab-id tab) (terminal-active-tab-id terminal))))
          (terminal-tabs terminal)))

(defun terminal-new-tab (terminal &key title)
  (let* ((id (terminal-next-tab-id terminal))
         (w (cl-term.screen:screen-width (terminal-screen terminal)))
         (h (cl-term.screen:screen-height (terminal-screen terminal)))
         (screen (cl-term.screen:make-screen :width w :height h
                  :scrollback-limit (cl-term.config:terminal-config-scrollback-limit
                                     (terminal-config terminal))))
         (tab (make-terminal-tab :id id :title (or title (format nil "tab-~D" id))
                                 :screen screen :parser (cl-term.ansi:make-ansi-parser))))
    (push tab (terminal-tabs terminal))
    (incf (terminal-next-tab-id terminal))
    tab))

(defun terminal-switch-tab (terminal tab-id)
  (let ((tab (find tab-id (terminal-tabs terminal) :key #'terminal-tab-id)))
    (when tab
      (setf (terminal-active-tab-id terminal) tab-id
            (terminal-screen terminal) (terminal-tab-screen tab)
            (terminal-parser terminal) (terminal-tab-parser tab))
      t)))

(defun terminal-rename-tab (terminal tab-id new-title)
  (let ((tab (find tab-id (terminal-tabs terminal) :key #'terminal-tab-id)))
    (when tab
      (setf (terminal-tab-title tab) new-title)
      tab)))

(defun terminal-close-tab (terminal tab-id)
  (let* ((tabs (terminal-tabs terminal))
         (tab (find tab-id tabs :key #'terminal-tab-id)))
    (when (and tab (> (length tabs) 1))
      (setf (terminal-tabs terminal) (remove tab tabs :test #'eq))
      (when (= tab-id (terminal-active-tab-id terminal))
        (let ((next (first (terminal-tabs terminal))))
          (setf (terminal-active-tab-id terminal) (terminal-tab-id next)
                (terminal-screen terminal) (terminal-tab-screen next)
                (terminal-parser terminal) (terminal-tab-parser next))))
      t)))

(defun terminal-bind-key (terminal key chord-output)
  (setf (gethash key (terminal-key-bindings terminal)) chord-output)
  terminal)

(defun send-key (terminal key)
  (let ((mapped (gethash key (terminal-key-bindings terminal))))
    (when mapped
      (send-input terminal mapped)
      t)))

(defun terminal-search-scrollback (terminal query &key (case-sensitive nil) (limit 200))
  (cl-term.screen:screen-search-scrollback (terminal-screen terminal) query
                                           :case-sensitive case-sensitive
                                           :limit limit))

(defun apply-sgr (terminal seq)
  (let ((screen (terminal-screen terminal))
        (attrs  (cl-term.screen:screen-current-attrs (terminal-screen terminal))))
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

(defun dispatch-csi (terminal seq)
  (let* ((screen (terminal-screen terminal))
         (params (cl-term.ansi:ansi-sequence-params seq))
         (p1     (or (first params) 0))
         (p2     (or (second params) 0)))
    (case (cl-term.ansi:ansi-sequence-final seq)
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
      ((#\H #\f)
       (cl-term.screen:screen-move-cursor screen (max 0 (1- (max 1 p1))) (max 0 (1- (max 1 p2)))))
      (#\J
       (case p1
         (0 (cl-term.screen:screen-erase-to-eol screen)
            (loop for row from (1+ (cl-term.screen:screen-cursor-row screen))
                  below (cl-term.screen:screen-height screen)
                  do (cl-term.screen:screen-clear-line screen row)))
         (1 (loop for row from 0 below (cl-term.screen:screen-cursor-row screen)
                  do (cl-term.screen:screen-clear-line screen row))
            (cl-term.screen:screen-erase-to-sol screen))
         (2 (cl-term.screen:screen-clear screen))))
      (#\K
       (case p1
         (0 (cl-term.screen:screen-erase-to-eol screen))
         (1 (cl-term.screen:screen-erase-to-sol screen))
         (2 (cl-term.screen:screen-clear-line screen (cl-term.screen:screen-cursor-row screen)))))
      (#\L (cl-term.screen:screen-insert-line screen (max 1 p1)))
      (#\M (cl-term.screen:screen-delete-line screen (max 1 p1)))
      (#\S (cl-term.screen:screen-scroll-up screen (max 1 p1)))
      (#\T (cl-term.screen:screen-scroll-down screen (max 1 p1)))
      (#\m (apply-sgr terminal seq))
      (#\r
       (let ((top (max 0 (1- (max 1 p1))))
             (bot (min (1- (cl-term.screen:screen-height screen))
                       (1- (if (zerop p2) (cl-term.screen:screen-height screen) p2)))))
         (when (< top bot)
           (setf (cl-term.screen:screen-scroll-top (terminal-screen terminal)) top
                 (cl-term.screen:screen-scroll-bot (terminal-screen terminal)) bot)
           (cl-term.screen:screen-move-cursor screen 0 0))))
      ((#\h #\l)
       (let ((private-p (member #\? (cl-term.ansi:ansi-sequence-intermediate seq)))
             (set-p (char= (cl-term.ansi:ansi-sequence-final seq) #\h)))
         (when private-p
           (case p1
             ((47 1049)
              (if set-p
                  (cl-term.screen:screen-switch-alt screen)
                  (cl-term.screen:screen-switch-normal screen)))))))
      (otherwise nil))))

(defun process-output (terminal data)
  (let ((screen (terminal-screen terminal))
        (parser (terminal-parser terminal)))
    (loop for char across data do (cl-term.ansi:parser-feed parser char))
    (let ((seqs (nreverse (cl-term.ansi:ansi-parser-result parser))))
      (setf (cl-term.ansi:ansi-parser-result parser) nil)
      (loop for seq in seqs do
        (case (cl-term.ansi:ansi-sequence-type seq)
          (:print (cl-term.screen:screen-put-char screen (cl-term.ansi:ansi-sequence-final seq)))
          (:ctrl
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
              (let* ((col (cl-term.screen:screen-cursor-col screen))
                     (stops (cl-term.screen:screen-tab-stops screen))
                     (next (find-if (lambda (s) (> s col)) stops)))
                (if next
                    (setf (cl-term.screen:screen-cursor-col screen) next)
                    (setf (cl-term.screen:screen-cursor-col screen)
                          (1- (cl-term.screen:screen-width screen))))))))
          (:csi (dispatch-csi terminal seq))
          (otherwise nil))))))

(defun %io-loop (terminal)
  (loop while (terminal-running-p terminal) do
    (let ((data (cl-term.pty:read-from-pty (terminal-pty terminal))))
      (when data
        (bt:with-lock-held ((terminal-lock terminal))
          (process-output terminal data)
          (when (terminal-on-update terminal)
            (funcall (terminal-on-update terminal) terminal)))))
    (sleep 0.01)))

(defun start-terminal (terminal &key shell)
  (when (terminal-running-p terminal)
    (error "Terminal is already running"))
  (let* ((command (or shell (cl-term.config:terminal-config-shell (terminal-config terminal))))
         (pty (cl-term.pty:spawn-shell :command command)))
    (setf (terminal-pty terminal) pty
          (terminal-running-p terminal) t
          (terminal-thread terminal) (bt:make-thread (lambda () (%io-loop terminal)) :name "cl-term-io")))
  terminal)

(defun stop-terminal (terminal)
  (setf (terminal-running-p terminal) nil)
  (when (terminal-thread terminal)
    (bt:join-thread (terminal-thread terminal))
    (setf (terminal-thread terminal) nil))
  (when (terminal-pty terminal)
    (cl-term.pty:pty-close (terminal-pty terminal))
    (setf (terminal-pty terminal) nil)))

(defun send-input (terminal text)
  (when (terminal-pty terminal)
    (cl-term.pty:write-to-pty (terminal-pty terminal) text)))

(defun receive-output (terminal)
  (when (terminal-pty terminal)
    (let ((data (cl-term.pty:read-from-pty (terminal-pty terminal))))
      (when data
        (bt:with-lock-held ((terminal-lock terminal))
          (process-output terminal data))))))
