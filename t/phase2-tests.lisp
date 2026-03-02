(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))
(push (truename "/home/slime/.openclaw/workspace-gensym/projects/cl-term/") asdf:*central-registry*)
(ql:quickload :cl-term :silent t)

(defvar *pass* 0)
(defvar *fail* 0)

(defmacro test (name &body body)
  `(handler-case
     (progn ,@body
            (format t "PASS: ~A~%" ,name)
            (incf *pass*))
     (error (e)
       (format t "FAIL: ~A — ~A~%" ,name e)
       (incf *fail*))))

;;; ── Screen tests ──────────────────────────────────────────────────────────────

(test "make-screen 80x24"
  (let ((s (cl-term.screen:make-screen :width 80 :height 24)))
    (assert (= 80 (cl-term.screen:screen-width s)))
    (assert (= 24 (cl-term.screen:screen-height s)))
    (assert (= 0 (cl-term.screen:screen-cursor-row s)))
    (assert (= 0 (cl-term.screen:screen-cursor-col s)))))

(test "screen-put-char writes character"
  (let ((s (cl-term.screen:make-screen :width 10 :height 5)))
    (cl-term.screen:screen-put-char s #\A)
    (assert (char= #\A (cl-term.screen:screen-cell-char s 0 0)))
    (assert (= 1 (cl-term.screen:screen-cursor-col s)))))

(test "screen-put-char advances cursor"
  (let ((s (cl-term.screen:make-screen :width 10 :height 5)))
    (loop for ch in '(#\H #\e #\l #\l #\o)
          do (cl-term.screen:screen-put-char s ch))
    (assert (char= #\H (cl-term.screen:screen-cell-char s 0 0)))
    (assert (char= #\o (cl-term.screen:screen-cell-char s 0 4)))
    (assert (= 5 (cl-term.screen:screen-cursor-col s)))))

(test "screen-move-cursor"
  (let ((s (cl-term.screen:make-screen :width 80 :height 24)))
    (cl-term.screen:screen-move-cursor s 5 10)
    (assert (= 5 (cl-term.screen:screen-cursor-row s)))
    (assert (= 10 (cl-term.screen:screen-cursor-col s)))))

(test "screen-clear-line clears to spaces"
  (let ((s (cl-term.screen:make-screen :width 10 :height 5)))
    (loop for ch in '(#\H #\e #\l #\l #\o)
          do (cl-term.screen:screen-put-char s ch))
    (cl-term.screen:screen-clear-line s 0)
    (assert (char= #\Space (cl-term.screen:screen-cell-char s 0 0)))
    (assert (char= #\Space (cl-term.screen:screen-cell-char s 0 4)))))

(test "screen-scroll-up scrolls lines up"
  (let ((s (cl-term.screen:make-screen :width 10 :height 3)))
    ;; Write A on row 0, B on row 1
    (cl-term.screen:screen-put-char s #\A)
    (cl-term.screen:screen-move-cursor s 1 0)
    (cl-term.screen:screen-put-char s #\B)
    (cl-term.screen:screen-scroll-up s 1)
    ;; B should now be on row 0
    (assert (char= #\B (cl-term.screen:screen-cell-char s 0 0)))))

;;; ── ANSI parser tests ────────────────────────────────────────────────────────

(test "parse plain text"
  (let ((seqs (cl-term.ansi:parse-ansi-sequences "Hello")))
    (assert (= 5 (length seqs)))
    (assert (every (lambda (s) (eq :print (cl-term.ansi:ansi-sequence-type s))) seqs))
    (assert (char= #\H (cl-term.ansi:ansi-sequence-final (first seqs))))))

(test "parse CSI cursor-up"
  (let* ((input (format nil "~C[3A" #\Escape))
         (seqs (cl-term.ansi:parse-ansi-sequences input)))
    (assert (= 1 (length seqs)))
    (let ((s (first seqs)))
      (assert (eq :csi (cl-term.ansi:ansi-sequence-type s)))
      (assert (char= #\A (cl-term.ansi:ansi-sequence-final s)))
      (assert (= 3 (first (cl-term.ansi:ansi-sequence-params s)))))))

(test "parse SGR reset"
  (let* ((input (format nil "~C[0m" #\Escape))
         (seqs (cl-term.ansi:parse-ansi-sequences input)))
    (assert (= 1 (length seqs)))
    (let ((s (first seqs)))
      (assert (eq :csi (cl-term.ansi:ansi-sequence-type s)))
      (assert (char= #\m (cl-term.ansi:ansi-sequence-final s)))
      (assert (cl-term.ansi:sgr-reset s)))))

(test "parse SGR bold"
  (let* ((input (format nil "~C[1m" #\Escape))
         (seqs (cl-term.ansi:parse-ansi-sequences input)))
    (assert (cl-term.ansi:sgr-bold (first seqs)))))

(test "parse SGR fg color 30-37"
  (let* ((input (format nil "~C[31m" #\Escape))
         (seqs (cl-term.ansi:parse-ansi-sequences input)))
    (assert (= 1 (cl-term.ansi:sgr-fg-color (first seqs))))))

(test "parse SGR 256-color fg"
  (let* ((input (format nil "~C[38;5;200m" #\Escape))
         (seqs (cl-term.ansi:parse-ansi-sequences input)))
    (assert (= 200 (cl-term.ansi:sgr-fg-color (first seqs))))))

(test "parse SGR 256-color bg"
  (let* ((input (format nil "~C[48;5;100m" #\Escape))
         (seqs (cl-term.ansi:parse-ansi-sequences input)))
    (assert (= 100 (cl-term.ansi:sgr-bg-color (first seqs))))))

(test "parse mixed text and escapes"
  (let* ((input (format nil "Hi~C[31mWorld~C[0m" #\Escape #\Escape))
         (seqs (cl-term.ansi:parse-ansi-sequences input)))
    ;; 2 chars + CSI + 5 chars + CSI = 9
    (assert (= 9 (length seqs)))))

;;; ── Renderer tests ───────────────────────────────────────────────────────────

(test "render-screen-plain produces correct text"
  (let ((s (cl-term.screen:make-screen :width 10 :height 3)))
    (loop for ch in '(#\H #\e #\l #\l #\o) do (cl-term.screen:screen-put-char s ch))
    (cl-term.screen:screen-move-cursor s 1 0)
    (loop for ch in '(#\W #\o #\r #\l #\d) do (cl-term.screen:screen-put-char s ch))
    (let ((text (cl-term.renderer:render-screen-plain s)))
      (assert (search "Hello" text))
      (assert (search "World" text)))))

(test "render-screen-ansi contains ESC sequences"
  (let ((s (cl-term.screen:make-screen :width 5 :height 1)))
    (loop for ch in '(#\T #\e #\s #\t) do (cl-term.screen:screen-put-char s ch))
    (let ((text (cl-term.renderer:render-screen-ansi s)))
      (assert (find #\Escape (coerce text 'list))))))

(test "color-256-to-rgb standard colors"
  (assert (equal '(0 0 0) (cl-term.renderer:color-256-to-rgb 0)))
  (assert (equal '(255 255 255) (cl-term.renderer:color-256-to-rgb 15))))

(test "color-256-to-rgb cube"
  ;; Index 16 = (0,0,0) -> should be (0,0,0)
  (assert (equal '(0 0 0) (cl-term.renderer:color-256-to-rgb 16)))
  ;; Index 231 = max cube = (255,255,255) — 16 + 215
  (let ((rgb (cl-term.renderer:color-256-to-rgb 231)))
    (assert (= 255 (first rgb)))))

(test "color-256-to-rgb grayscale"
  (let ((rgb (cl-term.renderer:color-256-to-rgb 232)))
    (assert (= 8 (first rgb)))
    (assert (= 8 (second rgb)))
    (assert (= 8 (third rgb)))))

;;; ── Terminal integration test ─────────────────────────────────────────────────

(test "make-terminal creates structure"
  (let ((term (cl-term.terminal:make-terminal :width 80 :height 24)))
    (assert term)
    (let ((sc (cl-term.terminal:terminal-screen term)))
      (assert (= 80 (cl-term.screen:screen-width sc)))
      (assert (= 24 (cl-term.screen:screen-height sc))))))

;;; ── Summary ──────────────────────────────────────────────────────────────────
(format t "~%=== Results: ~A passed, ~A failed ===~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
