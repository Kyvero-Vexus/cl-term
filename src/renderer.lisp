;;;; cl-term/src/renderer.lisp — Terminal screen renderer
;;;;
;;;; Renders cl-term screen buffers to plain text or ANSI escape sequences.

(in-package #:cl-term.renderer)

;;; ── 256-color palette ────────────────────────────────────────────────────────

(defun color-256-to-rgb (index)
  "Convert a 256-color index to (R G B) triplet (0-255 each).
  0-15: standard/bright colors (xterm defaults)
  16-231: 6x6x6 color cube
  232-255: grayscale ramp"
  (cond
    ((< index 16)
     (nth index
          '((0 0 0) (128 0 0) (0 128 0) (128 128 0)
            (0 0 128) (128 0 128) (0 128 128) (192 192 192)
            (128 128 128) (255 0 0) (0 255 0) (255 255 0)
            (0 0 255) (255 0 255) (0 255 255) (255 255 255))))
    ((< index 232)
     (let* ((n (- index 16))
            (b (mod n 6))
            (g (mod (floor n 6) 6))
            (r (floor n 36))
            (s (lambda (x) (if (zerop x) 0 (+ 55 (* x 40))))))
       (list (funcall s r) (funcall s g) (funcall s b))))
    (t
     (let ((v (+ 8 (* (- index 232) 10))))
       (list v v v)))))

;;; ── ANSI SGR helpers ─────────────────────────────────────────────────────────

(defun sgr-reset-str () (format nil "~C[0m" #\Escape))

(defun sgr-color-str (color-idx fg-p)
  "Return ANSI SGR string for foreground (FG-P=T) or background color."
  (when color-idx
    (if (< color-idx 8)
        (format nil "~C[~Am" #\Escape (+ (if fg-p 30 40) color-idx))
        (format nil "~C[~A;5;~Am" #\Escape (if fg-p 38 48) color-idx))))

(defun sgr-attrs-str (attrs)
  "Return ANSI SGR escape string to reproduce cell-attrs ATTRS."
  (with-output-to-string (s)
    (write-string (sgr-reset-str) s)
    (when (cl-term.screen:cell-attrs-bold attrs)
      (format s "~C[1m" #\Escape))
    (when (cl-term.screen:cell-attrs-underline attrs)
      (format s "~C[4m" #\Escape))
    (when (cl-term.screen:cell-attrs-blink attrs)
      (format s "~C[5m" #\Escape))
    (when (cl-term.screen:cell-attrs-reverse attrs)
      (format s "~C[7m" #\Escape))
    (let ((fg (cl-term.screen:cell-attrs-fg-color attrs))
          (bg (cl-term.screen:cell-attrs-bg-color attrs)))
      (when fg (write-string (or (sgr-color-str fg t) "") s))
      (when bg (write-string (or (sgr-color-str bg nil) "") s)))))

;;; ── Plain-text renderer ──────────────────────────────────────────────────────

(defun render-screen-plain (screen)
  "Render SCREEN to a plain text string, stripping all ANSI attributes."
  (check-type screen cl-term.screen:screen)
  (with-output-to-string (s)
    (let ((h (cl-term.screen:screen-height screen))
          (w (cl-term.screen:screen-width screen)))
      (dotimes (row h)
        (dotimes (col w)
          (write-char (cl-term.screen:screen-cell-char screen row col) s))
        (write-char #\Newline s)))))

;;; ── ANSI renderer ────────────────────────────────────────────────────────────

(defun render-screen-ansi (screen)
  "Render SCREEN with ANSI escape sequences (for terminal-in-terminal display)."
  (check-type screen cl-term.screen:screen)
  (with-output-to-string (s)
    (let ((h (cl-term.screen:screen-height screen))
          (w (cl-term.screen:screen-width screen))
          (last-attrs nil))
      (format s "~C[H" #\Escape)  ; home cursor
      (dotimes (row h)
        (dotimes (col w)
          (let* ((attrs (cl-term.screen:screen-cell-attrs screen row col))
                 (ch    (cl-term.screen:screen-cell-char  screen row col)))
            (unless (and last-attrs (equalp attrs last-attrs))
              (write-string (sgr-attrs-str attrs) s)
              (setf last-attrs attrs))
            (write-char ch s)))
        (write-char #\Newline s))
      (write-string (sgr-reset-str) s))))

;;; ── Screen dump (debug) ──────────────────────────────────────────────────────

(defun dump-screen (screen &optional (stream *standard-output*))
  "Print a human-readable dump of SCREEN to STREAM."
  (let ((h (cl-term.screen:screen-height screen))
        (w (cl-term.screen:screen-width  screen)))
    (format stream "~&=== Screen ~Ax~A cursor=~A:~A ===~%"
            w h
            (cl-term.screen:screen-cursor-row screen)
            (cl-term.screen:screen-cursor-col screen))
    (dotimes (row h)
      (write-char #\| stream)
      (dotimes (col w)
        (write-char (cl-term.screen:screen-cell-char screen row col) stream))
      (format stream "|~%"))
    (format stream "===~%")))
