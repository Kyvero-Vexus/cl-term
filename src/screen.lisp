;;;; cl-term/src/screen.lisp — Terminal screen buffer
;;;;
;;;; Implements a 2D grid of character cells representing the terminal display.
;;;; Each cell stores a character and text attributes (color, bold, etc.)

(in-package #:cl-term.screen)

;;; ── Text attributes ──────────────────────────────────────────────────────────

(defstruct cell-attrs
  "Text attributes for one character cell."
  (bold      nil :type boolean)
  (underline nil :type boolean)
  (blink     nil :type boolean)
  (reverse   nil :type boolean)
  (fg-color  7   :type (or null (integer 0 255)))  ; default: white
  (bg-color  0   :type (or null (integer 0 255)))) ; default: black

(defvar +default-attrs+ (make-cell-attrs)
  "Default cell attributes: no decoration, fg=white, bg=black.")

;;; ── Screen cell ──────────────────────────────────────────────────────────────

(defstruct (cell (:constructor %make-cell))
  "One character cell in the terminal screen."
  (char  #\Space :type character)
  (attrs nil))

(defun make-cell-blank ()
  (%make-cell :char #\Space :attrs (make-cell-attrs)))

;;; ── Screen buffer ────────────────────────────────────────────────────────────

(defstruct (screen (:constructor %make-screen))
  "Terminal screen buffer."
  (width  80  :type (integer 1 65535))
  (height 24  :type (integer 1 65535))
  (cells  nil :type simple-vector)   ; row-major: (aref cells (+ (* row w) col))
  (cursor-row 0 :type (integer 0 65535))
  (cursor-col 0 :type (integer 0 65535))
  (scroll-top 0 :type (integer 0 65535))    ; scroll region top (0-indexed)
  (scroll-bot 23 :type (integer 0 65535))   ; scroll region bottom
  (current-attrs nil)                        ; current drawing attrs
  (tab-stops nil :type list))               ; list of column positions

(defun make-screen (&key (width 80) (height 24))
  "Create a blank terminal screen buffer of WIDTH columns and HEIGHT rows."
  (let* ((cells (make-array (* width height) :initial-element nil)))
    (dotimes (i (* width height))
      (setf (aref cells i) (make-cell-blank)))
    (%make-screen
     :width  width
     :height height
     :cells  cells
     :cursor-row 0
     :cursor-col 0
     :scroll-top 0
     :scroll-bot (1- height)
     :current-attrs (make-cell-attrs)
     :tab-stops (loop for c from 8 below width by 8 collect c))))

(defun screen-cell-index (screen row col)
  (+ (* row (screen-width screen)) col))

(defun screen-ref (screen row col)
  "Return the CELL at (ROW, COL)."
  (aref (screen-cells screen) (screen-cell-index screen row col)))

(defun screen-cell-char (screen row col)
  (cell-char (screen-ref screen row col)))

(defun screen-cell-attrs (screen row col)
  (cell-attrs (screen-ref screen row col)))

;;; ── Cursor movement ──────────────────────────────────────────────────────────

(defun screen-clamp-cursor (screen)
  (setf (screen-cursor-row screen)
        (max 0 (min (1- (screen-height screen)) (screen-cursor-row screen)))
        (screen-cursor-col screen)
        (max 0 (min (1- (screen-width screen)) (screen-cursor-col screen)))))

(defun screen-move-cursor (screen row col)
  "Move cursor to (ROW, COL) clamped to screen bounds."
  (setf (screen-cursor-row screen) row
        (screen-cursor-col screen) col)
  (screen-clamp-cursor screen))

;;; ── Character writing ────────────────────────────────────────────────────────

(defun screen-put-char (screen char &optional attrs)
  "Write CHAR at cursor position with current attributes. Advances cursor."
  (let* ((row (screen-cursor-row screen))
         (col (screen-cursor-col screen))
         (w   (screen-width screen))
         (h   (screen-height screen)))
    (when (and (< row h) (< col w))
      (let ((cell (screen-ref screen row col)))
        (setf (cell-char  cell) char
              (cell-attrs cell) (or attrs (screen-current-attrs screen)))))
    ;; Advance cursor
    (incf (screen-cursor-col screen))
    (when (>= (screen-cursor-col screen) w)
      ;; Line wrap
      (setf (screen-cursor-col screen) 0)
      (incf (screen-cursor-row screen))
      (when (> (screen-cursor-row screen) (screen-scroll-bot screen))
        (screen-scroll-up screen 1)
        (setf (screen-cursor-row screen) (screen-scroll-bot screen))))))

;;; ── Scrolling ────────────────────────────────────────────────────────────────

(defun screen-scroll-up (screen n)
  "Scroll the scroll region up by N lines (content moves up, bottom cleared)."
  (let* ((top (screen-scroll-top screen))
         (bot (screen-scroll-bot screen))
         (w   (screen-width screen)))
    (dotimes (i n)
      ;; Shift rows up
      (loop for row from top to (1- bot) do
        (loop for col from 0 below w do
          (setf (aref (screen-cells screen) (screen-cell-index screen row col))
                (aref (screen-cells screen) (screen-cell-index screen (1+ row) col)))))
      ;; Clear bottom row
      (loop for col from 0 below w do
        (setf (aref (screen-cells screen) (screen-cell-index screen bot col))
              (make-cell-blank))))))

(defun screen-scroll-down (screen n)
  "Scroll the scroll region down by N lines (content moves down, top cleared)."
  (let* ((top (screen-scroll-top screen))
         (bot (screen-scroll-bot screen))
         (w   (screen-width screen)))
    (dotimes (i n)
      (loop for row from bot downto (1+ top) do
        (loop for col from 0 below w do
          (setf (aref (screen-cells screen) (screen-cell-index screen row col))
                (aref (screen-cells screen) (screen-cell-index screen (1- row) col)))))
      (loop for col from 0 below w do
        (setf (aref (screen-cells screen) (screen-cell-index screen top col))
              (make-cell-blank))))))

;;; ── Clearing ─────────────────────────────────────────────────────────────────

(defun screen-clear (screen)
  "Clear entire screen and reset cursor to (0,0)."
  (let ((n (* (screen-width screen) (screen-height screen))))
    (dotimes (i n)
      (setf (aref (screen-cells screen) i) (make-cell-blank))))
  (screen-move-cursor screen 0 0))

(defun screen-clear-line (screen row)
  "Clear entire line ROW."
  (loop for col from 0 below (screen-width screen) do
    (setf (aref (screen-cells screen) (screen-cell-index screen row col))
          (make-cell-blank))))

(defun screen-erase-to-eol (screen)
  "Erase from cursor to end of line."
  (let ((row (screen-cursor-row screen))
        (col (screen-cursor-col screen)))
    (loop for c from col below (screen-width screen) do
      (setf (aref (screen-cells screen) (screen-cell-index screen row c))
            (make-cell-blank)))))

(defun screen-erase-to-sol (screen)
  "Erase from start of line to cursor."
  (let ((row (screen-cursor-row screen))
        (col (screen-cursor-col screen)))
    (loop for c from 0 to col do
      (setf (aref (screen-cells screen) (screen-cell-index screen row c))
            (make-cell-blank)))))

(defun screen-insert-line (screen n)
  "Insert N blank lines at cursor row within scroll region."
  (let* ((top (screen-cursor-row screen))
         (bot (screen-scroll-bot screen))
         (w   (screen-width screen)))
    (dotimes (i n)
      ;; Shift rows down from bot to top+1
      (loop for row from bot downto (1+ top) do
        (loop for col from 0 below w do
          (setf (aref (screen-cells screen) (screen-cell-index screen row col))
                (aref (screen-cells screen) (screen-cell-index screen (1- row) col)))))
      (screen-clear-line screen top))))

(defun screen-delete-line (screen n)
  "Delete N lines at cursor row within scroll region."
  (let* ((top (screen-cursor-row screen))
         (bot (screen-scroll-bot screen))
         (w   (screen-width screen)))
    (dotimes (i n)
      (loop for row from top to (1- bot) do
        (loop for col from 0 below w do
          (setf (aref (screen-cells screen) (screen-cell-index screen row col))
                (aref (screen-cells screen) (screen-cell-index screen (1+ row) col)))))
      (screen-clear-line screen bot))))
