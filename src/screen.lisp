;;;; cl-term/src/screen.lisp — Terminal screen buffer

(in-package #:cl-term.screen)

(defstruct cell-attrs
  (bold nil :type boolean)
  (underline nil :type boolean)
  (blink nil :type boolean)
  (reverse nil :type boolean)
  (fg-color 7 :type (or null (integer 0 255)))
  (bg-color 0 :type (or null (integer 0 255))))

(defstruct (cell (:constructor %make-cell))
  (char #\Space :type character)
  (attrs nil))

(defun make-cell-blank ()
  (%make-cell :char #\Space :attrs (make-cell-attrs)))

(defstruct (screen (:constructor %make-screen))
  (width 80 :type (integer 1 65535))
  (height 24 :type (integer 1 65535))
  (cells nil :type simple-vector)
  (normal-cells nil)
  (alt-cells nil)
  (using-alt nil :type boolean)
  (scrollback nil :type list)
  (max-scrollback 2000 :type (integer 100 200000))
  (cursor-row 0 :type (integer 0 65535))
  (cursor-col 0 :type (integer 0 65535))
  (saved-cursor-row 0 :type (integer 0 65535))
  (saved-cursor-col 0 :type (integer 0 65535))
  (scroll-top 0 :type (integer 0 65535))
  (scroll-bot 23 :type (integer 0 65535))
  (current-attrs nil)
  (tab-stops nil :type list))

(defun make-screen (&key (width 80) (height 24) (scrollback-limit 2000))
  (flet ((blank-cells ()
           (let ((v (make-array (* width height) :initial-element nil)))
             (dotimes (i (* width height))
               (setf (aref v i) (make-cell-blank)))
             v)))
    (let* ((normal (blank-cells))
           (alt (blank-cells)))
      (%make-screen
       :width width
       :height height
       :cells normal
       :normal-cells normal
       :alt-cells alt
       :scrollback nil
       :max-scrollback scrollback-limit
       :cursor-row 0
       :cursor-col 0
       :saved-cursor-row 0
       :saved-cursor-col 0
       :scroll-top 0
       :scroll-bot (1- height)
       :current-attrs (make-cell-attrs)
       :tab-stops (loop for c from 8 below width by 8 collect c)))))

(defun screen-cell-index (screen row col)
  (+ (* row (screen-width screen)) col))

(defun screen-ref (screen row col)
  (aref (screen-cells screen) (screen-cell-index screen row col)))

(defun screen-cell-char (screen row col)
  (cell-char (screen-ref screen row col)))

(defun screen-cell-attrs (screen row col)
  (cell-attrs (screen-ref screen row col)))

(defun screen-clamp-cursor (screen)
  (setf (screen-cursor-row screen)
        (max 0 (min (1- (screen-height screen)) (screen-cursor-row screen)))
        (screen-cursor-col screen)
        (max 0 (min (1- (screen-width screen)) (screen-cursor-col screen)))))

(defun screen-move-cursor (screen row col)
  (setf (screen-cursor-row screen) row
        (screen-cursor-col screen) col)
  (screen-clamp-cursor screen))

(defun char-display-width (char)
  (let ((cp (char-code char)))
    (cond
      ((or (<= #x0300 cp #x036F)
           (<= #x1AB0 cp #x1AFF)
           (<= #x1DC0 cp #x1DFF)
           (<= #x20D0 cp #x20FF)
           (<= #xFE20 cp #xFE2F)) 0)
      ((or (<= #x1100 cp #x115F)
           (<= #x2329 cp #x232A)
           (<= #x2E80 cp #xA4CF)
           (<= #xAC00 cp #xD7A3)
           (<= #xF900 cp #xFAFF)
           (<= #xFE10 cp #xFE19)
           (<= #xFE30 cp #xFE6F)
           (<= #xFF00 cp #xFF60)
           (<= #xFFE0 cp #xFFE6)
           (<= #x1F300 cp #x1FAFF)) 2)
      (t 1))))

(defun screen-put-char (screen char &optional attrs)
  (let* ((row (screen-cursor-row screen))
         (col (screen-cursor-col screen))
         (w (screen-width screen))
         (h (screen-height screen))
         (cw (char-display-width char))
         (a (or attrs (screen-current-attrs screen))))
    (when (and (< row h) (< col w))
      (let ((cell (screen-ref screen row col)))
        (setf (cell-char cell) char
              (cell-attrs cell) a))
      (when (= cw 2)
        (when (< (1+ col) w)
          (let ((next (screen-ref screen row (1+ col))))
            (setf (cell-char next) #\Space
                  (cell-attrs next) a)))))
    (incf (screen-cursor-col screen) (max 1 cw))
    (when (>= (screen-cursor-col screen) w)
      (setf (screen-cursor-col screen) 0)
      (incf (screen-cursor-row screen))
      (when (> (screen-cursor-row screen) (screen-scroll-bot screen))
        (screen-scroll-up screen 1)
        (setf (screen-cursor-row screen) (screen-scroll-bot screen))))))

(defun screen-line-string (screen row)
  (coerce
   (loop for col from 0 below (screen-width screen)
         collect (screen-cell-char screen row col))
   'string))

(defun %trim-scrollback (screen)
  (let ((max (screen-max-scrollback screen)))
    (when (> (length (screen-scrollback screen)) max)
      (setf (screen-scrollback screen)
            (subseq (screen-scrollback screen)
                    (- (length (screen-scrollback screen)) max))))))

(defun screen-set-max-scrollback (screen n)
  (setf (screen-max-scrollback screen) (max 100 n))
  (%trim-scrollback screen)
  screen)

(defun screen-search-scrollback (screen query &key (case-sensitive nil) (limit 200))
  (let* ((needle (if case-sensitive query (string-downcase query)))
         (rows (screen-scrollback screen))
         (hits nil))
    (loop for line in (reverse rows)
          for idx from 0
          for text = (if case-sensitive line (string-downcase line))
          do (when (search needle text)
               (push line hits)
               (when (>= (length hits) limit)
                 (return))))
    (nreverse hits)))

(defun screen-switch-alt (screen)
  (unless (screen-using-alt screen)
    (setf (screen-normal-cells screen) (screen-cells screen)
          (screen-saved-cursor-row screen) (screen-cursor-row screen)
          (screen-saved-cursor-col screen) (screen-cursor-col screen)
          (screen-cells screen) (screen-alt-cells screen)
          (screen-using-alt screen) t)
    (screen-clear screen)))

(defun screen-switch-normal (screen)
  (when (screen-using-alt screen)
    (setf (screen-alt-cells screen) (screen-cells screen)
          (screen-cells screen) (screen-normal-cells screen)
          (screen-using-alt screen) nil)
    (screen-move-cursor screen
                        (screen-saved-cursor-row screen)
                        (screen-saved-cursor-col screen))))

(defun screen-scroll-up (screen n)
  (let* ((top (screen-scroll-top screen))
         (bot (screen-scroll-bot screen))
         (w (screen-width screen)))
    (dotimes (_ n)
      (when (and (not (screen-using-alt screen)) (= top 0))
        (push (screen-line-string screen 0) (screen-scrollback screen))
        (%trim-scrollback screen))
      (loop for row from top to (1- bot) do
        (loop for col from 0 below w do
          (setf (aref (screen-cells screen) (screen-cell-index screen row col))
                (aref (screen-cells screen) (screen-cell-index screen (1+ row) col)))))
      (loop for col from 0 below w do
        (setf (aref (screen-cells screen) (screen-cell-index screen bot col))
              (make-cell-blank))))))

(defun screen-scroll-down (screen n)
  (let* ((top (screen-scroll-top screen))
         (bot (screen-scroll-bot screen))
         (w (screen-width screen)))
    (dotimes (_ n)
      (loop for row from bot downto (1+ top) do
        (loop for col from 0 below w do
          (setf (aref (screen-cells screen) (screen-cell-index screen row col))
                (aref (screen-cells screen) (screen-cell-index screen (1- row) col)))))
      (loop for col from 0 below w do
        (setf (aref (screen-cells screen) (screen-cell-index screen top col))
              (make-cell-blank))))))

(defun screen-clear (screen)
  (let ((n (* (screen-width screen) (screen-height screen))))
    (dotimes (i n)
      (setf (aref (screen-cells screen) i) (make-cell-blank))))
  (screen-move-cursor screen 0 0))

(defun screen-clear-line (screen row)
  (loop for col from 0 below (screen-width screen) do
    (setf (aref (screen-cells screen) (screen-cell-index screen row col))
          (make-cell-blank))))

(defun screen-erase-to-eol (screen)
  (let ((row (screen-cursor-row screen))
        (col (screen-cursor-col screen)))
    (loop for c from col below (screen-width screen) do
      (setf (aref (screen-cells screen) (screen-cell-index screen row c))
            (make-cell-blank)))))

(defun screen-erase-to-sol (screen)
  (let ((row (screen-cursor-row screen))
        (col (screen-cursor-col screen)))
    (loop for c from 0 to col do
      (setf (aref (screen-cells screen) (screen-cell-index screen row c))
            (make-cell-blank)))))

(defun screen-insert-line (screen n)
  (let* ((top (screen-cursor-row screen))
         (bot (screen-scroll-bot screen))
         (w (screen-width screen)))
    (dotimes (_ n)
      (loop for row from bot downto (1+ top) do
        (loop for col from 0 below w do
          (setf (aref (screen-cells screen) (screen-cell-index screen row col))
                (aref (screen-cells screen) (screen-cell-index screen (1- row) col)))))
      (screen-clear-line screen top))))

(defun screen-delete-line (screen n)
  (let* ((top (screen-cursor-row screen))
         (bot (screen-scroll-bot screen))
         (w (screen-width screen)))
    (dotimes (_ n)
      (loop for row from top to (1- bot) do
        (loop for col from 0 below w do
          (setf (aref (screen-cells screen) (screen-cell-index screen row col))
                (aref (screen-cells screen) (screen-cell-index screen (1+ row) col)))))
      (screen-clear-line screen bot))))
