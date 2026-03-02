;;;; cl-term/src/ansi.lisp — ANSI/VT100 escape sequence parser
;;;;
;;;; Implements a state machine that processes a byte stream and emits
;;;; parsed ANSI escape sequences. Follows the VT100/ECMA-48 standard.

(in-package #:cl-term.ansi)

;;; ── Data types ───────────────────────────────────────────────────────────────

(defstruct ansi-sequence
  "A parsed ANSI/VT100 control sequence."
  (type         :csi  :type keyword)  ; :csi :osc :dcs :esc :ctrl
  (params       nil   :type list)     ; list of integer parameters
  (intermediate nil   :type list)     ; intermediate bytes (rare)
  (final        nil   :type (or null character)); final byte (e.g. #\m for SGR)
  (data         nil))                 ; raw data for OSC/DCS

;;; ── Parser state machine ─────────────────────────────────────────────────────

(deftype parser-state ()
  '(member :ground :escape :csi-entry :csi-param :csi-intermediate
           :osc-string :dcs-string))

(defstruct (ansi-parser (:constructor %make-ansi-parser))
  "Streaming ANSI escape sequence parser."
  (state        :ground :type parser-state)
  (params       nil)        ; accumulated CSI parameters
  (current-param 0)         ; parameter being accumulated
  (intermediate nil)        ; accumulated intermediate bytes
  (osc-data     nil)        ; accumulated OSC/DCS string
  (result       nil))       ; list of completed sequences (reversed)

(defun make-ansi-parser ()
  (%make-ansi-parser))

(defun %emit (parser seq)
  "Add a completed sequence to parser results."
  (push seq (ansi-parser-result parser)))

(defun %flush-param (parser)
  "Finalize the current CSI parameter."
  (when (or (ansi-parser-params parser) (plusp (ansi-parser-current-param parser)))
    (push (ansi-parser-current-param parser) (ansi-parser-params parser)))
  (setf (ansi-parser-current-param parser) 0))

(defun %reset-csi (parser)
  (setf (ansi-parser-params parser) nil
        (ansi-parser-current-param parser) 0
        (ansi-parser-intermediate parser) nil))

(defun parser-feed (parser char)
  "Feed character CHAR into PARSER. Updates parser state.
Completed sequences accumulate in parser-result."
  (let ((code (char-code char)))
    (ecase (ansi-parser-state parser)

      (:ground
       (cond
         ;; C0 controls
         ((< code 32)
          (case char
            (#\Escape (setf (ansi-parser-state parser) :escape))
            (t (%emit parser (make-ansi-sequence :type :ctrl :final char)))))
         ;; Printable
         (t (%emit parser (make-ansi-sequence :type :print :final char)))))

      (:escape
       (cond
         ;; ESC [ → CSI
         ((char= char #\[)
          (%reset-csi parser)
          (setf (ansi-parser-state parser) :csi-entry))
         ;; ESC ] → OSC
         ((char= char #\])
          (setf (ansi-parser-osc-data parser) nil
                (ansi-parser-state parser) :osc-string))
         ;; ESC P → DCS
         ((char= char #\P)
          (setf (ansi-parser-osc-data parser) nil
                (ansi-parser-state parser) :dcs-string))
         ;; Two-character ESC sequence
         (t
          (%emit parser (make-ansi-sequence :type :esc :final char))
          (setf (ansi-parser-state parser) :ground))))

      (:csi-entry
       (cond
         ;; Digit → start of parameter
         ((digit-char-p char)
          (setf (ansi-parser-current-param parser)
                (digit-char-p char))
          (setf (ansi-parser-state parser) :csi-param))
         ;; ; → empty first parameter
         ((char= char #\;)
          (push 0 (ansi-parser-params parser))
          (setf (ansi-parser-state parser) :csi-param))
         ;; Private marker bytes < = > ? before params
         ((and (>= code 60) (<= code 63))
          (push char (ansi-parser-intermediate parser))
          (setf (ansi-parser-state parser) :csi-param))
         ;; Intermediate byte
         ((and (>= code 32) (<= code 47))
          (push char (ansi-parser-intermediate parser))
          (setf (ansi-parser-state parser) :csi-intermediate))
         ;; Final byte → emit immediately with no params
         ((and (>= code 64) (<= code 126))
          (%emit parser (make-ansi-sequence :type :csi :params nil
                                             :intermediate nil :final char))
          (setf (ansi-parser-state parser) :ground))
         ;; CAN/SUB → abort
         ((member char '(#\Can #\Sub))
          (setf (ansi-parser-state parser) :ground))))

      (:csi-param
       (cond
         ((digit-char-p char)
          (setf (ansi-parser-current-param parser)
                (+ (* (ansi-parser-current-param parser) 10)
                   (digit-char-p char))))
         ((char= char #\;)
          (%flush-param parser))
         ((and (>= code 64) (<= code 126))
          ;; Final byte — emit sequence
          (%flush-param parser)
          (%emit parser
                 (make-ansi-sequence
                  :type :csi
                  :params (nreverse (ansi-parser-params parser))
                  :intermediate (nreverse (ansi-parser-intermediate parser))
                  :final char))
          (%reset-csi parser)
          (setf (ansi-parser-state parser) :ground))
         ((and (>= code 32) (<= code 47))
          (%flush-param parser)
          (push char (ansi-parser-intermediate parser))
          (setf (ansi-parser-state parser) :csi-intermediate))))

      (:csi-intermediate
       (cond
         ((and (>= code 32) (<= code 47))
          (push char (ansi-parser-intermediate parser)))
         ((and (>= code 64) (<= code 126))
          (%emit parser
                 (make-ansi-sequence
                  :type :csi
                  :params (nreverse (ansi-parser-params parser))
                  :intermediate (nreverse (ansi-parser-intermediate parser))
                  :final char))
          (%reset-csi parser)
          (setf (ansi-parser-state parser) :ground))))

      (:osc-string
       (cond
         ;; ST (ESC \) or BEL terminate OSC
         ((or (char= char #\Bell)
              (char= char (code-char 156)))
          (%emit parser (make-ansi-sequence :type :osc
                                             :data (coerce (nreverse (ansi-parser-osc-data parser)) 'string)))
          (setf (ansi-parser-state parser) :ground))
         ((char= char #\Escape)
          ;; ESC \ terminates — ESC goes here, next char should be \
          ;; Simplified: just emit on next char
          nil)
         (t
          (push char (ansi-parser-osc-data parser)))))

      (:dcs-string
       (cond
         ((or (char= char (code-char 156))
              (and (char= char #\\) (eq (ansi-parser-state parser) :dcs-string)))
          (%emit parser (make-ansi-sequence :type :dcs
                                             :data (coerce (nreverse (ansi-parser-osc-data parser)) 'string)))
          (setf (ansi-parser-state parser) :ground))
         (t
          (push char (ansi-parser-osc-data parser))))))))

(defun parse-ansi-sequences (string)
  "Parse STRING and return a list of ANSI-SEQUENCE objects."
  (let ((parser (make-ansi-parser)))
    (loop for ch across string do (parser-feed parser ch))
    (nreverse (ansi-parser-result parser))))

;;; ── SGR (Select Graphic Rendition) helpers ───────────────────────────────────

(defun sgr-bold      (seq) (member 1 (ansi-sequence-params seq)))
(defun sgr-underline (seq) (member 4 (ansi-sequence-params seq)))
(defun sgr-blink     (seq) (member 5 (ansi-sequence-params seq)))
(defun sgr-reverse   (seq) (member 7 (ansi-sequence-params seq)))
(defun sgr-reset     (seq) (or (null (ansi-sequence-params seq))
                               (member 0 (ansi-sequence-params seq))))

(defun sgr-fg-color (seq)
  "Return foreground color index (0-255) from SGR sequence, or NIL."
  (let ((params (ansi-sequence-params seq)))
    (cond
      ;; Standard colors 30-37
      ((some (lambda (p) (and (>= p 30) (<= p 37))) params)
       (- (find-if (lambda (p) (and (>= p 30) (<= p 37))) params) 30))
      ;; Extended: 38;5;n
      ((and (member 38 params)
            (let ((pos (position 38 params)))
              (and pos
                   (>= (length params) (+ pos 2))
                   (= (nth (1+ pos) params) 5))))
       (let ((pos (position 38 params)))
         (nth (+ pos 2) params)))
      (t nil))))

(defun sgr-bg-color (seq)
  "Return background color index (0-255) from SGR sequence, or NIL."
  (let ((params (ansi-sequence-params seq)))
    (cond
      ((some (lambda (p) (and (>= p 40) (<= p 47))) params)
       (- (find-if (lambda (p) (and (>= p 40) (<= p 47))) params) 40))
      ((and (member 48 params)
            (let ((pos (position 48 params)))
              (and pos (>= (length params) (+ pos 2))
                   (= (nth (1+ pos) params) 5))))
       (let ((pos (position 48 params)))
         (nth (+ pos 2) params)))
      (t nil))))
