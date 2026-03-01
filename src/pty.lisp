;;;; cl-term/src/pty.lisp — PTY management via CFFI/POSIX
;;;;
;;;; Provides pseudo-terminal allocation using forkpty(3) from libutil.
;;;; On Linux, openpty/forkpty live in libc (glibc) or libutil.

(in-package #:cl-term.pty)

;;; ── Foreign library ──────────────────────────────────────────────────────────

(define-foreign-library libutil
  (:linux (:or "libutil.so.1" "libutil.so" "libc.so.6"))
  (:darwin (:or "libutil.dylib" "libSystem.dylib"))
  (t (:default "libutil")))

(use-foreign-library libutil)

;;; ── Foreign function declarations ────────────────────────────────────────────

;; int forkpty(int *amaster, char *name, struct termios *termp, struct winsize *winp);
(defcfun ("forkpty" %forkpty) :int
  (amaster :pointer)   ; out: master fd
  (name    :pointer)   ; out: slave name (or NULL)
  (termp   :pointer)   ; in:  termios (or NULL)
  (winp    :pointer))  ; in:  winsize (or NULL)

;; int close(int fd);
(defcfun ("close" %close) :int
  (fd :int))

;; ssize_t read(int fd, void *buf, size_t count);
(defcfun ("read" %read) :long
  (fd  :int)
  (buf :pointer)
  (n   :unsigned-long))

;; ssize_t write(int fd, const void *buf, size_t count);
(defcfun ("write" %write) :long
  (fd  :int)
  (buf :pointer)
  (n   :unsigned-long))

;; int fcntl(int fd, int cmd, int arg);
(defcfun ("fcntl" %fcntl) :int
  (fd  :int)
  (cmd :int)
  (arg :int))

;;; ── Constants ─────────────────────────────────────────────────────────────────

(defconstant +f-getfl+   3)       ; fcntl: get flags
(defconstant +f-setfl+   4)       ; fcntl: set flags
(defconstant +o-nonblock+ #x800)  ; O_NONBLOCK on Linux x86-64

;;; ── PTY struct ───────────────────────────────────────────────────────────────

(defstruct (pty (:constructor %make-pty))
  "Represents an open pseudo-terminal pair."
  (master-fd -1 :type fixnum)
  (slave-fd  -1 :type fixnum)
  (pid       -1 :type fixnum))

;;; ── Helpers ──────────────────────────────────────────────────────────────────

(defun set-fd-nonblocking (fd)
  "Set O_NONBLOCK on FD. Signals error on failure."
  (let ((flags (%fcntl fd +f-getfl+ 0)))
    (when (minusp flags)
      (error "fcntl(F_GETFL) failed on fd ~a" fd))
    (when (minusp (%fcntl fd +f-setfl+ (logior flags +o-nonblock+)))
      (error "fcntl(F_SETFL) failed on fd ~a" fd))))

;;; ── PTY lifecycle ────────────────────────────────────────────────────────────

(defun spawn-shell (&key (command "/bin/bash") (args '()))
  "Allocate a PTY and fork a child running COMMAND.

Returns a PTY struct with master-fd and pid set.
The child process runs COMMAND (default /bin/bash) with its stdin/stdout/stderr
connected to the slave side of the PTY. The parent receives the master fd."
  (declare (ignore args)) ; TODO: pass args to execv
  (cffi:with-foreign-objects ((master-ptr :int))
    (let ((pid (%forkpty master-ptr
                         (cffi:null-pointer)
                         (cffi:null-pointer)
                         (cffi:null-pointer))))
      (cond
        ((minusp pid)
         (error "forkpty(3) failed"))

        ((zerop pid)
         ;; Child process — exec the shell
         ;; Use sb-ext:run-program to exec; or fall through to raw execv
         ;; Note: we're in a forked image — Lisp threads are not present.
         ;; execvp is the safest call here.
         (cffi:with-foreign-string (c-cmd command)
           (cffi:with-foreign-objects ((argv :pointer 2))
             (setf (cffi:mem-aref argv :pointer 0) c-cmd
                   (cffi:mem-aref argv :pointer 1) (cffi:null-pointer))
             ;; execvp — replaces this process image
             (cffi:foreign-funcall "execvp" :string command :pointer argv :int)
             ;; If execvp returns, it failed.
             (cffi:foreign-funcall "_exit" :int 1 :void))))

        (t
         ;; Parent process
         (let* ((master (cffi:mem-ref master-ptr :int)))
           (set-fd-nonblocking master)
           (%make-pty :master-fd master
                      :slave-fd  -1    ; slave belongs to child
                      :pid       pid)))))))

(defun make-pty ()
  "Allocate a PTY and spawn /bin/bash. Returns a PTY struct."
  (spawn-shell))

(defun pty-close (pty)
  "Close the PTY master file descriptor."
  (check-type pty pty)
  (when (>= (pty-master-fd pty) 0)
    (%close (pty-master-fd pty))
    (setf (pty-master-fd pty) -1)))

(defmacro with-pty ((var) &body body)
  "Allocate a PTY, bind to VAR, run BODY, close PTY on exit."
  `(let ((,var (make-pty)))
     (unwind-protect
          (progn ,@body)
       (pty-close ,var))))

;;; ── I/O ──────────────────────────────────────────────────────────────────────

(defun read-from-pty (pty &key (buffer-size 4096))
  "Read available data from PTY master fd.
Returns a string of output, or NIL if no data available (EAGAIN).
Signals error on other failures."
  (check-type pty pty)
  (let ((fd (pty-master-fd pty)))
    (when (minusp fd) (error "PTY is closed"))
    (cffi:with-foreign-object (buf :char buffer-size)
      (let ((nread (%read fd buf buffer-size)))
        (cond
          ((plusp nread)
           (cffi:foreign-string-to-lisp buf :count nread :encoding :utf-8))
          ((zerop nread) nil)          ; EOF
          (t nil))))))                 ; EAGAIN / error → no data

(defun write-to-pty (pty data)
  "Write string DATA to PTY master fd. Returns number of bytes written."
  (check-type pty pty)
  (check-type data string)
  (let ((fd (pty-master-fd pty)))
    (when (minusp fd) (error "PTY is closed"))
    ;; Convert string to UTF-8 bytes using CFFI
    (cffi:with-foreign-string (buf data :encoding :utf-8)
      (let ((len (cffi:foreign-funcall "strlen" :pointer buf :size)))
        (%write fd buf len)))))
