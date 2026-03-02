(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))
(push (truename "/home/slime/.openclaw/workspace-gensym/projects/cl-term/") asdf:*central-registry*)
(ql:quickload :cl-term :silent t)

(defvar *pass* 0)
(defvar *fail* 0)
(defmacro test (name &body body)
  `(handler-case (progn ,@body (format t "PASS: ~A~%" ,name) (incf *pass*))
     (error (e) (format t "FAIL: ~A — ~A~%" ,name e) (incf *fail*))))

(test "config defaults"
  (let ((cfg (cl-term.config:make-terminal-config)))
    (assert (>= (cl-term.config:terminal-config-scrollback-limit cfg) 100))
    (assert (string= "/bin/bash" (cl-term.config:terminal-config-shell cfg)))))

(test "screen scrollback captures line"
  (let ((s (cl-term.screen:make-screen :width 5 :height 2 :scrollback-limit 100)))
    (loop for ch across "abcde" do (cl-term.screen:screen-put-char s ch))
    (loop for ch across "fghij" do (cl-term.screen:screen-put-char s ch))
    (assert (find "abcde" (cl-term.screen:screen-scrollback s) :test #'search))))

(test "screen search scrollback"
  (let ((s (cl-term.screen:make-screen :width 5 :height 2 :scrollback-limit 100)))
    (loop for ch across "hello" do (cl-term.screen:screen-put-char s ch))
    (loop for ch across "world" do (cl-term.screen:screen-put-char s ch))
    (assert (plusp (length (cl-term.screen:screen-search-scrollback s "hell"))))))

(test "terminal key binding"
  (let ((t1 (cl-term.terminal:make-terminal)))
    (cl-term.terminal:terminal-bind-key t1 "F13" "xyz")
    (assert (string= "xyz" (gethash "F13" (cl-term.terminal:terminal-key-bindings t1))))))

(test "terminal tab scaffold"
  (let ((t1 (cl-term.terminal:make-terminal :width 80 :height 24)))
    (cl-term.terminal:terminal-new-tab t1 :title "build")
    (assert (= 2 (length (cl-term.terminal:terminal-list-tabs t1))))
    (assert (cl-term.terminal:terminal-switch-tab t1 1))))


(test "terminal rename tab"
  (let ((t1 (cl-term.terminal:make-terminal :width 80 :height 24)))
    (cl-term.terminal:terminal-new-tab t1 :title "build")
    (assert (cl-term.terminal:terminal-rename-tab t1 1 "logs"))
    (let ((tab (find 1 (cl-term.terminal:terminal-list-tabs t1)
                     :key (lambda (x) (getf x :id)))))
      (assert (string= "logs" (getf tab :title))))))

(test "terminal close tab keeps one and reselects active"
  (let ((t1 (cl-term.terminal:make-terminal :width 80 :height 24)))
    (cl-term.terminal:terminal-new-tab t1 :title "build")
    (cl-term.terminal:terminal-switch-tab t1 1)
    (assert (cl-term.terminal:terminal-close-tab t1 1))
    (assert (= 1 (length (cl-term.terminal:terminal-list-tabs t1))))
    (assert (eq t (getf (first (cl-term.terminal:terminal-list-tabs t1)) :active)))
    (assert (not (cl-term.terminal:terminal-close-tab t1 0)))))

(format t "~%=== Phase4 Results: ~A passed, ~A failed ===~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
