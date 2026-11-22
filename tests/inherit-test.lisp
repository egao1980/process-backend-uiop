(in-package #:process-backend-uiop/tests)

;;; A grandchild does not keep this image's standard handles
;;; (egao1980/supervisor-protocol#9).
;;;
;;; The test image launches a child Lisp image with piped stdio (as a JSON-RPC
;;; client launches cl-stack-code --rpc). The child launches a long sleeper
;;; through this backend (as its supervisor launches a shell job), prints the
;;; sleeper's pid and exits. The child's stdout must reach EOF here once the
;;; child is gone, although the sleeper still runs. Windows (SBCL) before the
;;; fix: the sleeper inherited the child's stdout pipe, so EOF came only when
;;; the sleeper exited.

(defun %lisp-runtime-argv ()
  #+sbcl (list (uiop:native-namestring sb-ext:*runtime-pathname*)
               "--core" (uiop:native-namestring sb-ext:*core-pathname*)
               "--noinform" "--non-interactive" "--no-sysinit" "--no-userinit")
  #-sbcl nil)

(defun %asdf-load-form ()
  "A form that loads ASDF in a --no-sysinit image."
  #+sbcl
  (let* ((home-fn (find-symbol "SBCL-HOMEDIR-PATHNAME" "SB-INT"))
         (home (and home-fn (ignore-errors (funcall home-fn))))
         (fasl (and home (probe-file (merge-pathnames "contrib/asdf.fasl" home)))))
    (if fasl
        (format nil "(load ~s)" (uiop:native-namestring fasl))
        "(require :asdf)"))
  #-sbcl "(require :asdf)")

(defun %registry-form ()
  "The source registry of this image's loaded systems, for the child image."
  (let ((dirs '()))
    (dolist (name (asdf:already-loaded-systems))
      (let ((dir (ignore-errors (asdf:system-source-directory name))))
        (when dir (pushnew (uiop:native-namestring dir) dirs :test #'string=))))
    `(:source-registry ,@(mapcar (lambda (d) (list :directory d)) dirs)
                       :ignore-inherited-configuration)))

(defun %sleeper-command (seconds)
  (append (%lisp-runtime-argv) (list "--eval" (format nil "(sleep ~d)" seconds))))

(defun %child-argv (sleeper)
  "A child image that launches SLEEPER through this backend, prints PID <n>, exits."
  (append (%lisp-runtime-argv)
          (loop for form in
                (list (%asdf-load-form)
                      (format nil "(asdf:initialize-source-registry '~s)" (%registry-form))
                      "(let ((*standard-output* (make-broadcast-stream)) (*error-output* (make-broadcast-stream))) (asdf:load-system \"process-backend-uiop\"))"
                      (format nil "(let ((h (process-protocol:launch '~s :process-group (process-protocol:supports-p :process-group)))) (format t \"PID ~~d~~%\" (process-protocol:pid h)) (finish-output) (uiop:quit 0))"
                              sleeper))
                append (list "--eval" form))))

(defun %kill-pid (pid)
  (uiop:run-program (if (windows-p)
                        (list "taskkill" "/F" "/PID" (princ-to-string pid))
                        (list "kill" "-9" (princ-to-string pid)))
                    :ignore-error-status t :output nil :error-output nil))

(defun %pid-line (stream)
  "The pid from the first PID <n> line of STREAM, or NIL at EOF."
  (loop for line = (read-line stream nil nil)
        while line
        do (let ((p (search "PID " line)))
             (when p
               (return (parse-integer line :start (+ p 4) :junk-allowed t))))))

#+sbcl
(deftest grandchild-does-not-hold-the-parents-pipe
  (let* ((child (process-protocol:launch (%child-argv (%sleeper-command 120))
                                         :error-output nil))
         (out (process-protocol:stdout child))
         (sleeper (%pid-line out))
         (reader nil))
    (unwind-protect
         (progn
           (ok (typep sleeper '(integer 1)) "the child printed the sleeper's pid")
           (ok (eql 0 (process-protocol:wait child :timeout 60)) "the child exited")
           (when sleeper
             (ok (pid-alive-p sleeper) "the sleeper still runs"))
           (setf reader (sb-thread:make-thread
                         (lambda ()
                           (handler-case (progn (loop while (read-line out nil nil)) :eof)
                             (error () :error)))
                         :name "inherit-test reader"))
           (ok (wait-for (lambda () (not (sb-thread:thread-alive-p reader))) 20)
               "the child's stdout reached EOF while the sleeper runs"))
      (when sleeper (%kill-pid sleeper))
      (when (and reader (sb-thread:thread-alive-p reader))
        (ignore-errors (sb-thread:join-thread reader :default nil :timeout 30))))))
