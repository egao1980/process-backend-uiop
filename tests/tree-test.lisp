(in-package #:process-backend-uiop/tests)

;;; process-protocol 0.4.0: :process-group, kill :tree, process-pid, supports-p (#9).

(defun integer-line (stream)
  "The next line of STREAM as an integer, or NIL."
  (let ((line (read-line stream nil nil)))
    (and line (parse-integer (string-trim '(#\Space #\Return #\Tab) line)
                             :junk-allowed t))))

(defun linux-pid-state (pid)
  "Linux: the state letter of PID from /proc, or NIL when there is no such process."
  (let ((stat (probe-file (format nil "/proc/~d/stat" pid))))
    (when stat
      (let* ((text (handler-case (uiop:read-file-string stat) (error () nil)))
             (end (and text (search ") " text :from-end t))))
        (if end (char text (+ end 2)) #\?)))))

(defun pid-alive-p (pid)
  "True while PID is a running (not zombie) process: /proc on Linux, ps(1) on
other Unix (kill -0 also succeeds on an unreaped zombie), tasklist on Windows."
  (cond
    ((windows-p)
     (let ((out (uiop:run-program (list "tasklist" "/NH" "/FO" "CSV"
                                        "/FI" (format nil "PID eq ~d" pid))
                                  :output :string :ignore-error-status t)))
       (and (search (format nil "\"~d\"" pid) out) t)))
    ((probe-file "/proc/self/stat")
     (let ((state (linux-pid-state pid)))
       (and state (not (member state '(#\Z #\X))) t)))
    (t
     (let ((out (string-trim '(#\Space #\Newline #\Tab)
                             (uiop:run-program (list "ps" "-o" "stat=" "-p" (princ-to-string pid))
                                               :output :string :ignore-error-status t))))
       (and (plusp (length out)) (char/= #\Z (char out 0)))))))

(defun wait-for (predicate seconds)
  "Poll PREDICATE until true or SECONDS pass. → its last value."
  (let ((deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop
      (let ((v (funcall predicate)))
        (when (or v (>= (get-internal-real-time) deadline))
          (return v)))
      (sleep 0.05))))

(defun pid-gone-p (pid &optional (seconds 5))
  (wait-for (lambda () (not (pid-alive-p pid))) seconds))

(defun grandchild-pid-script ()
  "sh script: background a sleep, print its OS pid, sleep in the foreground."
  (if (windows-p)
      ;; MSYS / Cygwin: $! is the POSIX pid; /proc/PID/winpid is the Windows one.
      "sleep 100 & cat /proc/$!/winpid; sleep 100"
      "sleep 100 & echo $!; sleep 100"))

(defun check-grandchild-pid (pid description)
  "PID must be a positive integer. Windows: skip when the MSYS shell has no
/proc/PID/winpid (the marker tests still cover the tree kill there)."
  (if (or pid (not (windows-p)))
      (ok (typep pid '(integer 1)) description)
      (skip "no /proc/PID/winpid from this sh: Windows pid check skipped")))

(defun check-grandchild-gone (pid description)
  "PID is gone within 5 s. Windows: skipped — an MSYS grandchild's
/proc/PID/winpid stays listed by tasklist after taskkill /T although the
marker tests show the tree is stopped (egao1980/process-backend-uiop#13)."
  (if (windows-p)
      (skip "Windows pid liveness after a tree kill: process-backend-uiop#13")
      (ok (pid-gone-p pid) description)))

(defun shell-path (pathname)
  "PATHNAME as sh(1) sees it (forward slashes on Windows)."
  (substitute #\/ #\\ (uiop:native-namestring pathname)))

(defun marker-path (name)
  (let ((p (uiop:merge-pathnames*
            (format nil "process-backend-uiop-~a-~d" name (random 1000000 (make-random-state t)))
            (uiop:temporary-directory))))
    (uiop:delete-file-if-exists p)
    p))

(defmacro with-tree-kill ((&rest programs) &body body)
  "BODY when the backend has :PROCESS-GROUP and PROGRAMS are on PATH."
  `(if (process-protocol:supports-p :process-group)
       (with-programs (,@programs) ,@body)
       (skip "backend has no :process-group here")))

(defun reap (h)
  (ignore-errors (process-protocol:wait h :timeout 10)))

(deftest pid-is-positive-integer
  (with-programs ("sleep")
    (let ((h (process-protocol:launch '("sleep" "2"))))
      (unwind-protect
           (progn
             (ok (typep (process-protocol:pid h) '(integer 1)))
             (ok (eql (process-protocol:pid h) (process-protocol:process-pid h))))
        (process-protocol:kill h :force t)
        (reap h))
      ;; Still known after exit.
      (ok (typep (process-protocol:pid h) '(integer 1))))))

(deftest supports-capabilities
  (ok (process-protocol:supports-p :pid))
  (ng (process-protocol:supports-p :no-such-capability))
  #+sbcl (ok (process-protocol:supports-p :process-group))
  #-sbcl (when (windows-p) (ok (process-protocol:supports-p :process-group))))

(deftest tree-kill-reaches-grandchild
  (with-tree-kill ("sh" "sleep")
    (let* ((h (process-protocol:launch (list "sh" "-c" (grandchild-pid-script))
                                       :process-group t))
           (grandchild (integer-line (process-protocol:stdout h))))
      (unwind-protect
           (progn
             (ok (process-protocol:alive-p h))
             (check-grandchild-pid grandchild "the shell printed the grandchild pid")
             (when grandchild
               (ok (pid-alive-p grandchild) "grandchild runs before the kill"))
             (ok (eq t (process-protocol:kill h :tree t)))
             (when grandchild
               (check-grandchild-gone grandchild "grandchild is gone after kill :tree t"))
             (reap h)
             (ng (process-protocol:alive-p h)))
        (when (process-protocol:alive-p h)
          (process-protocol:kill h :tree t :force t)
          (reap h))))))

(deftest tree-kill-grandchild-marker
  ;; The issue's acceptance test: a grandchild that would write a marker after
  ;; 2 s does not, after kill :tree t (all three OSes).
  (with-tree-kill ("sh" "sleep")
    (let* ((marker (marker-path "marker"))
           (h (process-protocol:launch
               (list "sh" "-c" (format nil "(sleep 2; echo late > '~a') & echo started; sleep 100"
                                       (shell-path marker)))
               :process-group t)))
      (unwind-protect
           (progn
             (ok (search "started" (or (read-line (process-protocol:stdout h) nil) "")))
             (process-protocol:kill h :tree t)
             (reap h)
             (sleep 3.5)
             (ng (probe-file marker) "the grandchild did not write the marker"))
        (uiop:delete-file-if-exists marker)))))

#-(or win32 windows)
(deftest plain-kill-leaves-grandchild
  ;; Control: without :tree only the shell dies — the gap #9 closes.
  (with-tree-kill ("sh" "sleep")
    (let* ((h (process-protocol:launch (list "sh" "-c" (grandchild-pid-script))
                                       :process-group t))
           (grandchild (integer-line (process-protocol:stdout h))))
      (unwind-protect
           (progn
             (ok (typep grandchild '(integer 1)))
             (process-protocol:kill h :force t)
             (reap h)
             (ng (process-protocol:alive-p h))
             (ok (pid-alive-p grandchild) "plain kill leaves the grandchild running")
             ;; The group outlives its leader: kill :tree t still reaches it.
             (process-protocol:kill h :tree t)
             (ok (pid-gone-p grandchild) "kill :tree t after the shell exited"))
        (process-protocol:kill h :tree t :force t)))))

(deftest tree-kill-needs-process-group
  (with-programs ("sleep")
    (let ((h (process-protocol:launch '("sleep" "30"))))
      (unwind-protect
           (progn
             (ok (signals (process-protocol:kill h :tree t)
                          'process-protocol:process-group-error))
             (ok (process-protocol:alive-p h) "a refused tree kill kills nothing")
             ;; CONTINUE: kill the direct child only.
             (handler-bind ((process-protocol:process-group-error #'continue))
               (process-protocol:kill h :tree t :force t))
             (reap h)
             (ng (process-protocol:alive-p h)))
        (when (process-protocol:alive-p h)
          (process-protocol:kill h :force t)
          (reap h))))))

(deftest tree-kill-after-exit-is-not-an-error
  (with-tree-kill ("sh")
    (let ((h (process-protocol:launch '("sh" "-c" "exit 0") :process-group t)))
      (ok (zerop (process-protocol:wait h :timeout 10)))
      (ok (eq t (process-protocol:kill h :tree t))))))

(deftest run-timeout-kills-tree
  (with-tree-kill ("sh" "sleep")
    (let ((marker (marker-path "run-marker"))
          (pidfile (marker-path "run-pid")))
      (unwind-protect
           (progn
             (ok (signals
                  (process-protocol:run
                   (list "sh" "-c"
                         (format nil "(sleep 3; echo late > '~a') & sleep 100 & ~a > '~a'; sleep 100"
                                 (shell-path marker)
                                 (if (windows-p) "cat /proc/$!/winpid" "echo $!")
                                 (shell-path pidfile)))
                   :process-group t :timeout 1)
                  'process-protocol:process-timeout-error))
             (let ((grandchild (and (probe-file pidfile)
                                    (with-open-file (s pidfile) (integer-line s)))))
               (check-grandchild-pid grandchild "the shell wrote the grandchild pid")
               (when grandchild
                 (check-grandchild-gone grandchild "timeout killed the grandchild")))
             (sleep 3)
             (ng (probe-file marker) "the timed-out tree did not write the marker"))
        (uiop:delete-file-if-exists marker)
        (uiop:delete-file-if-exists pidfile)))))

#-(or win32 windows)
(deftest process-group-refuses-inherited-stdin
  ;; SBCL only moves the child to a new group when stdin is not inherited.
  (with-tree-kill ("sleep")
    (ok (signals (process-protocol:launch '("sleep" "1") :process-group t :input :interactive)
                 'process-protocol:process-group-error))))
