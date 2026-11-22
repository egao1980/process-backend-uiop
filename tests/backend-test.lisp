(in-package #:process-backend-uiop/tests)

(defun windows-p ()
  (and (or (member :win32 *features*) (member :windows *features*)) t))

(defun find-program (name)
  "True when NAME is on PATH (Windows: also NAME.exe). The windows-latest
runner has sh / cat / printf / sleep from Git for Windows when tests run
under bash."
  (let ((exts (if (windows-p) '(".exe" "") '(""))))
    (loop for dir in (uiop:split-string (or (uiop:getenv "PATH") "")
                                        :separator (if (windows-p) ";" ":"))
          thereis (and (plusp (length dir))
                       (loop for ext in exts
                             thereis (uiop:file-exists-p
                                      (uiop:merge-pathnames*
                                       (uiop:parse-native-namestring
                                        (concatenate 'string name ext))
                                       (uiop:ensure-directory-pathname
                                        (uiop:parse-native-namestring dir)))))))))

(defmacro with-programs ((&rest names) &body body)
  `(if (every #'find-program ',names)
       (progn ,@body)
       (skip ,(format nil "needs ~{~a~^, ~} on PATH" names))))

(defun octets (&rest bytes)
  (coerce bytes '(simple-array (unsigned-byte 8) (*))))

(defun utf8 (octets)
  (babel:octets-to-string octets :encoding :utf-8))

(defun call-with-parent-env (name value thunk)
  (let ((old (uiop:getenv name)))
    ;; The backend reads the environ the child inherits; it must see the change.
    (if (handler-case (progn (setf (uiop:getenv name) value)
                             (let ((environ (process-backend-uiop::%inherited-environment)))
                               (and (listp environ)
                                    (member (concatenate 'string name "=" value) environ
                                            :test #'string=))))
          (error () nil))
        (unwind-protect (funcall thunk)
          ;; UIOP 3.3.1 cannot unset on SBCL (setenv NIL): leave it empty.
          (setf (uiop:getenv name) (or old "")))
        (skip "cannot set a variable in this process's environment"))))

(defmacro with-parent-env ((name value) &body body)
  "Run BODY with NAME=VALUE in this process's environment (skip if impossible)."
  `(call-with-parent-env ,name ,value (lambda () ,@body)))

;;; Existing behaviour

(deftest run-echo
  (multiple-value-bind (code out err)
      (process-protocol:run '("echo" "hi"))
    (ok (zerop code))
    (ok (zerop (length err)))
    (ok (search "hi" (utf8 out)))))

(deftest run-false-exit
  (multiple-value-bind (code out err)
      (process-protocol:run '("false"))
    (declare (ignore out err))
    (ok (= 1 code))))

(deftest launch-wait
  ;; Use sleep so the child is still alive when we probe (echo races on fast CI).
  (let ((h (process-protocol:launch '("sleep" "2"))))
    (ok (process-protocol:alive-p h))
    (ok (zerop (process-protocol:wait h)))
    (ng (process-protocol:alive-p h))))

(deftest backend-bound
  (ok (typep process-protocol:*process-backend*
             'process-backend-uiop:uiop-process-backend)))

;;; Binary-safe capture (#8)

(deftest run-binary-stdout
  (with-programs ("printf")
    (multiple-value-bind (code out err)
        (process-protocol:run '("printf" "a\\000\\377\\376b"))
      (ok (zerop code))
      (ok (typep out 'process-protocol:octet-vector))
      (ok (equalp (octets 97 0 255 254 98) out))
      (ok (equalp (octets) err)))))

(deftest run-binary-stderr
  (with-programs ("sh")
    (multiple-value-bind (code out err)
        (process-protocol:run '("sh" "-c" "printf 'x\\000\\377' >&2"))
      (ok (zerop code))
      (ok (equalp (octets) out))
      (ok (equalp (octets 120 0 255) err)))))

(deftest run-binary-input
  (with-programs ("cat")
    (let ((bytes (octets 0 1 255 10 13 0 128 192)))
      (multiple-value-bind (code out)
          (process-protocol:run '("cat") :input bytes)
        (ok (zerop code))
        (ok (equalp bytes out))))))

(deftest run-string-input-and-output
  (with-programs ("cat")
    (multiple-value-bind (code out)
        (process-protocol:run '("cat") :input "héllo λ" :output :string)
      (ok (zerop code))
      (ok (equal "héllo λ" out)))))

(deftest run-string-output-invalid-utf8
  (with-programs ("printf")
    (ok (signals (process-protocol:run '("printf" "\\377") :output :string)
                 'process-protocol:process-error))))

(deftest run-discard-stderr
  (with-programs ("sh")
    (multiple-value-bind (code out err)
        (process-protocol:run '("sh" "-c" "echo e >&2; printf o") :error-output nil)
      (ok (zerop code))
      (ok (equalp (octets 111) out))
      (ok (equalp (octets) err)))))

(deftest run-missing-program
  (ok (signals (process-protocol:run '("no-such-program-process-backend-uiop"))
               'process-protocol:process-error)))

;;; Environment (#9)

(defparameter *probe* "PROCESS_BACKEND_UIOP_PROBE")

(defun child-var (name &rest keys)
  (multiple-value-bind (code out)
      (apply #'process-protocol:run
             (list "sh" "-c" (format nil "printf %s \"$~a\"" name))
             :output :string keys)
    (assert (zerop code))
    out))

(deftest env-merges-home-survives
  (with-programs ("sh")
    (let ((home (uiop:getenv "HOME")))
      (ok (equal "1" (child-var "FOO" :env '(("FOO" . "1")))))
      (if (and home (not (windows-p)))
          (ok (equal home (child-var "HOME" :env '(("FOO" . "1")))))
          ;; MSYS sh rewrites a Windows HOME to /c/...; it must still be there.
          (ok (plusp (length (child-var "HOME" :env '(("FOO" . "1"))))))))))

(deftest env-merges-inherited
  (with-programs ("sh")
    (with-parent-env (*probe* "inherited")
      (ok (equal "inherited" (child-var *probe* :env '(("FOO" . "1")))))
      (ok (equal "over" (child-var *probe* :env (list (cons *probe* "over")))))
      ;; 0.2 call shape: "NAME=VALUE" strings, now merged.
      (ok (equal "inherited" (child-var *probe* :env '("FOO=1"))))
      (ok (equal "" (child-var *probe* :env (list (cons *probe* nil))))))))

(deftest env-replace
  (with-programs ("sh")
    (with-parent-env (*probe* "inherited")
      (let ((env (append '(("ONLY" . "1"))
                         ;; Windows programs need these to start at all.
                         (when (windows-p)
                           (loop for name in '("SYSTEMROOT" "PATH")
                                 for value = (uiop:getenv name)
                                 when value collect (cons name value))))))
        (ok (equal "1" (child-var "ONLY" :env env :env-mode :replace)))
        ;; On Unix ENV has no PATH: "sh" was still found through the parent's PATH.
        (ok (equal "" (child-var *probe* :env env :env-mode :replace)))))))

(deftest env-launch
  (with-programs ("sh")
    (let ((h (process-protocol:launch '("sh" "-c" "printf %s \"$FOO\"")
                                      :env '(("FOO" . "launched")) :input nil)))
      (ok (equal "launched" (uiop:slurp-stream-string (process-protocol:stdout h))))
      (ok (zerop (process-protocol:wait h))))))

;;; Octet streams (#10)

(defun read-exactly (stream n)
  (let* ((buf (make-array n :element-type '(unsigned-byte 8)))
         (got (read-sequence buf stream)))
    (subseq buf 0 got)))

(deftest launch-octet-streams-cat
  (with-programs ("cat")
    (let ((h (process-protocol:launch '("cat") :element-type :octets))
          (a (octets 0 255 10 13 97 0 128))
          (b (octets 1 2 3 0 254 10)))
      (unwind-protect
           (let ((in (process-protocol:stdin h))
                 (out (process-protocol:stdout h)))
             ;; Two round trips on one long-lived child.
             (write-sequence a in)
             (finish-output in)
             (ok (equalp a (read-exactly out (length a))))
             (write-sequence b in)
             (finish-output in)
             (ok (equalp b (read-exactly out (length b))))
             (write-byte 7 in)
             (finish-output in)
             (ok (eql 7 (read-byte out)))
             (close in)
             (ok (eq :eof (read-byte out nil :eof)))
             (ok (zerop (process-protocol:wait h))))
        (when (process-protocol:alive-p h)
          (process-protocol:kill h :force t))))))

;;; Timeout (#10)

(deftest run-timeout-kills-child
  (with-programs ("sleep")
    (let ((handle nil)
          (restarts '())
          (start (get-internal-real-time)))
      (ok (signals
           (handler-bind ((process-protocol:process-timeout-error
                            (lambda (c)
                              (setf handle (process-protocol:process-timeout-error-process c)
                                    restarts (mapcar #'restart-name (compute-restarts c))))))
             (process-protocol:run '("sleep" "30") :timeout 0.5))
           'process-protocol:process-timeout-error))
      (ok (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 10))
      (ok (member 'continue restarts))
      (ok (typep handle 'process-backend-uiop:uiop-process-handle))
      (ng (process-protocol:alive-p handle)))))

(deftest run-timeout-continue
  (with-programs ("sleep")
    (let ((count 0))
      (ok (eql 0 (handler-bind ((process-protocol:process-timeout-error
                                  (lambda (c)
                                    (when (< (incf count) 50)
                                      (continue c)))))
                   (values (process-protocol:run '("sleep" "1") :timeout 0.2)))))
      (ok (plusp count)))))

(deftest run-timeout-not-reached
  (with-programs ("printf")
    (multiple-value-bind (code out)
        (process-protocol:run '("printf" "ok") :timeout 30)
      (ok (zerop code))
      (ok (equalp (octets 111 107) out)))))

(deftest wait-timeout-leaves-child
  (with-programs ("sleep")
    (let ((h (process-protocol:launch '("sleep" "30"))))
      (unwind-protect
           (progn
             (ok (signals (process-protocol:wait h :timeout 0.3)
                          'process-protocol:process-timeout-error))
             (ok (process-protocol:alive-p h)))
        (process-protocol:kill h :force t)
        (process-protocol:wait h))
      (ng (process-protocol:alive-p h)))))

;;; env(1) fallback for implementations without environ access (Unix)

(deftest env-wrapper-argv
  (ok (equal '("/usr/bin/env" "-u" "GONE" "A=1")
             (process-backend-uiop::%env-wrapper '(("A" . "1") ("GONE" . nil)) :merge)))
  (ok (equal '("/usr/bin/env" "-i" "A=2")
             (process-backend-uiop::%env-wrapper '("A=1" ("A" . "2")) :replace))))

(deftest env-wrapper-runs
  (if (or (windows-p) (not (uiop:file-exists-p "/usr/bin/env")))
      (skip "env(1) fallback is Unix-only")
      (with-parent-env (*probe* "inherited")
        (flet ((via-wrapper (env mode)
                 (multiple-value-bind (code out)
                     (process-protocol:run
                      (append (process-backend-uiop::%env-wrapper env mode)
                              (list "sh" "-c" (format nil "printf '%s|%s' \"$FOO\" \"$~a\"" *probe*)))
                      :output :string)
                   (assert (zerop code))
                   out)))
          (ok (equal "1|inherited" (via-wrapper '(("FOO" . "1")) :merge)))
          (ok (equal "1|" (via-wrapper (list '("FOO" . "1") (cons *probe* nil)) :merge)))
          (ok (equal "1|" (via-wrapper '(("FOO" . "1") ("PATH" . "/usr/bin:/bin")) :replace)))))))

;;; Pipe capture (egao1980/compute-backend-seatbelt#2)

#-(or win32 windows)
(deftest run-captures-through-pipes
  ;; With threads, RUN's stdout and stderr are pipes, not temp files: a
  ;; sandboxed child may not stat a regular file it did not open.
  (with-programs ("sh" "test")
    (if (process-backend-uiop::%pipe-capture-p)
        (multiple-value-bind (code out err)
            (process-protocol:run '("sh" "-c" "test -p /dev/stdout && echo out-pipe; test -p /dev/stderr && echo err-pipe >&2")
                                  :output :string :error-output :string)
          (ok (zerop code))
          (ok (equal (format nil "out-pipe~%") out))
          (ok (equal (format nil "err-pipe~%") err)))
        (skip "no threads: temp-file capture"))))

(deftest run-large-output-both-streams
  ;; Both pipes are drained at once: neither fills up and stalls the child.
  (with-programs ("sh")
    (multiple-value-bind (code out err)
        (process-protocol:run
         '("sh" "-c" "i=0; while [ $i -lt 2000 ]; do echo 0123456789012345678901234567890123456789012345678901234567890123456789; echo e0123456789012345678901234567890123456789012345678901234567890123456789 >&2; i=$((i+1)); done")
         :timeout 60)
      (ok (zerop code))
      (ok (= (* 2000 71) (length out)))
      (ok (= (* 2000 72) (length err))))))

;;; Windows: cmd.exe gets its command line verbatim (#17)

(deftest windows-cmd-command-line
  (let ((f #'process-backend-uiop::%windows-cmd-command))
    (ok (equal '("cmd.exe" . "/d /s /c \"echo ran> \"C:\\x y\\ran.txt\"\"")
               (funcall f '("cmd.exe" "/d" "/s" "/c" "echo ran> \"C:\\x y\\ran.txt\"")))
        "the arguments after /c, verbatim, in one pair of quotes")
    (ok (equal '("C:\\Windows\\system32\\CMD.EXE" . "/C \"dir /b\"")
               (funcall f '("C:\\Windows\\system32\\CMD.EXE" "/C" "dir" "/b")))
        "a path, any case; several arguments are space-joined")
    (ok (equal '("cmd" . "/k \"\"") (funcall f '("cmd" "/k"))))
    (ok (equal '("cmd.exe" "/d") (funcall f '("cmd.exe" "/d"))) "no /c: unchanged")
    (ok (equal '("sh" "-c" "echo \"x\"") (funcall f '("sh" "-c" "echo \"x\"")))
        "other programs: unchanged")
    (ok (equal '("C:\\a b\\cmd.exe" "/c" "x") (funcall f '("C:\\a b\\cmd.exe" "/c" "x")))
        "a program with a space: unchanged")))

(deftest windows-cmd-quoted-redirect
  (if (not (windows-p))
      (skip "Windows only")
      (let* ((dir (uiop:ensure-directory-pathname
                   (merge-pathnames (format nil "pbu cmd ~36R" (random (expt 36 6)))
                                    (uiop:temporary-directory))))
             (file (merge-pathnames "ran.txt" dir)))
        (ensure-directories-exist dir)
        (unwind-protect
             (multiple-value-bind (code out)
                 (process-protocol:run
                  (list "cmd.exe" "/d" "/s" "/c"
                        (format nil "echo ran> \"~a\" && echo \"quoted & kept\""
                                (uiop:native-namestring file))))
               (ok (eql 0 code) (utf8 out))
               (ok (uiop:file-exists-p file) "the quoted redirect target was written")
               (ok (search "\"quoted & kept\"" (utf8 out)) "quotes reach cmd as written"))
          (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))
