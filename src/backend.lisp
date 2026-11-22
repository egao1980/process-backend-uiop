(in-package #:process-backend-uiop)

(defclass uiop-process-backend (process-protocol:process-backend) ())

(defun make-uiop-process-backend ()
  (make-instance 'uiop-process-backend))

(defun use-uiop-process-backend ()
  (setf process-protocol:*process-backend* (make-uiop-process-backend)))

(defclass uiop-process-handle (process-protocol:process-handle)
  ((info :initarg :info :reader handle-info)
   (command :initarg :command :initform nil :reader handle-command)
   (pid :initarg :pid :initform nil :reader handle-pid)
   (process-group :initarg :process-group :initform nil :reader handle-process-group-p
                  :documentation "T when launched with :PROCESS-GROUP T (Unix: PID is the group id).")
   (job :initarg :job :initform nil :reader handle-job
        :documentation "Windows (SBCL): the job object holding the child's tree, or NIL.")
   (exit-code :initform nil :accessor handle-exit-code%)))

(deftype octets () '(vector (unsigned-byte 8)))

(defun %empty-octets ()
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun %encoding (external-format)
  "Babel encoding name for EXTERNAL-FORMAT (a keyword, or a list headed by one)."
  (if (consp external-format) (first external-format) external-format))

(defun %wrap (command message cause)
  (error 'process-protocol:process-error
         :command command :cause cause
         :message (format nil "~a: ~a" message cause)))

;;; Command + environment
;;;
;;; Windows: UIOP gives a list command C-runtime quoting (" → \"), which is
;;; right for ordinary programs, but cmd.exe does not parse its command line
;;; that way: after /C (or /K) it takes the rest of the line as written. So a
;;; cmd.exe argv with /C or /K becomes the line
;;;   <program> <switches, C-runtime quoted> "<arguments after /C, space-joined>"
;;; passed to CreateProcess unescaped. That is the form /S expects (it strips
;;; the outer pair; without /S cmd strips the same pair whenever the line holds
;;; more than two quotes or is not one executable name). Node/libuv and Rust std
;;; special-case cmd.exe the same way (egao1980/process-backend-uiop#17).
;;; UIOP takes the unescaped line as a dotted command (PROGRAM . "ARGUMENTS"),
;;; the form it uses itself for string commands.

(defun %cmd-program-p (program)
  "PROGRAM (a string) names cmd.exe: cmd or cmd.exe, bare or as a path's last
component, any case."
  (let ((name (subseq program (1+ (or (position-if (lambda (c) (find c "/\\")) program
                                                   :from-end t)
                                      -1)))))
    (or (string-equal name "cmd") (string-equal name "cmd.exe"))))

(defun %windows-quote (arg)
  "ARG quoted for the C runtime's command-line parser (unchanged when it needs none)."
  (if (and (plusp (length arg))
           (not (find-if (lambda (c) (find c '(#\Space #\Tab #\Newline #\"))) arg)))
      arg
      (with-output-to-string (s)
        (write-char #\" s)
        (let ((slashes 0))
          (loop for c across arg
                do (cond ((char= c #\\) (incf slashes))
                         ((char= c #\")
                          (loop repeat (1+ (* 2 slashes)) do (write-char #\\ s))
                          (setf slashes 0))
                         (t (setf slashes 0)))
                   (write-char c s))
          (loop repeat slashes do (write-char #\\ s)))
        (write-char #\" s))))

(defun %windows-cmd-command (command)
  "COMMAND (a list argv) in the form UIOP should launch on Windows: for cmd.exe
with a /C or /K switch, (PROGRAM . LINE) where LINE is the switches up to it
\(C-runtime quoted) and the remaining arguments, space-joined, in one pair of
quotes, verbatim; any other COMMAND unchanged. (A PROGRAM containing a space or
a quote is left unchanged too: CreateProcess would get it unquoted.)"
  (let* ((program (first command))
         (args (rest command))
         (switch (position-if (lambda (a)
                                (and (stringp a)
                                     (or (string-equal a "/c") (string-equal a "/k"))))
                              args)))
    (if (and (stringp program)
             (every #'stringp args)
             switch
             (%cmd-program-p program)
             (not (find-if (lambda (c) (find c '(#\Space #\Tab #\"))) program)))
        (cons program
              (format nil "~{~a ~}\"~{~a~^ ~}\""
                      (mapcar #'%windows-quote (subseq args 0 (1+ switch)))
                      (subseq args (1+ switch))))
        command)))

(defun %normalize-command (command shell)
  (cond
    (shell
     (etypecase command
       (string command)
       (list (format nil "~{~a~^ ~}" command))))
    (t
     (etypecase command
       (list (if (and (process-protocol:windows-p)
                      #+(or sbcl ecl clasp) t #-(or sbcl ecl clasp) nil)
                 (%windows-cmd-command command)
                 command))
       (string
        (error 'process-protocol:process-error
               :command command
               :message "string command requires :shell t"))))))

(defun %inherited-environment ()
  "The parent environment as \"NAME=VALUE\" strings, or :UNSUPPORTED."
  (let ((environ #+sbcl (find-symbol "POSIX-ENVIRON" "SB-EXT") #-sbcl nil))
    (if (and environ (fboundp environ))
        (funcall environ)
        :unsupported)))

(defun %entry-name (entry)
  (if (consp entry)
      (car entry)
      (subseq entry 0 (or (position #\= entry :start (min 1 (length entry)))
                          (length entry)))))

(defun %env-wrapper (env env-mode)
  "env(1) argv prefix giving the child ENV / ENV-MODE (Unix, no environ access)."
  (let* ((entries (process-protocol:merge-environment env :mode :replace
                                                          :case-insensitive nil))
         (unsets (remove-if (lambda (name) (assoc name entries :test #'string=))
                            (remove-duplicates (mapcar #'%entry-name env) :test #'string=))))
    (append (list "/usr/bin/env")
            (if (eq env-mode :replace)
                (list "-i")
                (loop for name in unsets append (list "-u" name)))
            (process-protocol:environment-strings entries))))

(defun %command-and-env (command shell env env-mode)
  "→ (values uiop-command extra-launch-keys)."
  (check-type env-mode (member :merge :replace))
  (let ((cmd (%normalize-command command shell)))
    (if (and (null env) (eq env-mode :merge))
        (values cmd '())
        (let ((base (%inherited-environment)))
          (cond
            ((listp base)
             (values cmd
                     (list :environment
                           (process-protocol:environment-strings
                            (process-protocol:merge-environment env :base base :mode env-mode)))))
            ((process-protocol:windows-p)
             (error 'process-protocol:process-error
                    :command command
                    :message (format nil ":env is not supported by this backend on ~a / Windows"
                                     (lisp-implementation-type))))
            (t
             (values (append (%env-wrapper env env-mode)
                             (if (stringp cmd) (list "/bin/sh" "-c" cmd) cmd))
                     '())))))))

;;; Process groups (process-protocol 0.4.0)
;;;
;;; Unix (SBCL): SBCL's spawn() already makes the child a process-group leader
;;; (setpgid) whenever stdin is not inherited from the parent; :PROCESS-GROUP T
;;; refuses inherited stdin and checks getpgid(pid) = pid after the launch.
;;; The tree kill is killpg(pid, SIGTERM | SIGKILL). Other Unix Lisps: no
;;; :PROCESS-GROUP (their run-program does not promise a new group).
;;; Windows (SBCL): right after CreateProcess the child is put in a new job
;;; object (CreateJobObjectW + AssignProcessToJobObject, kernel32 through
;;; sb-alien: no native library). Its descendants inherit the job, so the
;;; tree kill is TerminateJobObject — also after the child itself exited.
;;; taskkill /T is not enough: an MSYS / Cygwin fork+exec (Git for Windows sh)
;;; leaves the exec'd program with a Windows parent pid that no longer exists,
;;; so walking parent pids misses it (egao1980/process-backend-uiop#13).
;;; Other Windows Lisps, or a failed assignment: taskkill /T /F /PID.

(defun %group-supported-p ()
  (or (process-protocol:windows-p)
      #+sbcl t
      #-sbcl nil))

#+(and sbcl (not (or win32 windows)))
(progn
  (defun %getpgid (pid)
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "getpgid" (function sb-alien:int sb-alien:int))
     pid))

  (defun %killpg (pgid signal)
    "killpg(2). A group that no longer exists (ESRCH) is not an error."
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "killpg" (function sb-alien:int sb-alien:int sb-alien:int))
     pgid signal)))

#+(and sbcl (or win32 windows))
(progn
  (defun %close-handle (handle)
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "CloseHandle" (function sb-alien:int sb-alien:system-area-pointer))
     handle))

  (defun %null-sap-p (sap)
    (zerop (sb-sys:sap-int sap)))

  (defun %job-for-pid (pid)
    "A new job object holding process PID, or NIL when either call fails."
    (let ((job (sb-alien:alien-funcall
                (sb-alien:extern-alien "CreateJobObjectW"
                                       (function sb-alien:system-area-pointer
                                                 sb-alien:system-area-pointer
                                                 sb-alien:system-area-pointer))
                (sb-sys:int-sap 0) (sb-sys:int-sap 0))))
      (unless (%null-sap-p job)
        ;; PROCESS_SET_QUOTA | PROCESS_TERMINATE: what AssignProcessToJobObject needs.
        (let* ((process (sb-alien:alien-funcall
                         (sb-alien:extern-alien "OpenProcess"
                                                (function sb-alien:system-area-pointer
                                                          (sb-alien:unsigned 32)
                                                          sb-alien:int
                                                          (sb-alien:unsigned 32)))
                         #x0101 0 pid))
               (assigned (and (not (%null-sap-p process))
                              (prog1 (/= 0 (sb-alien:alien-funcall
                                            (sb-alien:extern-alien
                                             "AssignProcessToJobObject"
                                             (function sb-alien:int
                                                       sb-alien:system-area-pointer
                                                       sb-alien:system-area-pointer))
                                            job process))
                                (%close-handle process)))))
          (if assigned
              job
              (progn (%close-handle job) nil))))))

  (defun %terminate-job (job)
    "TerminateJobObject(JOB, 1): every process in the job exits with code 1."
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "TerminateJobObject"
                            (function sb-alien:int sb-alien:system-area-pointer
                                      (sb-alien:unsigned 32)))
     job 1)))

(defun %attach-job (handle)
  "Windows (SBCL): put HANDLE's child in a new job object; the handle's
finalizer closes the job handle. Elsewhere, or when that fails: HANDLE as is."
  #+(and sbcl (or win32 windows))
  (let* ((pid (handle-pid handle))
         (job (and pid (%job-for-pid pid))))
    (when job
      (setf (slot-value handle 'job) job)
      (sb-ext:finalize handle (lambda () (%close-handle job)) :dont-save t)))
  handle)

(defun %group-error (command message &optional handle)
  (error 'process-protocol:process-group-error
         :command command :process handle :message message))

(defun %check-group-request (command input)
  "Signal PROCESS-GROUP-ERROR when :PROCESS-GROUP T cannot be honoured."
  (cond
    ((not (%group-supported-p))
     (%group-error command (format nil ":process-group t is not supported by this backend on ~a"
                                   (lisp-implementation-type))))
    ((and (not (process-protocol:windows-p))
          (member input '(t :interactive)))
     (%group-error command ":process-group t needs a non-inherited :input (SBCL keeps a child that shares the terminal's stdin in the parent's group)"))))

(defun %make-handle (command info process-group)
  "A handle for INFO. With PROCESS-GROUP on Unix, verify the child leads its group."
  (let* ((pid (uiop:process-info-pid info))
         (handle (make-instance 'uiop-process-handle
                                :command command :info info :pid pid
                                :process-group (and process-group t))))
    #+(and sbcl (not (or win32 windows)))
    (when process-group
      (let ((pgid (if pid (%getpgid pid) -1)))
        ;; -1: already gone (ESRCH); nothing escaped the group then.
        (unless (or (= pgid -1) (eql pgid pid))
          (%kill-child handle t)
          (uiop:wait-process info)
          (%group-error command (format nil "the child (pid ~a) is in process group ~a, not its own"
                                        pid pgid)
                        handle))))
    (when (and process-group (process-protocol:windows-p))
      (%attach-job handle))
    handle))

;;; This image's standard handles (Windows)
;;;
;;; SBCL starts a child with CreateProcess bInheritHandles = TRUE whenever a
;;; stream is redirected, and that hands over EVERY inheritable handle, not
;;; only the child's stdio. This image's own stdin / stdout / stderr are
;;; inheritable when its parent made them so — the pipes of a JSON-RPC client
;;; that launched it, say. A child that outlives this image then keeps the
;;; parent's pipes open: the parent's read of our stdout sees no EOF until
;;; that grandchild exits (a crashed cl-stack-code --rpc child's shell jobs
;;; held close-rpc-client for 5 minutes, then had exited before the crash
;;; sweep looked: egao1980/supervisor-protocol#9). Unix is not affected
;;; (SBCL's spawn closes every other descriptor in the child).
;;; So, on Windows (SBCL), each launch first marks the standard handles that
;;; are pipes or files and that the child does not take as its own stdio
;;; (T / :INTERACTIVE; a stream argument keeps all three) not inheritable,
;;; and restores them after CreateProcess, under one lock. Console handles
;;; are left alone.

(defun %std-spec-inherited-p (spec)
  "True when SPEC (an :input / :output / :error-output argument) hands the
child this image's own standard stream."
  (or (eq spec t) (eq spec :interactive)))

#+(and sbcl (or win32 windows))
(progn
  (defvar *std-handle-lock* (sb-thread:make-mutex :name "process-backend-uiop std handles"))

  (defconstant +handle-flag-inherit+ 1)

  (defun %std-handle (which)
    "GetStdHandle for WHICH (0 stdin, 1 stdout, 2 stderr) → its address, or NIL."
    (let* ((id (ecase which (0 #xFFFFFFF6) (1 #xFFFFFFF5) (2 #xFFFFFFF4)))
           (sap (sb-alien:alien-funcall
                 (sb-alien:extern-alien "GetStdHandle"
                                        (function sb-alien:system-area-pointer
                                                  (sb-alien:unsigned 32)))
                 id))
           (n (sb-sys:sap-int sap)))
      (and (/= n 0) (/= n (ldb (byte 64 0) -1)) (/= n (ldb (byte 32 0) -1)) n)))

  (defun %file-type (handle)
    "GetFileType: 1 disk, 2 character (console), 3 pipe, 0 unknown."
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "GetFileType"
                            (function (sb-alien:unsigned 32) sb-alien:system-area-pointer))
     (sb-sys:int-sap handle)))

  (defun %inheritable-p (handle)
    (sb-alien:with-alien ((flags (sb-alien:unsigned 32) 0))
      (and (/= 0 (sb-alien:alien-funcall
                  (sb-alien:extern-alien "GetHandleInformation"
                                         (function sb-alien:int sb-alien:system-area-pointer
                                                   (* (sb-alien:unsigned 32))))
                  (sb-sys:int-sap handle) (sb-alien:addr flags)))
           (logtest flags +handle-flag-inherit+))))

  (defun %set-inheritable (handle inheritable)
    "SetHandleInformation HANDLE_FLAG_INHERIT. → true on success."
    (/= 0 (sb-alien:alien-funcall
           (sb-alien:extern-alien "SetHandleInformation"
                                  (function sb-alien:int sb-alien:system-area-pointer
                                            (sb-alien:unsigned 32) (sb-alien:unsigned 32)))
           (sb-sys:int-sap handle) +handle-flag-inherit+
           (if inheritable +handle-flag-inherit+ 0)))))

(defun %call-with-private-std-handles (specs thunk)
  "Call THUNK (it starts one child). SPECS: the child's :input, :output and
:error-output arguments. Windows (SBCL): this image's standard handles that
the child does not take (see above) are not inheritable while THUNK runs.
Elsewhere: just THUNK."
  (declare (ignorable specs))
  #+(and sbcl (or win32 windows))
  (if (some #'streamp specs)
      (funcall thunk)
      (sb-thread:with-recursive-lock (*std-handle-lock*)
        (let* ((handles (loop for i below 3 collect (%std-handle i)))
               (kept (loop for h in handles for spec in specs
                           when (and h (%std-spec-inherited-p spec)) collect h))
               (cleared '()))
          (unwind-protect
               (progn
                 (dolist (h handles)
                   (when (and h
                              (not (member h kept))
                              (not (member h cleared))
                              (member (%file-type h) '(1 3))
                              (%inheritable-p h)
                              (%set-inheritable h nil))
                     (push h cleared)))
                 (funcall thunk))
            (dolist (h cleared)
              (%set-inheritable h t))))))
  #-(and sbcl (or win32 windows))
  (funcall thunk))

(defmacro with-private-std-handles ((&rest specs) &body body)
  `(%call-with-private-std-handles (list ,@specs) (lambda () ,@body)))

(defun %kill-child (handle force)
  (uiop:terminate-process (handle-info handle) :urgent force))

(defun %kill-tree (handle force)
  "Signal HANDLE's whole tree. HANDLE was launched with :PROCESS-GROUP T."
  (let ((pid (handle-pid handle)))
    (cond
      ((null pid) (%kill-child handle force))
      ((and (process-protocol:windows-p) (handle-job handle))
       ;; The job outlives the child while any descendant runs.
       #+(and sbcl (or win32 windows)) (%terminate-job (handle-job handle)))
      ((process-protocol:windows-p)
       ;; Parent pids are walked now: only while the child runs is its pid ours.
       (when (uiop:process-alive-p (handle-info handle))
         (uiop:run-program (list "taskkill" "/T" "/F" "/PID" (princ-to-string pid))
                           :output nil :error-output nil :ignore-error-status t)))
      (t
       #+(and sbcl (not (or win32 windows)))
       ;; The group outlives its leader while any member runs.
       (%killpg pid (if force 9 15))
       #-(and sbcl (not (or win32 windows)))
       (%kill-child handle force)))))

;;; Files

(defun %temp-file (prefix)
  (uiop:with-temporary-file (:pathname p :prefix prefix :keep t)
    p))

(defun %write-octets (octets path)
  (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence octets s))
  path)

(defun %read-octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let* ((v (make-array (file-length s) :element-type '(unsigned-byte 8)))
           (n (read-sequence v s)))
      (if (= n (length v)) v (subseq v 0 n)))))

(defun %stream-octets (stream encoding)
  (if (subtypep (stream-element-type stream) '(unsigned-byte 8))
      (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
            (buf (make-array 65536 :element-type '(unsigned-byte 8))))
        (loop for n = (read-sequence buf stream)
              while (plusp n)
              do (loop for i below n do (vector-push-extend (aref buf i) out)))
        (coerce out '(simple-array (unsigned-byte 8) (*))))
      (babel:string-to-octets (uiop:slurp-stream-string stream) :encoding encoding)))

(defun %input-file (input encoding temps)
  "INPUT as a UIOP :INPUT argument. String / octets / stream content goes
through a temp file (pushed on the TEMPS cell) so nothing is decoded."
  (flet ((temp (octets)
           (let ((p (%temp-file "process-in")))
             (push p (car temps))
             (%write-octets octets p))))
    (typecase input
      (null nil)
      (string (temp (babel:string-to-octets input :encoding encoding)))
      (octets (temp input))
      (stream (temp (%stream-octets input encoding)))
      (t input))))

(defun %decode (octets kind encoding command)
  "Captured OCTETS as KIND: NIL → empty octets, :OCTETS → OCTETS, :STRING → decoded."
  (ecase kind
    ((nil) (%empty-octets))
    (:octets octets)
    (:string
     (handler-case (babel:octets-to-string octets :encoding encoding)
       (babel-encodings:character-decoding-error (c)
         (%wrap command (format nil "cannot decode output as ~a" encoding) c))))))

(defun %capture (path kind encoding command)
  (%decode (if kind (%read-octets path) (%empty-octets)) kind encoding command))

;;; Pipe capture
;;;
;;; With threads (SBCL :SB-THREAD), RUN captures stdout / stderr through pipes,
;;; one drain thread per pipe, not through temp files: a sandboxed child
;;; (macOS Seatbelt) may be denied fstat(2) on a regular file it inherited but
;;; did not open, and macOS cat(1) exits on that (egao1980/compute-backend-seatbelt#2).
;;; A pipe has no path to deny. Without threads, temp files as before.

(defun %pipe-capture-p ()
  #+sb-thread t
  #-sb-thread nil)

(defun %drain-octets (stream)
  "Read STREAM to EOF, close it. → the octets, or the error that stopped the read."
  (handler-case
      (unwind-protect
           (let ((chunks '())
                 (total 0))
             (loop (let* ((buf (make-array 65536 :element-type '(unsigned-byte 8)))
                          (n (read-sequence buf stream)))
                     (when (zerop n) (return))
                     (push (if (= n (length buf)) buf (subseq buf 0 n)) chunks)
                     (incf total n)))
             (let ((out (make-array total :element-type '(unsigned-byte 8)))
                   (start 0))
               (dolist (chunk (nreverse chunks) out)
                 (replace out chunk :start1 start)
                 (incf start (length chunk)))))
        (ignore-errors (close stream)))
    (error (c) c)))

(defun %start-drain (stream)
  "A thread draining STREAM (%DRAIN-OCTETS), or NIL when STREAM is NIL."
  (when stream
    #+sb-thread
    (sb-thread:make-thread #'%drain-octets :name "process-backend-uiop drain"
                                           :arguments (list stream))
    #-sb-thread
    (error "pipe capture needs threads")))

(defun %drain-done-p (thread)
  #+sb-thread (not (sb-thread:thread-alive-p thread))
  #-sb-thread (declare (ignore thread)) #-sb-thread t)

(defun %join-drain (thread handle timeout command)
  "THREAD's octets (empty for NIL). Waits for EOF: every writer of the pipe (the
child and any descendant that kept it) has closed it. With TIMEOUT, polls and
signals PROCESS-TIMEOUT-ERROR + CONTINUE each time it expires."
  (if (null thread)
      (%empty-octets)
      (let ((result
              #+sb-thread
              (if (null timeout)
                  (sb-thread:join-thread thread :default nil)
                  (loop
                    (let ((deadline (+ (get-internal-real-time)
                                       (ceiling (* timeout internal-time-units-per-second))))
                          (pause 0.001))
                      (loop until (or (%drain-done-p thread)
                                      (>= (get-internal-real-time) deadline))
                            do (sleep pause)
                               (setf pause (min 0.05 (* pause 2))))
                      (when (%drain-done-p thread)
                        (return (sb-thread:join-thread thread :default nil)))
                      (restart-case
                          (error 'process-protocol:process-timeout-error
                                 :command (handle-command handle) :timeout timeout
                                 :process handle)
                        (continue ()
                          :report (lambda (s)
                                    (format s "Wait another ~a seconds for the output." timeout))
                          nil)))))
              #-sb-thread nil))
        (typecase result
          (octets result)
          (condition (%wrap command "cannot read output" result))
          (t (%wrap command "cannot read output" "the drain thread did not finish"))))))

;;; Waiting

(defun %reap (handle)
  (let ((code (uiop:wait-process (handle-info handle))))
    (setf (handle-exit-code% handle) code)
    code))

(defun %wait-until (handle seconds)
  "Poll HANDLE for SECONDS. → exit code, or NIL when still running."
  (let ((info (handle-info handle))
        (deadline (+ (get-internal-real-time)
                     (ceiling (* seconds internal-time-units-per-second))))
        (pause 0.001))
    (loop
      (unless (uiop:process-alive-p info)
        (return (%reap handle)))
      (when (>= (get-internal-real-time) deadline)
        (return nil))
      (sleep pause)
      (setf pause (min 0.05 (* pause 2))))))

(defun %wait (handle timeout)
  "Exit code of HANDLE. With TIMEOUT, PROCESS-TIMEOUT-ERROR + CONTINUE each time it expires."
  (if (null timeout)
      (%reap handle)
      (loop
        (let ((code (%wait-until handle timeout)))
          (when code (return code))
          (restart-case
              (error 'process-protocol:process-timeout-error
                     :command (handle-command handle) :timeout timeout :process handle)
            (continue ()
              :report (lambda (s) (format s "Wait another ~a seconds for the process." timeout))
              nil))))))

(defun %kill-and-reap (handle)
  "Kill HANDLE (its tree when launched with :PROCESS-GROUP T) and reap it."
  (let ((info (handle-info handle)))
    (cond ((handle-process-group-p handle) (%kill-tree handle t))
          ((uiop:process-alive-p info) (%kill-child handle t)))
    (%reap handle)))

;;; Protocol methods

(defmethod process-protocol:backend-run
    ((backend uiop-process-backend) command
     &key input (output :octets) (error-output :octets error-output-p) (error nil error-p)
       directory env (env-mode :merge) timeout discard-stderr shell
       (external-format :utf-8) process-group)
  (declare (ignore backend))
  (check-type timeout (or null (real 0)))
  (when process-group (%check-group-request command input))
  (let* ((encoding (%encoding external-format))
         (out-kind (process-protocol:normalize-capture output :output))
         (err-kind (cond (discard-stderr nil)
                         (error-output-p (process-protocol:normalize-capture error-output :error-output))
                         (error-p (process-protocol:normalize-capture error :error))
                         (t :octets)))
         (pipes (%pipe-capture-p))
         (temps (list '()))
         (handle nil)
         (finished nil))
    (multiple-value-bind (cmd env-keys) (%command-and-env command shell env env-mode)
      (unwind-protect
           (let* ((in (%input-file input encoding temps))
                  (out-file (when (and out-kind (not pipes))
                              (car (push (%temp-file "process-out") (car temps)))))
                  (err-file (when (and err-kind (not pipes))
                              (car (push (%temp-file "process-err") (car temps)))))
                  (info (let ((out-spec (if (and pipes out-kind) :stream out-file))
                              (err-spec (if (and pipes err-kind) :stream err-file)))
                          (handler-case
                              (with-private-std-handles (in out-spec err-spec)
                                (apply #'uiop:launch-program cmd
                                       :input in
                                       :output out-spec
                                       :error-output err-spec
                                       (append (when pipes
                                                 (list :element-type '(unsigned-byte 8)))
                                               (when directory (list :directory directory))
                                               env-keys)))
                            (error (c) (%wrap command "cannot run program" c)))))
                  ;; Drain before anything can block: a full pipe stalls the child.
                  (out-drain (when pipes (%start-drain (uiop:process-info-output info))))
                  (err-drain (when pipes (%start-drain (uiop:process-info-error-output info)))))
             (setf handle (%make-handle command info process-group))
             (let ((code (%wait handle timeout)))
               (if pipes
                   (let ((out (%join-drain out-drain handle timeout command))
                         (err (%join-drain err-drain handle timeout command)))
                     (setf finished t)
                     (values code
                             (%decode out out-kind encoding command)
                             (%decode err err-kind encoding command)))
                   (progn
                     (setf finished t)
                     (values code
                             (%capture out-file out-kind encoding command)
                             (%capture err-file err-kind encoding command))))))
        ;; Leaving early (timeout, unwind) kills the child (its tree with
        ;; :PROCESS-GROUP T), which closes the pipes: the drain threads end.
        (when (and handle (not finished))
          (%kill-and-reap handle))
        (mapc #'uiop:delete-file-if-exists (car temps))))))

(defmethod process-protocol:backend-launch
    ((backend uiop-process-backend) command
     &key input output (error-output nil error-output-p) error directory env
       (env-mode :merge) shell element-type (external-format :utf-8) process-group)
  (declare (ignore backend))
  (when process-group (%check-group-request command input))
  (let ((element-type (process-protocol:normalize-element-type element-type))
        (error-output (if error-output-p error-output error)))
    (multiple-value-bind (cmd env-keys) (%command-and-env command shell env env-mode)
      (%make-handle
       command
       (handler-case
           (let ((in (or input :stream))
                 (out (or output :stream))
                 (err (or error-output :stream)))
             (with-private-std-handles (in out err)
               (apply #'uiop:launch-program cmd
                      :input in
                      :output out
                      :error-output err
                      :element-type element-type
                      :external-format external-format
                      (append (when directory (list :directory directory))
                              env-keys))))
         (error (c) (%wrap command "cannot run program" c)))
       process-group))))

(defmethod process-protocol:process-wait ((handle uiop-process-handle) &key timeout)
  (check-type timeout (or null (real 0)))
  (or (handle-exit-code% handle)
      (%wait handle timeout)))

(defmethod process-protocol:process-kill ((handle uiop-process-handle) &key force tree)
  (cond
    ((not tree) (%kill-child handle force))
    ((handle-process-group-p handle) (%kill-tree handle force))
    (t
     (restart-case
         (error 'process-protocol:process-group-error
                :command (handle-command handle) :process handle
                :message "kill :tree t needs a handle launched with :process-group t")
       (continue ()
         :report "Kill the direct child only."
         (%kill-child handle force)))))
  t)

(defmethod process-protocol:process-pid ((handle uiop-process-handle))
  (handle-pid handle))

(defmethod process-protocol:backend-supports-p
    ((backend uiop-process-backend) (capability (eql :pid)))
  t)

(defmethod process-protocol:backend-supports-p
    ((backend uiop-process-backend) (capability (eql :process-group)))
  (%group-supported-p))

(defmethod process-protocol:process-alive-p ((handle uiop-process-handle))
  (uiop:process-alive-p (handle-info handle)))

(defmethod process-protocol:process-stdin ((handle uiop-process-handle))
  (uiop:process-info-input (handle-info handle)))

(defmethod process-protocol:process-stdout ((handle uiop-process-handle))
  (uiop:process-info-output (handle-info handle)))

(defmethod process-protocol:process-stderr ((handle uiop-process-handle))
  (uiop:process-info-error-output (handle-info handle)))

(defmethod process-protocol:process-exit-code ((handle uiop-process-handle))
  (or (handle-exit-code% handle)
      (unless (uiop:process-alive-p (handle-info handle))
        (%reap handle))))

(use-uiop-process-backend)
