(in-package #:process-backend-uiop)

(defclass uiop-process-backend (process-protocol:process-backend) ())

(defun make-uiop-process-backend ()
  (make-instance 'uiop-process-backend))

(defun use-uiop-process-backend ()
  (setf process-protocol:*process-backend* (make-uiop-process-backend)))

(defclass uiop-process-handle (process-protocol:process-handle)
  ((info :initarg :info :reader handle-info)
   (exit-code :initform nil :accessor handle-exit-code%)))

(defun %normalize-command (command shell)
  (cond
    (shell
     (etypecase command
       (string command)
       (list (format nil "~{~a~^ ~}" command))))
    (t
     (etypecase command
       (list command)
       (string
        (error 'process-protocol:process-error
               :message "string command requires :shell t"))))))

(defun %octets (string)
  (if (and string (plusp (length string)))
      (babel:string-to-octets string :encoding :utf-8)
      (make-array 0 :element-type '(unsigned-byte 8))))

(defun %input-arg (input)
  (cond
    ((null input) nil)
    ((stringp input) input)
    ((typep input '(vector (unsigned-byte 8)))
     (babel:octets-to-string input :encoding :utf-8))
    ((streamp input) input)
    (t input)))

(defmethod process-protocol:backend-run
    ((backend uiop-process-backend) command
     &key input output error directory env timeout discard-stderr shell)
  (declare (ignore backend output error timeout))
  (let ((cmd (%normalize-command command shell)))
    (handler-case
        (multiple-value-bind (out err code)
            (apply #'uiop:run-program cmd
                   (append
                    (list :ignore-error-status t
                          :output '(:string :stripped nil)
                          :error-output (if discard-stderr
                                           nil
                                           '(:string :stripped nil))
                          :input (%input-arg input)
                          :force-shell (and shell t))
                    (when directory (list :directory directory))
                    (when env (list :environment env))))
          (values code (%octets (or out "")) (%octets (or err ""))))
      (error (c)
        (error 'process-protocol:process-error
               :message (format nil "~a" c))))))

(defmethod process-protocol:backend-launch
    ((backend uiop-process-backend) command
     &key input output error directory env shell)
  (declare (ignore backend))
  (let* ((cmd (%normalize-command command shell))
         (info (apply #'uiop:launch-program cmd
                      (append
                       (list :input (or input :stream)
                             :output (or output :stream)
                             :error-output (or error :stream)
                             :force-shell (and shell t))
                       (when directory (list :directory directory))
                       (when env (list :environment env))))))
    (make-instance 'uiop-process-handle :info info)))

(defmethod process-protocol:process-wait ((handle uiop-process-handle) &key timeout)
  (declare (ignore timeout))
  (let ((code (uiop:wait-process (handle-info handle))))
    (setf (handle-exit-code% handle) code)
    code))

(defmethod process-protocol:process-kill ((handle uiop-process-handle) &key force)
  (uiop:terminate-process (handle-info handle) :urgent force)
  t)

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
        (setf (handle-exit-code% handle)
              (uiop:wait-process (handle-info handle))))))

(use-uiop-process-backend)
