(in-package #:process-backend-uiop/tests)

(deftest run-echo
  (multiple-value-bind (code out err)
      (process-protocol:run '("echo" "hi"))
    (ok (zerop code))
    (ok (zerop (length err)))
    (ok (search "hi" (babel:octets-to-string out :encoding :utf-8)))))

(deftest run-false-exit
  (multiple-value-bind (code out err)
      (process-protocol:run '("false"))
    (declare (ignore out err))
    (ok (= 1 code))))

(deftest launch-wait
  (let ((h (process-protocol:launch '("echo" "x"))))
    (ok (process-protocol:alive-p h))
    (ok (zerop (process-protocol:wait h)))
    (ng (process-protocol:alive-p h))))

(deftest backend-bound
  (ok (typep process-protocol:*process-backend*
             'process-backend-uiop:uiop-process-backend)))
