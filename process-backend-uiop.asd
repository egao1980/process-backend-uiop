(defsystem "process-backend-uiop"
  :version "0.2.0"
  :description "UIOP backend for process-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "process-protocol" "0.3.0") "babel")

  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "process-backend-uiop/tests"))))

(defsystem "process-backend-uiop/tests"
  :depends-on ("process-backend-uiop" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
