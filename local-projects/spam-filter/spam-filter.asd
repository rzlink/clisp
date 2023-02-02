(defsystem "spam-filter"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "spam-filter/tests"))))

(defsystem "spam-filter/tests"
  :author ""
  :license ""
  :depends-on ("spam-filter"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for spam-filter"
  :perform (test-op (op c) (symbol-call :rove :run c)))
