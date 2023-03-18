(defsystem "clcm"
  :depends-on ("cl-ppcre" "clcm/clcm")
  :class :package-inferred-system
  :license "mit"
  :version "0.1.0"
  :author "miyao satoaki"
  :pathname "src/"
  :in-order-to ((test-op (test-op "clcm/test"))))

(defsystem "clcm/test"
  :depends-on ("cl-json" "clcm/test/main")
  :license "mit"
  :author "miyao satoaki"
  :perform (test-op (op sys) (uiop:symbol-call :clcm/test/main :test)))
