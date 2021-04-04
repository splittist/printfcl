;;;; printfcl.asd

(asdf:defsystem #:printfcl
  :description "An extensible c-like printf implementation"
  :author "John Q. Splittist <splittist@splittist.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "printfcl")))

(asdf:defsystem #:printfcl/test
  :depends-on (#:printfcl)
  :serial t
  :components ((:file "test")))

(asdf:defsystem #:printfcl/javatime
  :description "Java-like time converters for printf"
  :depends-on (#:printfcl #:local-time)
  :serial t
  :components ((:file "javatime")))
