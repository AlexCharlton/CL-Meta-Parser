;;;; meta-parser.asd

(asdf:defsystem #:meta-parser
  :description "An OMeta inspired parser."
  :author "Alexander Charlton"
  :license "Public domain"
  :serial t
  :components ((:file "package")
               (:file "meta-parser")
               (:file "grammar")))

