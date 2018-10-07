;;;; rtf-reader.asd

(asdf:defsystem #:rtf-reader
  :description "Describe rtf-reader here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:esrap #:alexandria #:cl-fad)
  :components ((:file "package")
               (:file "rtf-reader")))
