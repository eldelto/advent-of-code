;;;; advent-of-code.asd

(asdf:defsystem #:advent-of-code
  :description "Describe advent-of-code here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("uiop" "alexandria")
  :components ((:file "package")
               (:file "01")
	       (:file "02")
	       (:file "03")
	       (:file "04")
	       (:file "05")))
