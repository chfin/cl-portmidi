;;;; portmidi.asd

(asdf:defsystem #:portmidi
  :serial t
  :description "Describe portmidi here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "portmidi")
	       (:file "messages")))
