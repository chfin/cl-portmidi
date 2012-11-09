;;;; portmidi.asd

(asdf:defsystem #:portmidi
  :serial t
  :description "Bindings to the PortMidi library"
  :author "Christoph Finkensiep <ch.finkensie@freenet.de>"
  :license "X11, see LICENSE"
  :depends-on (#:cffi)
  :components ((:file "package")
	       (:file "setup")
	       (:file "types")
               (:file "portmidi")
	       (:file "messages")))
