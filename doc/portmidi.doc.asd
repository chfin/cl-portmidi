;;;; portmidi.doc.asd

(asdf:defsystem #:portmidi.doc
  :serial t
  :description "Used to generate the documentation for portmidi"
  :author "Christoph Finkensiep <ch.finkensiep@freenet.de>"
  :license "X11, see ../LICENSE"
  :depends-on (#:portmidi
	       #:cl-gendoc)
  :components ((:file "package")
               (:file "portmidi.doc")))
