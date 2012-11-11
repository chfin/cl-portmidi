;;;; portmidi.doc.lisp

(in-package #:portmidi.doc)

(defun create-doc ()
  "Creates the documentation (doc/portmidi.html) using cl-gendoc."
  (gendoc:gendoc ( :output-filename "portmidi.html" :css "portmidi.css" :title "portmidi - PortMidi bindings for Common Lisp" :output-system :portmidi.doc)
		 (:mdf "intro.md")
		 (:apiref #:portmidi #:portmidi.doc)))
