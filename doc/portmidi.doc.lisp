;;;; portmidi.doc.lisp

(in-package #:portmidi.doc)

(defparameter *css* "
/* inspired by github ;) */
body {
    font-family: Helvetica,arial,sans;
    font-size: 14px;
    color: rgb(51,51,51);
    margin: 30px;
}

h1 {
    font-size: 28px;
    color: rgb(0,0,0)
}

h2 {
    font-size: 24px;
    color: rgb(0,0,0);
    border-bottom: 1px solid rgb(204,204,204);
}

code, tt {
    font-size: 12px;
    background-color: rgb(248,248,248);
    border: 1px solid rgb(234,234,234);
    border-radius: 3px 3px 3px 3px;
    padding: 0px 5px;
}

.apiref-row {
    padding-bottom: 1em;
    display: block;
    float: none;
}

.apiref-spec {
    display: inline;
    font-family: monospace;
    font-weight: bold;
    text-transform: lowercase;
}

.apiref-lambda {
    display: inline;
    font-family: monospace;
    text-transform: lowercase;
}

.apiref-result {
    display: block;
    /*font-family: monospace;*/
    font-style: italic;
    margin-left: 20px;
}

.apiref-doc {
    display: block;
    float: none;
    padding-left: 30px;
    border-bottom: 1px solid rgb(204,204,204);
    min-height: 1em;
}

pre {
    display: block;
    background: lightgrey;
    border: 1px solid black;
    padding: 10px;
    margin: 20px 5px 20px 5px;
}")

(defun dump-string (string file)
  (with-open-file (stream file :direction :output
			  :if-exists :overwrite
			  :if-does-not-exist :create
			  :external-format :utf8)
    (write-string string stream)))

(defun create-doc (directory)
  (let ((doc-f (make-pathname :name "portmidi" :type "html" :defaults directory))
	(css-f (make-pathname :name "portmidi" :type "css" :defaults directory)))
    (dump-string *css* css-f)
    (gendoc:gendoc (:output-filename doc-f :css "portmidi.css")
		   (:apiref #:portmidi))))
