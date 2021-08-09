;;;; package.lisp

(defpackage #:portmidi
  (:use #:cl #:cffi)
  (:nicknames #:pm)
  (:export #:initialize #:terminate
	   #:has-host-error #:get-error-text
	   #:count-devices
	   #:get-default-input-device-id #:get-default-output-device-id
	   #:get-device-info #:list-devices
	   #:open-input #:open-output
	   #:channel #:channels #:set-channel-mask
	   #:abort-midi #:close-midi #:synchronize-midi
	   #:read-midi #:write-midi #:write-short-midi 

	   #:message-status #:message-data1 #:message-data2
           #:make-message #:make-message*
	   #:note-off #:note-on)
  (:documentation
   "portmidi provides Common Lisp bindings to the PortMidi library.
In addition to the functions definend by portmidi.h it tries to provide some useful utility functions and abstractions.
@begin[Dependencies]{section}
  @begin{itemize}
    @item{CFFI}
    @item{The PortMidi library}
  @end{itemize}
@end{section}
@begin[Installation]{section}
  portmidi provides an ASDF system which can be loaded via ASDF or Quicklisp (as a local project so far).
@end{section}
@begin[Usage]{section}
  TODO: write something useful here.

  The package has the nickname pm.
@end{section}"))
