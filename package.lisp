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
	   #:note-off #:note-on))
