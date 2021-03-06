;;;; messages.lisp

(in-package #:portmidi)

(defun make-message (status data1 data2)
  "=> an integer representing a MIDI message
Combines the integers `status`, `data1` and `data2` to a MIDI message."
  (let ((d2 (boole boole-and (ash data2 16) #xFF0000))
	(d1 (boole boole-and (ash data1 8) #xFF00))
	(st (boole boole-and status #xFF)))
    (boole boole-ior d2
	   (boole boole-ior d1 st))))

(defun message-status (msg)
  "=> the status byte of the MIDI message `msg` as an integer"
  (boole boole-and msg #xFF))

(defun message-data1 (msg)
  "=> the first data byte of the MIDI message `msg` as an integer"
  (boole boole-and (ash msg -8) #xFF))

(defun message-data2 (msg)
  "=> the second data byte of the MIDI message `msg` as an integer"
  (boole boole-and (ash msg -16) #xFF))

(defun make-message* (upper lower data1 data2) ;internal
  "=> a MIDI message as an integer
Works like `make-message` but combines `upper` and `lower` to the status byte."
  (let ((status (boole boole-ior
		       (boole boole-and (ash upper 4) #xF0)
		       (boole boole-and lower #xF))))
    (make-message status data1 data2)))

(defun note-off (channel note &optional (velocity 80))
  "=> a \"Note Off\" MIDI message"
  (make-message* 8 channel note velocity))

(defun note-on (channel note &optional (velocity 80))
  "=> a \"Note On\" MIDI message"
  (make-message* 9 channel note velocity))
