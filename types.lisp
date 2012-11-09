;;;; types.lisp

(in-package #:portmidi)

(defctype pm-stream :void)
(defctype pm-device-id :int)
(defctype pm-timestamp :int32)
(defctype pm-message :int32)

(defcstruct pm-device-info
    "Information about a device. Usually owned by the lib."
  (structVersion :int)
  (interf :string)
  (name :string)
  (input :boolean)
  (output :boolean)
  (opened :boolean))

(defclass device-info ()
  ((interf
    :initarg :interf
    :reader device-info-interf)
   (name
    :initarg :name
    :reader device-info-name)
   (direction
    :initarg :direction
    :reader device-info-direction)
   (opened
    :initarg :opened
    :reader device-info-open-p))
  (:documentation "Contains information about a MIDI device.
Slots:
- interf, the name of the device's interface, string
- name, the name of the device, string
- direction, the direction of the device's stream, either :input or :output
- opened, indicates, if the device is open, boolean
Readers: device-info-
- interf: reads interf
- name: reads name
- direction: reads direction
- open-p: reads opened"))

(defmethod translate-from-foreign (value (type (eql 'pm-device-info)))
  )

(defcstruct pm-event
    "The PortMidi event structure. Streams use instances of this."
  (message pm-message)
  (timestamp pm-timestamp))

(define-foreign-type midi-stream-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser midi-stream))

;;; pm-error

(define-foreign-type pm-error-type ()
  ()
  (:actual-type :int)
  (:simple-parser pm-error))

(defmethod translate-from-foreign (value (type pm-error-type))
  "Raise an error, if value, a PmError code, is non-zero."
  (case value
    (0 nil)
    (1 t)
    (t (error (get-error-text value)))))
