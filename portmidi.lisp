;;;; portmidi.lisp

(in-package #:portmidi)

(define-foreign-library libportmidi
    (:unix (:or "libportmidi.so.0" "libportmidi.so"))
  (t (:default "libportmidi")))

(use-foreign-library libportmidi)

;(defctype pm-error :int)
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

(defcstruct pm-event
    "The PortMidi event structure. Streams use instances of this."
  (message pm-message)
  (timestamp pm-timestamp))

(define-foreign-type midi-stream-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser midi-stream))

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

(defcfun (initialize "Pm_Initialize") :void)

(defcfun (terminate "Pm_Terminate") :void)

(defcfun (has-host-error "Pm_HasHostError") :int
  (stream midi-stream))

(defcfun (get-error-text "Pm_GetErrorText") :string
  (errnum pm-error))

;;TODO: Pm_GetHostErrorText

(defcfun (count-devices "Pm_CountDevices") :int)

(defcfun (get-default-input-device-id "Pm_GetDefaultInputDeviceID")
    pm-device-id)

(defcfun (get-default-output-device-id "Pm_GetDefaultOutputDeviceID")
    pm-device-id)

(defcfun (get-device-info "Pm_GetDeviceInfo") :pointer
  (id pm-device-id))

(defun describe-device (device-id)
  (with-foreign-slots ((interf name input output)
		       (get-device-info device-id)
		       pm-device-info)
    (format nil "~a | ~a | input: ~a | output: ~a"
	    interf name input output)))

(defun list-devices ()
  (loop for i below (count-devices)
     collect (describe-device i)))

(defun open-input (input-device buffer-size)
  (with-foreign-object (stream-ptr :pointer)
    (let ((err (foreign-funcall "Pm_OpenInput"
				:pointer stream-ptr
				pm-device-id input-device
				:int32 buffer-size
				:pointer (null-pointer)
				:pointer (null-pointer)
				pm-error)))
      (case err
	(mem-ref stream-ptr 'midi-stream)
	(error (get-error-text errnum))))))

(defun open-output (output-device buffer-size latency)
  (with-foreign-object (stream-ptr :pointer)
    (let ((err (foreign-funcall "Pm_OpenOutput"
				:pointer stream-ptr
				pm-device-id output-device
				:int32 buffer-size
				:pointer (null-pointer)
				:pointer (null-pointer)
				:int32 latency
				pm-error)))
      (if err
	  (error (get-error-text errnum))
	  (mem-ref stream-ptr 'midi-stream)))))

;;TODO: filtering, "Pm_SetFilter"

;;resembles the Pm_Channel() macro
(defun channel (channel)
  (ash 1 channel))

;;creates the bit mask for multiple channels
(defun channels (&rest channels)
  (apply #'+ (mapcar #'channel channels)))

(defcfun (set-channel-mask "Pm_SetChannelMask") pm-error
  (stream midi-stream)
  (mask :int))

;;note: "Pm_Abort" renamed to "abort-stream"
(defcfun (abort-midi "Pm_Abort") pm-error
  (stream midi-stream))

(defcfun (close-midi "Pm_Close") pm-error
  (stream midi-stream))

(defcfun (synchronize-midi "Pm_Synchronize") pm-error
  (stream midi-stream))

;; TODO: message operations

(defun read-midi (stream)
  (with-foreign-object (buffer 'pm-event)
    (when (foreign-funcall "Pm_Read"
			   midi-stream stream
			   :pointer buffer
			   :int32 1
			   pm-error)
      (mem-ref buffer))))

(defcfun (poll-midi "Pm_Poll") pm-error
  (stream midi-stream))

(defun write-midi (stream event)
  (foreign-funcall "Pm_Write"
		   midi-stream stream
		   pm-event event
		   :int32 1
		   pm-error))

(defun write-short-midi (stream timestamp message)
  (with-foreign-object (event 'pm-event)
    (setf (foreign-slot-value event 'pm-event 'message) message
	  (foreign-slot-value event 'pm-event 'timestamp) timestamp)
    (write-midi stream event)))
