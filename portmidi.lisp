;;;; portmidi.lisp

(in-package #:portmidi)

(defcfun (initialize "Pm_Initialize") :void
  "=> nil
Initializes PortMidi. It's automatically called when loading the system, but you can also use it to re-initialize it after a call to terminate.")

;; ... here
(initialize)

(defcfun (terminate "Pm_Terminate") :void
  "=> nil
Terminates PortMidi.")

(defcfun (has-host-error "Pm_HasHostError") :boolean
  "=> t if there are pending host errors for `stream`"
  (stream midi-stream))

(defcfun (get-error-text "Pm_GetErrorText") :string
  "=> the error message corresponding to `errnum`"
  (errnum pm-error))

(defun get-host-error-text (stream)
  "=> the latest host error message for `stream`"
  (with-foreign-object (msg :string 256)
    (foreign-funcall "Pm_GetHostErrorText"
		     :string msg
		     :unsigned-int 256
		     :void)
    msg))

(defcfun (count-devices "Pm_CountDevices") :int
  "=> the number of available devices
Valid device ids range from `0` to `(- (count-devices) 1)`.")

(defcfun (get-default-input-device-id "Pm_GetDefaultInputDeviceID")
    pm-device-id
  "=> the id of the default input device")

(defcfun (get-default-output-device-id "Pm_GetDefaultOutputDeviceID")
    pm-device-id
  "=> he id of the default output device")

(defcfun (get-device-info "Pm_GetDeviceInfo") pm-device-info
  "=> an instance of device-info describing the device given by `id`
To quickly inspect a device use `describe-device`."
  (id pm-device-id))

(defun describe-device (device-id)
  "=> a string summarizing the devices information"
  (with-foreign-slots ((interf name input output)
		       (get-device-info device-id)
		       pm-device-info)
    (format nil "~a | ~a | input: ~a | output: ~a"
	    interf name input output)))

(defun list-devices ()
  "=> an alist of device ids and strings describing all available devices.
See `describe-device`"
  (loop for i below (count-devices)
    collect (cons i (describe-device i))))

(defun open-input (input-device buffer-size)
  "=> a `midi-stream` opened to receive MIDI data."
  (with-foreign-object (stream-ptr :pointer)
    (let ((err (foreign-funcall "Pm_OpenInput"
				:pointer stream-ptr
				pm-device-id input-device
				:int32 buffer-size
				:pointer (null-pointer)
				:pointer (null-pointer)
				pm-error)))
      (if err
	(error (get-error-text errnum))
        (mem-ref stream-ptr 'midi-stream)))))

(defun open-output (output-device buffer-size latency)
  "=> a `midi-stream` opened to send MIDI data"
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
  "=> the bitmask for `channel`"
  (ash 1 channel))

(defun channels (&rest channels)
  "=> a bitmask for multiple channels"
  (apply #'+ (mapcar #'channel channels)))

(defcfun (set-channel-mask "Pm_SetChannelMask") pm-error
  "=> nil
Sets a channel mask (`mask`) on `stream`."
  (stream midi-stream)
  (mask :int))

;;note: "Pm_Abort" renamed to "abort-midi"
(defcfun (abort-midi "Pm_Abort") pm-error
  "=> nil
Terminates outgoing messages on `stream`.
You should call `close-midi` immediately after this."
  (stream midi-stream))

(defcfun (close-midi "Pm_Close") pm-error
  "=> nil
Closes `stream`, flushing all buffers."
  (stream midi-stream))

(defcfun (synchronize-midi "Pm_Synchronize") pm-error
  "=> nil
Synchronizes `stream` with the clock.
See PortMidi documentation."
  (stream midi-stream))

(defun read-midi (stream)
  "=> a `midi-event`
Reads a single `midi-event` from `stream`."
  (with-foreign-object (buffer 'pm-event)
    (when (foreign-funcall "Pm_Read"
			   midi-stream stream
			   :pointer buffer
			   :int32 1
			   pm-error)
      (mem-ref buffer :int32))))

(defcfun (poll-midi "Pm_Poll") pm-error
  "=> t, if `stream` has pending incomming events, nil otherwise"
  (stream midi-stream))

(defun write-midi (stream event)
  "=> nil
Writes the `midi-event` `event` to `stream`."
  (foreign-funcall "Pm_Write"
		   midi-stream stream
		   pm-event event
		   :int32 1
		   pm-error))

(defun write-short-midi (stream timestamp message)
  "=> nil
Creates a `midi-event` from `timestamt` and `message` and writes it to `stream`."
  (with-foreign-object (event 'pm-event)
    (setf (foreign-slot-value event 'pm-event 'message) message
	  (foreign-slot-value event 'pm-event 'timestamp) timestamp)
    (write-midi stream event)))
