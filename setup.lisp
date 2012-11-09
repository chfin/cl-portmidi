;;;; setup.lisp

(in-package #:portmidi)

(define-foreign-library libportmidi
    (:unix (:or "libportmidi.so.0" "libportmidi.so"))
  (t (:default "libportmidi")))

(use-foreign-library libportmidi)
