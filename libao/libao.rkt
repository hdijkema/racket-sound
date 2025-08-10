#lang racket/base

(require "libao-ffi.rkt")
;(require finalizer)

(provide ao-open-live
         ao-play
         ao-mk-format
         ao-close
         ao-default-driver-id
         )

(define currently-open   0)
(define ao-device-handle 0)
(define ao-devices (make-hash))

(define (ao-mk-format bits rate channels byte-format . matrix)
  (let ((bf (if (eq? byte-format 'little-endian)
                AO-FMT-LITTLE
                (if (eq? byte-format 'big-endian)
                    AO-FMT-BIG
                    AO-FMT-NATIVE))))
    (let ((format (make-ao_sample_format bits rate channels bf #f)))
      format)))

(define (ao-default-driver-id)
   (ao_default_driver_id))
  

(define (ao-open-live driver-id sample-format . options)
  (when (= currently-open 0)
    (ao_initialize))
  (let ((ao-device (ao_open_live driver-id sample-format #f)))
    (set! currently-open (+ currently-open 1))
    (when (eq? ao-device _cpointer/null)
      (set! currently-open (- currently-open 1))
      (when (= currently-open 0)
        (ao_shutdown)))
    (if (eq? ao-device _cpointer/null)
        #f
        (begin
          (set! ao-device-handle (+ ao-device-handle 1))
          (hash-set! ao-devices ao-device-handle ao-device)
          ao-device-handle))))

(define (ao-close ao-handle)
  (let ((ao-device (hash-ref ao-devices ao-handle #f)))
    (when (eq? ao-device #f)
        (error (format "Not a valid ao-handle ~a" ao-handle)))
    (hash-remove! ao-devices ao-handle)
    (let ((r (ao_close ao-device)))
      (set! currently-open (- currently-open 1))
      (when (= currently-open 0)
        (ao_shutdown))
      r)))

(define (ao-play)
  #t)




