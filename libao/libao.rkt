#lang racket/base

(require "libao-ffi.rkt"
         (prefix-in fin: finalizer)
         ffi/unsafe)

(provide ao-open-live
         ao-play
         ao-mk-format
         ao-close
         ao-default-driver-id
         )

(define devices (make-hash))
(define device-number 1)


(define-struct ao-handle (handle-num
                          [bits #:auto #:mutable]
                          [bytes-per-sample #:auto #:mutable]
                          [byte-format #:auto #:mutable]
                          [channels #:auto #:mutable]
                          [rate #:auto #:mutable]
                          )
  #:auto-value #f
  )

(ao_initialize)

(define libao-plumber-flus-handle
  (plumber-add-flush! (current-plumber)
                      (lambda (my-handle)
                        (hash-for-each devices
                                       (lambda (handle-num device)
                                         (ao-close handle-num)))
                        (set! devices (make-hash))
                        (ao_shutdown)
                        (plumber-flush-handle-remove! my-handle)
                        )))



(define (ao-mk-format bits rate channels byte-format . matrix)
  (let ((bf (if (eq? byte-format 'little-endian)
                AO-FMT-LITTLE
                (if (eq? byte-format 'big-endian)
                    AO-FMT-BIG
                    AO-FMT-NATIVE))))
    (let ((format (make-ao_sample_format bits rate channels bf #f)))
      format)))

(define (ao-endianness->symbol e)
  (if (= e AO-FMT-LITTLE)
      'little-endian
      (if (= e AO-FMT-BIG)
          'big-endian
          'native)))

(define (ao-default-driver-id)
   (ao_default_driver_id))

(define (ao-open-live driver-id sample-format . options)
  (let ((id (if (eq? driver-id #f) (ao-default-driver-id) driver-id)))
    (let ((ao-device (ao_open_live id sample-format #f)))
      (if (eq? ao-device #f)
          (let ((handle (ao-handle -1)))
            handle)
          (let ((handle-num device-number))
            (set! device-number (+ device-number 1))
            (let ((handle (ao-handle handle-num)))
              (let* ((bits (ao_sample_format-bits sample-format))
                     (bytes-per-sample (inexact->exact (round (/ bits 8))))
                     (channels (ao_sample_format-channels sample-format))
                     (endianness (ao-endianness->symbol
                                  (ao_sample_format-byte_format sample-format)))
                     (rate (ao_sample_format-rate sample-format))
                    )
                (set-ao-handle-bits! handle bits)
                (set-ao-handle-bytes-per-sample! handle bytes-per-sample)
                (set-ao-handle-byte-format! handle endianness)
                (set-ao-handle-rate! handle rate)
                (set-ao-handle-channels! handle channels)
                (hash-set! devices handle-num ao-device)
                (fin:register-finalizer handle
                                        (lambda (handle)
                                          (ao-close handle)))
                handle))
            ))
      )))
              
(define (ao-close handle)
  (if (number? handle)
      (let ((ao-device (hash-ref devices handle #f)))
        (unless (eq? ao-device #f)
          (let ((r (ao_close ao-device)))
            (when (= r 0)
              (printf "Unexpected: cannot close ao-device"))))
        'internally-closed)
      (let ((handle-num (ao-handle-handle-num handle)))
        (let ((ao-device (hash-ref devices handle-num #f)))
          (if (eq? ao-device #f)
              'error-ao-device-non-existent
              (let ((r (ao_close ao-device)))
                (hash-remove! devices handle-num)
                (if (= r 0)
                    'error-closing-ao-device
                    'ok)))))))


(define count 0)
(define (abs x) (if (>= x 0) x (* x -1)))

(define (make-sample-bytes sample bytes-per-sample endianess)
  (letrec ((mk (lambda (i d)
                 (if (< i bytes-per-sample)
                     (cons (bitwise-and d 255)
                           (mk (+ i 1) (arithmetic-shift d -8)))
                     '()))))
    (let ((bytes (mk 0 sample)))
      (if (eq? endianess 'big-endian)
          (reverse bytes)
          bytes))))

(define (ao-play handle buffer)
  (let* ((bytes-per-sample (ao-handle-bytes-per-sample handle))
         (bits (ao-handle-bits handle))
         (channels (ao-handle-channels handle))
         (endianess (ao-handle-byte-format handle))
         (buf-len (vector-length (car buffer)))
         (audio-buf-len (* channels bytes-per-sample buf-len))
         (audio (malloc 'atomic audio-buf-len))
         (get-sample (lambda (k channel)
                       (let ((chan-buf (list-ref buffer channel)))
                         (vector-ref chan-buf k))))
         )
    ;(displayln (format "bps: ~a, buf-len: ~a, endianess: ~a, channels: ~a, bits ~a"
    ;                   bytes-per-sample buf-len endianess channels bits))
    (letrec ((i 0)
             (fill (lambda (k channel)
                     (if (< k buf-len)
                         (if (< channel channels)
                             (let* ((sample (get-sample k channel))
                                    (bytes (make-sample-bytes sample bytes-per-sample endianess))
                                    )
                               (for-each (lambda (byte)
                                           (ptr-set! audio _byte i byte)
                                           (set! i (+ i 1)))
                                         bytes)
                               ;; process sample
                               (fill k (+ channel 1)))
                             (fill (+ k 1) 0))
                         'filled))
                   ))
      (fill 0 0)
      (let* ((handle-num (ao-handle-handle-num handle))
             (ao-device (hash-ref devices handle-num #f)))
        (if (eq? ao-device #f)
            (error "No device for this handle")
            (ao_play ao-device audio audio-buf-len))))))




