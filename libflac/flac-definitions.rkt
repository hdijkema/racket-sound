(module flac-definitions racket/base
  
  (provide flac-stream-info
           make-flac-stream-info
           flac-stream-info->string
           
           flac-handle
           make-flac-handle
           flac-handle-ffi-decoder-handler
           flac-handle-stream-info
           set-flac-handle-stream-info!
           flac-handle-cb-stream-info
           set-flac-handle-cb-stream-info!
           flac-handle-cb-audio
           set-flac-handle-cb-audio!

           flac-handle->string

           flac-sample-rate
           flac-channels
           flac-bits-per-sample
           flac-total-samples
           flac-duration
           )

  (define-struct flac-stream-info
    (min-blocksize max-blocksize
     min-framesize max-framesize
     sample-rate
     channels
     bits-per-sample
     total-samples
     ))

  (define (flac-stream-info->string si)
    (format "sample-rate: ~a, channels: ~a, bits-per-sample: ~a, total-samples: ~a"
            (flac-stream-info-sample-rate si)
            (flac-stream-info-channels si)
            (flac-stream-info-bits-per-sample si)
            (flac-stream-info-total-samples si)))

  (define (flac-sample-rate h)
    (let ((si (flac-handle-stream-info h)))
      (if (eq? si #f)
          #f
          (flac-stream-info-sample-rate si))))

  (define (flac-channels h)
    (let ((si (flac-handle-stream-info h)))
      (if (eq? si #f)
          #f
          (flac-stream-info-channels si))))

  (define (flac-bits-per-sample h)
    (let ((si (flac-handle-stream-info h)))
      (if (eq? si #f)
          #f
          (flac-stream-info-bits-per-sample si))))

  (define (flac-total-samples h)
    (let ((si (flac-handle-stream-info h)))
      (if (eq? si #f)
          #f
          (flac-stream-info-total-samples si))))

  (define (flac-handle->string h)
    (let* ((si (flac-handle-stream-info h))
           (ffi (flac-handle-ffi-decoder-handler h))
           (ff  (ffi 'file)))
      (string-append
       (if (eq? ff #f)
           "no flac file available\n"
           (format "Flac File: ~a\n" ff))
       (if (eq? si #f)
           "no stream info available\n"
           (string-append
             (format "Stream Info: ~a\n" (flac-stream-info->string si))
             (format "Duration in seconds: ~a\n" (flac-duration h))))
       )))

  (define (flac-duration h)
    (let* ((si (flac-handle-stream-info h)))
      (if (eq? si #f)
          #f
          (let* ((total-samples (flac-stream-info-total-samples si))
                 (sample-rate (flac-stream-info-sample-rate si)))
            (inexact->exact (round (/ total-samples sample-rate)))))))

  (define-struct flac-handle
    (
     ffi-decoder-handler
     [cb-stream-info #:auto #:mutable]
     [cb-audio #:auto #:mutable]
     [stream-info #:auto #:mutable]
     )
     #:auto-value #f
    )

  ); end of module
