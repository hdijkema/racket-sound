(module flac-decoder racket/base

  (require ffi/unsafe
           "libflac-ffi.rkt"
           "flac-definitions.rkt"
           "../utils/utils.rkt")

  (provide flac-open
           flac-read
           flac-read-meta
           flac-stream-state
           (all-from-out "flac-definitions.rkt")
           test-file test-file2 test-file3
           kinds
           last-buffer last-buf-len
           )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to do the good stuff  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (flac-open flac-file cb-stream-info cb-audio)
    (if (file-exists? flac-file)
        (let ((handler (flac-ffi-decoder-handler)))
          (handler 'new)
          (handler 'init flac-file)
          (let ((h (make-flac-handle handler)))
            (set-flac-handle-cb-stream-info! h cb-stream-info)
            (set-flac-handle-cb-audio! h cb-audio)
            h))
        #f))

  (define (flac-stream-state handle)
    ((flac-handle-ffi-decoder-handler handle) 'state))


  (define kinds (make-hash))
  (define last-buffer #f)
  (define last-buf-len #f)
  (define (process-frame handle frame buffer)
    (let* ((h (flac-ffi-frame-header frame))
           (cb-audio (flac-handle-cb-audio handle))
           (ffi (flac-handle-ffi-decoder-handler handle))
           (type (hash-ref h 'number-type))
           (channels (hash-ref h 'channels))
           (block-size (hash-ref h 'blocksize)))
      (let ((buffers (ffi 'get-buffers buffer channels block-size)))
        (set! last-buffer buffers)
        (set! last-buf-len (hash-ref h 'blocksize))
        (hash-set! kinds type #t)
        (when (procedure? cb-audio)
          (cb-audio h buffers))
        ))
    ;(displayln "Processing frame"))
    #t
    )
  
  (define (process-meta handle meta)
    (let ((type (FLAC__StreamMetadata-type meta)))
      (display (format "  Got metadata type: ~a\n" type))
      (cond
        ([eq? type 'streaminfo]
         (let ((mh (flac-ffi-meta meta)))
           (let ((si (make-flac-stream-info
                      (hash-ref mh 'min-blocksize) (hash-ref mh 'max-blocksize)
                      (hash-ref mh 'min-framesize) (hash-ref mh 'max-framesize)
                      (hash-ref mh 'sample-rate)
                      (hash-ref mh 'channels)
                      (hash-ref mh 'bits-per-sample)
                      (hash-ref mh 'total-samples))))
             (set-flac-handle-stream-info! handle si)
             (let ((cb (flac-handle-cb-stream-info handle)))
               (when (procedure? cb)
                 (cb si))))))
        )
      ))

  (define (flac-read handle)
    (let* ((ffi-handler (flac-handle-ffi-decoder-handler handle))
           (state (ffi-handler 'state)))
      (letrec ((reader (lambda (frame-nr)
                         (let* ((st (ffi-handler 'state)))
                           (ffi-handler 'process-single)
                           (unless (eq? state st)
                             (set! state st)
                             (displayln
                              (format "Now in state ~a (frame-nr = ~a) (int-state = ~a)"
                                      st frame-nr (ffi-handler 'int-state)))
                             )
                           (when (ffi-handler 'has-errno?)
                             (displayln
                              (format "Error in stream: ~a" (ffi-handler 'errno)))
                             )
                           (when (ffi-handler 'has-meta-data?)
                             (ffi-handler 'process-meta-data
                                          (lambda (meta) (process-meta handle meta)))
                             )
                           (when (ffi-handler 'has-write-data?)
                             (ffi-handler 'process-write-data
                                          (lambda (frame buffer)
                                            (process-frame handle frame buffer)))
                             )
                           (if (eq? st 'end-of-stream)
                             'end-of-stream
                             (reader (+ frame-nr 1)))))
                       ))
        (reader 0))))

  (define (flac-read-meta handle)
    (let* ((ffi-handler (flac-handle-ffi-decoder-handler handle))
           (state (ffi-handler 'state)))
      (while (not (or (eq? state 'read-metadata)
                      (eq? state 'end-of-stream)
                      (eq? state 'aborted)
                      (eq? state 'memory-allocation-error)
                      (eq? state 'uninitialized)))
             (ffi-handler 'process-single)
             (set! state (ffi-handler 'state))
             state)
      (if (eq? state 'read-metadata)
          (begin
            (ffi-handler 'process-meta-data
                         (lambda (meta) (process-meta handle meta)))
            (flac-handle-stream-info handle))
          #f)))
                         
  (define test-file "C:/devel/racket/racket-sound/libflac/capr24.flac")
  (define test-file2 "C:/Muziek/Klassiek-Kamermuziek/Beethoven/Rachel Podger/01 Violin Sonata No. 1 in D Major, Op. 12 No. 1- I. Allegro con brio.flac")
  (define test-file3 "c:/Muziek/Pop/Radiohead/The Best of Radiohead (2008)/02. Paranoid Android.flac")

  ); end of module
