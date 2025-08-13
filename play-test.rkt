#lang racket/base
(require "libao/libao.rkt"
         "libflac/flac-decoder.rkt"
         )



(define fmt (ao-mk-format 16 44100 2 'big-endian))
(define ao-h (ao-open-live #f fmt))

(define (flac-play frame buffer)
  (ao-play ao-h buffer))

(define (flac-meta meta)
  (displayln meta))

(define flac-h (flac-open test-file3 flac-meta flac-play))

(flac-read flac-h)

