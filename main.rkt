#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         )

(define-ffi-definer define-libao (ffi-lib "libao"))


(define _libao-pointer (_cpointer 'ao_device))
(define _

