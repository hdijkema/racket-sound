#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         setup/dirs
         )

(provide ;_libao_pointer
         AO-FMT-LITTLE AO-FMT-BIG AO-FMT-NATIVE
         ao_initialize
         ao_default_driver_id
         ao_driver_id
         ao_open_live
         ao_play
         ao_close
         ao_shutdown
         ao_append_option
         make-ao_sample_format
         ao_sample_format-bits
         ao_sample_format-rate
         ao_sample_format-channels
         ao_sample_format-byte_format
         ao_sample_format-matrix
         (all-from-out ffi/unsafe)
         )


(define-ffi-definer define-libao
  (ffi-lib "libao" '("3" "4" "5" #f)
           #:get-lib-dirs (lambda ()
                            (cons (build-path ".") (get-lib-search-dirs)))
           #:fail (lambda ()
                    (ffi-lib (get-lib-path "libao-4.dll")))
           ))


(define _libao-pointer (_cpointer 'ao_device))
(define-cstruct _ao_sample_format (
                                   [bits _int]        ; bits per sample
                                   [rate _int]        ; samples per second in a single channel
                                   [channels _int]    ; number of audio channels
                                   [byte_format _int] ; byte ordering in sample, see "constants" below
                                   [matrix _pointer]  ; channel input matrix
                                   ))

(define-cstruct _ao_option (
                            [key _pointer]
                            [value _pointer]
                            [next _pointer]   ; number of audio channels (list)
                            ))

(define AO-FMT-LITTLE  1)
(define AO-FMT-BIG     2)
(define AO-FMT-NATIVE  4)

; void ao_initialize();
(define-libao ao_initialize (_fun -> _void))

; int ao_default_driver_id();
(define-libao ao_default_driver_id (_fun -> _int))

; ao_device* ao_open_live(int driver_id, ao_sample_format *format, ao_option *options);
(define-libao ao_open_live (_fun _int _pointer _pointer -> _libao-pointer)) 

; int ao_play(ao_device *device, char *output_samples, uint_32 num_bytes);
(define-libao ao_play (_fun _libao-pointer _pointer _uint32 -> _int))

; int ao_close(ao_device *device);
(define-libao ao_close (_fun _libao-pointer -> _int))

; void ao_shutdown();
(define-libao ao_shutdown (_fun -> _int))

; int ao_append_option(ao_option **options, const char *key, const char *value);
(define-libao ao_append_option (_fun _pointer _pointer _pointer -> _int))

; int ao_driver_id(char *short_name);
(define-libao ao_driver_id (_fun _pointer -> _int))



