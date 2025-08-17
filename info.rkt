#lang info

(define pkg-authors '(hnmdijkema))
(define version "0.1.0")
(define license 'GPL-2.0-or-later)   ; The liboa library has this license
(define collection "racket-sound")
(define pkg-desc "racket-sound - Integration of popular music/sound related libraries in racket")

(define scribblings
  '(
    ("scribblings/racket-sound.scrbl" () (library) "racket-sound")
    ("scribblings/liboa.scrbl" () (library) "racket-sound/liboa/libao.rkt")
    ("scribblings/flac-decoder.scrbl" () (library) "racket-sound/libflac/flac-decoder.rkt")
    ("scribblings/taglib.scrbl" () (library) "racket-sound/libtag/taglib.rkt")
    )
  )

(define deps
  '("racket/gui" "racket/base" "racket"))

(define build-deps
  '("racket-doc"
    "draw-doc"
    "rackunit-lib"
    "scribble-lib"
    ))
