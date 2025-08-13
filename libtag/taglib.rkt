(module taglib racket/base

  (require "taglib-ffi.rkt"
           "../utils/utils.rkt"
           racket/draw)

  (provide id3-tags

           tags-valid?

           tags-title
           tags-album
           tags-artist
           tags-comment
           tags-year
           tags-genre
           tags-track
           
           tags-length
           tags-sample-rate
           tags-bit-rate
           tags-channels

           tags-keys
           tags-ref

           tags-picture
           tags-picture->bitmap

           tags->hash

           id3-picture-mimetype
           id3-picture-kind
           id3-picture-size
           id3-picture-bytes
           )

  (define-struct id3-tag-struct
    (handle))

  (define-struct id3-picture
    (mimetype kind size bytes))

  (define (id3-tags file)
    (let ((valid? #f)
          (title "")
          (album "")
          (artist "")
          (comment "")
          (year -1)
          (genre "")
          (track -1)
          (length -1)
          (sample-rate -1)
          (bit-rate -1)
          (channels -1)
          (key-store (make-hash))
          (composer "")
          (album-artist "")
          (disc-number -1)
          (picture #f))
      (let ((tag-file (taglib_file_new file)))
        (set! valid? (taglib_file_is_valid tag-file))
        (when valid?
          (let ((tag (taglib_file_tag tag-file))
                (ap (taglib_file_audioproperties tag-file))
                (cp (lambda (s) (string-append s "")))
                )
            (set! title (cp (taglib_tag_title tag)))
            (set! album (cp (taglib_tag_album tag)))
            (set! artist (cp (taglib_tag_artist tag)))
            (set! comment (cp (taglib_tag_comment tag)))
            (set! genre (cp (taglib_tag_genre tag)))
            (set! year (taglib_tag_year tag))
            (set! track (taglib_tag_track tag))
            
            (set! length (taglib_audioproperties_length ap))
            (set! sample-rate (taglib_audioproperties_samplerate ap))
            (set! bit-rate (taglib_audioproperties_bitrate ap))
            (set! channels (taglib_audioproperties_channels ap))

            (let* ((keys (taglib_property_keys tag-file))
                   (i 0)
                   (key (taglib_property_key keys i))
                   (key-list '())
                   )
              (while (not (eq? key #f))
                     (set! key-list (append key-list (list (cp key))))
                     (set! i (+ i 1))
                     (set! key (taglib_property_key keys i)))
              (for-each (lambda (key)
                          (let ((props (taglib_property_get tag-file key)))
                            (let* ((vals '())
                                   (i 0)
                                   (val (taglib_property_val props i)))
                              (while (not (eq? val #f))
                                     (set! vals (append vals (list (cp val))))
                                     (set! i (+ i 1))
                                     (set! val (taglib_property_val props i)))
                              (taglib_property_free props)
                              (hash-set! key-store
                                         (string->symbol
                                          (string-downcase key)) vals)
                              )))
                        key-list)
              (set! composer (hash-ref key-store 'composer ""))
              (set! album-artist (hash-ref key-store 'albumartist ""))
              (set! disc-number (string->number
                                 (car 
                                  (hash-ref key-store 'discnumber (list "-1")))))
              )

            ; picture
            (let ((p (taglib-get-picture tag-file)))
              (if (eq? p #f)
                  (set! picture #f)
                  (let ((mimetype (car p))
                        (kind (caddr p))
                        (size (cadddr p))
                        (bytes (car (cddddr p))))
                    (set! picture (make-id3-picture mimetype kind size bytes))
                    )))

            ; cleaning up
            (taglib_tag_free_strings)
            (taglib_file_free tag-file)
            )
          )
        (let ((handle 
          (lambda (v . args)
            (cond
              [(eq? v 'valid?) valid?]
              [(eq? v 'title) title]
              [(eq? v 'album) album]
              [(eq? v 'artist) artist]
              [(eq? v 'comment) comment]
              [(eq? v 'composer) composer]
              [(eq? v 'genre) genre]
              [(eq? v 'year) year]
              [(eq? v 'track) track]
              [(eq? v 'length) length]
              [(eq? v 'sample-rate) sample-rate]
              [(eq? v 'bit-rate) bit-rate]
              [(eq? v 'channels) channels]
              [(eq? v 'keys) (hash-keys key-store)]
              [(eq? v 'val)
               (if (null? args)
                   #f
                   (hash-ref key-store (car args) #f))]
              [(eq? v 'picture) picture]
              [(eq? v 'to-hash)
               (let ((h (make-hash)))
                 (hash-set! h 'valid? valid?)
                 (hash-set! h 'title title)
                 (hash-set! h 'album album)
                 (hash-set! h 'artist artist)
                 (hash-set! h 'comment comment)
                 (hash-set! h 'composer composer)
                 (hash-set! h 'genre genre)
                 (hash-set! h 'year year)
                 (hash-set! h 'track track)
                 (hash-set! h 'length length)
                 (hash-set! h 'sample-rate sample-rate)
                 (hash-set! h 'bit-rate bit-rate)
                 (hash-set! h 'channels channels)
                 (hash-set! h 'picture picture)
                 (hash-set! h 'keys (hash-keys key-store))
                 h)]
              [else (error (format "Unknown tag-cmd '~a'" v))]
              ))))
          (make-id3-tag-struct handle))
          )))


  (define-syntax def
    (syntax-rules ()
      ((_ (fun v))
       (define (fun tags . args)
         (apply (id3-tag-struct-handle tags) (cons v args)))
       )))

  (define-syntax defs
    (syntax-rules ()
      ((_ f1)
       (def f1))
      ((_ f1 f2 ...)
       (begin
         (def f1)
         (def f2)
         ...))
      ))

  (defs
    (tags-valid? 'valid?)
    (tags-title 'title)
    (tags-album 'album)
    (tags-artist 'artist)
    (tags-comment 'comment)
    (tags-genre 'genre)
    (tags-composer 'composer)
    (tags-year 'year)
    (tags-track 'track)
    
    (tags-length 'length)
    (tags-sample-rate 'sample-rate)
    (tags-bit-rate 'bit-rate)
    (tags-channels 'channels)

    (tags-keys 'keys)
    (tags-ref 'val)

    (tags-picture 'picture)
    (tags->hash 'to-hash)
    )

  (define (tags-picture->bitmap tags)
    (let ((p (tags-picture tags)))
      (if (eq? p #f)
          #f
          (let* ((in (open-input-bytes (id3-picture-bytes p)))
                 (btm (read-bitmap in)))
            (close-input-port in)
            btm))))
            
            

  ); end of module

