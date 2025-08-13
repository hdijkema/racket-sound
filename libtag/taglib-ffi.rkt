#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         setup/dirs
         "../utils/utils.rkt"
         )

(provide TagLib_File_Type
         _TagLib_File-pointer
         _TagLib_Tag-pointer
         _TagLib_AudioProperties-pointer

         taglib_file_new
         taglib_file_new_type
         taglib_file_is_valid
         taglib_file_free

         taglib_file_tag
         taglib_file_audioproperties
         taglib_tag_free_strings
         
         taglib_tag_title
         taglib_tag_artist
         taglib_tag_album
         taglib_tag_comment
         taglib_tag_genre
         taglib_tag_year
         taglib_tag_track

         taglib_audioproperties_length
         taglib_audioproperties_bitrate
         taglib_audioproperties_samplerate
         taglib_audioproperties_channels

         taglib_property_keys
         taglib_property_key
         
         taglib_property_get
         taglib_property_val
         
         taglib_property_free

         taglib-get-picture
         )


(define-ffi-definer define-tag-lib
  (ffi-lib "tag" '("0" #f)
           #:get-lib-dirs (lambda ()
                            (cons (build-path ".") (get-lib-search-dirs)))
           #:fail (lambda ()
                    (ffi-lib (get-lib-path "tag.dll")))
           ))

(define-ffi-definer define-tag-c-lib
  (ffi-lib "tag_c" '("0" "1" "2" #f)
           #:get-lib-dirs (lambda ()
                            (cons (build-path ".") (get-lib-search-dirs)))
           #:fail (lambda ()
                    (ffi-lib (get-lib-path "tag_c.dll")))
           ))

(define TagLib_File_Type
  (_enum '(
           mpeg
           ogg-vorbis
           flac
           mpc
           ogg-flac
           wavpack
           speex
           true-audio
           mp4
           asf
           aiff
           wav
           ape
           it
           mod
           s3m
           xm
           opus
           dsf
           dsdiff
           shorten
           )))

(define _TagLib_File-pointer (_cpointer/null 'taglib-file))
(define _TagLib_Tag-pointer (_cpointer/null 'taglib-tag))
(define _TagLib_AudioProperties-pointer (_cpointer/null 'taglib-audioproperties))

; TagLib_File *taglib_file_new(const char *filename);
(define-tag-c-lib taglib_file_new
  (_fun _string/utf-8 -> _TagLib_File-pointer ))

; TagLib_File *taglib_file_new_type(const char *filename, TagLib_File_Type type);
(define-tag-c-lib taglib_file_new_type
  (_fun _string/utf-8 TagLib_File_Type -> _TagLib_File-pointer))

; void taglib_file_free(TagLib_File *file);
(define-tag-c-lib taglib_file_free
  (_fun _TagLib_File-pointer -> _void))

; BOOL taglib_file_is_valid(const TagLib_File *file);
(define-tag-c-lib taglib_file_is_valid
  (_fun _TagLib_File-pointer -> _bool))

; TagLib_Tag *taglib_file_tag(const TagLib_File *file);
(define-tag-c-lib taglib_file_tag
  (_fun _TagLib_File-pointer -> _TagLib_Tag-pointer))

; const TagLib_AudioProperties *taglib_file_audioproperties(const TagLib_File *file);
(define-tag-c-lib taglib_file_audioproperties
  (_fun _TagLib_File-pointer -> _TagLib_AudioProperties-pointer))

; void taglib_tag_free_strings(void);
(define-tag-c-lib taglib_tag_free_strings
  (_fun -> _void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax tg
  (syntax-rules ()
    ((_ name)
     (define-tag-c-lib name
       (_fun _TagLib_Tag-pointer -> _string/utf-8)))
    ((_ name ret-type)
     (define-tag-c-lib name
       (_fun _TagLib_Tag-pointer -> ret-type)))
    ))


; char *taglib_tag_title(const TagLib_Tag *tag);
; etc..
(tg taglib_tag_title)
(tg taglib_tag_artist)
(tg taglib_tag_album)
(tg taglib_tag_comment)
(tg taglib_tag_genre)
(tg taglib_tag_year _uint)
(tg taglib_tag_track _uint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; audio properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax ap
  (syntax-rules ()
    ((_ name)
     (define-tag-c-lib name
       (_fun _TagLib_AudioProperties-pointer -> _int)))
    ))

; int taglib_audioproperties_length(const TagLib_AudioProperties *audioProperties);
; etc...

(ap taglib_audioproperties_length)
(ap taglib_audioproperties_bitrate)
(ap taglib_audioproperties_samplerate)
(ap taglib_audioproperties_channels)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keys in the propertymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; char** taglib_property_keys(const TagLib_File *file);
(define-tag-c-lib taglib_property_keys
  (_fun _TagLib_File-pointer -> (_ptr i _string/utf-8)))

(define (taglib_property_key keys i)
  (ptr-ref keys _string/utf-8 i))

;char** taglib_property_get(const TagLib_File *file, const char *prop);
(define-tag-c-lib taglib_property_get
  (_fun _TagLib_File-pointer _string/utf-8 -> (_ptr i _string/utf-8)))

(define (taglib_property_val prop i)
  (ptr-ref prop _string/utf-8 i))

; void taglib_property_free(char **props);
(define-tag-c-lib taglib_property_free
  (_fun _pointer -> _void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Picture data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;typedef struct {
;  char *mimeType;
;  char *description;
;  char *pictureType;
;  char *data;
;  unsigned int size;
;} TagLib_Complex_Property_Picture_Data;

(define-cstruct _TagLib_Complex_Property_Picture_Data
  (
   [mimeType _string/utf-8]
   [description _string/utf-8]
   [pictureType _string/utf-8]
   [data _pointer]
   [size _uint]
   ))



; TagLib_Complex_Property_Attribute*** properties = *   taglib_complex_property_get(file, "PICTURE");
; * TagLib_File *file = taglib_file_new("myfile.mp3");
; * TagLib_Complex_Property_Attribute*** properties =
; *   taglib_complex_property_get(file, "PICTURE");
; * TagLib_Complex_Property_Picture_Data picture;
; * taglib_picture_from_complex_property(properties, &picture);
; * // Do something with picture.mimeType, picture.description,
; * // picture.pictureType, picture.data, picture.size, e.g. extract it.
; * FILE *fh = fopen("mypicture.jpg", "wb");
; * if(fh) {
; *   fwrite(picture.data, picture.size, 1, fh);
; *   fclose(fh);
; * }
; * taglib_complex_property_free(properties);

(define _Complex_Property_Attribute-pointer (_cpointer/null 'taglib-complex-property-attribute))

(define-tag-c-lib taglib_complex_property_get
  (_fun _TagLib_File-pointer _string/utf-8 -> _Complex_Property_Attribute-pointer))

(define-tag-c-lib taglib_picture_from_complex_property
  (_fun _Complex_Property_Attribute-pointer
        _TagLib_Complex_Property_Picture_Data-pointer
        -> _void))

(define-tag-c-lib taglib_complex_property_free
  (_fun _Complex_Property_Attribute-pointer -> _void))

;TAGLIB_C_EXPORT char** taglib_complex_property_keys(const TagLib_File *file);
(define-tag-c-lib taglib_complex_property_keys
  (_fun _TagLib_File-pointer -> (_ptr i _string/utf-8)))

;  void taglib_complex_property_free_keys(char **keys);
(define-tag-c-lib taglib_complex_property_free_keys
  (_fun _pointer -> _void))

(define (taglib-get-picture tag-file)
  (define (cp s) (string-append s ""))
  (define (to-bytestring data size)
    
    (let* ((v (make-vector size 0))
           (i 0))
      (while (< i size)
             (vector-set! v (ptr-ref data _byte i) i)
             (set! i (+ i 1)))
      v))
  (let ((props (taglib_complex_property_get tag-file "PICTURE")))
    (if (eq? props #f)
        #f
        (let ((pd (make-TagLib_Complex_Property_Picture_Data #f #f #f #f 0)))
          (taglib_picture_from_complex_property props pd)
          (let* ((mimetype (cp (TagLib_Complex_Property_Picture_Data-mimeType pd)))
                 (description  (cp (TagLib_Complex_Property_Picture_Data-description pd)))
                 (type (cp (TagLib_Complex_Property_Picture_Data-pictureType pd)))
                 (size (TagLib_Complex_Property_Picture_Data-size pd))
                 (data  (cast (TagLib_Complex_Property_Picture_Data-data pd)
                               _pointer
                               (_bytes o size)))
                 )
            (let ((r (list mimetype description type size data)))
              (taglib_complex_property_free props)
              r))))
    ))