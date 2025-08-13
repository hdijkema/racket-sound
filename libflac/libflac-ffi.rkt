;#lang racket/base
(module libflac-ffi racket/base
  
(require ffi/unsafe
         ffi/unsafe/define
         setup/dirs
         "../utils/utils.rkt"
         )

(provide flac-ffi-decoder-handler
         _FLAC__StreamMetadata
         FLAC__StreamMetadata-type
         flac-ffi-meta
         flac-ffi-frame-header
         FLAC__uint32-pointer
         FLAC__int32**
         )

(define-ffi-definer define-libflac
  (ffi-lib "libFLAC" '(#f)
           #:get-lib-dirs (lambda ()
                            (cons (build-path ".") (get-lib-search-dirs)))
           #:fail (lambda ()
                    (ffi-lib (get-lib-path "libFLAC.dll")))
           ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some FLAC Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define FLAC__MAX_CHANNELS 8)
(define FLAC__MAX_FIXED_ORDER 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLAC Integer types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define _uint32_t _uint32)
(define _uint64_t _uint64)
(define FLAC__uint8 _uint8)
(define FLAC__uint16 _uint16)
(define FLAC__int64 _int64)
(define FLAC__uint64 _uint64)
(define FLAC__uint32 _uint32)
(define FLAC__int32-pointer (_ptr i _int32))
(define FLAC__int32** (_ptr i (_ptr i _int32)))
(define FLAC__uint32-pointer (_ptr i _uint32))
(define FLAC__int64-pointer (_ptr i _int64))
(define FLAC__uint64-pointer (_ptr i _uint64))
(define FLAC__bool _int)
(define FLAC__byte _uint8)
(define FLAC__byte-pointer (_ptr i FLAC__byte))
(define _uint32_1bit_t _uint32)
(define _char _int8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLAC enumerations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define _FLAC__ChannelAssignment
  (_enum '(independent = 0
           left-side = 1
           right-side = 2
           mid-side = 3
           )))

(define _FLAC__FrameNumberType
  (_enum '(frame-number
           sample-number
          )))

(define _FLAC__SubframeType
  (_enum '(constant = 0
           verbatim = 1
           fixed = 2
           lpc = 3
           )))

(define _FLAC__MetadataType
  (_enum '(streaminfo = 0
           padding = 1
           application = 2
           seektable = 3
           vorbis-comment = 4
           cuesheet = 5
           picture = 6
           undefined = 7
           max--metadata-type-code = 126
           )))

(define _FLAC_StreamMetadata_Picture_Type
  (_enum '(other = 0
           file-icon-standard = 1
           file-icon = 2
           front-cover = 3
           back-cover = 4
           leaflet-page = 5
           media = 6
           lead-artist = 7
           artist = 8
           conductor = 9
           band = 10
           composer = 11
           lyricist = 12
           recording-location = 13
           during-recording = 14
           during-performance = 15
           video-screen-capture = 16
           fish = 17
           illustration = 18
           band-logotype = 19
           publisher-logotype = 20
           undefined
           )))


;typedef enum {
;    FLAC__STREAM_DECODER_SEARCH_FOR_METADATA = 0,
;    FLAC__STREAM_DECODER_READ_METADATA,
;    FLAC__STREAM_DECODER_SEARCH_FOR_FRAME_SYNC,
;    FLAC__STREAM_DECODER_READ_FRAME,
;    FLAC__STREAM_DECODER_END_OF_STREAM,
;    FLAC__STREAM_DECODER_OGG_ERROR,
;    FLAC__STREAM_DECODER_SEEK_ERROR,
;    FLAC__STREAM_DECODER_ABORTED,
;    FLAC__STREAM_DECODER_MEMORY_ALLOCATION_ERROR,
;    FLAC__STREAM_DECODER_UNINITIALIZED,
;    FLAC__STREAM_DECODER_END_OF_LINK
;} FLAC__StreamDecoderState;

(define _FLAC_StreamDecoderState
  (_enum '(search-for-metadata = 0
           read-metadata
           search-for-frame-sync
           read-frames
           end-of-stream
           ogg-error
           seek-error
           aborted
           memory-allocation-error
           uninitialized
           end-of-link
           )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLAC Frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-cstruct _FLAC__FrameHeader (
                                   [blocksize _uint32_t]
                                   [sample_rate _uint32_t]
                                   [channels _uint32_t]
                                   [channel_assignment _FLAC__ChannelAssignment]
                                   [bits_per_sample _uint32_t]
                                   [number_type _FLAC__FrameNumberType]
                                   [number (_union _uint32_t _uint64_t)]
                                   [crc FLAC__uint8]
                                   ))

(define (flac-ffi-frame-header frame)
  (let* ((hdr (FLAC__Frame-header frame))
         (h (make-hash)))
    (for-each
     (lambda (e)
       (hash-set! h (car e) (cdr e)))
     (list
      (cons 'blocksize (FLAC__FrameHeader-blocksize hdr))
      (cons 'sample-rate (FLAC__FrameHeader-sample_rate hdr))
      (cons 'channels (FLAC__FrameHeader-channels hdr))
      (cons 'channel-assignment (FLAC__FrameHeader-channel_assignment hdr))
      (cons 'bits-per-sample (FLAC__FrameHeader-bits_per_sample hdr))
      (cons 'number-type (FLAC__FrameHeader-number_type hdr))
      (cons 'number (if (eq? (FLAC__FrameHeader-number_type hdr) 'frame-number)
                        (union-ref (FLAC__FrameHeader-number hdr) 0)
                        (union-ref (FLAC__FrameHeader-number hdr) 1)))
      (cons 'crc (FLAC__FrameHeader-crc hdr)))
     )
    h))

(define-cstruct _FLAC__FrameFooter (
                                   [crc FLAC__uint16]
                                   ))

(define-cstruct _FLAC__Subframe_Constant (
                                          [value FLAC__int64]
                                          ))

(define _FLAC__VerbatimSubframeDataType
  (_enum '(int32
           int64)))

(define-cstruct _FLAC__Subframe_Verbatim (
                                          [data (_union
                                                 FLAC__int32-pointer
                                                 FLAC__int64-pointer
                                                 )]
                                          [data_type _FLAC__VerbatimSubframeDataType]
                                          ))


(define FLAC__EntropyCodingMethodType
  (_enum '(partitioned-rice = 0
           partitioned-rice2 = 1)))

(define-cstruct _FLAC__EntropyCodingMethod_PartitionedRiceContents
  (
   [parameters FLAC__uint32-pointer]
   [raw_bits   FLAC__uint32-pointer]
   [capacity_by_order FLAC__uint32-pointer]
   ))

(define-cstruct _FLAC__EntropyCodingMethod_PartitionedRice
  (
   [order _uint32_t]
   [contents _FLAC__EntropyCodingMethod_PartitionedRiceContents-pointer]
   ))

(define-cstruct _FLAC__EntropyCodingMethod
  (
   [type FLAC__EntropyCodingMethodType]
   [data (_union _FLAC__EntropyCodingMethod_PartitionedRice)]
   ))

(define-cstruct _FLAC__Subframe_Fixed (
                                       [entropy_coding_method _FLAC__EntropyCodingMethod]
                                       [order _uint32_t]
                                       [warmup (_array FLAC__int64 FLAC__MAX_FIXED_ORDER)]
                                       [residual FLAC__int32-pointer]
                                       ))

(define-cstruct _FLAC__Subframe_LPC (
                                     [jaja _int]
                                     ))

(define-cstruct _FLAC__Subframe (
                                [type _FLAC__SubframeType]
                                [data (_union
                                       _FLAC__Subframe_Constant
                                       _FLAC__Subframe_Fixed
                                       _FLAC__Subframe_LPC
                                       _FLAC__Subframe_Verbatim
                                       )]
                                [wated_bits _uint32_t]
                                ))

(define-cstruct _FLAC__Frame (
                            [header _FLAC__FrameHeader]
                            [subframes (_array _FLAC__Subframe FLAC__MAX_CHANNELS)]
                            [footer _FLAC__FrameFooter]
                            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLAC Metadata 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;typedef struct FLAC__StreamMetadata {
;    FLAC__MetadataType type;
;    FLAC__bool is_last;
;    uint32_t length;
;    union {
;        FLAC__StreamMetadata_StreamInfo stream_info;
;        FLAC__StreamMetadata_Padding padding;
;        FLAC__StreamMetadata_Application application;
;        FLAC__StreamMetadata_SeekTable seek_table;
;        FLAC__StreamMetadata_VorbisComment vorbis_comment;
;        FLAC__StreamMetadata_CueSheet cue_sheet;
;        FLAC__StreamMetadata_Picture picture;
;        FLAC__StreamMetadata_Unknown unknown;
;    } data;
;} FLAC__StreamMetadata;


(define-cstruct _FLAC__StreamMetadata_StreamInfo
  (
   [min_blocksize _uint32_t]
   [max_blocksize _uint32_t]
   [min_framesize _uint32_t]
   [max_framesize _uint32_t]
   [sample_rate _uint32_t]
   [channels _uint32_t]
   [bits_per_sample _uint32_t]
   [total_samples FLAC__uint64]
   [md5sum (_array FLAC__byte 16)]
   ))

(define (meta-stream-info si . hash)
  (let ((h (if (null? hash) (make-hash) (car hash))))
    (hash-set! h 'min-blocksize (FLAC__StreamMetadata_StreamInfo-min_blocksize si))
    (hash-set! h 'max-blocksize (FLAC__StreamMetadata_StreamInfo-max_blocksize si))
    (hash-set! h 'min-framesize (FLAC__StreamMetadata_StreamInfo-min_framesize si))
    (hash-set! h 'max-framesize (FLAC__StreamMetadata_StreamInfo-max_framesize si))
    (hash-set! h 'sample-rate (FLAC__StreamMetadata_StreamInfo-sample_rate si))
    (hash-set! h 'channels (FLAC__StreamMetadata_StreamInfo-channels si))
    (hash-set! h 'bits-per-sample (FLAC__StreamMetadata_StreamInfo-bits_per_sample si))
    (hash-set! h 'total-samples (FLAC__StreamMetadata_StreamInfo-total_samples si))
    h))

(define-cstruct _FLAC__StreamMetadata_Padding
  (
   [dummy _int]
   ))

(define-cstruct _FLAC__StreamMetadata_Application
  (
   [id (_array FLAC__byte 4)]
   [data FLAC__byte-pointer]
   ))

(define-cstruct _FLAC__StreamMetadata_SeekPoint
  (
   [sample_number FLAC__uint64]
   [stream_offset FLAC__uint64]
   [frame_samples _uint32_t]
   ))

(define-cstruct _FLAC__StreamMetadata_SeekTable
  (
   [num_points _uint32_t]
   [points _FLAC__StreamMetadata_SeekPoint-pointer]
   ))

(define-cstruct _FLAC__StreamMetadata_VorbisComment_Entry
  (
   [length FLAC__uint32]
   [entry FLAC__byte-pointer]
   ))

(define-cstruct _FLAC__StreamMetadata_VorbisComment
  (
   [vendor_string _FLAC__StreamMetadata_VorbisComment_Entry]
   [num_comments FLAC__uint32]
   [comments _FLAC__StreamMetadata_VorbisComment_Entry-pointer]
   ))

(define-cstruct _FLAC__StreamMetadata_CueSheet_Index
  (
   [offset FLAC__uint64]
   [number FLAC__byte]
   ))

(define-cstruct _FLAC__StreamMetadata_CueSheet_Track
  (
   [offset FLAC__uint64]
   [number FLAC__byte]
   [isrc (_array _char 13)]
   [type _uint32_1bit_t]
   [pre_emphasis _uint32_1bit_t]
   [num_indices FLAC__byte]
   [indices _FLAC__StreamMetadata_CueSheet_Index-pointer]
   ))

(define-cstruct _FLAC__StreamMetadata_CueSheet
  (
   [media_catalog_number (_array _char 129)]
   [lead_in FLAC__uint64]
   [is_cd FLAC__bool]
   [num_tracks _uint32_t]
   [tracks _FLAC__StreamMetadata_CueSheet_Track-pointer]
   ))

(define-cstruct _FLAC__StreamMetadata_Picture
  (
   [type _FLAC_StreamMetadata_Picture_Type]
   [mime_type _string/utf-8]
   [description FLAC__byte-pointer]
   [width FLAC__uint32]
   [height FLAC__uint32]
   [depth FLAC__uint32]
   [colors FLAC__uint32]
   [data_length FLAC__uint32]
   [date FLAC__byte-pointer]
   ))

(define-cstruct _FLAC__StreamMetadata_Unknown
  (
   [data FLAC__byte-pointer]
   ))
 

(define-cstruct _FLAC__StreamMetadata
  (
   [type _FLAC__MetadataType]
   [is_last FLAC__bool]
   [length _uint32_t]
   [data (_union
          _FLAC__StreamMetadata_StreamInfo
          _FLAC__StreamMetadata_Padding
          _FLAC__StreamMetadata_Application
          _FLAC__StreamMetadata_SeekTable
          _FLAC__StreamMetadata_VorbisComment
          _FLAC__StreamMetadata_CueSheet
          _FLAC__StreamMetadata_Picture
          _FLAC__StreamMetadata_Unknown
          )]
   ))

 (define (flac-ffi-meta meta)
   (let ((type (FLAC__StreamMetadata-type meta))
         (h (make-hash)))
     (cond
       ([eq? type 'streaminfo]
        (meta-stream-info (union-ref (FLAC__StreamMetadata-data meta) 0) h))
       (else (error (format "Cannot process metadata: ~a" type)))
       )
     h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLAC Generic Pointer Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define _FLAC__StreamDecoder-pointer (_cpointer 'flac-streamdecoder))
(define _FLAC__Data-pointer (_cpointer 'flac-client-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLAC Callback function definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
;typedef FLAC__StreamDecoderWriteStatus(* FLAC__StreamDecoderWriteCallback) (const FLAC__StreamDecoder *decoder, const FLAC__Frame *frame, const FLAC__int32 *const buffer[], void *client_data)
(define _FLAC__StreamDecoderWriteCallback
  (_fun _FLAC__StreamDecoder-pointer
        _FLAC__Frame-pointer
        FLAC__int32** 
        _FLAC__Data-pointer -> _int))

;typedef void(* FLAC__StreamDecoderMetadataCallback) (const FLAC__StreamDecoder *decoder, const FLAC__StreamMetadata *metadata, void *client_data)
(define _FLAC__StreamDecoderMetadataCallback
  (_fun _FLAC__StreamDecoder-pointer
        _FLAC__StreamMetadata-pointer
        _FLAC__Data-pointer -> _void))

(define _FLAC__StreamDecoderErrorCallback
  (_fun _FLAC__StreamDecoder-pointer
        _int
        _FLAC__Data-pointer -> _void))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exported FLAC functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-libflac FLAC__stream_decoder_new
  (_fun -> _FLAC__StreamDecoder-pointer))

(define-libflac FLAC__stream_decoder_delete
  (_fun _FLAC__StreamDecoder-pointer -> _void))

(define-libflac FLAC__stream_decoder_get_state
  (_fun _FLAC__StreamDecoder-pointer -> _int))

(define states (make-hash))
  (hash-set! states 0 'search-for-metadata)
  (hash-set! states 1 'read-metadata)
  (hash-set! states 2 'search-for-frame-sync)
  (hash-set! states 3 'read-frames)
  (hash-set! states 4 'end-of-stream)
  (hash-set! states 5 'ogg-error)
  (hash-set! states 6 'seek-error)
  (hash-set! states 7 'aborted)
  (hash-set! states 8 'memory-allocation-error)
  (hash-set! states 9 'uninitialized)
  (hash-set! states 10 'end-of-link)

(define (decoder-state int-st)
  (hash-ref states int-st #f))
  

(define-libflac FLAC__stream_decoder_init_file
  (_fun _FLAC__StreamDecoder-pointer
        _string/utf-8
        _FLAC__StreamDecoderWriteCallback
        _FLAC__StreamDecoderMetadataCallback
        _FLAC__StreamDecoderErrorCallback
        -> _int))

(define-libflac FLAC__stream_decoder_process_single
  (_fun _FLAC__StreamDecoder-pointer
        -> _bool))

(define-libflac FLAC__stream_decoder_process_until_end_of_metadata
  (_fun _FLAC__StreamDecoder-pointer
        -> _bool))

(define-libflac FLAC__stream_decoder_seek_absolute
  (_fun _FLAC__StreamDecoder-pointer FLAC__uint64
          -> _bool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Our interface for decoding to racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (flac-ffi-decoder-handler)
  (define write-data '())
  (define meta-data '())
  (define error-no -1)
  (define fl #f)
  (define flac-file #f)
  
  (define (write-callback fl frame buffer data)
    (set! write-data (append write-data (list (cons frame buffer))))
    0)

  (define (meta-callback fl meta data)
    (set! meta-data (append meta-data (list meta))))

  (define (error-callback fl errno data)
    (set! error-no errno)
    )

  (define (new)
    (set! fl (FLAC__stream_decoder_new))
    fl)

  (define (init file)
    (let ((r (FLAC__stream_decoder_init_file
             fl
             file
             write-callback
             meta-callback
             error-callback)))
      (set! flac-file file)
      r))

  (define (process-single)
    (FLAC__stream_decoder_process_single fl))

  (define (int-state)
     (FLAC__stream_decoder_get_state fl))
  
  (define (state)
    (decoder-state (int-state)))

  (define (process-meta-data cb)
    (for-each cb meta-data)
    (set! meta-data '()))

  (define (process-write-data cb)
    (for-each (lambda (d)
                (cb (car d) (cdr d)))
              write-data)
    (set! write-data '()))

  (define (buffer->vectorlist buffer channels size)
    (letrec ((for-channels
              (lambda (channel)
                (if (< channel channels)
                  (letrec ((v (make-vector size 0))
                           (p (ptr-ref buffer FLAC__int32-pointer channel))
                           (to-vec (lambda (i)
                                     (when (< i size)
                                       (vector-set! v i (ptr-ref p _int32 i))
                                       (to-vec (+ i 1)))))
                           )
                    (to-vec 0)
                    (cons v (for-channels (+ channel 1))))
                  '())))
             )
      (for-channels 0)))

  (define (seek-to-sample sample)
    (FLAC__stream_decoder_seek_absolute fl sample))

  (lambda (cmd . args)
    (cond
      [(eq? cmd 'write-data) write-data]
      [(eq? cmd 'meta-data) meta-data]
      
      [(eq? cmd 'new) (new)]
      [(eq? cmd 'init) (init (car args))]
      [(eq? cmd 'process-single) (process-single)]
      [(eq? cmd 'get-buffers) (buffer->vectorlist (car args) (cadr args) (caddr args))]
      
      [(eq? cmd 'int-state) (int-state)]
      [(eq? cmd 'state) (state)]
      
      [(eq? cmd 'has-write-data?) (not (null? write-data))]
      [(eq? cmd 'has-meta-data?) (not (null? meta-data))]
      [(eq? cmd 'has-errno?) (not (= error-no -1))]
      
      [(eq? cmd 'process-meta-data) (process-meta-data (car args))]
      [(eq? cmd 'process-write-data) (process-write-data (car args))]
      [(eq? cmd 'errno) error-no]
      
      [(eq? cmd 'seek-to-sample) (seek-to-sample (car args))]
      [(eq? cmd 'file) flac-file]
      
      [else (error (format "unknown command ~a" cmd))]
      ))
  )

); end of module