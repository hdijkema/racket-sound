(module utils racket/base

  (provide while
           get-lib-path
           )

  (define-syntax while
    (syntax-rules ()
      ((_ cond body ...)
       (letrec ((while-f (lambda (last-result)
                           (if cond
                               (let ((last-result (begin
                                                    body
                                                    ...)))
                                 (while-f last-result))
                               last-result))))
         (while-f #f))
       )
      ))


  (define (get-lib-path lib)
    (let ((platform (system-type)))
      (cond
        [(eq? platform 'windows) 
         (build-path (current-directory) ".." "lib" "dll" lib)]
        [else
         (error (format "Install the shared library: ~a" lib))]
        )))
                              

  ) ; end of module
