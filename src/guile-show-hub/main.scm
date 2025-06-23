
(define-module (guile-show-hub main)
  #:use-module (curl)
  #:use-module (ice-9 string-fun)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 local-eval)
  #:use-module (guix gexp)
  #:export (main))

(define guile-xyz-uri
  (make-parameter
   "https://codeberg.org/guix/guix/raw/branch/master/gnu/packages/guile-xyz.scm"))

(define (do-fetch uri)
  (let* ((handle (curl-easy-init))
         (_ (begin
              (curl-easy-setopt handle
                                'url uri)
              (curl-easy-setopt handle
                                'verbose #f)
              (curl-easy-setopt handle
                                'use-ssl 1)))
         (r (curl-easy-perform handle #t))
         (rr (catch #t
                    (lambda ()
                      (utf8->string r))
                    (lambda (key . args)
                      (display (format #f "error converting response: ~a: ~a"
                                       key args)) #f))))
    rr))

(define (read-all-sexps-from-string str)
  (call-with-input-string str
                          (lambda (port)
                            (let loop
                              ((exprs '()))
                              (let ((expr (read port)))
                                (if (eof-object? expr)
                                    (reverse exprs)
                                    (loop (cons expr exprs))))))))

(define* (analyze-package p
                          #:key (is-let? #f))
  (when is-let?
    (begin
      (newline)
      (write p)
      (newline)))
  `((name ,(assoc-ref p
                      'name))
    (version ,(assoc-ref p
                         'version))
    (synopsis ,(assoc-ref p
                          'synopsis))
    (description ,(assoc-ref p
                             'description))))

(define* (analyze-let-package p)
  (let* ((pp (cdr (caddr (caddr p)))))
    (analyze-package pp
                     #:is-let? #t)))

(define (safe-analyze-package p)
  (catch #t
         (lambda ()
           (analyze-package (caddr p)
                            #:is-let? #f))
         (lambda (key . args)
           (display (format #f "error: ~a: ~a" key args))
           (analyze-let-package p))))

(define (cond-rend x label)
  (when (not (equal? #f x))
    (display (format #f "\n~a: ~a" label
                     (car x)))))

(define (cond-car x)
  (when (not (equal? #f x))
    (car x)))

(define* (display-package-details details)
  (when (not (equal? #f
                     (assoc-ref details
                                'name)))
    (display "\n===================="))
  (cond-rend (cond-car (assoc-ref details
                                  'name))
             'name)
  (cond-rend (cond-car (assoc-ref details
                                  'version))
             'version)
  (cond-rend (cond-car (assoc-ref details
                                  'synopsis))
             'synopsis)
  (cond-rend (cond-car (assoc-ref details
                                  'description))
             'description))

(define (main)
  (display "\nStarting Guile Show Hub")
  (display (format #f "\nFetching from URL: ~a"
                   (guile-xyz-uri)))
  (let* ((raw-str (do-fetch (guile-xyz-uri)))
         (guile-packages (filter (lambda (s)
                                   (equal? 'define-public
                                           (car s)))
                                 (read-all-sexps-from-string raw-str)))
         (package-details (filter (lambda (p)
                                    (and (not (equal? '() p))
                                         (not (equal? #f p))))
                                  (map safe-analyze-package guile-packages))))
    (display (format #f "
Parsed ~a package s-expressions from guile-xyz.scm"
                     (length package-details)))

    (map display-package-details
         (list-head package-details 50))))
