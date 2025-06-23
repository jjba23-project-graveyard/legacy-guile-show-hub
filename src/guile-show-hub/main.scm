
(define-module (guile-show-hub main)
  #:use-module (curl)
  #:use-module (ice-9 string-fun)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 local-eval)
  #:use-module (guix gexp)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (sxml simple)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 threads)
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
  `((name ,(assoc-ref p
                      'name))
    (version ,(assoc-ref p
                         'version))
    (synopsis ,(assoc-ref p
                          'synopsis))
    (description ,(assoc-ref p
                             'description))))

(define (safe-analyze-package p)
  (let* ((is-let-package (and (list? (caddr p))
                              (> (length (caddr p)) 0)
                              (equal? 'let
                                      (car (caddr p))))))
    (if is-let-package
        (analyze-package (caddr (caddr p))
                         #:is-let? #t)
        (analyze-package (caddr p)
                         #:is-let? #f))))

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

(define* (html-package-details details)
  `(h3 ,(car (assoc-ref details
                        'name))))

(define (render-package-details xs)
  (append (map html-package-details xs)))

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
                                  (map safe-analyze-package guile-packages)))
         (hh `(html (body (div (h2 ,(format #f
                                     "guile-show-hub: packages (~a items)"
                                     (length package-details)))
                               (div (p ,(format #f "taken from: ~a"
                                                (guile-xyz-uri)))
                                    (p ,(format #f "last updated: ~a"
                                                (strftime "%c"
                                                          (localtime (current-time))))))
                               ,(render-package-details package-details)))))
         (page `(,hh)))
    
    (with-output-to-file "index.html"
      (lambda ()
        (sxml->xml page)))))


