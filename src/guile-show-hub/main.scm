
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

(define (analyze-package s)
  (display "\n===================")

  (display (format #f "\nname: ~a"
                   (car (assoc-ref (caddr s)
                                   'name))))
  (display (format #f "\nversion: ~a"
                   (car (assoc-ref (caddr s)
                                   'version))))
  (display (format #f "\nsynopsis: ~a"
                   (car (assoc-ref (caddr s)
                                   'synopsis))))
  (display (format #f "\ndescription: ~a"
                   (car (assoc-ref (caddr s)
                                   'description)))))

(define (analyze-let-package s)
  (analyze-package (caddr s)))

(define (main)
  (display "\nStarting Guile Show Hub")
  (display (format #f "\nFetching from URL: ~a"
                   (guile-xyz-uri)))
  (let* ((raw-str (do-fetch (guile-xyz-uri)))
         (guile-packages (filter (lambda (s)
                                   (equal? 'define-public
                                           (car s)))
                                 (read-all-sexps-from-string raw-str))))
    (display (format #f "
Parsed ~a package s-expressions from guile-xyz.scm"
                     (length guile-packages)))
    (for-each (lambda (s)
                (catch #t
                       (lambda ()
                         (analyze-package s))
                       (lambda (key . args)
                         (display (format #f "\nerror analyzing package: ~a: ~a\n"
                                          key args))
                         (analyze-let-package s))))
              (list-head guile-packages 10))
    (display "\n===================")))
