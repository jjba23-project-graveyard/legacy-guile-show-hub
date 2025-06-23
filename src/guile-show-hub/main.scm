
(define-module (guile-show-hub main)
  )

(define guile-xyz-uri
  (make-parameter
   "https://codeberg.org/guix/guix/raw/branch/master/gnu/packages/guile-xyz.scm"))

(define (main)
  (display "Starting Guile Show Hub")
  (display (format #f "Fetching from ~a" guile-xyz-uri)))

