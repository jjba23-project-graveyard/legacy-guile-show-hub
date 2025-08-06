;; guile-show-hub

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; guile-show-hub is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; guile-show-hub is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with guile-show-hub.  If not, see <https://www.gnu.org/licenses/>.

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
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 threads)
  #:use-module (guile-show-hub sxml)
  #:export (main))

(define guile-xyz-uri
  (make-parameter
   "https://codeberg.org/guix/guix/raw/branch/master/gnu/packages/guile-xyz.scm"))

(define (do-fetch uri)
  
  (let* ((_ (display (format #f "\nFetching from URL: ~a" uri)))
         (handle (curl-easy-init))
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
    (version ,(if is-let?
                  (format #f "~a"
                          (cond-car (assoc-ref p
                                               'version)))
                  (cond-car (assoc-ref p
                                       'version))))
    (home-page ,(assoc-ref p
                           'home-page))
    (build-system ,(cond-car (assoc-ref p
                                        'build-system)))
    (synopsis ,(assoc-ref p
                          'synopsis))
    (license ,(if (assoc-ref p
                             'license)
                  (format #f "~a"
                          (cond-car (assoc-ref p
                                               'license))) #f))
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

(define (cond-car x)
  (if (not (equal? #f x))
      (car x) x))

(define (mk-style pairs)
  (string-join (map (lambda (p)
                      (format #f "~a: ~a"
                              (car p)
                              (cdr p))) pairs) "; "))

(define* (html-package-details details)
  `((div (@ (style ,(mk-style '((border-radius . "1em") (padding . "1em")
                                (margin-bottom . "1em")
                                (background-color . "#f5e9cb")
                                (border . "1px solid var(--fg-faint-color)")
                                (max-width . "86ch")
                                (color . "#484431")))))
         (div (@ (style ,(mk-style '((display . flex) (flex-wrap . wrap)
                                     (gap . "1em")))))
              (div (@ (style ,(mk-style '((display . flex)
                                          (flex-direction . row)
                                          (flex-wrap . wrap)
                                          (align-items . center)
                                          (gap . "1em")))))
                   ,(if (car (assoc-ref details
                                        'home-page))
                        `((a (@ (style ,(mk-style '((color . "var(--fg-color)")
                                                    (font-size . medium))))
                                (onMouseOver "this.style.color='#80431a'")
                                (onMouseOut "this.style.color='#484431'")
                                (href ,(car (assoc-ref details
                                                       'home-page))))
                             (h3 (@ (style ,(mk-style '((font-size . x-large)))))
                                 ,(car (assoc-ref details
                                                  'name)))))
                        `((h3 ,(car (assoc-ref details
                                               'name)))))

                   (h5 (@ (style ,(mk-style '((font-family . "var(--mono-font)")
                                              (font-size . medium)))))
                       ,(assoc-ref details
                                   'version))))
         ,(if (car (assoc-ref details
                              'synopsis))
              `((em (@ (style ,(mk-style '((font-size . large)))))
                    ,(latex-to-html (car (car (assoc-ref details
                                                         'synopsis))))))
              '())
         ,(if (car (assoc-ref details
                              'description))
              `((p (@ (style ,(mk-style '((font-size . large)
                                          (font-family . "var(--serif-font)")
                                          (white-space . wrap)
                                          (text-align . justify)
                                          (max-width . "70ch")))))
                   ,(latex-to-html (car (car (assoc-ref details
                                                        'description))))))
              '())
         ,(if (car (assoc-ref details
                              'license))
              `((p (@ (style ,(mk-style '((font-size . small)
                                          (font-family . "var(--serif-font)")
                                          (max-width . "70ch")))))
                   ,(car (assoc-ref details
                                    'license))))
              '())
         (details (summary (@ (style ,(mk-style '((cursor . pointer)))))
                           (small (@ (style ,(mk-style '((font-size . small)))))
                                  "more details"))
                  (div (@ (style ,(mk-style '((padding . "1.2em")
                                              (display . flex)
                                              (flex-direction . column)
                                              (gap . "1.2em")))))
                       ,(if (car (assoc-ref details
                                            'home-page))
                            `((div (span (@ (style ,(mk-style '((font-size . small)))))
                                         "home-page: ")
                                   (a (@ (style ,(mk-style '((font-size . small)
                                                             (color . "var(--fg-alt-color)"))))
                                         (href ,(car (assoc-ref details
                                                                'home-page))))
                                      ,(car (assoc-ref details
                                                       'home-page)))))
                            '())
                       ,(if (car (assoc-ref details
                                            'build-system))
                            `((div (span (@ (style ,(mk-style '((font-size . small)))))
                                         "build-system: ")
                                   (span (@ (style ,(mk-style '((font-size . medium)
                                                                (font-family . "var(--mono-font)")))))
                                         ,(format #f "~a"
                                                  (car (assoc-ref details
                                                                  'build-system))))))
                            '()))))))

(define (render-package-details xs)
  (append (map html-package-details xs)))

(define* (custom-fonts #:key (preconnects `((link (@ (rel preconnect)
                                                     (href
                                                      "https://fonts.googleapis.com")))
                                            (link (@ (rel preconnect)
                                                     (href
                                                      "https://fonts.gstatic.com")
                                                     (crossorigin "")))))
                       (stylesheet-href
                        "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;1,100;1,200;1,300;1,400;1,500;1,600;1,700&family=IBM+Plex+Sans:ital,wght@0,100..700;1,100..700&family=IBM+Plex+Serif:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;1,100;1,200;1,300;1,400;1,500;1,600;1,700&display=swap"))
  `(,preconnects (link (@ (rel stylesheet)
                          (href ,stylesheet-href)))))

(define latex-code-pattern
  (make-parameter "@code\\{[^}]+\\}{1}"))

(define latex-code-replacement
  (make-parameter "<code style=\"~a\">~a</code>"))

(define latex-code-replacement-procedure
  (lambda (styles)
    (lambda (m)
      (format #f
              (latex-code-replacement)
              (mk-style styles)
              (string-drop-right (string-drop (match:substring m) 6) 1)))))

(define latex-math-pattern
  (make-parameter "@math\\{[^}]+\\}{1}"))

(define latex-math-replacement
  (make-parameter "<code style=\"~a\">~a</code>"))

(define latex-math-replacement-procedure
  (lambda (styles)
    (lambda (m)
      (format #f
              (latex-math-replacement)
              (mk-style styles)
              (string-drop-right (string-drop (match:substring m) 6) 1)))))

(define latex-command-pattern
  (make-parameter "@command\\{[^}]+\\}{1}"))

(define latex-command-replacement
  (make-parameter "<code style=\"~a\">~a</code>"))

(define latex-command-replacement-procedure
  (lambda (styles)
    (lambda (m)
      (format #f
              (latex-command-replacement)
              (mk-style styles)
              (string-drop-right (string-drop (match:substring m) 9) 1)))))

(define latex-acronym-pattern
  (make-parameter "@acronym\\{[^}]+\\}{1}"))

(define latex-acronym-replacement
  (make-parameter "~a (~a)"))

(define latex-acronym-replacement-procedure
  (lambda ()
    (lambda (m)
      (let* ((match-acronym (string-drop-right (string-drop (match:substring m)
                                                            9) 1))
             (acronym-parts (string-split match-acronym #\,)))
        (format #f
                (latex-acronym-replacement)
                (car acronym-parts)
                (string-trim-both (cadr acronym-parts)))))))

(define latex-itemize-pattern
  (make-parameter "@itemize(.|\\r|\\n)*?@end\\sitemize"))

(define latex-itemize-replacement
  (make-parameter "<ul style=\"~a\">~a</ul>"))

(define latex-itemize-replacement-procedure
  (lambda (styles)
    (lambda (m)
      (format #f
              (latex-itemize-replacement)
              (mk-style styles)
              (string-join (map (lambda (l)
                                  (format #f "<li>~a</li>" l))
                                (string-split (string-replace-substring (string-drop-right
                                                                         (string-drop
                                                                          (match:substring
                                                                           m)
                                                                          8)
                                                                         12)
                                               "@item " "") #\;)))))))

(define latex-example-pattern
  (make-parameter "@example(.|\\r|\\n)*?@end\\sexample"))

(define latex-example-replacement
  (make-parameter "<pre style=\"~a\">~a</pre>"))

(define latex-example-replacement-procedure
  (lambda (styles)
    (lambda (m)
      (format #f
              (latex-example-replacement)
              (mk-style styles)
              (string-drop-right (string-drop (match:substring m) 8) 12)))))

(define latex-url-pattern
  (make-parameter "@url\\{[^}]+\\}{1}"))

(define latex-url-replacement
  (make-parameter "<a style=\"~a\" href=\"~a\">~a</a>"))

(define latex-url-replacement-procedure
  (lambda (styles)
    (lambda (m)
      (let* ((uri-and-title (string-drop-right (string-drop (match:substring m)
                                                            5) 1))
             (frags (string-split uri-and-title #\,))
             (the-url (if (> (length frags) 0)
                          (car frags) uri-and-title))
             (the-title (if (> (length frags) 1)
                            (cadr frags) uri-and-title)))
        (format #f
                (latex-url-replacement)
                (mk-style styles) the-url the-title)))))

(define latex-uref-pattern
  (make-parameter "@uref\\{[^}]+\\}{1}"))

(define latex-uref-replacement
  (make-parameter "<a style=\"~a\" href=\"~a\">~a</a>"))

(define latex-uref-replacement-procedure
  (lambda (styles)
    (lambda (m)
      (let* ((uri-and-title (string-drop-right (string-drop (match:substring m)
                                                            6) 1))
             (frags (string-split uri-and-title #\,))
             (the-url (if (> (length frags) 0)
                          (car frags) uri-and-title))
             (the-title (if (> (length frags) 1)
                            (string-trim-both (cadr frags)) uri-and-title)))
        (format #f
                (latex-uref-replacement)
                (mk-style styles) the-url the-title)))))

(define* (latex-to-html raw-str
                        #:key (code-styles '((font-family . "var(--mono-font)")
                                             (font-size . medium)))
                        (math-styles '((font-family . "var(--mono-font)")
                                       (font-size . medium)))
                        (command-styles '((font-family . "var(--mono-font)")
                                          (font-size . medium)))
                        (example-styles '((font-family . "var(--mono-font)")
                                          (border-radius . "1em")
                                          (background-color . "var(--bg-color)")
                                          (white-space . pre-wrap)
                                          (padding-top . "0.9em")
                                          (padding-bottom . "0.9em")
                                          (padding-right . "0.5em")
                                          (padding-left . "0.5em")
                                          (font-size . medium)))
                        (url-styles '((color . "var(--fg-alt-color)")
                                      (font-family . "var(--sans-font)")))
                        (itemize-styles '()))
  (let* ((str (if (string? raw-str) raw-str
                  (format #f "~a" raw-str)))
         (parse-functions (list (lambda (x)
                                  (regexp-substitute/global #f
                                                            (latex-code-pattern)
                                                            x
                                                            'pre
                                                            (latex-code-replacement-procedure
                                                             code-styles)
                                                            'post))
                                (lambda (x)
                                  (regexp-substitute/global #f
                                                            (latex-math-pattern)
                                                            x
                                                            'pre
                                                            (latex-math-replacement-procedure
                                                             math-styles)
                                                            'post))
                                (lambda (x)
                                  (regexp-substitute/global #f
                                                            (latex-command-pattern)
                                                            x
                                                            'pre
                                                            (latex-command-replacement-procedure
                                                             command-styles)
                                                            'post))
                                (lambda (x)
                                  (regexp-substitute/global #f
                                                            (latex-example-pattern)
                                                            x
                                                            'pre
                                                            (latex-example-replacement-procedure
                                                             example-styles)
                                                            'post))
                                (lambda (x)
                                  (regexp-substitute/global #f
                                                            (latex-url-pattern)
                                                            x
                                                            'pre
                                                            (latex-url-replacement-procedure
                                                             url-styles)
                                                            'post))

                                (lambda (x)
                                  (regexp-substitute/global #f
                                                            (latex-uref-pattern)
                                                            x
                                                            'pre
                                                            (latex-uref-replacement-procedure
                                                             url-styles)
                                                            'post))

                                (lambda (x)
                                  (regexp-substitute/global #f
                                                            (latex-itemize-pattern)
                                                            x
                                                            'pre
                                                            (latex-itemize-replacement-procedure
                                                             itemize-styles)
                                                            'post))
                                (lambda (x)
                                  (regexp-substitute/global #f
                                                            (latex-acronym-pattern)
                                                            x
                                                            'pre
                                                            (latex-acronym-replacement-procedure)
                                                            'post)))))
    (fold (lambda (fun accum)
            (apply fun
                   (list accum))) str parse-functions)))

(define (project-logo)
  `((figure (img (@ (style ,(mk-style '((border-radius . "1em")
                                        (max-width . "40ch"))))
                    (src
                     "https://jointhefreeworld.org/static-assets/guile-show-hub/guile-show-hub-small.png")
                    (alt "Guile Show Hub Project Logo")
                    (loading lazy)))
            (figcaption (small (em
                                "guile-show-hub logo was generated with AI (DALL-E)"))))))

(define (top-section)
  `((div (@ (style ,(mk-style '((margin . "0.8em") (display . flex)
                                (flex-direction . column)
                                (gap . "1em")
                                (max-width . "70ch")))))
         (h1 (@ (style ,(mk-style '((font-family . "var(--serif-font)")))))
             "λ guile-show-hub")
         (p (@ (style ,(mk-style '((font-size . medium)))))
            (span
             "Showcase of all the available Guile Scheme packages via the ")
            (a (@ (style ,(mk-style '((font-family . "var(--serif-font)")
                                      (color . "var(--fg-alt-color)"))))
                  (href "https://guix.gnu.org/")) "Guix package manager"))

         (p (@ (style ,(mk-style '((font-size . medium)))))
          "Let us promote the adoption of this great and powerful language, and to make the ecosystem better.")
         (div (@ (style ,(mk-style '((display . flex) (flex-wrap . wrap)
                                     (margin . "0.8em")
                                     (justify-content . center)
                                     (gap . "0.6em")))))
              (img (@ (src
                       "https://jointhefreeworld.org/ggg/dist/scheme-guile.svg")
                      (alt "Guile Scheme")))

              (img (@ (src
                       "https://jointhefreeworld.org/ggg/dist/gnu-guix.svg")
                      (alt "GNU Guix")))
              (img (@ (src
                       "https://jointhefreeworld.org/ggg/dist/license-agpl3+.svg")
                      (alt "GNU AGPL v3+")))))))

(define (top-section2)
  `((div (@ (style ,(mk-style '((display . flex) (margin . "0.8em")
                                (flex-direction . column)
                                (gap . "1em")))))
         (p (@ (style ,(mk-style '((font-size . medium)))))
            "Copyright © Josep Bigorra &lt;jjbigorra@gmail.com&gt;")
         (div (@ (style ,(mk-style '((max-width . "70ch")))))
              (em (@ (style ,(mk-style '((font-size . small)))))
               "guile-show-hub is free software licensed under the GNU Affero General Public License v3 or newer"))
         (em (small ,(format #f "last updated: ~a"
                             (strftime "%c"
                                       (localtime (current-time))))))
         (small (div (em "data taken from: ")
                     (a (@ (href ,(guile-xyz-uri))
                           (style ,(mk-style '((font-family . "var(--sans-font)")
                                               (color . "var(--fg-alt-color)")))))
                        ,(guile-xyz-uri))))

         (div (em (@ (style ,(mk-style '((font-size . small)))))
                  "find the source code here: ")
              (a (@ (style ,(mk-style '((font-size . small)
                                        (font-family . "var(--sans-font)")
                                        (color . "var(--fg-alt-color)"))))
                    (href "https://codeberg.org/jjba23/guile-show-hub"))
                 "https://codeberg.org/jjba23/guile-show-hub")))))

(define destination-dir
  (make-parameter "./dist"))

(define (main)
  (display "\nStarting Guile Show Hub")
  (let* ((raw-str (do-fetch (guile-xyz-uri)))
         (guile-packages (filter (lambda (s)
                                   (equal? 'define-public
                                           (car s)))
                                 (read-all-sexps-from-string raw-str)))
         (package-details (filter (lambda (p)
                                    (and (not (equal? '() p))
                                         (not (equal? #f p))
                                         (car (assoc-ref p
                                                         'name))))
                                  (map safe-analyze-package guile-packages)))
         (head `((meta (@ (charset UTF-8)))
                 (meta (@ (name viewport)
                          (content "width=device-width")))
                 (title "Guile Show Hub")
                 ,(custom-fonts)
                 (style "
:root {
  --sans-font: \"IBM Plex Sans\", sans-serif;
  --serif-font: \"IBM Plex Serif\", serif;
  --mono-font: \"IBM Plex Mono\", monospace;

  --fg-color: #484431;
  --fg-alt-color: #80431a;
  --fg-faint-color: #dcdcdc;
  --bg-color: #fff7e9;
  --bg-alt-color: #f5e9cb;
}
")))
         (hh `(html (@ (style ,(mk-style '((background-color . "var(--bg-color)")
                                           (color . "var(--fg-color)")
                                           (font-family . "var(--serif-font)")))))
                    ,head
                    (body (div (div (@ (style ,(mk-style '((display . flex)
                                                           (flex-direction . row)
                                                           (flex-wrap . wrap)
                                                           (align-items . center)
                                                           (gap . "1em")
                                                           (padding . "1em")
                                                           (justify-content . space-between)))))
                                    (div ,(top-section)
                                         ,(top-section2))
                                    ,(project-logo))
                               (hr (@ (style ,(mk-style '((color . "var(--fg-faint-color)"))))))
                               (h2 (@ (style ,(mk-style '((font-family . "var(--sans-font)")))))
                                   ,(format #f "packages (~a items)"
                                            (length package-details)))
                               (div (@ (style ,(mk-style '((margin . "0.8em")))))
                                    ,(render-package-details package-details))))))
         (page `("<!DOCTYPE html>" ,hh)))
    
    (catch #t
           (lambda ()
             (mkdir (destination-dir)))
           (lambda (key . args)
             #t))
    (with-output-to-file (format #f "~a/index.html"
                                 (destination-dir))
      (lambda ()
        (sxml->xml page)))))


