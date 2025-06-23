(use-modules (guix gexp)
             (guix packages)
             (guix git-download)
             ((guix licenses)
              #:prefix license:)
             (guix build-system guile)
             ((guix build utils)
              #:select (with-directory-excursion ))
             (gnu packages)
             (gnu packages guile)
             (gnu packages curl)
             (gnu packages guile-xyz))

(define %source-dir
  (dirname (current-filename)))

(package
  (name "guile-show-hub")
  (version "0.0.2")
  (source
   (local-file %source-dir
               #:recursive? #t
               #:select? (git-predicate %source-dir)))
  (arguments
   `(#:source-directory "src"
     #:phases (modify-phases %standard-phases
                (add-before 'build 'remove-guix.scm
                  (lambda _
                    (delete-file "guix.scm")))
                (add-before 'build 'install-program-files
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                          (share (string-append (assoc-ref outputs "out")
                                                "/share")))
                      (mkdir-p (string-append share "/scripts"))
                      (mkdir-p (string-append share "/resources"))
                      (install-file "resources/help.txt"
                                    (string-append share "/resources/help.txt"))
                      (install-file "scripts/guile-show-hub" bin)
                      (install-file "scripts/log.sh"
                                    (string-append share "/scripts/"))
                      (chmod (string-append bin "/guile-show-hub") #o755)))))))
  (build-system guile-build-system)
  (native-inputs (list guile-3.0 guile-curl))
  (inputs (list guile-3.0 guile-curl))
  (synopsis
   "")
  (description
   "")
  (home-page "https://codeberg.org/jjba23/guile-show-hub")
  (license license:lgpl3+))

