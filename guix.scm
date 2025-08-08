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
  (version "0.0.4")
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
  (propagated-inputs (list guile-curl))
  (inputs (list guile-3.0))
  (synopsis "")
  (description "")
  (home-page "https://codeberg.org/jjba23/guile-show-hub")
  (license license:lgpl3+))

