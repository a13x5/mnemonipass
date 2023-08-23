(use-modules (guix packages)
	     (guix licenses)
	     (guix gexp)
	     (guix download)
	     (guix build-system gnu)
	     (gnu packages)
	     (gnu packages commencement))

;; GNU packages have older version
(define gambit-495
  (package
    (name "gambit-c")
    (version "4.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.gambitscheme.org/"
             version "/gambit-v"
             (string-map (lambda (c) (if (char=? c #\.) #\_ c)) version)
             ".tgz"))
       (sha256
        (base32 "1p61z1rp0ya4i61mq3hzmr67r3xbvi9h058cf9ci2yqfbzdzi3p2"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; According to the ./configure script, this makes the build slower and
       ;; use >= 1 GB memory, but makes Gambit much faster.
       '("--enable-single-host")))
    (home-page "http://www.gambitscheme.org/")
    (synopsis "Efficient Scheme interpreter and compiler")
    (description
     "Gambit consists of two main programs: gsi, the Gambit Scheme
interpreter, and gsc, the Gambit Scheme compiler.  The interpreter contains
the complete execution and debugging environment.  The compiler is the
interpreter extended with the capability of generating executable files.  The
compiler can produce standalone executables or compiled modules which can be
loaded at run time.  Interpreted code and compiled code can be freely
mixed.")
    ;; Dual license.
    (license (list lgpl2.1+ asl2.0))))

(define mnemonipass
  (package
   (name "mnemonipass")
   (version "0.0.1")
   (source (local-file (dirname (current-filename)) #:recursive? #t))
   (build-system gnu-build-system)
   (arguments
    (list
     #:tests? #f
     #:make-flags #~(list (string-append "PREFIX=" #$output))
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'configure))))
   (native-inputs
    (list
     gambit-495
     (list gcc-toolchain "static")))
   
   (synopsis "Mnemonic passwords generator")
   (home-page "https://github.com/a13x5/mnemonipass")
   (description "Mnemonic passwords generator")
   (license gpl3+)))

mnemonipass
