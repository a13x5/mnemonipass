(import (srfi 48))
(include "cli-parsing.scm")
(include "pass-entropy.scm")

(define specials '("!" "@" "#" "$" "%" "^" "&" "*" "(" ")"))

(define cli-options
  (list
   (make-cli-option 'string "n" "nouns" "Nouns dictionary file name" #!void)
   (make-cli-option 'string "v" "verbs" "Verbs dictionary file name" #!void)
   (make-cli-option 'string "j" "adjectives" "Adjectives dictionary file name" #!void) 
   (make-cli-option 'string "a" "adverbs" "Adverbs dictionary file name" #!void)
   (make-cli-option 'number "w" "words" "A number of words to use [maximum 6]" 3)
   (make-cli-option 'number "d" "digits" "A number of digits to use" 0)
   (make-cli-option 'number "c" "characters" "A number of characters to use from each word" 3)
   (make-cli-option 'toggle "s" "specials" "Use a special characters" #f)
   (make-cli-option 'toggle "C" "capitalize" "Capitalize each word" #f)
   (make-cli-option 'toggle "h" "help" "Print help" #f)))

(define (make-help-message cli-opts)
  (string-append "Usage: mnemonipass [OPTIONS]\n"
		 "Creates a mnemonic password from dictionary files provided.\n"
		 "\n"
		 (make-cli-options-description cli-opts)
		 "\n"
		 "Examples:\n"
		 "mnemonipass -n ./nouns -v ./verbs    Creates simple password using nouns and verbs files as dictionary.\n"
		 "mnemonipass -n ./nouns -v ./verbs -a ./adverbs -j ./adjectives -w 6 -d 3 -s     Creates stronger password using 4 dictionaries, 3 digits with special characters.\n"
		 "\n"))

(define (string-split str char)
    (let loop ((str-lst (string->list str)) (word '()) (out '()))
      (cond
       ((null-list? str-lst) 
	(map list->string (reverse (cons (reverse word) out))))
       ((char=? char (car str-lst))
	(loop (cdr str-lst) '() (cons (reverse word) out)))
       (else (loop (cdr str-lst) (cons (car str-lst) word) out)))))

(define (string-capitalize str)
  (let ((str-lst (string->list str)))
    (list->string (cons (char-upcase (car str-lst)) (cdr str-lst)))))

(define (nth n lst)
  (if (or (> n (length lst)) (< n 0))
    (error "Index out of bounds")
    (if (eq? n 0)
      (car lst)
      (nth (- n 1) (cdr lst)))))

(define (nth-random lst)
  (nth (random-integer (length lst)) lst))

(define (dictfile->list file)
  (if (eq? file #!void)
      #!void
      (filter (lambda (str) (not (string=? "" str)))
	      (string-split
	       (call-with-input-file file
		 (lambda (port) (read-line port #f)))
	       #\newline))))

(define (make-digits digits-num specials?)
  (let ((nspecials
	 (if (and specials? (> digits-num 0))
	     (random-integer digits-num)
	     0)))
    (let loop
	((ndigits (- digits-num nspecials))
	 (nspecials nspecials)
	 (out '()))
      (cond
       ((> ndigits 0) 
	(loop (- ndigits 1) nspecials (cons (number->string (random-integer 10)) out)))
       ((> nspecials 0)
	(loop ndigits (- nspecials 1) (cons (nth (random-integer 10) specials) out)))
       (else out)))))

(define (make-words words-num nouns verbs adjectives adverbs)
  (cond
   ((= words-num 3)
    (list (nth-random nouns) (nth-random verbs) (nth-random nouns)))
   ((= words-num 4)
    (if (eq? adjectives #!void)
	(error "Adjectives must be passed when the number of words is more than 3")
	(list (nth-random adjectives) (nth-random nouns) (nth-random verbs) (nth-random nouns))))
   ((= words-num 5)
    (if (eq? adjectives #!void)
	(error "Adjectives must be passed when the number of words is more than 3")
	(list (nth-random adjectives) (nth-random nouns) (nth-random verbs) (nth-random adjectives) (nth-random nouns))))
   ((= words-num 6)
    (if (or (eq? adjectives #!void) (eq? adverbs #!void))
	(error "Both adjectives and adverbs must be passed when the number of words is more than 5")
    (list (nth-random adjectives) (nth-random nouns) (nth-random adverbs) (nth-random verbs) (nth-random adjectives) (nth-random nouns))))
   (else (error "Invalid number of words. Please specify range from 3 to 6"))))

(define (make-phrase words digits capitalize)
  (append digits
	  (if capitalize
	      (map string-capitalize words)
	      words)))

(define (phrase->pass phrase char-num)
  (map (lambda (str) (if (< (string-length str) char-num)
			 str
			 (substring str 0 char-num)))
	       phrase))

(define (main)
  (define cli-opts-hash (with-exception-handler
			    (lambda (exc)
			      (display (format "Error parsing arguments:~% ~a~%Please check if options are correct~%~%~a"
					       exc
					       (make-help-message cli-options)))
			      (exit 1))
			  (lambda () (parse-cli (command-line) cli-options))))
  
  (define (get-opt opt)
    (get-option-value opt cli-opts-hash cli-options))

  (cond
   ((get-opt "help")
    (display (make-help-message cli-options))
    (exit 0))
   ((or (eq? #!void (get-opt "nouns")) (eq? #!void (get-opt "verbs")))
    (display (format "Both nouns and verbs options are mandatory~%~%~a" (make-help-message cli-options)))
    (exit 1))
   ((and (get-opt "specials") (= (get-opt "digits") 0))
    (display "The specials option must be used with digits\n")
    (exit 1)))
  
  (random-source-randomize! default-random-source)
  
  (let* ((nouns (dictfile->list (get-opt "nouns")))
	 (verbs (dictfile->list (get-opt "verbs")))
	 (adjectives (dictfile->list (get-opt "adjectives")))
	 (adverbs (dictfile->list (get-opt "adverbs")))
	 (words-num (get-opt "words"))
	 (digits-num (get-opt "digits"))
	 (capitalize (get-opt "capitalize"))
	 (use-specials (get-opt "specials"))
	 (char-num (get-opt "characters"))
	 (phrase (make-phrase
		  (make-words words-num nouns verbs adjectives adverbs)
		  (make-digits digits-num use-specials)
		  capitalize))
	 (pass (string-concatenate (phrase->pass phrase char-num)))
	 (entropy (calc-password-entropy pass (characters-pool capitalize (> digits-num 0) specials))))
    
    (display (format "Mnemonic phrase:~%~t~a~%" (string-concatenate phrase " ")))
    (display (format "Password:~%~t~a~%" pass))
    (display (pretty-entropy-format entropy))))
