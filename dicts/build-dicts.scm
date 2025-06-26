(define (string-split str char)
    (let loop ((str-lst (string->list str)) (word '()) (out '()))
      (cond
       ((null-list? str-lst)
	(map list->string (reverse (cons (reverse word) out))))
       ((char=? char (car str-lst))
	(loop (cdr str-lst) '() (cons (reverse word) out)))
       (else (loop (cdr str-lst) (cons (car str-lst) word) out)))))

(define (dictfile->list file)
  (filter (lambda (str) (not (string=? "" str)))
	  (string-split
	   (call-with-input-file file
	     (lambda (port) (read-line port #f)))
	   #\newline)))

(let ((nouns (dictfile->list "nouns.txt"))
      (verbs (dictfile->list "verbs.txt"))
      (adjectives (dictfile->list "adjectives.txt"))
      (adverbs (dictfile->list "adverbs.txt")))
  (call-with-output-file "dicts.scm"
    (lambda (port)
      (write `(define nouns ',nouns) port)
      (write `(define verbs ',verbs) port)
      (write `(define adjectives ',adjectives) port)
      (write `(define adverbs ',adverbs) port))))
