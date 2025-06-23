(import (srfi 48))
(import (srfi 69))

(define-structure cli-option type key name description default)

(define (is-option? key)
  (string=? (substring key 0 1) "-"))

(define (cli-option-key-match? key cli-opt)
  (string=? (substring key 1 2) (cli-option-key cli-opt)))

(define (cli-option-name-match? name cli-opt)
  (string=? name (cli-option-name cli-opt)))

(define (coerce-option-value value type)
  (cond
   ((eq? type 'string)
    value)
   ((eq? type 'number)
    (string->number value))
   (else (error (format "Unknown option type ~a" (cli-option-type cli-opt))))))

(define (option-exists? key cli-opts)
  (let loop ((cli-opts cli-opts) (exists #f))
    (cond
     ((null-list? cli-opts)
      exists)
     (exists exists)
     (else
      (loop (cdr cli-opts) (cli-option-key-match? key (car cli-opts)))))))

(define (make-cli-options-description cli-opts)
  (let ((nice-default (lambda (opt)
			(cond
			 ((eq? opt #!void)
			  "None")
			 ((eq? opt #f)
			  "No")
			 ((eq? opt #t)
			  "Yes")
			 (else opt)))))
    (let loop ((cli-opts cli-options) (descr ""))
      (if (null-list? cli-opts)
	  descr
	  (loop (cdr cli-opts)
		(string-append
		 (format "~t-~a - ~a (default: ~a)~%"
			 (cli-option-key (car cli-opts))
			 (cli-option-description (car cli-opts))
			 (nice-default (cli-option-default (car cli-opts))))
		 descr))))))

(define (get-options-value proc key cli-opts)
  (let loop ((cli-opts cli-opts))
    (cond
     ((null-list? cli-opts)
      (error (format "Invalid option ~a ~%" key)))
     ((cli-option-key-match? key (car cli-opts))
      (proc (car cli-opts)))
     (else (loop (cdr cli-opts))))))

(define (get-option-default name cli-opts)
  (let loop ((cli-opts cli-opts))
    (cond
     ((null-list? cli-opts)
      (error (format "Invalid option name ~a ~%" name)))
     ((cli-option-name-match? name (car cli-opts))
      (cli-option-default (car cli-opts)))
     (else (loop (cdr cli-opts))))))

(define (get-option-value name cli-opts-hash cli-opts)
  (hash-table-ref/default cli-opts-hash name (get-option-default name cli-opts)))
	 
(define (parse-cli cli cli-opts)
  (let ((cli-opt-hash (make-hash-table)))
    (let loop ((cli (cdr cli)))
      (cond
       ((null-list? cli)
	cli-opt-hash)
       ((and (is-option? (car cli)) (option-exists? (car cli) cli-opts))
	(let ((opt-name (get-options-value cli-option-name (car cli) cli-opts))
	      (opt-type (get-options-value cli-option-type (car cli) cli-opts)))
	  (cond
	   ((eq? 'toggle opt-type)
	    (hash-table-set! cli-opt-hash opt-name #t)
	    (loop (cdr cli)))
	   (else
	    (hash-table-set! cli-opt-hash opt-name
			     (coerce-option-value (cadr cli) opt-type))
	    (loop (cddr cli))))))
       ((and (is-option? (car cli)) (not (option-exists? (car cli) cli-opts)))
	(error (format "Invalid option ~a ~%" (car cli))))
       (else (error (format "Error parsing option ~a ~%" (car cli))))))))
