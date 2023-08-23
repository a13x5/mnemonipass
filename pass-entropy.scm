(import (srfi 48))

(define (characters-pool capitals numbers specials)
  (let loop ((pool 26)
	     (capitalize capitalize)
	     (numbers numbers)
	     (specials specials))
    (cond
     (capitalize
      (loop (* pool 2) #f numbers specials))
     (numbers
      (loop (+ pool 10) capitalize #f specials))
     (specials
      (loop (+ pool 10) capitalize numbers #f))
     (else pool))))

;; Based on https://www.pleacher.com/mp/mlessons/algebra/entropy.html
(define (calc-password-entropy pass char-pool)
  (log (expt char-pool (string-length pass)) 2))

(define (pretty-entropy-format entropy)
  (define entropy-grade
    (cond
      ((< entropy 28) "Garbage.")
      ((and (> entropy 29) (< entropy 35)) "Weak, Don't use it.")
      ((and (> entropy 36) (< entropy 59)) "Okeyish, but not recommended.")
      ((and (> entropy 60) (< entropy 127)) "Strong, You can use it.")
      ((> entropy 128) "Very Strong, This one is the best!")))
  
  (format "The password entropy is ~2,2F bits which is ~a~%" entropy entropy-grade))
