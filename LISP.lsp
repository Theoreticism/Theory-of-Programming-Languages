;;;; Caesar cipher program in LISP
;;;; Author: Christopher Lee

(defun rotate (cc shift)
	;; Rotates every letter in the sentence according to the shift amount provided
	(let* ((chcode (char-code cc))
		(lowera (char-code #\a)) (lowerz (char-code #\z))
		(uppera (char-code #\A)) (upperz (char-code #\Z))
		;; Returns the lowercase or uppercase letter if between ASCII 97 and 122, 65 or 90
		;; Otherwise returns nil for the following if statement
		(base (cond ((and (>= chcode lowera) (<= chcode lowerz)) lowera)
					((and (>= chcode uppera) (<= chcode upperz)) uppera)
					(t nil))))
		;; If lowercase or uppercase a-z, do the rotate. Otherwise, with the nil from above,
		;; return the same character (cc).
		(if base (code-char (+ (mod (+ (- chcode base) shift) 26) base)) cc)))

(defun encrypt (sentence shift)
	;; Maps the rotate function to the sequence sentence
	(map 'string #'(lambda (c) (rotate c shift)) sentence))

(defun decrypt (sentence shift)
	;; Decrypting is the same as encrypting a negative shift
	(encrypt sentence (- shift)))
	
(defun solve (sentence maxshift)
	;; Solves the given sentence and max shift value using decryption
	(let ((countup 0))
		(loop while (>= maxshift 0) do
			(format t "Caesar ~D: ~A ~%" maxshift (decrypt sentence countup))
			(setq maxshift (- maxshift 1))
			(setq countup (+ countup 1)))))

;;; Change sentence to be encrypted, shift amount, and max shift amount for solve here
(let* ((sentence "Hello world")(shift 3)(maxshift 26)(sentencetwo "HAL")
	(encrypted (encrypt sentence shift))(decrypted (decrypt encrypted shift)))
	(format t "Original sentence: ~A ~%" sentence)
	(format t "Shift amount: ~A ~%" shift)
	(format t "Encrypted sentence: ~A ~%" encrypted)
	(format t "Decrypted sentence: ~A ~%" decrypted)
	(solve sentencetwo maxshift))
