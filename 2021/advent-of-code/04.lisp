(in-package #:advent-of-code)

(defun split-bingo-parts (lines)
  (list (car lines) (cddr lines)))

(defun parse-bingo-numbers (number-line separator)
  (let ((result '()))
    (loop for x in (uiop:split-string number-line :separator separator)
	  do (if (not (string-equal x ""))
		 (setq result (append result (list (parse-integer x))))))
    result))

(defun parse-bingo-fields (bingo-field-lines)
  (let ((bingo-fields '())
	(bingo-field '()))
    (loop for field-line in bingo-field-lines
	  do (if (string-equal field-line "")
		 (progn
		   (setq bingo-fields (append bingo-fields (list bingo-field)))
		   (setq bingo-field '()))
		 (setq bingo-field
		       (cons
			(parse-bingo-numbers field-line " ")
			bingo-field))))
    bingo-fields))

(defun solve-04 ()
  (let* ((lines (file-to-list "04-test.txt"))
	 (bingo-parts (split-bingo-parts lines))
	 (bingo-numbers (parse-bingo-numbers (car bingo-parts) ","))
	 (bingo-fields (parse-bingo-fields (second bingo-parts))))
    (list bingo-numbers bingo-fields)))
