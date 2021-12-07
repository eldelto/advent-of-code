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
		       (append
			bingo-field
			(list (parse-bingo-numbers field-line " "))))))
    bingo-fields))

(defun row-matches (numbers row)
  (let ((matches 0))
    (loop for x in row
     do (if (find x numbers)
	    (setq matches (1+ matches))))
    matches))
  
(defun check-rows (numbers bingo-field)
  (let ((max-matches (list-length (car bingo-field)))
	(match-list (loop for row in bingo-field
			  collect (row-matches numbers row))))
    (find max-matches match-list)))

(defun get-bingo-field-columns (bingo-field)
  (let ((columns '())
	(max-index (1- (list-length (car bingo-field)))))
    (loop for index from 0 to max-index
	    do (let ((column '()))
		 (loop for row in bingo-field
		     do (setq column (cons (nth index row) column)))
		 (setq columns (cons column columns))))
    columns))

(defun check-bingo-field (numbers bingo-field)
  (let* ((row-match (check-rows numbers bingo-field))
	 (columns (get-bingo-field-columns bingo-field))
	 (column-match (check-rows numbers columns)))
    (or row-match column-match)))

(defun play-bingo (numbers bingo-fields)
  (block outer
    (let ((played-numbers '()))
      (loop for number in numbers
	    do (setq played-numbers (cons number played-numbers))
	       (loop for bingo-field in bingo-fields
		     do (if (check-bingo-field played-numbers bingo-field)
			    (return-from outer
			      (list played-numbers bingo-field))))))))

(defun play-losing-bingo (numbers initial-bingo-fields)
  (block outer
    (let ((played-numbers '())
	  (bingo-fields initial-bingo-fields))
      (loop for number in numbers
	    do   (setq played-numbers (cons number played-numbers))
		 (loop for bingo-field in bingo-fields
		       do (cond
			    ((and
			      (check-bingo-field played-numbers bingo-field)
			      (= (list-length bingo-fields) 1))
			     (return-from outer
			       (list played-numbers bingo-field)))
			    ((check-bingo-field played-numbers bingo-field)
			     (setq bingo-fields
				   (remove bingo-field bingo-fields)))))))))

(defun calc-bingo-result (played-numbers bingo-field)
  (let ((sum 0)
	(field-numbers (alexandria:flatten bingo-field)))
    (loop for number in field-numbers
	  do (if (not (find number played-numbers))
		 (setq sum (+ sum number))))
    (* sum (car played-numbers))))

(defun solve-04 ()
  (let* ((lines (file-to-list "04.txt"))
	 (bingo-parts (split-bingo-parts lines))
	 (bingo-numbers (parse-bingo-numbers (car bingo-parts) ","))
	 (bingo-fields (parse-bingo-fields (second bingo-parts)))
	 (game-result (play-bingo bingo-numbers bingo-fields))
	 (played-numbers (car game-result))
	 (winning-field (second game-result))
	 (result (calc-bingo-result played-numbers winning-field)))
    result))

(defun solve-04b ()
  (let* ((lines (file-to-list "04.txt"))
	 (bingo-parts (split-bingo-parts lines))
	 (bingo-numbers (parse-bingo-numbers (car bingo-parts) ","))
	 (bingo-fields (parse-bingo-fields (second bingo-parts)))
	 (game-result (play-losing-bingo bingo-numbers bingo-fields))
	 (played-numbers (car game-result))
	 (winning-field (second game-result))
	 (result (calc-bingo-result played-numbers winning-field)))
    result))
