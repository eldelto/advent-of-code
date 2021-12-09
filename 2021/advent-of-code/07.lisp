(in-package #:advent-of-code)

(defun calculate-fuel-for-position (crab-list position)
  (apply '+ (loop for crab in crab-list
		  collect (abs (- crab position)))))

(defun nth-triangle (number)
  (/ (+ (* number number) number) 2))

(defun calculate-fuel-for-position2 (crab-list position)
  (apply '+ (loop for crab in crab-list
		  collect (nth-triangle (abs (- crab position))))))

(defun calculate-cheapest-position (crab-list fuel-function)
  (let ((min (apply 'min crab-list))
	(max (apply 'max crab-list)))
    (apply 'min (loop for position from min to max
		      collect (funcall fuel-function crab-list position)))))

(defun solve-07 ()
  (let* ((rows (file-to-list "07.txt"))
	 (crab-str-list (uiop:split-string (car rows) :separator ","))
	 (crab-list (mapcar 'parse-integer crab-str-list))
	 (fuel (calculate-cheapest-position
		crab-list 'calculate-fuel-for-position)))
    fuel))


(defun solve-07b ()
  (let* ((rows (file-to-list "07.txt"))
	 (crab-str-list (uiop:split-string (car rows) :separator ","))
	 (crab-list (mapcar 'parse-integer crab-str-list))
	 (fuel (calculate-cheapest-position
		crab-list 'calculate-fuel-for-position2)))
    fuel))
