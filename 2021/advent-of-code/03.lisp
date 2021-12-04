(in-package #:advent-of-code)

(defun binary-string-to-integer-list (str)
  (mapcar 'digit-char-p  (loop for c across str collect c)))

(defun zip-add (&rest values)
  (mapcar '+ values))

(defun counts-to-binary-list (counts threshold &optional (default 1))
    (loop for count in counts
	  collect (cond ((> count threshold) 1)
			((< count threshold) 0)
			((= count threshold) default)
			)))

(defun binary-list-to-string (binary-list)
  (let ((str-list (mapcar #'write-to-string binary-list)))
	 (apply #'uiop:strcat str-list)))
    
(defun binary-list-to-integer (binary-list)
  (let ((str-list (binary-list-to-string binary-list)))
    (parse-integer str-list :radix 2)))

(defun counts-to-binary (counts len)
  (let ((binary-list (counts-to-binary-list counts len)))
    (binary-list-to-string binary-list)))

(defun binary-not (binary-str)
    (binary-list-to-string (loop for c across binary-str
	  collect (if (char= c #\0)
		 1
		 0))))

(defun filter (f list)
  (let ((result '()))
    (dolist (item list)
      (if (funcall f item)
	  (setq result (cons item result))))
    result))

(defun find-oxygen-generator-rating (numbers &optional (index 0))
  (if (= (list-length numbers) 1)
      (car numbers)
      (let* ((counts (apply #'mapcar #'+ numbers))
	     (binary-list (counts-to-binary-list counts (/ (list-length numbers) 2)))
	     (wanted-number (car binary-list)))
	(find-oxygen-generator-rating
	 (filter (lambda (x) (= (nth index x) wanted-number)) numbers)
	 (1+ index)))))

(defun find-co2-scrubber-rating (numbers &optional (index 0))
  (if (= (list-length numbers) 1)
      (car numbers)
      (let* ((counts (apply #'mapcar #'+ numbers))
	     (binary-list (counts-to-binary-list counts (/ (list-length numbers) 2)))
	     (wanted-number (car binary-list)))
	(find-co2-scrubber-rating
	 (filter (lambda (x) (not (= (nth index x) wanted-number))) numbers)
	 (1+ index)))))


(defun solve-03 ()
  (let* ((input (file-to-list "03.txt"))
	 (numbers (mapcar #'binary-string-to-integer-list input))
	 (counts (apply #'mapcar #'+ numbers))
	 (gamma-rate-str (counts-to-binary counts (/ (list-length numbers) 2)))
	 (epsilon-rate-str (binary-not gamma-rate-str))
	 (gamma-rate (parse-integer gamma-rate-str :radix 2))
	 (epsilon-rate (parse-integer epsilon-rate-str :radix 2))
	 (result (* gamma-rate epsilon-rate)))
    result))

(defun solve-03b ()
  (let* ((input (file-to-list "03-test.txt"))
	 (numbers (mapcar #'binary-string-to-integer-list input))
	 (counts (apply #'mapcar #'+ numbers))
	 (oxygen-binary-list (counts-to-binary-list counts (/ (list-length numbers) 2)))
	 (co2-binary-list (counts-to-binary-list counts (/ (list-length numbers) 2) 0))
	 (oxygen-generator-rating-list (find-oxygen-generator-rating oxygen-binary-list numbers))
	 (co2-scrubber-rating-list (find-co2-scrubber-rating co2-binary-list numbers))
	 (oxygen-generator-rating (binary-list-to-integer oxygen-generator-rating-list))
	 (co2-scrubber-rating (binary-list-to-integer co2-scrubber-rating-list))
	 (result (* oxygen-generator-rating co2-scrubber-rating)))
   (list result oxygen-binary-list counts)))
