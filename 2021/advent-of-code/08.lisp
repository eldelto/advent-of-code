(in-package #:advent-of-code)

(defun clean-string-split (string separator)
  (remove ""
	  (mapcar (lambda (s) (string-trim " " s))
		  (uiop:split-string string :separator separator))
	  :key 'string :test 'string-equal))

(defun parse-digits (row)
  (let ((parts (uiop:split-string row :separator "|")))
    (list (clean-string-split (car parts) " ")
	  (clean-string-split (second parts) " "))))

(defun is-easy-digit (digit)
  (let ((len (length digit)))
    (or (= len 2) (= len 3) (= len 4) (= len 7))))

(defun string-to-chars (string)
  (loop for char across string
        collect char))

(defun list-to-number (number-list)    
  (apply '+  (loop for number in (reverse number-list)
		   for x from 0
		   collect (* number (expt 10 x)))))

(defun find-in-signal-list (condition signal-list)
  (block outer
    (loop for signal in signal-list
	  do (let ((char-list (string-to-chars signal)))
	       (if (funcall condition char-list)
		   (return-from outer char-list))))))

(defun find-1 (digit-signal)
  (= (length digit-signal) 2))

(defun find-4 (digit-signal)
  (= (length digit-signal) 4))

(defun find-7 (digit-signal)
  (= (length digit-signal) 3))

(defun find-8 (digit-signal)
  (= (length digit-signal) 7))

(defun find-3 (digit-signal 1-signal)
  (and (= (length digit-signal) 5) (subsetp 1-signal digit-signal)))

(defun find-9 (digit-signal 4-signal)
  (and (= (length digit-signal) 6) (subsetp 4-signal digit-signal)))

(defun find-0 (digit-signal 1-signal 4-signal)
  (and (= (length digit-signal) 6)
       (subsetp 1-signal digit-signal)
       (not (subsetp 4-signal digit-signal))))

(defun find-6 (digit-signal 1-signal 4-signal)
  (and (= (length digit-signal) 6)
       (not (subsetp 4-signal digit-signal))
       (not (subsetp 1-signal digit-signal))))

(defun find-5 (digit-signal 6-signal 9-signal)
  (and (= (length digit-signal) 5)
       (subsetp (intersection 6-signal 9-signal) digit-signal)))

(defun find-2 (digit-signal 6-signal 9-signal)
  (and (= (length digit-signal) 5)
       (subsetp (set-exclusive-or 6-signal 9-signal) digit-signal)))

(defun decode-digit-signals (digit-signals)
  (let* ((s1 (find-in-signal-list 'find-1 digit-signals))
	 (s4 (find-in-signal-list 'find-4 digit-signals))
	 (s7 (find-in-signal-list 'find-7 digit-signals))
	 (s8 (find-in-signal-list 'find-8 digit-signals))
	 (s3 (find-in-signal-list (lambda (x) (find-3 x s1)) digit-signals))
	 (s9 (find-in-signal-list (lambda (x) (find-9 x s4)) digit-signals))
	 (s0 (find-in-signal-list (lambda (x) (find-0 x s1 s4)) digit-signals))
	 (s6 (find-in-signal-list (lambda (x) (find-6 x s1 s4)) digit-signals))
	 (s5 (find-in-signal-list (lambda (x) (find-5 x s6 s9)) digit-signals))
	 (s2 (find-in-signal-list (lambda (x) (find-2 x s6 s9)) digit-signals)))
    (list s0 s1 s2 s3 s4 s5 s6 s7 s8 s9)))

(defun proper-subsetp (a b)
  (null (set-exclusive-or a b)))

(defun decode-digits (parts)
  (let* ((digit-signals (car parts))
	 (digits (second parts))
	 (decoded-signals (decode-digit-signals digit-signals))
	 (digit-list (loop for digit in digits
			   collect (let ((digit-chars (string-to-chars digit)))
				     (position digit-chars decoded-signals
					       :test 'proper-subsetp)))))
    (list-to-number digit-list)))

(defun solve-08 ()
  (let* ((rows (file-to-list "08.txt"))
	 (parts-list (mapcar 'parse-digits rows))
	 (digits (alexandria:flatten (mapcar 'second parts-list)))
	 (result (list-length (filter-easy-digits digits))))
    result))

(defun solve-08b ()
  (let* ((rows (file-to-list "08.txt"))
	 (parts-list (mapcar 'parse-digits rows))
	 (digits (mapcar 'decode-digits parts-list))
	 (result (apply '+ (alexandria:flatten digits))))
    result))
