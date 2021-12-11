(in-package #:advent-of-code)

(defun clean-string-split (string separator)
  (remove "" (mapcar (lambda (s) (string-trim " " s)) (uiop:split-string string :separator separator))))
 
(defun parse-digits (row)
    (let ((parts (uiop:split-string row :separator "|")))
      (list (clean-string-split (car parts) " ") (clean-string-split (second parts) " "))))
    

(defun solve-08 ()
  (let* ((rows (file-to-list "08-test.txt"))
	 (part-list (mapcar 'parse-digits rows)))
    part-list))
