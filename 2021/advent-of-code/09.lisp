(in-package #:advent-of-code)

(defun parse-matrix (lines)
  (let* ((y-len (list-length lines))
	(x-len (list-length (string-to-chars (car lines))))
	(matrix (make-array (list y-len x-len))))
    (loop for line in lines
	  for y from 0
	  do (loop for value in
			     (mapcar 'digit-char-p (string-to-chars line))
		   for x from 0
		   do (setf (aref matrix y x) value)))
    matrix))

(defclass heightmap-point ()
  ((value :initarg :value)
   (y :initarg :y)
   (x :initarg :x)
   ))

(defun heightmap-point-eql (p0 p1)
  (and
   (= (slot-value p0 'value) (slot-value p1 'value))
   (= (slot-value p0 'x) (slot-value p1 'x))
   (= (slot-value p0 'y) (slot-value p1 'y))))

(defun get-heightmap-point(matrix y x)
  (make-instance 'heightmap-point :value (aref matrix y x) :y y :x x))

(defun matrix-neighbours (matrix y x)
  (let ((neighbours '()))
    (if (> y 0)
	(push (get-heightmap-point matrix (1- y) x) neighbours))
    (if (> x 0)
	(push (get-heightmap-point matrix y (1- x)) neighbours))
    (if (< y (1- (array-dimension matrix 0)))
	(push (get-heightmap-point matrix (1+ y) x) neighbours))
    (if (< x (1- (array-dimension matrix 1)))
	(push (get-heightmap-point matrix y (1+ x)) neighbours))
    neighbours))
    
(defun find-valleys (matrix)
  (let ((valleys '()))
    (loop for y from 0 to (1- (array-dimension matrix 0))
	  do (loop for x from 0 to (1- (array-dimension matrix 1))
		   do (let ((point (get-heightmap-point matrix y x))
			    (neighbours (matrix-neighbours matrix y x)))
			(if (not (remove-if
				  (lambda (x) (> (slot-value x 'value)
						 (slot-value point 'value)))
				  neighbours))
			    (push point valleys)))))
    valleys))

(defun find-basin-points (matrix point &optional (basin-points '()))
  (if (or
       (>= (slot-value point 'value) 9)
       (find point basin-points :test 'heightmap-point-eql))
      basin-points
      (let ((new-points (set-difference
			 (matrix-neighbours
			  matrix
			  (slot-value point 'y)
			  (slot-value point 'x))
			 basin-points :test 'heightmap-point-eql)))
	(remove-duplicates
	 (alexandria:flatten
	  (mapcar
	   (lambda (p) (find-basin-points matrix p (append new-points basin-points)))
	   new-points))))))

(defun solve-09 ()
  (let* ((rows (file-to-list "09.txt"))
	 (matrix (parse-matrix rows))
	 (valleys (find-valleys matrix))
	 (risk-levels (mapcar (lambda (x) (1+ (slot-value x 'value))) valleys))
	 (result (apply '+ risk-levels)))
    result))

(defun solve-09b ()
  (let* ((rows (file-to-list "09-test.txt"))
	 (matrix (parse-matrix rows))
	 (valleys (find-valleys matrix))
	 (basin-point-list (mapcar
			    (lambda (v) (find-basin-points matrix v)) valleys))
	 (basin-size-list (mapcar 'list-length basin-point-list)))
;	 (result (apply '* (subseq basin-size-list 0 3))))
    basin-size-list))
