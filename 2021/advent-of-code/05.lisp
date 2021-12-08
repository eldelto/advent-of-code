(in-package #:advent-of-code)

(defun parse-lines (rows)
  (loop for row in rows
	for points = (remove-if (lambda (s) (string-equal s ""))
				(uiop:split-string row :separator " -> "))
	collect (mapcar 'parse-point points)))

(defun parse-point (str-point)
  (mapcar 'parse-integer (uiop:split-string str-point :separator ",")))

(defun is-diagonal-line (line)
  (let* ((start (car line))
	(end (second line)))
    (not (or (= (car start) (car end)) (= (second start) (second end))))))

(defun is-crooked-diagonal-line (line)
  (let* ((inclination (calculate-line-inclination line))
	 (x (car inclination))
	(y (second inclination)))
    (or (> (abs x) 1) (> (abs y) 1))))

(defun add-points (p0 p1)
  (list (+ (car p0) (car p1)) (+ (second p0) (second p1))))

(defun divide-point (point divisor)
  (list (/ (car point) divisor) (/ (second point) divisor)))

(defun smallest-absolute-value (values)
  (apply 'min (remove 0 (mapcar 'abs values))))

(defun normalize-point (point)
  (divide-point point (smallest-absolute-value point)))

(defun calculate-line-inclination (line)
   (let* ((start (car line))
	(end (second line)))
     (normalize-point
      (list (- (car end) (car start)) (- (second end) (second start))))))

(defun gethash-or-default (key hash-table default)
  (let ((value (gethash key hash-table)))
    (if value
	value
	default)))

(defun calculate-line-points (line)
  (let* ((start (car line))
	 (end (second line))
	 (inclination (calculate-line-inclination line))
	 (point start)
	 (line-points (loop until (equal point end)
			    do (setq point (add-points point inclination))
			    collect point)))
    (cons start line-points)))

(defun calculate-line-intersections (line-points-list)
  (let ((intersections (make-hash-table :test 'equal)))
    (loop for line-points in line-points-list
	  do (loop for point in line-points
		   do (let ((count (gethash-or-default point intersections 0)))
			(setf (gethash point intersections) (1+ count)))))
    (loop for key being the hash-keys of intersections
            using (hash-value value)
	  collect (list value key))))

(defun solve-05 ()
  (let* ((rows (file-to-list "05.txt"))
	 (lines (parse-lines rows))
	 (filtered-lines (remove-if 'is-diagonal-line lines))
	 (line-points-list (mapcar 'calculate-line-points filtered-lines))
	 (intersections (calculate-line-intersections line-points-list))
	 (greater-two-intersections (remove-if
				     (lambda (x) (< (car x) 2))
				     intersections))
	 (result (list-length greater-two-intersections)))
    result))

(defun solve-05b ()
  (let* ((rows (file-to-list "05.txt"))
	 (lines (parse-lines rows))
	 (filtered-lines (remove-if 'is-crooked-diagonal-line lines))
	 (line-points-list (mapcar 'calculate-line-points filtered-lines))
	 (intersections (calculate-line-intersections line-points-list))
	 (greater-two-intersections (remove-if
				     (lambda (x) (< (car x) 2))
				     intersections))
	 (result (list-length greater-two-intersections)))
    result))
