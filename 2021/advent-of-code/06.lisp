(in-package #:advent-of-code)

(defun count-fish-states (fish-list)
  (reverse
   (loop for i from 0 to 8
	 collect (let ((count 0))
		   (loop for fish in fish-list
			 do (if (= fish i)
				(setq count (1+ count))))
		   count))))

(defun run-fish-simulation-step (state-list)
  (let* ((state-8 (car (last state-list)))
	 (new-state-list (cons state-8 (butlast state-list))))
    (setf (nth 2 new-state-list) (+ state-8 (nth 2 new-state-list)))
    new-state-list))

(defun run-fish-simulation (initial-fish-list cycles)
  (let ((fish-list initial-fish-list))
    (loop repeat cycles
	  do (setq fish-list (run-fish-simulation-step fish-list)))
    fish-list))

(defun solve-06 ()
  (let* ((rows (file-to-list "06.txt"))
	 (fish-str-list (uiop:split-string (car rows) :separator ","))
	 (fish-list (mapcar 'parse-integer fish-str-list))
	 (fish-state-list (count-fish-states fish-list))
	 (new-fish-state-list (run-fish-simulation fish-state-list 256))
	 (result (apply '+ new-fish-state-list)))
    result))





