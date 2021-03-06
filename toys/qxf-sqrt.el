(defun qxf-sqrt (x)
    (let ((ts (float-time)) (result (/ x 2.0)) (bot 0) (top x) (err x) (count 0) (tmp 0))
	(while (> err 1e-6)
	    (setq tmp (- (* result result) x))
	    (if (> tmp 0)
		(setq top result)
		(setq bot result))
	    (setq result (/ (+ bot top) 2))
	    (setq err (abs tmp))
	    (setq count (1+ count)))
	(prin1 (format "Time cost: %f. Loop time: %d." (- (float-time) ts) count))
	result))

(prin1 (format "%f %f" (qxf-sqrt 999987654) (sqrt 2)))
