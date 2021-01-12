; nth roots of unity:
; exp([2 * k * pi * i /n) = cos(2 * k * pi / n) + i * sin(2 * k * pi / n), k = 0,1,...,n-1.

(cos pi)
(round (sin pi))
pi

(let*
    (
	(N 8)
	(c (* 2 pi))
	(v nil)
	)
    (dotimes (i N)
	(setq v (* c (/ (float i) N)))
	(princ (format "%d: (%.2f,%.2f); " i (cos v) (sin v)))
	)
    :damn
    )

