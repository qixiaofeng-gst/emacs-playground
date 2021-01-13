(provide 'qxf-utils)

; {[Macro] defmacro name args [doc] [declare] body...}

(defun *append-to-buffer (*message *buffer)
    (with-current-buffer *buffer
	(insert (format "%s\n" *message))))

(defun *print-to-buffer (*message *buffer)
    (with-current-buffer *buffer
	(erase-buffer)
	(insert (format "%s\n" (current-time-string)))
	(*append-to-buffer *message *buffer)))

(defun *trim (*string)
    (let*
        (
            (*length (length *string))
            (*first-nonspace-index (*get-nonspace-index *string 0 :forward))
            (*last-nonspace-index (*get-nonspace-index *string (1- *length) :backward))
        )
        (if (eq nil *first-nonspace-index)
            ""
            (substring *string *first-nonspace-index (1+ *last-nonspace-index))
        )
    )
)

(defmacro qxf-*-print (*target)
    `(let
	 (
	     (*out-string "")
	     (*out
		 (lambda (*c)
		     (if (eq *c ?\n)
			 (setq *out-string (format "%s\\n" *out-string))
			 (setq *out-string (format "%s%c" *out-string *c))
			 )
		     )
		 )
	     )
	 (prin1 (quote ,*target) *out)
	 *out-string
	 )
    )

(defmacro qxf-*-stringify (*target)
    `(format "%s:%s" (qxf-*-print ,*target) ,*target)
    )

(print "Loaded qxf-utils.")
