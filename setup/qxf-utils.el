(provide 'qxf-utils)

(defalias '曰 'defalias)

(曰 '道 'defmacro)
(曰 '法 'defun)
(曰 '自 'defvar)
(曰 '然 'setq)

; “道法自然” 仅用于本文件中，勿在其它文件中使用。
; {[Macro] defmacro name args [doc] [declare] body...}

(道 m4-bind (a7-keys a7-command &optional a7-map)
    (if (null a7-map)
        `(define-key global-map (kbd ,a7-keys) (quote ,a7-command))
        `(define-key ,a7-map (kbd ,a7-keys) (quote ,a7-command))
    )
)

(道 ++ (a7-number &optional a7-increment)
    (if (eq nil a7-increment)
        `(setq ,a7-number (1+ ,a7-number))
        `(setq ,a7-number (+ ,a7-increment ,a7-number))
    )
)

(道 m4-make-object-oriented-like (a7-object)
    `(fset (quote ,a7-object)
        (lambda (*key &optional *value)
            (if (eq nil *value)
                (plist-get ,a7-object *key)
                (plist-put ,a7-object *key *value)
            )
        )
    )
)

(道 m4-insert-to-list (a7-value a7-list)
    `(setq ,a7-list (push ,a7-value ,a7-list))
)

(道 m4-initialize-outline-entry (a7-object a7-signature a7-line-number)
    `(progn
        (m4-make-object-oriented-like ,a7-object)
        (setq ,a7-object (list :signature ,a7-signature :line-number ,a7-line-number))
    )
)

(道 m4-stringify (a7-target)
    `(format "%s:%s" (m4-to-string ,a7-target) ,a7-target)
)

(道 m4-to-string (a7-target)
    `(let
        (
            (*out-string "")
            (*out
                (lambda (a7-char)
                    (if (eq a7-char ?\n)
                        (setq *out-string (format "%s\\n" *out-string))
                        (setq *out-string (format "%s%c" *out-string a7-char))
                    )
                )
            )
        )
        (prin1 (quote ,a7-target) *out)
        *out-string
    )
)

(法 f7-get-lines (a7-string)
    (let*
        (
            (*lines '())
            (*length (length a7-string))
            (l4-start-index 0)
            (*end-index nil)
        )
        (catch :break
            (while (< l4-start-index *length)
                (setq *end-index (f7-get-newline-index a7-string l4-start-index :forward))
                (when (null *end-index)
                    (push (substring a7-string l4-start-index) *lines)
                    (throw :break t)
                )
                (push (substring a7-string l4-start-index *end-index) *lines)
                (setq l4-start-index (1+ *end-index))
            )
        )
        (reverse *lines)
    )
)

(法 f7-get-file-contents (a7-file-path)
    (with-temp-buffer
        (insert-file-contents a7-file-path)
        (buffer-substring-no-properties (point-min) (point-max))
    )
)

(法 f7-append-to-buffer (a7-message a7-buffer)
    (with-current-buffer a7-buffer
        (let ((buffer-read-only))
            (insert (format "%s\n" a7-message))
        )
    )
)

(法 f7-print-to-buffer (a7-message a7-buffer)
    (with-current-buffer a7-buffer
        (let ((buffer-read-only nil))
            (erase-buffer)
            (insert (format "%s\n" (current-time-string)))
            (f7-append-to-buffer a7-message a7-buffer)
        )
    )
)

(法 f7-trim-string (a7-string)
    (let*
        (
            (*length (length a7-string))
            (*first-nonspace-index (f7-get-nonspace-index a7-string 0 :forward))
            (*last-nonspace-index (f7-get-nonspace-index a7-string (1- *length) :backward))
        )
        (if (eq nil *first-nonspace-index)
            ""
            (substring a7-string *first-nonspace-index (1+ *last-nonspace-index))
        )
    )
)

(法 f7-get-nearest-block-start (a7-string a7-current-point)
    (let*
        (
            (*index 0)
            (*tmp-index nil)
            (*last-index 0)
            (*length (length a7-string))
            (*result nil)
        )
        (while (and (< *index *length) (eq *result nil))
            (setq *tmp-index (string-match "\n(" a7-string *index))
            (cond
                ((eq nil *tmp-index) (setq *result *last-index))
                ((> *tmp-index a7-current-point) (setq *result *last-index))
                (t
                    (setq *index (1+ (f7-get-index-of-char a7-string (+ 2 *tmp-index) ?\))))
                    (setq *last-index *tmp-index)
                )
            )
        )
        *result
    )
)

(defun f7-get-index-of-string-end (a7-string a7-start)
    (let*
        (
            (*result nil)
            (*index a7-start)
            (*length (length a7-string))
            (l4-current-char nil)
        )
        ; (*append-to-side-bar (format "str-idx [%s] %2d/%d" a7-string a7-start *length))
        (while (and (eq *result nil) (< *index *length))
            (setq l4-current-char (elt a7-string *index))
            (cond
                ((eq l4-current-char ?\") (setq *result *index))
                ((eq l4-current-char ?\\) (setq *index (+ 2 *index)))
                (t (setq *index (1+ *index)))
            )
        )
        *result
    )
)

(法 f7-get-index-of-char (a7-string a7-start a7-char)
    (let*
        (
            (*result nil)
            (*length (length a7-string))
            (*index a7-start)
            (*tmp-index nil)
            (*break nil)
            (l4-current-char nil)
        )
        ; (*append-to-side-bar (format "chr-idx [%s] %2d/%d, %c" a7-string a7-start *length a7-char))
        (while (and (eq *result nil) (< *index *length) (eq *break nil))
            (setq l4-current-char (elt a7-string *index))
            (cond
                ((eq a7-char ?\")
                    (setq *break t)
                    (setq *tmp-index (f7-get-index-of-string-end a7-string *index))
                    (if (eq *tmp-index nil)
                        :pass
                        (setq *result *tmp-index)
                    )
                )
                ((eq l4-current-char ?\;) ; TODO Raise error when (eq a7-char ?\;)
                    (setq *tmp-index (f7-get-newline-index a7-string *index :forward))
                    (if (null *tmp-index)
                        (setq *break t)
                        (setq *index (1+ *tmp-index))
                    )
                )
                ((eq l4-current-char ?\\) (setq *index (+ 2 *index)))
                ((eq l4-current-char a7-char) (setq *result *index))
                ((eq l4-current-char ?\")
                    (setq *tmp-index (f7-get-index-of-string-end a7-string (1+ *index)))
                    (if (eq *tmp-index nil)
                        (setq *break t)
                        (setq *index (1+ *tmp-index))
                    )
                )
                ((eq l4-current-char ?\()
                    (setq *tmp-index (f7-get-index-of-char a7-string (1+ *index) ?\)))
                    (if (eq *tmp-index nil)
                        (setq *break t)
                        (setq *index (1+ *tmp-index))
                    )
                )
                (t (++ *index))
            )
        )
        *result
    )
)

(法 f7-scan-for-unpaired (a7-string)
    (let*
        (
            (*result nil)
            (*break nil)
            (*index 0)
            (*pair-index nil)
            (*length (length a7-string))
            (l4-current-char nil)
        )
        (while (and (eq *break nil) (< *index *length))
            (setq l4-current-char (elt a7-string *index))
            (cond
                ((eq l4-current-char ?\;) (setq *break t))
                ((eq l4-current-char ?\\) (setq *index (+ 2 *index)))
                ((eq l4-current-char ?\")
                    (setq *pair-index (f7-get-index-of-char a7-string (1+ *index) ?\"))
                    (if (eq *pair-index nil)
                        (progn (setq *break t) (setq *result *index))
                        (setq *index (1+ *pair-index))
                    )
                )
                ((eq l4-current-char ?\()
                    (setq *pair-index (f7-get-index-of-char a7-string (1+ *index) ?\)))
                    (if (eq *pair-index nil)
                        (progn (setq *break t) (setq *result *index))
                        (setq *index (1+ *pair-index))
                    )
                )
                (t (setq *index (1+ *index)))
            )
        )
        *result
    )
)

(法 f7-find-index (a7-string a7-cb-validate a7-start a7-direction)
    (let*
        (
            (*result nil)
            (*index a7-start)
            (l4-current-char nil)
            (*length (length a7-string))
            (*update-index nil)
            (*validate nil)
            (l4-should-continue nil)
        )
        (fset '*update-index
            (cond
                ((eq :forward a7-direction) (lambda () (setq *index (1+ *index))))
                ((eq :backward a7-direction) (lambda () (setq *index (1- *index))))
                (t (error "Direction has to be :forward or :backward."))
            )
        )
        (fset 'l4-should-continue
            (cond
                ((eq :forward a7-direction) (lambda () (< *index *length)))
                ((eq :backward a7-direction) (lambda () (> *index -1)))
            )
        )
        (fset '*validate a7-cb-validate)
        (while (and (l4-should-continue) (eq nil *result))
            (setq l4-current-char (elt a7-string *index))
            (if (*validate l4-current-char)
                (setq *result *index)
                (*update-index)
            )
        )
        *result
    )
)

(法 f7-get-newline-index (a7-string a7-start a7-direction)
    (f7-find-index a7-string (lambda (l4-current-char) (eq ?\n l4-current-char)) a7-start a7-direction)
)

(法 f7-get-space-index (a7-string a7-start a7-direction)
    (f7-find-index a7-string
        (lambda (l4-current-char) (or (eq ?\s l4-current-char) (eq ?\t l4-current-char)))
        a7-start a7-direction
    )
)

(法 f7-get-nonspace-index (a7-string a7-start a7-direction)
    (f7-find-index
        a7-string
        (lambda (l4-current-char) (not (or (eq ?\s l4-current-char) (eq ?\t l4-current-char))))
        a7-start
        a7-direction
    )
)

(法 f7-count-lines (a7-string)
    (let*
        (
            (l4-start 0)
            (*length (length a7-string))
            (l4-count 1)
            (*linebreak-index nil)
        )
        (catch :break
            (while (< l4-start *length)
                (setq *linebreak-index (string-match "\n" a7-string l4-start))
                (when (null *linebreak-index) (throw :break t))
                (++ l4-count)
                (setq l4-start (1+ *linebreak-index))
            )
        )
        l4-count
    )
)

(print "Loaded qxf-utils.")
