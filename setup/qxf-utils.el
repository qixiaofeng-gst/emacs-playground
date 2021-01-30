(provide 'qxf-utils)

(defalias '曰 'defalias)

(曰 '道 'defmacro)
(曰 '法 'defun)
(曰 '自 'defvar)
(曰 '然 'setq)

; {[Macro] defmacro name args [doc] [declare] body...}

(道 m4-bind (*keys *command)
    `(define-key global-map (kbd ,*keys) (quote ,*command))
)

(道 ++ (*number &optional *increment)
    (if (eq nil *increment)
        `(setq ,*number (1+ ,*number))
        `(setq ,*number (+ ,*increment ,*number))
    )
)

(道 *make-object-oriented-like (*object)
    `(fset (quote ,*object)
        (lambda (*key &optional *value)
            (if (eq nil *value)
                (plist-get ,*object *key)
                (plist-put ,*object *key *value)
            )
        )
    )
)

(道 m4-insert-to-list (l4-value l4-list)
    `(setq ,l4-list (push ,l4-value ,l4-list))
)

(道 *init-outline-entry (*object *signature *line-number)
    `(progn
        (*make-object-oriented-like ,*object)
        (setq ,*object (list :signature ,*signature :line-number ,*line-number))
    )
)

(defmacro qxf-*-stringify (*target)
    `(format "%s:%s" (qxf-*-print ,*target) ,*target)
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

(法 *get-lines (*string)
    (let*
        (
            (*lines '())
            (*length (length *string))
            (*start-index 0)
            (*end-index nil)
        )
        (catch :break
            (while (< *start-index *length)
                (setq *end-index (*get-newline-index *string *start-index :forward))
                (when (null *end-index)
                    (push (substring *string *start-index) *lines)
                    (throw :break t)
                )
                (push (substring *string *start-index *end-index) *lines)
                (setq *start-index (1+ *end-index))
            )
        )
        (reverse *lines)
    )
)

(defun f7-get-file-contents (*file-path)
    (with-temp-buffer
        (insert-file-contents *file-path)
        (buffer-substring-no-properties (point-min) (point-max))
    )
)

(defun *append-to-buffer (*message *buffer)
    (with-current-buffer *buffer
        (insert (format "%s\n" *message))
    )
)

(defun *print-to-buffer (*message *buffer)
    (with-current-buffer *buffer
        (erase-buffer)
        (insert (format "%s\n" (current-time-string)))
        (*append-to-buffer *message *buffer)
    )
)

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

(defun *get-nearest-block-start (*string *current-point)
    (let*
        (
            (*index 0)
            (*tmp-index nil)
            (*last-index 0)
            (*length (length *string))
            (*result nil)
        )
        (while (and (< *index *length) (eq *result nil))
            (setq *tmp-index (string-match "\n(" *string *index))
            (cond
                ((eq nil *tmp-index) (setq *result *last-index))
                ((> *tmp-index *current-point) (setq *result *last-index))
                (t
                    (setq *index (1+ (*get-index-of-char *string (+ 2 *tmp-index) ?\))))
                    (setq *last-index *tmp-index)
                )
            )
        )
        *result
    )
)

(defun *get-index-of-string-end (*string *start)
    (let*
        (
            (*result nil)
            (*index *start)
            (*length (length *string))
            (*cc nil)
        )
        ; (*append-to-side-bar (format "str-idx [%s] %2d/%d" *string *start *length))
        (while (and (eq *result nil) (< *index *length))
            (setq *cc (elt *string *index))
            (cond
                ((eq *cc ?\") (setq *result *index))
                ((eq *cc ?\\) (setq *index (+ 2 *index)))
                (t (setq *index (1+ *index)))
            )
        )
        *result
    )
)

(defun *get-index-of-char (*string *start *c)
    (let*
        (
            (*result nil)
            (*length (length *string))
            (*index *start)
            (*tmp-index nil)
            (*break nil)
            (*cc nil)
        )
        ; (*append-to-side-bar (format "chr-idx [%s] %2d/%d, %c" *string *start *length *c))
        (while (and (eq *result nil) (< *index *length) (eq *break nil))
            (setq *cc (elt *string *index))
            (cond
                ((eq *c ?\")
                    (setq *break t)
                    (setq *tmp-index (*get-index-of-string-end *string *index))
                    (if (eq *tmp-index nil)
                        :pass
                        (setq *result *tmp-index)
                    )
                )
                ((eq *cc ?\;) ; TODO Raise error when (eq *c ?\;)
                    (setq *tmp-index (*get-newline-index *string *index :forward))
                    (if (null *tmp-index)
                        (setq *break t)
                        (setq *index (1+ *tmp-index))
                    )
                )
                ((eq *cc ?\\) (setq *index (+ 2 *index)))
                ((eq *cc *c) (setq *result *index))
                ((eq *cc ?\")
                    (setq *tmp-index (*get-index-of-string-end *string (1+ *index)))
                    (if (eq *tmp-index nil)
                        (setq *break t)
                        (setq *index (1+ *tmp-index))
                    )
                )
                ((eq *cc ?\()
                    (setq *tmp-index (*get-index-of-char *string (1+ *index) ?\)))
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

(defun *scan-for-unpaired (*string)
    (let*
        (
            (*result nil)
            (*break nil)
            (*index 0)
            (*pair-index nil)
            (*length (length *string))
            (*cc nil)
        )
        (while (and (eq *break nil) (< *index *length))
            (setq *cc (elt *string *index))
            (cond
                ((eq *cc ?\;) (setq *break t))
                ((eq *cc ?\\) (setq *index (+ 2 *index)))
                ((eq *cc ?\")
                    (setq *pair-index (*get-index-of-char *string (1+ *index) ?\"))
                    (if (eq *pair-index nil)
                        (progn (setq *break t) (setq *result *index))
                        (setq *index (1+ *pair-index))
                    )
                )
                ((eq *cc ?\()
                    (setq *pair-index (*get-index-of-char *string (1+ *index) ?\)))
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

(defun *find-index (*string *cb-validate *start *direction)
    (let*
        (
            (*result nil)
            (*index *start)
            (*cc nil)
            (*length (length *string))
            (*update-index nil)
            (*validate nil)
            (*continue nil)
        )
        (fset '*update-index
            (cond
                ((eq :forward *direction) (lambda () (setq *index (1+ *index))))
                ((eq :backward *direction) (lambda () (setq *index (1- *index))))
                (t (error "Direction has to be :forward or :backward."))
            )
        )
        (fset '*continue
            (cond
                ((eq :forward *direction) (lambda () (< *index *length)))
                ((eq :backward *direction) (lambda () (> *index -1)))
            )
        )
        (fset '*validate *cb-validate)
        (while (and (*continue) (eq nil *result))
            (setq *cc (elt *string *index))
            (if (*validate *cc)
                (setq *result *index)
                (*update-index)
            )
        )
        *result
    )
)

(defun *get-newline-index (*string *start *direction)
    (*find-index *string (lambda (*cc) (eq ?\n *cc)) *start *direction)
)

(defun *get-space-index (*string *start *direction)
    (*find-index *string
        (lambda (*cc) (or (eq ?\s *cc) (eq ?\t *cc)))
        *start *direction
    )
)

(defun *get-nonspace-index (*string *start *direction)
    (*find-index
        *string
        (lambda (*cc) (not (or (eq ?\s *cc) (eq ?\t *cc))))
        *start
        *direction
    )
)

(defun *count-lines (*string)
    (let*
        (
            (*start 0)
            (*length (length *string))
            (*count 1)
            (*linebreak-index nil)
        )
        (catch :break
            (while (< *start *length)
                (setq *linebreak-index (string-match "\n" *string *start))
                (when (null *linebreak-index) (throw :break t))
                (++ *count)
                (setq *start (1+ *linebreak-index))
            )
        )
        *count
    )
)

(print "Loaded qxf-utils.")
