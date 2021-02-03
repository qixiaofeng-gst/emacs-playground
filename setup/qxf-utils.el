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
        (lambda (a7-key &optional a7-value)
            (if (eq nil a7-value)
                (plist-get ,a7-object a7-key)
                (plist-put ,a7-object a7-key a7-value)
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
            (l4-out-string "")
            (l4-f7-receive
                (lambda (a7-char)
                    (if (eq a7-char ?\n)
                        (setq l4-out-string (format "%s\\n" l4-out-string))
                        (setq l4-out-string (format "%s%c" l4-out-string a7-char))
                    )
                )
            )
        )
        (prin1 (quote ,a7-target) l4-f7-receive)
        l4-out-string
    )
)

(法 f7-get-lines (a7-string)
    (let*
        (
            (l4-lines '())
            (l4-length (length a7-string))
            (l4-start-index 0)
            (l4-end-index nil)
        )
        (catch :break
            (while (< l4-start-index l4-length)
                (setq l4-end-index (f7-get-newline-index a7-string l4-start-index :forward))
                (when (null l4-end-index)
                    (push (substring a7-string l4-start-index) l4-lines)
                    (throw :break t)
                )
                (push (substring a7-string l4-start-index l4-end-index) l4-lines)
                (setq l4-start-index (1+ l4-end-index))
            )
        )
        (reverse l4-lines)
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
            (l4-length (length a7-string))
            (l4-first-nonspace-index (f7-get-nonspace-index a7-string 0 :forward))
            (l4-last-nonspace-index (f7-get-nonspace-index a7-string (1- l4-length) :backward))
        )
        (if (eq nil l4-first-nonspace-index)
            ""
            (substring a7-string l4-first-nonspace-index (1+ l4-last-nonspace-index))
        )
    )
)

(法 f7-get-nearest-block-start (a7-string a7-current-point)
    (let*
        (
            (l4-index 0)
            (l4-temporary-index nil)
            (l4-last-index 0)
            (l4-length (length a7-string))
            (l4-result nil)
        )
        (while (and (< l4-index l4-length) (eq l4-result nil))
            (setq l4-temporary-index (string-match "\n(" a7-string l4-index))
            (cond
                ((eq nil l4-temporary-index) (setq l4-result l4-last-index))
                ((> l4-temporary-index a7-current-point) (setq l4-result l4-last-index))
                (t
                    (setq l4-index (1+ (f7-get-index-of-char a7-string (+ 2 l4-temporary-index) ?\))))
                    (setq l4-last-index l4-temporary-index)
                )
            )
        )
        l4-result
    )
)

(defun f7-get-index-of-string-end (a7-string a7-start)
    (let*
        (
            (l4-result nil)
            (l4-index a7-start)
            (l4-length (length a7-string))
            (l4-current-char nil)
        )
        (while (and (eq l4-result nil) (< l4-index l4-length))
            (setq l4-current-char (elt a7-string l4-index))
            (cond
                ((eq l4-current-char ?\") (setq l4-result l4-index))
                ((eq l4-current-char ?\\) (setq l4-index (+ 2 l4-index)))
                (t (setq l4-index (1+ l4-index)))
            )
        )
        l4-result
    )
)

(法 f7-get-index-of-char (a7-string a7-start a7-char)
    (let*
        (
            (l4-result nil)
            (l4-length (length a7-string))
            (l4-index a7-start)
            (l4-temporary-index nil)
            (l4-break nil)
            (l4-current-char nil)
        )
        (while (and (eq l4-result nil) (< l4-index l4-length) (eq l4-break nil))
            (setq l4-current-char (elt a7-string l4-index))
            (cond
                ((eq a7-char ?\")
                    (setq l4-break t)
                    (setq l4-temporary-index (f7-get-index-of-string-end a7-string l4-index))
                    (if (eq l4-temporary-index nil)
                        :pass
                        (setq l4-result l4-temporary-index)
                    )
                )
                ((eq l4-current-char ?\;) ; TODO Raise error when (eq a7-char ?\;)
                    (setq l4-temporary-index (f7-get-newline-index a7-string l4-index :forward))
                    (if (null l4-temporary-index)
                        (setq l4-break t)
                        (setq l4-index (1+ l4-temporary-index))
                    )
                )
                ((eq l4-current-char ?\\) (setq l4-index (+ 2 l4-index)))
                ((eq l4-current-char a7-char) (setq l4-result l4-index))
                ((eq l4-current-char ?\")
                    (setq l4-temporary-index (f7-get-index-of-string-end a7-string (1+ l4-index)))
                    (if (eq l4-temporary-index nil)
                        (setq l4-break t)
                        (setq l4-index (1+ l4-temporary-index))
                    )
                )
                ((eq l4-current-char ?\()
                    (setq l4-temporary-index (f7-get-index-of-char a7-string (1+ l4-index) ?\)))
                    (if (eq l4-temporary-index nil)
                        (setq l4-break t)
                        (setq l4-index (1+ l4-temporary-index))
                    )
                )
                (t (++ l4-index))
            )
        )
        l4-result
    )
)

(法 f7-scan-for-unpaired (a7-string)
    (let*
        (
            (l4-result nil)
            (l4-break nil)
            (l4-index 0)
            (l4-pair-index nil)
            (l4-length (length a7-string))
            (l4-current-char nil)
        )
        (while (and (eq l4-break nil) (< l4-index l4-length))
            (setq l4-current-char (elt a7-string l4-index))
            (cond
                ((eq l4-current-char ?\;) (setq l4-break t))
                ((eq l4-current-char ?\\) (setq l4-index (+ 2 l4-index)))
                ((eq l4-current-char ?\")
                    (setq l4-pair-index (f7-get-index-of-char a7-string (1+ l4-index) ?\"))
                    (if (eq l4-pair-index nil)
                        (progn (setq l4-break t) (setq l4-result l4-index))
                        (setq l4-index (1+ l4-pair-index))
                    )
                )
                ((eq l4-current-char ?\()
                    (setq l4-pair-index (f7-get-index-of-char a7-string (1+ l4-index) ?\)))
                    (if (eq l4-pair-index nil)
                        (progn (setq l4-break t) (setq l4-result l4-index))
                        (setq l4-index (1+ l4-pair-index))
                    )
                )
                (t (setq l4-index (1+ l4-index)))
            )
        )
        l4-result
    )
)

(法 f7-find-index (a7-string a7-cb-validate a7-start a7-direction)
    (let*
        (
            (l4-result nil)
            (l4-index a7-start)
            (l4-current-char nil)
            (l4-length (length a7-string))
            (l4-update-index nil)
            (l4-f7-validate nil)
            (l4-should-continue nil)
        )
        (fset 'l4-update-index
            (cond
                ((eq :forward a7-direction) (lambda () (setq l4-index (1+ l4-index))))
                ((eq :backward a7-direction) (lambda () (setq l4-index (1- l4-index))))
                (t (error "Direction has to be :forward or :backward."))
            )
        )
        (fset 'l4-should-continue
            (cond
                ((eq :forward a7-direction) (lambda () (< l4-index l4-length)))
                ((eq :backward a7-direction) (lambda () (> l4-index -1)))
            )
        )
        (fset 'l4-f7-validate a7-cb-validate)
        (while (and (l4-should-continue) (eq nil l4-result))
            (setq l4-current-char (elt a7-string l4-index))
            (if (l4-f7-validate l4-current-char)
                (setq l4-result l4-index)
                (l4-update-index)
            )
        )
        l4-result
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
            (l4-length (length a7-string))
            (l4-count 1)
            (l4-linebreak-index nil)
        )
        (catch :break
            (while (< l4-start l4-length)
                (setq l4-linebreak-index (string-match "\n" a7-string l4-start))
                (when (null l4-linebreak-index) (throw :break t))
                (++ l4-count)
                (setq l4-start (1+ l4-linebreak-index))
            )
        )
        l4-count
    )
)

(print "Loaded qxf-utils.")
