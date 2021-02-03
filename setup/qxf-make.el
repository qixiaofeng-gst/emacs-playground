(provide 'qxf-make)
(require 'qxf-utils)
(require 'qxf-sidebar)
(require 'seq)

(defconst qxf-mic-array-root "/home/qixiaofeng/Documents/sandbox/hachi-mic-array")
(defconst qxf-focus-record "~/.emacs.d/backup/focus-record.txt")
(defconst g5-buffers-history-record "~/.emacs.d/backup/opened-buffers-record.txt")
(defvar qxf-insertion-templates
    (list
        "f7" (f7-get-file-contents "~/.emacs.d/setup/function.template")
        "c6" (f7-get-file-contents "~/.emacs.d/setup/command.template")
        "m4" (f7-get-file-contents "~/.emacs.d/setup/macro.template")
    )
)

(defvar qxf-window-editor (frame-root-window))
(defvar qxf-window-shell-out nil)

(defvar qxf-buffer-side-bar (get-buffer-create "*side-bar*"))
(defvar qxf-window-side-bar nil)
(defvar g5-opened-buffers
    (let*
        (
            (l4-list '())
            (l4-index 0)
            (l4-path-and-point nil)
            (l4-point 0)
            (l4-path nil)
        )
        (dolist (l4-entry (f7-get-lines (f7-get-file-contents g5-buffers-history-record)))
            (setq l4-path-and-point (split-string l4-entry "\s"))
            (print l4-path-and-point)
            (if (eq 2 (length l4-path-and-point))
                (setq
                    l4-path (elt l4-path-and-point 0)
                    l4-point (elt l4-path-and-point 1)
                )
                (setq l4-path (elt l4-path-and-point 0))
            )
            (m4-insert-to-list (list l4-entry (vector :_closed_: l4-index)) l4-list)
            (++ l4-index)
        )
        l4-list
    )
)

(defvar qxf-string-cache "")
(defvar qxf-code-indent 4)

(with-current-buffer qxf-buffer-side-bar
    (f7-sidebar-mode)
)

(add-hook 'buffer-list-update-hook 'f7-update-opened-buffers)
(add-hook 'kill-emacs-hook 'qxf-record-focus)
(add-hook 'kill-emacs-hook 'f7-save-opened-buffers)

(defun f7-save-opened-buffers ()
    (with-temp-file g5-buffers-history-record
        (dolist (l4-buffer g5-opened-buffers)
            (insert (format "%s\n" (car l4-buffer)))
        )
    )
)

(defun f7-append-to-sidebar (a7-message)
    (f7-append-to-buffer a7-message qxf-buffer-side-bar)
)

(defun f7-print-to-sidebar (a7-message)
    (f7-print-to-buffer a7-message qxf-buffer-side-bar)
)

; {[function] buffer-list &optional frame}
; {[function] buffer-name &optional buffer}
; {[function] buffer-file-name &optional buffer}
; {[function] buffer-modified-p &optional buffer}
; {[macro] with-current-buffer buffer-or-name body...}
; {[function] elt sequence index}
; {[function] make-string count character &optional multibyte}
; {[Function] string-match regexp string &optional start}
; {[Function] reverse sequence}
; {[Special Form] progn forms...}
; {[Special Form] setq [symbol form]...}
; {[Macro] dotimes (var count [result]) body...}
; {add-to-list}
; ?\(, ?\)
; {[Special Form] cond (condition [body-forms...])...}
; {[Function] buffer-size &optional buffer}

(defun qxf-search-word ()
    "Currently only single line supported."
    (declare (interactive-only t))
    (interactive)
    (princ "TOOD Implement convenient searching.")
    ; (isearch-mode t t nil t)
    ; (setq isearch-new-string "form")
    ; (setq isearch-string "form")
    ; (setq isearch-new-forward t isearch-new-nonincremental nil)
    ; (isearch-repeat 'forward)
    :defun-end
)
(m4-bind "C-c s" qxf-search-word)

(defun c6-open-with-sidebar-index (a7-index)
    (interactive "nSidebar index of file to open:")
    (let*
        (
            (l4-entry (f7-find-opened-entry a7-index))
        )
        (if (null l4-entry)
            (princ (format "Invalid input:%d" a7-index))
            (find-file (elt l4-entry 0))
        )
    )
    (f7-render-sidebar)
)
(m4-bind "C-c ]" c6-open-with-sidebar-index)

(defun c6-close-with-sidebar-index ()
    (interactive)
    (princ "Command placeholder. Intend to close and remove a opened entry.")
    :defun-end
)
(m4-bind "C-c }" c6-close-with-sidebar-index)

(defun f7-find-opened-entry (a7-target-index)
    (catch :return
        (dolist (l4-entry g5-opened-buffers)
            (when (= (aref (elt l4-entry 1) 1) a7-target-index)
                (throw :return l4-entry)
            )
        )
    )
)

(defun qxf-jump-to-previous-empty-line ()
    (interactive)
    (re-search-backward "\n\n")
    (forward-char)
    :defun-end
)
(m4-bind "C-c -" qxf-jump-to-previous-empty-line)

(defun qxf-jump-to-next-empty-line ()
    (interactive)
    (re-search-forward "\n\n")
    (backward-char)
    :defun-end
)
(m4-bind "C-c _" qxf-jump-to-next-empty-line)

(defun qxf-auto-insert-parentheses ()
    (interactive)
    (let*
        (
            (l4-char (elt (buffer-substring-no-properties (point) (1+ (point))) 0))
        )
        (if (or (eq ?\s l4-char) (eq ?\) l4-char) (eq ?\( l4-char) (eq ?\n l4-char))
            (progn (insert "()") (backward-char))
            (re-search-forward "[\s\n\\)]")
            (backward-char)
            (insert ")")
            (re-search-backward "[\s\\(]")
            (forward-char)
            (insert "(")
            (re-search-forward"\)")
            (backward-char)
        )
    )
    :defun-end
)
(m4-bind "C-c (" qxf-auto-insert-parentheses)

(defun f7-auto-insert-paired (a7-pair)
    (insert a7-pair)
    (backward-char)
)

(defun qxf-auto-expand-empty-line ()
    (interactive)
    (let*
        (
            (l4-char (elt (buffer-substring-no-properties (point) (1+ (point))) 0))
        )
        (if (eq ?\n l4-char)
            (f7-auto-insert-paired "\n\n")
            (princ "[C-c C-j] could only be used on empty line.")
        )
    )
    :defun-end
)
(m4-bind "C-c C-j" qxf-auto-expand-empty-line)

(defun qxf-auto-insert-double-quotes ()
    (interactive)
    (f7-auto-insert-paired "\"\"")
    :defun-end
)
(m4-bind "C-c \"" qxf-auto-insert-double-quotes)

(defun qxf-auto-insert-brackets ()
    (interactive)
    (f7-auto-insert-paired "[]")
    :defun-end
)
(m4-bind "C-c [" qxf-auto-insert-brackets)

(defun qxf-auto-insert-braces ()
    (interactive)
    (f7-auto-insert-paired "{}")
    :defun-end
)
(m4-bind "C-c {" qxf-auto-insert-braces)

(defun qxf-kill-line ()
    (interactive)
    (beginning-of-line)
    (kill-line)
    (kill-line)
    (end-of-line)
    :defun-end
)
(m4-bind "C-c k" qxf-kill-line)

(defun qxf-backtab-trim-inner-spaces ()
    (interactive)
    (let*
        (
            (l4-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
            (l4-length (length l4-line))
            (l4-start-index (f7-get-nonspace-index l4-line 0 :forward))
            (l4-index 0)
            (l4-result "")
            (l4-f7-concat nil)
            (l4-seperator nil)
        )
        (fset 'l4-f7-concat
            (lambda (a7-string)
                (setq l4-seperator
                    (if
                        (or
                            (eq ?\) (elt a7-string 0))
                            (eq 0 (length l4-result))
                        )
                        ""
                        "\s"
                    )
                )
                (setq l4-result (concat l4-result l4-seperator a7-string))
            )
        )
        (catch :return
            (while (< l4-index l4-length)
                (setq l4-index (f7-get-space-index l4-line l4-start-index :forward))
                (when (eq nil l4-index)
                    (l4-f7-concat (substring l4-line l4-start-index l4-length))
                    (throw :return nil)
                )
                (l4-f7-concat (substring l4-line l4-start-index l4-index))
                (setq l4-index (f7-get-nonspace-index l4-line l4-index :forward))
                (when (eq nil l4-index)
                    (throw :return nil)
                )
                (setq l4-start-index l4-index)
            )
        )
        (delete-region (line-beginning-position) (line-end-position))
        (insert l4-result)
        (backward-char)
        (qxf-format-lisp)
    )
    :defun-end
)
(m4-bind "<backtab>" qxf-backtab-trim-inner-spaces)

(defun qxf-jump-to-nearest-block-start ()
    (interactive)
    (goto-char (+ 2 (f7-get-nearest-block-start (buffer-string) (- (point) 1))))
    :defun-end
)
(m4-bind "C-c b" qxf-jump-to-nearest-block-start)

(defun qxf-jump-to-nearest-block-end ()
    (interactive)
    :insert-for-test
    (let*
        (
            (l4-string (buffer-string))
            (l4-start (f7-get-nearest-block-start l4-string (point)))
        )
        (goto-char (1+ (f7-get-index-of-char l4-string (+ 3 l4-start) ?\))))
    )
    :defun-end
)
(m4-bind "C-c f" qxf-jump-to-nearest-block-end)

(defun qxf-test-scan-text ()
    (interactive)
    (f7-print-to-sidebar "qxf-test-scan-text")
    (f7-append-to-sidebar (format "====>>> %s" (f7-scan-for-unpaired "(hello\")\"=====")))
    (f7-append-to-sidebar (format "====>>> %s" (f7-scan-for-unpaired "(hello\")()(\"world)!")))
    (f7-append-to-sidebar (format "====>>> %s" (f7-scan-for-unpaired "(hello\"))")))
    (f7-append-to-sidebar (format "====>>> %s" (f7-scan-for-unpaired "(hello")))
    (f7-append-to-sidebar (format "====>>> %s" (f7-scan-for-unpaired "(hello\")\")")))
    (f7-append-to-sidebar (format "====>>> %s" (f7-scan-for-unpaired "(hello()")))
    (f7-append-to-sidebar (format "====>>> %s" (f7-scan-for-unpaired "(hello())")))
    (f7-append-to-sidebar (format "====>>> %s" (f7-scan-for-unpaired "(hello())\;")))
    (f7-append-to-sidebar (format "====>>> %s" (f7-scan-for-unpaired "(hello())\"")))
    (f7-append-to-sidebar (format "====>>> %s" (f7-scan-for-unpaired "(hello())(")))
    (f7-append-to-sidebar (format "%s" (eq 1 1)))
    (f7-append-to-sidebar (format "%s" (eq "a" "a")))
    :defun-end
)
(m4-bind "C-c t" qxf-test-scan-text)

(defun qxf-temporary-test ()
    (interactive)
    (let
        (
            (l4-to-print "Test results:\n")
            (l4-f7-test (lambda (a7-in) (f7-append-to-sidebar (format "%s|" a7-in))))
            (l4-f7-out nil)
            (l4-string nil)
            (l4-readed nil)
            (l4-readed-a nil)
            (l4-readed-b nil)
            (l4-test-map nil)
            (l4-test-list '())
            (l4-lines nil)
            (l4-cons '("a" . "b"))
        )
        (m4-initialize-outline-entry l4-test-map "hello signature" 123)
        (with-temp-buffer
            (print "this is the damn good thing" (current-buffer))
            (print "hello temp buffer" (current-buffer))
            (print '(1 2 3) (current-buffer))
            (setq l4-string (buffer-string))
            (goto-char 1)
            (setq l4-readed (read (current-buffer)))
            (setq l4-readed-a (read (current-buffer)))
            (setq l4-readed-b (read (current-buffer)))
        )
        (push l4-test-map l4-test-list)
        (fset 'l4-f7-out (lambda (a7-msg) (setq l4-to-print (format "%s%s\n" l4-to-print a7-msg))))
        (f7-print-to-sidebar "Atomic test start.")
        ; Test for property list.
        (prin1 (plist-get (elt l4-test-list 0) :line-number) qxf-buffer-side-bar)
        (print (l4-test-map :one) qxf-buffer-side-bar)
        (prin1 (car l4-cons) qxf-buffer-side-bar)
        (print (cdr l4-cons) qxf-buffer-side-bar)
        (l4-test-map :one 111)
        (prin1 (plist-get l4-test-map :one) qxf-buffer-side-bar)
        (l4-test-map :three 333)
        (print (l4-test-map :three) qxf-buffer-side-bar)
        (prin1 (l4-test-map :line-number) qxf-buffer-side-bar)
        (print (l4-test-map :signature) qxf-buffer-side-bar)
        ; Test for with-temp-buffer
        (prin1 l4-f7-test qxf-buffer-side-bar)
        (prin1 l4-string qxf-buffer-side-bar)
        (setq l4-lines (f7-get-lines l4-string))
        (dolist (l4-line l4-lines)
            (princ l4-line qxf-buffer-side-bar)
        )
        (print l4-readed qxf-buffer-side-bar)
        (prin1 l4-readed-a qxf-buffer-side-bar)
        (print l4-readed-b qxf-buffer-side-bar)
        ; Test for other things.
        (princ "test print" l4-f7-test)
        (l4-f7-out "=======")
        (l4-f7-out (m4-stringify (numberp nil)))
        (l4-f7-out (m4-stringify (numberp t)))
        (l4-f7-out (m4-stringify (type-of (type-of "hello"))))
        (l4-f7-out (m4-stringify (eq 'string (type-of "hello"))))
        (l4-f7-out (m4-stringify (string-match "\n" "teststring")))
        (l4-f7-out (format "?\\n:%s" ?\n))
        (l4-f7-out (format "?\\(:%s" ?\())
        (l4-f7-out (format "?\\):%s" ?\)))
        (l4-f7-out (format "?\\\":%s" ?\"))
        (l4-f7-out (format "?\\\\:%s" ?\\))
        (l4-f7-out (format "%s" (point)))
        (l4-f7-out (m4-stringify (eq :test-const (read ":test-const"))))
        (l4-f7-out (m4-stringify (eq 'abc (make-symbol "abc"))))
        (l4-f7-out (m4-stringify (equal "abc" "abc")))
        (f7-append-to-sidebar l4-to-print)
    )
    :defun-end
)
(m4-bind "C-c t" qxf-temporary-test)

(defun qxf-create-newline ()
    (interactive)
    (let*
        (
            (l4-limit (f7-get-nearest-block-start (buffer-string) (point)))
        )
        (insert "\n")
        (qxf-format-lisp)
        (forward-line 2)
        (if (eq nil (re-search-backward "\n\s+\n" l4-limit t))
            (forward-line -2)
            (forward-char)
        )
        (end-of-line)
    )
    :defun-end
)
(m4-bind "C-c j" qxf-create-newline)

(defun f7-format-form (a7-string-form &optional a7-indent)
    (when (eq nil a7-indent)
        (setq a7-indent 0)
    )
    (setq a7-string-form (f7-trim-string a7-string-form))
    (let*
        (
            (l4-indent-string (make-string (* a7-indent qxf-code-indent) ?\s))
            (l4-first-newline-index (f7-get-newline-index a7-string-form 0 :forward))
            (l4-unpaired-index nil)
            (l4-unpaired-char nil)
            (l4-close-bracket-index nil)
            (l4-close-newline-index nil)
            (l4-close-line nil)
            (l4-nonspace-index nil)
            (l4-rest-string nil)
            (l4-inner-string nil)
            (l4-length (length a7-string-form))
            (l4-first-line nil)
        )
        (catch :return
            (when (eq nil l4-first-newline-index)
                (throw :return (concat l4-indent-string a7-string-form))
            )
            (setq l4-first-line (substring a7-string-form 0 l4-first-newline-index))
            (setq l4-unpaired-index (f7-scan-for-unpaired l4-first-line))
            (when (eq nil l4-unpaired-index)
                (throw :return
                    (concat
                        l4-indent-string l4-first-line "\n"
                        (f7-format-form (substring a7-string-form (1+ l4-first-newline-index) l4-length) a7-indent)
                    )
                )
            )
            (setq l4-unpaired-char (elt a7-string-form l4-unpaired-index))
            (when (not (eq l4-unpaired-char ?\())
                (f7-append-to-sidebar (format "%c <<<" l4-unpaired-char))
                (throw :return
                    (concat l4-indent-string "\; ERROR Only unpaired \"(\" allowed.\n" a7-string-form)
                )
            )
            (setq l4-close-bracket-index (f7-get-index-of-char a7-string-form (1+ l4-unpaired-index) ?\)))
            (when (eq nil l4-close-bracket-index)
                (throw :return
                    (concat l4-indent-string "\; ERROR Missing close bracket.\n" a7-string-form)
                )
            )
            (setq l4-close-newline-index (f7-get-newline-index a7-string-form l4-close-bracket-index :backward))
            (setq l4-close-line (substring a7-string-form (1+ l4-close-newline-index) l4-close-bracket-index))
            (setq l4-nonspace-index (f7-get-nonspace-index l4-close-line 0 :forward))
            (setq l4-rest-string (f7-trim-string (substring a7-string-form (1+ l4-close-bracket-index) l4-length)))
            (when (not (eq 0 (length l4-rest-string)))
                (if (string-prefix-p "\n" l4-rest-string)
                    (progn
                        (setq l4-rest-string
                            (f7-format-form (substring l4-rest-string 1 (length l4-rest-string)) a7-indent)
                        )
                        (setq l4-rest-string (concat "\n" l4-rest-string))
                    )
                    (setq l4-rest-string (f7-format-form l4-rest-string a7-indent))
                )
            )
            (setq l4-inner-string
                (if (eq nil l4-nonspace-index)
                    (substring a7-string-form
                        (if (eq l4-first-newline-index l4-close-newline-index)
                            l4-first-newline-index
                            (1+ l4-first-newline-index)
                        )
                        l4-close-newline-index
                    )
                    (substring a7-string-form (1+ l4-first-newline-index) l4-close-bracket-index)
                )
            )
            (concat
                l4-indent-string l4-first-line "\n"
                (f7-format-form l4-inner-string (1+ a7-indent))
                "\n" l4-indent-string ")" l4-rest-string
            )
        )
    )
)

(defun qxf-format-lisp ()
    (interactive)
    (let*
        (
            (l4-string (buffer-substring-no-properties 1 (buffer-size)))
            (l4-point-o (point))
            (l4-point-a (1+ (f7-get-nearest-block-start l4-string l4-point-o)))
            (l4-point-b (1+ (f7-get-index-of-char l4-string (1+ l4-point-a) ?\))))
            (l4-block (substring l4-string l4-point-a l4-point-b))
            (l4-formatted (f7-format-form l4-block))
        )
        (if (string-equal l4-block l4-formatted)
            (princ "Current block is already pretty enough.")
            (delete-region (1+ l4-point-a) (1+ l4-point-b))
            (insert l4-formatted)
            (goto-char l4-point-o)
            (princ "Formatted current block.")
        )
    )
    :defun-end
)
(m4-bind "C-c q" qxf-format-lisp)

(defun qxf-duplicate-line ()
    (interactive)
    (let
        (
            (l4-point-a nil)
            (l4-point-b nil)
        )
        (beginning-of-line)
        (setq l4-point-a (point))
        (re-search-forward "\n")
        (setq l4-point-b (point))
        (insert (buffer-substring-no-properties l4-point-a l4-point-b))
        (forward-line -1)
        (end-of-line)
    )
    :defun-end
)
(m4-bind "C-c d" qxf-duplicate-line)

(defun qxf-focus-line-beginning ()
    (interactive)
    (beginning-of-line)
    (goto-char (1- (re-search-forward "[^[:space:]]")))
    (if (string= "(" (buffer-substring-no-properties (point) (1+ (point))))
        (forward-char)
    )
    :defun-end
)
(m4-bind "C-c a" qxf-focus-line-beginning)

(defmacro m4-cut-current-line (a7-variable)
    `(let*
        (
            (l4-point (point))
            (l4-start-point (line-beginning-position))
            (l4-offset (- l4-point l4-start-point))
            (l4-string
                (delete-and-extract-region l4-start-point (1+ (line-end-position)))
            )
        )
        (setq ,a7-variable (list :offset l4-offset :string l4-string))
        (m4-make-object-oriented-like ,a7-variable)
    )
)

(defun qxf-move-line-down ()
    (interactive)
    (let*
        (
            (l4-line nil)
        )
        (m4-cut-current-line l4-line)
        (forward-line 1)
        (insert (l4-line :string))
        (forward-line -1)
        (forward-char (l4-line :offset))
    )
    :defun-end
)
(m4-bind "C-c n" qxf-move-line-down)

(defun qxf-move-line-up ()
    (interactive)
    (let
        (
            (l4-line nil)
        )
        (m4-cut-current-line l4-line)
        (forward-line -1)
        (insert (l4-line :string))
        (forward-line -1)
        (forward-char (l4-line :offset))
    )
    :defun-end
)
(m4-bind "C-c p" qxf-move-line-up)

(defun qxf-copy-region ()
    (interactive)
    (setq qxf-string-cache (buffer-substring (region-beginning) (region-end)))
    (deactivate-mark t)
)
(m4-bind "C-c c" qxf-copy-region)

(defun qxf-paste ()
    (interactive)
    (insert qxf-string-cache)
)
(m4-bind "C-c y" qxf-paste)

(defun f7-record-current-buffer (a7-buffer a7-point)
    (let*
        (
            (l4-to-save (format "%s\n%d\n" (buffer-file-name a7-buffer) a7-point))
        )
        (with-temp-file qxf-focus-record (insert l4-to-save))
    )
)

(defun qxf-record-focus ()
    (interactive)
    (f7-record-current-buffer (current-buffer) (point))
    (princ "Recorded current focus.")
)
(m4-bind "C-c DEL" qxf-record-focus)

(defun qxf-load-record ()
    (interactive)
    (let*
        (
            (l4-lines (f7-get-lines (f7-get-file-contents qxf-focus-record)))
            (l4-path-to-open (elt l4-lines 0))
            (l4-point (string-to-number (elt l4-lines 1)))
        )
        (qxf-focus-editor)
        (find-file l4-path-to-open)
        (goto-char l4-point)
        (princ "Loaded focus record.")
    )
)
(m4-bind "C-c =" qxf-load-record)

(defun qxf-focus-editor ()
    (interactive)
    (select-window qxf-window-editor)
    (f7-render-sidebar)
)
(m4-bind "C-c e" qxf-focus-editor)

(defun f7-list-outmost-blocks ()
    (let*
        (
            (l4-list '())
            (l4-entry nil)
            (l4-start-index 0)
            (l4-end-index 0)
            (l4-string (buffer-substring-no-properties (point-min) (point-max)))
            (l4-length (length l4-string))
        )
        (catch :break
            (while (< l4-start-index l4-length)
                (setq l4-start-index (f7-get-index-of-char l4-string l4-start-index ?\())
                (when (null l4-start-index) (throw :break t))
                (setq l4-end-index (f7-get-index-of-char l4-string (1+ l4-start-index) ?\)))
                (when (null l4-end-index)
                    (princ "Lisp code is broken.")
                    (throw :break t)
                )
                (push 
                    (m4-initialize-outline-entry l4-entry
                        (substring l4-string l4-start-index (1+ l4-end-index))
                        (f7-count-lines (substring l4-string 0 l4-start-index))
                    )
                    l4-list
                )
                (setq l4-start-index (1+ l4-end-index))
            )
        )
        (setq l4-list (reverse l4-list))
        l4-list
    )
)

(defun f7-parse-defined-names (a7-list)
    (let*
        (
            (l4-signature nil)
            (l4-newline-index nil)
            (l4-start-index nil)
            (l4-end-index nil)
            (l4-filtered-list nil)
        )
        (dolist (l4-block a7-list)
            (m4-make-object-oriented-like l4-block)
            (setq l4-signature (l4-block :signature))
            (setq l4-newline-index (f7-get-newline-index l4-signature 0 :forward))
            (setq l4-signature
                (if (null l4-newline-index)
                    l4-signature
                    (substring l4-signature 0 l4-newline-index)
                )
            )
            (when (string-prefix-p "(def" l4-signature)
                (setq l4-start-index (f7-get-space-index l4-signature 0 :forward))
                (if (null l4-start-index)
                    (setq l4-signature "**check-code**")
                    (++ l4-start-index)
                    (setq l4-end-index (f7-get-space-index l4-signature l4-start-index :forward))
                    (setq l4-signature (substring l4-signature l4-start-index l4-end-index))
                )
                (l4-block :signature l4-signature)
                (push l4-block l4-filtered-list)
            )
        )
        (setq l4-filtered-list
            (seq-sort-by
                (lambda (a7-entry) (plist-get a7-entry :signature))
                #'string<
                l4-filtered-list
            )
        )
        (dolist (l4-entry l4-filtered-list)
            (f7-append-to-sidebar
                (format
                    "%s [%d]"
                    (plist-get l4-entry :signature)
                    (plist-get l4-entry :line-number)
                )
            )
        )
    )
)

(defun qxf-render-lisp-outline ()
    (interactive)
    (f7-print-to-sidebar "所有定义：")
    (f7-parse-defined-names (f7-list-outmost-blocks))
)
(m4-bind "C-c |" qxf-render-lisp-outline)

(defun qxf-focus-side-bar ()
    (interactive)
    (f7-render-sidebar)
)
(m4-bind "C-c \\" qxf-focus-side-bar)

(defun qxf-make-mic-array ()
    (interactive)
    (shell-command (format "date && cd %s/build && make -j 8 && ./listdevs" qxf-mic-array-root))
)
(m4-bind "C-c 1" qxf-make-mic-array)

(defun qxf-cmake-mic-array ()
    (interactive)
    (shell-command (format "date && cd %s/build && cmake .." qxf-mic-array-root))
    :defun-end
)
(m4-bind "C-c 2" qxf-cmake-mic-array)

(defun c6-toggle-debug-error ()
    (interactive)
    (if (equal debug-on-error t)
        (setq debug-on-error nil)
        (setq debug-on-error t)
    )
    :defun-end
)
(define-key global-map (kbd "C-c 3") 'c6-toggle-debug-error)

(defun qxf-set-c-offset ()
    (interactive)
    (set-variable 'c-basic-offset 4)
)
(m4-bind "C-c 4" qxf-set-c-offset)

(defun qxf-layout-3-pane ()
    (interactive)
    (qxf-focus-editor)
    (delete-other-windows)
    ; (find-file (format "%s/src/listdevs.c" qxf-mic-array-root))
    (shell-command "echo Make shell area.")
    (setq qxf-window-shell-out (split-window nil -20 'below))
    (set-window-buffer qxf-window-shell-out "*Shell Command Output*")
    (setq qxf-window-side-bar
        (split-window nil (+ 120 (f7-get-line-number-width))
            'left
        )
    )
    (set-window-buffer qxf-window-side-bar qxf-buffer-side-bar)
    (f7-render-sidebar)
    (shell-command "echo Initialized shell area.")
)
(m4-bind "C-c 0" qxf-layout-3-pane)

(defun qxf-layout-2-pane ()
    (interactive)
    (qxf-focus-editor)
    (delete-other-windows)
    (setq qxf-window-side-bar
        (split-window nil (+ 120 (f7-get-line-number-width))
            'left
        )
    )
    (set-window-buffer qxf-window-side-bar qxf-buffer-side-bar)
    (f7-render-sidebar)
    :defun-end
)
(m4-bind "C-c 9" qxf-layout-2-pane)

(defun qxf-insert-command (a7-template-type a7-command-name)
    (interactive "sTemplate-type:\nsTarget-name:")
    (insert
        (format
            (lax-plist-get qxf-insertion-templates a7-template-type)
            a7-command-name a7-command-name
        )
    )
    :defun-end
)
(m4-bind "C-c i" qxf-insert-command)

(defun f7-get-line-number-width ()
    (+ 2 (length (format "%d" (count-lines (point-min) (point-max)))))
)

(defmacro m4-remove-association (a7-key a7-alist)
    `(setq ,a7-alist (assoc-delete-all ,a7-key ,a7-alist))
)

(defun f7-update-opened-buffers ()
    (let*
        (
            (l4-name nil)
            (l4-index 0)
        )
        (dolist (l4-buffer (buffer-list))
            (setq l4-name (buffer-file-name l4-buffer))
            (when (stringp l4-name)
                (m4-remove-association l4-name g5-opened-buffers)
                (m4-insert-to-list (list l4-name (vector l4-buffer l4-index)) g5-opened-buffers)
                (++ l4-index)
            )
        )
        (setq g5-opened-buffers
            (seq-sort-by (lambda (l4-entry) (elt l4-entry 0)) #'string< g5-opened-buffers)
        )
        (setq l4-index 0)
        (dolist (l4-entry g5-opened-buffers)
            (aset (elt l4-entry 1) 1 l4-index)
            (++ l4-index)
        )
    )
)

(defun f7-render-sidebar ()
    (with-current-buffer qxf-buffer-side-bar
        (let ((buffer-read-only nil))
            (erase-buffer)
            (insert (format "%s\n" (current-time-string)))
            (dolist (l4-pair g5-opened-buffers)
                (f7-render-buffer-entry l4-pair)
            )
        )
    )
)

(defun f7-render-buffer-entry (a7-pair)
    (let*
        (
            (l4-values (elt a7-pair 1))
            (l4-buffer (aref l4-values 0))
            (l4-buffer-name
                (if (eq :_closed_: l4-buffer) (symbol-name :_closed_:)
                    (if (null (buffer-live-p l4-buffer))
                        (symbol-name :_killed_:)
                        (buffer-name l4-buffer)
                    )
                )
            )
            (l4-file-path (car a7-pair))
            (l4-directory-path (file-name-directory l4-file-path))
        )
        ; (message (format "While rendering buffer: %s" l4-buffer))
        (insert
            (format "[%-16s | %32s | %3d]\n"
                (f7-clamp-string l4-buffer-name 16)
                (f7-clamp-string
                    (if
                        (or
                            (equal l4-buffer-name (symbol-name :_closed_:))
                            (equal l4-buffer-name (symbol-name :_killed_:))
                        )
                        l4-file-path
                        l4-directory-path
                    )
                    32 t
                )
                (aref l4-values 1)
            )
        )
    )
    :end-defun
)

(defun f7-clamp-string (a7-string a7-width &optional a7-is-left)
    (let*
        (
            (l4-length (length a7-string))
        )
        (if (> l4-length a7-width)
            (if a7-is-left
                (concat "..." (substring a7-string (- l4-length (- a7-width 3))))
                (concat (substring a7-string 0 (- a7-width 3)) "...")
            )
            a7-string
        )
    )
)

(print "Loaded qxf-make.")

