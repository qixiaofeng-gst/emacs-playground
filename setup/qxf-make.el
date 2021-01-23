(provide 'qxf-make)
(require 'qxf-utils)
(require 'qxf-sidebar)
(require 'seq)

(defconst qxf-mic-array-root "/home/qixiaofeng/Documents/sandbox/hachi-mic-array")
(defconst qxf-focus-record "~/.emacs.d/backup/focus-record.txt")
(defconst g5-opened-buffers-record "~/.emacs.d/backup/opened-buffers-record.txt")
(defvar qxf-insertion-templates
    (list
        "f7" (*get-file-contents "~/.emacs.d/setup/function.template")
        "c6" (*get-file-contents "~/.emacs.d/setup/command.template")
        "m4" (*get-file-contents "~/.emacs.d/setup/macro.template")
    )
)

(defvar qxf-window-editor (frame-root-window))
(defvar qxf-window-shell-out nil)

(defvar qxf-buffer-side-bar (get-buffer-create "*side-bar*"))
(defvar qxf-window-side-bar nil)
(defvar g5-opened-buffers
    (let*
        ((l4-list '()))
        (dolist (l4-entry (*get-lines (*get-file-contents g5-opened-buffers-record)))
	    (m4-insert-to-list (cons l4-entry :__:) l4-list)
        )
        l4-list
    )
)

(defvar qxf-string-cache "")
(defvar qxf-code-indent 4)

(with-current-buffer qxf-buffer-side-bar
    (setq display-line-numbers t)
    (f7-sidebar-mode)
)

(add-hook 'buffer-list-update-hook 'f7-update-opened-buffers)
(add-hook 'kill-emacs-hook 'qxf-record-focus)
(add-hook 'kill-emacs-hook 'f7-save-opened-buffers)

(defun f7-save-opened-buffers ()
    (with-temp-file g5-opened-buffers-record
        (dolist (l4-buffer g5-opened-buffers)
            (insert (format "%s\n" (car l4-buffer)))
        )
    )
)

(defun *append-to-side-bar (*message)
    (*append-to-buffer *message qxf-buffer-side-bar)
)

(defun *print-to-side-bar (*message)
    (*print-to-buffer *message qxf-buffer-side-bar)
)

; {[function] buffer-list &optional frame}
; {[function] buffer-name &optional buffer}
; {[function] buffer-file-name &optional buffer}
; {[function] buffer-modified-p &optional buffer}
; {[macro] with-current-buffer buffer-or-name body...}

; Align the outmost brackets pair.
; TODO Validate the outmost begin line. 
;      1. Only one open "(" allowed.
;      2. If not valid, print message and jump to the second open "(".
; TODO Align the 2, 3, ... brackets pair.
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

(defun qxf-search-word
    ()
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

(defun qxf-jump-to-previous-empty-line
    ()
    (interactive)
    (re-search-backward "\n\n")
    (forward-char)
    :defun-end
)
(m4-bind "C-c -" qxf-jump-to-previous-empty-line)

(defun qxf-jump-to-next-empty-line
    ()
    (interactive)
    (re-search-forward "\n\n")
    (backward-char)
    :defun-end
)
(m4-bind "C-c _" qxf-jump-to-next-empty-line)

(defun qxf-auto-insert-parentheses
    ()
    (interactive)
    (let*
        (
            (*char (elt (buffer-substring-no-properties (point) (1+ (point))) 0))
        )
        (if (or (eq ?\s *char) (eq ?\) *char) (eq ?\( *char) (eq ?\n *char))
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

(defun *auto-insert-paired (*pair)
    (insert *pair)
    (backward-char)
)

(defun qxf-auto-expand-empty-line
    ()
    (interactive)
    (let*
        (
            (*char (elt (buffer-substring-no-properties (point) (1+ (point))) 0))
        )
        (if (eq ?\n *char)
            (*auto-insert-paired "\n\n")
            (princ "[C-c C-j] could only be used on empty line.")
        )
    )
    :defun-end
)
(m4-bind "C-c C-j" qxf-auto-expand-empty-line)

(defun qxf-auto-insert-double-quotes
    ()
    (interactive)
    (*auto-insert-paired "\"\"")
    :defun-end
)
(m4-bind "C-c \"" qxf-auto-insert-double-quotes)

(defun qxf-auto-insert-brackets
    ()
    (interactive)
    (*auto-insert-paired "[]")
    :defun-end
)
(m4-bind "C-c [" qxf-auto-insert-brackets)

(defun qxf-auto-insert-braces
    ()
    (interactive)
    (*auto-insert-paired "{}")
    :defun-end
)
(m4-bind "C-c {" qxf-auto-insert-braces)

(defun qxf-kill-line
    ()
    (interactive)
    (beginning-of-line)
    (kill-line)
    (kill-line)
    (end-of-line)
    :defun-end
)
(m4-bind "C-c k" qxf-kill-line)

(defun qxf-backtab-trim-inner-spaces
    ()
    (interactive)
    (let*
        (
            (*line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
            (*length (length *line))
            (*start-index (*get-nonspace-index *line 0 :forward))
            (*index 0)
            (*result "")
            (*concat nil)
            (*seperator nil)
        )
        (fset '*concat
            (lambda (*string)
                (setq *seperator
                    (if
                        (or
                            (eq ?\) (elt *string 0))
                            (eq 0 (length *result))
                        )
                        ""
                        "\s"
                    )
                )
                (setq *result (concat *result *seperator *string))
            )
        )
        (catch :return
            (while (< *index *length)
                (setq *index (*get-space-index *line *start-index :forward))
                (when (eq nil *index)
                    (*concat (substring *line *start-index *length))
                    (throw :return nil)
                )
                (*concat (substring *line *start-index *index))
                (setq *index (*get-nonspace-index *line *index :forward))
                (when (eq nil *index)
                    (throw :return nil)
                )
                (setq *start-index *index)
            )
        )
        (delete-region (line-beginning-position) (line-end-position))
        (insert *result)
        (backward-char)
        (qxf-format-lisp)
    )
    :defun-end
)
(m4-bind "<backtab>" qxf-backtab-trim-inner-spaces)

(defun qxf-jump-to-nearest-block-start
    ()
    (interactive)
    (goto-char (+ 2 (*get-nearest-block-start (buffer-string) (- (point) 1))))
    :defun-end
)
(m4-bind "C-c b" qxf-jump-to-nearest-block-start)

(defun qxf-jump-to-nearest-block-end
    ()
    (interactive)
    :insert-for-test
    (let*
        ; Something else for test.
        (
            (*string (buffer-string))
            (*start (*get-nearest-block-start *string (point)))
        )
        (goto-char (1+ (*get-index-of-char *string (+ 3 *start) ?\))))
    )
    :defun-end
)
(m4-bind "C-c f" qxf-jump-to-nearest-block-end)

(defun qxf-test-scan-text
    ()
    (interactive)
    (*print-to-side-bar "qxf-test-scan-text")
    (*append-to-side-bar (format "====>>> %s" (*scan-for-unpaired "(hello\")\"=====")))
    (*append-to-side-bar (format "====>>> %s" (*scan-for-unpaired "(hello\")()(\"world)!")))
    (*append-to-side-bar (format "====>>> %s" (*scan-for-unpaired "(hello\"))")))
    (*append-to-side-bar (format "====>>> %s" (*scan-for-unpaired "(hello")))
    (*append-to-side-bar (format "====>>> %s" (*scan-for-unpaired "(hello\")\")")))
    (*append-to-side-bar (format "====>>> %s" (*scan-for-unpaired "(hello()")))
    (*append-to-side-bar (format "====>>> %s" (*scan-for-unpaired "(hello())")))
    (*append-to-side-bar (format "====>>> %s" (*scan-for-unpaired "(hello())\;")))
    (*append-to-side-bar (format "====>>> %s" (*scan-for-unpaired "(hello())\"")))
    (*append-to-side-bar (format "====>>> %s" (*scan-for-unpaired "(hello())(")))
    (*append-to-side-bar (format "%s" (eq 1 1)))
    (*append-to-side-bar (format "%s" (eq "a" "a")))
    :defun-end)
(m4-bind "C-c t" qxf-test-scan-text)

(defun qxf-temporary-test
    ()
    (interactive)
    (let
        (
            (*to-print "Test results:\n")
            (*test (lambda (*in) (*append-to-side-bar (format "%s|" *in))))
            (*out nil)
            (*string nil)
            (*readed nil)
            (*readed-a nil)
            (*readed-b nil)
            (*test-map nil)
            (*test-list '())
            (*lines nil)
            (l4-cons '("a" . "b"))
        )
        (*init-outline-entry *test-map "hello signature" 123)
        (with-temp-buffer
            (print "this is the damn good thing" (current-buffer))
            (print "hello temp buffer" (current-buffer))
            (print '(1 2 3) (current-buffer))
            (setq *string (buffer-string))
            (goto-char 1)
            (setq *readed (read (current-buffer)))
            (setq *readed-a (read (current-buffer)))
            (setq *readed-b (read (current-buffer)))
        )
        (push *test-map *test-list)
        (fset '*out (lambda (*msg) (setq *to-print (format "%s%s\n" *to-print *msg))))
        (*print-to-side-bar "Atomic test start.")
        ; Test for property list.
        (prin1 (plist-get (elt *test-list 0) :line-number) qxf-buffer-side-bar)
        (print (*test-map :one) qxf-buffer-side-bar)
        (prin1 (car l4-cons) qxf-buffer-side-bar)
        (print (cdr l4-cons) qxf-buffer-side-bar)
        (*test-map :one 111)
        (prin1 (plist-get *test-map :one) qxf-buffer-side-bar)
        (*test-map :three 333)
        (print (*test-map :three) qxf-buffer-side-bar)
        (prin1 (*test-map :line-number) qxf-buffer-side-bar)
        (print (*test-map :signature) qxf-buffer-side-bar)
        ; Test for with-temp-buffer
        (prin1 *test qxf-buffer-side-bar)
        (prin1 *string qxf-buffer-side-bar)
        (setq *lines (*get-lines *string))
        (dolist (*line *lines)
            (princ *line qxf-buffer-side-bar)
        )
        (print *readed qxf-buffer-side-bar)
        (prin1 *readed-a qxf-buffer-side-bar)
        (print *readed-b qxf-buffer-side-bar)
        ; Test for other things.
        (princ "test print" *test)
        (*out "=======")
        (*out (qxf-*-stringify (numberp nil)))
        (*out (qxf-*-stringify (numberp t)))
        (*out (qxf-*-stringify (type-of (type-of "hello"))))
        (*out (qxf-*-stringify (eq 'string (type-of "hello"))))
        (*out (qxf-*-stringify (string-match "\n" "teststring")))
        (*out (format "?\\n:%s" ?\n))
        (*out (format "?\\(:%s" ?\())
        (*out (format "?\\):%s" ?\)))
        (*out (format "?\\\":%s" ?\"))
        (*out (format "?\\\\:%s" ?\\))
        (*out (format "%s" (point)))
        (*out (qxf-*-stringify (eq :test-const (read ":test-const"))))
        (*out (qxf-*-stringify (eq 'abc (make-symbol "abc"))))
        (*out (qxf-*-stringify (equal "abc" "abc")))
        (*append-to-side-bar *to-print)
    )
    :defun-end
)
(m4-bind "C-c t" qxf-temporary-test)

(defun qxf-create-newline
    ()
    (interactive)
    (let*
        (
            (*limit (*get-nearest-block-start (buffer-string) (point)))
        )
        (insert "\n")
        (qxf-format-lisp)
        (forward-line 2)
        (if (eq nil (re-search-backward "\n\s+\n" *limit t))
            (forward-line -2)
            (forward-char)
        )
        (end-of-line)
    )
    :defun-end
)
(m4-bind "C-c j" qxf-create-newline)

(defun *format-form (*string-form &optional *indent)
    (when (eq nil *indent)
        (setq *indent 0)
    )
    (setq *string-form (*trim *string-form))
    (let*
        (
            (*indent-string (make-string (* *indent qxf-code-indent) ?\s))
            (*first-newline-index (*get-newline-index *string-form 0 :forward))
            (*unpaired-index nil)
            (*unpaired-char nil)
            (*close-bracket-index nil)
            (*close-newline-index nil)
            (*close-line nil)
            (*nonspace-index nil)
            (*rest-string nil)
            (*inner-string nil)
            (*length (length *string-form))
            (*first-line nil)
        )
        (catch :return
            (when (eq nil *first-newline-index)
                (throw :return (concat *indent-string *string-form))
            )
            (setq *first-line (substring *string-form 0 *first-newline-index))
            (setq *unpaired-index (*scan-for-unpaired *first-line))
            (when (eq nil *unpaired-index)
                (throw :return
                    (concat
                        *indent-string *first-line "\n"
                        (*format-form (substring *string-form (1+ *first-newline-index) *length) *indent)
                    )
                )
            )
            (setq *unpaired-char (elt *string-form *unpaired-index))
            (when (not (eq *unpaired-char ?\())
                (*append-to-side-bar (format "%c <<<" *unpaired-char))
                (throw :return
                    (concat *indent-string "\; ERROR Only unpaired \"(\" allowed.\n" *string-form)
                )
            )
            (setq *close-bracket-index (*get-index-of-char *string-form (1+ *unpaired-index) ?\)))
            (when (eq nil *close-bracket-index)
                (throw :return
                    (concat *indent-string "\; ERROR Missing close bracket.\n" *string-form)
                )
            )
            (setq *close-newline-index (*get-newline-index *string-form *close-bracket-index :backward))
            (setq *close-line (substring *string-form (1+ *close-newline-index) *close-bracket-index))
            (setq *nonspace-index (*get-nonspace-index *close-line 0 :forward))
            (setq *rest-string (*trim (substring *string-form (1+ *close-bracket-index) *length)))
            (when (not (eq 0 (length *rest-string)))
                (if (string-prefix-p "\n" *rest-string)
                    (progn
                        (setq *rest-string
                            (*format-form (substring *rest-string 1 (length *rest-string)) *indent)
                        )
                        (setq *rest-string (concat "\n" *rest-string))
                    )
                    (setq *rest-string (*format-form *rest-string *indent))
                )
            )
            (setq *inner-string
                (if (eq nil *nonspace-index)
                    (substring *string-form
                        (if (eq *first-newline-index *close-newline-index)
                            *first-newline-index
                            (1+ *first-newline-index)
                        )
                        *close-newline-index
                    )
                    (substring *string-form (1+ *first-newline-index) *close-bracket-index)
                )
            )
            (concat
                *indent-string *first-line "\n"
                (*format-form *inner-string (1+ *indent))
                "\n" *indent-string ")" *rest-string
            )
        )
    )
)

(defun qxf-format-lisp
    ()
    (interactive)
    (let*
        (
            (*string (buffer-substring-no-properties 1 (buffer-size)))
            (*point-o (point))
            (*point-a (1+ (*get-nearest-block-start *string *point-o)))
            (*point-b (1+ (*get-index-of-char *string (1+ *point-a) ?\))))
            (*block (substring *string *point-a *point-b))
            (*formatted (*format-form *block))
        )
        (if (string-equal *block *formatted)
            (princ "Current block is already pretty enough.")
            (delete-region (1+ *point-a) (1+ *point-b))
            (insert *formatted)
            (goto-char *point-o)
            (princ "Formatted current block.")
        )
    )
    :defun-end
)
(m4-bind "C-c q" qxf-format-lisp)

(defun qxf-duplicate-line
    ()
    (interactive)
    (let
        (
            (*point-a nil)
            (*point-b nil)
        )
        (beginning-of-line)
        (setq *point-a (point))
        (re-search-forward "\n")
        (setq *point-b (point))
        (insert (buffer-substring-no-properties *point-a *point-b))
        (forward-line -1)
        (end-of-line)
    )
    :defun-end
)
(m4-bind "C-c d" qxf-duplicate-line)

(defun qxf-focus-line-beginning
    ()
    (interactive)
    (beginning-of-line)
    (goto-char (1- (re-search-forward "[^[:space:]]")))
    (if (string= "(" (buffer-substring-no-properties (point) (1+ (point))))
        (forward-char)
    )
    :defun-end
)
(m4-bind "C-c a" qxf-focus-line-beginning)

(defmacro m4-cut-current-line (l4-variable)
    `(let*
        (
            (l4-point (point))
            (l4-start-point (line-beginning-position))
            (l4-offset (- l4-point l4-start-point))
            (l4-string
                (delete-and-extract-region l4-start-point (1+ (line-end-position)))
            )
        )
        (setq ,l4-variable (list :offset l4-offset :string l4-string))
        (*make-object-oriented-like ,l4-variable)
    )
)

(defun qxf-move-line-down
    ()
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

(defun qxf-move-line-up
    ()
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

(defun qxf-copy-region
    ()
    (interactive)
    (setq qxf-string-cache (buffer-substring (region-beginning) (region-end)))
    (deactivate-mark t)
)
(m4-bind "C-c c" qxf-copy-region)

(defun qxf-paste
    ()
    (interactive)
    (insert qxf-string-cache)
)
(m4-bind "C-c y" qxf-paste)

(defun *record-current-buffer (*buffer *point)
    (let*
        (
            (*to-save (format "%s\n%d\n" (buffer-file-name *buffer) *point))
        )
        (with-temp-file qxf-focus-record (insert *to-save))
    )
)

(defun qxf-record-focus
    ()
    (interactive)
    (*record-current-buffer (current-buffer) (point))
    (princ "Recorded current focus.")
)
(m4-bind "C-c DEL" qxf-record-focus)

(defun qxf-load-record
    ()
    (interactive)
    (let*
        (
            (*lines (*get-lines (*get-file-contents qxf-focus-record)))
            (*path-to-open (elt *lines 0))
            (*point (string-to-number (elt *lines 1)))
        )
        (qxf-focus-editor)
        (find-file *path-to-open)
        (goto-char *point)
        (princ "Loaded focus record.")
    )
)
(m4-bind "C-c =" qxf-load-record)

(defun qxf-focus-editor
    ()
    (interactive)
    (select-window qxf-window-editor)
    (*render-side-bar)
)
(m4-bind "C-c e" qxf-focus-editor)

(defun *list-outmost-blocks ()
    (let*
        (
            (*list '())
            (*entry nil)
            (*start-index 0)
            (*end-index 0)
            (*string (buffer-substring-no-properties (point-min) (point-max)))
            (*length (length *string))
        )
        (catch :break
            (while (< *start-index *length)
                (setq *start-index (*get-index-of-char *string *start-index ?\())
                (when (null *start-index) (throw :break t))
                (setq *end-index (*get-index-of-char *string (1+ *start-index) ?\)))
                (when (null *end-index)
                    (princ "Lisp code is broken.")
                    (throw :break t)
                )
                (push 
                    (*init-outline-entry *entry
                        (substring *string *start-index (1+ *end-index))
                        (*count-lines (substring *string 0 *start-index))
                    )
                    *list
                )
                (setq *start-index (1+ *end-index))
            )
        )
        (setq *list (reverse *list))
        ; (seq-sort-by (lambda (*e) (elt *e 0)) #'string< *list)
        ; (setq *list (seq-sort 'string< *list))
        *list
    )
)

(defun *parse-signature (*list)
    (let*
        (
            (*signature nil)
            (*newline-index nil)
            (*start-index nil)
            (*end-index nil)
            (*filtered-list nil)
        )
        (dolist (*block *list)
            (*make-object-oriented-like *block)
            (setq *signature (*block :signature))
            (setq *newline-index (*get-newline-index *signature 0 :forward))
            (setq *signature
                (if (null *newline-index)
                    *signature
                    (substring *signature 0 *newline-index)
                )
            )
            (when (string-prefix-p "(def" *signature)
                (setq *start-index (*get-space-index *signature 0 :forward))
                (if (null *start-index)
                    (setq *signature "**check-code**")
                    (++ *start-index)
                    (setq *end-index (*get-space-index *signature *start-index :forward))
                    (setq *signature (substring *signature *start-index *end-index))
                )
                (*block :signature *signature)
                (push *block *filtered-list)
            )
        )
        (setq *filtered-list
            (seq-sort-by
                (lambda (*e) (plist-get *e :signature))
                #'string<
                *filtered-list
            )
        )
        (dolist (*entry *filtered-list)
            (princ
                (format
                    "%s [%d]\n"
                    (plist-get *entry :signature)
                    (plist-get *entry :line-number)
                )
                qxf-buffer-side-bar
            )
        )
    )
)

(defun qxf-render-lisp-outline
    ()
    (interactive)
    (*print-to-side-bar "所有定义：")
    (*parse-signature (*list-outmost-blocks))
    :defun-end
)
(m4-bind "C-c |" qxf-render-lisp-outline)

(defun qxf-focus-side-bar
    ()
    (interactive)
    (*render-side-bar)
)
(m4-bind "C-c \\" qxf-focus-side-bar)

(defun qxf-make-mic-array
    ()
    (interactive)
    (shell-command (format "date && cd %s/build && make -j 8 && ./listdevs" qxf-mic-array-root))
)
(m4-bind "C-c 1" qxf-make-mic-array)

(defun qxf-cmake-mic-array
    ()
    (interactive)
    (shell-command (format "date && cd %s/build && cmake .." qxf-mic-array-root))
    :defun-end
)
(m4-bind "C-c 2" qxf-cmake-mic-array)

(defun c6-toggle-debug-error
    ()
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

(defun qxf-layout-3-pane
    ()
    (interactive)
    (qxf-focus-editor)
    (delete-other-windows)
    ; (find-file (format "%s/src/listdevs.c" qxf-mic-array-root))
    (shell-command "echo Make shell area.")
    (setq qxf-window-shell-out (split-window nil -20 'below))
    (set-window-buffer qxf-window-shell-out "*Shell Command Output*")
    (setq qxf-window-side-bar
        (split-window nil (+ 120 (*get-line-number-width))
            'left
        )
    )
    (set-window-buffer qxf-window-side-bar qxf-buffer-side-bar)
    (*render-side-bar)
    (shell-command "echo Initialized shell area.")
)
(m4-bind "C-c 0" qxf-layout-3-pane)

(defun qxf-layout-2-pane
    ()
    (interactive)
    (qxf-focus-editor)
    (delete-other-windows)
    (setq qxf-window-side-bar
        (split-window nil (+ 120 (*get-line-number-width))
            'left
        )
    )
    (set-window-buffer qxf-window-side-bar qxf-buffer-side-bar)
    (*render-side-bar)
    :defun-end
)
(m4-bind "C-c 9" qxf-layout-2-pane)

(defun qxf-insert-command (*template-type *command-name)
    (interactive "sTemplate-type:\nsTarget-name:")
    (insert
        (format
            (lax-plist-get qxf-insertion-templates *template-type)
            *command-name *command-name
        )
    )
    :defun-end
)
(m4-bind "C-c i" qxf-insert-command)

(defun *clamp-string (*string *width &optional *is-left)
    (let*
        (
            (*length (length *string))
        )
        (if (> *length *width)
            (if *is-left
                (concat "..." (substring *string (- *length (- *width 3))))
                (concat (substring *string 0 (- *width 3)) "...")
            )
            *string
        )
    )
)

(defun f7-render-buffer-entry (l4-pair)
    (let*
        (
            (l4-buffer (cdr l4-pair))
            (*buffer-name (if (eq :__: l4-buffer) (symbol-name :__:) (buffer-name l4-buffer)))
            (*file-path (car l4-pair))
            (*directory-path (file-name-directory *file-path))
        )
        (insert
            (format "[%-16s | %s]\n"
                (*clamp-string *buffer-name 16)
                (*clamp-string (if (equal *buffer-name (symbol-name :__:)) *file-path *directory-path) 16 t)
            )
        )
    )
    :end-defun
)

(defun *get-line-number-width ()
    (+ 2 (length (format "%d" (count-lines (point-min) (point-max)))))
)

(defmacro m4-remove-association (l4-key l4-alist)
    `(setq ,l4-alist (assoc-delete-all ,l4-key ,l4-alist))
)

(defun f7-update-opened-buffers ()
    (let*
        (
            (l4-name nil)
        )
        (dolist (*buffer (buffer-list))
            (setq l4-name (buffer-file-name *buffer))
            (when (stringp l4-name)
                (m4-remove-association l4-name g5-opened-buffers)
		(m4-insert-to-list (cons l4-name *buffer) g5-opened-buffers)
            )
        )
    )
)

(defun *render-side-bar ()
    (with-current-buffer qxf-buffer-side-bar
        (erase-buffer)
        (insert (format "%s\n" (current-time-string)))
        (dolist (l4-pair g5-opened-buffers)
            (f7-render-buffer-entry l4-pair)
        )
    )
    :end-defun
)

(print "Loaded qxf-make.")

