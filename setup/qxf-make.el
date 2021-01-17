(provide 'qxf-make)
(require 'qxf-utils)
(require 'seq)

(defconst qxf-mic-array-root "/home/qixiaofeng/Documents/sandbox/hachi-mic-array")
(defconst qxf-focus-record "~/.emacs.d/backup/focus-record.txt")
(defvar qxf-insertion-template (*get-file-contents "~/.emacs.d/setup/insertion-templates.txt"))
(defvar qxf-window-editor (frame-root-window))
(defvar qxf-window-shell-out nil)

(defvar qxf-buffer-side-bar (get-buffer-create "*side-bar*"))
(defvar qxf-window-side-bar nil)
(defvar qxf-opened-buffers '())

(defvar qxf-string-cache "")
(defvar qxf-code-indent 4)

(with-current-buffer qxf-buffer-side-bar
    (setq display-line-numbers t)
)

(add-hook 'buffer-list-update-hook '*render-side-bar)
(add-hook 'kill-emacs-hook 'qxf-record-focus)

(defun *append-to-side-bar (*message)
    (*append-to-buffer *message qxf-buffer-side-bar)
)

(defun *print-to-side-bar (*message)
    (*print-to-buffer *message qxf-buffer-side-bar)
)

; TODO Implement file outline. List functions with sort and line numbers. [C-c |]
; TODO Sidebar for available buffers.
;      *. Side-bar content save and load.
;      1. Show opened file buffers.
;      2. Add [C-c <down>] and [C-c <up>] for editor switch.
;      3. Perhaps use blur hook of editor window to referesh the side-bar.
; TODO Implement the project concept.
;      1. Make the qxf-mic-array-root (actually is work-root) changeable.
;      2. Way for search the project root, perhaps a hidden config file.
; TODO Implement [C-c s] and [C-c r], convenient search, extract keyword at current point.
; TODO Local varialble rename.
; TODO Auto jump to definition/header/declaration.

; DONE Hide the menu bar in qxf-general.el.
; DONE Create copy and paste logic.
; DONE Implement line movement.
; DONE Implement snippet insertion. e.g. command definition.
; DONE Make the editor 120+<number-columns>. {[function] count-lines start end}
; DONE Add a [C-c a] to jump to the line beginning.
; DONE Add a cmake command.
; DONE Make line copy. [C-c d]
; DONE Implement append-to-side-bar.
; DONE Assign [C-c 0] to 3 panel mode, but not jump to mic-array-root.
; DONE Assign [C-c 9] to 2 panel mode.
; DONE Know how to make data structure.
;      * property list,
;        * {[Function] plist-get plist property}
;        * {[Function] plist-put plist property value}
;        * {[Function] plist-member plist property}
;      * associate list,
;        * {[Function] assoc key alist &optional testfn}
;        * {[Function] rassoc value alist}
;        * {[Function] assq key alist}
;      * record, is a vector, first solt is value to use by {type-of}.
;        * {[Function] record type &rest objects}
;        * {[Function] make-record type length object}
; DONE Extract print-to-buffer.
; DONE Jump to nearest outmost bracket. [C-c b] and [C-c f].
; DONE Make the indent-sexp as I like: a brackets pair is not in same line have to be in same column. [C-c q]
; DONE Implement [C-c (] to auto insert ().
; DONE Implement [<backtab>]. Trim inner spaces(    ).
; DONE Implement [C-c j] to break with auto-format.
; DONE Take a look at package chapter. Extract private functions into qxf-utils.
; DONE Assign [C-c i] to quick insertion.
;      * template Load from file.
; DONE Show more information for entries in sidebar.
; DONE Implement [C-c "], [C-c [] and [C-c {] inputing pair shortcut.
; FIXED [C-c q] Spaces at line-end.
; FIXED [C-c f] printed a lot things.
; FIXED [C-c q] Error on line with only empty string.

; CANCELED Implement point history. [C-c .] and [C-c ,] to jump.
; CANCELED Use hook and mode to deal with confliction with c mode.

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
(define-key global-map (kbd "C-c s") 'qxf-search-word)

(defun qxf-jump-to-previous-empty-line
    ()
    (interactive)
    (re-search-backward "\n\n")
    (forward-char)
    :defun-end
)
(define-key global-map (kbd "C-c -") 'qxf-jump-to-previous-empty-line)

(defun qxf-jump-to-next-empty-line
    ()
    (interactive)
    (re-search-forward "\n\n")
    (backward-char)
    :defun-end
)
(define-key global-map (kbd "C-c _") 'qxf-jump-to-next-empty-line)

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
(define-key global-map (kbd "C-c (") 'qxf-auto-insert-parentheses)

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
(define-key global-map (kbd "C-c C-j") 'qxf-auto-expand-empty-line)

(defun qxf-auto-insert-double-quotes
    ()
    (interactive)
    (*auto-insert-paired "\"\"")
    :defun-end
)
(define-key global-map (kbd "C-c \"") 'qxf-auto-insert-double-quotes)

(defun qxf-auto-insert-brackets
    ()
    (interactive)
    (*auto-insert-paired "[]")
    :defun-end
)
(define-key global-map (kbd "C-c [") 'qxf-auto-insert-brackets)

(defun qxf-auto-insert-braces
    ()
    (interactive)
    (*auto-insert-paired "{}")
    :defun-end
)
(define-key global-map (kbd "C-c {") 'qxf-auto-insert-braces)

(defun qxf-kill-line
    ()
    (interactive)
    (beginning-of-line)
    (kill-line)
    (kill-line)
    (end-of-line)
    :defun-end
)
(define-key global-map (kbd "C-c k") 'qxf-kill-line)

(defun qxf-backtab-trim-inner-spaces
    ()
    (interactive)
    (let*
        (
            (*line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
            (*length (length *line))
            (*start-index 0)
            (*index (*get-nonspace-index *line 0 :forward))
            (*result "")
            (*concat nil)
            (*seperator nil)
        )
        (fset '*concat
            (lambda (*string)
                (setq *seperator (if (eq ?\) (elt *string 0)) "" "\s"))
                (setq *result (concat *result *seperator *string))
            )
        )
        (catch :return
            (while (< *index *length)
                (setq *index (*get-space-index *line *index :forward))
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
(define-key global-map (kbd "<backtab>") 'qxf-backtab-trim-inner-spaces)

(defun qxf-jump-to-nearest-block-start
    ()
    (interactive)
    (goto-char (+ 2 (*get-nearest-block-start (buffer-string) (- (point) 1))))
    :defun-end
)
(define-key global-map (kbd "C-c b") 'qxf-jump-to-nearest-block-start)

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
(define-key global-map (kbd "C-c f") 'qxf-jump-to-nearest-block-end)

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
(define-key global-map (kbd "C-c t") 'qxf-test-scan-text)

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
        (*test-map :one 111)
        (prin1 (plist-get *test-map :one) qxf-buffer-side-bar)
        (*test-map :three 333)
        (print (*test-map :three) qxf-buffer-side-bar)
        (prin1 (*test-map :line-number) qxf-buffer-side-bar)
        (print (*test-map :signature) qxf-buffer-side-bar)
        ; Test for with-temp-buffer
        (prin1 *test qxf-buffer-side-bar)
        (prin1 *string qxf-buffer-side-bar)
        (prin1 *readed qxf-buffer-side-bar)
        (prin1 *readed-a qxf-buffer-side-bar)
        (prin1 *readed-b qxf-buffer-side-bar)
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
        (*append-to-side-bar *to-print)
    )
    :defun-end
)
(define-key global-map (kbd "C-c t") 'qxf-temporary-test)

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
(define-key global-map (kbd "C-c j") 'qxf-create-newline)

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
(define-key global-map (kbd "C-c q") 'qxf-format-lisp)

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
(define-key global-map (kbd "C-c d") 'qxf-duplicate-line)

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
(define-key global-map (kbd "C-c a") 'qxf-focus-line-beginning)

(defun qxf-move-line-down
    ()
    (interactive)
    (let
	((-temp-string (delete-and-extract-region (line-beginning-position) (1+ (line-end-position)))))
	(forward-line 1)
	(insert -temp-string)
	(forward-line -1))
    :defun-end)
(define-key global-map (kbd "C-c n") 'qxf-move-line-down)

(defun qxf-move-line-up
    ()
    (interactive)
    (let
	((-temp-string (delete-and-extract-region (line-beginning-position) (1+ (line-end-position)))))
	(forward-line -1)
	(insert -temp-string)
	(forward-line -1))
    :defun-end)
(define-key global-map (kbd "C-c p") 'qxf-move-line-up)

(defun qxf-copy-region
    ()
    (interactive)
    (setq qxf-string-cache (buffer-substring (region-beginning) (region-end)))
    (keyboard-quit)
)
(define-key global-map (kbd "C-c c") 'qxf-copy-region)

(defun qxf-paste
    ()
    (interactive)
    (insert qxf-string-cache)
)
(define-key global-map (kbd "C-c y") 'qxf-paste)

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
(define-key global-map (kbd "C-c DEL") 'qxf-record-focus)

(defun qxf-load-record
    ()
    (interactive)
    (let*
        (
            (*string nil)
            (*start-index 0)
            (*end-index nil)
            (*path-to-open nil)
            (*point nil)
        )
        (setq
            *string
            (with-temp-buffer
                (insert-file-contents qxf-focus-record)
                (buffer-substring-no-properties (point-min) (point-max))
            )
        )
        (setq *end-index (*get-newline-index *string *start-index :forward))
        (setq *path-to-open (substring *string *start-index *end-index))
        (setq *start-index (1+ *end-index))
        (setq *end-index (*get-newline-index *string *start-index :forward))
        (setq *point (string-to-number (substring *string *start-index *end-index)))
        (qxf-focus-editor)
        (find-file *path-to-open)
        (goto-char *point)
        (princ "Loaded focus record.")
    )
)
(define-key global-map (kbd "C-c =") 'qxf-load-record)

(defun qxf-focus-editor
    ()
    (interactive)
    (*render-side-bar)
    (select-window qxf-window-editor)
)
(define-key global-map (kbd "C-c e") 'qxf-focus-editor)

(defun *list-outmost-statements ()
    (let*
        (
            (*list '())
            (*start-index 0)
            (*end-index 0)
            (*string (buffer-substring-no-properties 1 (buffer-size)))
            (*length (length *string))
        )
        (catch :break
            (while t
                ; (*get-index-of-char )
                (push *start-index *list)
                (++ *start-index 100)
                (when (>= *start-index *length)
                    (throw :break t)
                )
            )
        )
        ; (seq-sort-by (lambda (*e) (elt *e 0)) #'string< *list)
        (setq *list (seq-sort '< *list))
        (princ *list qxf-buffer-side-bar)
    )
)

(defun qxf-render-lisp-outline
    ()
    (interactive)
    (*list-outmost-statements)
    :defun-end
)
(define-key global-map (kbd "C-c |") 'qxf-render-lisp-outline)

(defun qxf-focus-side-bar
    ()
    (interactive)
    (*render-side-bar)
    (select-window qxf-window-side-bar)
)
(define-key global-map (kbd "C-c \\") 'qxf-focus-side-bar)

(defun qxf-make-mic-array
    ()
    (interactive)
    (shell-command (format "date && cd %s/build && make -j 8 && ./listdevs" qxf-mic-array-root))
)
(define-key global-map (kbd "C-c 1") 'qxf-make-mic-array)

(defun qxf-cmake-mic-array
    ()
    (interactive)
    (shell-command (format "date && cd %s/build && cmake .." qxf-mic-array-root))
    :defun-end
)
(define-key global-map (kbd "C-c 2") 'qxf-cmake-mic-array)

(defun qxf-layout-3-pane
    ()
    (interactive)
    (qxf-focus-editor)
    (delete-other-windows)
    ; (find-file (format "%s/src/listdevs.c" qxf-mic-array-root))
    (shell-command "echo Make shell area.")
    (setq qxf-window-shell-out (split-window nil -20 'below))
    (set-window-buffer qxf-window-shell-out "*Shell Command Output*")
    (setq qxf-window-side-bar (split-window nil (+ 120 (*get-line-number-width)) 'left))
    (set-window-buffer qxf-window-side-bar qxf-buffer-side-bar)
    (*render-side-bar)
    (shell-command "echo Initialized shell area.")
)
(define-key global-map (kbd "C-c 0") 'qxf-layout-3-pane)

(defun qxf-layout-2-pane
    ()
    (interactive)
    (qxf-focus-editor)
    (delete-other-windows)
    (setq qxf-window-side-bar (split-window nil (+ 120 (*get-line-number-width)) 'left))
    (set-window-buffer qxf-window-side-bar qxf-buffer-side-bar)
    (*render-side-bar)
    :defun-end
)
(define-key global-map (kbd "C-c 9") 'qxf-layout-2-pane)

(defun qxf-insert-command (-command-name)
    (interactive "sCommand-name:")
    (insert (format qxf-insertion-template -command-name -command-name))
    :defun-end
)
(define-key global-map (kbd "C-c i") 'qxf-insert-command)

(defun qxf-set-c-offset ()
    (interactive)
    (set-variable 'c-basic-offset 4)
)
(define-key global-map (kbd "C-c 4") 'qxf-set-c-offset)

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

(defun *render-entry (*buffer)
    (let*
        (
            (*buffer-name (buffer-name *buffer))
            (*file-path (buffer-file-name *buffer))
            (*should-render (stringp *file-path))
            (*directory-path (if *should-render (file-name-directory *file-path) nil))
        )
        (when *should-render
            (insert
                (format "[%-16s | %s]\n"
                    (*clamp-string *buffer-name 16)
                    (*clamp-string *directory-path 16 t)
                )
            )
        )
    )
    :end-defun
)

(defun *get-line-number-width
    ()
    (+ 2 (length (format "%d" (count-lines (point-min) (point-max)))))
)

(defun *render-side-bar ()
    (with-current-buffer qxf-buffer-side-bar
        (erase-buffer)
        (insert (format "%s\n" (current-time-string)))
        (dolist (*buffer (buffer-list))
            (*render-entry *buffer)
        )
    )
    :end-defun
)

(print "Loaded qxf-make.")

