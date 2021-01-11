(provide 'qxf-make)
(require 'qxf-utils)

(defconst qxf-mic-array-root "/home/qixiaofeng/Documents/sandbox/hachi-mic-array")
(defconst qxf-focus-record "~/.emacs.d/backup/focus-record.txt")
(defvar qxf-buffer-side-bar (get-buffer-create "*side-bar*"))
(defvar qxf-window-editor (frame-root-window))
(defvar qxf-window-shell-out nil)
(defvar qxf-window-side-bar nil)
(defvar qxf-string-cache "")
(defvar qxf-code-indent 4)

(defun *append-to-side-bar (*message)
    (*append-to-buffer *message qxf-buffer-side-bar))

(defun *print-to-side-bar (*message)
    (*print-to-buffer *message qxf-buffer-side-bar))

; TODO Make the indent-sexp as I like: a brackets pair is not in same line have to be in same column. [C-c q]
; TODO Implement [C-c s] and [C-c r], convenient search.
; TODO Implement [<backtab>].
; TODO Implement point history. [C-c .] and [C-c ,] to jump.
; TODO Assign [C-c i] to quick insertion.
;      * Load template from file.
; TODO Sidebar for available buffers.
;      1. Show opened file buffers.
;      2. Add [C-c <down>] and [C-c <up>] for editor switch.
;      3. Perhaps use blur hook of editor window to referesh the side-bar.
;      4. Side-bar content save and load.
; TODO Implement the project concept.
;      1. Make the qxf-mic-array-root (actually is work-root) changeable.
;      2. Way for search the project root, perhaps a hidden config file.
; TODO Local varialble rename.

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

; {[function] buffer-list &optional frame}
; {[function] buffer-name &optional buffer}
; {[function] buffer-file-name &optional buffer}
; {[function] buffer-modified-p &optional buffer}
; {[macro] with-current-buffer buffer-or-name body...}

; WIP Align the outmost brackets pair.
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
		    (setq *index (1+ *tmp-index))
		    (setq *last-index *tmp-index)
		    )
		)
	    )
	; (*print-to-side-bar (format "%s, %c" *tmp-index (elt *string *current-point)))
	*result
	)
    )

(defun qxf-jump-to-nearest-block-start
    ()
    (interactive)
    (goto-char (+ 2 (*get-nearest-block-start (buffer-string) (- (point) 1))))
    :defun-end)
(define-key global-map (kbd "C-c b") 'qxf-jump-to-nearest-block-start)

(defun qxf-jump-to-nearest-block-end
    ()
    (interactive)
    (let*
	(
	    (*string (buffer-string))
	    (*start (*get-nearest-block-start *string (point)))
	    )
	(goto-char (+ 1 (*get-index-of-char *string (+ 3 *start) ?\))))
	)
    :defun-end)
(define-key global-map (kbd "C-c f") 'qxf-jump-to-nearest-block-end)

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
	))

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
		    (setq *tmp-index (*get-index-of-string-end *string (1+ *index)))
		    (if (eq *tmp-index nil)
			:pass
			(setq *result *tmp-index)
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
		(t (setq *index (1+ *index)))
		)
	    )
	*result
	)
    )

; 1. Check next char:
;    ": find next "
;    \: index + 2
;    (: find next )
;    default: index + 1
(defun *scan-rest (*string)
    (let*
	(
	    (*result nil)
	    (*index 0)
	    (*pair-index nil)
	    (*length (length *string))
	    (*cc nil)
	    )
	(*append-to-side-bar (format "scn-rst [%s] %2d/%d" *string 0 *length))
	(while (and (eq *result nil) (< *index *length))
	    (setq *cc (elt *string *index))
	    (cond
		((eq *cc ?\\) (setq *index (+ 2 *index)))
		((eq *cc ?\")
		    (setq *pair-index (*get-index-of-char *string (1+ *index) ?\"))
		    (if (eq *pair-index nil)
			(setq *result *index)
			(setq *index (1+ *pair-index))
			)
		    )
		((eq *cc ?\()
		    (setq *pair-index (*get-index-of-char *string (1+ *index) ?\)))
		    (if (eq *pair-index nil)
			(setq *result *index)
			(setq *index (1+ *pair-index))
			)
		    )
		(t (setq *index (1+ *index)))
		)
	    )
	*result
	)
    )

(defun qxf-test-scan-text
    ()
    (interactive)
    (*print-to-side-bar "qxf-test-scan-text")
    (*append-to-side-bar (format "====>>> %s" (*scan-rest "(hello\")\"=====")))
    (*append-to-side-bar (format "====>>> %s" (*scan-rest "(hello\")()(\"world)!")))
    (*append-to-side-bar (format "====>>> %s" (*scan-rest "(hello\"))")))
    (*append-to-side-bar (format "====>>> %s" (*scan-rest "(hello")))
    (*append-to-side-bar (format "====>>> %s" (*scan-rest "(hello\")\")")))
    (*append-to-side-bar (format "====>>> %s" (*scan-rest "(hello()")))
    (*append-to-side-bar (format "====>>> %s" (*scan-rest "(hello())")))
    (*append-to-side-bar (format "%s" (eq 1 1)))
    (*append-to-side-bar (format "%s" (eq "a" "a")))
    :defun-end)
(define-key global-map (kbd "C-c t") 'qxf-test-scan-text)

; 1. Atomic line does not contain any open bracket(un-paired "(" or ")").
; 2. Atomic line does not contain any "\n".
; ======= WIP =======
(defun *is-atomic-line (*string-line)
    (let
	(
	    (*index (string-match "\n" *string-line))
	    )
	(if *index
	    *index ; Here should return data structure.
	    (progn
		(setq *index 0)
		t
		)
	    )
	)
    )

(defun qxf-test-is-atomic-line
    ()
    (interactive)
    (let
	(
	    (*to-print "Test results:\n")
	    (*test
		(lambda (*test-string)
		    (setq *to-print
			(concat *to-print
			    (format "%s:%s\n" *test-string (*is-atomic-line *test-string))))
		    )
		)
	    (*out (lambda (*msg) (setq *to-print (format "%s%s\n" *to-print *msg))))
	    )
	(funcall *test "Hello \n test!")
	(funcall *test "Hello test!")
	(funcall *out "=======")
	(funcall *out (qxf-*-stringify (numberp nil)))
	(funcall *out (qxf-*-stringify (numberp t)))
	(funcall *out (qxf-*-stringify (type-of (type-of "hello"))))
	(funcall *out (qxf-*-stringify (eq 'string (type-of "hello"))))
	(funcall *out (qxf-*-stringify (string-match "\n" "teststring")))
	(funcall *out (format "?\\n:%s" ?\n))
	(funcall *out (format "?\\(:%s" ?\())
	(funcall *out (format "?\\):%s" ?\)))
	(funcall *out (format "?\\\":%s" ?\"))
	(funcall *out (format "?\\\\:%s" ?\\))
	(*print-to-side-bar *to-print))
    :defun-end)
(define-key global-map (kbd "C-c t") 'qxf-test-is-atomic-line)

(defun *get-newline-index (*string *start *direction)
    (let*
	(
	    (*result nil)
	    (*index *start)
	    (*cc nil)
	    (*length (length *string))
	    (*update-index nil)
	    (*continue nil)
	    )
	(fset '*update-index
	    (cond
		((eq :forward *direction) (lambda () (setq *index (1+ *index))))
		((eq :backward *direction) (lambda () (setq *index (1- *index))))
		(t (error "Direction has to be :forward or :backward."))
		))
	(fset '*continue
	    (cond
		((eq :forward *direction) (lambda () (< *index *length)))
		((eq :backward *direction) (lambda () (> *index -1)))
		))
	(while (and (*continue) (eq nil *result))
	    (setq *cc (elt *string *index))
	    (if (eq ?\n *cc)
		(setq *result *index)
		(*update-index)
		)
	    )
	*result
	)
    )

(defun *get-distance-between (*start *end)
    (1- (- *end *start)))

(defun *is-all-space-between (*string *start *end)
    (let*
	(
	    (*index (1+ *start))
	    (*result t)
	    (*cc nil)
	    )
	(while (and (eq *result t) (< *index *end))
	    (setq *cc (elt *string *index))
	    (if (eq ?\s *cc)
		(setq *index (1+ *index))
		(setq *result nil)
		)
	    )
	*result
	))

(defun *count-lines (*string)
    (let*
	(
	    (*start 0)
	    (*length (length *string))
	    (*count (if (> *length 0) 1 0))
	    (*break nil)
	    (*linebreak-index nil)
	    )
	(while (and (> *count 0) (< *start *length) (eq *break nil))
	    (setq *linebreak-index (string-match "\n" *string *start))
	    (if (eq nil *linebreak-index)
		(setq *break t)
		(progn
		    (setq *count (1+ *count))
		    (setq *start (1+ *linebreak-index))
		    )
		)
	    )
	*count
	)
    )

(defun *has-newline-between (*string *start *end)
    (let*
	(
	    (*result nil)
	    (*string-to-search (substring *string (1+ *start) *end))
	    )
	(if (eq nil (string-match "\n" *string-to-search))
	    :pass
	    (setq *result t)
	    )
	*result
	)
    )

; ======= WIP =======
(defun *format-form (*string-form &optional *indent)
    (if (eq nil *indent)
	(setq *indent 0)
	:pass
	)
    (let*
	(
	    (*length (length *string-form))
	    (*left-bracket-index (*get-index-of-char *string-form 0 ?\())
	    (*right-bracket-index (*get-index-of-char *string-form (1+ *left-bracket-index) ?\)))
	    (*first-newline-index nil)
	    (*last-newline-index nil)
	    (*distance nil)
	    (*all-space-flag "nonset")
	    (*indent-string (make-string (* *indent qxf-code-indent) ?\s))
	    )
	(if (*has-newline-between *string-form *left-bracket-index *right-bracket-index)
	    (progn
		(setq *first-newline-index (*get-newline-index *string-form *left-bracket-index :forward))
		(setq *last-newline-index (*get-newline-index *string-form *right-bracket-index :backward))
		(setq *distance (*get-distance-between *last-newline-index *right-bracket-index))
		(setq *all-space-flag
		    (*is-all-space-between *string-form *last-newline-index *right-bracket-index)
		    )
		(if *all-space-flag
		    (setq *string-form
			(concat *indent-string (substring *string-form 0 (1+ *first-newline-index))
			    
			    *indent-string (substring *string-form *right-bracket-index *length)
			    ))
		    (setq *string-form
			(concat *indent-string (substring *string-form 0 (1+ *first-newline-index))
			    
			    "\n" *indent-string (substring *string-form *right-bracket-index *length)
			    ))
		    )
		)
	    (concat
		*indent-string
		*string-form
		)
	    )
	)
    )

(defun qxf-format-lisp
    ()
    (interactive)
    (let*
	(
	    (*point-o (point))
	    (*string (buffer-string))
	    (*point-a (1+ (*get-nearest-block-start *string *point-o)))
	    (*point-b (1+ (*get-index-of-char *string (1+ *point-a) ?\))))
	    )
	(*print-to-side-bar
	    (*format-form (substring *string *point-a *point-b))
	    )
	)
    :defun-end)
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
	)
    :defun-end)
(define-key global-map (kbd "C-c d") 'qxf-duplicate-line)

(defun qxf-focus-line-beginning
    ()
    (interactive)
    (beginning-of-line)
    (goto-char (- (re-search-forward "[^[:space:]]") 1))
    :defun-end)
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
    (keyboard-quit))
(define-key global-map (kbd "C-c c") 'qxf-copy-region)

(defun qxf-paste
    ()
    (interactive)
    (insert qxf-string-cache))
(define-key global-map (kbd "C-c y") 'qxf-paste)

(defun qxf-record-focus
    ()
    (interactive)
    (let
	((-temp-string (format "\"%s\"\n" (buffer-file-name (current-buffer)))))
	(with-temp-file qxf-focus-record (insert -temp-string))
	)
    )
(define-key global-map (kbd "C-c DEL") 'qxf-record-focus)

(defun qxf-load-record
    ()
    (interactive)
    (let*
	(
	    (-temp-buffer (find-file-noselect qxf-focus-record))
	    (-path-string (read -temp-buffer)))
	(kill-buffer -temp-buffer)
	(qxf-focus-editor)
	(find-file -path-string)
	)
    )
(define-key global-map (kbd "C-c =") 'qxf-load-record)

(defun qxf-focus-editor
    ()
    (interactive)
    (*render-side-bar)
    (select-window qxf-window-editor))
(define-key global-map (kbd "C-c -") 'qxf-focus-editor)

(defun qxf-focus-side-bar
    ()
    (interactive)
    (*render-side-bar)
    (select-window qxf-window-side-bar))
(define-key global-map (kbd "C-c \\") 'qxf-focus-side-bar)

(defun qxf-make-mic-array
    ()
    (interactive)
    (shell-command (format "date && cd %s/build && make -j 8 && ./listdevs" qxf-mic-array-root)))
(define-key global-map (kbd "C-c 1") 'qxf-make-mic-array)

(defun qxf-cmake-mic-array
    ()
    (interactive)
    (shell-command (format "date && cd %s/build && cmake .." qxf-mic-array-root))
    :defun-end)
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
    (shell-command "echo Initialized shell area."))
(define-key global-map (kbd "C-c 0") 'qxf-layout-3-pane)

(defun qxf-layout-2-pane
    ()
    (interactive)
    (qxf-focus-editor)
    (delete-other-windows)
    (setq qxf-window-side-bar (split-window nil (+ 120 (*get-line-number-width)) 'left))
    (set-window-buffer qxf-window-side-bar qxf-buffer-side-bar)
    (*render-side-bar)
    :defun-end)
(define-key global-map (kbd "C-c 9") 'qxf-layout-2-pane)

(defun qxf-insert-command (-command-name)
    (interactive "sCommand-name:")
    (insert (format "(defun %s
    ()
    (interactive)
    (prin1 \"Command placeholder.\")
    :defun-end)
(define-key global-map (kbd \"C-c t\") '%s)" -command-name -command-name))
    :defun-end)
(define-key global-map (kbd "C-c i") 'qxf-insert-command)

(defun qxf-set-c-offset
    ()
    (interactive)
    (set-variable 'c-basic-offset 4))
(define-key global-map (kbd "C-c 4") 'qxf-set-c-offset)

(defun *render-entry (*buffer)
    (let (
	     (*buffer-name (buffer-name *buffer))
	     )
	(if (or (string-prefix-p "*" *buffer-name) (string-prefix-p " *" *buffer-name))
	    :do-nothing
	    (insert (format "[%s]\n" *buffer-name)))
	)
    :end-defun)

(defun *get-line-number-width
    ()
    (+ 2 (length (format "%d" 
		(count-lines (point-min) (point-max))
		)
	))
    )

(defun *render-side-bar ()
    (with-current-buffer qxf-buffer-side-bar
	(erase-buffer)
	(insert (format "%s\n" (current-time-string)))
	(dolist (*buffer (buffer-list))
	    (*render-entry *buffer)))
    :end-defun)

(print "Loaded qxf-make.")

