(provide 'qxf-make)

(defconst qxf-mic-array-root "/home/qixiaofeng/Documents/sandbox/hachi-mic-array")
(defconst qxf-focus-record "~/.emacs.d/backup/focus-record.txt")
(defvar qxf-buffer-side-bar (get-buffer-create "*side-bar*"))
(defvar qxf-window-editor (frame-root-window))
(defvar qxf-window-shell-out nil)
(defvar qxf-window-side-bar nil)
(defvar qxf-string-cache "")

; TODO Implement [<backtab>].
; TODO Extract print-to-buffer.
; TODO Implement point history. [C-c .] and [C-c ,] to jump.
; TODO Make the indent-sexp as I like: a brackets pair is not in same line have to be in same column. [C-c q]
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
; DONE Add a C-c a to jump to the line beginning.
; DONE Add a cmake command.
; DONE Make line copy. [C-c d]
; DONE Implement append-to-side-bar.
; DONE Assign [C-c 0] to 3 panel mode, but not jump to mic-array-root.
; DONE Assign [C-c 9] to 2 panel mode.

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

; ======= WIP =======
; {[Special Form] cond (condition [body-forms...])...}
(defun *get-index-of-char (*string *start *c)
    (*append-to-side-bar (format "%s, %d, %c" *string *start *c))
    (let
	(
	    (*result nil)
	    (*length (length *string))
	    (*index *start)
	    (*cc nil)
	    )
	(while (and (eq *result nil) (< *index *length))
	    (setq *cc (elt *string *index))
	    (cond
		((eq *cc ?\\) (setq *index (+ 2 *index)))
		((eq *cc *c) (setq *result *index))
		(t (setq *index (1+ *index)))
		)
	    )
	*result
	)
    )
; (*get-index-of-char "\\\"(\")\\\"" 0 ?\")
; 1. Check next char:
;    ": find next "
;    \: index + 2
;    (: find next )
;    default: index + 1
(defun *scan-rest (*string *start)
    (*print-to-side-bar (format "%s, %d" *string *start))
    (let*
	(
	    (*result nil)
	    (*index *start)
	    (*pair-index nil)
	    (*length (length *string))
	    (*cc nil)
	    )
	(while (and (eq *result nil) (< *index *length))
	    (setq *cc (elt *string *index))
	    (cond
		((eq *cc ?\")
		    (setq *pair-index (*get-index-of-char *string (1+ *index) ?\"))
		    (if (eq *pair-index nil)
			(setq *index (1+ *index))
			(setq *index (1+ *pair-index))
			)
		    )
		(t (setq *index (1+ *index)))
		)
	    )
	*result
	)
    )
(*scan-rest "\"hello" 0)

; 1. Atomic line does not contain any open bracket(un-paired "(" or ")").
; 2. Atomic line does not contain any "\n".
; ======= WIP =======
; TODO Know how to make data structure.
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

; {[Macro] defmacro name args [doc] [declare] body...}
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
	 (princ (quote ,*target) *out)
	 *out-string
	 )
    )
(defmacro qxf-*-stringify (*target)
    `(format "%s:%s" (qxf-*-print ,*target) ,*target)
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
	(funcall *out (qxf-*-stringify (string-match "\n" "teststring")))
	(funcall *out (format "?\\n:%s" ?\n))
	(funcall *out (format "?\\(:%s" ?\())
	(funcall *out (format "?\\):%s" ?\)))
	(funcall *out (format "?\\\":%s" ?\"))
	(funcall *out (format "?\\\\:%s" ?\\))
	(*print-to-side-bar *to-print))
    :defun-end)
(define-key global-map (kbd "C-c t") 'qxf-test-is-atomic-line)

; ======= WIP =======
(defun *format-form (*string-form)
    (let
	(
	    (*reversed (reverse *string-form))
	    (*length (length *string-form))
	    (*offset 0)
	    (*+1 (elt *string-form 0))
	    (*-1 (elt *string-form (- (length *string-form) 1))))
	(if (and (eq *+1 ?\() (eq *-1 ?\)))
	    (progn
		(setq *offset (string-match "\n" *reversed))
		(setq *string-form
		    (concat (substring *string-form 0 (- *length *offset)) ")"))
		(concat
		    *string-form
		    "\n=======\n"
		    )
		)
	    (format "Not a valid block. Start:[%c], end:[%c]. %s" *+1 *-1
		"Expected start:[(], expected end:[)].")
	    )
	)
    ) ; Here we should test with :end-defun

(defun qxf-format-lisp
    ()
    (interactive)
    (let
	(
	    (*point-o (point))
	    (*point-a nil)
	    (*point-b nil)
	    )
	(re-search-backward "\n\(")
	(setq *point-a (1+ (point)))
	(forward-sexp)
	(setq *point-b (point))
	(*print-to-side-bar
	    (*format-form (buffer-substring-no-properties *point-a *point-b))
	    )
	(goto-char *point-o)
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
    (let
	(
	    (-temp-buffer (find-file-noselect qxf-focus-record))
	    (-path-string "invalid path"))
	(setq -path-string (read -temp-buffer))
	(qxf-focus-editor)
	(find-file -path-string)
	(kill-buffer -temp-buffer)))
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

(defun *append-to-side-bar (*message)
    (with-current-buffer qxf-buffer-side-bar
	(insert *message))
    )

(defun *print-to-side-bar (*message)
    (with-current-buffer qxf-buffer-side-bar
	(erase-buffer)
	(insert (format "%s\n" (current-time-string)))
	(insert *message)))

(print "Loaded qxf-make.")
