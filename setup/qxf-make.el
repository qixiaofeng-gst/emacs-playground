(provide 'qxf-make)

(defconst qxf-mic-array-root "/home/qixiaofeng/Documents/sandbox/hachi-mic-array")
(defconst qxf-focus-record "~/.emacs.d/backup/focus-record.txt")
(defvar qxf-editor-window (frame-root-window))
(defvar qxf-side-bar nil)
(defvar qxf-string-cache "")

; TODO Sidebar for available buffers.
; TODO Hide the menu bar in qxf-general.el.
; TODO Implement line movement.
; TODO Make the editor 120+<number-columns>.
; TODO Make the indent-sexp as I like: a brackets pair is not in same line have to be in same column.

; DONE Create copy and paste logic.

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
    (select-window qxf-editor-window))
(define-key global-map (kbd "C-c -") 'qxf-focus-editor)

(defun qxf-focus-side-bar
    ()
    (interactive)
    (select-window qxf-side-bar))
(define-key global-map (kbd "C-c \\") 'qxf-focus-side-bar)

(defun qxf-make-mic-array
    ()
    (interactive)
    (qxf-focus-editor)
    (shell-command (format "date && cd %s/build && make -j 8 && ./listdevs" qxf-mic-array-root)))
(define-key global-map (kbd "C-c 1") 'qxf-make-mic-array)

(defun qxf-open-mic-array
    ()
    (interactive)
    (qxf-focus-editor)
    (delete-other-windows)		; Which is bound to "C-x 1".
    (find-file (format "%s/src/listdevs.c" qxf-mic-array-root))
    (shell-command "echo Make shell area.")
    (let
	((-new-window (split-window nil -20 'below)))
	(set-window-buffer -new-window "*Shell Command Output*")
	(setq qxf-side-bar (split-window nil 120 'left))
	(set-window-buffer qxf-side-bar (get-buffer-create "*side-bar*"))
	(shell-command "echo Initialized shell area.")))
(define-key global-map (kbd "C-c 0") 'qxf-open-mic-array)

(defun qxf-set-c-offset
    ()
    (interactive)
    (set-variable 'c-basic-offset 4))
(define-key global-map (kbd "C-c 9") 'qxf-set-c-offset)

(print "Loaded qxf-make.")
