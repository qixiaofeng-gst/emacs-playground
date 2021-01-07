(provide 'qxf-make)

; (define-variable qxf-make-work-directory "/home/qixiaofeng/Documents/sandbox/hachi-mic-array")
; M-x set-variable RET c-basic-offset RET 4 RET

(defconst qxf-mic-array-root "/home/qixiaofeng/Documents/sandbox/hachi-mic-array")

(defun qxf-make-mic-array
    ()
    (interactive)
    (shell-command (format "date && cd %s/build && make -j 8 && ./listdevs" qxf-mic-array-root)))
(define-key global-map (kbd "C-c 1") 'qxf-make-mic-array)

(defun qxf-open-mic-array
    ()
    (interactive)
    (delete-other-windows)		; Which is bound to "C-x 1".
    (find-file (format "%s/src/listdevs.c" qxf-mic-array-root))
    (shell-command "echo Make shell area.")
    (let
	((new-window (split-window nil -20 'below)))
	(set-window-buffer new-window "*Shell Command Output*")
	(shell-command "echo Initialized shell area.")))
(define-key global-map (kbd "C-c 0") 'qxf-open-mic-array)

(defun qxf-set-c-offset
    ()
    (interactive)
    (set-variable 'c-basic-offset 4))
(define-key global-map (kbd "C-c 9") 'qxf-set-c-offset)

(print "Loaded qxf-make.")
