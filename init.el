(defun qxf-msg (m) (print (format "[Msg by xiaofeng.qi]: %s" m)))
(defvar qxf-timestamp)
(setq qxf-timestamp (float-time))
(qxf-msg "Loading personal init.el.")

; Initialize 'straight'.
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
	(with-current-buffer
	    (url-retrieve-synchronously
		"https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
		'silent 'inhibit-cookies)
	    (goto-char (point-max))
	    (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

; Use 'Helm'
(straight-use-package 'helm)
(straight-use-package 'helm-gtags)
; Enable Helm
(helm-mode 1)

; Two really useful bindings: C-j newline; C-8 DEL.
; A tutorial for use Emacs as c-ide: https://tuhdo.github.io/c-ide.html

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn t)

(setq lisp-indent-offset 4)
(global-display-line-numbers-mode)
(qxf-msg (format "Loaded personal init.el cost %.3f seconds." (- (float-time) qxf-timestamp)))
