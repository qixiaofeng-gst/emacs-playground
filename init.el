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

; Save the temporary files (ending with ~) to somewhere.
(setq backup-directory-alist
    '(("." . "~/.emacs.d/backup"))
    backup-by-copying t
    version-control t
    delete-old-versions t
    kept-new-versions 20
    kept-old-versions 5)

; TODO Use setup-general.el instead.
(add-to-list 'load-path (format "%s/setup" user-emacs-directory))
; Mandatory requirements for using (require <feature-name-symbol>):
; 1. Correctly configured load-path.
; 2. The .el file name must same as the <feature-name-symbol>.
; 3. There have to be a (provide <feature-name-symbol>) line in the .el file.
(require 'qxf-general)
(require 'qxf-helm-gtags)

; Two really useful bindings: C-j newline; C-8 DEL.
; A tutorial for use Emacs as c-ide: https://tuhdo.github.io/c-ide.html

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn t)

(setq lisp-indent-offset 4)
(global-display-line-numbers-mode)
(column-number-mode 1)
(qxf-msg (format "Loaded personal init.el cost %.3f seconds." (- (float-time) qxf-timestamp)))
