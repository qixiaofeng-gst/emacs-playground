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
;;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; customize
(custom-set-variables
    '(helm-gtags-path-style 'relative)
    '(helm-gtags-ignore-case t)
    '(helm-gtags-auto-update t))

;; key bindings
(with-eval-after-load 'helm-gtags
    (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
    (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
    (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
    (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
    (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))

; TODO Use setup-general.el instead.
(add-to-list 'load-path (format "%s/setup" user-emacs-directory))
(require 'general)

; Two really useful bindings: C-j newline; C-8 DEL.
; A tutorial for use Emacs as c-ide: https://tuhdo.github.io/c-ide.html

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn t)

(setq lisp-indent-offset 4)
(global-display-line-numbers-mode)
(qxf-msg (format "Loaded personal init.el cost %.3f seconds." (- (float-time) qxf-timestamp)))
