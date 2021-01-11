(provide 'qxf-general)

(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)

(setq lisp-indent-offset 4)

; Save the temporary files (ending with ~) to somewhere.
(setq backup-directory-alist
    '(("." . "~/.emacs.d/backup"))
    backup-by-copying t
    version-control t
    delete-old-versions t
    kept-new-versions 20
    kept-old-versions 5)

; TODO Move functions here.

(defun qxf-open-init-el
    ()
    (interactive)
    (find-file (format "%s/init.el" user-emacs-directory)))
(define-key global-map (kbd "C-c `") 'qxf-open-init-el)

(print "Loaded qxf-general.")

