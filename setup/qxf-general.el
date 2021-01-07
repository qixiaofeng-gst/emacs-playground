(provide 'qxf-general)

(defun qxf-open-init-el
    ()
    (interactive)
    (find-file (format "%s/init.el" user-emacs-directory)))
(define-key global-map (kbd "C-c `") 'qxf-open-init-el)

(print "Printed from general package.")

