(provide 'qxf-make)

; (define-variable qxf-make-work-directory "/home/qixiaofeng/Documents/sandbox/hachi-mic-array")
; M-x set-variable RET c-basic-offset RET 4 RET

(defun qxf-make-mic-array
    ()
    (interactive)
    (shell-command "date && cd /home/qixiaofeng/Documents/sandbox/hachi-mic-array/build && make -j 8 && ./listdevs"))
(define-key global-map (kbd "C-c 1") 'qxf-make-mic-array)

(print "Loaded qxf-make.")
