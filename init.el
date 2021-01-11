(defvar qxf-timestamp (float-time))
(defun qxf-msg (m) (print (format "[Msg by xiaofeng.qi]: %s" m)))
(qxf-msg "Loading personal init.el.")

; Mandatory requirements for using (require <feature-name-symbol>):
; 1. Correctly configured load-path.
; 2. The .el file name must same as the <feature-name-symbol>.
; 3. There have to be a (provide <feature-name-symbol>) line in the .el file.
(add-to-list 'load-path (format "%s/setup" user-emacs-directory))
(require 'qxf-utils)
(require 'qxf-general)
(require 'qxf-make)

; Two really useful bindings: C-j newline; C-8 DEL.
; A tutorial for use Emacs as c-ide: https://tuhdo.github.io/c-ide.html

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn t)

(qxf-msg (format "Loaded personal init.el cost %.3f seconds." (- (float-time) qxf-timestamp)))
