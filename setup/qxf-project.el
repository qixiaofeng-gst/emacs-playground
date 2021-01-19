(provide 'qxf-project)

; Original open-project workflow:
"
Stores projects list in the user emacs directory.

Project configuration file: .qe_project, it contains
* Focus record for current project.
* File access history for current project.

1. [C-c C-o]
   1. Show a projects list. (List presented in the center of the screen.)
   2. Open one of them with a number inputed from minibuffer.
2. [C-c C-s]
   1. Scan project configuration file.
   2. Start from current directory to system root directory.
   3. Record the found project done.
3. [C-c C-c]
   1. Scan project, abort if found else continue.
   2. Create project configuration file under current directory.
"

(defun f7-scan-project
    (let*
        (
            (l4-x nil)
        )
        (princ "TODO Implement f7-scan-project.")
    )
)

(defun c6-create-project
    ()
    (interactive)
    (prin1 "Command placeholder.")
    :defun-end
)
(define-key global-map (kbd "C-c C-c") 'c6-create-project)

(print "Loaded qxf-project.")

