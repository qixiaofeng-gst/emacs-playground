(provide 'qxf-sidebar)
(require 'qxf-utils)

(defvar g5-sidebar-keymap
    (let
        (
            (l4-keymap (make-sparse-keymap))
        )
        (m4-bind "x" c6-focus-editor l4-keymap)
        l4-keymap
    )
    "The keymap for sidebar."
)

(define-derived-mode f7-sidebar-mode special-mode "Sidebar"
    (kill-all-local-variables)
    (use-local-map g5-sidebar-keymap)
    (setq
        major-mode 'f7-sidebar-mode
        display-line-numbers t
        mode-name "Sidebar"
    )
)

(princ "Loaded qxf-sidebar.\n")

