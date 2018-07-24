(load-file (concat (file-name-directory
                    (shell-command-to-string
                     (concat "readlink -f " load-file-name)))
                   "emacs/dotemacs.el"))
