;;; atelier-theme.el - Based on Atelier Seaside Scheme

;;; based on:
;;;   http://atelierbram.github.io/syntax-highlighting/atelier-schemes/seaside/

(deftheme atelier "Aterlier Seaside")

(custom-theme-set-faces
 'atelier

 '(default ((t (:background "black" :foreground "white"))))
 '(minibuffer-prompt ((t (:foreground "white"))))
 '(mode-line ((t (:background "black" :foreground "white" :underline t))))
 '(region ((t (:background "white" :foreground "black"))))

 ;; font-lock
 '(font-lock-builtin-face ((t (:foreground "magenta"))))
 '(font-lock-comment-face ((t (:foreground "brightblack"))))
 '(font-lock-constant-face ((t (:foreground "cyan"))))
 '(font-lock-doc-string-face ((t (:foreground "cyan"))))
 '(font-lock-function-name-face ((t (:foreground "blue"))))
 '(font-lock-keyword-face ((t (:foreground "magenta"))))
 '(font-lock-string-face ((t (:foreground "green"))))
 '(font-lock-type-face ((t (:foreground "yellow"))))
 '(font-lock-variable-name-face ((t (:foreground "cyan"))))
 '(font-lock-warning-face ((t (:foreground "yellow"))))
 '(font-lock-preprocessor-face ((t (:foreground "brightwhite"))))

 ;; cperl
 '(cperl-array-face ((t (:background "black" :foreground "cyan"))))
 '(cperl-hash-face ((t (:background "black" :foreground "cyan"))))

 ;; diff
 '(diff-header ((t (:background "black" :foreground "green"))))
 '(diff-file-header ((t (:background "black" :foreground "green"))))
 '(diff-added ((t (:background "black" :foreground "brightcyan"))))
 '(diff-removed ((t (:background "black" :foreground "brightblue"))))

 ;; eshell
 '(eshell-prompt ((t (:foreground "green"))))
 '(eshell-ls-directory ((t (:foreground "blue"))))
 '(eshell-ls-executable ((t (:foreground "green"))))
 '(eshell-ls-symlink ((t (:foreground "magenta"))))
 '(eshell-ls-archive ((t (:foreground "yellow"))))

 ;; hl-line
 '(hl-line ((t (:background "blue"))))

 ;; isearch
 '(isearch ((t (:background "yellow" :foreground "white"))))

 ;; linum
 '(linum ((t (:background "grey" :foreground "white"))))

 ;; nxml
 '(nxml-tag-delimiter ((t (:foreground "#C9CCC7"))))
 '(nxml-tag-slash ((t (:foreground "#C9CCC7"))))

 ;; org
 '(org-date ((t (:foreground "magenta"))))
 '(org-done ((t (:foreground "green"))))
 '(org-hide ((t (:foreground "black"))))
 '(org-link ((t (:foreground "blue"))))
 '(org-todo ((t (:foreground "aqua"))))
 '(org-column ((t (:background "black"))))
 '(org-scheduled-previously ((t (:foreground "green"))))
 '(org-scheduled-today ((t (:foreground "green"))))
 '(org-scheduled ((t (:foreground "green"))))
 '(org-agenda-done ((t (:foreground "blue"))))
 '(org-agenda-structure ((t (:foreground "blue"))))

 ;; show-paren
 '(show-paren-match-face ((t (:background "blue" :foreground "white"))))
 '(show-paren-mismatch-face ((t (:background "yellow" :foreground "black"))))

 )

(provide-theme 'atelier)
