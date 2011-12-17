;;; tomorrow-theme.el --- GNU Emacs 24 port of the Tomorrow Theme.

;;; Commentary

;; Originally by Chris Kempson https://github.com/ChrisKempson/Tomorrow-Theme
;; Ported to GNU Emacs by Chris Charles,
;; ported to Emacs 24 by Xin Wang.

;;; Code:

(deftheme tomorrow "Tomorrow")

(let ((class '((class color) (min-colors 89)))
      (background "#ffffff")
      (current-line "#e9efff")
      (selection "#c5cce9")
      (foreground "#4d4d4c")
      (comment "#8e908c")
      (cursor "#aeafad")
      (red "#c82829")
      (orange "#f5871f")
      (yellow "#eab700")
      (green "#718c00")
      (aqua "#3e999f")
      (blue "#4271ae")
      (purple "#8959a8"))

  (custom-theme-set-faces
   'tomorrow
   `(default ((t (:background ,background :foreground ,foreground))))
   `(fringe ((t (:background ,current-line))))
   `(minibuffer-prompt ((t (:foreground ,blue))))
   `(mode-line ((t (:background ,current-line :foreground ,foreground))))
   `(region ((t (:background ,selection))))

   ;; Font-lock stuff
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground ,green))))
   `(font-lock-doc-string-face ((t (:foreground ,comment))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,purple))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,red))))
   `(font-lock-warning-face ((t (:foreground ,red))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,current-line))))

   ;; linum-mode
   `(linum ((t (:background ,current-line :foreground ,foreground))))

   ;; org-mode
   `(org-date ((t (:foreground ,purple))))
   `(org-done ((t (:foreground ,green))))
   `(org-hide ((t (:foreground ,background))))
   `(org-link ((t (:foreground ,blue))))
   `(org-todo ((t (:foreground ,aqua))))
   `(org-column ((t (:background ,background))))
   `(org-scheduled-previously ((t (:foreground ,green))))
   `(org-scheduled-today ((t (:foreground ,green))))
   `(org-scheduled ((t (:foreground "#99cc99"))))
   `(org-agenda-done ((t (:foreground ,comment))))
   `(org-agenda-structure ((t (:foreground ,blue))))

   ;; show-paren-mode
   `(show-paren-match-face ((t (:background ,blue :foreground ,current-line))))
   `(show-paren-mismatch-face ((t (:background ,orange :foreground ,current-line))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,green))))
   `(eshell-ls-directory ((,class (:foreground ,blue))))
   `(eshell-ls-executable ((,class (:foreground ,green))))
   `(eshell-ls-symlink ((,class (:foreground ,purple))))
   `(eshell-ls-archive ((,class (:foreground ,orange))))

   ;; nxml
   `(nxml-element-local-name ((,class (:foreground ,comment))))
   `(nxml-tag-delimiter ((,class (:foreground "#C9CCC7"))))
   `(nxml-tag-slash ((,class (:foreground "#C9CCC7"))))

   ;; tabbar
   `(tabbar-default ((,class (:background ,current-line))))
   `(tabbar-selected ((,class (:background ,current-line
			       :foreground ,green
			       :weight bold
			       :box (:line-width 3 :color ,current-line)))))
   `(tabbar-unselected ((,class (:background ,current-line
			       :foreground ,comment
			       :weight bold
			       :box (:line-width 3 :color ,current-line)))))
   ))


(provide-theme 'tomorrow)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; tomorrow-theme.el ends here
