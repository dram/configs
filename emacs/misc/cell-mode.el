(defvar cell-mode-font-lock-keywords
  '(("\\<\\(after\\|and\\|apply\\|assert\\|assume\\|break\\|class\\|delete\\|elif\\|elapsed\\|else\\|every\\|fail\\|false\\|for\\|if\\|implicit\\|insert\\|let\\|loop\\|match\\|not\\|or\\|print\\|protocol\\|reactive\\|read\\|return\\|sans\\|schema\\|set\\|since\\|then\\|true\\|type\\|typevar\\|undefined\\|update\\|upon\\|using\\|volatile\\|when\\|while\\|write\\)\\>" . font-lock-keyword-face)
    ("\\(\\w+\\)\\[\\(\\w+\\)\\*?\\]\\s-+\\(\\w+\\)"
     (1 font-lock-type-face)
     (2 font-lock-type-face)
     (3 font-lock-function-name-face))))

(define-derived-mode cell-mode c-mode "Cell"
  "Major mode for editing Cell code."
  (setq-local syntax-propertize-function
              (syntax-propertize-rules ("\\(#\\)\\(#\\)"
                                        (1 "< 1b") (2 "< 2b"))))
  (font-lock-add-keywords nil cell-mode-font-lock-keywords))

(add-to-list 'auto-mode-alist '("\\.cell$" . cell-mode))

(provide 'cell-mode)
