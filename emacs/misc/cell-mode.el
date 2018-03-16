(defvar cell-mode-set-type-signature-regex
  ;; [Foo]
  "\\[\\([[:upper:]]\\w*\\)\\*?\\]")
(defvar cell-mode-generic-type-signature-regex
  ;; Foo[Bar]
  "\\([[:upper:]]\\w*\\)\\[\\([[:upper:]]\\w*\\)\\*?\\]")
(defvar cell-mode-map-type-signature-regex
  ;; [Foo -> Bar]
  "\\[\\([[:upper:]]\\w*\\)\\*?\\s-*->\\s-*\\([[:upper:]]\\w*\\)\\*?\\]")

(defvar cell-mode-font-lock-keywords
  `(("\\<\\(after\\|and\\|apply\\|assert\\|assume\\|break\\|class\\|delete\\|elif\\|elapsed\\|else\\|every\\|fail\\|false\\|for\\|if\\|implicit\\|insert\\|let\\|loop\\|match\\|not\\|or\\|print\\|protocol\\|reactive\\|read\\|return\\|sans\\|schema\\|set\\|since\\|then\\|true\\|type\\|typevar\\|undefined\\|update\\|upon\\|using\\|volatile\\|when\\|while\\|write\\)\\>" . font-lock-keyword-face)
    (":: \\([[:upper:]]\\w*\\)"
     (1 font-lock-type-face))
    (,cell-mode-set-type-signature-regex
     (1 font-lock-type-face))
    (,cell-mode-generic-type-signature-regex
     (1 font-lock-type-face)
     (2 font-lock-type-face))
    (,cell-mode-map-type-signature-regex
     (1 font-lock-type-face)
     (2 font-lock-type-face))
    (,(concat cell-mode-set-type-signature-regex "\\s-*\\(\\w+\\)(")
     (1 font-lock-type-face)
     (2 font-lock-function-name-face))
    (,(concat cell-mode-generic-type-signature-regex "\\s-*\\(\\w+\\)(")
     (1 font-lock-type-face)
     (2 font-lock-type-face)
     (3 font-lock-function-name-face))
    (,(concat cell-mode-map-type-signature-regex "\\s-*\\(\\w+\\)(")
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
