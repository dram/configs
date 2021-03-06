(require 'cc-mode)

(require 'cc-langs)

(c-add-language 'cell-mode 'c-mode)

(setq cell-font-lock-extra-types '("[[:upper:]]\\sw*"))

(defvar cell-mode-font-lock-keywords
  `((":\\(\\sw+\\)" . (1 font-lock-constant-face))
    ("[(,]\\(\\s-*\\sw+\\):" . (1 font-lock-reference-face))
    (,(concat "\\<"
              (c-make-keywords-re nil
                '("after" "and" "apply" "assert" "assume" "break"
                  "class" "delete" "elif" "elapsed" "else" "every"
                  "fail" "false" "for" "if" "implicit" "insert" "let"
                  "loop" "match" "not" "or" "print" "protocol"
                  "reactive" "read" "return" "sans" "schema" "set"
                  "since" "then" "true" "type" "typevar" "undefined"
                  "update" "upon" "using" "volatile" "when" "while"
                  "write"))
              "\\>")
     . font-lock-keyword-face)
    (,(concat "\\<"
              (c-make-keywords-re nil
                '("abs" "all" "any" "append" "bit" "bwand" "bwor"
                  "chars" "difference" "disjoint" "drop" "apply" "error"
                  "failed" "intermix" "intersection" "is_digit"
                  "is_lower" "is_space" "is_upper" "isort" "join" "just"
                  "length" "lower" "max" "maybe" "merge" "min" "mod"
                  "nat" "none" "nothing" "only" "result" "reverse"
                  "slice" "sqrt" "string" "subset" "substr" "succeeded"
                  "sum" "take" "union" "untag" "unzip" "unzip3" "upper"
                  "value" "value_unsafe" "values" "xor" "zip\\)\\>"))
              "\\>")
     . font-lock-builtin-face)))

(define-derived-mode cell-mode c-mode "Cell"
  "Major mode for editing Cell code."
  (setq-local syntax-propertize-function
              (syntax-propertize-rules ("\\(#\\)\\(#\\)"
                                        (1 "< 1b") (2 "< 2b"))))

  (font-lock-add-keywords nil c-font-lock-keywords-2 'set)
  (font-lock-add-keywords nil cell-mode-font-lock-keywords)

  (c-initialize-cc-mode t)
  (c-init-language-vars cell-mode)
  (c-common-init 'cell-mode)

  (electric-indent-mode -1)
  (setq-local indent-line-function 'insert-tab)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2))

(add-to-list 'auto-mode-alist '("\\.cell$" . cell-mode))

(provide 'cell-mode)
