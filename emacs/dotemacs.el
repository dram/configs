; -*- mode: emacs-lisp; mode: outline-minor; outline-regexp: ";;+" -*-

(setq config-root (file-name-directory (expand-file-name load-file-name)))

;; general

(require 'cl-lib)

(add-to-list 'load-path (concat config-root "misc"))

(prefer-coding-system 'gbk-unix)
(prefer-coding-system 'utf-8-unix)

(when (eq system-type 'windows-nt)
  (set-file-name-coding-system 'gbk)
  (setenv "HOME" "d:/home/"))

(setq default-directory "~")

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'saveplace)
(save-place-mode)

(setq backup-by-copying t
      backup-directory-alist `(("." . "~/.emacs-saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq mode-require-final-newline nil)

(setq scroll-conservatively 100)

(require 'linum)

(autoload 'php-mode "php-mode.el"
  "Major mode for editing PHP files" t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;(setq hs-hide-comments-when-hiding-all nil)

(setq comment-multi-line t)
(setq c-default-style '((c-mode . "bsd")
			(other . "gnu")))

(define-key global-map (kbd "RET") 'newline-and-indent)

(setq recenter-positions '(top middle bottom))

(setq ring-bell-function 'ignore)

(setq-default tags-case-fold-search nil)

(require 'package)
(setq package-archives
      '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))
(package-initialize)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defun server-stop ()
  (interactive)
  (save-buffers-kill-emacs))

(column-number-mode 1)

(setq browse-url-browser-function #'eww-browse-url)

(global-set-key (kbd "C-c =") "Ξ")

;; appearance

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(menu-bar-mode -99)
(show-paren-mode t)
(blink-cursor-mode -1)

(setq default-indicate-buffer-boundaries 'right)

(setq resize-mini-windows nil)

(setq tooltip-use-echo-area t)

(load-theme 'modus-vivendi)

(add-hook 'rst-mode-hook
	  (lambda ()
	    (custom-set-faces
	     '(rst-level-1-face ((t nil)) t)
	     '(rst-level-2-face ((t nil)) t)
	     '(rst-level-3-face ((t nil)) t)
	     '(rst-level-4-face ((t nil)) t))))

(setq frame-title-format '("" "[%b] - Emacs"))

(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (when (display-graphic-p)
	      (set-frame-size frame 80 30)

	      (cl-case system-type
		(gnu/linux
		 (set-frame-font "Source Code Pro-11" nil (list frame)))
		(windows-nt
		 (set-frame-font "Lucida Console-10.5" nil (list frame))))

	      (setq-default line-spacing 0)

	      (let ((font (cl-case system-type
			    (windows-nt "Microsoft YaHei")
			    (gnu/linux (font-spec :family"WenQuanYi Micro Hei"
						  :size 16)))))
		(set-fontset-font (frame-parameter frame 'font) 'han font)
		(set-fontset-font (frame-parameter frame 'font) 'cjk-misc font)
		(set-fontset-font (frame-parameter frame 'font) 'symbol font)))))

;; asciidoc

(when (require 'adoc-mode nil t)
(add-to-list 'auto-mode-alist '("\\.adoc$" . adoc-mode))
)

;; ido

(require 'ido)

(setq recentf-max-saved-items 100)
(setq ido-use-virtual-buffers t)
(ido-mode t)
(add-to-list 'ido-ignore-files "__pycache__")

;; org

(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (when (display-graphic-p)
	      (let ((set
		     (create-fontset-from-fontset-spec
		      (cl-case system-type
			(windows-nt
			 "-b&h-Lucida Console-normal-normal-*-*-14-*-*-*-m-0-fontset-orgmode")
			(gnu/linux
			 "-*-Inconsolata-normal-normal-*-*-15-*-*-*-m-0-fontset-orgmode"))))
		    (font (cl-case system-type
			    (windows-nt "华文仿宋")
			    (gnu/linux "WenQuanYi Micro Hei"))))
		(set-fontset-font set 'han (font-spec :family font :size 16))
		(set-fontset-font set 'cjk-misc (font-spec :family font :size 16))
		(set-fontset-font set 'symbol (font-spec :family font :size 16)))

	      (defface org-default-buffer-face '((t :font "fontset-orgmode")) "")
	      (set-face-attribute 'org-default-buffer-face frame
				  :fontset "fontset-orgmode"))))

(mapc (lambda (hook)
	(add-hook hook 
		  (lambda ()
		    (setq buffer-face-mode-face 'org-default-buffer-face)
		    (buffer-face-mode))))
      '(org-mode-hook org-agenda-mode-hook))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)


(setq org-agenda-entry-text-maxlines 100)
(setq org-agenda-overriding-columns-format "%TODO %3PRIORITY %TAGS %25ITEM")

(setq org-startup-truncated nil)

(setq org-log-done 'time)

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/GTD/gtd.org" "Inbox")
	 "* TODO %?\nADDED: %<%Y-%m-%d %H:%M>\n")))

(setq org-refile-targets '(("~/GTD/gtd.org" :level . 1)
			   ("~/GTD/someday.org" :maxlevel . 2)
			   ("~/GTD/done.org" :level . 1)))

(setq org-agenda-files '("~/GTD/gtd.org"))
(setq org-agenda-custom-commands
      '(("a" "Office and Home Lists"
	 ((agenda "" ((org-agenda-span 'day)))
          (tags-todo "OFFICE"
		     ((org-agenda-overriding-header "")
		      (org-agenda-block-separator nil)
		      (org-agenda-tags-todo-honor-ignore-options t)
		      (org-agenda-todo-ignore-scheduled 'past)))
          (tags-todo "HOME"
		     ((org-agenda-overriding-header "")
		      (org-agenda-block-separator nil)
		      (org-agenda-tags-todo-honor-ignore-options t)
		      (org-agenda-todo-ignore-scheduled 'past)))))))

(when (eq system-type 'windows-nt)
  (setq org-export-docbook-xslt-stylesheet
	"d:/docbook/article-fo.xsl")
  (setq org-export-docbook-xslt-proc-command
	"d:/docbook/libxslt/bin/xsltproc.exe --output %o %s %i")
  (setq org-export-docbook-xsl-fo-proc-command
	"d:/docbook/fop/fop -c d:/docbook/fop.xconf %i %o"))

;; rust

(add-hook 'rust-mode-hook (lambda ()
                            (setq rust-format-on-save t)))

;; html

(add-hook 'mhtml-mode-hook (lambda ()
                             (setq indent-tabs-mode nil)))

;; cell

(require 'cell-mode)

;; cobol

(when (require 'cobol-mode nil t)
(setq cobol-source-format 'free)

(add-to-list 'auto-mode-alist '("\\.\\(cbl\\|cob\\|cpy\\)$" . cobol-mode))

(add-hook 'cobol-mode-hook (lambda ()
                             (setq indent-tabs-mode nil)
                             (setq indent-line-function 'insert-tab)
                             (setq tab-width 4)))
)

;; sam

(add-to-list 'auto-mode-alist '("\\.sam$" . text-mode))

;; lml

(add-to-list 'auto-mode-alist '("\\.lml$" . text-mode))

;; running-life

(add-to-list 'load-path (concat config-root "running-life"))

(when (require 'running-life nil t)

(setq running-life-start-sound (expand-file-name (concat config-root
                                                         "misc/crank.wav")))
(setq running-life-finish-sound (expand-file-name (concat config-root
                                                          "misc/deskbell.wav")))
(setq running-life-text-directory "~/RunningLife")
(setq running-life-auto-insert-text
      "\n%Y/%m/%d %H:%M\n----------------\n\n.. rubric:: \n")
)

;; mpg123

(require 'mpg123 nil t)

;; mingus

(add-to-list 'load-path (concat config-root "mingus"))
(when (require 'mingus nil t)

(setq mingus-playlist-separator " ● ")

(defadvice mingus-git-out (around mingus-kill-all-buffers-when-quit activate)
  (dolist (b (buffer-list))
    (if (mingus-buffer-p (buffer-name b))
	(kill-buffer b))))

(defadvice mingus-ls (around mingus-do-not-change-header-line activate)
  (let ((orig header-line-format))
    ad-do-it
    (setq header-line-format orig)))
)

;; nxml

(setq nxml-slash-auto-complete-flag t)

(defun surround-region-with-tag (tag-name beg end)
  (interactive "sTag name: \nr")
  (save-excursion
    (goto-char beg)
    (insert "<" tag-name ">")
    (goto-char (+ end 2 (length tag-name)))
    (insert "</" tag-name ">")))

(defun nxml-insert-tag-with-cursor-word ()
  (interactive)
  (backward-kill-word 1)
  (let ((tag-name (current-kill 0)))
    (insert "<" tag-name ">" "</" tag-name ">")
    (left-char (+ 3 (length tag-name)))))

(add-hook 'nxml-mode-hook
	  (lambda ()
	    (local-set-key (kbd "\C-c\C-r") 'surround-region-with-tag)))

;; input method

; (when (require 'im-agent nil t)
;   (set-input-method 'im-agent))

;; slime

(when (require 'paredit nil t)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook #'enable-paredit-mode)
)

(when (require 'slime nil t)
(slime-setup '(slime-fancy slime-indentation))

(setq slime-lisp-implementations '((sbcl ("sbcl")) (ecl ("ecl"))))
(setq slime-net-coding-system 'utf-8-unix)
(setq common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec/")

(add-to-list 'auto-mode-alist '("\\.lisp-expr$" . lisp-mode))

(add-hook 'lisp-mode-hook 'hs-minor-mode)

(add-hook 'slime-mode-hook
	  (lambda ()
	    (local-set-key [tab] 'slime-indent-and-complete-symbol)))

)

;; arc

(when (require 'arc nil t)

(put 'tag 'arc-indent-function 1)

(add-to-list 'auto-mode-alist '("\\.arc$" . arc-mode))

)

;; scheme

(add-to-list 'auto-mode-alist '("\\.sld$" . scheme-mode))

(mapc (lambda (sym) (put sym 'scheme-indent-function 1))
      '(call-with-input-process
	call-with-port
	eval-when for guard match module))

(put 'let-optionals 'scheme-indent-function 2)

(font-lock-add-keywords
 'scheme-mode
 (mapcar (lambda (x)
	   (cons (concat "\\<" x "\\>") 'font-lock-keyword-face))
	 '("call-with-port"
	   "call-with-values"
	   "define-constant"
	   "export"
	   "for"
	   "import"
	   "match"
	   "module"
	   "syntax-case"
	   "values"
	   "with-input-from-file"
	   "with-output-to-file"
	   "with-syntax")))

(setq scheme-program-name "chibi")

(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'inferior-scheme-mode-hook #'enable-paredit-mode)

;; clips

(when (require 'clips-mode nil t)
(put 'call-with-input-file 'clips-indent-function 1)
(put 'call-with-input-process 'clips-indent-function 2)
(put 'call-with-output-file 'clips-indent-function 1)
(put 'call-with-output-process 'clips-indent-function 2)

(add-hook 'clips-mode-hook #'enable-paredit-mode)

)

;; perl

(defalias 'perl-mode 'cperl-mode)

;; prolog

(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.pro$" . prolog-mode))

(add-hook 'prolog-mode-hook (lambda ()
			      (setq indent-tabs-mode nil)))

;; mercury

(add-to-list 'auto-mode-alist '("\\.m$" . mercury-mode))

(add-hook 'mercury-mode-hook (lambda ()
                               ;; dirty hack, reset with new `prolog-system'
                               (prolog-mode-variables)
                               ;; set `.' as a word constituent
                               (modify-syntax-entry ?. "w")
                               ;; dirty hack for `prolog-font-lock-keywords'
                               (setq-local major-mode 'prolog-mode)))

;; logtalk

(add-to-list 'auto-mode-alist '("\\.lgt$" . prolog-mode))

;; poplog

(when (require 'pop-mode nil t)

(setq pop-program-name "poplog pop11")

(setenv "usepop" (file-name-directory
                  (shell-command-to-string "readlink -f $(which poplog)")))

(add-hook 'pop-mode-hook (lambda ()
                           (setq-local tab-width 4)
                           (setq-local indent-tabs-mode t)))
)

;; erlang

(add-to-list 'interpreter-mode-alist '("escript" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl$" . erlang-mode))

(setq erlang-indent-level 2)

(add-hook 'erlang-mode-hook (lambda ()
                              (setq indent-tabs-mode nil)
                              (add-to-list 'write-file-functions
                                           #'delete-trailing-whitespace)))

;; c++

(add-hook 'c++-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; modula-2

(add-to-list 'auto-mode-alist '("\\.def$" . modula-2-mode))
(add-to-list 'auto-mode-alist '("\\.mod$" . modula-2-mode))

(setq-default m2-indent 2)

(add-hook 'm2-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; pascal

(setq-default pascal-indent-level 2)

(add-hook 'pascal-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; sml

(add-to-list 'auto-mode-alist '("\\.fun$" . sml-mode))
(add-to-list 'auto-mode-alist '("\\.mlb$" . sml-mode))
(add-hook 'sml-mode-hook (lambda () (setq indent-tabs-mode nil)))

;; self

(add-to-list 'auto-mode-alist '("\\.self$" . text-mode))

;; python

(setq python-shell-virtualenv-path "~/venv/")

;; java

(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 4
                  indent-tabs-mode nil)))

;; javascript

(setq js-indent-level 2)

(add-hook 'js-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (hs-minor-mode)))

;; eshell

(require 'eshell)

(setq eshell-history-size 1000)

(setq eshell-cmpl-cycle-completions nil)

(add-hook 'eshell-output-filter-functions #'eshell-truncate-buffer)

(defun eshell/e (file)
  (find-file file))

(defun eshell/start-bg (cmd &rest args)
  (let ((arg-string (reduce (lambda (res x) (concat res " " x))
			    args)))
    (w32-shell-execute 1 cmd arg-string 0)))

(defun eshell/clear ()
  (interactive)
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))

(defadvice eshell/cd (around eshell/cd-expand-without-cdpath activate)
  (if (ad-get-arg 0)
      (ad-set-arg 0 (expand-file-name (ad-get-arg 0))))
  ad-do-it)

;; tramp

(require 'tramp)

(setq tramp-default-method "plink")

(setq tramp-verbose 2)

(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(setq tramp-auto-save-directory "~/.emacs.d/tramp-auto-saves")
