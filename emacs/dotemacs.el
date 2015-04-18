; -*- mode: emacs-lisp; mode: outline-minor; outline-regexp: ";;+" -*-

;; general

(require 'cl)

(add-to-list 'load-path "~/emacs/misc")

(prefer-coding-system 'gbk-unix)
(prefer-coding-system 'utf-8-unix)

(when (eq system-type 'windows-nt)
  (set-file-name-coding-system 'gbk)
  (setenv "HOME" "d:/home/"))

(setq default-directory "~")

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'saveplace)
(setq-default save-place t)

(setq backup-by-copying t
      backup-directory-alist `(("." . "~/.emacs-saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq scroll-conservatively 100)

(require 'linum)

(autoload 'php-mode "php-mode.el"
  "Major mode for editing PHP files" t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;(setq hs-hide-comments-when-hiding-all nil)

(setq comment-multi-line t)
(setq c-default-style '((c-mode . "bsd")
			(c++-mode . "bsd")
			(other . "gnu")))

(define-key global-map (kbd "RET") 'newline-and-indent)

(setq recenter-positions '(top middle bottom))

(setq ring-bell-function 'ignore)

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defun server-stop ()
  (interactive)
  (save-buffers-kill-emacs))

(column-number-mode 1)

(setq browse-url-browser-function
      (lambda (url &rest ignore)
	(interactive)
	(shell-command (concat "lynx -dump -nolist -width=78 " url))
	(pop-to-buffer "*Shell Command Output*")))

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

(add-to-list 'custom-theme-load-path "~/emacs/theme")
(load-theme 'impact t)

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

	      (case system-type
		(gnu/linux
		 (set-frame-font "Source Code Pro-11" nil (list frame)))
		(windows-nt
		 (set-frame-font "Lucida Console-10.5" nil (list frame))))

	      (setq-default line-spacing 0)

	      (let ((font (case system-type
			    (windows-nt "Microsoft YaHei")
			    (gnu/linux (font-spec :family"WenQuanYi Micro Hei"
						  :size 16)))))
		(set-fontset-font (frame-parameter frame 'font) 'han font)
		(set-fontset-font (frame-parameter frame 'font) 'cjk-misc font)
		(set-fontset-font (frame-parameter frame 'font) 'symbol font)))))

;; evil

(when (not (require 'undo-tree nil t))
  (package-install 'undo-tree))

(when (not (require 'evil nil t))
  (package-install 'evil))

(evil-mode 1)

(evil-set-toggle-key "C-S-z")

(setq evil-want-fine-undo t)

(setq evil-search-module 'evil-search)

(defadvice outline-minor-mode (after bind-key-for-outline-minor-mode activate)
  (define-key evil-normal-state-local-map "zo" 'show-entry)
  (define-key evil-normal-state-local-map "zc" 'hide-entry)
  (define-key evil-normal-state-local-map "zm" 'hide-body)
  (define-key evil-normal-state-local-map "zr" 'show-all))

(add-hook 'evil-insert-state-exit-hook
	  (lambda ()
	    (if (eolp)
		(delete-horizontal-space t))))

(define-key evil-normal-state-map "," nil)

(define-key evil-normal-state-map ",o" #'browse-url-at-point)

(define-key evil-normal-state-map ",vv" #'vc-next-action)
(define-key evil-normal-state-map ",v=" #'vc-diff)

(add-hook 'minibuffer-exit-hook #'deactivate-input-method)

;; recentf, ido, iswitchb

(require 'recentf)
(setq recentf-max-saved-items 100)
(setq recentf-show-file-shortcuts-flag nil)
(recentf-mode t)

(evil-set-initial-state 'recentf-dialog-mode 'normal)

(define-key evil-normal-state-map ",m" (lambda ()
					 (interactive)
					 (split-window nil nil 'above)
					 (recentf-open-files)))

(evil-define-key 'normal recentf-dialog-mode-map "q"
  (lambda ()
    (interactive)
    (recentf-cancel-dialog)
    (delete-window)))

(evil-define-key 'normal recentf-dialog-mode-map (kbd "RET")
  (lambda ()
    (interactive)
    (delete-window (previous-window))
    (evil-ret)))

(require 'ido)
(ido-mode 'files)
(add-to-list 'ido-ignore-files "__pycache__")

(iswitchb-mode 1)

(define-key evil-normal-state-map ",b" 'iswitchb-buffer)
(define-key evil-normal-state-map ",f" 'ido-find-file)
(define-key evil-normal-state-map ",k" (lambda ()
					 (interactive)
					 (kill-buffer (current-buffer))))

;; org

(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (when (display-graphic-p)
	      (let ((set
		     (create-fontset-from-fontset-spec
		      (case system-type
			(windows-nt
			 "-b&h-Lucida Console-normal-normal-*-*-14-*-*-*-m-0-fontset-orgmode")
			(gnu/linux
			 "-*-Inconsolata-normal-normal-*-*-15-*-*-*-m-0-fontset-orgmode"))))
		    (font (case system-type
			    (windows-nt "华文仿宋")
			    (gnu/linux "WenQuanYi Micro Hei"))))
		(set-fontset-font set 'han (font-spec :family font :size 16))
		(set-fontset-font set 'cjk-misc (font-spec :family font :size 16))
		(set-fontset-font set 'symbol (font-spec :family font :size 16)))

	      (defface org-default-buffer-face '((t :font "fontset-orgmode")) "")
	      (set-face-attribute 'org-default-buffer-face frame
				  :fontset "fontset-orgmode"))))

(add-hook 'org-mode-hook
	  (lambda ()
	    (setq evil-auto-indent nil)
	    (define-key evil-normal-state-local-map (kbd "TAB") 'org-cycle)))

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

;; running-life

(add-to-list 'load-path "~/emacs/running-life")

(when (require 'running-life nil t)

(setq running-life-start-sound (expand-file-name "~/emacs/misc/crank.wav"))
(setq running-life-finish-sound (expand-file-name "~/emacs/misc/deskbell.wav"))
(setq running-life-text-directory "~/RunningLife")
(setq running-life-auto-insert-text
      "\n%Y/%m/%d %H:%M\n----------------\n\n.. rubric:: \n")
)

;; mpg123

(require 'mpg123 nil t)

;; mingus

(add-to-list 'load-path "~/emacs/mingus")
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

(evil-set-initial-state 'mingus-help-mode 'emacs)
(evil-set-initial-state 'mingus-browse-mode 'emacs)
(evil-set-initial-state 'mingus-playlist-mode 'emacs)

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
	    (local-set-key (kbd "\C-c\C-r") 'surround-region-with-tag)
	    (define-key evil-normal-state-local-map "zo" 'nxml-show-direct-text-content)
	    (define-key evil-normal-state-local-map "zc" 'nxml-hide-direct-text-content)
	    (define-key evil-normal-state-local-map "zm" 'nxml-hide-all-text-content)
	    (define-key evil-normal-state-local-map "zr" 'nxml-show-all)

	    (define-key evil-insert-state-local-map ";;" 'nxml-insert-tag-with-cursor-word)))

;; input method

(require 'im-agent)

(set-input-method 'im-agent)

(case system-type
  (gnu/linux
   (define-key evil-insert-state-map (kbd "C-SPC") 'toggle-input-method)
   (unless (display-graphic-p)
     (define-key evil-insert-state-map (kbd "C-@") 'toggle-input-method)))
  (windows-nt
   (define-key evil-insert-state-map [apps] 'toggle-input-method)))

;; slime

(when (not (require 'paredit nil t))
  (package-install 'paredit))

(require 'paredit)

(setq inferior-lisp-program "sbcl")

(when (require 'slime nil t)
(slime-setup '(slime-fancy))
(setq slime-net-coding-system 'utf-8-unix)
(setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")

(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))

(add-hook 'slime-mode-hook
	  (lambda ()
	    (set-variable lisp-indent-function 'common-lisp-indent-function t)
	    (local-set-key [tab] 'slime-indent-and-complete-symbol)))

(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook #'enable-paredit-mode)
)

;; arc

(when (require 'arc nil t)

(put 'tag 'arc-indent-function 1)

(add-to-list 'auto-mode-alist '("\\.arc$" . arc-mode))

(add-hook 'arc-mode-hook
	  (lambda ()
	      (setq indent-tabs-mode nil)))
)

;; scheme

(add-to-list 'auto-mode-alist '("\\.sld$" . scheme-mode))

(mapc (lambda (sym) (put sym 'scheme-indent-function 1))
      '(call-with-input-process
	call-with-port
	eval-when for guard match module parameterize unless when))

(put 'let-optionals 'scheme-indent-function 2)
(put 'define-library 'scheme-indent-function 'defun)

(font-lock-add-keywords
 'scheme-mode
 (mapcar (lambda (x)
	   (cons (concat "\\<" x "\\>") 'font-lock-keyword-face))
	 '("call-with-port"
	   "call-with-values"
	   "case-lambda"
	   "cond-expand"
	   "define-constant"
	   "define-library"
	   "define-record-type"
	   "export"
	   "for"
	   "import"
	   "match"
	   "module"
	   "parameterize"
	   "syntax-case"
	   "unless"
	   "values"
	   "when"
	   "with-syntax")))

(setq scheme-program-name "chibi")

(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'inferior-scheme-mode-hook #'enable-paredit-mode)

(add-hook 'scheme-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

;; python

(setq python-shell-virtualenv-path "~/venv/")

;; javascript

(add-hook 'js-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (hs-minor-mode)))

;; eshell

(require 'eshell)

(setq eshell-history-size 1000)

(setq eshell-cmpl-cycle-completions nil)

(add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

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

(setq tramp-auto-save-directory "~/.emacs.d/tramp-auto-saves")

;; diminish

(when (not (require 'diminish nil t))
  (package-install 'diminish))

(diminish 'undo-tree-mode)
(eval-after-load "face-remap" '(diminish 'buffer-face-mode))
