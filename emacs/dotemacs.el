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

(server-start)

(autoload 'php-mode "php-mode.el"
  "Major mode for editing PHP files" t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(autoload 'yaml-mode "yaml-mode.el"
  "Major mode for editing YAML files" t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;(setq hs-hide-comments-when-hiding-all nil)

(setq comment-multi-line t)
(setq c-default-style "linux")

(define-key global-map (kbd "RET") 'newline-and-indent)

(setq recenter-positions '(top middle bottom))

(setq ring-bell-function 'ignore)

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; appearance

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(tool-bar-mode -1)
(menu-bar-mode -99)
(scroll-bar-mode -1)
(show-paren-mode t)
(blink-cursor-mode -1)

(setq default-indicate-buffer-boundaries 'right)

(setq resize-mini-windows t)

(setq tooltip-use-echo-area t)

(require 'color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow-day)

(setq frame-title-format '("" "[%b] - Emacs"))

;(set-default-font "Consolas-11")
(case system-type
  (gnu/linux
    (set-default-font "Inconsolata-13"))
  (windows-nt
    (set-default-font "Lucida Console-10.5")))
;(set-default-font "Lucida Sans Typewriter-10")
(setq-default line-spacing 1)

(let ((font (case system-type
	      (windows-nt "Microsoft YaHei")
	      (gnu/linux (font-spec :family "WenQuanYi Micro Hei" :size 15)))))
  (set-fontset-font (frame-parameter nil 'font) 'han font)
  (set-fontset-font (frame-parameter nil 'font) 'cjk-misc font)
  (set-fontset-font (frame-parameter nil 'font) 'symbol font))

;; ido

(require 'recentf)
(setq recentf-max-saved-items 100)
(recentf-mode t)

(require 'ido)
(ido-mode 'both)
(setq ido-use-virtual-buffers t)

;; org

(let ((set
       (create-fontset-from-fontset-spec
	(case system-type
	  (windows-nt
	   "-b&h-Lucida Console-normal-normal-*-*-14-*-*-*-m-0-fontset-orgmode")
	  (gnu/linux
	   "-*-Inconsolata-normal-normal-*-*-16-*-*-*-m-0-fontset-orgmode"))))
      (font (case system-type
	      (windows-nt "华文仿宋")
	      (gnu/linux "WenQuanYi Micro Hei"))))
  (set-fontset-font set 'han (font-spec :family font :size 16))
  (set-fontset-font set 'cjk-misc (font-spec :family font :size 16))
  (set-fontset-font set 'symbol (font-spec :family font :size 16)))

(defface org-default-buffer-face '((t :font "fontset-orgmode")) "")
(set-face-attribute 'org-default-buffer-face nil :fontset "fontset-orgmode")

(add-hook 'org-mode-hook
	  (lambda ()
	    (setq evil-auto-indent nil)))

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

;; tabbar

(require 'tabbar)
(tabbar-mode 1)

(setq tabbar-help-on-tab-function nil
      tabbar-buffer-home-button '(("") "")
      tabbar-scroll-left-button '(("") "")
      tabbar-scroll-right-button '(("") ""))

(setq tabbar-separator '(0.3))

(setq tabbar-buffer-groups-function
      (lambda ()
	(cond
	 ((eq major-mode 'eshell-mode)
	  '("Shell"))
	 ((eq major-mode 'js-mode)
	  '("Js"))
	 ((eq major-mode 'emacs-lisp-mode)
	  '("Elisp"))
	 ((eq major-mode 'org-agenda-mode)
	  '("Agenda"))
	 ((memq major-mode
		'(mingus-playlist-mode mingus-browse-mode mingus-help-mode))
	  '("Mingus"))
	 ((member (buffer-name)
		  '("*slime-events*" "*inferior-lisp*" "*slime-repl sbcl*"
		    "*slime-compilation*"))
	  '("Slime"))
	 (t (tabbar-buffer-groups)))))

(setq tabbar-buffer-list-function
      (lambda ()
	(remove-if
	 (lambda (b)
	   (or (member (buffer-name b)
		       '("*scratch*" "*Messages*"
			 "*Quail Completions*" "*Completions*"
			 "*Help*" "*Buffer List*" "*Calendar*"
			 ))))
	 (tabbar-buffer-list))))

(setq tabbar-select-tab-function
      (lambda (event tab)
	(let ((orig tabbar--buffer-show-groups)
	      (res (tabbar-buffer-select-tab event tab)))
	  (tabbar-buffer-show-groups orig)
	  res)))

(tabbar-buffer-show-groups t)

(global-set-key [(control home)] 'tabbar-press-home)
(global-set-key [(control next)] 'tabbar-forward)
(global-set-key [(control prior)] 'tabbar-backward)

(let ((bg "#282a23") (selected "#b5bd68") (unselected "#969896"))
  (set-face-attribute 'tabbar-default nil :background bg)
  (set-face-attribute 'tabbar-selected nil
		      :background bg
		      :foreground selected
		      :weight 'bold
		      :box `(:line-width 3 :color ,bg))
  (set-face-attribute 'tabbar-unselected nil
		      :background bg
		      :foreground unselected
		      :weight 'bold
		      :box `(:line-width 3 :color ,bg)))

;; evil

(require 'undo-tree)
(add-to-list 'load-path "~/emacs/evil")
(require 'evil)
(evil-set-initial-state 'mingus-help-mode 'emacs)
(evil-set-initial-state 'mingus-browse-mode 'emacs)
(evil-set-initial-state 'mingus-playlist-mode 'emacs)
(evil-mode 1)

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

;; mingus

(add-to-list 'load-path "~/emacs/mingus")
(require 'mingus)

(setq mingus-playlist-separator " ● ")

(defadvice mingus-git-out (around mingus-kill-all-buffers-when-quit activate)
  (dolist (b (buffer-list))
    (if (mingus-buffer-p (buffer-name b))
	(kill-buffer b))))

(defadvice mingus-ls (around mingus-do-not-change-header-line activate)
  (let ((orig header-line-format))
    ad-do-it
    (setq header-line-format orig)))

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

(add-to-list 'load-path "~/emacs/emacs-eim/")

(autoload 'eim-use-package "eim" "Another emacs input method")

(setq eim-use-tooltip nil)

(register-input-method
 "eim-py" "euc-cn" 'eim-use-package
 "拼音" "汉字拼音输入法" "py.txt")

(set-input-method 'eim-py)

(case system-type
  (gnu/linux
   (define-key evil-insert-state-map (kbd "C-SPC") 'toggle-input-method))
  (windows-nt
   (define-key evil-insert-state-map [apps] 'toggle-input-method)))

;; slime

(setq inferior-lisp-program "ccl")
(add-to-list 'load-path "~/emacs/slime")
(require 'slime)
(slime-setup '(slime-fancy))
(setq slime-net-coding-system 'utf-8-unix)
(setq common-lisp-hyperspec-root "/usr/share/doc/HyperSpec/")

(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))

(add-hook 'slime-mode-hook
	  (lambda ()
	    (set-variable lisp-indent-function 'common-lisp-indent-function t)
	    (local-set-key [tab] 'slime-indent-and-complete-symbol)))

(require 'autopair)

(add-hook 'emacs-lisp-mode-hook 'autopair-mode)
(add-hook 'slime-mode-hook 'autopair-mode)
(add-hook 'slime-repl-mode-hook 'autopair-mode)

(defun autopair-insert-paren ()
    (interactive)
    (setq last-command-event ?\()
    (autopair-insert-opening))

(defun autopair-close-paren ()
    (interactive)
    (setq last-command-event ?\))
    (autopair-skip-close-maybe))

(evil-define-key 'insert slime-mode-map "[" 'autopair-insert-paren)
(evil-define-key 'insert slime-repl-mode-map "[" 'autopair-insert-paren)
(evil-define-key 'insert slime-mode-map "]" 'autopair-close-paren)
(evil-define-key 'insert slime-repl-mode-map "]" 'autopair-close-paren)

;; python

(require 'python-mode)
(add-hook 'python-mode-hook (lambda ()
			      (setq outline-regexp
				    "^\\s-*class\\_>\\|^\\s-*def\\_>")))

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

(require 'diminish)

(diminish 'undo-tree-mode)
(eval-after-load "face-remap" '(diminish 'buffer-face-mode))
