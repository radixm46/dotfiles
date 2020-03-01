;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; emacs minimal configuration by radixM491VA
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8)
(global-display-line-numbers-mode)
(column-number-mode t)
(ido-mode t)
(global-hl-line-mode t)
(show-paren-mode t)
(setq ring-bell-function 'ignore)


;; log
(setq message-log-maxa 10000)
(setq history-length 1000)
(setq history-delete-duplicates t)

;; locale and encoding
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-coding-systems 'utf-8)
;; (setq coding-system-for-read 'utf-8) ;; conflicts ddskk
(setq coding-system-for-write 'utf-8)

(setq-default tab-width 4 indent-tabs-mode nil)

(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")
(which-function-mode 1)
(transient-mark-mode 1)


;; init package
(require 'package)

;; use custom.el for package-selected-packages
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
    (load custom-file))

(setq package-archives (append
   '(
     ("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ;; ("marmalade" . "http://marmalade-repo.org/packages/")
     ("org" . "http://orgmode.org/elpa/")
     )
   package-archives
   ))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


(setq load-prefer-newer t)


;; auto load use-package package
(unless (package-installed-p 'use-package)  ;; can't notify upgradable package
    (package-install 'use-package)
    (require 'use-package)
)

;; skk config(use default package.el)
(unless (package-installed-p 'ddskk)
  (package-install 'ddskk)
)
(package-initialize)
(setq default-input-method "japanese-skk")
(require 'skk-study)


;; -------- package config under use-package --------


;; enable evil
(use-package evil
  :ensure t
  :config (evil-mode 1)
)


;; enable relative line number
(use-package linum-relative
  :ensure t
  :config (linum-relative-global-mode)
)


;; enable doom-modeline
(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode 1)
)


(use-package neotree
  :ensure t
  :config
  (global-set-key [f7] 'neotree-toggle))


(use-package magit
  :ensure t)


(use-package git-gutter
  :ensure t)


;; org-mode config
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
