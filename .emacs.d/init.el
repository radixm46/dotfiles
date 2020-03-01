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
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

(setq-default tab-width 4 indent-tabs-mode nil)

(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")
(which-function-mode 1)
(transient-mark-mode 1)


;; init package
(require 'package)

(setq package-archives (append
   '(
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ;; ("marmalade" . "http://marmalade-repo.org/packages/")
     ("org" . "http://orgmode.org/elpa/")
     )
   package-archives
   ))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


(setq load-prefer-newer t)

(defvar required-packages
  '(
    ddskk
    evil linum-relative
    doom-modeline
    neotree

    ;; git
    magit git-gutter
    ))


(dolist (pkg required-packages)
  (unless (package-installed-p pkg)  ;; can't notify upgradable package
    ;; (package-install pkg)
    ;; (package-initialize)
))

;; skk config
(when (require 'skk nil t)
  (setq default-input-method "japanese-skk")
  (require 'skk-study)
)

;; enable evil
(when (require 'evil nil t)
  (evil-mode 1)
)


;; enable relative line number
(when (require 'linum-relative nil t)
 (linum-relative-global-mode)
 )


;; enable doom-modeline
(when (require 'doom-modeline nil t)
  (doom-modeline-mode 1)
)


;; org-mode config
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (ddskk))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
