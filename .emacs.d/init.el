;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; emacs configuration by radixM491VA
(setq inhibit-splash-screen t)
(column-number-mode t)
(setq ring-bell-function 'ignore)
(setq-default truncate-lines t)
;; (global-visual-line-mode nil)
;; (setq line-move-visual t)
;; (setq word-wrap t)
(setq create-lockfiles nil)

;; check directory and set target
(let ((my-autosave-dir "~/.emacs.d/.cache/autosaved")
      (my-hist-dir "~/.emacs.d/.cache/hist"))
  (if (not (file-directory-p my-autosave-dir))
      (make-directory my-autosave-dir t))
  (if (not (file-directory-p my-hist-dir))
      (make-directory my-hist-dir t)))

;; configure auto save files
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/.cache/autosaved" t)))
(setq delete-auto-save-files t)
(setq auto-save-default t)
(setq auto-save-timeout 15)
(setq auto-save-interval 120)
(setq auto-save-list-file-prefix nil)

;; make backup files
(setq backup-directory-alist '((".*" .  "~/.emacs.d/.cache/hist")))
(setq make-backup-files t)
(setq version-control t) ;; enable version control
(setq kept-new-versions 5)
(setq kept-old-versions 1)
(setq delete-old-versions t)

(setq default-frame-alist (append (list '(font . "HackGen35Nerd Console-15")
                                        '(width . 80)
                                        '(height . 55)) default-frame-alist))
(setq font-for-tables "HackGenNerd Console")
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(display-time)

(define-key global-map [f9] 'other-frame)

;; mouse support on terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))


;; control line spacing
(setq-default line-spacing 0.25)

(defun sw-lnsp (wdth)
  (interactive "nset line spacing: ")
  (setq line-spacing wdth))
(defun dbl-lnsp ()
  (interactive)
  (setq line-spacing 2.0))

;; log
(setq message-log-maxa 10000)
(setq history-length 1000)
(setq history-delete-duplicates t)

;; locale and encoding
(set-locale-environment nil)
(set-default 'buffer-file-coding-system 'utf-8)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-coding-systems 'utf-8)
;; (setq coding-system-for-read 'utf-8) ;; conflicts ddskk
(setq coding-system-for-write 'utf-8)

;; configure indent tab to false as default
(setq-default tab-width 4 indent-tabs-mode nil)

(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")
(which-function-mode 1)
(transient-mark-mode 1)

(setq find-file-visit-truename t)


;; ---------------  load package ---------------
(load "~/.emacs.d/elisp/initpkg.el")

;; -------- package config under use-package --------
(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-arguments (list "-l"))
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package all-the-icons
  :ensure t) ;; need installation by all-the-icons-install-fonts

;; --------------- lsp package ---------------
(load "~/.emacs.d/elisp/lsp.el")

;; --------------- minor modes ---------------
(load "~/.emacs.d/elisp/minors.el")

;; ---------- loading doom theme  ----------
(load "~/.emacs.d/elisp/doom.el")

;; --------------- major modes ---------------
(load "~/.emacs.d/elisp/majors-s.el")

;; --------------- major modes ---------------
(load "~/.emacs.d/elisp/majors.el")

;; ------------ loading local.el  ------------
(if (file-exists-p "~/.emacs.d/elisp/local.el")
  (load "~/.emacs.d/elisp/local.el")
  ;; create elisp/local.el if not exists
  (with-temp-file "~/.emacs.d/elisp/local.el" nil))
