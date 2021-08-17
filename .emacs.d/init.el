;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; emacs configuration by radixM491VA

;; ---------------  load package ---------------
(load "~/.emacs.d/elisp/initpkg.el")

(leaf set-appearance
  :config
  (setq inhibit-splash-screen t)
  (column-number-mode t)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode 0)
  (scroll-bar-mode -1)
  (display-time)
  (which-function-mode 1)
  (transient-mark-mode 1)
  (setq ring-bell-function 'ignore)
  ;; (global-visual-line-mode nil)
  ;; (setq line-move-visual t)
  ;; (setq word-wrap t)
  (setq default-frame-alist (append (list '(font . "HackGen35Nerd Console-14.1")
                                          '(width . 80)
                                          '(height . 55)) default-frame-alist))
  (setq font-for-tables "HackGenNerd Console")

  (leaf set-linespacing
    :config
    ;; control line spacing
    (setq-default line-spacing 0.25)
    (defun sw-lnsp (wdth)
      "switch line spacing"
      (interactive "nset line spacing: ")
      (setq line-spacing wdth))
    (defun dbl-lnsp ()
      "switch line spacing to double"
      (interactive)
      (setq line-spacing 2.0))
    )
  )

(leaf conf-cache-history
  :config
  (leaf set-cache-path
    :doc "check directory and set target"
    :config
    (let ((my-autosave-dir "~/.emacs.d/.cache/autosaved")
          (my-hist-dir "~/.emacs.d/.cache/hist"))
      (if (not (file-directory-p my-autosave-dir))
          (make-directory my-autosave-dir t))
      (if (not (file-directory-p my-hist-dir))
          (make-directory my-hist-dir t)))
    )

  (leaf autosave
    :doc "configure auto save files"
    :config
    (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/.cache/autosaved" t)))
    (setq delete-auto-save-files t)
    (setq auto-save-default t)
    (setq auto-save-timeout 15)
    (setq auto-save-interval 120)
    (setq auto-save-list-file-prefix nil))

  (leaf backup-files
    :doc "make backup files"
    :config
    (setq backup-directory-alist '((".*" .  "~/.emacs.d/.cache/hist")))
    (setq make-backup-files t)
    (setq version-control t) ;; enable version control
    (setq kept-new-versions 5)
    (setq kept-old-versions 1)
    (setq delete-old-versions t)
    (setq create-lockfiles nil))

  (leaf history
    :doc "configure history, log"
    :config
    (setq message-log-maxa 10000)
    (setq history-length 1000)
    (setq history-delete-duplicates t))
  )

;; mouse support on terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))
(defun emacs-works-on-term-p ()
  "returns t if emacs seems running on term"
  (not (or (daemonp) (display-graphic-p))))
(define-key global-map [f9] 'other-frame)


(leaf lang-locales
  :config
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
  )

(leaf editor-defaults
  :config
  ;; configure indent tab to false as default
  (setq-default truncate-lines t)
  (setq-default tab-width 4 indent-tabs-mode nil)
  (setq eol-mnemonic-dos "(CRLF)")
  (setq eol-mnemonic-mac "(CR)")
  (setq eol-mnemonic-unix "(LF)")
  )

(setq find-file-visit-truename t)

;; -------- package config under use-package --------

;; --------------- minor modes ---------------
(load "~/.emacs.d/elisp/minors.el")

;; ---------- loading doom theme  ----------
(load "~/.emacs.d/elisp/doom.el")

;; --------------- lsp package ---------------
(load "~/.emacs.d/elisp/lsp.el")

;; --------------- major modes ---------------
(load "~/.emacs.d/elisp/majors-s.el")

;; --------------- major modes ---------------
(load "~/.emacs.d/elisp/majors.el")

;; ------------ loading local.el  ------------
(if (file-exists-p "~/.emacs.d/elisp/local.el")
  (load "~/.emacs.d/elisp/local.el")
  ;; create elisp/local.el if not exists
  (with-temp-file "~/.emacs.d/elisp/local.el" nil))
