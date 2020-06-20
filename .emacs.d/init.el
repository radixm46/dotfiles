;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; emacs configuration by radixM491VA
(setq inhibit-splash-screen t)
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8)
(global-display-line-numbers-mode)
(column-number-mode t)
(ido-mode t)
(show-paren-mode t)
(setq ring-bell-function 'ignore)
(setq-default truncate-lines t)
; (global-visual-line-mode nil)
; (setq line-move-visual t)
; (setq word-wrap t)
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq delete-auto-save-files t)
(setq auto-save-default nil)

(add-hook 'prog-mode-hook
          '(lambda () (electric-pair-mode t)))

(setq default-frame-alist
      (append (list
               '(font . "HackGenNerd Console-16"))
  default-frame-alist))
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(display-time)


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

;; configure tab
(setq-default tab-width 4 indent-tabs-mode nil)

(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")
(which-function-mode 1)
(transient-mark-mode 1)


; ---------------  load package ---------------
(load "~/.emacs.d/elisp/initpkg.el")

;; -------- package config under use-package --------


;; enable evil
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-u") 'scroll-down-command)
  ;; enable emacs cursor movement in insert mode
  (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-f") 'forward-char)
  (define-key evil-insert-state-map (kbd "C-b") 'backward-char)
  ;(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
  ;(define-key evil-insert-state-map (kbd "C-n") 'next-line)
)


; ---------- loading doom theme  ----------
(load "~/.emacs.d/elisp/doom.el")


;; enable relative line number
(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-backend 'display-line-numbers-mode)
  (linum-relative-global-mode)
)


(use-package all-the-icons
  :ensure t
) ;; need installation by all-the-icons-color-install-fonts


(use-package highlight-indent-guides
  :ensure t
  :hook
    (prog-mode . highlight-indent-guides-mode)
  :config
    (setq highlight-indent-guides-method 'column)
    (setq highlight-indent-guides-auto-odd-face-perc 10)
    (setq highlight-indent-guides-auto-even-face-perc 10)
    ;(setq highlight-indent-guides-auto-character-face-perc 20)
    (setq highlight-indent-guides-auto-enabled t)
    (setq highlight-indent-guides-responsive 'top)
    (setq highlight-indent-guides-delay 0)
)


(use-package rainbow-delimiters
  :ensure t
  :hook
    (prog-mode . rainbow-delimiters-mode)
)


;; configure whitespace
(use-package whitespace
  :config
  (global-whitespace-mode t)
  (setq whitespace-global-modes
    '(not dired-mode tar-mode neotree))
  (setq whitespace-line-column 80)
  (setq whitespace-style
    '(face  ; enable
      trailing
      tabs  ; conflicts highlight indent mode
      space-mark
      tab-mark
      ;newline
      ;newline-mark
      ;empty  ; empty line
      ;lines-tail
      ;spaces
  ))
  (setq whitespace-display-mappings
    '(
       (tab-mark ?\t [?\xBB ?\t] [?\\ ?\t])
       (newline-mark ?\n [?\x21B2 ?\n])  ; display when in some mejor mode
     )
  )
  ;; fix color
  (set-face-attribute whitespace-trailing nil
    :foreground "DeepPink"
    :background (face-attribute 'default :background)
    :underline t)
  (set-face-attribute whitespace-tab nil
    :foreground "gray22"
    :background nil
    :underline t)
  ;(set-face-attribute 'whitespace-newline nil
  ;  :foreground "SlateGray"
  ;  :background nil
  ;  :underline nil)
  ;(set-face-attribute 'whitespace-space nil
  ;  :background my/bg-color
  ;  :foreground "GreenYellow"
  ;  :weight 'bold)
  ;(set-face-attribute 'whitespace-empty nil
  ;  :background my/bg-color)
)


(use-package magit
  :ensure t)


(use-package git-gutter
  :ensure t
  :custom
    (git-gutter:modified-sign "~")
    (git-gutter:added-sign    "+")
    (git-gutter:deleted-sign  "-")
  :config
    (global-git-gutter-mode +1)
  )


(use-package flycheck
 :ensure t)


(use-package which-key
  :ensure t
  :config
  (which-key-mode)
)


; --------------- lsp package ---------------
(load "~/.emacs.d/elisp/lsp.el")

; --------------- major modes ---------------
(load "~/.emacs.d/elisp/modes.el")

; ------------ loading local.el  ------------
(if (file-exists-p "~/.emacs.d/elisp/local.el")
  (load "~/.emacs.d/elisp/local.el")
  ; create elisp/local.el if not exists
  (with-temp-file "~/.emacs.d/elisp/local.el" nil)
)
