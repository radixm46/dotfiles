;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; emacs configuration by radixM491VA
(setq inhibit-splash-screen t)
(global-display-line-numbers-mode)
(column-number-mode t)
(show-paren-mode t)
(setq ring-bell-function 'ignore)
(setq-default truncate-lines t)
; (global-visual-line-mode nil)
; (setq line-move-visual t)
; (setq word-wrap t)
(setq create-lockfiles nil)

; check directory and set target
(let ((my-autosave-dir "~/.emacs.d/.tmp/autosaved")
      (my-hist-dir "~/.emacs.d/.tmp/hist"))
  (if (not (file-directory-p my-autosave-dir))
      (make-directory my-autosave-dir t))
  (if (not (file-directory-p my-hist-dir))
      (make-directory my-hist-dir t))
  )

; configure auto save files
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/.tmp/autosaved" t)))
(setq delete-auto-save-files t)
(setq auto-save-default t)
(setq auto-save-timeout 15)
(setq auto-save-interval 120)
(setq auto-save-list-file-prefix nil)

; make backup files
(setq backup-directory-alist '((".*" .  "~/.emacs.d/.tmp/hist")))
(setq make-backup-files t)
(setq version-control t) ; enable version control
(setq kept-new-versions 5)
(setq kept-old-versions 1)
(setq delete-old-versions t)

(add-hook 'prog-mode-hook
          '(lambda () (electric-pair-mode t)))

(setq default-frame-alist
      (append (list
               '(font . "HackGenNerd Console-15"))
  default-frame-alist))
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(display-time)

;; mouse support on terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))


; control line spacing
(setq-default line-spacing 0.25)

(defun sw-lnsp (wdth)
  (interactive "nset line spacing: ")
  (setq line-spacing wdth)
  )
(defun dbl-lnsp ()
  (interactive)
  (setq line-spacing 2.0)
  )

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


; ---------------  load package ---------------
(load "~/.emacs.d/elisp/initpkg.el")

;; -------- package config under use-package --------
;; ido extensions
(use-package amx
  :ensure t
  :bind
  (:map global-map
        ("M-x" . amx)
        ("M-X" . amx-major-mode-commands)
        )
  :init
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t)
  :custom
  (amx-backend 'ido)
  (amx-prompt-string "⚡> ")
  )
(use-package ido-vertical-mode
  :ensure t
  :custom
  (ido-vertical-show-count t)
  :config (ido-vertical-mode t))
(use-package ido-completing-read+
  :ensure t
  )

(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-arguments (list "-l"))
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize))
  )

(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-visualizer-timestamps t)
  :config
  (global-undo-tree-mode)
  )

(use-package origami
  :ensure t
  :hook (prog-mode . origami-mode))
;; enable evil
;(setq evil-want-integration t)
;; This is optional since it's already set to t by default.
(setq evil-want-keybinding nil)
(use-package evil
  :ensure t
  :custom
  (evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
)
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  (define-key evil-normal-state-map (kbd "C-u") 'scroll-down-command)
  ;; enable emacs cursor movement in insert mode
  (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-f") 'forward-char)
  (define-key evil-insert-state-map (kbd "C-b") 'backward-char)
  ;(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
  ;(define-key evil-insert-state-map (kbd "C-n") 'next-line)
  )

(use-package nyan-mode
  :ensure t
  :init (nyan-mode)
  :config
  (defun nyan-try ()
    (nyan-stop-animation)
    (setq nyan-wavy-trail nil)
    )
  (defun nyan-xit ()
    (nyan-start-animation)
    (setq nyan-wavy-trail t)
    )
  (nyan-xit)
  :hook
  ((evil-normal-state-entry . nyan-try)
   (evil-normal-state-exit . nyan-xit)
  )
)


; ---------- loading doom theme  ----------
(load "~/.emacs.d/elisp/doom.el")


;; enable relative line number
(use-package linum-relative
  :ensure t
  :custom
  (linum-relative-backend 'display-line-numbers-mode)
  :config
  (linum-relative-global-mode)
)


(use-package all-the-icons
  :ensure t
) ;; need installation by all-the-icons-install-fonts


(use-package highlight-indent-guides
  :ensure t
  :hook
    (prog-mode . highlight-indent-guides-mode)
  :custom
  ;(if (not (display-graphic-p))
  ;    (progn
  ;      (setq highlight-indent-guides-auto-enabled nil)
  ;      (set-face-background 'highlight-indent-guides-odd-face "black")
  ;      (set-face-background 'highlight-indent-guides-top-odd-face "green")
  ;      (set-face-background 'highlight-indent-guides-even-face "black")
  ;      (set-face-background 'highlight-indent-guides-top-even-face "green"))
  ;    (setq highlight-indent-guides-auto-enabled t)
  ;  )
  (highlight-indent-guides-method 'column)
  (highlight-indent-guides-auto-odd-face-perc 10)
  (highlight-indent-guides-auto-even-face-perc 10)
  ;(setq highlight-indent-guides-auto-character-face-perc 20)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0)
)


(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
)


;; configure whitespace
(use-package whitespace
  :custom
  (whitespace-global-modes '(not dired-mode tar-mode neotree))
  (whitespace-line-column 80)
  (whitespace-style
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
  (whitespace-display-mappings
    '(
       (tab-mark ?\t [?\xBB ?\t] [?\\ ?\t])
       (newline-mark ?\n [?\x21B2 ?\n])  ; display when in some mejor mode
     )
  )
  :config
  (global-whitespace-mode t)
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
  :hook (prog-mode . git-gutter-fix-init)
  :custom
  (git-gutter:modified-sign " ")
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "▶")
  :config
  (set-face-background 'git-gutter:modified "DodgerBlue2")
  (set-face-background 'git-gutter:added "SpringGreen2")
  (set-face-foreground 'git-gutter:added "dark slate")
  (set-face-foreground 'git-gutter:deleted "tomato2")
  (global-git-gutter-mode +1)
  (defun git-gutter-fix-init ()
    (interactive)
    (if (eq git-gutter-mode nil)
        (git-gutter))
    (git-gutter))
  :bind
  (:map global-map
        ("C-x C-g g" . git-gutter-fix-init)
        ("C-x C-g j" . git-gutter:next-hunk)
        ("C-x C-g k" . git-gutter:previous-hunk)
        ("C-x C-g G" . git-gutter:end-of-hunk)
        ("C-x C-g r" . git-gutter:update-all-windows)
        ("C-x C-g d" . git-gutter:popup-hunk)
        ("C-x C-g v" . git-gutter:mark-hunk)
        ("C-x C-g x" . git-gutter:revert-hunk)
        ("C-x C-g s" . git-gutter:stage-hunk))
  )


(use-package flycheck
 :ensure t)

(use-package company
  ;company completion framework
    :ensure t
    :custom
    (company-idle-delay 0)
    (company-selection-wrap-around t)
    (completion-ignore-case t)
    :config
    (global-company-mode)
    )

(use-package ace-window
  :ensure t
  :bind
  (:map global-map
        ("M-o" . ace-window))
  )

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  )

(use-package hl-todo
  :ensure t
  :hook
  (prog-mode . hl-todo-mode)
)

(use-package editorconfig
  :ensure t
  :hook (prog-mode . editorconfig-mode)
  ;:config
  ;(editorconfig-mode 1)
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

; ----------- simple writing view -----------
(defun my-reset-margins ()
  (interactive) (set-window-margins nil nil nil))

(defun my-center-margins (wdth)
  (interactive "nset text width: ")
  (my-reset-margins)
  (let ((margin-size (/ (- (window-width) wdth) 2)))
    (if (>= margin-size 0)
      (set-window-margins nil margin-size margin-size)
      (message "frame width is too narrow"))
    ))

(defun my-s2writing-style ()
  (interactive)
  (my-center-margins 90)
  (sw-lnsp 1.25)
  ;(visual-fill-column-mode)
  )
