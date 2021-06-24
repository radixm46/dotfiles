;; minor mode plugins

;; emacs built-in modes
(use-package electric-pair-mode
  :hook
  ((conf-mode
    prog-mode) . electric-pair-mode))

(use-package visual-line-mode
  :hook
  ((org-mode markdown-mode) . visual-line-mode))

(use-package display-line-numbers
  :if (>= emacs-major-version 26)
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-width 4)
  :hook
  ((conf-mode
    fundamental-mode
    outline-mode
    prog-mode
    text-mode) . display-line-numbers-mode))

(use-package flymake
  :if (>= emacs-major-version 26)
  :hook
  (prog-mode . flymake-mode))

(use-package display-fill-column-indicator
  :if (>= emacs-major-version 27)
  :custom
  (display-fill-column-indicator-column 90)
  :hook
  ((conf-mode
    fundamental-mode
    outline-mode
    prog-mode
    text-mode) . display-fill-column-indicator-mode))

;; ------------------------------------------------------------

;; skk config(use
(use-package skk
  :ensure ddskk
  :init
  (setq default-input-method "japanese-skk"
        skk-user-directory "~/.emacs.d/skk"
        skk-init-file "~/.emacs.d/elisp/initskk.el"))

;; ido extensions
(use-package ido
  ;; built-in
  :init
  (ido-mode t)
  (ido-everywhere t)
  :custom
  (ido-enable-flex-matching t)
  :config
  (use-package amx
    :ensure t
    :bind
    (:map global-map
          ("M-x" . amx)
          ("M-X" . amx-major-mode-commands))
    :custom
    (amx-backend 'ido)
    (amx-prompt-string "⚡> "))
  (use-package ido-vertical-mode
    :ensure t
    :custom
    (ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
    (ido-vertical-show-count t)
    (ido-vertical-indicator " ▶")
    :config (ido-vertical-mode t))
  (use-package ido-completing-read+
    :ensure t
    :custom
    (magit-completing-read-function 'magit-ido-completing-read)
    (gnus-completing-read-function 'gnus-ido-completing-read)
    :config (ido-ubiquitous-mode 1))
  (use-package ido-yes-or-no
    :ensure t
    :config (ido-yes-or-no-mode t)))

(use-package origami
  :ensure t
  :hook
  (prog-mode . origami-mode))

;; enable evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :custom
  (evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))
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
    (setq nyan-wavy-trail nil))
  (defun nyan-xit ()
    (nyan-start-animation)
    (setq nyan-wavy-trail t))
  (nyan-xit)
  :hook
  ((evil-normal-state-entry . nyan-try)
   (evil-normal-state-exit . nyan-xit)))

(use-package highlight-indent-guides
  :ensure t
  :hook
  ((conf-mode
    outline-mode
    prog-mode
    text-mode) . highlight-indent-guides-mode)
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
  (highlight-indent-guides-delay 0))

(use-package rainbow-delimiters
  :ensure t
  :hook
  ((conf-mode
    prog-mode) . rainbow-delimiters-mode))

;; configure whitespace
(use-package whitespace
  :hook
  ((conf-mode
    outline-mode
    prog-mode
    text-mode) . whitespace-mode)
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
     ))
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
  (global-git-gutter-mode t)
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
        ("C-x C-g s" . git-gutter:stage-hunk)))

(use-package company
  ;company completion framework
    :ensure t
    :custom
    (company-idle-delay 0)
    (company-selection-wrap-around t)
    (completion-ignore-case t)
    :config
    (global-company-mode))

(use-package ace-window
  :ensure t
  :bind
  (:map global-map
        ("M-o" . ace-window)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package hl-todo
  :ensure t
  :hook
  (prog-mode . hl-todo-mode))

(use-package editorconfig
  :ensure t
  :config (editorconfig-mode))

(use-package ov
:ensure t
:init
(defun check-serif-available()
    (if (member "Noto Serif CJK JP" (font-family-list))
        (setq serif-available-p t)
        (setq serif-available-p nil)))
(setq ov-serif nil)
(defun toggle-buffer-ov-serif (trigger-state-p)
  "temporary make buffer to serif, ov-clear if ov available at buffer"
  (interactive)
  (if serif-available-p
      (if trigger-state-p
        (progn (sw-lnsp 0.8)
               (setq ov-serif
                     (ov (point-min) (point-max) 'face '(:family "Noto Serif CJK JP"))))
        (progn (sw-lnsp 0.25)
               (ov-reset ov-serif)))))
:hook (server-after-make-frame . check-serif-available)
:commands (ov-reset ov-clear ov))

(use-package darkroom
  :ensure t
  :init
  (defun my-darkroom-init ()
    (toggle-buffer-ov-serif darkroom-mode))
  :hook (darkroom-mode . my-darkroom-init)
        ;(darkroom-mode . ov-clear)
  :bind ([f8] . darkroom-mode)
  :custom (darkroom-text-scale-increase 1)
  ;:config
  ;(doom-modeline t) ;TODO:enable modeline
  )

(use-package quickrun
  :ensure t)

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all)
  )

