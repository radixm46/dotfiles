;; minor mode plugins

;; emacs built-in modes
(leaf flymake
  :tag "builtin"
  :emacs>= "26"
  :hook
  (prog-mode-hook . flymake-mode))
;; ------------------------------------------------------------

;; ido extensions
(leaf ido
  :tag "builtin"
  :init
  (ido-mode t)
  (ido-everywhere t)
  :custom
  (ido-enable-flex-matching . t)
  (ido-save-directory-list-file . "~/.emacs.d/.cache/ido.last")
  :config
  (leaf amx
    :ensure t
    :bind
    (:global-map ("M-x" . amx)
                 ("M-X" . amx-major-mode-commands))
    :custom
    (amx-save-file . "~/.emacs.d/.cache/amx-items")
    (amx-backend . 'ido)
    (amx-prompt-string . "⚡> ")
    :global-minor-mode amx-mode)
  (leaf ido-vertical-mode
    :ensure t
    :custom
    (ido-vertical-define-keys . 'C-n-C-p-up-down-left-right)
    (ido-vertical-show-count . t)
    (ido-vertical-indicator . " ▶")
    :config (ido-vertical-mode t))
  (leaf ido-completing-read+
    :ensure t
    :commands ido-ubiquitous-mode
    :custom
    (magit-completing-read-function . 'magit-ido-completing-read)
    (gnus-completing-read-function  . 'gnus-ido-completing-read)
    :config (ido-ubiquitous-mode 1))
  (leaf ido-yes-or-no
    :ensure t
    :commands ido-yes-or-no-mode
    :config (ido-yes-or-no-mode t))
  (leaf ido-complete-space-or-hyphen
    :ensure t
    :config (ido-complete-space-or-hyphen-mode t))

  (defun my/ido-recentf ()
    "works with recentf-mode"
    (interactive)
    (find-file (ido-completing-read "Find from recent: " recentf-list)))
  )

(leaf origami
  :ensure t
  :hook (prog-mode-hook . origami-mode))

;; enable evil
(leaf evil
  :ensure t
  :init (setq evil-want-keybinding nil)
  :custom
  (evil-undo-system . 'undo-tree)
  :config
  (leaf evil-surround
    :ensure t
    :global-minor-mode global-evil-surround-mode)
  (leaf evil-collection
    :after evil
    :ensure t
    :config
    (evil-collection-init)
    (define-key evil-normal-state-map (kbd "C-u") 'scroll-down-command)
    ;; enable emacs cursor movement in insert mode
    (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
    (define-key evil-insert-state-map (kbd "C-f") 'forward-char)
    (define-key evil-insert-state-map (kbd "C-b") 'backward-char)
    ;; (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
    ;; (define-key evil-insert-state-map (kbd "C-n") 'next-line)
    )
  :global-minor-mode evil-mode
  )

(leaf git-gutter
  :ensure t
  :hook (prog-mode-hook . git-gutter-fix-init)
  :custom
  (git-gutter:modified-sign . " ")
  (git-gutter:added-sign    . "+")
  (git-gutter:deleted-sign  . "▶")
  :config
  (set-face-background 'git-gutter:modified "DodgerBlue2")
  (set-face-foreground 'git-gutter:modified "DodgerBlue2")
  (set-face-background 'git-gutter:added "SpringGreen2")
  (set-face-foreground 'git-gutter:added "dark slate")
  (set-face-foreground 'git-gutter:deleted "tomato2")
  (defun git-gutter-fix-init ()
    (interactive)
    (if (eq git-gutter-mode nil)
        (git-gutter))
    (git-gutter))
  :bind (:global-map
         ("C-x C-g g" . git-gutter-fix-init)
         ("C-x C-g j" . git-gutter:next-hunk)
         ("C-x C-g k" . git-gutter:previous-hunk)
         ("C-x C-g G" . git-gutter:end-of-hunk)
         ("C-x C-g r" . git-gutter:update-all-windows)
         ("C-x C-g d" . git-gutter:popup-hunk)
         ("C-x C-g v" . git-gutter:mark-hunk)
         ("C-x C-g x" . git-gutter:revert-hunk)
         ("C-x C-g s" . git-gutter:stage-hunk))
  :global-minor-mode (global-git-gutter-mode t)
  )

(leaf company
  ;; company completion framework
  :ensure t
  :custom
  (company-idle-delay . 0)
  (company-selection-wrap-around . t)
  (completion-ignore-case . t)
  :config
  (global-company-mode)
  (leaf company-box
    :ensure t
    :hook (company-mode-hook . company-box-mode))
  )

(leaf ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(leaf which-key
  :ensure t
  :config
  (leaf which-key-posframe
    :ensure t
    :custom
    (which-key-posframe-poshandler . 'posframe-poshandler-frame-top-left-corner))
  (which-key-mode)
  (if (emacs-works-on-term-p)
      (which-key-setup-side-window-right-bottom)(which-key-posframe-mode))
  )

(leaf hydra
  :ensure t)

(leaf editorconfig
  :ensure t
  :config (editorconfig-mode))

(leaf quickrun :ensure t)

(leaf yasnippet
  :ensure t
  :hook (prog-mode-hook . yas-minor-mode)
  :config (yas-reload-all))
