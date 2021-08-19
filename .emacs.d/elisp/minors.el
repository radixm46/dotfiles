;; minor mode plugins

;; emacs built-in modes
(leaf flymake
  :tag "builtin"
  :emacs>= "26"
  :hook
  (prog-mode-hook . flymake-mode))
;; ------------------------------------------------------------

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

(leaf quickrun :ensure t)

(leaf yasnippet
  :ensure t
  :hook (prog-mode-hook . yas-minor-mode)
  :config (yas-reload-all))
