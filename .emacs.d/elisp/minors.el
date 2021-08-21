;; minor mode plugins
;; ------------------------------------------------------------

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

(leaf quickrun :ensure t)
