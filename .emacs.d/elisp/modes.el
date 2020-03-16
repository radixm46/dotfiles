;; major modes loaded by init.el


(use-package neotree
  :ensure t
  :config
    (global-set-key [f7] 'neotree-toggle)
    (setq neo-window-fixed-size nil)
    ;; keymap for using with evil mode
    (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
    (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
    (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
    (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
)


(use-package rustic
  :ensure t
  :defer t
  ;:init
  ;(add-hook 'rustic-mode-hook #'lsp)
  ;(add-hook 'rustic-mode-hook
  ;  '(lambda ()
  ;    (racer-mode t)
  ;    (dumb-jump-mode t)
  ;    (highlight-symbol-mode t)
  ;    ))
  :hook (
    (rustic-mode . lsp)
    (rustic-mode . rucer))
  :mode ("\\.rs\\'" . rustic-mode)
  :commands (rustic-mode)

  :config
    ;(use-package quickrun
    ;:defer t
    ;:ensure t)
    (use-package racer
    :defer t
    :ensure t
    :commands racer
    )
)


(use-package python-mode
  :ensure t
  :hook (python-mode . lsp)
  :mode ("\\.py\\'" . python-mode)
)


(use-package go-mode
  :ensure t
  :hook (go-mode . lsp)
  :mode ("\\.go\\'" . go-mode)
)


;; org-mode config
(use-package org
  :ensure t
  :hook
  (org-mode . visual-line-mode)
  :mode
  ("\\.org\\'" . org-mode)
  :config  ; change to 'bind'
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
)

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :hook
  (markdown-mode . visual-line-mode)
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
)
