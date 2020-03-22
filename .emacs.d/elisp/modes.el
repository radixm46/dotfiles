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

;; required for racer
(use-package rust-mode
  :ensure t
  :config
  (setq auto-mode-alist
    (delete '("\\.rs\\'" . rust-mode) auto-mode-alist))
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
  :hook
  ((rustic-mode . lsp)
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
    :config
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
  :hook (org-mode . visual-line-mode)
  :mode ("\\.org\\'" . org-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c C-0" . my-org-goto-dir))
  :config  ;
  ;(setq system-time-locale "C") ; dates written in eng
  (setq org-startup-truncated t)
  (setq org-startup-folded nil)
  (setq org-directory "~/org/orgfiles")
  (setq org-default-notes-file
        (concat org-directory "/notes.org"))
  (setq org-todofile
        (concat org-directory "/todo.org"))
  (setq org-agenda-files
        (list org-directory))
  (setq org-refile-targets
        '((org-agenda-files :maxlevel . 3)))
  (defun my-org-goto-dir ()
    (interactive)
    (find-file "~/org"))
  (setq org-todo-keywords
        '((sequence "NEXT(n)" "TODO(t)" "WAITING(w)" "PROJ(p)" "|"
                    "DONE(d)" "CANCELLED(c)")))
  (setq org-capture-templates
        '(("t" "Task to Inbox" entry (file+headline org-todofile "Inbox")
           "** TODO %?\n  %U\n  %a") ; %u->%t
          ("n" "Note" entry (file+headline "" "Inbox")
           "** %?\nEntered on %U\n %i\n %a"))
      )
  (setq org-log-done 'time)
  (setq org-clock-clocked-in-display 'frame-title)
)

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . visual-line-mode)
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
)
