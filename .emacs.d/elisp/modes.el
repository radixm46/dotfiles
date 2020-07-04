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

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package lsp-treemacs
  :after treemacs lsp-mode
  :ensure t
  :config
  (lsp-treemacs-sync-mode 1))

;(use-package treemacs-projectile
;  :after treemacs projectile
;  :ensure t)

;(use-package treemacs-icons-dired
;  :after treemacs dired
;  :ensure t
;  :config (treemacs-icons-dired-mode))

;(use-package treemacs-persp ;;treemacs-persective if you use perspective.el vs. persp-mode
;  :after treemacs persp-mode ;;or perspective vs. persp-mode
;  :ensure t
;  :config (treemacs-set-scope-type 'Perspectives))

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
  :hook
  ((evil-normal-state-entry . nyan-try)
   (evil-normal-state-exit . nyan-xit)
  )
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
   (rustic-mode . racer))
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

(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))




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
  (setq org-highest-priority ?1)
  (setq org-lowest-priority ?4)
  (setq org-default-priority ?3)
  (setq org-capture-templates
        '(("t" "Task to Inbox" entry (file+headline org-todofile "Inbox")
           "** TODO %?\n  %U\n  %a") ; %u->%t
          ("n" "Note to Inbox" entry (file+headline "" "Inbox")
           "** %?\nEntered on %U\n %i\n %a")))
  (setq org-log-done 'time)
  (setq org-clock-clocked-in-display 'frame-title)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.5))
  (setq org-image-actual-width nil)

  ;configure bibtex
  (defun org-mode-reftex-setup ()
    (load-library "reftex")
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
         (reftex-parse-all))
    (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
    )
  (add-hook 'org-mode-hook 'org-mode-reftex-setup)
  ; (use-package ox-bibtex)

  ;configure org-pomodoro
  (use-package org-pomodoro
    :ensure t
    )
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
