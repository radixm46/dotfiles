;; major modes loaded by init.el

;; required for racer
(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp)
  :custom
  (rust-format-on-save t)
  :config
  (use-package cargo
    :ensure t)
  )

(leaf python-mode
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode-hook . lsp-deferred)
  :config
  (leaf poetry
    :ensure t
    :hook (python-mode-hook . poetry-tracking-mode))
  (leaf pipenv
    :ensure t
    :custom
    (pipenv-projectile-after-switch-function . #'pipenv-projectile-after-switch-extended)
    :init
    (defun python-pipenv-init ()
      "init python environment on python-mode"
      (if (not pipenv-mode)
          (pipenv-mode))
      (if (eq python-shell-virtualenv-root nil)
          (pipenv-activate))))
    ;:hook (python-mode . python-pipenv-init))
  )

(use-package go-mode
  :ensure t
  :hook ((go-mode . lsp)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :mode ("\\.go\\'" . go-mode)
)

;; web-mode
(use-package web-mode
  :ensure t
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.phtml\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.vue\\'" . web-mode))
  :hook (web-mode . lsp)
  :custom
  ;; highlights
  (web-mode-enable-current-element-highlight t)
  ;; configure indent
  (web-mode-markup-indent-offset 4)
  (web-mode-css-indent-offset 4)
  (web-mode-code-indent-offset 4)
  ;; auto close tags
  (eb-mode-enable-auto-pairing t)
  (eb-mode-enable-auto-closing t)
  ;; auto colorize css
  (web-mode-enable-css-colorization t)
  (web-mode-enable-block-face t)
  ;(web-mode-enable-part-face t)
  (web-mode-enable-comment-interpolation t)
  (web-mode-enable-heredoc-fontification t)
)

(use-package js-mode
  ;; emacs built-in mode
  :if (>= emacs-major-version 27)
  :mode ("\\.jsx\\'" . js2-mode)
  :hook (js-mode . lsp)
  :custom
  (js-indent-level 2)
  :config
  (use-package js2-mode
    :if (>= emacs-major-version 27)
    :ensure t
    :hook (js-mode . js2-minor-mode)
    :config (setq js2-basic-offset 2))
  )
(use-package js2-mode
  :if (<= emacs-major-version 26)
  :ensure t
  :mode
  (("\\.js\\'" . js2-mode)
   ("\\.jsx\\'" . js2-mode))
  :hook (js2-mode . lsp)
  :config (setq js2-basic-offset 2)
  )

(use-package sh-mode
  ;; emacs built-in mode
  :hook (sh-mode . lsp))

(use-package csv-mode
  :ensure t
  :mode (".csv" ".tsv"))

(use-package json-mode
  :ensure t
  ;; :mode ("\\.json\\'" . json-mode)
)

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode)
)

;; org-mode config
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c C-o 0" . my/org-goto-dir))
  :config  ;
  (evil-define-key 'normal org-mode-map "j" 'evil-next-visual-line)
  (evil-define-key 'normal org-mode-map "k" 'evil-previous-visual-line)
  (evil-define-key 'normal org-mode-map "gj" 'evil-next-line)
  (evil-define-key 'normal org-mode-map "gk" 'evil-previous-line)
  ;(setq system-time-locale "C") ;; dates written in eng
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
  (defun my/org-goto-dir ()
    "open dir '~/org' with dired"
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

  ;; modify table face to 1:2
  (set-face-attribute 'org-table nil :family font-for-tables)

  (use-package org-bullets
    :ensure t
    :hook (org-mode . org-bullets-mode))

  ;; configure org-pomodoro
  (use-package org-pomodoro
    :ensure t
    :bind ([f6] . org-pomodoro))

  ;; configure bibtex
  (defun org-mode-reftex-setup ()
    "setup reftex on org-mode doc"
    (load-library "reftex")
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
         (reftex-parse-all))
    (define-key org-mode-map (kbd "C-c )") 'reftex-citation))

  ; (use-package ox-bibtex)

  :hook
   (org-mode . org-mode-reftex-setup)
)

;; markdown-mode
(use-package markdown-mode
  :ensure t
  ;:hook (markdown-mode . visual-line-mode)
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown")
  :config
  (evil-define-key 'normal markdown-mode-map "j" 'evil-next-visual-line)
  (evil-define-key 'normal markdown-mode-map "k" 'evil-previous-visual-line)
  (evil-define-key 'normal markdown-mode-map "gj" 'evil-next-line)
  (evil-define-key 'normal markdown-mode-map "gk" 'evil-previous-line)
  ;; modify table face to 1:2
  (set-face-attribute 'markdown-table-face nil :family font-for-tables)
)

(use-package systemd
  :ensure t)
