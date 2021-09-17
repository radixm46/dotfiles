;; major modes loaded by init.el

(leaf emacs-lisp-mode
  :tag "builtin"
  :hook
  (emacs-lisp-mode-hook . hs-minor-mode)
  (emacs-lisp-mode-hook . flycheck-mode)
  (emacs-lisp-mode-hook . eldoc-box-hover-mode))

;; required for racer
(leaf rust-mode
  :ensure t
  :hook (rust-mode-hook . lsp)
  :custom (rust-format-on-save . t)
  :config
  (leaf cargo :ensure t)
  )

(leaf python-mode
  :tag "builtin"
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode-hook . lsp-deferred)
  :init
  (leaf poetry
    :ensure t
    :hook (python-mode-hook . poetry-tracking-mode))
  (leaf pipenv
    :ensure t
    :custom
    (pipenv-projectile-after-switch-function . #'pipenv-projectile-after-switch-extended)
    ;; :hook (python-mode . python-pipenv-init))
    :init
    (defun python-pipenv-init ()
      "init python environment on python-mode"
      (if (not pipenv-mode)
          (pipenv-mode))
      (if (eq python-shell-virtualenv-root nil)
          (pipenv-activate))))
  )

(leaf go-mode
  :ensure t
  :hook ((go-mode-hook . lsp)
         (before-save-hook . lsp-format-buffer)
         (before-save-hook . lsp-organize-imports))
  :mode ("\\.go\\'" . go-mode)
  )

(leaf web-mode
  :ensure t
  :mode
  (("\\.html?\\'"
    "\\.phtml\\'"
    "\\.[agj]sp\\'"
    "\\.as[cp]x\\'"
    "\\.erb\\'"
    "\\.mustache\\'"
    "\\.djhtml\\'"
    "\\.vue\\'"
    "\\.js\\'"
    "\\.jsx\\'") . web-mode)
  :hook (web-mode-hook . lsp)
  :preface
  (leaf flycheck-use-tidy :if (executable-find "tidy")
    :doc "enable flycheck with html-tidy on web-mode"
    :config (add-hook 'lsp-after-initialize-hook
                      (lambda ()
                        (flycheck-add-mode 'html-tidy 'web-mode)
                        (flycheck-add-next-checker 'lsp 'html-tidy))))
  :custom
  ;; highlights
  (web-mode-enable-current-element-highlight . t)
  ;; configure indent
  (web-mode-markup-indent-offset . 2)
  (web-mode-css-indent-offset . 2)
  (web-mode-code-indent-offset . 2)
  ;; auto close tags
  (web-mode-enable-auto-pairing . t)
  (web-mode-enable-auto-closing . t)
  (web-mode-auto-close-style . 2)
  (web-mode-tag-auto-close-style . 2)
  ;; auto colorize css
  (web-mode-enable-css-colorization . t)
  (web-mode-enable-block-face . t)
  ;; (web-mode-enable-part-face t)
  (web-mode-enable-comment-interpolation . t)
  (web-mode-enable-heredoc-fontification . t))

(leaf javascript
  :doc "config javascript environment"
  :config
  (leaf js-mode :emacs>= "27"
    :tag "builtin"
    :doc "use js-mode with lsp"
    :mode ("\\.js\\'" . js-mode)
    :custom (js-indent-level . 2)
    :hook (js-mode-hook . lsp)
    :init
    (leaf js2-mode
      :ensure t
      :doc "use js2-mode as minor mode with js-mode"
      :hook (js-mode-hook . js2-minor-mode)
      :custom
      (js2-basic-offset . 2))

    (leaf flycheck-use-eslint :if (executable-find "eslint") :disabled t
      :hook (lsp-after-initialize-hook . (lambda ()
                                           (flycheck-add-mode 'javascript-eslint 'js-mode)
                                           (flycheck-add-next-checker 'lsp 'javascript-eslint))))
    )

  (leaf js2-mode :emacs< "27"
    :ensure t
    :doc "use js2-mode as major mode with lsp"
    :mode ("\\.js\\'" . js2-mode)
    :hook (js2-mode-hook . lsp)
    :custom (js2-basic-offset . 2)
    :init
    )

  )

(leaf sh-mode
  :tag "builtin"
  :hook
  (sh-mode-hook . lsp)
  (sh-mode-hook . flycheck-mode)
  ;; :config (setq flycheck-checker 'sh-shellcheck)
  :init
  (leaf shfmt :if (executable-find "shfmt")
    :doc "format code on save"
    :ensure t
    :hook (sh-mode-hook . shfmt-on-save-mode))
  (leaf flycheck-by-shellcheck :if (executable-find "shellcheck")
    :doc "use shellcheck for flychecker with lsp"
    :hook (lsp-after-initialize-hook . (lambda () (flycheck-add-next-checker 'lsp 'sh-shellcheck))))
  )

(leaf csv-mode
  :ensure t
  :mode (".csv" ".tsv"))

(leaf json-mode
  :ensure t
  ;; :mode ("\\.json\\'" . json-mode)
  )

(leaf yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

(leaf systemd :ensure t)

(leaf org
  :doc "org-mode config"
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c C-o 0" . my/org-goto-dir))
  :custom
  (org-startup-truncated        . t)
  (org-startup-folded           . nil)
  ;; priority
  (org-priority-highest         . ?1) ; FIXME: value
  (org-priority-lowest          . ?4)
  (org-priority-default         . ?3)
  ;; keywords
  (org-refile-targets           . '((org-agenda-files :maxlevel . 3)))
  (org-todo-keywords            . '((sequence "NEXT(n)" "TODO(t)" "WAITING(w)" "PROJ(p)" "|"
                                              "DONE(d)" "CANCELLED(c)")))
  (org-capture-templates        . '(("t" "Task to Inbox" entry (file+headline org-todofile "Inbox")
                                     "** TODO %?\n  %U\n  %a") ; %u->%t
                                    ("n" "Note to Inbox" entry (file+headline "" "Inbox")
                                     "** %?\nEntered on %U\n %i\n %a")))
  (org-log-done                 . 'time)
  (org-clock-clocked-in-display . 'frame-title)
  (org-image-actual-width       . nil)
  :config
  (evil-define-key 'normal org-mode-map "j" 'evil-next-visual-line)
  (evil-define-key 'normal org-mode-map "k" 'evil-previous-visual-line)
  (evil-define-key 'normal org-mode-map "gj" 'evil-next-line)
  (evil-define-key 'normal org-mode-map "gk" 'evil-previous-line)
  ;; (setq system-time-locale "C") ; dates written in eng
  (custom-set-variables ; set directory
   '(org-directory            "~/org/orgfiles")
   '(org-default-notes-file   (concat org-directory "/notes.org"))
   '(org-agenda-files         (list org-directory))
   ;; set latex option
   '(org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))
  (defvar org-todofile       (concat org-directory "/todo.org") "default org todo file path")

  (set-face-attribute 'org-table nil :family font-for-tables)

  (defun my/org-goto-dir ()
    "open dir '~/org' with dired"
    (interactive)
    (find-file "~/org"))

  (leaf org-bullets
    :doc "pretty looking bullet list"
    :ensure t
    :hook (org-mode-hook . org-bullets-mode))

  (leaf org-pomodoro
    :doc "configure org-pomodoro"
    :ensure t
    :bind ([f6] . org-pomodoro))

  (leaf org-sidebar
    :doc "sidebar for org-mode"
    :ensure t :require org-sidebar ; explicitly require
    :bind
    (:org-mode-map
     ("M-9" . org-sidebar-tree-toggle)
     ("M-8" . org-sidebar-toggle)))

  ;; configure bibtex
  (defun org-mode-reftex-setup ()
    "setup reftex on org-mode doc"
    (load-library "reftex")
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
         (reftex-parse-all))
    (define-key org-mode-map (kbd "C-c )") 'reftex-citation))

  ;; (use-package ox-bibtex)

  :hook
  (org-mode-hook . org-mode-reftex-setup)
  (org-mode-hook . org-indent-mode)
  )

(leaf org-roam
  :ensure t
  :init (setq org-roam-v2-ack t)
  :bind
  (("C-c n c" . org-roam-capture)
   ("C-c n f" . org-roam-find-file)
   ("C-c n g" . org-roam-ui-mode)
   (:org-mode-map
    ("C-c n n" . org-roam-node-insert)
    ("C-c n i" . org-roam-insert)
    ("C-c n I" . org-roam-insert-immediate)
    ("C-c n a" . org-roam-alias-add)
    ("C-c n A" . org-roam-alias-remove)
    ("C-c n t" . org-roam-tag-add)
    ("C-c n T" . org-roam-tag-remove)))
  :custom
  (org-roam-capture-templates . '(("d" "default" plain
                                   "%?"
                                   :if-new
                                   (file+head "${slug}.org" "#+title: ${title}\n")
                                   :immediate-finish t
                                   :unnarrowed t)
                                  ;; ("r" "bibliography reference" plain "%?"
                                  ;;  :if-new
                                  ;;  (file+head "references/${citekey}.org" "#+title: ${title}\n")
                                  ;;  :unnarrowed t)
                                  ;;("r" "Roam" plain (function org-roam--capture-get-point)
                                  ;;"%?"
                                  ;;:file-name "%<%Y%m%d%H%M%S>-${slug}.org"
                                  ;;:head "#+title: ${title}\n"
                                  ;;:unnarrowed t)
                                  ))
  :hook (org-roam-capture-new-node-hook . org-roam-db-sync)
  :config
  (custom-set-variables
   '(org-roam-directory           (file-truename "~/org/roam"))
   '(org-roam-index-file          (file-truename "~/org/roam/Index.org"))
   '(org-roam-db-location         (file-truename "~/.emacs.d/.cache/org-roam.db")))

  (org-roam-db-autosync-mode)

  (leaf org-roam-ui
    :doc "org-roam frontend package"
    :straight
    (org-roam-ui :type git :host github
                 :repo "org-roam/org-roam-ui" :branch "main"
                 :files ("*.el" "out"))
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))
  )

(leaf markdown-mode
  :ensure t
  ;; :hook (markdown-mode . visual-line-mode)
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'"  . gfm-mode)
   ("\\.md\\'"
    "\\.markdown\\'") . markdown-mode)
  :custom (markdown-command . "multimarkdown")
  :config
  (evil-define-key 'normal markdown-mode-map "j" 'evil-next-visual-line)
  (evil-define-key 'normal markdown-mode-map "k" 'evil-previous-visual-line)
  (evil-define-key 'normal markdown-mode-map "gj" 'evil-next-line)
  (evil-define-key 'normal markdown-mode-map "gk" 'evil-previous-line)
  ;; modify table face to 1:2
  (set-face-attribute 'markdown-table-face nil :family font-for-tables)
  )
