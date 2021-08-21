;; major modes loaded by init.el

(leaf emacs-lisp-mode
  :tag "builtin"
  :hook (emacs-lisp-mode-hook . hs-minor-mode))

;; required for racer
(leaf rust-mode
  :ensure t
  :hook (rust-mode-hook . lsp)
  :custom (rust-format-on-save . t)
  :config
  (leaf cargo :ensure t)
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
  (("\\.html?\\'" . web-mode)
   ("\\.phtml\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.vue\\'" . web-mode)
   ("\\.js\\'" . web-mode)
   ("\\.jsx\\'" . web-mode))
  :hook (web-mode-hook . lsp)
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
  ;(web-mode-enable-part-face t)
  (web-mode-enable-comment-interpolation . t)
  (web-mode-enable-heredoc-fontification . t))

(leaf js-mode
  :tag "builtin"
  :emacs>= "27"
  ;; :mode
  ;; (("\\.js\\'" . js2-mode)
  ;;  ("\\.jsx\\'" . js2-mode))
  :hook (js-mode-hook . lsp)
  :custom (js-indent-level . 2)
  :config
  (leaf js2-mode
    ;;:if (>= emacs-major-version 27)
    :ensure t
    :hook (js-mode . js2-minor-mode)
    :config (setq js2-basic-offset 2)))

(leaf sh-mode
  :tag "builtin"
  :hook (sh-mode-hook . lsp))

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
  (org-priority-highest         . ?1) ;; FIXME: value
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
  :config  ;
  (evil-define-key 'normal org-mode-map "j" 'evil-next-visual-line)
  (evil-define-key 'normal org-mode-map "k" 'evil-previous-visual-line)
  (evil-define-key 'normal org-mode-map "gj" 'evil-next-line)
  (evil-define-key 'normal org-mode-map "gk" 'evil-previous-line)
  ;(setq system-time-locale "C") ;; dates written in eng
  (custom-set-variables
   ;; set directory
   '(org-directory            "~/org/orgfiles")
   '(org-default-notes-file   (concat org-directory "/notes.org"))
   '(org-agenda-files         (list org-directory))
   ;; set latex option
   '(org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))
   (setq org-todofile         (concat org-directory "/todo.org")) ;; not defined at default


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

  ;; configure bibtex
  (defun org-mode-reftex-setup ()
    "setup reftex on org-mode doc"
    (load-library "reftex")
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
         (reftex-parse-all))
    (define-key org-mode-map (kbd "C-c )") 'reftex-citation))

  -;; (use-package ox-bibtex)

  :hook
  (org-mode . org-mode-reftex-setup)
  )

(leaf markdown-mode
  :ensure t
  ;:hook (markdown-mode . visual-line-mode)
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command . "multimarkdown")
  :config
  (evil-define-key 'normal markdown-mode-map "j" 'evil-next-visual-line)
  (evil-define-key 'normal markdown-mode-map "k" 'evil-previous-visual-line)
  (evil-define-key 'normal markdown-mode-map "gj" 'evil-next-line)
  (evil-define-key 'normal markdown-mode-map "gk" 'evil-previous-line)
  ;; modify table face to 1:2
  (set-face-attribute 'markdown-table-face nil :family font-for-tables)
  )

