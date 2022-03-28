;; major modes loaded by init.el

(leaf emacs-lisp-mode
  :tag "builtin"
  :hook
  (emacs-lisp-mode-hook . hs-minor-mode)
  (emacs-lisp-mode-hook . flycheck-mode)
  (emacs-lisp-mode-hook . eldoc-box-hover-at-point-mode)
  )

;; required for racer
(leaf rust-mode
  :ensure t
  :hook (rust-mode-hook . lsp)
  :custom (rust-format-on-save . t)
  :config
  (leaf cargo
    :ensure t
    :hook (rust-mode-hook . cargo-minor-mode))
  )

(leaf *python-config
  :doc "python related configuration"
  :config
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

  (leaf python-mode
    :tag "builtin" :ensure t
    :mode ("\\.py\\'" . python-mode)
    :hook (python-mode-hook . lsp-deferred))

  (leaf hy-mode
    :ensure t
    :mode ("\\.hy\\'" . hy-mode)
    :hook
    ((hy-mode-hook . hs-minor-mode)
     (hy-mode-hook . poetry-tracking-mode)
     (hy-mode-hook . eldoc-box-hover-at-point-mode))
    :init
    (leaf *eglot-hyls-config :disabled t
      :doc "enable hy-lang language server with eglot"
      :require eglot
      :config
      (add-to-list 'eglot-server-programs
                   '(hy-mode . ("hyls" . nil)))
      (add-hook 'hy-mode-hook 'eglot-ensure))
    )
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
    "\\.svelte\\'"
    "\\.js\\'"
    "\\.jsx\\'") . web-mode)
  :hook (web-mode-hook . lsp)
  :preface
  (leaf *flycheck-use-tidy :disabled t
    :if (executable-find "tidy")
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

(leaf html-mode
  :tag "builtin")

(leaf javascript
  :doc "config javascript environment"
  :config
  (leaf js-mode :emacs>= "27"
    :tag "builtin"
    :doc "use js-mode with lsp"
    :mode ("\\.json\\'" . js-mode)
    :custom (js-indent-level . 2)
    :hook (js-mode-hook . lsp)
    :init
    (leaf js2-mode
      :ensure t
      :doc "use js2-mode as minor mode with js-mode on .js file"
      ;; :hook (js-mode-hook . js2-minor-mode)
      :mode ("\\.js\\'" . (lambda ()
                            (js-mode)
                            (js2-minor-mode)))
      :custom (js2-basic-offset . 2)
      )

    (leaf flycheck-use-eslint :if (executable-find "eslint") :disabled t
      :hook
      (lsp-after-initialize-hook . (lambda ()
                                     (flycheck-add-mode 'javascript-eslint 'js-mode)
                                     (flycheck-add-next-checker 'lsp 'javascript-eslint))))
    )

  (leaf js2-mode :emacs< "27"
    :ensure t
    :doc "use js2-mode as major mode with lsp"
    :mode ("\\.js\\'" . js2-mode)
    :hook (js2-mode-hook . lsp)
    :custom (js2-basic-offset . 2))
  )

(leaf typescript-mode
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode)
  :hook (typescript-mode-hook . lsp)
  :custom
  (typescript-indent-level . 2)
  :config
  (leaf tide
    :ensure t
    :hook
    ;; (before-save-hook  . tide-format-before-save)
    (typescript-mode-hook . tide-setup)
    :config
    (tide-hl-identifier-mode +1))
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
  (leaf *flycheck-by-shellcheck :if (executable-find "shellcheck")
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
  :custom
  (org-startup-truncated        . t)
  (org-startup-folded           . nil)
  ;; priority
  (org-priority-highest         . 1)
  (org-priority-lowest          . 4)
  (org-priority-default         . 3)
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
  (leaf *org-config-directory :if (f-directory-p "~/org/orgfiles")
    :custom
    (org-directory              . "~/org/orgfiles")
    :config
    (custom-set-variables
     '(org-default-notes-file   (concat org-directory "/notes.org"))
     '(org-agenda-files         (directory-files org-directory t ".org$" t)))
    (defvar org-todofile        (concat org-directory "/todo.org") "default org todo file path"))

  (custom-set-variables
   ;; set latex option
   '(org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))

  (defun my/org-goto-dir ()
    "open dir '~/org' with dired"
    (interactive)
    (find-file "~/org"))

  (leaf *reconfig-org-vi-key :if (fboundp 'evil-mode)
    :config
    (evil-define-key 'normal org-mode-map
      "j" 'evil-next-visual-line
      "k" 'evil-previous-visual-line
      "gj" 'evil-next-line
      "gk" 'evil-previous-line))

  (leaf *reconfig-org-consult-key :if (fboundp 'consult-buffer)
    :doc "bind consult functions if available"
    :bind
    (:org-mode-map
     ("M-g o"   . consult-org-heading)
     ("M-g M-o" . consult-org-heading)
     ("M-g a"   . consult-org-agenda)
     ("M-g M-a" . consult-org-agenda)))

  ;; (setq system-time-locale "C") ; dates written in eng

  (set-face-attribute 'org-table nil :family font-for-tables)

  (leaf org-bullets
    :doc "pretty looking bullet list"
    :ensure t
    :hook (org-mode-hook . org-bullets-mode))

  (leaf org-pomodoro
    :doc "configure org-pomodoro"
    :ensure t)

  (leaf org-sidebar
    :doc "sidebar for org-mode"
    :ensure t :require org-sidebar ; explicitly require
    )

  ;; configure bibtex
  (defun org-mode-reftex-setup ()
    "setup reftex on org-mode doc"
    (load-library "reftex")
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
         (reftex-parse-all))
    (define-key org-mode-map (kbd "C-c )") 'reftex-citation))

  ;; (use-package ox-bibtex)

  (leaf org-ref :ensure t)

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
    :hydra
    (hydra-org-roam
     (:hint nil) "
^^^                              org roam^^^
^^^^^^--------------------------------------------------------------------------
 _c_:   capture              _f_:   find file            _g_:   ui mode
^^^^^^..........................................................................
 _i_:   insert               _I_:   insert immediate

 ^node^                      ^alias^                     ^tag^
 _n i_: insert               _a a_: add                  _t a_: add
 _n f_: find                 _a r_: remove               _t r_: remove
 _n v_: visit
 _n r_: random

 _SPC_: BACK
"
     ("c"   org-roam-capture)
     ("f"   org-roam-find-file)
     ("g"   org-roam-ui-mode)
     ("i"   org-roam-insert)
     ("I"   org-roam-insert-immediate)
     ("n i" org-roam-node-insert)
     ("n f" org-roam-node-find)
     ("n v" org-roam-node-visit)
     ("n r" org-roam-node-random)
     ("a a" org-roam-alias-add)
     ("a r" org-roam-alias-remove)
     ("t a" org-roam-tag-add)
     ("t r" org-roam-tag-remove)
     ("SPC" hydra-org/body :exit t))
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

  :hook
  (org-mode-hook . org-mode-reftex-setup)
  (org-mode-hook . org-indent-mode)

  :hydra
  ((hydra-org
    (:hint nil) "
^^^                                org functions^^^
^^^^^^--------------------------------------------------------------------------------
 ^ ^                         ^link^                      ^agenda^
 _c_:   capture              _l s_: store                _a_:  ^^agenda (consult)
 ^ ^                         _l i_: insert (last)        _A_:  ^^agenda

^^^^^^............................... (in org mode) ..................................
 ^time stamp^                ^sidebar^                   ^move^ ^around^
 _t a_: active               _s t_: toggle               _h_:^ ^  heading (consult)
 _t A_: active (+time)       _s T_: toggle (tree)        _j_/_k_: visible heading
 _t i_: inactive             _s b_: with backlinks       _i_/_I_: item
 _t I_: inactive (+time)     ^   ^                       _b_/_B_: block
 ^   ^                       ^   ^                       _l_/_L_: link

 _m t_: timer                _m r_: roam                 _o_:^ ^  open org dir
"
    ("c"   org-capture :exit t)
    ("A"   org-agenda :exit t)
    ("a"   consult-org-agenda :exit t)
    ("l s" org-store-link)
    ("l i" org-insert-last-stored-link)
    ("t a" (org-time-stamp nil) :exit t)
    ("t A" (org-time-stamp t) :exit t) ; with time
    ("t i" (org-time-stamp-inactive nil) :exit t)
    ("t I" (org-time-stamp-inactive t) :exit t) ; with time
    ("s t" org-sidebar-toggle)
    ("s T" org-sidebar-tree-toggle)
    ("s b" org-sidebar-backlinks)
    ("h"   consult-org-heading)
    ("o"   my/org-goto-dir)
    ("j"   org-next-visible-heading)
    ("k"   org-previous-visible-heading)
    ("i"   org-next-item)
    ("I"   org-previous-item)
    ("b"   org-next-block)
    ("B"   org-previous-block)
    ("l"   org-next-link)
    ("L"   org-previous-link)
    ("m t" hydra-org-timers/body :exit t)
    ("m r" hydra-org-roam/body :exit t)
    ;; enable evil folding on current hydra
    ("z a" evil-toggle-fold)
    ("z o" evil-open-fold)
    ("z O" evil-open-fold-rec)
    ("z m" evil-close-folds)
    ("z r" evil-open-folds)
    ("z c" evil-close-fold)
    ;; enable evil scroll line binding
    ("z t" evil-scroll-line-to-top)
    ("z z" evil-scroll-line-to-center)
    ("z b" evil-scroll-line-to-bottom))

   (hydra-org-timers
    (:hint nil) "
^^^                               org timer^^^
^^^^^^--------------------------------------------------------------------------------
 _t_: timer                  _c_: set timer              _p_: pomodoro
 _s_: start                  _I_: timer item             _P_: extend last pomodoro
 _e_: stop                   _C_: change (region)
 _i_: pause or continue
 _S_: show remaining

 _SPC_: BACK
"
    ("t" org-timer)
    ("c" org-timer-set-timer)
    ("s" org-timer-start)
    ("e" org-timer-stop)
    ("i" org-timer-pause-or-continue)
    ("S" org-timer-show-remaining-time)
    ("C" org-timer-change-times-in-region)
    ("I" org-timer-item)
    ("p" org-pomodoro)
    ("P" org-pomodoro-extend-last-clock)
    ("SPC" hydra-org/body :exit t))
   )
  :bind
  (("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c C-o 0" . my/org-goto-dir)
   ("M-6"  . hydra-org/body)
   ("<f6>" . hydra-org/body))
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
  (evil-define-key 'normal markdown-mode-map
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    "gj" 'evil-next-line
    "gk" 'evil-previous-line)
  ;; modify table face to 1:2
  (set-face-attribute 'markdown-table-face nil :family font-for-tables)
  )


(leaf *docker-env
  :doc "docker related modes"
  :config
  (leaf docker
    :ensure t
    :doc "emacs integration for Docker"
    :url "https://github.com/Silex/docker.el")

  (leaf dockerfile-mode
    :ensure t
    :doc "Docker file mode for emacs"
    :url "https://github.com/spotify/dockerfile-mode")

  (leaf docker-compose-mode
    :ensure t
    :doc "major mode for editing docker-compose files, supports context-aware completion of docker-compose keys"
    :url "https://github.com/meqif/docker-compose-mode")

  (leaf docker-tramp
    :ensure t
    :doc "TRAMP method for Docker containers"
    :url "https://github.com/emacs-pe/docker-tramp.el"
    :custom (docker-tramp-use-names . t))
  )

(leaf powershell
  :ensure t
  :mode ("\\.ps1\\'"))

(leaf java-mode
  :tag "builtin"
  :doc "java environment config"
  :config
  (leaf lsp-java
    :ensure t
    :hook (java-mode-hook . lsp)
    ;; :custom
    ;; (lsp-java-format-settings-url . "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
    )
  )

(leaf elm-mode
  :ensure t
  :mode ("\\.elm\\'" . elm-mode)
  :custom
  (elm-indent-simple-offset . 2)
  (elm-indent-offset . 2)
  :hook (elm-mode-hook . lsp))

(leaf haskell-mode
  :ensure t
  :mode
  (("\\.hs\\'"
    "\\.lhs\\'"
    "\\.cabal\\'") . haskell-mode)
  )

(leaf elfeed :if (file-exists-p "~/.config/elfeed")
  :ensure t
  :url "https://github.com/skeeto/elfeed"
  :preface (defconst elfeed-dir-path "~/.config/elfeed/" "elfeed config path")
  :custom
  (elfeed-db-directory .  "~/.config/elfeed/.db")
  (elfeed-enclosure-default-dir .  elfeed-dir-path)
  (elfeed-search-filter . "@1-months-ago +unread")
  (elfeed-search-title-max-width . 95)
  (elfeed-search-date-format . '("%Y-%m-%d %k:%M" 16 :left))
  :init
  (leaf elfeed-org :if (file-exists-p (expand-file-name "elfeed.org" elfeed-dir-path))
    :ensure t
    :url "https://github.com/remyhonig/elfeed-org"
    :init (elfeed-org)
    :config (setq rmh-elfeed-org-files (list (expand-file-name "elfeed.org" elfeed-dir-path))))

  :config
  (dolist (face '(elfeed-search-tag-face
                  elfeed-search-date-face
                  elfeed-search-feed-face
                  elfeed-search-title-face
                  elfeed-search-unread-title-face))
    (set-face-attribute face nil :family font-for-tables :weight 'normal :height 1.0))

  (leaf *elfeed-func
    :doc "custom defined func"
    :config
    (defun my/elfeed-search-toggle-star ()
      "add/remove star tag on elfeed entry"
      (interactive)
      (apply 'elfeed-search-toggle-all '(star)))

    (defun my/elfeed-search-toggle-later ()
      "add/remove star tag on elfeed entry"
      (interactive)
      (apply 'elfeed-search-toggle-all '(later)))

    (defun my/elfeed-show-entry-share (&optional use-generic-p)
      "Copy the entry title and URL as org link to the clipboard."
      (interactive "P")
      (let* ((link (elfeed-entry-link elfeed-show-entry))
             (title (elfeed-entry-title elfeed-show-entry))
             (feed-info (concat title "\n" link)))
        (when feed-info
          (kill-new feed-info)
          (x-set-selection 'PRIMARY feed-info)
          (message "Yanked: %s" feed-info))))

    (defun my/elfeed-show-eww-open (&optional use-generic-p)
      "open with eww"
      (interactive "P")
      (let ((browse-url-browser-function #'eww-browse-url))
        (elfeed-show-visit use-generic-p)))

    (defun my/elfeed-search-eww-open (&optional use-generic-p)
      "open with eww"
      (interactive "P")
      (let ((browse-url-browser-function #'eww-browse-url))
        (elfeed-search-browse-url use-generic-p)))

    (evil-define-key 'normal elfeed-search-mode-map
      "m" 'my/elfeed-search-toggle-star
      "l" 'my/elfeed-search-toggle-later
      "b" 'my/elfeed-search-eww-open)
    (evil-define-key 'normal elfeed-show-mode-map
      "b" 'my/elfeed-show-eww-open
      "Y" 'my/elfeed-show-entry-share))

  :hook
  (elfeed-show-mode-hook . darkroom-mode)
  (elfeed-show-mode-hook . toggle-truncate-lines))
