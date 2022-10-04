;; major modes loaded by init.el

(leaf emacs-lisp-mode
  :tag "builtin"
  :hook
  (emacs-lisp-mode-hook . hs-minor-mode)
  (emacs-lisp-mode-hook . flycheck-mode)
  (emacs-lisp-mode-hook . eldoc-box-hover-mode)
  (emacs-lisp-mode-hook . highlight-symbol-mode)
  :init
  (leaf highlight-defined
    :ensure t
    :preface
    (defun rdm/highlight-defined-patch-face ()
      "patch face color from font-lock-face"
      (custom-set-faces
       `(highlight-defined-function-name-face
         ((nil (:foreground ,(face-attribute 'font-lock-keyword-face :foreground)))))
       `(highlight-defined-builtin-function-name-face
         ((nil (:foreground ,(face-attribute 'font-lock-keyword-face :foreground)))))
       `(highlight-defined-special-form-name-face
         ((nil (:foreground ,(face-attribute 'font-lock-type-face :foreground)))))
       `(highlight-defined-macro-name-face
         ((nil (:foreground ,(face-attribute 'font-lock-preprocessor-face :foreground)))))
       ;; `(highlight-defined-face-name-face
       ;;   ((nil (:foreground ,(doom-color 'fg) :background ,(doom-color 'dark-cyan)))))
       `(highlight-defined-variable-name-face
         ((nil (:foreground ,(face-attribute 'font-lock-variable-name-face :foreground)))))
       ))
    :hook
     (emacs-lisp-mode-hook . highlight-defined-mode)
     (highlight-defined-mode-hook . rdm/highlight-defined-patch-face)
    :custom (highlight-defined-face-use-itself . t)
    )
  )

(leaf conf-mode
  :tag "builtin"
  :hook (conf-mode-hook . hl-todo-mode)
  :mode ("\\.env\\'" . conf-mode)
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

(leaf *javascript-conig
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
  :mode ("\\.yml\\'" . yaml-mode)
  :hook (yaml-mode-hook . hl-todo-mode))

(leaf systemd :ensure t)

(leaf lua-mode :ensure t)

(leaf org
  :doc "org-mode config"
  :ensure t
  :require t
  :mode ("\\.org\\'" . org-mode)
  :custom
  ;; priority
  (org-priority-highest         . 1)
  (org-priority-lowest          . 4)
  (org-priority-default         . 3)
  ;; keywords
  (org-refile-targets           . '((org-agenda-files :maxlevel . 3)))
  (org-todo-keywords            . '((sequence "TODO(t)" "NEXT(n)" "STARTED(s!)" "WAITING(w@/!)" "PROJ(p)" "|"
                                              "DONE(d!)" "CANCELLED(c@)")))
  (org-capture-templates        . `( ;; NOTE: require material icons
                                    ("t" ,(format  "%s Task to Inbox" (all-the-icons-faicon "check" :v-adjust 0.0))
                                     entry (file+headline org-todofile "Inbox")
                                     "** TODO %?\n  %U\n  %a"
                                     :empty-lines-before 1) ; %u->%t
                                    ("n" ,(format  "%s Note to Inbox" (all-the-icons-faicon "sticky-note" :v-adjust 0.0))
                                     entry (file+headline "" "Inbox")
                                     "** %?\nEntered on %U\n %i\n %a"
                                     :empty-lines-before 1)
                                    ))
  (org-log-done                 . 'time)
  (org-clock-clocked-in-display . 'frame-title)
  ;; for org src
  (org-src-preserve-indentation . t)
  ;; Edit settings
  (org-auto-align-tags . t)
  (org-tags-column . 75)
  (org-catch-invisible-edits . 'show-and-error)
  (org-special-ctrl-a/e . nil)
  (org-insert-heading-respect-content . nil)
  ;; agenda filter preset
  (org-agenda-tag-filter-preset . '("-agenda_ignore"))
  (org-agenda-include-diary     . t)

  :config
  (leaf *org-config-directory :if (f-directory-p "~/org/orgfiles")
    :custom
    (org-directory              . "~/org/orgfiles")
    :config
    (custom-set-variables
     '(org-default-notes-file   (concat org-directory "/notes.org"))
     '(org-agenda-files         (directory-files org-directory t ".org$" t)))
    (defvar org-todofile        (concat org-directory "/todo.org") "default org todo file path"))

  (defun rdm/org-goto-dir ()
    "open dir '~/org' with dired"
    (interactive)
    (find-file "~/org"))

  (leaf *org-appearance
    :custom
    '(
      (org-startup-truncated     . t)
      (org-startup-folded        . nil)
      (org-image-actual-width    . nil)
      (org-hide-emphasis-markers . t)
      (org-pretty-entities       . nil)
      (org-ellipsis              . "…")
      )
    (org-agenda-block-separator . ?─)
    (org-agenda-time-grid . '((daily today require-timed)
                              (800 1000 1200 1400 1600 1800 2000)
                              " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
    (org-agenda-current-time-string . "⭠ now ─────────────────────────────────────────────────")
    :config
    (leaf *org-latex-preview-svg :if (executable-find "dvisvgm") :disabled t
      :doc "if dvisvgm available, startup with latex preview"
      :custom
      (org-preview-latex-default-process . 'dvisvgm)
      (org-startup-with-latex-preview    . nil)
      :config
      (custom-set-variables
       '(org-format-latex-options (plist-put org-format-latex-options :scale 1.4)))
      )

    (leaf *org-latex-preview-imagemagick
      :doc "use imagemagick for latex preview"
      :require ob-latex
      :after org
      :custom
      (org-latex-create-formula-image-program . 'imagemagick)
      (org-startup-with-latex-preview    . nil)
      :config
      (custom-set-variables
       '(org-format-latex-options (plist-put org-format-latex-options :scale 1.6)))
      (org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))
      )

    (custom-set-faces
     `(org-hide ((nil (:foreground ,(face-attribute 'default :background))))) ; force load bg-color
     `(org-table ((nil :family ,font-for-tables))) ;; use 1:2 font for table
     )

    (leaf org-bullets :disabled t
      :ensure t
      :doc "pretty looking bullet list"
      :hook (org-mode-hook . org-bullets-mode))

    (leaf org-modern
      :straight
      (org-modern :type git :host github
                  :repo "minad/org-modern" :branch "main")
      :doc "better looking org mode"
      :preface
      ;; NOTE: do not use org-modern-todo
      (let ((bg-c (doom-color 'bg))
            (box-p '(:line-width (0 . -4))))
        (custom-set-variables
         '(org-modern-todo  nil)
         '(org-todo-keyword-faces
           `(
             ("TODO"    . ((t (:foreground ,bg-c :box ,box-p :background ,(doom-color 'green)))))
             ("NEXT"    . ((t (:foreground ,bg-c :box ,box-p :background ,(doom-color 'blue)))))
             ("STARTED" . ((t (:foreground ,bg-c :box ,box-p :background ,(doom-color 'cyan)))))
             ("WAITING" . ((t (:foreground ,bg-c :box ,box-p :background ,(doom-color 'yellow)))))
             ("PROJ"    . ((t (:foreground ,bg-c :box ,box-p :background ,(doom-color 'magenta)))))
             )))
        )
      :hook
      (org-mode-hook . org-modern-mode)
      :custom
      ;;org-modern custom
      (org-modern-label-border . 4)
      (org-modern-table . nil)
      (org-modern-horizontal-rule . nil)
      )
    )

  (leaf *reconfig-org-vi-key :if (fboundp 'evil-mode)
    :config
    (evil-set-initial-state 'org-agenda-mode 'insert)
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

  (leaf org-pomodoro
    :doc "configure org-pomodoro"
    :ensure t
    :custom
    (org-pomodoro-ask-upon-killing   . t)
    (org-pomodoro-format             . "🍅 %s")
    (org-pomodoro-short-break-format . "☕ %s")
    (org-pomodoro-long-break-format  . "🥞 %s")
    )

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
  ;; (org-mode-hook . org-mode-reftex-setup)
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
    ("o"   rdm/org-goto-dir)
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
   ("C-c C-o 0" . rdm/org-goto-dir)
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
  :config
  (leaf lsp-haskell :if (executable-find "haskell-language-server-wrapper")
    :ensure t
    :doc "if haskell-language-server available in PATH, enable lsp-haskell"
    :hook
    ((haskell-mode-hook
      haskell-literate-mode-hook) . lsp)
    )
  )

(leaf elfeed :if (file-exists-p "~/.config/elfeed")
  :ensure t
  :url "https://github.com/skeeto/elfeed"
  :preface (defconst elfeed-dir-path "~/.config/elfeed/" "elfeed config path")
  :hook
  (elfeed-search-mode-hook . (lambda () (rdm/sw-lnsp 0.55)
                               (face-remap-add-relative 'hl-line `(:background ,(doom-color 'base3)))))
  :init
  (add-to-list 'undo-tree-incompatible-major-modes 'elfeed-search-mode)
  (leaf elfeed-org :if (file-exists-p (expand-file-name "elfeed.org" elfeed-dir-path))
    :ensure t
    :url "https://github.com/remyhonig/elfeed-org"
    :init (elfeed-org)
    :config (setq rmh-elfeed-org-files (list (expand-file-name "elfeed.org" elfeed-dir-path))))

  (leaf *elfeed-patch-entry-switch
    :doc "patch entry-switch behavior"
    :custom
    (elfeed-show-entry-switch . #'display-buffer)
    :config
    (defun rdm/elfeed-show-next ()
      (interactive)
      (let ((elfeed-show-entry-switch #'switch-to-buffer))
        (elfeed-show-next)))

    (defun rdm/elfeed-show-prev ()
      (interactive)
      (let ((elfeed-show-entry-switch #'switch-to-buffer))
        (elfeed-show-prev)))

    (evil-define-key 'normal elfeed-show-mode-map
      (kbd "C-j") 'rdm/elfeed-show-next
      "]]"        'rdm/elfeed-show-next
      "gj"        'rdm/elfeed-show-next
      (kbd "C-k") 'rdm/elfeed-show-prev
      "[["        'rdm/elfeed-show-prev
      "gk"        'rdm/elfeed-show-prev)
   )

  (leaf *elfeed-func
    :doc "custom defined func"
    :config
    (defun rdm/elfeed-search-toggle-star ()
      "add/remove star tag on elfeed entry"
      (interactive)
      (apply 'elfeed-search-toggle-all '(star)))

    (defun rdm/elfeed-search-tag-later-unread (entry)
      "add later and unread tag on elfeed entry"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (elfeed-tag entry 'unread 'later)
        (elfeed-search-update-entry entry)
        (unless elfeed-search-remain-on-entry (forward-line))))

    (defun rdm/elfeed-search-tag-junk (entry)
      "add/remove junk tag on elfeed entry"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (elfeed-untag entry 'unread)
        (elfeed-tag entry 'junk)
        (elfeed-search-update-entry entry)
        (unless elfeed-search-remain-on-entry (forward-line))))

    (defun rdm/elfeed-show-entry-share (&optional use-generic-p)
      "Copy the entry title and URL as org link to the clipboard."
      (interactive "P")
      (let* ((link (elfeed-entry-link elfeed-show-entry))
             (title (elfeed-entry-title elfeed-show-entry))
             (feed-info (concat title "\n" link)))
        (when feed-info
          (kill-new feed-info)
          (x-set-selection 'PRIMARY feed-info)
          (message "Yanked: %s" feed-info))))

    (defun rdm/elfeed-search-entry-share (entry)
      "Copy the entry title and URL as org link to the clipboard."
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (let* ((link (elfeed-entry-link entry))
               (title (elfeed-entry-title entry))
               (feed-info (concat title "\n" link)))
          (when feed-info
            (kill-new feed-info)
            (x-set-selection 'PRIMARY feed-info)
            (message "Yanked: %s" feed-info)))))

    (defun rdm/elfeed-show-eww-open (&optional use-generic-p)
      "open with eww"
      (interactive "P")
      (let ((browse-url-browser-function #'eww-browse-url))
        (elfeed-show-visit use-generic-p)))

    (defun rdm/elfeed-search-eww-open (&optional use-generic-p)
      "open with eww"
      (interactive "P")
      (let ((browse-url-browser-function #'eww-browse-url))
        (elfeed-search-browse-url use-generic-p)))

    (defun rdm/elfeed-search-untag-later-unread (entry)
      "Remove the 'unread' and 'later' tag from entry.
based on elfeed-search-show-entry"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (elfeed-untag entry 'unread 'later)
        (elfeed-search-update-entry entry)
        (unless elfeed-search-remain-on-entry (forward-line))))

    (defun rdm/elfeed-search-untag-unread (entry)
      "Remove the 'unread' tag from entry and recenter.
based on elfeed-search-show-entry"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (elfeed-untag entry 'unread)
        (elfeed-search-update-entry entry)
        (forward-line) (recenter)))

    (defun rdm/elfeed-search-show-entry (entry)
      "Display the currently selected item in a buffer.
based on elfeed-search-show-entry"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (elfeed-untag entry 'unread 'later)
        (elfeed-search-update-entry entry)
        (forward-line) (recenter)
        (elfeed-show-entry entry)))

    (defun rdm/elfeed-search-show-prev-entry (entry)
      "Display currently selected item in buffer.
based on elfeed-search-show-entry"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (elfeed-untag entry 'unread 'later)
        (elfeed-search-update-entry entry)
        (forward-line -1) (recenter)
        (elfeed-show-entry entry)))

    (defun rdm/elfeed-search-browse-url (&optional use-generic-p)
      "open with browser, untag unread and later.
based on elfeed-search-browse-url"
      (interactive "P")
      (let ((buffer (current-buffer))
            (entries (elfeed-search-selected)))
        (cl-loop for entry in entries
                 do (elfeed-untag entry 'unread 'later)
                 when (elfeed-entry-link entry)
                 do (if use-generic-p
                        (browse-url-generic it)
                      (browse-url it)))
        ;; `browse-url' could have switched to another buffer if eww or another
        ;; internal browser is used, but the remainder of the functions needs to
        ;; run in the elfeed buffer.
        (with-current-buffer buffer
          (mapc #'elfeed-search-update-entry entries)
          (unless (or elfeed-search-remain-on-entry (use-region-p))
            (forward-line)))))

    (defun rdm/elfeed-search-filter-feed-name (entry)
      "filter unread via highlighted feed address"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (let* ((meta (elfeed-entry-feed entry))
               (title (elfeed-meta meta :title))
               (title-esc (replace-regexp-in-string " " "\\s-" title t t))
               (filter-str (concat "=" title-esc)))
          (when filter-str
            (elfeed-search-set-filter (concat filter-str " +unread -later"))))))

    (evil-define-key 'normal elfeed-search-mode-map
      "m"  'rdm/elfeed-search-toggle-star
      "l"  'rdm/elfeed-search-tag-later-unread
      "b"  'rdm/elfeed-search-eww-open
      "u"  'rdm/elfeed-search-untag-later-unread
      "Y"  'rdm/elfeed-search-entry-share
      "F"  'rdm/elfeed-search-filter-feed-name
      "f"  'hydra-elfeed-search-filter/body
      "ta" 'elfeed-search-tag-all
      "tr" 'elfeed-search-untag-all
      "tj" 'rdm/elfeed-search-tag-junk
      "go" 'rdm/elfeed-search-browse-url
      "oo" 'rdm/elfeed-search-show-entry
      (kbd "RET") 'rdm/elfeed-search-untag-unread
      (kbd "SPC") 'rdm/elfeed-search-untag-unread
      (kbd "S-SPC") 'evil-previous-line
      (kbd "C-j") 'rdm/elfeed-search-show-entry
      (kbd "M-j") 'rdm/elfeed-search-show-entry
      "]]"        'rdm/elfeed-search-show-entry
      "gj"        'rdm/elfeed-search-show-entry
      (kbd "C-k") 'rdm/elfeed-search-show-prev-entry
      (kbd "M-k") 'rdm/elfeed-search-show-prev-entry
      "[["        'rdm/elfeed-search-show-prev-entry
      "gk"        'rdm/elfeed-search-show-prev-entry
      )

    (evil-define-key 'normal elfeed-show-mode-map
      "b"  'rdm/elfeed-show-eww-open
      "Y"  'rdm/elfeed-show-entry-share
      "ta" 'elfeed-show-tag
      "tr" 'elfeed-show-untag))

  :custom
  (elfeed-db-directory .  "~/.config/elfeed/.db")
  (elfeed-enclosure-default-dir .  elfeed-dir-path)
  (elfeed-search-filter . "@1-months-ago +unread -later -junk")
  (elfeed-search-title-max-width . 95)
  (elfeed-search-date-format . '("%Y-%m-%d (%a) %k:%M" 22 :left))

  :hydra
  (hydra-elfeed-search-filter (nil nil)
                              "elfeed filters"
                              ("u" (elfeed-search-set-filter "+unread -junk")        "all unread")
                              ("a" (elfeed-search-set-filter " -junk")               "all entries")
                              ("n" (elfeed-search-set-filter "+news +unread -later -junk") "news")
                              ("N" (elfeed-search-set-filter "+news -junk")          "news(all)")
                              ("l" (elfeed-search-set-filter "+later +unread") "later")
                              ("L" (elfeed-search-set-filter "+later -unread") "later(read)")
                              ("s" (elfeed-search-set-filter "+star")          "starred")
                              )

  :config
  (leaf *patch-elfeed-list-face
    :config
    (dolist (face '(elfeed-search-tag-face
                    elfeed-search-date-face
                    elfeed-search-feed-face
                    elfeed-search-title-face
                    elfeed-search-unread-title-face))
      (set-face-attribute face nil :family font-for-tables :weight 'normal :height 1.0))
    )

  ;; :hook (elfeed-show-mode-hook . darkroom-mode)
  )

(leaf eww
  :tag "builtin"
  :doc "text base web browser"
  :custom
  (shr-width . 70)
  (shr-indentation . 4)
  (shr-use-fonts . t)
  (shr-bullet . "* ")
  (eww-search-prefix . "https://www.google.com/search?q=")
  :hook
  (eww-mode-hook . (lambda () (rdm/sw-lnsp 0.75)))
  :config
  (leaf *eww-patch-faces :after doom-themes
    :config
    (custom-set-faces
     `(eww-form-text ((t :background ,(doom-color 'bg-alt))))
     )
    )

  (leaf *eww-bind-all-the-icons :if (fboundp 'all-the-icons-insert)
    :custom
    `(
      (shr-bullet                        . ,(format "%s " (all-the-icons-faicon "caret-right" :v-adjust -0.05)))
      (eww-form-checkbox-symbol          . ,(all-the-icons-faicon "square-o"))
      (eww-form-checkbox-selected-symbol . ,(all-the-icons-faicon "check-square-o"))
      )
    )

  (leaf *eww-toggle-inhibit-images :emacs>= "28.1"
    :doc "disable image by default"
    :custom
    (shr-inhibit-images . t)
    :config
    (evil-define-key 'normal eww-mode-map
      "i" 'eww-toggle-images
      "I" 'eww-toggle-images)
    )


  (leaf *eww-use-color-config
    :doc "config colors in eww"
    :custom
    (shr-use-colors . nil)
    :config
    (evil-define-key 'normal eww-mode-map
      "c" 'eww-toggle-colors
      "C" 'eww-toggle-colors)
    )
)
