;;; langs.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;
;; major modes loaded by init.el
;;

;;; Code:

(leaf emacs-lisp-mode
  :tag "builtin"
  :hook
  (emacs-lisp-mode-hook . hs-minor-mode)
  (emacs-lisp-mode-hook . flycheck-mode)
  (emacs-lisp-mode-hook . eldoc-mode)
  (emacs-lisp-mode-hook . highlight-symbol-mode)
  :init
  (leaf highlight-defined
    :ensure t
    :config
    (leaf *patch-highlight-defined-faces :after doom-themes
      :doc "patch face color from font-lock-face"
      :preface
      (defun patch-highlight-defined-faces ()
        (custom-set-faces
         `(highlight-defined-function-name-face
           ((t (:foreground ,(face-attribute 'font-lock-keyword-face :foreground)))))
         `(highlight-defined-builtin-function-name-face
           ((t (:foreground ,(face-attribute 'font-lock-keyword-face :foreground)))))
         `(highlight-defined-special-form-name-face
           ((t (:foreground ,(face-attribute 'font-lock-type-face :foreground)))))
         `(highlight-defined-macro-name-face
           ((t (:foreground ,(face-attribute 'font-lock-preprocessor-face :foreground)))))
         `(highlight-defined-variable-name-face
           ((t (:foreground ,(face-attribute 'font-lock-variable-name-face :foreground)))))
         ))
      :hook
      ((highlight-defined-mode-hook
        after-load-theme-hook) . patch-highlight-defined-faces)
      )

    (leaf *highlight-symbol-evil-bind
      :config
      (evil-define-key 'normal emacs-lisp-mode-map
        (kbd "M-n") 'highlight-symbol-next
        (kbd "M-p") 'highlight-symbol-prev
        "gn" 'highlight-symbol-next
        "gp" 'highlight-symbol-prev
        "gN" 'highlight-symbol-next-in-defun
        "gP" 'highlight-symbol-prev-in-defun))

    :hook (emacs-lisp-mode-hook . highlight-defined-mode)
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
      (when (not pipenv-mode)
        (pipenv-mode))
      (when (eq python-shell-virtualenv-root nil)
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
  (web-mode-markup-indent-offset             . 2)
  (web-mode-css-indent-offset                . 2)
  (web-mode-code-indent-offset               . 2)
  ;; auto close tags
  (web-mode-enable-auto-pairing              . t)
  (web-mode-enable-auto-closing              . t)
  (web-mode-auto-close-style                 . 2)
  (web-mode-tag-auto-close-style             . 2)
  ;; auto colorize css
  (web-mode-enable-css-colorization          . t)
  (web-mode-enable-block-face                . t)
  ;; (web-mode-enable-part-face t)
  (web-mode-enable-comment-interpolation     . t)
  (web-mode-enable-heredoc-fontification     . t))

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
  :mode ("\\.org\\'" . org-mode)
  :custom
  ;; priority
  (org-priority-highest               . 1)
  (org-priority-lowest                . 4)
  (org-priority-default               . 3)
  ;; keywords
  (org-refile-targets                 . '((org-agenda-files :maxlevel . 3)))
  (org-todo-keywords                  . '((sequence "TODO(t)" "NEXT(n)" "STARTED(s!)" "WAITING(w@/!)" "PROJ(p)" "|"
                                                    "DONE(d!)" "CANCELLED(c@)")))
  (org-capture-templates              . `( ;; NOTE: require material icons
                                          ("t" ,(format "%s  Task to Inbox" (nerd-fonts "fa-check"))
                                           entry (file+headline org-todofile "Inbox")
                                           "-*- TODO %?\nEntered on %U\n%a"
                                           :empty-lines-before 1) ; %u->%t
                                          ("n" ,(format "%s  Note to Inbox" (nerd-fonts "fa-sticky-note"))
                                           entry (file+headline "" "Inbox")
                                           "-*- %?\nEntered on %U\n %i\n%a"
                                           :empty-lines-before 1)
                                          ))
  (org-log-done                       . 'time)
  (org-clock-clocked-in-display       . 'frame-title)
  ;; for org src
  (org-src-preserve-indentation       . t)
  ;; Edit settings
  (org-auto-align-tags                . t)
  (org-tags-column                    . 75)
  (org-catch-invisible-edits          . 'show-and-error)
  (org-special-ctrl-a/e               . nil)
  (org-insert-heading-respect-content . nil)
  ;; agenda filter preset
  (org-agenda-tag-filter-preset       . '("-agenda_ignore"))
  (org-agenda-include-diary           . t)
  :init
  (leaf evil-org
    :ensure t
    :hook
    (org-mode-hook . evil-org-mode)
    :preface
    (evil-set-initial-state 'org-agenda-mode 'motion)
    :init
    (require 'evil-org-agenda)
    (add-hook 'org-agenda-mode-hook #'evil-org-agenda-set-keys)
    :config
    ;; enable evil-org features
    (evil-org-set-key-theme
     '(textobjects insert navigation additional shift todo heading))
    (leaf *patch-evil-org-ctrl-behavior
      :hook
      (conf-on-term-hook . (lambda () (setq evil-want-C-i-jump nil)))
      (conf-on-gui-hook  . (lambda () (setq evil-want-C-i-jump t)))
      )
    )

  (leaf *reconfig-org-consult-key :after consult
    :doc "bind consult functions if available"
    :bind
    (:org-mode-map
     ("M-g o"   . consult-org-heading)
     ("M-g M-o" . consult-org-heading)))

  :config
  (leaf *org-config-directory
    :if (file-directory-p (expand-file-rec '("org" "orgfiles") (getenv "HOME")))
    :custom
    `(
      (org-directory              . ,(expand-file-rec '("org" "orgfiles") (getenv "HOME")))
      (org-default-notes-file     . ,(expand-file-name
                                      "notes.org" (expand-file-rec '("org" "orgfiles") (getenv "HOME"))))
      (org-agenda-files           . `,(directory-files
                                       (expand-file-rec '("org" "orgfiles") (getenv "HOME")) t ".org$" t))
      )
    :config
    (defvar org-todofile
      (expand-file-name "todo.org" org-directory)
      "default org todo file path"))

  (defun rdm/org-goto-dir ()
    "open dir '~/org' with dired"
    (interactive)
    (find-file "~/org"))

  (leaf *org-appearance
    :custom
    (org-startup-truncated          . t)
    (org-startup-folded             . nil)
    (org-image-actual-width         . nil)
    (org-hide-emphasis-markers      . t)
    (org-pretty-entities            . nil)
    (org-ellipsis                   . "…")
    (org-agenda-block-separator     . ?─)
    (org-agenda-time-grid           . '((daily today require-timed)
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
      (customize-set-variable
       'org-format-latex-options (plist-put org-format-latex-options :scale 1.4))
      )

    (leaf *org-latex-preview-imagemagick
      :doc "use imagemagick for latex preview"
      :require ob-latex
      :after org
      :custom
      (org-latex-create-formula-image-program . 'imagemagick)
      (org-startup-with-latex-preview         . nil)
      :config
      (customize-set-variable
       'org-format-latex-options (plist-put org-format-latex-options :scale 1.6))
      (add-to-list 'org-babel-load-languages '(latex . t))
      )

    (leaf *patch-org-mode-header-size
      :doc "set larger face for org-level-1 to 5"
      :preface
      (defun patch-org-mode-header-size ()
        (set-face-attribute 'org-document-title nil :height 2.0)
        (set-face-attribute 'org-level-1 nil :height 1.75)
        (set-face-attribute 'org-level-2 nil :height 1.50)
        (set-face-attribute 'org-level-3 nil :height 1.25)
        (set-face-attribute 'org-level-4 nil :height 1.1)
        (set-face-attribute 'org-level-5 nil :height 1.0))
      :hook
      (after-load-theme-hook . patch-org-mode-header-size)
      )

    (leaf *patch-org-mode-faces :after doom-themes
      :preface
      (defun patch-org-mode-face ()
        (custom-set-faces
         `(org-hide             ((t (:foreground ,(doom-color 'bg))))) ; org-hide
         `(org-table             ((t :family ,font-for-tables)))       ; use 1:2 font for table
         `(org-ellipsis         ((t (:background ,(doom-color 'bg)))))
         `(org-block-begin-line ((t (:background ,(doom-color 'bg)))))
         `(org-block-end-line   ((t (:background ,(doom-color 'bg)))))
         ))
      :hook ;; without `doom-color': (face-attribute 'default :background)
      ((after-load-theme-hook
        after-load-theme-hook) . patch-org-mode-face)
      )

    (leaf org-bullets :disabled t
      :ensure t
      :doc "pretty looking bullet list"
      :hook (org-mode-hook . org-bullets-mode))

    (leaf *patch-org-todo-faces :after doom-themes
      :doc  "prettify todo keywords"
      :preface
      (defun patch-org-todo-faces ()
        (customize-set-variable
         'org-todo-keyword-faces
         `(
           ("TODO"    . ((t (:foreground ,(doom-color 'bg) :box '(:line-width (0 . -4)) :background ,(doom-color 'green)))))
           ("NEXT"    . ((t (:foreground ,(doom-color 'bg) :box '(:line-width (0 . -4)) :background ,(doom-color 'blue)))))
           ("STARTED" . ((t (:foreground ,(doom-color 'bg) :box '(:line-width (0 . -4)) :background ,(doom-color 'cyan)))))
           ("WAITING" . ((t (:foreground ,(doom-color 'bg) :box '(:line-width (0 . -4)) :background ,(doom-color 'yellow)))))
           ("PROJ"    . ((t (:foreground ,(doom-color 'bg) :box '(:line-width (0 . -4)) :background ,(doom-color 'magenta)))))
           )))
      :hook
      (after-load-theme-hook . patch-org-todo-faces)
      )

    (leaf org-modern
      :straight
      (org-modern :type git :host github
                  :repo "minad/org-modern" :branch "main")
      :doc "better looking org mode"
      :hook
      (org-mode-hook . org-modern-mode)
      :custom
      ;;org-modern custom
      (org-modern-todo            . nil)
      (org-modern-label-border    . 4)
      (org-modern-table           . nil)
      (org-modern-horizontal-rule . nil)
      )
    )

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
    :ensure t
    )

  (leaf *configure-bibtex
    :defun reftex-parse-all
    :config
    (defun org-mode-reftex-setup ()
      "setup reftex on org-mode doc"
      (load-library "reftex")
      (and (buffer-file-name)
           (file-exists-p (buffer-file-name))
           (reftex-parse-all))
      (define-key org-mode-map (kbd "C-c )") 'reftex-citation))
   )
  ;; (use-package ox-bibtex)

  (leaf org-ref :ensure t)

  (leaf org-roam
    :ensure t
    :pre-setq (org-roam-v2-ack . t)
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
     (:hint nil)
     (format "\
^^^                           %s org roam^^^
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
" (nerd-fonts "fa-file-text-o"))
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
    `(
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
      (org-roam-directory         . ,(expand-file-rec '("org" "roam") (getenv "HOME")))
      (org-roam-index-file        . ,(expand-file-rec '("org" "roam" "Index.org") (getenv "HOME")))
      (org-roam-db-location       . ,(cache-sub-file  "org-roam.db"))
      )
    :hook (org-roam-capture-new-node-hook . org-roam-db-sync)
    :config
    (org-roam-db-autosync-mode)

    (leaf org-roam-ui :after org-roam
      :doc "org-roam frontend package"
      :straight
      (org-roam-ui :type git :host github
                   :repo "org-roam/org-roam-ui" :branch "main"
                   :files ("*.el" "out"))
      :custom
      (org-roam-ui-sync-theme     . t)
      (org-roam-ui-follow         . t)
      (org-roam-ui-update-on-save . t)
      (org-roam-ui-open-on-start  . t)
      )
    )

  (leaf org-pdftools :after pdf-tools
    :doc "org mode for pdf-tools integration (contains org-noter)"
    :ensure t
    :hook (org-mode-hook . org-pdftools-setup-link)
    :custom
    `(
      (org-noter-notes-search-path . `(,(expand-file-rec '("org" "roam" "noter") (getenv "HOME"))))
      )
    )

  :hook
  ;; (org-mode-hook . org-mode-reftex-setup)
  (org-mode-hook . org-indent-mode)

  :hydra
  ((hydra-org
    (:hint nil)
    (format "\
^^^                              %s org functions^^^
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
" (nerd-fonts "fa-file-text-o"))
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
  (("C-c l"     . org-store-link)
   ("C-c c"     . org-capture)
   ("C-c a"     . org-agenda)
   ("C-c b"     . org-iswitchb)
   ("C-c C-o 0" . rdm/org-goto-dir)
   ("M-6"       . hydra-org/body)
   ("<f6>"      . hydra-org/body))
  )

(leaf *lml-modes
  :doc "lightweight markup languages"
  :config
  (leaf markdown-mode
    :ensure t
    ;; :hook (markdown-mode . visual-line-mode)
    :commands (markdown-mode gfm-mode)
    :mode
    (("README\\.md\\'"  . gfm-mode)
     ("\\.md\\'"
      "\\.markdown\\'") . markdown-mode)
    :custom
    (markdown-command . "multimarkdown")
    (markdown-hide-markup . t)
    (markdown-header-scaling . t)
    (markdown-header-scaling-values . '(1.75 1.5 1.25 1.1 1.0 1.0))
    (markdown-enable-highlighting-syntax . t)
    (markdown-enable-math . t)
    :config
    (evil-define-key 'normal markdown-mode-map
      "j" 'evil-next-visual-line
      "k" 'evil-previous-visual-line
      "gj" 'evil-next-line
      "gk" 'evil-previous-line)
    ;; modify table face to 1:2
    (set-face-attribute 'markdown-table-face nil :family font-for-tables)
    )

  (leaf valign
    :doc "CJK supported table vertical alignment (on graphical environment)"
    :ensure t
    :custom
    (valign-fancy-bar . t)
    :preface
    (defun valign-buffer-on-gui ()
      (mapc #'(lambda (buffer)
                (with-current-buffer buffer
                  (when (or (eq major-mode #'org-mode)
                            (eq major-mode #'markdown-mode))
                    (valign-mode +1))))
            (buffer-list))
      )
    (defun valign-remove-buffer-on-term ()
      (mapc #'(lambda (buffer)
                (with-current-buffer buffer
                  (when (or (eq major-mode #'org-mode)
                            (eq major-mode #'markdown-mode))
                    (valign-mode -1))))
            (buffer-list))
      )
    :hook
    ((org-mode-hook
      markdown-mode-hook) . (lambda () (when (display-graphic-p) (valign-mode +1))))
    (conf-on-gui-hook     . valign-buffer-on-gui)
    (conf-on-term-hook    . valign-remove-buffer-on-term)
    )
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
  (elfeed-search-mode-hook . (lambda () (rdm/sw-lnsp 0.75)))
  :init
  (add-to-list 'undo-tree-incompatible-major-modes 'elfeed-search-mode)

  (leaf elfeed-org :if (file-exists-p (expand-file-name "elfeed.org" elfeed-dir-path))
    :ensure t
    :url "https://github.com/remyhonig/elfeed-org"
    :init (elfeed-org)
    :custom
    `((rmh-elfeed-org-files . `,(list (expand-file-name "elfeed.org" elfeed-dir-path)))))

  (leaf *patch-elfeed-faces :after doom-themes
    :hook
    ((elfeed-search-update-hook
      after-load-theme-hook) . (lambda ()
                                 (face-remap-add-relative 'hl-line `(:background ,(doom-color 'bg-alt)))
                                 (set-face-attribute 'elfeed-search-tag-face nil :foreground `,(doom-color 'green))
                                 (dolist (face '(elfeed-search-tag-face
                                                 elfeed-search-date-face
                                                 elfeed-search-feed-face
                                                 elfeed-search-title-face
                                                 elfeed-search-unread-title-face))
                                   (set-face-attribute
                                    face nil :family font-for-tables :height 1.0))))
    )

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

    (defun rdm/elfeed-show-default-open (&optional use-generic-p)
      "open with default browser"
      (interactive "P")
      (elfeed-show-visit use-generic-p))

    (defun rdm/elfeed-search-default-open (&optional use-generic-p)
      "open with default browser"
      (interactive "P")
      (elfeed-search-browse-url use-generic-p))

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

    (defun rdm/elfeed-unread-p (entry)
      "returns t if selected entry has unread tag"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (elfeed-tagged-p 'unread entry))

    (defun rdm/elfeed-search-mark-read (entry)
      "mark entry read and remove later tag, then update elfeed-search"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (elfeed-untag entry 'unread 'later)
      (elfeed-search-update-entry entry))

    (defun rdm/elfeed-search-mark-read-show (entry)
      "Mark entry as read and show"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (elfeed-show-entry entry)
      (rdm/elfeed-search-mark-read entry)
      )

    (defun rdm/elfeed-search-untag-later-unread (entry)
      "Remove the 'unread' and 'later' tag from entry.
based on elfeed-search-show-entry"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (rdm/elfeed-search-mark-read entry)
        (unless elfeed-search-remain-on-entry (forward-line))))

    (defun rdm/elfeed-search-untag-unread (entry)
      "Remove the 'unread' tag from entry and recenter.
based on elfeed-search-show-entry"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (rdm/elfeed-search-mark-read entry)
        (forward-line) (recenter))
      )

    (defun rdm/elfeed-search-show-read-entry ()
      "Display the currently selected item in a buffer.
based on elfeed-search-show-entry.
  open first line if unread, if not show next and mark unrerad.
"
      (interactive)
      (if (and (eq (line-number-at-pos (point)) 1)
               (call-interactively #'rdm/elfeed-unread-p))
          (call-interactively #'rdm/elfeed-search-mark-read-show)
        (progn (forward-line) (recenter)
               (call-interactively #'rdm/elfeed-search-mark-read-show))
        ))

    (defun rdm/elfeed-search-show-read-prev-entry ()
      "Display currently selected item in buffer.
based on elfeed-search-show-entry"
      (interactive)
      (forward-line -1) (recenter)
      (call-interactively #'rdm/elfeed-search-mark-read-show))

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
      "b"  'rdm/elfeed-search-default-open
      "u"  'rdm/elfeed-search-untag-later-unread
      "Y"  'rdm/elfeed-search-entry-share
      "F"  'rdm/elfeed-search-filter-feed-name
      "f"  'hydra-elfeed-search-filter/body
      "ta" 'elfeed-search-tag-all
      "tr" 'elfeed-search-untag-all
      "tj" 'rdm/elfeed-search-tag-junk
      "go" 'rdm/elfeed-search-browse-url
      "oo" 'rdm/elfeed-search-mark-read-show
      (kbd "RET") 'rdm/elfeed-search-show-read-entry
      (kbd "SPC") 'rdm/elfeed-search-show-read-prev-entry
      (kbd "S-SPC") 'evil-previous-line
      (kbd "C-j") 'rdm/elfeed-search-show-read-entry
      (kbd "M-j") 'rdm/elfeed-search-show-read-entry
      "]]"        'rdm/elfeed-search-show-read-entry
      "gj"        'rdm/elfeed-search-show-read-entry
      (kbd "C-k") 'rdm/elfeed-search-show-read-prev-entry
      (kbd "M-k") 'rdm/elfeed-search-show-read-prev-entry
      "[["        'rdm/elfeed-search-show-read-prev-entry
      "gk"        'rdm/elfeed-search-show-read-prev-entry
      )

    (evil-define-key 'normal elfeed-show-mode-map
      "b"  'rdm/elfeed-show-default-open
      "Y"  'rdm/elfeed-show-entry-share
      "ta" 'elfeed-show-tag
      "tr" 'elfeed-show-untag))

  :custom
  (elfeed-db-directory           . "~/.config/elfeed/.db")
  (elfeed-enclosure-default-dir  . elfeed-dir-path)
  (elfeed-search-filter          . "@1-months-ago +unread -later -junk")
  (elfeed-search-title-max-width . 95)
  (elfeed-search-date-format     . '("%Y-%m-%d (%a) %k:%M" 22 :left))

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
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; langs.el ends here
