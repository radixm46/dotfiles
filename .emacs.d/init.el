;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; emacs minimal configuration by radixM491VA
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default 'buffer-file-coding-system 'utf-8)
(global-display-line-numbers-mode)
(column-number-mode t)
(ido-mode t)
(show-paren-mode t)
(setq ring-bell-function 'ignore)


;; Initial frame settings for GUI
(setq default-frame-alist
  (append (list
    '(font . "HackGen Console for Powerline-16"))
  default-frame-alist))

(tool-bar-mode -1)


;; log
(setq message-log-maxa 10000)
(setq history-length 1000)
(setq history-delete-duplicates t)

;; locale and encoding
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-coding-systems 'utf-8)
;; (setq coding-system-for-read 'utf-8) ;; conflicts ddskk
(setq coding-system-for-write 'utf-8)

(setq-default tab-width 4 indent-tabs-mode nil)

(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")
(which-function-mode 1)
(transient-mark-mode 1)


;; init package
(require 'package)

;; use custom.el for package-selected-packages
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
    (load custom-file))

(setq package-archives (append
   '(
     ("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ;; ("marmalade" . "http://marmalade-repo.org/packages/")
     ("org" . "http://orgmode.org/elpa/")
     )
   package-archives
   ))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


(setq load-prefer-newer t)


;; auto load use-package package
(unless (package-installed-p 'use-package)  ;; can't notify upgradable package
    (package-install 'use-package)
    (require 'use-package)
)

;; skk config(use default package.el)
(unless (package-installed-p 'ddskk)
  (package-install 'ddskk)
)
(package-initialize)
(setq default-input-method "japanese-skk")
(require 'skk-study)


;; -------- package config under use-package --------


;; enable evil
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-u") 'scroll-down-command)
  ;; enable emacs cursor movement in insert mode
  (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-f") 'forward-char)
  (define-key evil-insert-state-map (kbd "C-b") 'backward-char)
  ;(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
  ;(define-key evil-insert-state-map (kbd "C-n") 'next-line)
)


;; enable relative line number
(use-package linum-relative
  :ensure t
  :config (linum-relative-global-mode)
)

(use-package all-the-icons
  :ensure t
) ; need installation by all-the-icons-color-install-fonts

;; enable doom-modeline
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1)
  ;; How tall the mode-line should be. It's only respected in GUI.
  ;; If the actual char height is larger, it respects the actual height.
  (setq doom-modeline-height 25)

  ;; How wide the mode-line bar should be. It's only respected in GUI.
  (setq doom-modeline-bar-width 3)

  ;; Whether display icons in the mode-line. Respects `all-the-icons-color-icons'.
  ;; While using the server mode in GUI, should set the value explicitly.
  ;(setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-icon t)

  ;; The limit of the window width.
  ;; If `window-width' is smaller than the limit, some information won't be displayed.
  (setq doom-modeline-window-width-limit fill-column)

  ;; How to detect the project root.
  ;; The default priority of detection is `ffip' > `projectile' > `project'.
  ;; nil means to use `default-directory'.
  ;; The project management packages have some issues on detecting project root.
  ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
  ;; to hanle sub-projects.
  ;; You can specify one if you encounter the issue.
  (setq doom-modeline-project-detection 'project)

  (setq doom-modeline-buffer-file-name-style 'auto)


  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)

  (setq doom-modeline-unicode-fallback nil)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-enable-word-count nil)

  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

  (setq doom-modeline-buffer-encoding t)

  (setq doom-modeline-indent-info nil)

  (setq doom-modeline-checker-simple-format t)

  (setq doom-modeline-number-limit 99)

  ;; The maximum displayed length of the branch name of version control.
  (setq doom-modeline-vcs-max-length 12)

  ;; Whether display the perspective name. Non-nil to display in the mode-line.
  (setq doom-modeline-persp-name t)

  ;; If non nil the default perspective name is displayed in the mode-line.
  (setq doom-modeline-display-default-persp-name nil)

  ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
  (setq doom-modeline-lsp t)

  ;; Whether display the GitHub notifications. It requires `ghub' package.
  (setq doom-modeline-github nil)

  ;; The interval of checking GitHub.
  (setq doom-modeline-github-interval (* 30 60))

  ;; Whether display the modal state icon.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (setq doom-modeline-modal-icon t)

  ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  ;(setq doom-modeline-mu4e nil)

  ;; Whether display the gnus notifications.
  (setq doom-modeline-gnus t)

  ;; Wheter gnus should automatically be updated and how often (set to nil to disable)
  (setq doom-modeline-gnus-timer 2)

  ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
  ;(setq doom-modeline-irc t)

  ;; Function to stylize the irc buffer names.
  ;(setq doom-modeline-irc-stylize 'identity)

  ;; Whether display the environment version.
  (setq doom-modeline-env-version t)
  ;; Or for individual languages
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-env-enable-ruby t)
  (setq doom-modeline-env-enable-perl t)
  (setq doom-modeline-env-enable-go t)
  (setq doom-modeline-env-enable-elixir t)
  (setq doom-modeline-env-enable-rust t)

  ;; Change the executables to use for the language version string
  (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
  (setq doom-modeline-env-ruby-executable "ruby")
  (setq doom-modeline-env-perl-executable "perl")
  (setq doom-modeline-env-go-executable "go")
  (setq doom-modeline-env-elixir-executable "iex")
  (setq doom-modeline-env-rust-executable "rustc")

  ;; What to dispaly as the version while a new one is being loaded
  (setq doom-modeline-env-load-string "...")

  ;; Hooks that run before/after the modeline version string is updated
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil)
)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  ;(load-theme 'doom-one t)
  (load-theme 'doom-solarized-dark t)

  ;(load-theme 'doom-nord t)
  ;(load-theme 'doom-tomorrow-night t)
  ;(set-face-attribute 'hl-line nil :inherit nil :background "brightblack")

  ;(global-hl-line-mode t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (doom-themes-treemacs-config)
  ;(doom-themes-neotree-config)
)

;(use-package col-highlight
;  :ensure t
;)

(use-package highlight-indent-guides
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'column)
  (setq highlight-indent-guides-auto-odd-face-perc 10)
  (setq highlight-indent-guides-auto-even-face-perc 10)
  ;(setq highlight-indent-guides-auto-character-face-perc 20)
  (setq highlight-indent-guides-auto-enabled t)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-delay 0)
)

;(use-package neotree
;  :ensure t
;  :config
;  (global-set-key [f7] 'neotree-toggle)
;  (setq neo-window-fixed-size nil)
;)


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
  ;:bind
  ;(:map global-map
  ;      ("M-0"       . treemacs-select-window)
  ;      ("C-x t 1"   . treemacs-delete-other-windows)
  ;      ("C-x t t"   . treemacs)
  ;      ("C-x t B"   . treemacs-bookmark)
  ;      ("C-x t C-t" . treemacs-find-file)
  ;      ("C-x t M-t" . treemacs-find-tag)
  ;      )
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  )

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp
  :after treemacs persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))


(use-package magit
  :ensure t)


(use-package git-gutter
  :ensure t)


;; -------- package for lsp --------

(use-package lsp-mode
  :ensure t
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l", "s-l" )
  :init (setq lsp-keymap-prefix "C-c C-l")
   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;(XXX-mode . lsp)
         ; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
        )
  :commands lsp
 )

;; optionally
(use-package company :ensure t) ; completion
(use-package company-lsp :ensure t :commands company-lsp)
;; if you are helm user
;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list
)

;; optionally if you want to use debugger
;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
             :ensure t
             :config
             (which-key-mode))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom (scroll-margin 0)
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-header t)
  ;(setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-max-width 150)
  (setq lsp-ui-doc-max-height 30)
  (setq lsp-ui-peek-enable t)
)


; -------- lauguage specific package --------
(use-package rustic
  :ensure t
  :defer t
 ;:init
 ;(add-hook ‘rustic-mode-hook
 ;‘(lambda ()
 ;  (racer-mode t)
 ;  (dumb-jump-mode t)
 ;  (highlight-symbol-mode t)
 ;  (rainbow-delimiters-mode t)
 ;  (smartparens-mode t)))
 ;:mode (“\\.rs$” . rustic-mode)
 ;:commands (rustic-mode)

 ;:config
 ;(use-package quickrun
 ;:defer t
 ;:ensure t)
 ;(use-package racer
 ;:defer t
 ;:ensure t)
)



;; org-mode config
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
