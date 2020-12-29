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

  ;(defun doom-frame-init-func (&optional frame)
  ;  "frame init on doom-modeline"
  ;  ;(if window-system (progn
  ;  ;))
  ;  (if (not window-system) (progn
  ;    (setq doom-modeline-icon nil)
  ;  ))
  ;)

  ;(add-hook 'after-make-frame-functions 'doom-frame-init-func)

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

  (defun doom-on-term ()
    (interactive)
    (load-theme 'doom-spacegrey t)
    (setq doom-modeline-icon nil)
    )
  (defun doom-on-gui ()
    (interactive)
    (load-theme 'doom-spacegrey t)
    (setq doom-modeline-icon t)
    (doom-themes-treemacs-config)
    )

  (if (display-graphic-p)
      (doom-on-gui)
      (doom-on-term)
    ;(load-theme 'doom-gruvbox t)
    ;(load-theme 'doom-solarized-dark t)
    ;(load-theme 'doom-spacegrey t)
    ;(load-theme 'doom-tomorrow-night t)
    ;(load-theme 'doom-molokai t)
    ;(load-theme 'doom-dark+ t)
    ;(load-theme 'doom-dracula t)
    ;(load-theme 'doom-monokai-pro t)
    ;(load-theme 'doom-material t)
    ;(load-theme 'doom-nord t)
    ;(load-theme 'wheatgrass t)
    ;(load-theme 'doom-one t)
    ) ; loads different theme on terminal(temporary)

  ;(set-face-attribute 'hl-line nil :inherit nil :background "brightblack")

  ;(global-hl-line-mode t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  ;(doom-themes-treemacs-config)
  (doom-themes-neotree-config)
)
