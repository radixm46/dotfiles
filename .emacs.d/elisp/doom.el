;; enable doom-modeline
(leaf doom-modeline
  :ensure t
  :custom
  ;; How tall the mode-line should be. It's only respected in GUI.
  ;; If the actual char height is larger, it respects the actual height.
  (doom-modeline-height . 25)

  ;; How wide the mode-line bar should be. It's only respected in GUI.
  (doom-modeline-bar-width . 3)

  ;; Whether display icons in the mode-line. Respects `all-the-icons-color-icons'.
  ;; While using the server mode in GUI, should set the value explicitly.
  ;(setq doom-modeline-icon (display-graphic-p))
  (doom-modeline-icon . t)

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
  (doom-modeline-window-width-limit . fill-column)

  ;; How to detect the project root.
  ;; The default priority of detection is `ffip' > `projectile' > `project'.
  ;; nil means to use `default-directory'.
  ;; The project management packages have some issues on detecting project root.
  ;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
  ;; to hanle sub-projects.
  ;; You can specify one if you encounter the issue.
  (doom-modeline-project-detection . 'projectile)

  (doom-modeline-buffer-file-name-style . 'auto)

  (doom-modeline-major-mode-icon . t)
  (doom-modeline-major-mode-color-icon . t)
  (doom-modeline-buffer-state-icon . t)
  (doom-modeline-buffer-modification-icon . t)

  (doom-modeline-unicode-fallback . t)
  (doom-modeline-minor-modes . nil)

  (doom-modeline-enable-word-count . nil)
  (doom-modeline-continuous-word-count-modes . '(markdown-mode gfm-mode org-mode))

  (doom-modeline-buffer-encoding . t)

  (doom-modeline-indent-info . nil)

  (doom-modeline-checker-simple-format . t)

  (doom-modeline-number-limit . 99)

  ;; The maximum displayed length of the branch name of version control.
  (doom-modeline-vcs-max-length . 12)

  ;; Whether display the perspective name. Non-nil to display in the mode-line.
  (doom-modeline-persp-name . t)

  ;; If non nil the default perspective name is displayed in the mode-line.
  (doom-modeline-display-default-persp-name . nil)

  ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
  (doom-modeline-lsp . t)

  ;; Whether display the GitHub notifications. It requires `ghub' package.
  (doom-modeline-github . nil)

  ;; The interval of checking GitHub.
  (doom-modeline-github-interval . '(* 30 60))

  ;; Whether display the modal state icon.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (doom-modeline-modal-icon . t)

  ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  (doom-modeline-mu4e . nil)

  ;; Whether display the gnus notifications.
  (doom-modeline-gnus . t)

  ;; Wheter gnus should automatically be updated and how often (set to nil to disable)
  (doom-modeline-gnus-timer . 2)

  ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
  (doom-modeline-irc . nil)
  ;; Function to stylize the irc buffer names.
  (doom-modeline-irc-stylize . 'identity)

  ;; Whether display the environment version.
  (doom-modeline-env-version . t)
  ;; Or for individual languages
  (doom-modeline-env-enable-python . t)
  (doom-modeline-env-enable-ruby . t)
  (doom-modeline-env-enable-perl . t)
  (doom-modeline-env-enable-go . t)
  (doom-modeline-env-enable-elixir . t)
  (doom-modeline-env-enable-rust . t)

  ;; Change the executables to use for the language version string
  (doom-modeline-env-python-executable . "python") ; or `python-shell-interpreter'
  (doom-modeline-env-ruby-executable . "ruby")
  (doom-modeline-env-perl-executable . "perl")
  (doom-modeline-env-go-executable . "go")
  (doom-modeline-env-elixir-executable . "iex")
  (doom-modeline-env-rust-executable . "rustc")

  ;; What to dispaly as the version while a new one is being loaded
  (doom-modeline-env-load-string . "...")

  ;; Hooks that run before/after the modeline version string is updated
  (doom-modeline-before-update-env-hook . nil)
  (doom-modeline-after-update-env-hook . nil)

  :config
  (doom-modeline-mode 1)
)

(leaf doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold . t)
  (doom-themes-enable-italic . t)

  :config
  (defun doom-on-term ()
    "switch doom-theme without doom-modeline-icon disabled"
    (interactive)
    (load-theme 'doom-spacegrey t)
    (custom-set-variables '(doom-modeline-icon nil)))
  (defun doom-on-gui ()
    "switch doom-theme with doom-modeline-icon enabled"
    (interactive)
    (load-theme 'doom-molokai t)
    (custom-set-variables '(doom-modeline-icon t)))

  (if (emacs-works-on-term-p)
    (doom-on-term) (doom-on-gui))

  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (doom-themes-treemacs-config)
  (doom-themes-neotree-config)
)
