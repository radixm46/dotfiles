;; major modes loaded by init.el
(leaf file-manage
  :doc "file management related config"
  :config
  (leaf dired
    :tag "builtin"
    :config (put 'dired-find-alternate-file 'disabled nil))

  (leaf neotree
    :ensure t
    :bind ([f7] . neotree-toggle)
    :custom (neo-window-fixed-size . nil)
    :config
    ;; keymap for using with evil mode
    (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
    (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
    (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
    (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle))

  (leaf treemacs
    :ensure t
    ;; :defer t
    ;; :require treemacs
    :init
    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
    :bind
    (("M-0"       . treemacs-select-window)
     ("C-x t 1"   . treemacs-delete-other-windows)
     ("C-x t t"   . treemacs)
     ("C-x t B"   . treemacs-bookmark)
     ("C-x t C-t" . treemacs-find-file)
     ("C-x t M-t" . treemacs-find-tag))
    :config
    (leaf treemacs-custom :after treemacs
      :doc "eval after treemacs load"
      :custom
      (treemacs-deferred-git-apply-delay      . 0.5)
      (treemacs-directory-name-transformer    . #'identity)
      (treemacs-display-in-side-window        . t)
      (treemacs-eldoc-display                 . t)
      (treemacs-file-event-delay              . 5000)
      (treemacs-file-extension-regex          . treemacs-last-period-regex-value)
      (treemacs-file-follow-delay             . 0.2)
      (treemacs-file-name-transformer         . #'identity)
      (treemacs-follow-after-init             . t)
      (treemacs-git-command-pipe              . "")
      (treemacs-goto-tag-strategy             . 'refetch-index)
      (treemacs-indentation                   . 2)
      (treemacs-indentation-string            . " ")
      (treemacs-is-never-other-window         . nil)
      (treemacs-max-git-entries               . 5000)
      (treemacs-missing-project-action        . 'ask)
      (treemacs-move-forward-on-expand        . nil)
      (treemacs-no-png-images                 . nil)
      (treemacs-no-delete-other-windows       . t)
      (treemacs-project-follow-cleanup        . nil)
      (treemacs-position                      . 'left)
      (treemacs-recenter-distance             . 0.1)
      (treemacs-recenter-after-file-follow    . nil)
      (treemacs-recenter-after-tag-follow     . nil)
      (treemacs-recenter-after-project-jump   . 'always)
      (treemacs-recenter-after-project-expand . 'on-distance)
      (treemacs-show-cursor                   . nil)
      (treemacs-show-hidden-files             . t)
      (treemacs-silent-filewatch              . nil)
      (treemacs-silent-refresh                . nil)
      (treemacs-sorting                       . 'alphabetic-asc)
      (treemacs-space-between-root-nodes      . t)
      (treemacs-tag-follow-cleanup            . t)
      (treemacs-tag-follow-delay              . 1.5)
      (treemacs-user-mode-line-format         . nil)
      (treemacs-user-header-line-format       . nil)
      (treemacs-width                         . 35)
      :config ;; use expression to set variables
      (custom-set-variables
       '(treemacs-collapse-dirs                 (if treemacs-python-executable 3 0))
       '(treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory))))
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode 0)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t) (treemacs-git-mode 'deferred))
      (`(t . _) (treemacs-git-mode 'simple)))

    ;; treemacs integrations
    (leaf treemacs-evil
      :ensure t
      :after (treemacs evil)
      :require treemacs-evil
      ;; fix tab action
      :bind (:evil-treemacs-state-map ("TAB" . treemacs-TAB-action)))
    (leaf treemacs-magit
      :ensure t
      :after (treemacs magit))
    (leaf treemacs-projectile
      :after treemacs projectile
      :ensure t)
    (leaf treemacs-all-the-icons
      :after treemacs all-the-icons
      :ensure t :require t
      :config (treemacs-load-theme "all-the-icons"))
    (leaf treemacs-icons-dired
      :doc "treemacs icons on dired (treemacs-all-the-icons, use all-the-icons)"
      :after treemacs dired
      :ensure t
      :config (treemacs-icons-dired-mode))
    ;;(use-package treemacs-persp ;;treemacs-persective if you use perspective.el vs. persp-mode
    ;;  :after treemacs persp-mode ;;or perspective vs. persp-mode
    ;;  :ensure t
    ;;  :config (treemacs-set-scope-type 'Perspectives))
    )
  )

(leaf magit
  :ensure t
  :custom (magit-completing-read-function . 'magit-ido-completing-read))

(leaf undo-tree
  :ensure t
  :custom (undo-tree-visualizer-timestamps . t)
  :global-minor-mode (global-undo-tree-mode))

(leaf vterm
  :ensure t
  :init
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)
  (add-hook 'vterm-mode-hook (lambda () (sw-lnsp 1)))
  :bind (:vterm-mode-map ("C-u" . vterm-send-C-u)))
