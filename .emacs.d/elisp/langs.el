;;; langs.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;
;; major modes loaded by init.el
;;

;;; Code:

(leaf text-mode
  :tag "builtin"
  :init
  (leaf *font-remap-text-mode
    :hook (text-mode-hook . remap-font-to-doc)
    :preface (push #'text-mode remap-font-to-doc-modes-list)))

(leaf emacs-lisp-mode
  :tag "builtin"
  :mode-hook
  (emacs-lisp-mode-hook . ((hs-minor-mode +1)
                           (flycheck-mode +1)
                           (eldoc-mode +1)
                           (highlight-symbol-mode +1)
                           (smartparens-strict-mode +1)
                           (paredit-mode +1)))
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
    :custom (highlight-defined-face-use-itself . t))

  :config
  ;; add original C-j behavior to M-RET
  (evil-define-key '(normal insert) lisp-interaction-mode-map
    (kbd "M-RET") 'eval-print-last-sexp))

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
  :setq (rust-ts-mode-hook . rust-mode-hook)
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
    :hook (python-mode-hook . lsp-deferred)
    :setq (python-ts-mode-hook . python-mode-hook))

  (leaf hy-mode
    :ensure t
    :mode ("\\.hy\\'" . hy-mode)
    :mode-hook
    (hy-mode-hook . ((hs-minor-mode +1)
                     (smartparens-strict-mode +1)
                     (paredit-mode +1)
                     (eglot-ensure +1)))
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
    :setq (js-ts-mode-hook . js-mode-hook)
    :init
    (leaf js2-mode
      :ensure t
      :doc "use js2-mode as minor mode with js-mode on .js file"
      ;; :hook (js-mode-hook . js2-minor-mode)
      :mode ("\\.js\\'" . (lambda ()
                            (js-mode)
                            (js2-minor-mode)))
      :custom (js2-basic-offset . 2))

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
  :setq (typescript-ts-mode-hook . typescript-mode-hook)
  :custom
  (typescript-indent-level . 2)
  :config
  (leaf tide :emacs< "27"
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
  :setq (bash-ts-mode-hook . sh-mode-hook)
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
  :hook (yaml-mode-hook . hl-todo-mode)
  :setq (yaml-ts-mode-hook . yaml-mode-hook))

(leaf systemd :ensure t)

(leaf lua-mode :ensure t)

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

  (leaf docker-tramp :emacs< "29"
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
    :setq (java-ts-mode-hook . java-mode-hook)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; langs.el ends here
