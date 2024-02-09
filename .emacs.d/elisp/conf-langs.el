;;; conf-langs.el --- major modes -*- lexical-binding: t -*-
;;; Commentary:
;;
;; major modes loaded by init.el
;;

;;; Code:
(eval-when-compile
  (load (expand-file-name "elisp/initpkg"   user-emacs-directory))
  (!el-load "elisp/util"
            "elisp/conf-evil"))


(leaf text-mode
  :tag "builtin"
  :commands text-mode
  :init
  (leaf *font-remap-text-mode
    :defvar remap-font-to-doc-modes-list
    :hook (text-mode-hook . remap-font-to-doc)
    :preface (push #'text-mode remap-font-to-doc-modes-list)))

(leaf emacs-lisp-mode
  :tag "builtin"
  :commands emacs-lisp-mode
  :defun
  highlight-symbol-mode
  smartparens-strict-mode
  paredit-mode
  :mode-hook
  (emacs-lisp-mode-hook . (;; (flycheck-mode +1)
                           (hs-minor-mode           +1)
                           (flymake-mode            +1)
                           (eldoc-mode              +1)
                           (highlight-symbol-mode   +1)
                           (smartparens-strict-mode +1)
                           (paredit-mode            +1)
                           ;; check parens before save
                           (add-hook 'before-save-hook
                                     'check-parens nil 'local)))
  :init
  (leaf highlight-defined
    :ensure t
    :defun straight-use-package
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
        after-load-theme-hook) . patch-highlight-defined-faces))

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
  :commands conf-mode
  :hook (conf-mode-hook . hl-todo-mode)
  :mode ("\\.env\\'"    . conf-mode))

;; required for racer
(leaf rust-mode
  :ensure t
  :commands rust-mode
  :hook (rust-mode-hook . lsp)
  :custom (rust-format-on-save . t)
  :setq (rust-ts-mode-hook . rust-mode-hook)
  :config
  (leaf cargo
    :ensure t
    :hook (rust-mode-hook . cargo-minor-mode)))

(leaf *python-config
  :doc "python related configuration"
  :config
  (leaf poetry
    :ensure t
    :hook (python-mode-hook . poetry-tracking-mode)
    :custom (poetry-tracking-strategy . 'projectile))

  (leaf pipenv
    :ensure t
    :defvar
    (pipenv-mode
     python-shell-virtualenv-root)
    :defun
    (pipenv-activate
     pipenv-projectile-after-switch-extended)
    :custom
    (pipenv-projectile-after-switch-function . #'pipenv-projectile-after-switch-extended)
    ;; :hook (python-mode . python-pipenv-init))
    :init
    (defun python-pipenv-init ()
      "init python environment on python-mode"
      (when (not pipenv-mode)
        (pipenv-mode))
      (when (null python-shell-virtualenv-root)
        (pipenv-activate))))

  (leaf python-mode
    :tag "builtin" :ensure t
    :commands python-mode
    :hook (python-mode-hook . lsp-deferred)
    :setq (python-ts-mode-hook . python-mode-hook))

  (leaf hy-mode
    :ensure t
    :commands hy-mode
    :mode ("\\.hy\\'" . hy-mode)
    :mode-hook
    (hy-mode-hook . ((hs-minor-mode           +1)
                     (smartparens-strict-mode +1)
                     (paredit-mode            +1)
                     (eglot-ensure            +1)))
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
  :commands go-mode
  :hook (go-mode-hook . lsp)
  :defun lsp-format-buffer lsp-organize-imports
  :defvar go-ts-mode-hook go-mode-hook
  :setq (go-ts-mode-hook . go-mode-hook)
  :mode ("\\.go\\'" . go-mode)
  )

(leaf web-mode
  :ensure t
  :commands web-mode
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
    :if (!executable-find "tidy")
    :doc "enable flycheck with html-tidy on web-mode"
    ;; :config (add-hook 'lsp-after-initialize-hook
    ;;                   (lambda ()
    ;;                     (flycheck-add-mode 'html-tidy 'web-mode)
    ;;                     (flycheck-add-next-checker 'lsp 'html-tidy)))
    )
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
  :commands html-mode
  :tag "builtin")

(leaf *javascript-conig
  :doc "config javascript environment"
  :config
  (leaf js-mode :emacs>= "27"
    :tag "builtin"
    :doc "use js-mode with lsp"
    :mode ("\\.json\\'" . js-mode)
    :commands js-mode
    :custom (js-indent-level . 2)
    :hook (js-mode-hook . lsp)
    :defvar js-ts-mode-hook  js-mode-hook
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

    (leaf flycheck-use-eslint :if (!executable-find "eslint") :disabled t
      :hook
      (lsp-after-initialize-hook . (lambda ()
                                     (flycheck-add-mode 'javascript-eslint 'js-mode)
                                     (flycheck-add-next-checker 'lsp 'javascript-eslint))))
    )

  (leaf js2-mode :emacs< "27"
    :ensure t
    :commands js2-mode
    :doc "use js2-mode as major mode with lsp"
    :mode ("\\.js\\'" . js2-mode)
    :hook (js2-mode-hook . lsp)
    :custom (js2-basic-offset . 2))
  )

(leaf typescript-mode
  :ensure t
  :commands typescript-mode
  :hook (typescript-mode-hook . lsp)
  :defvar typescript-ts-mode-hook typescript-mode-hook
  :setq (typescript-ts-mode-hook . typescript-mode-hook)
  :custom
  (typescript-indent-level . 2)
  :config
  (leaf tide :emacs< "27"
    :ensure t
    :hook
    ;; (before-save-hook  . tide-format-before-save)
    (typescript-mode-hook . tide-setup)
    :defun tide-hl-identifier-mode
    :config
    (tide-hl-identifier-mode +1))
  )

(leaf sh-mode
  :tag "builtin"
  :commands sh-mode
  :hook
  ;; (sh-mode-hook . lsp)
  ;; (sh-mode-hook . flycheck-mode)
  :defvar bash-ts-mode-hook sh-mode-hook
  :setq (bash-ts-mode-hook . sh-mode-hook)
  ;; :config (setq flycheck-checker 'sh-shellcheck)
  :init
  (leaf shfmt :when (!executable-find "shfmt")
    :doc "format code on save"
    :ensure t
    :commands shfmt-on-save-mode
    :custom (shfmt-arguments . '("--indent" "4"
                                 "--binary-next-line"
                                 "--case-indent"
                                 "--keep-padding"
                                 "--space-redirects"))
    :mode-hook (sh-mode-hook . ((shfmt-on-save-mode +1))))
  (leaf *flycheck-by-shellcheck :if (!executable-find "shellcheck")
    :disabled t
    :doc "use shellcheck for flychecker with lsp"
    :hook (lsp-after-initialize-hook . (lambda () (flycheck-add-next-checker 'lsp 'sh-shellcheck))))
  )

(leaf csv-mode
  :ensure t
  :commands csv-mode)

(leaf json-mode :emacs< "29"
  :ensure t
  :commands json-mode
  :mode ("\\.json\\'" . json-mode))

(leaf yaml-mode
  :ensure t
  :commands yaml-mode
  :hook (yaml-mode-hook . hl-todo-mode)
  :defvar yaml-ts-mode-hook yaml-mode-hook
  :setq (yaml-ts-mode-hook . yaml-mode-hook))

(leaf systemd
  :ensure t
  :commands systemd-mode)

(leaf lua-mode :ensure t
  :commands lua-mode)

(leaf *docker-env
  :doc "docker related modes"
  :config
  (leaf docker
    :ensure t
    :doc "emacs integration for Docker"
    :url "https://github.com/Silex/docker.el"
    :commands docker-image-mode)

  (leaf dockerfile-mode
    :ensure t
    :commands dockerfile-mode
    :doc "Docker file mode for emacs"
    :url "https://github.com/spotify/dockerfile-mode")

  (leaf docker-compose-mode
    :ensure t
    :commands docker-compose-mode
    :doc "major mode for editing docker-compose files, supports context-aware completion of docker-compose keys"
    :url "https://github.com/meqif/docker-compose-mode")

  (leaf docker-tramp :emacs< "29"
    :ensure t
    :doc "TRAMP method for Docker containers"
    :url "https://github.com/emacs-pe/docker-tramp.el"
    :custom (docker-tramp-use-names . t))
  )

(leaf *lml-modes
  :doc "lightweight markup languages"
  :config
  (leaf markdown-mode
    :ensure t
    ;; :hook (markdown-mode . visual-line-mode)
    :commands (markdown-mode gfm-mode)
    :mode
    ("README\\.md\\'"  . gfm-mode)
    :custom
    (markdown-command                    . "multimarkdown")
    (markdown-hide-markup                . t)
    (markdown-header-scaling             . t)
    (markdown-header-scaling-values      . '(1.75 1.5 1.25 1.1 1.0 1.0))
    (markdown-enable-highlighting-syntax . t)
    (markdown-enable-math                . t)
    :init
    (leaf *font-remap-markdown-mode
      :hook
      (markdown-mode-hook . remap-font-to-doc)
      (gfm-mode-hook      . remap-font-to-doc)
      :preface
      (push #'markdown-mode  remap-font-to-doc-modes-list)
      (push #'gfm-mode       remap-font-to-doc-modes-list))
    :config
    (evil-define-key 'normal markdown-mode-map
      "j" 'evil-next-visual-line
      "k" 'evil-previous-visual-line
      "gj" 'evil-next-line
      "gk" 'evil-previous-line)
    ;; modify table face to 1:2
    (set-face-attribute 'markdown-table-face nil :family font-for-tables))

  (leaf valign
    :doc "CJK supported table vertical alignment (on graphical environment)"
    :ensure t
    :commands valign-mode
    :custom
    (valign-fancy-bar . t)
    :preface
    (defun valign-buffer-on-gui ()
      (mapc #'(lambda (buffer)
                (with-current-buffer buffer
                  (when (or (eq major-mode #'org-mode)
                            (eq major-mode #'markdown-mode))
                    (valign-mode +1))))
            (buffer-list)))
    (defun valign-remove-buffer-on-term ()
      (mapc #'(lambda (buffer)
                (with-current-buffer buffer
                  (when (or (eq major-mode #'org-mode)
                            (eq major-mode #'markdown-mode))
                    (valign-mode -1))))
            (buffer-list)))
    :hook
    ((org-mode-hook
      markdown-mode-hook) . (lambda () (when (display-graphic-p) (valign-mode +1))))
    (conf-on-gui-hook     . valign-buffer-on-gui)
    (conf-on-term-hook    . valign-remove-buffer-on-term))
  )

(leaf powershell
  :ensure t
  :commands powershell-mode)

(leaf java-mode
  :tag "builtin"
  :doc "java environment config"
  :commands java-mode
  :config
  (leaf lsp-java :disabled t
    :ensure t
    :hook (java-mode-hook . lsp)
    :setq (java-ts-mode-hook . java-mode-hook)
    ;; :custom
    ;; (lsp-java-format-settings-url . "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")
    )
  )

(leaf elm-mode
  :ensure t
  :commands elm-mode
  :mode ("\\.elm\\'" . elm-mode)
  :custom
  (elm-indent-simple-offset . 2)
  (elm-indent-offset . 2)
  :hook (elm-mode-hook . lsp))

(leaf haskell-mode
  :ensure t
  :commands haskell-mode
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

(leaf *mermaid-setup :when (!executable-find "mmdc")
  :config
  (leaf mermaid-mode
    :doc "configure mermaid-mode"
    :ensure t
    :commands mermaid-mode
    :defun tree-sitter-hl-mode
    :mode-hook (mermaid-mode-hook . ((tree-sitter-hl-mode -1)))
    :custom
    (mermaid-output-format . "png")
    (mermaid-flags         . "--backgroundColor transparent --theme forest"))

  (leaf ob-mermaid
    :doc "configure mermaid for org babel"
    :ensure t
    :custom `(ob-mermaid-cli-path . ,(!executable-find "mmdc"))
    :commands org-babel-execute:mermaid)
  ;; configure mermaid for babel (ob-mermaid)
  :preface
  (eval-after-load
      'org-mode
    '(progn (add-to-list 'org-babel-load-languages '(mermaid . t))))
  :defvar org-babel-load-languages)

(leaf gnuplot :if (!executable-find "gnuplot")
  :ensure t
  :commands gnuplot-mode
  :mode
  (("\\.gp\\'"
    "\\.gnuplot\\'") . gnuplot-mode)
  :config (add-to-list 'org-babel-load-languages '(gnuplot . t)))

(leaf git-modes
  :ensure t
  :commands gitignore-mode gitconfig-mode gitattributes-mode)

(leaf fsharp-mode
  :ensure t
  :commands fsharp-mode
  :defun exec-path-from-shell-copy-env
  :preface (exec-path-from-shell-copy-env "DOTNET_ROOT") ; import dotnet runtime path
  :mode-hook (fsharp-mode-hook . ((rainbow-delimiters-mode -1)))
  :config
  (leaf *eglot-fsharp
    :doc "load `eglot-fsharp.el' inside straight cloned repo"
    :defvar eglot--managed-mode
    :mode-hook
    (fsharp-mode-hook . ((unless eglot--managed-mode
                           (call-interactively 'eglot))))
    :load-path
    `(,(!expand-file-name
        "straight/repos/emacs-fsharp-mode"
        straight-base-dir))
    :require eglot-fsharp
    :custom `(eglot-fsharp-server-install-dir . ,(cache-sub-dir "FsAutoComplete"))
  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conf-langs.el ends here
