;; -------- packages for lsp --------

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-document-sync-method nil) ;; always send incremental document
  (lsp-response-timeout 5)
  (lsp-prefer-flymake t) ;'flymake
  (lsp-eldoc-enable-hover nil)
  :commands (lsp lsp-deferred)
  :bind
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l", "s-l" )
  ; :init (setq lsp-keymap-prefix "C-c C-l")
  (:map lsp-mode-map
        ("C-c r"   . lsp-rename))
  :config
  ;; LSP UI tools
  (use-package lsp-ui
    :ensure t
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable t)
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature t)
    (lsp-ui-doc-position 'top) ;; top, bottom, or at-point
    (lsp-ui-doc-max-width 150)
    (lsp-ui-doc-max-height 30)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit nil)
    ;; lsp-ui-flycheck
    (lsp-ui-flycheck-enable nil)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable t)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-diagnostics t)
    (lsp-ui-sideline-show-code-actions t)
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable nil)
    (lsp-ui-imenu-kind-position 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-peek-height 20)
    (lsp-ui-peek-list-width 50)
    (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
    :preface
    (defun ladicle/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
          (progn
            (lsp-ui-doc-mode -1)
            (lsp-ui-doc--hide-frame))
        (lsp-ui-doc-mode 1)))
    :bind
    (:map lsp-mode-map
          ("C-c C-r" . lsp-ui-peek-find-references)
          ("C-c C-j" . lsp-ui-peek-find-definitions)
          ("C-c i"   . lsp-ui-peek-find-implementation)
          ("C-c m"   . lsp-ui-imenu)
          ("C-c s"   . lsp-ui-sideline-mode)
          ("C-c d"   . ladicle/toggle-lsp-ui-doc)
          ("C-c D"   . lsp-ui-doc-focus-frame)
     :map lsp-ui-doc-frame-mode-map
          ("C-c q"   . lsp-ui-doc-unfocus-frame))
    :hook (lsp-mode . lsp-ui-mode))

  (use-package lsp-treemacs
    :ensure t
    ;:after (treemacs lsp-mode)
    :hook (lsp . lsp-treemacs)
    :bind
    (:map global-map
          ("C-x t e" . lsp-treemacs-errors-list)
          ("C-x t s" . lsp-treemacs-symbols))
    :config
    (lsp-treemacs-sync-mode 1))

  (use-package lsp-origami
    :ensure t
    :hook (lsp-after-open . lsp-origami-try-enable))

  (require 'lsp-ido)
 )


;; optionally
;; if you are helm user
;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; optionally if you want to use debugger
;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
