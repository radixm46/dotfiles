;; -------- package for lsp --------

(use-package lsp-mode
  :ensure t
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l", "s-l" )
  ; :init (setq lsp-keymap-prefix "C-c C-l")
  :custom
  (lsp-document-sync-method nil) ;; always send incremental document
  (lsp-response-timeout 5)
  (lsp-prefer-flymake nil) ;'flymake
  :hook (
      ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
      ;(XXX-mode . lsp)
      ; if you want which-key integration
      (lsp-mode . lsp-enable-which-key-integration)
     )
  :commands lsp
  :bind
  (:map lsp-mode-map
        ("C-c r"   . lsp-rename))
  :config
    (require 'lsp-clients)
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
      (lsp-ui-doc-use-webkit t)
      ;; lsp-ui-flycheck
      (lsp-ui-flycheck-enable t) ;nil
      ;; lsp-ui-sideline
      (lsp-ui-sideline-enable nil)
      (lsp-ui-sideline-ignore-duplicate t)
      (lsp-ui-sideline-show-symbol t)
      (lsp-ui-sideline-show-hover t)
      (lsp-ui-sideline-show-diagnostics t) ;nil
      (lsp-ui-sideline-show-code-actions t) ;nil
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
            ("C-c d"   . ladicle/toggle-lsp-ui-doc))
      :hook (lsp-mode . lsp-ui-mode))

  ;; Lsp completion
    (use-package company-lsp
      :ensure t
      :config
      (use-package company
        :ensure t
        :config
        (global-company-mode)
        (setq company-idle-delay 0)
        (setq company-selection-wrap-around t)
        (setq completion-ignore-case t)
        ) ; completion
      :custom
      (company-lsp-cache-candidates t) ;; always using cache
      (company-lsp-async t)
      (company-lsp-enable-recompletion nil)
      :commands company-lsp
    )
 )

;; optionally
;; if you are helm user
;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; optionally if you want to use debugger
;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
