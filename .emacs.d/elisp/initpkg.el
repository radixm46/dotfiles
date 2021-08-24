;; init package
(require 'package)

;; use custom.el for package-selected-packages
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(setq package-archives (append '(("melpa" . "https://melpa.org/packages/")
                                 ("melpa-stable" . "https://stable.melpa.org/packages/")
                                 ("org" . "http://orgmode.org/elpa/")) package-archives))
(setq load-prefer-newer t)

;; auto load use-package package
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (require 'use-package)

;; auto load leaf.el package
(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))
(require 'leaf)

;; configure leaf related packages
(leaf leaf
  :config
  (leaf leaf-convert :ensure t)

  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left)))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    ;;(leaf blackout :ensure t)
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init))
  ;; install feather.el
  (leaf feather :disabled t
    :el-get conao3/feather.el
    :init
    :custom (leaf-alias-keyword-alist . '((:ensure . :feather)))
    :config
    (feather-mode))

  (leaf straight-setup :if (executable-find "git")
    :doc "setup straight.el (require git)"
    :config
    (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 5))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

    ;; use straight as leaf default package management
    :custom (leaf-alias-keyword-alist . '((:ensure . :straight))))
  )

(leaf paradox
  :doc "modernized emacs package menu"
  :ensure t
  :config (paradox-enable))

