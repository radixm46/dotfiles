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

(leaf leaf
  :doc "configure leaf related packages"
  :config
  (leaf leaf-keywords
    :ensure t
    :doc "additional leaf.el keywords"
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    ;; (leaf hydra :ensure t)
    (leaf el-get :ensure t :disabled t
      :doc "Manage the external elisp bits and pieces you depend upon")
    ;;(leaf blackout :ensure t)
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init))

  (leaf leaf-convert :ensure t
    :doc "convert many format to leaf format")

  (leaf leaf-tree
    :doc "interactive side-bar feature"
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left)))

  ;; install feather.el
  (leaf feather :disabled t
    :el-get conao3/feather.el
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
    :custom
    (leaf-alias-keyword-alist . '((:ensure . :straight)))
    (straight-vc-git-default-clone-depth . 5)
    (straight-disable-byte-compile . nil)
    (straight-disable-native-compile . t))

  ;; optional packages for leaf keywords (under straight.el)
  (leaf hydra :ensure t)

  (leaf blackout :disabled t
    :ensure t)
  )

(leaf paradox
  :doc "modernized emacs package menu"
  :ensure t
  :config (paradox-enable))

