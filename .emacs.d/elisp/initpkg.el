;;; initpkg.el --- init package env -*- no-byte-compile: t; lexical-binding: t -*-
;;; Commentary:
;;
;; package manager configuration.
;;

;;; Code:
;; init package

;; use custom.el for package-selected-packages
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; load custom.el if exists
;; (when (file-exists-p custom-file) (load custom-file))

;; setup straight.el
;; NOTE: straight.el configures straight-disable-native-compile with straight--native-comp-available
(prog1 '*straight-setup
  (prog1 '*straight-patch-alias-resolve
    ;; workaround for emacs28
    ;; https://github.com/raxod502/straight.el/issues/701
  (when (version<= "28" emacs-version)
    (advice-add 'straight--build-autoloads :around
                #'(lambda (oldfun &rest r)
                    (let ((find-file-visit-truename nil))
                      (apply oldfun r))))))

  (customize-set-variable 'straight-vc-git-default-clone-depth 1 "customized at initpkg")
  (defvar bootstrap-version)
  (let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 5))
    (unless
        (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char
         (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(prog1 '*patch-straight-use-repos
  (customize-set-variable 'straight-recipe-repositories
                          '(org-elpa melpa gnu-elpa-mirror nongnu-elpa emacsmirror-mirror)
                          "customiized at initpkg"))

;; setup leaf via straight.el
(prog1 '*leaf-with-straight-setup
  (straight-use-package 'leaf)
  (straight-use-package 'leaf-keywords)
  (leaf-keywords-init)
  (customize-set-variable 'leaf-alias-keyword-alist
                          '((:ensure . :straight))
                          "customized at initpkg"))


(leaf *leaf-configure
  :doc "configure leaf related packages"
  :config
  (leaf leaf-convert :ensure t
    :doc "convert many format to leaf format")

  (leaf leaf-tree
    :doc "interactive side-bar feature"
    :ensure t
    :custom
    ((imenu-list-size     . 30)
     (imenu-list-position . 'left)))

  ;; install feather.el
  (leaf feather :disabled t
    :el-get conao3/feather.el
    :custom (leaf-alias-keyword-alist . '((:ensure . :feather)))
    :config
    (feather-mode))
  )

;; optional packages for leaf keywords (under straight.el)
(leaf hydra :ensure t)
(leaf blackout :disabled t
  :ensure t)

(leaf paradox :if package-enable-at-startup
  :doc "modernized emacs package menu"
  :ensure t
  :config (paradox-enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; initpkg.el ends here
