;;; initpkg.el --- init package env -*- lexical-binding: t -*-
;;; Commentary:
;;
;; package manager configuration.
;;

;;; Code:

;; disable magick filename while init
(when (not noninteractive)
  (defconst rdm/saved-file-name-handler-alist file-name-handler-alist
    "stored `file-name-handler-alist' value while init")
  (setq file-name-handler-alist nil)
  (add-hook 'after-init-hook #'(lambda () (setq file-name-handler-alist
                                                rdm/saved-file-name-handler-alist))))

;; define macros for pkg manage
(prog1 '*pkg-manage-macros
  (defmacro with-suppressed-message (&rest body)
    "Suppress new messages temporarily in the echo area and
the `*Messages*' buffer while BODY is evaluated."
    (declare (indent 0))
    (let ((message-log-max nil))
      `(with-temp-message (or (current-message) "") ,@body)))

  (defmacro !executable-find (exec)
    "Macro retuns result of `(executable-find EXEC)'."
    (eval (executable-find exec)))

  (defmacro !system-type (system)
    "Macro retuns result of `(eq system-type 'SYSTEM)'."
    (eval `(eq system-type ,system)))

  (defmacro !expand-file-name (relative &optional basedir)
    "Macro retuns result of `(expand-file-name RELATIVE BASEDIR)'."
    (eval `(expand-file-name ,relative ,basedir)))

  (defmacro !el-load (&rest modules)
  "Load each module in the `modules' list, expanding relative paths
from the `user-emacs-directory'.
Each module name is expected to be a string."
    (let ((path (mapcar (lambda (e)
                          (expand-file-name e user-emacs-directory))
                        modules)))
       `(dolist (e ',path) (load e))))

  (defmacro !sys-featurep (feat)
    "Checks if the specified `FEAT' matches any entry in
`system-configuration-features' using `string-match-p'.
`FEAT' must be a string or a regular expression."
    (eval (string-match-p feat system-configuration-features)))
  )


;; define cache related functions
;; setup .config/cache/emacs
(prog1 '*cache-managing
  (defconst emacs-cache-root-dir
    (!expand-file-name "emacs"
                       (if (fboundp 'xdg-cache-home) (xdg-cache-home)
                         (expand-file-name ".cache" (getenv "HOME"))))
    "emacs cache directory path (default: $HOME/.cache)")

  (defsubst cache-sub-dir (&optional name)
    "Check directory existence, and return full path to given NAME.
Create if cache sub directory NAME not exists.
If argument is not given, returns `emacs-cache-root-dir'"
    (let ((lpath (if (null name)
                     ;; if cache dirname not given, use emacs-cache-root-dir
                     emacs-cache-root-dir
                   (expand-file-name name emacs-cache-root-dir))))
      (if (not (file-directory-p lpath))
          (progn (make-directory lpath t) lpath)
        lpath)))

  (defsubst cache-sub-file (name &optional subdir)
    "returns file path like `emacs-cache-root-dir/SUBDIR/NAME'
if name is not given, returns `emacs-cache-root-dir/NAME'
argument NAME could be directory or filename"
    (expand-file-name name (cache-sub-dir subdir)))
  )

;; use `custom-file' for package-selected-packages
(prog1 '*customize-file-handling
  (customize-set-variable
   'custom-file (!expand-file-name "elisp/local-custom.el" user-emacs-directory)
   "customized at initpkg")
  ;; load `custom-file' if exists and save before quit
  ;; (when (and (not noninteractive)
  ;;            (custom-file))
  ;;   (load custom-file)
  ;;   (add-hook 'kill-emacs-hook 'custom-save-all))
  )

;; setup straight.el
;; NOTE: straight.el configures straight-disable-native-compile with straight--native-comp-available
(eval-and-compile
  ;; add eln-cache dir to path (required on bytecomp)
  (when (and noninteractive
             (featurep 'native-compile))
    (let ((eln-dir (!expand-file-name ".cache/emacs/eln-cache" (getenv "HOME"))))
      (if (fboundp 'startup-redirect-eln-cache)
          (startup-redirect-eln-cache eln-dir) ;; defined above 29.1
        (push eln-dir native-comp-eln-load-path))))

  (prog1 '*straight-setup
    (customize-set-variable
     'straight-vc-git-default-clone-depth 1 "customized at initpkg")
    (customize-set-variable
     'straight-enable-use-package-integration nil "customized at initpkg")
    (customize-set-variable
     'straight-enable-package-integration nil "customized at initpkg")
    (defvar bootstrap-version)
    (let ((bootstrap-file
           (!expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 5))
      (unless
          (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

    (prog1 '*straight-patch-alias-resolve
      ;; workaround for emacs28
      ;; https://github.com/raxod502/straight.el/issues/701
      (when (and (version<= "28" emacs-version)
                 (version< emacs-version "29")) ; problem seems not to occur in emacs29
        (advice-add 'straight--build-autoloads :around
                    #'(lambda (oldfun &rest r)
                        (let ((find-file-visit-truename nil))
                          (apply oldfun r)))))))


  (prog1 '*patch-straight-use-repos
    (customize-set-variable 'straight-recipe-repositories
                            '(org-elpa melpa gnu-elpa-mirror nongnu-elpa emacsmirror-mirror)
                            "customized at initpkg"))

  (prog1 '*configure-straight-profiles
    (customize-set-variable 'straight-profiles
                            '((nil   . "default.el")
                              (local . "local.el"))
                            "customized at initpkg"))

  ;; setup leaf via straight.el
  (prog1 '*leaf-with-straight-setup
    (straight-use-package 'leaf)
    (straight-use-package 'leaf-keywords)
    (leaf-keywords-init)
    (customize-set-variable 'leaf-alias-keyword-alist
                            '((:ensure . :straight))
                            "customized at initpkg"))

  (prog1 '*leaf-with-straight-freeze
    ;; emacs to load all pkgs for freezing versions
    (defun rdm/freeze-vers-p ()
      (and noninteractive (getenv "FREEZE_EMACS")))

    (defun rdm/freeze-versions ()
      (when (rdm/freeze-vers-p)
        (message "*** Loading init.el done, freezing %s... ***"
                 straight-current-profile)
        (straight-check-all)
        (when (equal "2" (rdm/freeze-vers-p))
          (message "*** Updating pkgs. ***")
          (straight-pull-all))
        (straight-freeze-versions t)
        (message "*** Freezing %s done! ***" straight-current-profile)))

    (if (rdm/freeze-vers-p)
        (progn
          (message "*** Loading init in freeze versions mode. ***")

          (setq
           straight-disable-native-compile  t
           native-comp-deferred-compilation nil
           garbage-collection-messages      nil
           leaf-defaults                    '(:leaf-defer nil)
           leaf-alias-keyword-alist         '((:ensure . :straight)
                                              (:after  . :doc) ;; disable after keyword
                                              (:config . :init))))
      (when (and (null after-init-time) (not "0" (rdm/freeze-vers-p)))
        (message "*** straight.el thaw versions. ***")
        (straight-thaw-versions)))))

(leaf *leaf-configure
  :doc "configure leaf related packages"
  :config
  (leaf leaf-convert :ensure t
    :doc "convert many format to leaf format")

  (leaf leaf-tree
    :doc "interactive side-bar feature"
    :ensure t
    :commands leaf-tree-mode
    :custom
    ((imenu-list-size     . 30)
     (imenu-list-position . 'left)))

  ;; install feather.el
  (leaf feather :disabled t
    :el-get conao3/feather.el
    :custom (leaf-alias-keyword-alist . '((:ensure . :feather)))
    :config
    (feather-mode))

  (leaf bytecomp
    :tag "builtin"
    :custom (byte-compile-docstring-max-column . 120))
  )

;; optional packages for leaf keywords (under straight.el)
(leaf hydra :ensure t)


(leaf blackout :disabled t
  :ensure t)


(leaf paradox :when package-enable-at-startup
  :doc "modernized emacs package menu"
  :ensure t
  :defun paradox-enable
  :config (paradox-enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; initpkg.el ends here
