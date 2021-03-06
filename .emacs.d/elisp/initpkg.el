;; init package
(require 'package)

;; use custom.el for package-selected-packages
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq package-archives (append '(("melpa" . "https://melpa.org/packages/")
                                 ("melpa-stable" . "https://stable.melpa.org/packages/")
                                 ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                                 ("org" . "http://orgmode.org/elpa/")) package-archives))

(setq load-prefer-newer t)

;; auto load use-package package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
