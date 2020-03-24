
;; init package
(require 'package)

;; use custom.el for package-selected-packages
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq package-archives
      (append
       '(("melpa" . "https://melpa.org/packages/")
         ("melpa-stable" . "https://stable.melpa.org/packages/")
         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
         ("org" . "http://orgmode.org/elpa/"))
   package-archives
))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


(setq load-prefer-newer t)


;; auto load use-package package
(unless (package-installed-p 'use-package)  ;; can't notify upgradable package
  (package-install 'use-package)
  (require 'use-package))

;; skk config(use default package.el)
(unless (package-installed-p 'ddskk)
  (package-install 'ddskk))
(package-initialize)
(setq default-input-method "japanese-skk")

;; skk dict, setting files
(setq skk-user-directory "~/.emacs.d/skk")
(setq skk-get-jisyo-directory "~/.emacs.d/skk/dict")
(setq skk-init-file "~/.emacs.d/elisp/initskk.el")

(require 'skk-study)
