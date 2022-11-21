;;; early-init.el --- Early Initialization. -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;;
;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;;; Code:
;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))

;; init package.el required
(setq package-enable-at-startup nil)
(setq load-prefer-newer t)

;; init frame
(setq frame-inhibit-implied-resize t)
(push '(menu-bar-lines . 0)  default-frame-alist)
(push '(tool-bar-lines . 0)  default-frame-alist)
(push '(width          . 80) default-frame-alist)
(push '(height         . 45) default-frame-alist)
(push '(font           . "UDEV Gothic 35NF-13.0") default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Suppress flashing at startup
(setq inhibit-redisplay t)
(setq inhibit-message   t)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq inhibit-redisplay nil)
			(setq inhibit-message   nil)
			(redisplay)))

;; Startup setting
(setq inhibit-splash-screen   t)
(setq inhibit-startup-message t)
(setq byte-compile-warnings   '(not cl-functions obsolete))

(setq inhibit-startup-echo-area-message t)
(setq initial-major-mode                'fundamental-mode)
(setq initial-scratch-message           'nil)



(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
