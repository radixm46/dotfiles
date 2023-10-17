;;; early-init.el --- Early Initialization. -*- lexical-binding: t -*-
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
(push '(width          . 90) default-frame-alist)
(push '(height         . 75) default-frame-alist)
(push '(font           . "UDEV Gothic 35NFLG-13.0") default-frame-alist)
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



;; add timestamps to message buffer if certain env_var is set
(defun sh/current-time-microseconds ()
  "Return the current time formatted to include microseconds."
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d]" now-ms))))

(defun sh/ad-timestamp-message (FORMAT-STRING &rest _)
  "Advice to run before messages that prepends a timestamp to each message.
  Activate this advice with:
`(advice-add \\='message :before \\='sh/ad-timestamp-message)'"
  (unless (string-equal FORMAT-STRING "%s%s")
    (let ((deactivate-mark nil)
          (inhibit-read-only t))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (if (not (bolp))
            (newline))
        (insert (sh/current-time-microseconds) " ")))))

(when (getenv-internal "PROFILE_EMACS")
  (advice-add 'message :before 'sh/ad-timestamp-message))

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
