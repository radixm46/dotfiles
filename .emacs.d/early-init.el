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

;; init functions
(prog1 '*conf-on-state
  (defsubst emacs-works-on-term-p ()
    "returns t if emacs seems running on term"
    (not (or (daemonp) (display-graphic-p))))

  (defvar conf-on-term-hook nil "Hook run by conf-on-term.")
  (defvar conf-on-gui-hook  nil "Hook run by conf-on-gui.")

  (defun conf-on-term ()
    "fire hooked functions for swtitch appearance on tui frame"
    (interactive)
    (message "switch to term")
    (run-hooks 'conf-on-term-hook))

  (defun conf-on-gui ()
    "fire hooked functions for swtitch appearance on gui frame"
    (interactive)
    (message "switch to gui")
    (run-hooks 'conf-on-gui-hook))
  )

;; setup .config/cache/emacs
(prog1 '*conf-chache-dir
 (defsubst expand-file-rec (path basedir)
   "generate full path from basedir with given list

example:
  (expand-file-rec '(\"foo\" \"bar\" \"baz\") \"~\")
  => \"/home/user/foo/bar/buzz\""
   (cond
    ((length= path 1)
     (expand-file-name (car path) basedir))
    ((length> path 1)
     (expand-file-rec
      (cdr path) (expand-file-name (car path) basedir)))
    (t
     (message "invalid path value"))
    ))

 (defvar emacs-cache-root-dir
   (expand-file-rec '(".cache" "emacs") (getenv "HOME"))
   "emacs cache directory path (default: $HOME/.cache)")

 (defsubst cache-sub-dir (&optional name)
   "check directory path (create if not exists), and returns full path to dir.
if argument is not given, returns `emacs-cache-root-dir'"
   (let ((lpath (if (eq name nil)
                    (expand-file-name emacs-cache-root-dir)
                  (expand-file-name name emacs-cache-root-dir))))
     (if (not (file-directory-p lpath))
         (progn (make-directory lpath t) lpath)
       lpath)))

 (defsubst cache-sub-file (name &optional subdir )
   "returns file path like 'emacs-cache-root-dir/subdir/name'
if name is not given, returns 'emacs-cache-root-dir/name'
argument `name' could be directory or filename"
   (expand-file-name name (cache-sub-dir subdir)))

 ;; create eln-cache in .cache
 (push (cache-sub-dir "eln-cache") native-comp-eln-load-path)
 )

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
