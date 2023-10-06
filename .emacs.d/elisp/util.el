;;; util.el --- directory utility func -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Directory path utility functions
;;

;;; Code:


(leaf *conf-cache-history
  :config
  (leaf '*conf-on-state
    :config
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
  (leaf *conf-chache-dir
    :defun
    (cache-sub-dir
     expand-file-rec)
    :config
    (defun expand-file-rec (path basedir)
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

    (defun cache-sub-dir (&optional name)
      "Check directory existence, and return full path to given NAME.
Create if cache sub directory NAME not exists.
If argument is not given, returns `emacs-cache-root-dir'"
      (let ((lpath (if (eq name nil)
                       (expand-file-name emacs-cache-root-dir)
                     (expand-file-name name emacs-cache-root-dir))))
        (if (not (file-directory-p lpath))
            (progn (make-directory lpath t) lpath)
          lpath)))

    (defun cache-sub-file (name &optional subdir)
      "returns file path like 'emacs-cache-root-dir/SUBDIR/NAME'
if name is not given, returns 'emacs-cache-root-dir/NAME'
argument NAME could be directory or filename"
      (expand-file-name name (cache-sub-dir subdir)))

    ;; create eln-cache in .cache
    (push (cache-sub-dir "eln-cache") native-comp-eln-load-path)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; util.el ends here
