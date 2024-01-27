;;; util.el --- directory utility func -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Directory path utility functions
;;

;;; Code:
;; load package init
(eval-when-compile
  (load (expand-file-name "elisp/initpkg" user-emacs-directory)))
(require 'straight)


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


(leaf exec-path-from-shell
  :doc "load path from shell at startup"
  :ensure t
  :custom (exec-path-from-shell-arguments . '("-l"))
  :commands
  exec-path-from-shell-setenv
  exec-path-from-shell-getenv
  :defun exec-path-from-shell-initialize
  :config (when (daemonp) (exec-path-from-shell-initialize)))


(leaf *line-space-contol
  :doc "modifying line spaces on buffer"
  :config
  (defun rdm/sw-lnsp (wdth)
    "switch line spacing"
    (interactive "nset line spacing: ")
    (setq-local line-spacing wdth))
  (defun rdm/dbl-lnsp ()
    "switch line spacing to double"
    (interactive)
    (setq-local line-spacing 2.0))
  (defun rdm/lnsp-default ()
    "remove local line-spacing"
    (interactive)
    (kill-local-variable 'line-spacing)))


(leaf *text-scale-adjust
  :doc "adjust text scale on buffer"
  :preface
  (defvar rdm/text-scale-amount
    nil "text scale amount applied via rdm/text-scale-adjust, nil or number.
 if number given, applied for text-scale on current buffer")

  (defun rdm/text-scale-adjust ()
    "run text-scale-adjust on buffer with amount `rdm/text-scale-amount'"
    (interactive)
    (when (numberp rdm/text-scale-amount)
      (text-scale-increase rdm/text-scale-amount))))

(defsubst rdm/frame-bg-dark-p ()
  "Check `background-mode' of `frame-parameter'. If `dark' is set, return t."
  (eq (frame-parameter nil 'background-mode) 'dark))

(provide 'rdm/util)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; util.el ends here
