;;; initskk.el --- ddskk config -*- no-byte-compile: t -*-
;;; Commentary:
;;
;; Provides ddskk configuration
;;

;;; Code:

;; skk init file
(setq skk-get-jisyo-directory (cache-sub-file "dict" "skk"))

(if (or (directory-empty-p skk-get-jisyo-directory)
        (not (file-directory-p skk-get-jisyo-directory)))
  (skk-get skk-get-jisyo-directory))

(dolist (conf
         `((skk-large-jisyo                          . ,(expand-file-name "SKK-JISYO.L" skk-get-jisyo-directory))
           (skk-azik-keyboard-type                   . 'us101)
           (skk-show-japanese-menu                   . t)
           (skk-show-inline                          . t)
           (skk-show-tooltip                         . nil)
           (skk-show-candidates-always-pop-to-buffer . t)
           (skk-henkan-strict-okuri-precedence       . t)
           (skk-henkan-number-to-display-candidates  . 7)
           (skk-dcomp-activate                       . t)
           (skk-dcomp-multiple-activate              . nil)
           (skk-dcomp-multiple-rows                  . 10)
           (skk-japanese-message-and-error           . t)
           (skk-delete-implies-kakutei               . nil)
           (skk-use-look                             . t)
           (skk-auto-insert-paren                    . t)
           (skk-search-katakana                      . jisx0201-kana)))
  (customize-set-variable
   (car conf) (cdr conf) "configured at initskk.el")
  )

;;(define-key skk-j-mode-map (kbd "M-n"))

;(setq skk-cdb-large-jisyo "~/.emacs.d/skkdict/SKK-JISYO.L.cdb")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; initskk.el ends here
