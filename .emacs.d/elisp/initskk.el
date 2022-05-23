;; skk init file
(setq skk-get-jisyo-directory "~/.emacs.d/.cache/skk/dict")

(unless (file-directory-p skk-get-jisyo-directory)
  (skk-get skk-get-jisyo-directory))

(custom-set-variables
 '(skk-large-jisyo "~/.emacs.d/.cache/skk/dict/SKK-JISYO.L")
 '(skk-use-azik t)
 '(skk-azik-keyboard-type 'us101)
 '(skk-show-japanese-menu t)
 '(skk-show-inline t)
 '(skk-show-tooltip nil) ;; not correctly working with mini buffer
 '(skk-show-candidates-always-pop-to-buffer t)
 '(skk-henkan-strict-okuri-precedence t)
 '(skk-henkan-number-to-display-candidates 7)
 '(skk-dcomp-activate t)
 '(skk-dcomp-multiple-activate nil)
 '(skk-dcomp-multiple-rows 10)
 ;; '(skk-henkan-show-candidates-rows          2) ;; obsolete
 '(skk-japanese-message-and-error t)
 '(skk-delete-implies-kakutei nil)
 '(skk-use-look t)
 '(skk-auto-insert-paren t)
 )

;;(define-key skk-j-mode-map (kbd "M-n"))

;(setq skk-cdb-large-jisyo "~/.emacs.d/skkdict/SKK-JISYO.L.cdb")
