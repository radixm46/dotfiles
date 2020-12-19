;; skk init file

(setq skk-use-azik t)
(setq skk-azik-keyboard-type 'us101)
(setq skk-show-inline t)
(setq skk-show-tooltip t)
(setq skk-show-candidates-always-pop-to-buffer t)
(setq skk-henkan-show-candidates-rows 2)

;(setq skk-dcomp-activate t)
;(setq skk-dcomp-multiple-activate t)
;(setq skk-dcomp-multiple-rows 10)
;(define-key skk-j-mode-map (kbd "M-n"))

(setq skk-japanese-message-and-error t)
(setq skk-show-japanese-menu t)

(setq skk-delete-implies-kakutei nil)
(setq skk-use-look t)
(setq skk-henkan-strict-okuri-precedence t)


(if (file-directory-p skk-get-jisyo-directory)
    (ignore) (skk-get skk-get-jisyo-directory))
(setq skk-large-jisyo "~/.emacs.d/skk/dict/SKK-JISYO.L")
;(setq skk-cdb-large-jisyo "~/.emacs.d/skkdict/SKK-JISYO.L.cdb")

(use-package skk-study
  :ensure t)
