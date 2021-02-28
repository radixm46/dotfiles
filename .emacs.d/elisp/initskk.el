;; skk init file
(setq skk-use-azik t
      skk-azik-keyboard-type 'us101
      skk-show-inline t
      skk-show-tooltip t
      skk-show-candidates-always-pop-to-buffer t
      skk-henkan-show-candidates-rows 2
      skk-dcomp-activate nil
      ;skk-dcomp-multiple-activate t
      ;skk-dcomp-multiple-rows 10
      skk-japanese-message-and-error t
      skk-show-japanese-menu t
      skk-delete-implies-kakutei nil
      skk-use-look t
      skk-henkan-strict-okuri-precedence t)
;(define-key skk-j-mode-map (kbd "M-n"))

(if (file-directory-p skk-get-jisyo-directory)
    (ignore) (skk-get skk-get-jisyo-directory))
(setq skk-large-jisyo "~/.emacs.d/skk/dict/SKK-JISYO.L")
;(setq skk-cdb-large-jisyo "~/.emacs.d/skkdict/SKK-JISYO.L.cdb")
