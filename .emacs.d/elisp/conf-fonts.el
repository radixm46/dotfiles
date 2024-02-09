;;; conf-fonts.el --- configure fonts -*- lexical-binding: t -*-
;;; Commentary:
;; fonts related things

;;; Code:

(eval-when-compile
  (load (expand-file-name "elisp/initpkg" user-emacs-directory)))
(require 'straight)


(leaf *font-configure
  :doc "font configure"
  :config
  (defun rdm/apply-func-in-fonts (fonts &optional func-found func-fail)
    "Apply `func-found' to the first available font in `fonts', if found.
If `func-found' is nil or not given, return the found font name.
If no font in `fonts' matches and `func-fail' is given, invoke `func-fail'.

`fonts' is a list of font names.
`func-found' is a function taking one argument, the found font.
`func-fail' is a function taking no arguments."
    (let ((fonts-available (font-family-list))
          (font-found nil))
      (catch 'found
        (dolist (f fonts)
          (when (member f fonts-available)
            (setq font-found f)
            (throw 'found f))))
      (if font-found
          (funcall (or func-found #'identity) font-found)
        (when func-fail (funcall func-fail)))))

  (leaf *font-patch-for-modes
    :doc "define fonts for some major modes remap"
    :defun
    rdm/apply-func-in-fonts rdm/default-frame-font rdm/update-font-conf
    remap-buffers-font-to-doc remap-font-to-doc remap-font-to-term
    remap-buffers-font-to-tables remap-buffers-font-to-term
    :mode-hook
    (conf-on-gui-hook . (;;"bind fonts on gui frame create, remap"
                         (rdm/update-font-conf)
                         (remap-buffers-font-to-doc)
                         (remap-buffers-font-to-term)
                         (remap-buffers-font-to-tables)))
    :preface
    (defsubst rdm/default-frame-font ()
      "Get the default font family from the default frame, without font size."
      (let ((default-fparam (cdr (assq 'font default-frame-alist))))
        (substring
         default-fparam
         0 (string-match "-" default-fparam))))

    (defvar font-for-tables "UDEV Gothic" "1:2 width font for table") ;; refered via org mode etc.
    (defvar font-for-term   nil           "1:2 width font for term")
    (defvar font-for-doc    nil           "3:5 width font for doc")

    (defun rdm/update-font-conf ()
      "update `font-for-...' params and fixed pitch"
      ;; configure `fixed-pitch'
      (let ((f (rdm/apply-func-in-fonts
                '("UDEV Gothic JPDOC"
                  "UDEV Gothic NFLG"
                  "Noto Sans Mono CJK JP"))))
        (when f (custom-set-faces `(fixed-pitch ((t :family ,f))))))

      (setq font-for-tables
            (rdm/apply-func-in-fonts
             '("PlemolJP ConsoleHS"
               "UDEV Gothic JPDOC"
               "HackGen"
               "Noto Sans Mono CJK JP")
             nil #'(face-attribute 'fixed-pitch :family)))

      (setq font-for-term
            (rdm/apply-func-in-fonts
             '("UDEV Gothic NFLG"
               "HackGen Console NF"
               "Noto Sans Mono CJK JP")
             nil #'(lambda () font-for-tables)))

      (setq font-for-doc
            (rdm/apply-func-in-fonts
             '("PlemolJP35 HS"
               "UDEV Gothic 35JPDOC"
               "Noto Sans CJK JP")
             nil #'(rdm/default-frame-font)))

      (let ((f (rdm/apply-func-in-fonts
                '("Hiragino Sans"
                  "BIZTER"
                  "BIZ UDP Gothic"
                  "Noto Sans CJK JP"))))
        (when f (custom-set-faces `(variable-pitch ((t :family ,f))))))
      )

    ;; aplly font-for-doc to buffers
    (defun remap-font-to-doc ()
      "if font-for-doc set, remap buffer face"
      (interactive)
      (when font-for-doc
        (face-remap-add-relative 'default `(:family ,font-for-doc))))

    (defvar remap-font-to-doc-modes-list nil
      "major mode list for font update")

    (defun remap-buffers-font-to-doc ()
      "remap buffer face with `font-for-doc'"
      (mapc #'(lambda (buffer)
                (with-current-buffer buffer
                  (when (member major-mode remap-font-to-doc-modes-list)
                    (remap-font-to-doc))))
            (buffer-list)))

    (defvar remap-font-to-term-modes-list nil
      "major mode list for font update")

    (defun remap-font-to-term ()
      "if `font-for-term' set, remap buffer face"
      (interactive)
      (when font-for-term
        (face-remap-add-relative 'default `(:family ,font-for-term))))

    ;; NOTE: hookに未設定
    (defun remap-buffers-font-to-term ()
      "remap buffer face with `font-for-term'"
      (mapc #'(lambda (buffer)
                (with-current-buffer buffer
                  (when (member major-mode remap-font-to-term-modes-list)
                    (remap-font-to-term))))
            (buffer-list)))

    (defun remap-font-to-tables ()
      "if `font-for-tables' set, remap buffer face"
      (interactive)
      (when font-for-tables
        (face-remap-add-relative 'default `(:family ,font-for-tables))))

    (defvar remap-font-to-tables-modes-list nil
      "major mode list for font update")

    (defun remap-buffers-font-to-tables ()
      "remap buffer face with `font-for-tables'"
      (mapc #'(lambda (buffer)
                (with-current-buffer buffer
                  (when (member major-mode remap-font-to-tables-modes-list)
                    (remap-font-to-term))))
            (buffer-list)))
    ;; (defvar font-for-code   "UDEV Gothic 35LG"     "3:5 width font for code") ;; NOTE: これまだ使う予定ない。org-modeのblockで部分的にリガチャ適用とかできるようになったら意義が出るかも
    ;; (defun remap-face-with-code-font () ;; NOTE: 通常、プログラミングで利用する場合コードポイントの幅はそんなに必要ないはずなので、利用する機会はないはず
    ;;   "remap buffer face with `font-for-code'"
    ;;   (interactive)
    ;;   (face-remap-add-relative 'default `(:family ,font-for-code))
    ;;   )

    )

  (leaf ligature :emacs>= "28"
    :doc "configure ligatures"
    :straight
    (ligature-mode :type git :host github
                   :repo "mickeynp/ligature.el" :branch "master")
    :defun ligature-set-ligatures
    :commands ligature-mode
    :config
    ;; all Jetbrains Mono Ligatures
    ;; TODO: xml系がtext-modeベースなので、htmlでリガチャ反映されないのをなんとかする
    (ligature-set-ligatures '(prog-mode
                              comint-mode)
                            '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<=" ">=" "&&" "&&&" "&="
                              "++" "+++" "***" ";;" "!!" "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>"
                              "<<<" ">>>" "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                              "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$" "$>" "<+>"
                              "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--" "-->" "->" "->>" "<<-" "<-"
                              "<=<" "=<<" "<<=" "<==" "<=>" "<==>" "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<"
                              ">->" "<-<" "<-|" "<=|" "|=>" "|->" "<->" "<<~" "<~~" "<~" "<~>" "~~" "~~>" "~>"
                              "~-" "-~" "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||" "|||>" "<|||"
                              "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::=" ":?" ":?>" "//" "///" "/*" "*/"
                              "/=" "//=" "/==" "@_" "__" "???" ";;;"))

    (defun activate-ligature-on-window-system ()
      "enable ligature on window system"
      (when (window-system) (ligature-mode)))
    :hook
    ((prog-mode-hook
      comint-mode-hook) . activate-ligature-on-window-system))

  (leaf *hide-nobreak-whitespace :emacs>= "28"
    :doc "hack to disable face for double byte space"
    :setq (nobreak-char-display . nil))

  :preface
  (defmacro rdm/available-serif-font ()
    "returns serif font family available in list"
    (eval (rdm/apply-func-in-fonts '("Hiragino Mincho ProN"
                                     "BIZ UDP Mincho"
                                     "Noto Serif CJK JP"))))

  (leaf nerd-icons
    ;; TODO: fix web-mode icons
    :ensure t
    :require t
    :doc "Nerd-icons.el is a library for easily using Nerd Font icons inside Emacs,
 an alternative to all-the-icons."
    :custom
    (nerd-icons-scale-factor . 1.2) ;; bit larger
    (nerd-icons-color-icons  . t)
    :defvar nerd-icons-extension-icon-alist
    :push ((nerd-icons-extension-icon-alist . '("org_archive" nerd-icons-sucicon "nf-custom-orgmode" :face nerd-icons-lblue))))

  (leaf emojify
    :emacs<= "24.3"
    :emacs> "28"
    :ensure t
    :custom
    `((emojify-emojis-dir      . ,(cache-sub-dir "emojis"))
      (emojify-display-style   . 'unicode)
      (emojify-composed-text-p . nil))
    :config
    (leaf *emojify-init :emacs< "28"
      :custom (emojify-display-style . 'ascii)
      :hook (after-init-hook . global-emojify-mode)))
  )

(provide 'rdm/conf-fonts)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conf-fonts.el ends here
