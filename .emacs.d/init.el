;;; init.el --- emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Emacs configuration by radixM491VA
;;

;;; Code:

;; setup load performance profiling
(when (and (not noninteractive)
           (getenv "PROFILE_EMACS"))
  (prog1 '*configure-setup-tracker
    ;; https://zenn.dev/zk_phi/books/cba129aacd4c1418ade4/viewer/4d0a9dde1043c6eaffad
    (defvar setup-tracker--level 0)
    (defvar setup-tracker--parents nil)
    (defvar setup-tracker--times nil)

    (when load-file-name
      (push load-file-name setup-tracker--parents)
      (push (current-time) setup-tracker--times)
      (setq setup-tracker--level (1+ setup-tracker--level)))

    (add-variable-watcher
     'load-file-name
     (lambda (_ v &rest __)
       (cond ((equal v (car setup-tracker--parents))
              nil)
             ((equal v (cadr setup-tracker--parents))
              (setq setup-tracker--level (1- setup-tracker--level))
              (let* ((now (current-time))
                     (start (pop setup-tracker--times))
                     (elapsed (+ (* (- (nth 1 now) (nth 1 start)) 1000)
                                 (/ (- (nth 2 now) (nth 2 start)) 1000))))
                (with-current-buffer (get-buffer-create "*setup-tracker*")
                  (save-excursion
                    (goto-char (point-min))
                    (dotimes (_ setup-tracker--level) (insert "> "))
                    (insert
                     (file-name-nondirectory (pop setup-tracker--parents))
                     " (" (number-to-string elapsed) " msec)\n")))))
             (t
              (push v setup-tracker--parents)
              (push (current-time) setup-tracker--times)
              (setq setup-tracker--level (1+ setup-tracker--level)))))))

  ;; set profiler
  (require 'profiler)
  (profiler-start 'cpu)
  (add-hook 'after-init-hook
            #'(lambda () (profiler-report) (profiler-stop))))

;; ---------------  load package ---------------
(eval-and-compile
  (load (expand-file-name "elisp/initpkg" user-emacs-directory))
  (!el-load "elisp/util"
            "elisp/conf-fonts")
  (require 'rdm/util))


(leaf *startup
  :doc  "startup config"
  :config
  (leaf *config-for-non-darwin :disabled (!system-type 'darwin)
    :custom (command-line-ns-option-alist . nil))
  (leaf *config-for-non-linux  :disabled (!system-type 'gnu/linux)
    :custom (command-line-x-option-alist  . nil))
  (leaf *configure-garbage-collection
    :custom
    `((gc-cons-percentage          . 0.2)
      (garbage-collection-messages . t))
    :mode-hook
    (after-init-hook . (;; set garbage collection with timer (if available, use `garbage-collect-maybe')
                        (let ((gc-func (if (functionp 'garbage-collect-maybe)
                                           (lambda (fact)
                                             (when (garbage-collect-maybe fact)
                                               (message "garbage-collect-maybe done! (fact: %d)" fact)))
                                         (lambda (_) (garbage-collect)))))
                          (mapc (lambda (arg)
                                  (run-with-idle-timer
                                   (car arg) t
                                   (lambda () (funcall gc-func (cdr arg)))))
                                '((30.0 . 1) (60.0 . 3))))
                        (garbage-collect-maybe 2))))

  (leaf gcmh :disabled t
    :doc "gcmh works after startup
Enforce a sneaky Garbage Collection strategy to minimize GC interference with user activity."
    :ensure t
    :custom (gcmh-verbose . t)
    :defun gcmh-mode
    :mode-hook (emacs-startup-hook . ((gcmh-mode 1))))

  (leaf xt-mouse
    :doc "mouse support on terminal"
    :tag "builtin"
    :mode-hook
    (conf-on-term-hook . ((xterm-mouse-mode +1)))
    (conf-on-gui-hook  . ((xterm-mouse-mode -1))))
  )


(leaf *conf-cache-history
  :config
  (leaf autosave
    :doc "configure auto save files"
    :tag "builtin"
    :preface
    (leaf *disable-auto-save-message :emacs>= "27.1"
      :doc "supress auto saving message"
      :custom (auto-save-no-message . t))
    :custom
    `((auto-save-file-name-transforms . `((".*" ,(!expand-file-name "\\2" (cache-sub-dir "autosaved")) t)))
      (delete-auto-save-files         . t)
      (auto-save-default              . t)
      (auto-save-timeout              . 15)
      (auto-save-interval             . 120)
      (auto-save-list-file-prefix     . nil)))

  (leaf autorevert
    :tag "builtin"
    :doc "autorevert files if modified on disk"
    :custom (auto-revert-interval . 2)
    :global-minor-mode global-auto-revert-mode)

  (leaf *backup-files
    :doc "make backup files"
    :tag "builtin"
    :custom
    `((backup-directory-alist         . `(("." .  ,(cache-sub-dir "backup"))))
      (backup-by-copying              . t)
      (auto-save-list-file-prefix     . ,(!expand-file-name ".saves-" (cache-sub-dir "auto-save-list")))
      (make-backup-files              . t)
      (version-control                . t) ;; enable version control
      (kept-new-versions              . 5)
      (kept-old-versions              . 1)
      (delete-old-versions            . t)
      (create-lockfiles               . nil)))

  (leaf history
    :doc "configure history, log"
    :tag "builtin"
    :custom
    (message-log-max           . 10000)
    (history-length            . 1000)
    (history-delete-duplicates . t))

  (leaf recentf
    :tag "builtin"
    :custom
    `((recentf-save-file       . ,(cache-sub-file "recentf"))
      (recentf-max-saved-items . 2000)
      (recentf-auto-cleanup    . 'never)
      (recentf-exclude         . '("/\\.cache/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:"
                                   "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/"
                                   "/org/orgfiles/")))
    :defvar recentf-auto-save-timer
    :defun recentf-save-list
    :init
    (setq recentf-auto-save-timer
          (run-with-idle-timer 30 t #'(lambda ()
                                        (with-suppressed-message (recentf-save-list)))))
    :global-minor-mode recentf-mode)

  (leaf bookmark
    :tag "builtin"
    :custom
    `((bookmark-default-file . ,(cache-sub-file "bookmarks"))))

  (leaf multisession :emacs>= "29.1"
    :tag "builtin"
    :custom
    `((multisession-storage   . 'sqlite)
      (multisession-directory . ,(cache-sub-dir "multisession"))))

  (leaf persist
    :ensure t
    :defvar persist--directory-location
    :setq
    `((persist--directory-location . ,(cache-sub-dir "persist"))))
  )


(leaf *lang-locales
  :doc "locale and encoding"
  :custom
  (default-buffer-file-coding-system . 'utf-8)
  (coding-system-for-write           . 'utf-8)
  ;; (setq coding-system-for-read 'utf-8) ;; conflicts ddskk
  :config
  (set-locale-environment         nil)
  (set-default                    'buffer-file-coding-system 'utf-8)
  (set-language-environment       "Japanese")
  (set-default-coding-systems     'utf-8)
  (set-terminal-coding-system     'utf-8)
  (set-keyboard-coding-system     'utf-8)
  (set-buffer-file-coding-system  'utf-8)
  (prefer-coding-system           'utf-8)

  (leaf kkc
    :doc "built-in kanji-kana conversion"
    :tag "builtin"
    :custom
    `((kkc-init-file-name .  ,(cache-sub-file "kkcrc"))))

  (leaf ddskk
    :doc "setup ddsk"
    :ensure t
    :custom
    `((skk-use-azik         . t)
      (default-input-method . "japanese-skk")
      (skk-user-directory   . ,(cache-sub-dir "skk"))
      (skk-init-file        . ,(!expand-file-name "elisp/initskk.el" user-emacs-directory))))
  )


(leaf *editor-functions
  :custom
  (eol-mnemonic-dos          . "(CRLF)")
  (eol-mnemonic-mac          . "(CR)")
  (eol-mnemonic-unix         . "(LF)")
  (find-file-visit-truename  . t)
  (tab-width                 . 4)
  (indent-tabs-mode          . nil)
  (sentence-end-double-space . nil)
  (truncate-lines            . t)
  (word-wrap                 . t)
  (use-short-answers         . t)
  ;; configure indent tab to false as default
  ;; (setq line-move-visual t)
  :init
  (leaf *word-wrap-by-category :emacs>= "28.1"
    :doc "better wordwrapping above emacs 28"
    :custom (word-wrap-by-category . t))

  (leaf visual-line-mode
    :doc "visual line mode enables word wrap"
    :tag "builtin"
    :hook
    ((org-mode-hook
      markdown-mode-hook) . visual-line-mode)
    ;; (global-visual-line-mode nil)
    )

  (leaf pixel-scroll :emacs>= "29"
    :doc "enable pixel scrolling"
    :tag "builtin"
    :custom
    (pixel-scroll-precision-interpolate-page . t)
    :global-minor-mode pixel-scroll-precision-mode)

  (leaf ctrlf
    :doc "an intuitive and efficient solution for single-buffer text search in Emacs"
    :ensure t
    :commands ctrlf-mode
    :mode-hook (after-init-hook . ((ctrlf-mode +1))))

  (leaf ediff
    :ensure t
    :tag "builtin"
    :defvar ediff-word-2 ediff-word-3
    :defun ediff-setup-windows-plain
    :commands
    ediff ediff-files
    ediff3 ediff-files3
    :custom
    (ediff-window-setup-function . #'ediff-setup-windows-plain)
    (ediff-split-window-function . #'split-window-horizontally)
    :setq
    (ediff-word-2 . "0-9.,。、")
    (ediff-word-3 . "`'?!:;\"{}[]()「」（）？！")
    :config
    (defun ediff-window-display-p ()
      "overwritten ediff builtin function, for not generate new frame" nil))

  (leaf *load-evil-conf
    :doc "load evil"
    :preface
    (eval-and-compile (!el-load "elisp/conf-evil")))

  (leaf editorconfig
    :doc "load editorconfig"
    :ensure t
    :commands editorconfig-mode
    :mode-hook (after-init-hook . ((editorconfig-mode))))

  (leaf treesit-auto :emacs>= "29.1" :when (!sys-featurep "TREE_SITTER")
    :ensure t
    :doc "automatically install and enables treesit major modes"
    :custom (treesit-auto-install . t)
    :defvar treesit-auto-opt-out-list
    :global-minor-mode global-treesit-auto-mode
    :config
    (leaf treesit-fold
      :doc "`ts-fold' port works for built-in `treesit' feature"
      :straight
      (treesit-fold
       :type git :host github
       :repo "abougouffa/treesit-fold")
      :custom
      (treesit-fold-indicators-fringe       . 'left-fringe)
      (treesit-fold-indicators-priority     . 30)
      (treesit-fold-summary-show            . nil)
      (treesit-fold-summary-max-length      . 60)
      (treesit-fold-summary-exceeded-string . "...")
      (treesit-fold-summary-format          . " <S> %s ")
      :global-minor-mode
      global-treesit-fold-mode
      global-treesit-fold-indicators-mode
      )
    )

  (leaf tree-sitter
    :emacs< "29.1" :emacs>= "25.1"
    :doc "provides tree-sitter highlighting"
    :ensure t
    :commands (tree-sitter-hl-mode)
    :hook ((tree-sitter-after-on-hook . tree-sitter-hl-mode)
           (after-init-hook           . global-tree-sitter-mode))
    :init (leaf tree-sitter-langs :ensure t)
    :global-minor-mode global-tree-sitter-mode
    :config
    (leaf ts-fold
      :doc "Code-folding using tree-sitter"
      :straight
      (ts-fold
       :type git :host github
       :repo "emacs-tree-sitter/ts-fold")
      :custom
      (ts-fold-summary-show            . t)
      (ts-fold-summary-max-length      . 70)
      (ts-fold-summary-exceeded-string . "...")
      (ts-fold-summary-format          . "<Sum>: %s ")
      :config
      (leaf ts-fold-indicators
        :straight
        (ts-fold-indicators
         :type git :host github
         :repo "emacs-tree-sitter/ts-fold"))

      (leaf *patch-ts-fold-with-treesit-modes :emacs>= "29"
        :after tree-sitter
        :doc "bind *-mode fold range to *-ts-mode until emacs29 support has come"
        :url "https://github.com/emacs-tree-sitter/ts-fold/issues/48"
        :defvar ts-fold-range-alist
        :config
        (mapcar (lambda (a)
                  (when-let ((matched (assoc (car a) ts-fold-range-alist)))
                    (add-to-list 'ts-fold-range-alist (cons (cdr a) (cdr matched)))))
                '((sh-mode         . bash-ts-mode)
                  (c++-mode        . c++-ts-mode)
                  (c++-mode        . c-or-c++-ts-mode)
                  (c-mode          . c-ts-mode)
                  (csharp-mode     . csharp-ts-mode)
                  (css-mode        . css-ts-mode)
                  (go-mode         . go-ts-mode)
                  (java-mode       . java-ts-mode)
                  (js-mode         . js-ts-mode)
                  (json-mode       . json-ts-mode)
                  (python-mode     . python-ts-mode)
                  (ruby-mode       . ruby-ts-mode)
                  (rust-mode       . rust-ts-mode)
                  (conf-toml-mode  . toml-ts-mode)
                  (typescript-mode . typescript-ts-mode)
                  (yaml-mode       . yaml-ts-mode))))
      :hook
      (prog-mode-hook . ts-fold-mode)
      (prog-mode-hook . ts-fold-indicators-mode))

    (leaf *patch-major-ts-modes :emacs>= "29"
      :doc "bind major-ts-mode for tree-sitter syntax highlighting"
      :defvar tree-sitter-major-mode-language-alist
      :config
      (dolist (mode-tsl '((bash-ts-mode       . bash)
                          (c++-ts-mode        . cpp)
                          (c-or-c++-ts-mode   . cpp)
                          (c-ts-mode          . c)
                          (csharp-ts-mode     . c-sharp)
                          (css-ts-mode        . css)
                          (go-ts-mode         . go)
                          (java-ts-mode       . java)
                          (js-ts-mode         . javascript)
                          (json-ts-mode       . json)
                          (python-ts-mode     . python)
                          (ruby-ts-mode       . ruby)
                          (rust-ts-mode       . rust)
                          (toml-ts-mode       . toml)
                          (tsx-ts-mode        . typescript)
                          (typescript-ts-mode . typescript)
                          (yaml-ts-mode       . yaml)))
        (add-to-list 'tree-sitter-major-mode-language-alist mode-tsl)))
    )

  (leaf origami
    :doc "folding blocks"
    :commands origami-mode
    :ensure t)

  (leaf yasnippet
    :ensure t
    :hook
    ((prog-mode-hook     . yas-minor-mode)
     (emacs-startup-hook . yas-global-mode))
    :init
    (leaf yasnippet-snippets :ensure t)
    :config
    (leaf consult-yasnippet
      :doc "consulting yasnippet candidates"
      :straight
      (consult-yasnippet :type git :host github
                         :repo "mohkale/consult-yasnippet")
      :bind
      (("M-g s"   . consult-yasnippet)
       ("M-g M-s" . consult-yasnippet))))

  (leaf tab-bar :emacs>= "27.1"
    :tag "builtin"
    :doc "configure tab-bar mode"
    :custom (tab-bar-show . 1)
    :custom-face (tab-bar-tab . '((t (:underline t))))
    :hydra
    (hydra-tab-bar
     (:hint nil)
     (format "\
                       ^^%s tab-bar-mode^^
^^^^-----------------------------------------------------------------
 _n_:   new                       _j_:   next
 _x_:   close                     _k_:   previous
 _r_:   rename                    _s_:   select

 _SPC_: BACK
" (nerd-icons-faicon "nf-fa-window_maximize"))
     ("n"   tab-bar-new-tab)
     ("x"   tab-bar-close-tab)
     ("r"   tab-rename)
     ("j"   tab-bar-switch-to-next-tab)
     ("k"   tab-bar-switch-to-prev-tab)
     ("s"   tab-bar-select-tab-by-name)
     ("SPC" hydra-manage-windows/body :exit t))
    )

  (leaf quickrun :ensure t
    :commands quickrun)

  (leaf highlight-symbol
    :ensure t
    :doc "automatic and manual symbol highlighting for Emacs"
    :custom (highlight-symbol-idle-delay . 1.0)
    :commands highlight-symbol-mode
    :defun doom-color
    :init
    (leaf *patch-highlight-symbol
      :preface
      (defsubst patch-highlight-symbol ()
        "patch highlight-symbol color"
        (custom-set-faces
         `(highlight-symbol-face  ((t (:background ,(doom-color 'dark-cyan) :foreground ,(doom-color 'bg)))))))
      :hook (after-load-theme-hook . patch-highlight-symbol)))

  (leaf *config-completion-styles
    :doc "setup completion packages"
    :config
    (leaf fussy
      :doc "provide a completion-style to Emacs that is able to leverage
 flx as well as various other fuzzy matching scoring packages"
      :ensure t
      :init
      (leaf orderless :ensure t
        :doc "Advanced completion style")
      (leaf hotfuzz :ensure t
        :doc "fuzzy Emacs completion style with a better scoring algorithm.")
      :custom
      (fussy-use-cache             . t)
      (fussy-score-fn              . 'fussy-hotfuzz-score)
      (fussy-filter-fn             . 'fussy-filter-orderless)
      (fussy-compare-same-score-fn . 'fussy-histlen->strlen<))
    :custom
    (completion-styles             . '(fussy basic))
    (completion-category-overrides . '((file      . (styles partial-completion))))
    :mode-hook
    (minibuffer-setup-hook . ((setq-local completion-styles '(orderless)))))
  )


(leaf *conf-appearance
  :custom
  (ring-bell-function             . 'ignore)
  (fill-column                    . 85)
  (line-spacing                   . 0.40)
  (idle-update-delay              . 0.8)
  ;; hide cursor on inactive windows
  (cursor-in-non-selected-windows . nil)
  (highlight-nonselected-windows  . nil)
  :config
  (display-time)
  (column-number-mode t)
  (blink-cursor-mode 0)
  (which-function-mode 1)
  (transient-mark-mode 1)

  (leaf *config-custom-themes-path
    :doc "load custom theme from elisp dir"
    :custom `(custom-theme-directory . ,(!expand-file-name "elisp" user-emacs-directory)))

  (leaf *conf-theme-hook
    :doc "reconfigure appearance after `load-theme' fired"
    :url "https://emacs.stackexchange.com/a/28947"
    :preface
    (defvar after-load-theme-hook nil
      "Hook run after color theme is loaded using `load-theme'.")
    (defun run-after-load-theme-hook (&rest _args)
      "Run `after-load-theme-hook'."
      (run-hooks 'after-load-theme-hook))
    :advice
    (:after load-theme
            run-after-load-theme-hook)
    )

  (leaf *config-doom-modeline
    :config
    (eval-and-compile ;; load required for doom-* functions
      (load (!expand-file-name "elisp/doom" user-emacs-directory))))

  (leaf solaire-mode :after doom-themes
    :doc "alternate bg color for 'unreal buffers'"
    :ensure t
    :custom (solaire-global-mode . 1)
    :global-minor-mode solaire-global-mode)

  (leaf *parenthesis-related-things
    :doc "smartparens and paredit for manipulate, paren for highlighting"
    :config
    (leaf paren
      :doc "configure show-paren-mode"
      :tag "builtin"
      :config
      (leaf *patch-paren-face
        :preface
        (defsubst patch-paren-face ()
          (custom-set-faces
           `(show-paren-match  ((t (:background ,(doom-color 'green)))))))
        :hook (after-load-theme-hook . patch-paren-face))
      :hook
      (prog-mode-hook
       conf-mode-hook . show-paren-mode))

    (leaf electric-pair-mode :disabled t
      :doc "automatic pares parenthesis"
      :tag "builtin"
      :emacs>= "24"
      :hook
      ((conf-mode-hook
        prog-mode-hook) . electric-pair-mode))

    (leaf smartparens
      :ensure t
      :custom (sp-show-pair-delay . 0.125)
      :commands smartparens-mode
      :mode-hook
      (after-load-theme-hook . ((custom-set-faces
                                 `(sp-show-pair-match-face
                                   ((t (:background ,(doom-color 'green) :foreground ,(doom-color 'bg))))))))
      :hook
      ((prog-mode-hook
        conf-mode-hook) . smartparens-mode))

    (leaf paredit :ensure t
      :commands paredit-mode
      :doc "for `evil-cleverparens' dependency"))

  (leaf display-line-numbers
    :tag "builtin"
    :emacs>= "26"
    :custom
    (display-line-numbers-type . 'relative)
    ;; preserve width
    (display-line-numbers-width . 4)
    :hook
    ((conf-mode-hook
      fundamental-mode-hook
      outline-mode-hook
      prog-mode-hook
      text-mode-hook
      git-timemachine-mode-hook) . display-line-numbers-mode)
    :defvar display-line-numbers-type
    :defun display-line-numbers--turn-on
    :config
    (defun rdm/display-line-numbers-toggle-rel ()
      "toggle relative"
      (interactive)
      (if (eq display-line-numbers-type 'relative)
          (customize-set-variable 'display-line-numbers-type  t)
        (customize-set-variable 'display-line-numbers-type  'relative))
      (display-line-numbers--turn-on)) ;; read config

    (leaf *patch-display-line-numbers-current
      :preface
      (defsubst patch-display-line-numbers-current ()
        "patch `line-number-current-line'"
        (custom-set-faces
         `(line-number-current-line ((t (:foreground ,(doom-color 'fg) :background ,(doom-color 'bg))))))
        )
      :hook
      (after-load-theme-hook . patch-display-line-numbers-current))
    )

  (leaf display-fill-column-indicator
    :tag "builtin"
    :emacs>= "27"
    :custom
    (display-fill-column-indicator-column . 90)
    (display-fill-column-indicator-character . ?\N{U+FF65}) ;; ·
    :hook
    ((conf-mode-hook
      fundamental-mode-hook
      outline-mode-hook
      prog-mode-hook
      text-mode-hook) . display-fill-column-indicator-mode)
    :config
    (leaf *patch-fill-column-indicator
      :preface
      (defsubst patch-fill-column-indicator ()
        (custom-set-faces
         `(fill-column-indicator ((t (:foreground ,(doom-color 'blue)))))))
      :hook
      (after-load-theme-hook . patch-fill-column-indicator)
      )
    )

  (leaf hl-todo
    :ensure t
    :commands hl-todo-mode
    :hook
    (prog-mode-hook . hl-todo-mode))

  (leaf ov
    :ensure t
    :init
    (defvar rdm/ov-serif nil "overlay with serif font")
    (defun rdm/toggle-buffer-ov-serif (trigger-state-p)
      "temporary make buffer to serif, ov-clear if ov available at buffer"
      (let ((serif-fnt (rdm/available-serif-font)))
        (if (and serif-fnt trigger-state-p)
            (progn (rdm/sw-lnsp 0.8)
                   (setq-local rdm/ov-serif
                               (ov (point-min) (point-max) 'face `(:family ,serif-fnt))))
          (progn (rdm/lnsp-default) (ov-reset rdm/ov-serif)))))
    :commands (ov-reset ov-clear ov))

  (leaf darkroom
    :ensure t
    :init
    (defun rdm/darkroom-init ()
      (rdm/toggle-buffer-ov-serif darkroom-mode))
    :hook (darkroom-mode-hook . rdm/darkroom-init)
    ;;(darkroom-mode . ov-clear)
    :bind ([f9] . darkroom-mode)
    :custom (darkroom-text-scale-increase . 1)
    ;;:config
    ;;(doom-modeline t) ;TODO:enable modeline
    )

  (leaf spacious-padding
    :ensure t
    :custom
    (spacious-padding-widths           . '(:internal-border-width 15
                                           :header-line-width     4
                                           :mode-line-width       6
                                           :tab-width             4
                                           :right-divider-width   30
                                           :scroll-bar-width      8))
    (spacious-padding-subtle-mode-line . '(:mode-line-active   'default
                                           :mode-line-inactive vertical-border))
    :global-minor-mode spacious-padding-mode)


  (leaf nyan-mode
    :ensure t
    :defun nyan-start-animation nyan-stop-animation
    :defvar nyan-wavy-trail
    :custom (nyan-cat-face-number . 4)
    :config
    (defun nyan-try ()
      (nyan-stop-animation)
      (setq nyan-wavy-trail nil))
    (defun nyan-xit ()
      (nyan-start-animation)
      (setq nyan-wavy-trail t))
    (nyan-xit)
    :hook
    ((evil-normal-state-entry-hook . nyan-try)
     (evil-normal-state-exit-hook . nyan-xit))
    :global-minor-mode nyan-mode)

  (leaf highlight-indent-guides
    :ensure t
    :commands highlight-indent-guides-mode
    :hook
    ((conf-mode-hook
      outline-mode-hook
      prog-mode-hook
      text-mode-hook
      git-timemachine-mode-hook) . highlight-indent-guides-mode)
    :custom
    (highlight-indent-guides-auto-enabled        . nil)
    (highlight-indent-guides-auto-odd-face-perc  . 10)
    (highlight-indent-guides-auto-even-face-perc . 10)
    (highlight-indent-guides-method              . 'column)
    (highlight-indent-guides-responsive          . 'top)
    (highlight-indent-guides-delay               . 0)
    ;; (highlight-indent-guides-auto-character-face-perc . 90)
    :config
    (leaf *patch-highlight-indent-guides-color
      :doc "patch `indent-guides-color'"
      :preface
      (defsubst patch-highlight-indent-guides-color ()
        (let ((inactive-c (doom-blend 'bg 'bg-alt    0.7))
              (active-c   (doom-blend 'bg 'dark-blue 0.5)))
          (custom-set-faces
           `(highlight-indent-guides-odd-face      ((t (:background ,inactive-c))))
           `(highlight-indent-guides-top-odd-face  ((t (:background ,active-c))))
           `(highlight-indent-guides-even-face     ((t (:background ,inactive-c))))
           `(highlight-indent-guides-top-even-face ((t (:background ,active-c)))))))
      :hook
      ((after-load-theme-hook
        highlight-indent-guides-mode-on-hook) . patch-highlight-indent-guides-color))
    )

  (leaf rainbow-delimiters
    :ensure t
    :commands rainbow-delimiters-mode
    :hook
    ((conf-mode-hook
      prog-mode-hook
      git-timemachine-mode-hook) . rainbow-delimiters-mode))

  (leaf whitespace
    :doc "configure whitespace"
    :hook
    ((conf-mode-hook
      outline-mode-hook
      prog-mode-hook
      text-mode-hook
      git-timemachine-mode-hook) . whitespace-mode)
    :custom
    (whitespace-global-modes . '(not dired-mode tar-mode neotree magit-status-mode magit-revision-mode hexl-mode))
    (whitespace-line-column . 80)
    (whitespace-style . '(face ;; enable
                          trailing
                          space-mark
                          newline newline-mark
                          ;; tab-mark tabs ;; conflicts highlight indent mode
                          ;;empty  ; empty line
                          ;;lines-tail
                          ;;spaces
                          ))
    (whitespace-display-mappings . '((tab-mark     ?\t [?\xBB ?\t] [?\\ ?\t])
                                     (newline-mark ?\n [?\u21A9 ?\n])))
    :custom-face
    (whitespace-tab      . '((t (:inherit 'shadow))))
    (whitespace-newline  . '((nil (:inherit 'shadow))))
    ;;(set-face-attribute 'whitespace-newline nil
    ;;  :foreground "SlateGray"
    ;;  :background nil
    ;;  :underline nil)
    ;;(set-face-attribute 'whitespace-space nil
    ;;  :background rdm/bg-color
    ;;  :foreground "GreenYellow"
    ;;  :weight 'bold)
    ;;(set-face-attribute 'whitespace-empty nil
    ;;  :background rdm/bg-color)
    :config
    (leaf *patch-whitespace-face
      :preface
      (defsubst patch-whitespace-face ()
        (custom-set-faces
         `(whitespace-trailing
           ((nil (:background ,(doom-color 'bg) ;:inherit 'default
                              :foreground ,(doom-color 'magenta)
                              :underline (:style wave)))))))
      :hook
      (after-load-theme-hook . patch-whitespace-face)
      )
    :global-minor-mode global-whitespace-mode
    )

  (leaf beacon :disabled t
    :ensure t
    :custom
    ((beacon-size . 45)
     (beacon-color . "orange3")
     (beacon-blink-duration . 0.20)
     (beacon-blink-delay . 0.20))
    :config
    (beacon-mode t)
    (eval-after-load 'Evil
      (evil-define-key 'normal 'global (kbd "SPC") 'beacon-blink))
    (add-to-list 'beacon-dont-blink-predicates
                 (lambda () (eq major-mode 'eww-mode)))
    )

  (leaf pulsar
    :ensure t
    :custom
    (pulsar-pulse                  . t)
    (pulsar-face                   . 'pulsar-magenta)
    (pulsar-highlight-face         . 'pulsar-yellow)
    (pulsar-delay                  . 0.040)
    (pulsar-iterations             . 10)
    (pulsar-pulse-functions        . '(
                                       ;; move-to-window-line-top-bottom
                                       ;; reposition-window
                                       ;; forward-page
                                       ;; backward-page
                                       ;; scroll-up-command
                                       ;; scroll-down-command
                                       recenter-top-bottom
                                       evil-scroll-line-to-top
                                       evil-scroll-line-to-center
                                       evil-scroll-line-to-bottom
                                       org-next-visible-heading
                                       org-previous-visible-heading
                                       org-forward-heading-same-level
                                       org-backward-heading-same-level
                                       outline-backward-same-level
                                       outline-forward-same-level
                                       outline-next-visible-heading
                                       outline-previous-visible-heading
                                       outline-up-heading
                                       ))
    :hook
    ((prog-mode-hook
      conf-mode-hook
      text-mode-hook) . pulsar-mode)
    ;; integrate with other functions
    (consult-after-jump-hook . pulsar-reveal-entry)
    (consult-after-jump-hook . pulsar-recenter-center)
    (imenu-after-jump-hook   . pulsar-reveal-entry)
    (imenu-after-jump-hook   . pulsar-recenter-center)
    :commands pulsar-mode
    :config
    ;; (pulsar-global-mode 1)
    (leaf *patch-pulsar-color
      :preface
      (defsubst patch-pulsar-color ()
        (custom-set-faces
         `(pulsar-magenta ((t (:background ,(doom-color 'magenta)))))
         `(pulsar-yellow  ((t (:background ,(doom-color 'yellow)))))
         `(pulsar-red     ((t (:background ,(doom-color 'red)))))
         `(pulsar-blue    ((t (:background ,(doom-color 'blue)))))
         `(pulsar-cyan    ((t (:background ,(doom-color 'cyan)))))
         `(pulsar-green   ((t (:background ,(doom-color 'green)))))))
      :hook
      ((after-load-theme-hook
        pulsar-mode-hook) . patch-pulsar-color))

    (evil-define-key 'normal 'global
      (kbd "SPC")   'pulsar-pulse-line
      (kbd "S-SPC") 'pulsar-highlight-line)
    )
  )

(leaf *conf-appearance-on-state
  :doc "apply theme and frame state hooks after init"
  :mode-hook
  (server-after-make-frame-hook . (;; NOTE: to activate solaire-mode on launch
                                   ;; https://github.com/hlissner/emacs-solaire-mode/issues/46
                                   (unless (frame-list) (load-theme 'doom-spacegrey t))
                                   (if (display-graphic-p)
                                       (conf-on-gui) (conf-on-term))))
  (after-init-hook              . ((load-theme 'doom-spacegrey t)
                                   (if (display-graphic-p)
                                       (conf-on-gui) (conf-on-term)))))


(leaf *minibuffer-extentions
  :doc "extend minibuffer functions"
  :config
  (leaf consult
    :ensure t
    :doc "Useful search and navigation commands"
    :hydra
    ((hydra-consult
      (:hint nil)
      (format "\
                           ^^%s consult functions^^
^^^^--------------------------------------------------------------------------------
 _b_:   virtual buffers                    _s_:   search
 _e_:   editing                            _g_:   grep & find
 _r_:   register                           _c_:   compilation
 _n_:   navigation                         _i_:   miscellaneous
^^^^................................................................................
 ^modes^                                   ^prefix^
 _m c_: mode command                       ^[l]ocal / [g]lobal / [m]ajor^
 _m m_: minor mode menu                    ^[i] on / [o]ff / [l]ocal / [g]lobal^
 ^history^
 _h h_: history                            _h c_: complex command
"  (nerd-icons-faicon "nf-fa-list_ul"))
      ("b" hydra-consult:virtual-buffer/body :exit t)
      ("e" hydra-consult:editing/body        :exit t)
      ("r" hydra-consult:register/body       :exit t)
      ("n" hydra-consult:navigation/body     :exit t)
      ("s" hydra-consult:search/body         :exit t)
      ("g" hydra-consult:grep-find/body      :exit t)
      ("c" hydra-consult:compilation/body    :exit t)
      ("o" hydra-consult:org/body            :exit t)
      ("i" hydra-consult:miscellaneous/body  :exit t)
      ("m m" consult-minor-mode-menu)
      ("m c" consult-mode-command)
      ("h h" consult-history         :exit t)
      ("h c" consult-complex-command :exit t))

     (hydra-consult:virtual-buffer
      (:hint nil)
      (format "\
                       ^^^%s consult virtual-buffer^^^
^^^^^^--------------------------------------------------------------------------------
 _b_:   buffer                                           _r_:   recent file
                             _B_:   bookmark

 _SPC_: BACK
" (nerd-icons-faicon "nf-fa-list_ul"))
      ("b" consult-buffer       :exit t)
      ("B" consult-bookmark     :exit t)
      ("r" consult-recent-file  :exit t)
      ("SPC" hydra-consult/body :exit t))

     (hydra-consult:editing
      (:hint nil)
      (format "\
                           ^^%s consult editing^^
^^^^--------------------------------------------------------------------------------
 _y_:   yank from kill ring                _r_:   yank replace
 _p_:   yank pop                           _k_:   kmacro

 _SPC_: BACK
" (nerd-icons-faicon "nf-fa-list_ul"))
      ("y" consult-yank-from-kill-ring :exit t)
      ("p" consult-yank-pop            :exit t)
      ("r" consult-yank-replace        :exit t)
      ("k" consult-kmacro              :exit t)
      ("SPC" hydra-consult/body :exit t))

     (hydra-consult:register
      (:hint nil)
      (format "\
                          ^^%s consult register^^^
^^^^^^--------------------------------------------------------------------------------
 _r_:   register                                         _s_:   store
 _l_:   load

 _SPC_: BACK
" (nerd-icons-faicon "nf-fa-list_ul"))
      ("r" consult-register        :exit t)
      ("l" consult-register-load   :exit t)
      ("s" consult-register-store  :exit t)
      ("SPC" hydra-consult/body :exit t))

     (hydra-consult:navigation
      (:hint nil)
      (format "\
                           ^^%s consult navitation^^^
^^^^^^--------------------------------------------------------------------------------
 _g_:   goto line            _m_:   makr                 _i_:   imenu
 _o_:   outline              _M_:   global mark          _I_:   imenu multi

 _SPC_: BACK
" (nerd-icons-faicon "nf-fa-list_ul"))
      ("g" consult-goto-line   :exit t)
      ("m" consult-mark        :exit t)
      ("M" consult-global-mark :exit t)
      ("o" consult-outline     :exit t)
      ("i" consult-imenu       :exit t)
      ("I" consult-imenu-multi :exit t)
      ("SPC" hydra-consult/body :exit t))

     (hydra-consult:search
      (:hint nil)
      (format "\
                          ^^^%s consult search^^^
^^^^^^--------------------------------------------------------------------------------
 _l_:   line                                             _k_:   keep lines
 _L_:   line multi           _M_:   multi occur          _f_:   focus lines

 _SPC_: BACK
" (nerd-icons-faicon "nf-fa-list_ul"))
      ("l" consult-line        :exit t)
      ("L" consult-line-multi  :exit t)
      ("M" consult-multi-occur :exit t)
      ("k" consult-keep-lines  :exit t)
      ("f" consult-focus-lines :exit t)
      ("SPC" hydra-consult/body :exit t))

     (hydra-consult:grep-find
      (:hint nil)
      (format "\
                          ^^^%s consult find^^^
^^^^^^--------------------------------------------------------------------------------
 _g_:   grep                 _r_:   ripgrep              _f_:   find
 _G_:   git grep             _l_:   locate

 _SPC_: BACK
" (nerd-icons-faicon "nf-fa-list_ul"))
      ("g" consult-grep     :exit t)
      ("r" consult-ripgrep  :exit t)
      ("G" consult-git-grep :exit t)
      ("f" consult-find     :exit t)
      ("l" consult-locate   :exit t)
      ("SPC" hydra-consult/body :exit t))

     (hydra-consult:compilation
      (:hint nil)
      (format "\
                     ^^^%s consult compilation^^^
^^^^^^--------------------------------------------------------------------------------
 _e_:   compile error        _m_:   flymake              _x_:   xref
 ^ ^                         _c_:   flycheck

 _SPC_: BACK
" (nerd-icons-faicon "nf-fa-list_ul"))
      ("e" consult-compile-error :exit t)
      ("m" consult-flymake       :exit t)
      ("c" consult-flycheck      :exit t)
      ("x" consult-xref          :exit t)
      ("SPC" hydra-consult/body :exit t))

     (hydra-consult:miscellaneous
      (:hint nil)
      (format "\
                      ^^%s consult miscellaneous^^
^^^^--------------------------------------------------------------------------------
 _m_:   man                                _t_:   theme
 _p_:   preview at point

 _SPC_: BACK
" (nerd-icons-faicon "nf-fa-list_ul"))
      ("m" consult-man                      :exit t)
      ("t" consult-theme                    :exit t)
      ("p" consult-preview-at-point         :exit t)
      ("SPC" hydra-consult/body :exit t))
     )

    :bind
    (("M-g g"   . consult-line)
     ("M-g M-g" . consult-line) ;; replace native goto-line
     ("M-g l"   . consult-goto-line)
     ("M-g M-l" . consult-goto-line)
     ("M-g o"   . consult-outline)
     ("M-g M-o" . consult-outline)
     ("M-g i"   . consult-imenu)
     ("M-g M-i" . consult-imenu)
     ("M-g m"   . consult-mark)
     ("M-g M-m" . consult-mark)
     ("M-g M"   . consult-global-mark)
     ("M-g M-M" . consult-global-mark)
     ("M-g d"   . consult-dir)
     ("M-g M-d" . consult-dir)
     ("M-g c"   . consult-minor-mode-menu) ; narrowing with i/o/l/g
     ("M-g M-c" . consult-minor-mode-menu)
     ("M-g C"   . consult-mode-command) ; narrowing with l/g/m
     ("M-g M-C" . consult-mode-command)
     ("M-g B"   . consult-bookmark)
     ("M-g f"   . consult-find)
     ("M-g M-f" . consult-find)
     ("M-g b"   . consult-buffer)
     ("M-g M-b" . consult-buffer)
     ("M-g a"   . consult-org-agenda)
     ("M-g M-a" . consult-org-agenda)
     ("M-g M-y" . consult-yank-from-kill-ring)
     ("M-g y"   . consult-yank-from-kill-ring)
     ("M-y"     . consult-yank-from-kill-ring)
     ("C-x b"   . consult-buffer) ; replace switch-to-buffer. narrowing with b/f/m/p
     ("M-5"     . hydra-consult/body)
     ("<f5>"    . hydra-consult/body))
    (:org-mode-map ;; override outline on org-mode
     ("M-g o"   . consult-org-heading)
     ("M-g M-o" . consult-org-heading))

    :init
    (leaf *consult-use-fd :when (!executable-find "fd")
      :doc "use fd for find if avalable"
      :custom (consult-find-command . "fd --color=never --full-path ARG OPTS"))

    (leaf *consult-use-rigrep :when (!executable-find "rg")
      :doc "use ripgrep for grep if avalable"
      :custom (consult-grep-command . '(concat "rg --null --color=ansi"
                                               "--max-columns=250"
                                               "--no-heading --line-number "
                                               ;; adding these to default
                                               "--smart-case --hidden "
                                               "--max-columns-preview "
                                               ;; add back filename to get parsing to work
                                               "--with-filename "
                                               ;; defaults
                                               "-e ARG OPTS")))
    (evil-ex-define-cmd "ls" 'consult-buffer)
    (evil-ex-define-cmd "reg" 'consult-yank-from-kill-ring)
    (leaf consult-dir :ensure t))

  (leaf marginalia
    :ensure t
    :doc "Rich annotations in the minibuffer"
    :defun marginalia-mode
    :config (marginalia-mode))

  (leaf embark
    :ensure t
    :doc "Emacs Mini-Buffer Actions Rooted in Keymaps"
    :bind
    (("C-." . embark-act)         ; pick some comfortable binding
     ("C-;" . embark-dwim)        ; good alternative: M-.
     ("C-h B" . embark-bindings)) ; alternative for `describe-bindings'
    :defun embark-prefix-help-command
    ;; Optionally replace the key help with a completing-read interface
    :pre-setq (prefix-help-command . #'embark-prefix-help-command)
    :init
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))

    (leaf embark-consult
      :ensure t
      :doc "consult embark binding package"
      :after consult
      ;; :demand t ; only necessary if you have the hook below
      :leaf-defer nil
      ;; if you want to have consult previews as you move around an
      ;; auto-updating embark collect buffer
      :hook (embark-collect-mode-hook . consult-preview-at-point-mode))
    )

  (leaf savehist
    :tag "builtin"
    :doc "Persist minibuffer history over Emacs restarts."
    :custom `((savehist-file . ,(cache-sub-file "history")))
    :init (savehist-mode))

  (leaf vertico
    :ensure t
    :doc "VERTical Interactive COmpletion"
    :defun vertico-mode crm-indicator
    :bind (:vertico-map
           ("M-j"    . next-history-element)
           ("M-k"    . previous-history-element)
           ("C-u"    . vertico-scroll-down)
           ("C-d"    . vertico-scroll-up)
           ("[home]" . vertico-first)
           ("[end]"  . vertico-last))
    :init (vertico-mode)

    ;; Add prompt indicator to `completing-read-multiple'.
    ;; Alternatively try `consult-completing-read-multiple'.
    (defun crm-indicator (args)
      (cons (concat "[CRM] " (car args)) (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    (leaf *hide-commads-unnecessary :emacs>= "28"
      :doc
      "Emacs 28: Hide commands in M-x which do not work in the current mode.
       Vertico commands are hidden in normal buffers."
      :setq
      (read-extended-command-predicate . #'command-completion-default-include-p))

    :custom
    (vertico-count . 15)
    (vertico-cycle . t)
    ;; Enable recursive minibuffers
    (enable-recursive-minibuffers . t)
    ;; Do not allow the cursor in the minibuffer prompt
    (minibuffer-prompt-properties . '(read-only t cursor-intangible t face minibuffer-prompt))
    :hook (minibuffer-setup-hook . cursor-intangible-mode))

  (leaf all-the-icons-completion :emacs>= "26.1" :disabled t
    :after all-the-icons marginalia
    :doc "Add icons to completion candidates using the built in completion metadata functions."
    :ensure t
    :hook
    (conf-on-term-hook . (lambda () (all-the-icons-completion-mode -1)))
    (conf-on-gui-hook  . (lambda () (all-the-icons-completion-mode +1)))
    )

  (leaf nerd-icons-completion :after nerd-icons marginalia
    :ensure t
    :global-minor-mode nerd-icons-completion-mode)
  )


(leaf *competion-linting
  :doc "completion and linting packages"
  :custom
  (completion-ignore-case . t)
  :config
  (leaf auto-complete
    :tag "builtin"
    :setq-default
    (ac-sources  . '(ac-source-filename
                     ac-source-words-in-all-buffer)))

  (leaf ispell :when (!executable-find "aspell")
    :tag "builtin"
    :commands ispell
    :setq-default
    (ispell-program-name     . "aspell")
    (ispell-local-dictionary . "en_US"))

  (leaf eldoc
    :tag "builtin"
    :commands eldoc
    :config
    (leaf eldoc-box
      :ensure t
      :defun eldoc-box-hover-mode
      :mode-hook (when (display-graphic-p) (eldoc-box-hover-mode))
      :custom
      (eldoc-box-only-multi-line    . nil)
      (eldoc-box-fringe-use-same-bg . t)
      (eldoc-box-clear-with-C-g     . t)
      (eldoc-box-cleanup-interval   . 0.5)
      :config
      (leaf *patch-eldoc-box-faces
        :preface
        (defsubst patch-eldoc-box-faces ()
          (custom-set-faces ;; modify bg to eldoc-box-body
           `(eldoc-box-body   ((t (:inherit 'default :background ,(doom-color 'bg-alt)))))
           `(eldoc-box-border ((t (:background ,(doom-color 'bg)))))))
        :hook
        (after-load-theme-hook . patch-eldoc-box-faces))
      )
    )

  (leaf flymake
    :tag "builtin"
    :emacs>= "26"
    :commands flymake-mode
    :hook (prog-mode-hook . flymake-mode)
    :bind
    (:flymake-mode-map ("M-j"   . flymake-goto-next-error)
                       ("M-k"   . flymake-goto-prev-error)
                       ("C-M-n" . flymake-goto-next-error)
                       ("C-M-p" . flymake-goto-prev-error)
                       ("C-M-l" . consult-flymake))
    :config
    (leaf flymake-collection :ensure t)
    (leaf flymake-diagnostic-at-point
      :ensure t
      :commands flymake-diagnostic-at-point-display-popup
      :custom
      (flymake-diagnostic-at-point-display-diagnostic-function . 'flymake-diagnostic-at-point-display-minibuffer)
      (flymake-diagnostic-at-point-error-prefix . ">> ")
      :hook
      (flymake-mode-hook . flymake-diagnostic-at-point-mode)))

  (leaf flycheck :disabled t
    :ensure t
    :bind
    (:flycheck-mode-map ("M-j" . flycheck-next-error)
                        ("M-k" . flycheck-previous-error))
    :config
    (leaf *patch-flycheck-faces
      :doc "on doom themes loaded"
      :preface
      (defsubst patch-flycheck-faces ()
        (custom-set-faces
         `(flycheck-warning ((t (:background ,(doom-color 'bg)))))
         `(flycheck-info    ((t (:background ,(doom-color 'bg)))))
         `(flycheck-error   ((t (:background ,(doom-color 'bg)))))
         ))
      :hook
      (after-load-theme-hook . patch-flycheck-faces)
      )


    (leaf flycheck-posframe
      :ensure t
      :mode-hook (flycheck-mode-hook . ((when (display-graphic-p)
                                          (flycheck-posframe-mode))))

      :custom
      `(
        (flycheck-posframe-position       . 'window-bottom-right-corner)
        (flycheck-posframe-info-prefix    . ,(format "%s " (nerd-icons-faicon "nf-fa-info")))
        (flycheck-posframe-warning-prefix . ,(format "%s " (nerd-icons-faicon "nf-fa-exclamation_triangle")))
        (flycheck-posframe-error-prefix   . ,(format "%s " (nerd-icons-faicon "nf-fa-exclamation_circle")))
        )
      )

    (leaf consult-flycheck
      :ensure t
      :bind
      (:flycheck-mode-map
       :package flycheck
       ("C-M-l" . consult-flycheck)))

    (leaf *flycheck-margin-faicon
      :doc "use faicon on flycheck margin string"
      :setq
      `((flycheck-default-margin-str . ,(nerd-icons-faicon "nf-fa-angle_double_right")))
      )
    )

  (leaf company :emacs< "27"
    :doc "company completion framework"
    :ensure t
    :defvar company-backends
    :custom
    (company-idle-delay . 0)
    (company-selection-wrap-around . t)
    (company-minimum-prefix-length . 1)
    (company-transformers . '(company-sort-by-occurrence
                              company-sort-by-backend-importance
                              company-sort-prefer-same-case-prefix))
    :config
    (leaf *add-yasnippet-backend
      :config
      (add-to-list 'company-backends 'company-yasnippet))
    (leaf company-box
      :ensure t
      :hook (company-mode-hook . company-box-mode))
    :global-minor-mode global-company-mode)

  (leaf corfu :emacs>= "27"
    :ensure t
    :commands corfu-mode
    :custom
    (corfu-cycle            . t)          ;; Enable cycling for `corfu-next/previous'
    (corfu-auto             . t)          ;; Enable auto completion
    (corfu-auto-prefix      . 2)
    (corfu-auto-delay       . 0.2)
    (corfu-separator        . ?\s)        ;; Orderless field separator
    (corfu-quit-no-match    . 'separator) ;; Never quit, even if there is no match
    (corfu-quit-at-boundary . 'separator) ;; Never quit at completion boundary
    (corfu-preview-current  . nil)        ;; Disable current candidate preview
    (corfu-preselect        . 'valid)     ;; Preselect the prompt
    (corfu-on-exact-match   . nil)        ;; Configure handling of exact matches
    (corfu-scroll-margin    . 5)          ;; Use scroll margin
    :hook
    ((text-mode-hook
      prog-mode-hook
      conf-mode-hook) . corfu-mode)
    :init
    (leaf *corfu-emacs-custom
      :custom
      (completion-cycle-threshold . 3)
      (tab-always-indent          . 'complete))

    :config

    (leaf *config-corfu-with-evil-map :disabled t
      :after evil
      :doc "this configure is not required anymore (done with `evil-collection-corfu-setup')"
      :url "https://github.com/minad/corfu/issues/12"
      :defvar corfu-map
      :preface
      (evil-make-overriding-map corfu-map)
      :advice
      (:after corfu--setup
              evil-normalize-keymaps)
      (:after corfu--teardown
              evil-normalize-keymaps))
    )

  (leaf *corfu-official-extensions :after corfu
    :doc "extensions provided from corfu"
    :load-path
    `(,(!expand-file-name
        "straight/repos/corfu/extensions"
        straight-base-dir))
    :config
    (leaf *corfu-popup-info
      :require corfu-popupinfo
      :custom
      (corfu-popupinfo-delay . 0.5)
      :global-minor-mode corfu-popupinfo-mode))

  (leaf *corfu-ui-config :after corfu
    :doc "corfu ui related configuration"
    :config
    (leaf kind-icon
      :doc "adds configurable icon or text-based completion prefixes based on the :company-kind property"
      :ensure t
      :custom (kind-icon-default-face . 'corfu-default)
      :defvar corfu-margin-formatters kind-icon-mapping
      :defun kind-icon-margin-formatter
      :config
      (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
      (leaf *kind-icon-with-nerd-icons
        :doc "use `nerd-icons' with corfu kind-icon"
        :url "https://github.com/rainstormstudio/nerd-icons.el#use-nerd-icons-with-corfu"
        :after nerd-icons
        :custom (kind-icon-use-icons . nil)
        :setq
        (kind-icon-mapping . `((array          ,(nerd-icons-codicon "nf-cod-symbol_array")       :face font-lock-type-face)
                               (boolean        ,(nerd-icons-codicon "nf-cod-symbol_boolean")     :face font-lock-builtin-face)
                               (class          ,(nerd-icons-codicon "nf-cod-symbol_class")       :face font-lock-type-face)
                               (color          ,(nerd-icons-codicon "nf-cod-symbol_color")       :face success)
                               (command        ,(nerd-icons-codicon "nf-cod-terminal")           :face default)
                               (constant       ,(nerd-icons-codicon "nf-cod-symbol_constant")    :face font-lock-constant-face)
                               (constructor    ,(nerd-icons-codicon "nf-cod-triangle_right")     :face font-lock-function-name-face)
                               (enummember     ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
                               (enum-member    ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
                               (enum           ,(nerd-icons-codicon "nf-cod-symbol_enum")        :face font-lock-builtin-face)
                               (event          ,(nerd-icons-codicon "nf-cod-symbol_event")       :face font-lock-warning-face)
                               (field          ,(nerd-icons-codicon "nf-cod-symbol_field")       :face font-lock-variable-name-face)
                               (file           ,(nerd-icons-codicon "nf-cod-symbol_file")        :face font-lock-string-face)
                               (folder         ,(nerd-icons-codicon "nf-cod-folder")             :face font-lock-doc-face)
                               (interface      ,(nerd-icons-codicon "nf-cod-symbol_interface")   :face font-lock-type-face)
                               (keyword        ,(nerd-icons-codicon "nf-cod-symbol_keyword")     :face font-lock-keyword-face)
                               (macro          ,(nerd-icons-codicon "nf-cod-symbol_misc")        :face font-lock-keyword-face)
                               (magic          ,(nerd-icons-codicon "nf-cod-wand")               :face font-lock-builtin-face)
                               (method         ,(nerd-icons-codicon "nf-cod-symbol_method")      :face font-lock-function-name-face)
                               (function       ,(nerd-icons-codicon "nf-cod-symbol_method")      :face font-lock-function-name-face)
                               (module         ,(nerd-icons-codicon "nf-cod-file_submodule")     :face font-lock-preprocessor-face)
                               (numeric        ,(nerd-icons-codicon "nf-cod-symbol_numeric")     :face font-lock-builtin-face)
                               (operator       ,(nerd-icons-codicon "nf-cod-symbol_operator")    :face font-lock-comment-delimiter-face)
                               (param          ,(nerd-icons-codicon "nf-cod-symbol_parameter")   :face default)
                               (property       ,(nerd-icons-codicon "nf-cod-symbol_property")    :face font-lock-variable-name-face)
                               (reference      ,(nerd-icons-codicon "nf-cod-references")         :face font-lock-variable-name-face)
                               (snippet        ,(nerd-icons-codicon "nf-cod-symbol_snippet")     :face font-lock-string-face)
                               (string         ,(nerd-icons-codicon "nf-cod-symbol_string")      :face font-lock-string-face)
                               (struct         ,(nerd-icons-codicon "nf-cod-symbol_structure")   :face font-lock-variable-name-face)
                               (text           ,(nerd-icons-codicon "nf-cod-text_size")          :face font-lock-doc-face)
                               (typeparameter  ,(nerd-icons-codicon "nf-cod-list_unordered")     :face font-lock-type-face)
                               (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered")     :face font-lock-type-face)
                               (unit           ,(nerd-icons-codicon "nf-cod-symbol_ruler")       :face font-lock-constant-face)
                               (value          ,(nerd-icons-codicon "nf-cod-symbol_field")       :face font-lock-builtin-face)
                               (variable       ,(nerd-icons-codicon "nf-cod-symbol_variable")    :face font-lock-variable-name-face)
                               (t              ,(nerd-icons-codicon "nf-cod-code")               :face font-lock-warning-face)))))

    (leaf *corfu-fussy-integration :after fussy
      :url "https://github.com/jojojames/fussy#corfu-integration"
      :advice ;; For cache functionality.
      (:before corfu--capf-wrapper
               fussy-wipe-cache)
      :mode-hook
      (corfu-mode-hook . ((setq-local fussy-max-candidate-limit 5000
                                      fussy-default-regex-fn 'fussy-pattern-first-letter
                                      fussy-prefer-prefix nil))))

    (leaf *corfu-ui-on-term
      :preface
      (leaf corfu-terminal :after corfu
        :doc "`corfu' popup on terminal"
        :straight
        (corfu-terminal
         :type git
         :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
      :defun corfu-terminal-mode
      :defvar corfu-mode
      :mode-hook
      (corfu-mode-hook   . ((if (display-graphic-p)
                                (corfu-terminal-mode -1)
                              (corfu-terminal-mode +1))))
      (conf-on-term-hook . ((dolist (buff (buffer-list))
                              (with-current-buffer buff
                                (when (and corfu-mode
                                           (not (display-graphic-p)))
                                  (corfu-terminal-mode +1))))))
      (conf-on-gui-hook  . ((dolist (buff (buffer-list))
                              (with-current-buffer buff
                                (when (and corfu-mode
                                           (display-graphic-p))
                                  (corfu-terminal-mode -1))))))
      ))
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  ;; :global-minor-mode global-corfu-mode

  (leaf lsp-mode
    :doc "configure lsp"
    :ensure t
    :commands (lsp lsp-deferred)
    :custom
    `(
      (lsp-keymap-prefix                      . "C-c C-l") ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l", "s-l" )
      (lsp-document-sync-method               . nil) ;; always send incremental document
      (lsp-response-timeout                   . 5)
      (lsp-eldoc-enable-hover                 . nil)
      (lsp-auto-configure                     . t)
      (lsp-headerline-breadcrumb-icons-enable . nil)
      (lsp-session-file                       . ,(cache-sub-file ".lsp-session-v1"))
      (lsp-server-install-dir                 . ,(cache-sub-dir "lsp")) ;; server install target path
      (lsp-completion-provider                . :none) ;; configured with corfu
      )
    :preface (setenv "LSP_USE_PLISTS" "true") ; use `plist' instead of `has-table'
    :bind (:lsp-mode-map ("C-c r"   . lsp-rename))
    :defvar lsp-prefer-flymake
    :defun flycheck-posframe-mode eldoc-box-hover-at-point-mode
    :mode-hook
    (lsp-mode-hook . ((eldoc-box-hover-at-point-mode -1)
                      (flycheck-posframe-mode -1))) ; disable flycheck-posframe
    :setq (lsp-prefer-flymake . t)
    :config
    (leaf lsp-ui
      :doc "LSP UI tools"
      :ensure t
      :custom
      ;; lsp-ui-doc
      (lsp-ui-doc-enable                 . t)
      (lsp-ui-doc-header                 . t)
      (lsp-ui-doc-include-signature      . t)
      (lsp-ui-doc-position               . 'top) ;; top, bottom, or at-point
      (lsp-ui-doc-max-width              . 150)
      (lsp-ui-doc-max-height             . 30)
      (lsp-ui-doc-use-childframe         . t)
      (lsp-ui-doc-use-webkit             . nil)
      (lsp-ui-doc-show-with-mouse        . t)
      (lsp-ui-doc-show-with-cursor       . t)
      ;; lsp-ui-flycheck
      (lsp-ui-flycheck-enable            . t)
      ;; lsp-ui-sideline
      (lsp-ui-sideline-enable            . t)
      (lsp-ui-sideline-ignore-duplicate  . t)
      (lsp-ui-sideline-show-symbol       . t)
      (lsp-ui-sideline-show-hover        . t)
      (lsp-ui-sideline-show-diagnostics  . t)
      (lsp-ui-sideline-show-code-actions . t)
      ;; lsp-ui-imenu
      (lsp-ui-imenu-enable               . nil)
      (lsp-ui-imenu-kind-position        . 'top)
      ;; lsp-ui-peek
      (lsp-ui-peek-enable                . t)
      (lsp-ui-peek-peek-height           . 20)
      (lsp-ui-peek-list-width            . 50)
      (lsp-ui-peek-fontify               . 'on-demand) ;; never, on-demand, or always
      :defvar lsp-ui-doc-mode
      :defun
      (lsp-ui-doc--hide-frame
       lsp-ui-doc-mode)
      :preface
      (defun ladicle/toggle-lsp-ui-doc ()
        (interactive)
        (if lsp-ui-doc-mode
            (progn (lsp-ui-doc-mode -1)
                   (lsp-ui-doc--hide-frame))
          (lsp-ui-doc-mode 1)))
      :bind
      ((:lsp-mode-map
        ("C-c C-r" . lsp-ui-peek-find-references)
        ("C-c C-j" . lsp-ui-peek-find-definitions)
        ("C-c i"   . lsp-ui-peek-find-implementation)
        ("C-c m"   . lsp-ui-imenu)
        ("C-c s"   . lsp-ui-sideline-mode)
        ("C-c d"   . ladicle/toggle-lsp-ui-doc)
        ("C-c D"   . lsp-ui-doc-focus-frame))
       (:lsp-ui-doc-frame-mode-map
        ("C-c q"   . lsp-ui-doc-unfocus-frame)))
      :hook
      (lsp-mode-hook . lsp-ui-mode)
      )

    (leaf lsp-treemacs :after treemacs
      :ensure t
      :hook (lsp . lsp-treemacs)
      :bind (:global-map ("C-x t e" . lsp-treemacs-errors-list)
                         ("C-x t s" . lsp-treemacs-symbols))
      :defun lsp-treemacs-sync-mode
      :config (lsp-treemacs-sync-mode 1))

    (leaf lsp-origami :ensure t)

    (leaf lsp-ido :disabled t
      :after lsp
      :require lsp-ido)

    (leaf consult-lsp
      :ensure t
      :after lsp-mode
      :bind (:lsp-mode-map ("C-M-l" . consult-lsp-diagnostics)))
    ;; optionally
    ;; if you are helm user
    ;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
    ;; if you are ivy user
    ;;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
    ;; optionally if you want to use debugger
    ;;(use-package dap-mode)
    ;; (use-package dap-LANGUAGE) to load the dap adapter for your language
    :hydra
    ((hydra-lsp-functions
      (:hint nil)
      (format "\
                            ^^^%s lsp functions^^^
^^^^^^--------------------------------------------------------------------------------
 ^act on code^               ^find (ui peek)^            ^signature^
 _r_:   rename               _s r_: references           _s j_: next
 _f_:   format buffer        _s d_: definitions          _s k_: previous
 _F_:   format region        _s i_: implementation       _s q_: stop
 _o_:   organize imports     _s s_: workspace symbol     _s d_: toggle full doc
 ^ ^                         ^   ^                       _s a_: toggle auto activate
 ^consult^                   ^find^
 _c d_: lsp diagnostics      _s D_: definition           ^miscellaneous^
 _c s_: lsp symbols          _s R_: references           _C i_: install server
 ^   ^                       _s e_: declaration          _C D_: doctor
 ^diagnos (lsp-ui)^          ^   ^                       _C d_: disconnect
 _l_:   flycheck-list        _a_:   appearance           _C v_: version
" (nerd-icons-faicon "nf-fa-code"))
      ;; act on code
      ("r"   lsp-rename :exit t)
      ("f"   lsp-format-buffer :exit t)
      ("F"   lsp-format-region  :exit t)
      ("o"   lsp-organize-imports :exit t)
      ;; consult
      ("c d" consult-lsp-diagnostics)
      ("c s" consult-lsp-symbols)
      ;; diagnos
      ("l"   lsp-ui-flycheck-list)
      ;; find
      ("s r" lsp-ui-peek-find-references)
      ("s d" lsp-ui-peek-find-definitions)
      ("s i" lsp-ui-peek-find-implementation)
      ("s s" lsp-ui-peek-find-workspace-symbol)
      ("s D" lsp-find-definition)
      ("s R" lsp-find-references)
      ("s e" lsp-find-declaration)
      ;; sianature
      ("s j" lsp-signature-next)
      ("s k" lsp-signature-previous)
      ("s q" lsp-signature-stop)
      ("s d" lsp-signature-toggle-full-docs)
      ("s a" lsp-toggle-signature-auto-activate)
      ;; appearance
      ("a"   hydra-lsp-appearance/body :exit t)
      ;; misc
      ("C i" lsp-install-server)
      ("C d" lsp-disconnect)
      ("C D" lsp-doctor)
      ("C v" lsp-version))

     (hydra-lsp-appearance
      (:hint nil)
      (format "\
                            ^^^%s lsp functions^^^
^^^^^^--------------------------------------------------------------------------------
 ^ ^                         ^len^                       ^treemacs^
 _s_   :sideline mode        _l l_ :lens mode            _t s_ lsp symbols
 _m_   :imenu                _l h_ :hide                 _t r_ lsp references
 ^ ^                         _l s_ :show                 _t e_ lsp errors list
 ^lsp ui doc^                ^   ^                       _t i_ lsp implementations
 _d d_ :toggle               ^   ^                       _t t_ lsp type hierarchy
 _d f_ :focus frame          ^   ^                       _t c_ lsp call hierarchy
 _d g_ :glance

 _SPC_ :BACK
" (nerd-icons-faicon "nf-fa-code"))
      ;; appearance
      ("s"   lsp-ui-sideline-mode)
      ("m"   lsp-ui-imenu)
      ("d d" ladicle/toggle-lsp-ui-doc)
      ("d f" lsp-ui-doc-focus-frame)
      ("d g" lsp-ui-doc-glance)
      ("l l" lsp-lens-mode)
      ("l h" lsp-lens-hide)
      ("l s" lsp-lens-show)
      ;; treemacs
      ("t s" lsp-treemacs-symbols :exit t)
      ("t r" lsp-treemacs-references :exit t)
      ("t e" lsp-treemacs-errors-list :exit t)
      ("t c" lsp-treemacs-call-hierarchy :exit t)
      ("t t" lsp-treemacs-type-hierarchy :exit t)
      ("t i" lsp-treemacs-implementations :exit t)

      ("SPC" hydra-lsp-functions/body :exit t))
     )

    :bind
    (:global-map
     ("<f7>" . hydra-lsp-functions/body)
     ("M-7"  . hydra-lsp-functions/body))
    (:lsp-mode-map
     ("C-c C-s"   . consult-lsp-file-symbols)
     ("C-c C-S"   . consult-lsp-symbols))
    )

  (leaf eglot
    :ensure t
    :doc "another lsp client"
    :commands eglot
    :preface
    (leaf *eglot-with-eldoc-box
      :doc "use eldoc-box-mode if available"
      :mode-hook
      (eglot-managed-mode-hook . ((when (fboundp 'eldoc-box-hover-mode)
                                    (eldoc-box-hover-mode)))))
    (leaf consult-eglot :ensure t)
    :hydra
    ((hydra-eglot-functions
      (:hint nil)
      (format "\
                           ^^^%s eglot functions^^^
^^^^^^--------------------------------------------------------------------------------
 ^act on code^               ^find^                       ^consult^
 _r_:   rename               _s r_: references (xref)     _c s_: lsp symbols
 _f_:   format buffer        _s d_: declaration           ^miscellaneous^
 _F_:   format region        _s t_: type definition       _C s_: shutdown
 _o_:   organize imports     _s i_: implementation        _C S_: shutdown all
" (nerd-icons-faicon "nf-fa-code"))
      ;; act on code
      ("r"   eglot-rename :exit t)
      ("f"   eglot-format-buffer :exit t)
      ("F"   eglot-format         :exit t)
      ("o"   eglot-code-action-organize-imports :exit t)
      ;; consult
      ("c s" consult-eglot-symbols)
      ;; find
      ("s r" xref-find-references)
      ("s t" eglot-find-typeDefinition)
      ("s d" eglot-find-declaration)
      ("s i" eglot-find-implementation)
      ;; misc
      ("C s" eglot-shutdown)
      ("C S" eglot-shutdown-all)))
    :bind
    (:eglot-mode-map
     ("<f7>"        . hydra-eglot-functions/body)
     ("M-7"         . hydra-eglot-functions/body)
     ("C-c r"       . eglot-rename)
     ("C-c R"       . eglot-rename)
     ("C-c C-l g t" . eglot-find-typeDefinition)
     ("C-c C-l g d" . eglot-find-declaration)
     ("C-c C-l g i" . eglot-find-implementation)
     ("C-c C-s"     . consult-lsp-file-symbols)
     ("C-c C-S"     . consult-lsp-symbols))
    )

  (leaf cape :after corfu
    :doc "provides Completion At Point Extensions
 which can be used in combination with the Corfu or the default completion UI."
    :ensure t
    :config
    (leaf company
      :doc "required for company-yasnippet"
      :ensure t
      :defun
      company-yasnippet
      cape-company-to-capf cape-capf-super
      cape-dabbrev cape-file cape-dict cape-elisp-symbol cape-keyword
      cape-company-yasnippet
      cape-super-text cape-super-prog
      :config
      (defalias 'cape-company-yasnippet
        (cape-company-to-capf #'company-yasnippet))
      )

    (leaf *cape-capf-config
      :doc "configure completion-at-point-functions"
      :preface
      ;; define cape-super-*
      (defalias 'cape-super-text
        (cape-capf-super
         #'cape-dabbrev #'cape-file
         #'cape-dict))
      (defalias 'cape-super-prog
        (cape-capf-super
         #'cape-elisp-symbol #'cape-keyword))

      (defun rdm/set-capf-prog-conf ()
        (setq-local completion-at-point-functions
                    (list #'cape-company-yasnippet
                          #'cape-keyword
                          #'cape-super-text))
        )
      (defun rdm/set-capf-text-conf ()
        (setq-local completion-at-point-functions
                    (list #'cape-company-yasnippet
                          #'cape-super-text))
        )
      (defun rdm/set-capf-elisp-conf ()
        (setq-local completion-at-point-functions
                    (list #'cape-company-yasnippet
                          #'cape-super-prog
                          #'cape-super-text))
        )
      ;; lsp related packages
      (leaf *eglot-with-corfu :after eglot fussy
        :doc "corfu with eglot config"
        :url "https://github.com/minad/corfu/wiki"
        :defun cape-capf-buster eglot-completion-at-point cape-super-eglot
        :preface
        (defalias 'cape-super-eglot
          (cape-capf-buster
           (cape-capf-super
            #'eglot-completion-at-point
            #'cape-company-yasnippet
            #'cape-keyword)))

        ;; Option 1: Specify explicitly to use fussy for Eglot
        (add-to-list 'completion-category-overrides '(eglot (styles fussy)) t)
        ;; Option 2: Undo the Eglot modification of completion-category-defaults
        ;; (with-eval-after-load 'eglot
        ;;   (setq completion-category-defaults nil))

        (defun rdm/set-capf-eglot-prog ()
          (setq-local completion-at-point-functions
                      (list #'cape-super-eglot
                            #'cape-super-text)))

        :hook ;(eglot--pre-command-hook . ())
        (eglot-managed-mode-hook . rdm/set-capf-eglot-prog)
        )

      (leaf *lsp-mode-with-corfu :after lsp-mode
        :doc "corfu with lsp-mode config"
        :defun lsp-completion-at-point cape-super-lsp
        :preface
        (defalias 'cape-super-lsp
          (cape-capf-buster
           (cape-capf-super
            #'cape-company-yasnippet
            #'lsp-completion-at-point
            #'cape-keyword)))

        (defun rdm/set-capf-lsp-prog-conf ()
          (setq-local completion-at-point-functions
                      (list #'cape-super-lsp
                            #'cape-super-text)))
        :hook
        (lsp-completion-mode-hook . rdm/set-capf-lsp-prog-conf)
        )

      :hook
      ((prog-mode-hook
        conf-mode-hook)     . rdm/set-capf-prog-conf)
      (text-mode-hook       . rdm/set-capf-text-conf)
      (emacs-lisp-mode-hook . rdm/set-capf-elisp-conf)
      )
    )

  (leaf dap-mode :after lsp-mode
    :doc "debug adapter protocol package"
    :ensure t
    :require dap-hydra
    :bind
    (:global-map
     ("<f8>" . dap-hydra)
     ("M-8"  . dap-hydra))
    :custom
    `((dap-auto-configure-features . '(sessions locals breakpoints expressions repl controls tooltip))
      (dap-breakpoints-file        . ,(cache-sub-file ".dap-breakpoints")))
    :defun
    dap-mode dap-auto-configure-mode
    dap-ui-mode dap-ui-controls-mode
    :config
    (dap-mode 1)
    (dap-auto-configure-mode 1)
    (dap-ui-mode 1)
    (dap-ui-controls-mode 1))
  )


(leaf *network-related
  :config
  (leaf auth-source
    :tag "builtin"
    :custom
    (auth-source-cache-expiry . 7200)
    (auth-sources             . '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"))
    :init
    (defun rdm/deepl-api-key ()
      "API key for Deepl translation"
      (let ((auth-source-cache-expiry nil))
        (if-let* ((key (auth-source-pick-first-password :host "api.deepl.com")))
            key (user-error "Key not found"))))
    (defun rdm/openai-api-key ()
      "API key for OpenAI API"
      (let ((auth-source-cache-expiry 86400))
        (if-let* ((key (auth-source-pick-first-password :host "api.openai.com")))
            key (user-error "Key not found")))))

  (leaf nsm
    :tag "builtin"
    :doc "network security manager"
    :custom `((nsm-settings-file . ,(cache-sub-file "network-security.data"))))

  (leaf request
    :ensure t
    :doc "easy http request client"
    :custom `((request-storage-directory . ,(cache-sub-dir "request"))))

  (leaf plz :ensure t
    :doc "HTTP library for Emacs")

  (leaf eww
    :tag "builtin"
    :doc "text base web browser"
    :commands eww
    :custom
    `((shr-width                   . 85)
      (shr-indentation             . 4)
      (shr-use-fonts               . t)
      (shr-max-image-proportion    . 0.6)
      (url-configuration-directory . ,(cache-sub-dir "url"))
      (eww-bookmarks-directory     . ,(cache-sub-dir "eww"))
      (eww-search-prefix           . "https://www.google.com/search?q=")
      (eww-bookmarks-directory     . ,(cache-sub-dir "eww")))
    :mode-hook
    (rdm/sw-lnsp 0.75)
    (rdm/text-scale-adjust)
    :init
    (leaf *eww-with-xwidget :emacs>= "29.1"
      :when (!sys-featurep "XWIDGETS")
      :doc "use xwidgets for audio/movie rendering"
      :custom (shr-use-xwidgets-for-media . nil))

    :config
    (leaf *eww-patch-faces
      :hook
      ((after-load-theme-hook
        eww-mode-hook) . (lambda ()
                           (custom-set-faces
                            `(eww-form-text ((t :background ,(doom-color 'bg-alt))))))))

    (leaf *eww-bind-nerd-icons
      :custom
      `((shr-bullet                        . ,(format "%s " (nerd-icons-octicon "nf-oct-square_fill")))
        (eww-form-checkbox-symbol          . ,(nerd-icons-faicon "nf-fa-square_o"))
        (eww-form-checkbox-selected-symbol . ,(nerd-icons-faicon "nf-fa-check_square_o"))))

    (leaf *eww-toggle-inhibit-images :emacs>= "28.1"
      :doc "disable image by default"
      :custom (shr-inhibit-images . t)
      :config
      (evil-define-key 'normal eww-mode-map
        "i" 'eww-toggle-images
        "I" 'eww-toggle-images))

    (leaf *eww-use-color-config
      :doc "config colors in eww"
      :custom (shr-use-colors . nil)
      :config
      (evil-define-key 'normal eww-mode-map
        "c" 'eww-toggle-colors
        "C" 'eww-toggle-colors))

    (leaf *patch-eww-heading-size
      :preface
      (defun patch-eww-heading-size ()
        (set-face-attribute 'shr-h1 nil :height 1.4)
        (set-face-attribute 'shr-h2 nil :height 1.2)
        (set-face-attribute 'shr-h3 nil :height 1.1)
        (set-face-attribute 'shr-h4 nil :height 1.0))
      :hook
      (eww-mode-hook . patch-eww-heading-size)))

  (leaf xwidget-webkit :when (!sys-featurep "XWIDGETS")
    :tag "builtin"
    :commands xwidget-webkit-browse-url
    :config
    (evil-define-key nil xwidget-webkit-mode-map
      (kbd "SPC") 'evil-collection-xwidget-webkit-scroll-half-page-up))
  )


(leaf *helpful-things
  :config
  (leaf ibuffer
    :tag "builtin"
    :custom
    (ibuffer-default-sorting-mode . 'filename/process)
    :bind
    (:global-map ("C-x C-b" . ibuffer))
    :config
    (leaf nerd-icons-ibuffer
      :ensure t
      :hook (ibuffer-mode-hook . nerd-icons-ibuffer-mode)))

  (leaf rainbow-mode
    :ensure t
    :commands rainbow-mode
    :doc "preview colors from hex values")

  (leaf ace-window
    :ensure t
    :commands ace-window
    :custom (aw-keys . '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    :custom-face ;; larger leading char
    (aw-leading-char-face .
                          '((t (:inherit 'highlight :height 5.0 :width normal :underline t))))
    )

  (leaf ace-link
    :ensure t
    :doc "quickly follow links"
    :commands ace-link
    :bind ("M-O" . ace-link)
    :defun ace-link-setup-default
    :config (ace-link-setup-default))

  (leaf minimap
    :ensure t
    :commands (minimap-bufname
               minimap-create
               minimap-kill)
    :custom
    (minimap-major-modes     . '(prog-mode))
    (minimap-window-location . 'right)
    (minimap-update-delay    . 0.20)
    (minimap-minimum-width   . 15))

  (leaf which-key
    :ensure t
    :commands which-key-mode
    :config
    (leaf which-key-posframe
      :ensure t
      :custom
      (which-key-posframe-poshandler . 'posframe-poshandler-window-center)
      (which-key-posframe-parameters . '((line-spacing . 0)))
      :defvar which-key-posframe-mode
      :defun
      which-key-posframe-mode
      which-key-setup-minibuffer
      which-key-setup-side-window-right-bottom
      :config
      (leaf *which-key-display-hook-on-state
        :doc "prefer posframe on gui"
        :hook
        (conf-on-term-hook . (lambda ()
                               (when which-key-posframe-mode (which-key-posframe-mode -1))
                               (which-key-setup-side-window-right-bottom)))
        (conf-on-gui-hook  . (lambda ()
                               (which-key-setup-minibuffer)
                               (which-key-posframe-mode +1)))
        )
      (leaf *patch-which-key-posframe-faces
        :preface
        (defsubst patch-which-key-posframe-faces ()
          (custom-set-faces
           `(which-key-posframe        ((nil (:background ,(doom-color 'bg-alt)))))
           `(which-key-posframe-border ((nil (:background ,(doom-color 'fg-alt)))))))
        :hook
        (after-load-theme-hook . patch-which-key-posframe-faces)))
    :mode-hook (after-init-hook . ((which-key-mode)))
    )

  (leaf transient
    :ensure t
    :doc "required by magit, git-timemachine"
    :custom
    `((transient-save-history . t)
      (transient-levels-file  . ,(!expand-file-name "levels.el"  (cache-sub-dir "transient")))
      (transient-values-file  . ,(!expand-file-name "values.el"  (cache-sub-dir "transient")))
      (transient-history-file . ,(!expand-file-name "history.el" (cache-sub-dir "transient")))))

  (leaf *undo-fu-setup
    :config
    (leaf undo-fu
      :ensure t
      :custom (evil-undo-system . 'undo-fu))

    (leaf undo-fu-session
      :ensure t
      :custom
      `((undo-fu-session-directory                . ,(cache-sub-dir "undo-fu-session"))
        (undo-fu-session-incompatible-files       . '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
        (undo-fu-session-incompatible-major-modes . '(comint-mode image-mode term-mode)))
      :global-minor-mode undo-fu-session-global-mode)

    (leaf vundo
      :ensure t
      :defvar vundo-unicode-symbols
      :custom
      (vundo-window-side . 'top)
      (vundo-glyph-alist . vundo-unicode-symbols)
      :bind ("C-x u" . vundo)))

  (leaf undo-tree :disabled t
    :ensure t
    :commands (undo-tree-mode turn-on-undo-tree-mode global-undo-tree-mode)
    :custom
    `((undo-tree-visualizer-timestamps   . t)
      (undo-tree-auto-save-history       . t)
      (undo-tree-visualizer-diff         . t)
      (undo-tree-history-directory-alist . `(("[^\\.cache\\/emacs\\/.*]" . ,(cache-sub-dir "undo-tree")))))
    :hook
    ((prog-mode-hook
      conf-mode-hook
      text-mode-hook) . turn-on-undo-tree-mode))

  (leaf dashboard
    :ensure t
    :custom
    `((dashboard-banner-logo-title     . ,(format "Welcome to Emacs %s! [on %s (%s)]"
                                                  emacs-version
                                                  system-name
                                                  system-type)) ;; Set the title
      (dashboard-startup-banner        . 3)
      (dashboard-center-content        . t)
      (dashboard-show-shortcuts        . t)
      (dashboard-set-heading-icons     . t)
      (dashboard-set-file-icons        . t)
      (dashboard-set-init-info         . t)
      (dashboard-set-footer            . t)
      (dashboard-bookmarks-item-format . "%.20s - %.40s")
      (dashboard-match-agenda-entry    . "-agenda_ignore")
      (dashboard-items                 . '((agenda    . 5)
                                           (recents   . 5)
                                           (bookmarks . 5)
                                           (projects  . 5)
                                           (registers . 5)))
      (initial-buffer-choice           . (lambda ()
                                           (dashboard-refresh-buffer)
                                           (get-buffer "*dashboard*"))))
    :preface
    (defun dashboard-banner-selector (&rest _args)
      "choose banner before refreshing dashboard"
      (let ((banner-img (!expand-file-name "banner.png" user-emacs-directory)))
        (if (and (file-regular-p banner-img)
                 (display-graphic-p))
            (customize-set-variable 'dashboard-startup-banner banner-img)
          (customize-set-variable 'dashboard-startup-banner 3))))
    :advice
    (:before dashboard-refresh-buffer
             dashboard-banner-selector)
    :bind ("<f12>" . dashboard-refresh-buffer))

  (leaf calendar
    :tag "builtin"
    :custom
    `((calendar-month-name-array   . ["01" "02" "03" "04" "05" "06"
                                      "07" "08" "09" "10" "11" "12" ])
      (calendar-week-start-day     . 0)
      (calendar-mark-holidays-flag . t)
      (diary-file                  . ,(cache-sub-file "diary")))
    :hook
    (calendar-today-visible-hook   . calendar-mark-today)
    :config
    (leaf *patch-calendar-faces
      :preface
      (defsubst patch-calendar-faces ()
        (custom-set-faces
         `(holiday
           ((t (:foreground ,(doom-color 'orange) :background ,(doom-color 'bg)))))
         `(calendar-today
           ((t (:foreground ,(doom-color 'base3) :background ,(doom-color 'green) :underline nil))))))
      :hook
      (after-load-theme-hook . patch-calendar-faces))

    (leaf japanese-holidays
      :ensure t
      :custom
      `((japanese-holiday-weekend         . '(0 6))
        (japanese-holiday-weekend-marker  . '(holiday             ;; sun
                                              nil nil nil nil nil ;; mon - fri
                                              japanese-holiday-saturday)))
      :hook
      ((calendar-today-visible-hook   . japanese-holiday-mark-weekend)
       (calendar-today-invisible-hook . japanese-holiday-mark-weekend))
      :defvar
      (japanese-holidays
       holiday-local-holidays
       calendar-holidays)
      :config
      (setq calendar-holidays (append japanese-holidays
                                      holiday-local-holidays))

      (leaf *patch-calendar-face
        :preface
        (defsubst patch-japanese-holiday-saturday ()
          (custom-set-faces
           `(japanese-holiday-saturday
             ((nil (:foreground ,(doom-color 'blue) :background ,(doom-color 'bg)))))))
        :hook
        (after-load-theme-hook . patch-japanese-holiday-saturday))
      )
    )

  (leaf *online-doc-viewer
    :config
    (leaf info
      :tag "builtin"
      :doc "customize info-mode appearance (add line spacing, enlarge)"
      :commands info
      :preface (push #'Info-mode remap-font-to-doc-modes-list)
      :mode-hook
      (Info-mode-hook . ((rdm/sw-lnsp 0.75)
                         (rdm/text-scale-adjust)
                         (remap-font-to-doc)))
      :custom-face
      (Info-quoted . '((t (:inherit 'font-lock-constant-face))))
      :config
      ;; if on macOS and homebrew installed, add homebrew info directory
      (when (and (!system-type 'darwin)
                 (!executable-find "brew"))
        (add-to-list 'Info-directory-list
                     (!expand-file-name
                      "share/info"
                      (string-trim (shell-command-to-string "brew --prefix"))))))

    (leaf man
      :tag "builtin"
      :doc "view man"
      :commands man
      :defun Man-mode
      :preface (push #'Man-mode remap-font-to-doc-modes-list)
      :mode-hook
      (rdm/sw-lnsp 0.75)
      (rdm/text-scale-adjust)
      (remap-font-to-doc))

    (leaf woman
      :tag "builtin"
      :doc "view man pages without external program"
      :commands woman
      :defun woman-mode
      :defvar woman-manpath
      :preface (push #'woman-mode remap-font-to-doc-modes-list)
      :custom
      (woman-fill-column    . 75)
      (woman-default-indent . 7)
      :custom-face
      (woman-italic . '((t (:italic t :underline t))))
      :mode-hook
      (rdm/sw-lnsp 0.75)
      (rdm/text-scale-adjust)
      (remap-font-to-doc)
      :config
      ;; if on macOS and homebrew installed, add homebrew man directory
      (when (and (!system-type 'darwin)
                 (!executable-find "brew"))
        (add-to-list 'woman-manpath
                     (!expand-file-name
                      "share/man"
                      (string-trim (shell-command-to-string "brew --prefix"))))))
    )

  (leaf eshell
    :tag "builtin"
    :commands eshell
    :custom
    `((eshell-directory-name    . ,(cache-sub-dir     "eshell"))
      (eshell-history-file-name . ,(!expand-file-name "history" (cache-sub-dir "eshell")))))

  (leaf vterm
    :ensure t
    :preface ; release keys for binding
    (global-unset-key [f2])
    (global-unset-key "\M-2")
    :commands vterm
    :mode-hook
    (vterm-mode-hook . ((rdm/sw-lnsp 1)
                        (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
                        (buffer-face-mode t)))
    :custom
    (vterm-max-scrollback     . 10000)
    (vterm-buffer-name-string . "vterm: %s")
    :config
    (evil-set-initial-state 'vterm-mode 'emacs)
    (defun rdm/vterm-new-buffer-in-current-window()
      "new vterm on current window"
      (interactive)
      (let ((display-buffer-alist nil)) (vterm)))
    (leaf vterm-toggle
      :ensure t
      :commands vterm-toggle
      :custom
      (vterm-toggle-fullscreen-p . nil)
      (vterm-toggle-scope-p . 'project)
      :config
      (add-to-list 'display-buffer-alist
                   '((lambda(bufname _) (with-current-buffer bufname (eq major-mode 'vterm-mode)))
                     (display-buffer-reuse-window display-buffer-at-bottom)
                     ;;(display-buffer-reuse-window display-buffer-in-direction)
                     ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                     (direction . bottom)
                     (lambda() (when (version<= "27" emacs-version) (dedicated . t)))
                     ;;(dedicated . t) ;dedicated is supported in emacs27
                     (reusable-frames . visible)
                     (window-height . 0.25))))
    (defun rdm/vterm-toggle-full ()
      "vterm toggle into full screen"
      (interactive)
      (let ((vterm-toggle-fullscreen-p t)) (vterm-toggle)))
    :bind
    (("M-2"   . vterm-toggle)
     ("<f2>"  . vterm-toggle)
     ("C-M-2" . rdm/vterm-toggle-full)
     ("M-@"   . rdm/vterm-new-buffer-in-current-window))
    (:vterm-mode-map
     ("C-l"     . (lambda () (interactive) (vterm-send-key (kbd "C-l"))))
     ("C-u"     . (lambda () (interactive) (vterm-send-key (kbd "C-u"))))
     ("C-t"     . (lambda () (interactive) (vterm-send-key (kbd "C-t"))))
     ("C-c C-c" . (lambda () (interactive) (vterm-send-key (kbd "C-c"))))
     ("<Esc>"   . vterm-send-escape)
     ("C-c C-[" . vterm-send-escape)
     ("M-2"     . vterm-toggle)
     ("<f2>"    . vterm-toggle))
    :defun vterm-send-key)

  (leaf tramp
    :tag "builtin"
    :custom
    `(
      (tramp-default-method        . "ssh")
      (tramp-persistency-file-name . ,(cache-sub-file "tramp"))
      (tramp-verbose               . 2)
      ;; (tramp-chunksize             . 2000)
      )
    :config
    (leaf *tramp-customize :emacs>= "28"
      :custom
      (tramp-use-ssh-controlmaster-options . nil))

    )

  (leaf epa :emacs>= "27"
    :tag "builtin"
    :doc "gpg pinentry mode"
    :custom
    (epa-pinentry-mode . 'loopback))

  (leaf pdf-tools
    :doc "require to execute `pdf-tools-install'"
    :ensure t
    :commands
    (pdf-view-mode
     pdf-loader-install
     pdf-tools-install
     pdf-tools-install-noverify)
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :bind (:pdf-view-mode-map ("C-s" . ctrlf-forward-default))
    :setq-default (pdf-view-display-size . 'fit-page)
    :defun pdf-links-minor-mode pdf-annot-minor-mode
    :mode-hook
    (pdf-view-mode-hook . ((line-number-mode     -1)
                           (pdf-links-minor-mode +1)
                           (pdf-annot-minor-mode +1)))
    :custom (pdf-view-selection-style . 'glyph) ; NOTE: required for selecting non-english langs
    :config
    (leaf *switch-pdf-view-dark
      :doc "detect whether doom-theme bg color seems dark (to avoid crush)"
      :defun pdf-view-dark-minor-mode
      :preface
      (defun pdf-view-mode-switch-dark ()
        (if (rdm/frame-bg-dark-p)
            (pdf-view-dark-minor-mode +1)
          (pdf-view-dark-minor-mode -1)))
      (defun pdf-view-mode-switch-dark-with-theme-load ()
        (dolist (buff (buffer-list))
          (with-current-buffer buff
            (when (eq major-mode #'pdf-view-mode)
              (if (rdm/frame-bg-dark-p)
                  (pdf-view-dark-minor-mode +1)
                (pdf-view-dark-minor-mode -1))))))
      :hook
      (pdf-view-mode-hook    . pdf-view-mode-switch-dark)
      (after-load-theme-hook . pdf-view-mode-switch-dark-with-theme-load))
    ;; (pdf-tools-install t)  ; Standard activation command
    (pdf-loader-install) ; On demand loading, leads to faster startup time
    )

  (leaf *darwin-dictionary-integration :when (!system-type 'darwin)
    :doc "dictionary app integration on macOS"
    :defun macos-dict--lookup
    :config
    (defsubst macos-dict--lookup (word)
      "Lookup word with dictionary.app by apple."
      (call-process "open" nil 0 nil (concat "dict://" word) "-g"))

    (defun macos-dict-lookup-word ()
      "Lookup the word at point with dictionary.app by apple."
      (interactive)
      (macos-dict--lookup (read-from-minibuffer "dictionary.app: " (current-word))))

    (leaf *monokakido-dict :when (monokakido--app-installed-p)
      :doc "use Disctionaries.app by monokakido (based on gist url)"
      :url "https://gist.github.com/skoji/aad5f66cbffc370e29888671e0801c6d"
      :defun monokakido--lookup
      :preface
      (eval-when-compile
        (defmacro monokakido--app-installed-p ()
          "if monokakido Dictionaries.app available, returns non-nil (runs on bytecomp)"
          (eval (string-match-p "Dictionaries.app"
                                (shell-command-to-string
                                 "mdfind 'kind:application' 'monokakido' 2>/dev/null")))))

      :config
      (defun monokakido--lookup (word)
        "Lookup word with Dictionaries.app by Monokakido."
        (call-process "open" nil 0 nil (concat "mkdictionaries:///?text=" word) "-g"))

      (defun monokakido-lookup-word ()
        "Lookup the word at point with Dictionaries.app by Monokakido."
        (interactive)
        (monokakido--lookup (read-from-minibuffer "Monokakido: " (current-word))))

      (evil-define-key '(normal visual) 'global
        (kbd "C-w f") 'monokakido-lookup-word
        (kbd "C-w F") 'macos-dict-lookup-word))

    (leaf *bind-macos-dict :unless (monokakido--app-installed-p)
      :doc "bind macos-dict-lookup-word to key with dictionary.app by apple"
      :config
      (evil-define-key '(normal visual) 'global
        (kbd "C-w f") 'macos-dict-lookup-word)))
  )


(leaf *load-conf-ext-fronts
  :doc "frontend of external apps"
  :config (!el-load "elisp/conf-ext-fronts"))


(leaf *git-related-packages
  :config
  (leaf git-gutter
    :ensure t
    :commands
    (global-git-gutter-mode git-gutter-mode)
    :custom
    `((git-gutter:modified-sign . "=")
      (git-gutter:added-sign    . "+")
      (git-gutter:deleted-sign  . ,(nerd-icons-faicon "nf-fa-minus"))
      (git-gutter:ask-p         . nil))
    :config
    (leaf *patch-git-gutter-color
      :preface
      (defsubst patch-git-gutter-color ()
        (custom-set-faces ;; modify bg fg
         `(git-gutter:modified ((t (:foreground ,(doom-color 'blue)  :background ,(doom-color 'blue)))))
         `(git-gutter:added    ((t (:foreground ,(doom-color 'base4) :background ,(doom-color 'green)))))
         `(git-gutter:deleted  ((t (:foreground ,(doom-color 'red)))))))
      :hook
      (after-load-theme-hook . patch-git-gutter-color)
      )
    :hydra
    (hydra-git-gutter
     (:hint nil)
     (format "\
                            ^^^%s git gutter^^^
^^^^^^--------------------------------------------------------------------------------
 ^move to hunk^              ^act on hunk^               ^etc^
^^^^^^................................................................................
 _j_:   next                 _d_:   popup                _r_:   update all windows
 _k_:   previous             _v_:   mark                 _b_:   magit blame
 _G_:   end of hunk          _s_:   stage                _h_:   open git timemachine
 ^ ^                         _x_:   revert      _<f3>_ / _M-3_:   open magit mode
" (nerd-icons-faicon "nf-fa-code_fork"))
     ("j" git-gutter:next-hunk)
     ("k" git-gutter:previous-hunk)
     ("G" git-gutter:end-of-hunk)
     ("d" git-gutter:popup-hunk)
     ("v" git-gutter:mark-hunk)
     ("x" git-gutter:revert-hunk)
     ("s" git-gutter:stage-hunk)
     ("b" magit-blame)
     ("r" git-gutter:update-all-windows)
     ("h" git-timemachine :exit t)
     ("<f3>" magit :exit t)
     ("M-3"  magit :exit t))

    :bind
    (:global-map
     ("<f3>" . hydra-git-gutter/body)
     ("M-3"  . hydra-git-gutter/body))
    :global-minor-mode (global-git-gutter-mode t))

  (leaf magit
    :ensure t
    ;:custom (magit-completing-read-function . 'magit-ido-completing-read)
    :mode-hook (magit-section-mode-hook . ((whitespace-mode -1)))
    :config
    (leaf magit-todos
      :ensure t
      :custom ;; TODO: add grouping via category
      (magit-todos-max-items     . 15)
      (magit-todos-ignore-case   . nil)
      (magit-todos-rg-extra-args . '("--hidden" "--no-config"))
      (magit-todos-exclude-globs . '(".git/" ".node_modules/" ".venv/"))
      :hook (magit-section-mode-hook . magit-todos-mode))

    (leaf *magit-git-gutter-integration
      :doc "refresh git-gutter on magit operation"
      :url "https://stackoverflow.com/questions/23344540/emacs-update-git-gutter-annotations-when-staging-or-unstaging-changes-in-magit"
      :defvar git-gutter-mode
      :mode-hook
      (magit-post-refresh-hook . ((dolist (buff (buffer-list))
                                    (with-current-buffer buff
                                      (when (and git-gutter-mode
                                                 (get-buffer-window buff))
                                        (git-gutter-mode t)))))))
    )

  (leaf git-timemachine
    :ensure t
    :commands git-timemachine git-timemachine-toggle)
  )


(leaf *file-manage
  :doc "file management related config"
  :config
  (leaf dired
    :tag "builtin"
    :preface
    (leaf osx-trash :when (!system-type 'darwin)
      :ensure t
      :commands osx-trash-setup
      :hook (after-init-hook . osx-trash-setup))
    :custom
    `(;; use gnu-ls if available on macOS
      (insert-directory-program  . ,(if (and (!system-type 'darwin)
                                             (!executable-find "gls"))
                                        "gls" "ls"))
      ;; set `dired-use-ls-dired' to nil with darwin ls commands
      (dired-use-ls-dired           . ,(if (and (not
                                                 (!executable-find "gls"))
                                                (!system-type 'darwin))
                                           nil "unspecified"))
      (dired-listing-switches    . ,(if (and (not
                                              (!executable-find "gls"))
                                             (!system-type 'darwin))
                                        "-lTh" ;; for BSD ls options
                                      (concat "-l --human-readable"
                                              " --sort=time --time-style=long-iso"
                                              " --group-directories-first --no-group")))

      (dired-dwim-target         . t)
      (delete-by-moving-to-trash . t)
      ;; NOTE: on macOS, require full disk access to see inside trash bin via dired
      (trash-directory           . ,(when (!system-type 'darwin) "~/.Trash")))
    :config
    (leaf async
      :doc "for dired async"
      :ensure t
      :global-minor-mode dired-async-mode)
    (ffap-bindings)
    ;; by selecting directory, do not creaete new dired buffer
    (put 'dired-find-alternate-file 'disabled nil))

  (leaf dirvish
    :when (or (not (!system-type 'darwin))
              (!executable-find "gls"))
    :ensure t
    :commands dirvish dirvish-override-dired-mode
    :mode-hook (dired-mode-hook . ((when (and window-system
                                              (fboundp 'pdf-tools-install))
                                     (pdf-tools-install t))))
    :hook (after-init-hook . dirvish-override-dired-mode)
    :custom
    `(
      ;; kill all session buffers on quit
      (dirvish-reuse-session       . nil)
      ;; cache
      (dirvish-cache-dir                  . ,(cache-sub-dir "dirvish"))
      (dirvish-media-auto-cache-threshold . '(500 . 4))
      ;; appearance
      (dirvish-default-layout       . '(0 0.2 0.8)) ;; no preview
      (dirvish-header-line-position . 'default)
      (dirvish-header-line-height   . '(25 . 25))
      (dirvish-header-line-format   . '(:left (path) :right (free-space)))
      (dirvish-mode-line-position   . 'default)
      (dirvish-mode-line-format     . '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
      ;; bookmarks TODO: add config for windows
      (dirvish-quick-access-entries . `(("h" ,(getenv "HOME")   "Home")
                                        ("d" ,(!expand-file-name "Downloads" (getenv "HOME")) "Downloads")
                                        ("D" ,(!expand-file-name "Documents"  (getenv "HOME")) "Documents")
                                        ("e" ,(!expand-file-name user-emacs-directory) "Emacs directory")
                                        ("m" ,(cond ((!system-type 'darwin)    "/Volumes/")
                                                    ((!system-type 'gnu/linux) "/mnt/")
                                                    (t "(not available)")) "Drives")
                                        ("t" ,(cond ((!system-type 'darwin)    (!expand-file-name "~/.Trash"))
                                                    ((!system-type 'gnu/linux) (!expand-file-name "~/.local/share/Trash/files/"))
                                                    (t "(not available)")) "TrashCan")))
      )

    :init
    (leaf *dirvish-on-state
      :doc "switch dirvish attributes"
      :custom
      `((dirvish-attributes           . `,(if (or (daemonp) (display-graphic-p))
                                              '(vc-state nerd-icons collapse git-msg file-time file-size)
                                            '(nerd-icons collapse git-msg file-time file-size)))
        (dirvish-side-attributes      . `,(if (or (daemonp) (display-graphic-p))
                                              '(vc-state nerd-icons collapse git-msg file-size)
                                            '(nerd-icons collapse git-msg file-size))))
      :mode-hook
      (dired-mode-hook . ((if (display-graphic-p)
                              (progn
                                (customize-set-variable
                                 'dirvish-attributes
                                 '(vc-state nerd-icons collapse git-msg file-time file-size))
                                (customize-set-variable
                                 'dirvish-side-attributes
                                 '(vc-state nerd-icons collapse git-msg file-size)))
                            (progn
                              (customize-set-variable
                               'dirvish-attributes
                               '(nerd-icons git-msg file-time file-size))
                              (customize-set-variable
                               'dirvish-side-attributes
                               '(nerd-icons git-msg file-size)))))))

    :config
    (leaf *dirvish-preview-config
      :doc "configure dirvish preview dispathcer"
      :custom
      `((dirvish-preview-dispatchers  . `(,(when (!executable-find "eza") 'eza)
                                          image gif video audio epub archive
                                          ,(when (featurep 'xwidget-internal) 'html)
                                          ,(cond ((fboundp  'pdf-loader-install) 'pdf)
                                                 ((featurep 'xwidget-internal)   'pdf-xwidget)
                                                 (t 'pdf-preface)))))
      :preface
      ;; load dirvish on compile to expand macros
      (eval-and-compile (leaf dirvish :ensure t :require t))

      ;; define preview
      (dirvish-define-preview eza (file)
        "Use `eza' to generate directory preview."
        :require ("eza") ; tell Dirvish to check if we have the executable
        (when (file-directory-p file) ; we only interest in directories here
          `(shell . ("eza" "--long" "--color=always" "--icons=always" "--no-quotes"
                     "--almost-all" "--group-directories-first"
                     "--smart-group" "--git" ,file))))

      ;; for html preview with xwidget
      (when (featurep 'xwidget-internal)
        (dirvish-define-preview html (file ext)
          "Use xwidgets for HTML file previews"
          (when (equal ext "html")
            (let ((url (concat "file://" file)))
              (xwidget-webkit-browse-url url)
              (evil-window-mru))))

        (dirvish-define-preview pdf-xwidget (file ext)
          "Use xwidgets for PDF file previews"
          (when (equal ext "pdf")
            (let ((url (concat "file://" file)))
              (xwidget-webkit-browse-url url)
              (evil-window-mru)))))
      )

    (leaf *dirvish-preview-hooks-mediainfo :when (!executable-find "mediainfo")
      :hook
      ((dirvish-image-preview-dp-hook
        dirvish-video-preview-dp-hook
        dirvish-gif-preview-dp-hook
        dirvish-audio-preview-dp-hook) . dirvish-media-properties))

    (evil-define-key 'normal dirvish-mode-map
      (kbd "TAB") 'dirvish-subtree-toggle
      (kbd "Q")   'dirvish-quit
      (kbd "q")   'dirvish-quick-access
      (kbd "l")   'dirvish-subtree-toggle
      (kbd "h")   'dirvish-subtree-menu
      ;; dirvish fd
      (kbd "ff")  'dirvish-fd
      (kbd "fj")  'dirvish-fd-jump
      (kbd "fr")  'dirvish-fd-roam
      (kbd "fk")  'dirvish-fd-kill
      (kbd ";")   'dirvish-layout-toggle
      ;; dirvish menus
      (kbd "Mf")  'dirvish-fd-switches-menu

      (kbd "MM")  'dirvish-chxxx-menu
      (kbd "MO")  'dirvish-chxxx-menu

      (kbd "Me")  'dirvish-emerge-menu
      (kbd "Mh")  'dirvish-dispatch
      (kbd "Mi")  'dirvish-file-info-menu
      (kbd "Ml")  'dirvish-ls-switches-menu
      (kbd "Mm")  'dirvish-mark-menu
      (kbd "Ms")  'dirvish-setup-menu
      (kbd "My")  'dirvish-yank-menu
      (kbd "MH")  'dirvish-history-menu

      (kbd "Md")  'dirvish-subdir-menu ;; dirvish subdir-menu
      (kbd "Mv")  'dirvish-vc-menu ;; dirvish vc-menu
      (kbd "Mg")  'dirvish-epa-dired-menu ;; dirvish epa-dired-menu
      (kbd "Mr")  'dirvish-renaming-menu ;; dirvish renaming-menu
      )
    )

  (leaf neotree :disabled t
    :ensure t
    ;; :bind ([f7] . neotree-toggle)
    :custom
    (neo-window-fixed-size . t)
    (neo-window-width      . 35)
    (neo-window-position   . 'right)
    :config
    ;; keymap for using with evil mode
    (evil-define-key 'normal neotree-mode-map
      (kbd "TAB") 'neotree-enter
      (kbd "SPC") 'neotree-quick-look
      (kbd "q")   'neotree-hide
      (kbd "RET") 'neotree-enter
      (kbd "g")   'neotree-refresh
      (kbd "n")   'neotree-next-line
      (kbd "p")   'neotree-previous-line
      (kbd "A")   'neotree-stretch-toggle
      (kbd "H")   'neotree-hidden-file-toggle))

  (leaf project
    :tag "builtin"
    :custom `((project-list-file . ,(cache-sub-file "projects"))))

  (leaf projectile
    :ensure t
    :defun projectile-mode
    :custom
    `((projectile-cache-file          . ,(cache-sub-file "projectile.cache" "projectile"))
      (projectile-known-projects-file . ,(cache-sub-file "projectile-known-projects.eld" "projectile")))
    :config (projectile-mode +1))

  (leaf treemacs
    :ensure t
    :defvar winum-keymap
    :commands treemacs
    :require t
    :init
    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))

    (leaf treemacs-nerd-icons
      :url "https://github.com/rainstormstudio/treemacs-nerd-icons"
      :ensure t
      :require t
      :mode-hook
      ;; NOTE: 常にロード
      (treemacs-mode-hook . ((treemacs-load-theme "nerd-icons"))))

    (leaf treemacs-all-the-icons :disabled t
      :after treemacs-themes all-the-icons
      :require t
      :ensure t
      :mode-hook
      (conf-on-term-hook  . ((treemacs-load-theme "Default")))
      (conf-on-gui-hook   . ((treemacs-load-theme "all-the-icons")))
      (treemacs-mode-hook . ((if (display-graphic-p)
                                 (treemacs-load-theme "all-the-icons")
                               (treemacs-load-theme "Default")))))

    (leaf *treemacs-custom
      :doc "eval after treemacs load"
      :defvar
      treemacs-last-period-regex-value
      treemacs-python-executable
      :custom
      `((treemacs-deferred-git-apply-delay      . 0.5)
        (treemacs-directory-name-transformer    . #'identity)
        (treemacs-display-in-side-window        . t)
        (treemacs-eldoc-display                 . t)
        (treemacs-file-event-delay              . 5000)
        (treemacs-file-extension-regex          . treemacs-last-period-regex-value)
        (treemacs-file-follow-delay             . 0.2)
        (treemacs-file-name-transformer         . #'identity)
        (treemacs-follow-after-init             . t)
        (treemacs-git-command-pipe              . "")
        (treemacs-goto-tag-strategy             . 'refetch-index)
        (treemacs-indentation                   . 2)
        (treemacs-indentation-string            . " ")
        (treemacs-is-never-other-window         . nil)
        (treemacs-max-git-entries               . 5000)
        (treemacs-missing-project-action        . 'ask)
        (treemacs-move-forward-on-expand        . nil)
        (treemacs-no-png-images                 . nil)
        (treemacs-no-delete-other-windows       . t)
        (treemacs-project-follow-cleanup        . nil)
        (treemacs-position                      . 'left)
        (treemacs-recenter-distance             . 0.1)
        (treemacs-recenter-after-file-follow    . nil)
        (treemacs-recenter-after-tag-follow     . nil)
        (treemacs-recenter-after-project-jump   . 'always)
        (treemacs-recenter-after-project-expand . 'on-distance)
        (treemacs-show-cursor                   . nil)
        (treemacs-show-hidden-files             . t)
        (treemacs-silent-filewatch              . nil)
        (treemacs-silent-refresh                . nil)
        (treemacs-sorting                       . 'alphabetic-asc)
        (treemacs-space-between-root-nodes      . t)
        (treemacs-tag-follow-cleanup            . t)
        (treemacs-tag-follow-delay              . 1.5)
        (treemacs-user-mode-line-format         . nil)
        (treemacs-user-header-line-format       . nil)
        (treemacs-width                         . 35)
        (treemacs-collapse-dirs                 . 3)
        ;; (treemacs-collapse-dirs                 . ,(if treemacs-python-executable 3 0))
        (treemacs-persist-file                  . ,(cache-sub-file "treemacs-persist"))))
    :bind
    (("M-0"       . treemacs-select-window)
     ("C-x t 1"   . treemacs-delete-other-windows)
     ("C-x t t"   . treemacs)
     ("C-x t B"   . treemacs-bookmark)
     ("C-x t C-t" . treemacs-find-file)
     ("C-x t M-t" . treemacs-find-tag))
    :defun
    (treemacs-git-mode
     treemacs-fringe-indicator-mode
     treemacs-filewatch-mode
     treemacs-follow-mode
     treemacs-load-theme)
    :config
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)
    (treemacs-follow-mode 0)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (!executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t) (treemacs-git-mode 'deferred))
      (`(t . _) (treemacs-git-mode 'simple)))

    ;; treemacs integrations
    (leaf treemacs-evil :after evil
      :doc "treemacs `evil' keybindings (part of treemacs)"
      :ensure t :require t
      ;; fix tab action
      :bind (:evil-treemacs-state-map ("TAB" . treemacs-TAB-action)))

    (leaf treemacs-magit :after magit
      :doc "treemacs `magit' integration (part of treemacs)"
      :ensure t :require t)

    (leaf treemacs-projectile :after projectile
      :doc "treemacs `projectile' integration (part of treemacs)"
      :ensure t :require t)

    (leaf treemacs-icons-dired :disabled t
      :doc "treemacs icons on dired (treemacs-all-the-icons, use all-the-icons)"
      :after treemacs dired
      :ensure t
      :config (treemacs-icons-dired-mode))
    ;;(use-package treemacs-persp ;;treemacs-persective if you use perspective.el vs. persp-mode
    ;;  :after treemacs persp-mode ;;or perspective vs. persp-mode
    ;;  :ensure t
    ;;  :config (treemacs-set-scope-type 'Perspectives))

    )
  )


(leaf *load-conf-elfeed
  :doc "load elfeed plugins if elfeed dir available"
  :config
  (!el-load "elisp/conf-elfeed"))


(leaf *load-major-modes
  :doc "load some major modes"
  :config
  (!el-load "elisp/conf-org"
            "elisp/conf-langs"))


(leaf *load-local-conf
  :config
  (let ((local-conf (!expand-file-name "elisp/local.el" user-emacs-directory))
        (straight-current-profile 'local))
    (unless (file-exists-p local-conf)
      (write-region "\
;;; local.el --- local configuration. -*-  no-byte-compile: t -*-
;;; Commentary:
;;
;; local config file
;;
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; local.el ends here
" nil local-conf))
    (load (file-name-sans-extension local-conf))))

;; reconfigure gc after init
(setq gc-cons-threshold (eval-when-compile (* 192 1024 1024)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
