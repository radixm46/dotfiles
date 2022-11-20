;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; emacs configuration by radixM491VA

;; ---------------  load package ---------------
(load "~/.emacs.d/elisp/initpkg.el")


(leaf *startup
  :config
  (leaf gcmh
    :doc "gcmh works after startup"
    :doc "Enforce a sneaky Garbage Collection strategy to minimize GC interference with user activity."
    :ensure t
    :hook (emacs-startup-hook . (lambda () (gcmh-mode 1)))
    )

  (leaf *mouse-support-on-term :unless (window-system)
    :doc "mouse support on terminal"
    :require mouse
    :setq (mouse-sel-mode . t)
    :config
    (xterm-mouse-mode t)
    (defun track-mouse (e)))

  (leaf cl-lib
    :tag "builtin"
    :require t
    :doc "load cl-lib")

  (leaf exec-path-from-shell
    :doc "load path from shell at startup"
    :ensure t
    :custom (exec-path-from-shell-arguments . '("-l"))
    :config (when (daemonp) (exec-path-from-shell-initialize)))
  )


(leaf *conf-cache-history
  :config
  (leaf autosave
    :doc "configure auto save files"
    :tag "builtin"
    :preface
    (leaf *disable-auto-save-message :emacs>= "27"
      :doc "supress auto saving message"
      :custom (auto-save-no-message . t))
    :custom
    `(
      (auto-save-file-name-transforms . `((".*" ,(expand-file-name "\\2" (cache-sub-dir "autosaved")) t)))
      (delete-auto-save-files         . t)
      (auto-save-default              . t)
      (auto-save-timeout              . 15)
      (auto-save-interval             . 120)
      (auto-save-list-file-prefix     . nil)
      ))

  (leaf *backup-files
    :doc "make backup files"
    :tag "builtin"
    :custom
    `(
      (backup-directory-alist         . `(("." .  ,(cache-sub-dir "backup"))))
      (auto-save-list-file-prefix     . ,(expand-file-name ".saves-"
                                          (cache-sub-dir "auto-save-list")))
      (auto-save-list-file-name)
      (make-backup-files              . t)
      (version-control                . t) ;; enable version control
      (kept-new-versions              . 5)
      (kept-old-versions              . 1)
      (delete-old-versions            . t)
      (create-lockfiles               . nil)
      ))

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
    `(
      (recentf-save-file       . ,(cache-sub-file "recentf"))
      (recentf-max-saved-items . 2000)
      (recentf-auto-cleanup    . 'never)
      (recentf-exclude         . '("/\\.cache/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:"
                                   "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/"))
      )
    :init
    (defmacro with-suppressed-message (&rest body)
      "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
      (declare (indent 0))
      (let ((message-log-max nil))
        `(with-temp-message (or (current-message) "") ,@body)))
    (setq recentf-auto-save-timer
          (run-with-idle-timer 30 t #'(lambda ()
                                       (with-suppressed-message (recentf-save-list)))))
    :global-minor-mode recentf-mode
    )

  (leaf bookmark
    :tag "builtin"
    :custom
    `((bookmark-default-file . ,(cache-sub-file "bookmarks")))
    )
  )


(leaf *lang-locales
  :doc "locale and encoding"
  :custom
  (default-buffer-file-coding-system . 'utf-8)
  (default-coding-systems            . 'utf-8)
  (coding-system-for-write           . 'utf-8)
  ;; (setq coding-system-for-read 'utf-8) ;; conflicts ddskk
  :config
  (set-locale-environment nil)
  (set-default 'buffer-file-coding-system 'utf-8)
  (set-language-environment "Japanese")
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  ;; skk config(use
  (leaf ddskk
    :doc "setup ddsk"
    :ensure t
    :custom
    `(
      (skk-use-azik         . t)
      (default-input-method . "japanese-skk")
      (skk-user-directory   . ,(cache-sub-dir "skk"))
      (skk-init-file        . ,(expand-file-name "elisp/initskk.el" user-emacs-directory))
      )
    )
  )


(leaf *font-configure
  :doc "font configure"
  :config
  (defvar font-for-tables "UDEV Gothic NF" "1:2 width font for table")

  (leaf nerd-fonts
    :doc "Emacs nerd-fonts utilities."
    :straight
    (nerd-fonts :type git :host github
                :repo "twlz0ne/nerd-fonts.el")
    :require t)

  (defun rdm/apply-func-in-fonts (fonts func)
    "apply func to available font in list"
    (let ((fonts-available (font-family-list)))
      (catch 'found
        (dolist (f fonts)
          (if (member f fonts-available)
              (progn (funcall func f)
                     (throw 'found f)))))))

  (leaf *hide-nobreak-whitespace :emacs>= "28"
    :doc "hack to disable face for double byte space"
    :setq (nobreak-char-display . nil))

  (leaf *set-variable-pitch-on-gui
    :doc "modify variable pitch face to available font in list"
    :hook
    (conf-on-gui-hook . (lambda ()
                          (rdm/apply-func-in-fonts
                           '("Hiragino Sans"
                             "BIZ UDP Gothic"
                             "Noto Sans CJK JP")
                           (lambda (f) (custom-set-faces `(variable-pitch  ((t (:family ,f))))))
                           ))))

  (defun rdm/available-serif-font()
    "returns serif font family available in list"
    (rdm/apply-func-in-fonts '("Hiragino Mincho ProN"
                               "BIZ UDP Mincho"
                               "Noto Serif CJK JP")
                             (lambda (f) '())))

  (leaf all-the-icons ;; need installation by all-the-icons-install-fonts
    :ensure t
    :config
    (leaf *patch-all-the-icons-fileicon
      :doc "use org icon to org_archive extension"
      :after all-the-icons
      :push
      ((all-the-icons-extension-icon-alist . '("org_archive" all-the-icons-fileicon "org" :face all-the-icons-lgreen)))
      )
    )
  (leaf emojify
    :ensure t
    :custom
    `(
      (emojify-emojis-dir . ,(cache-sub-dir "emojis"))
      (emojify-display-style . 'unicode)
      (emojify-composed-text-p . nil)
      )
    :config
    (leaf *emojify-init :emacs< "28"
      :custom (emojify-display-style . 'ascii)
      :hook (after-init-hook . global-emojify-mode))
    )
  )


(leaf *editor-functions
  :custom
  (eol-mnemonic-dos         . "(CRLF)")
  (eol-mnemonic-mac         . "(CR)")
  (eol-mnemonic-unix        . "(LF)")
  (find-file-visit-truename . t)
  (tab-width                . 4)
  (indent-tabs-mode         . nil)
  (truncate-lines           . t)
  ;; configure indent tab to false as default
  ;; (setq line-move-visual t)
  ;; (setq word-wrap t)
  :init
  (leaf visual-line-mode
    :doc "visual line mode enables word wrap"
    :tag "builtin"
    :hook
    ((org-mode-hook
      markdown-mode-hook) . visual-line-mode)
    ;; (global-visual-line-mode nil)
    )

  (leaf rg
    :doc "Use ripgrep in Emacs. (binaries included)"
    :ensure t
    :config
    (rg-enable-default-bindings))

  (leaf ctrlf
    :doc "an intuitive and efficient solution for single-buffer text search in Emacs"
    :ensure t
    :config
    (ctrlf-mode +1))

  (leaf ediff
    :ensure t
    :tag "builtin"
    :custom (ediff-window-setup-function . #'ediff-setup-windows-plain)
    :config
    (defun ediff-window-display-p ()
      "overwritten ediff builtin function, for not generate new frame" nil)
    )

  (leaf evil
    :ensure t
    :pre-setq (evil-want-keybinding . nil)
    :custom (evil-undo-system . 'undo-tree)
    :hydra
    (hydra-manage-windows
     (:hint nil)
     (format "\
                              ^^^^^^^%s managing windows^^^^^^^^^^
^^^^^^^^^^^^^^^^^^^--------------------------------------------------------------------------------
     ^^select^^            ^^^move^^^               ^^^resize^^^               ^^^split^
       ^_k_^            _R_    _K_    _r_          ^ ^   _+_   ^ ^            ^ ^  _v_
       ^^↑^^            ^ ^    ^↑^    ^ ^          ^ ^   ^+^   ^ ^            ^ ^  ^|^
   _h_ ←   → _l_         ^_H_ ←   → _L_^           _<_ - _=_ + _>_           ^_s_ --+--^
       ^^↓^^            ^ ^    ^↓^    ^ ^          ^ ^   ^-^   ^ ^            ^ ^  ^|^
       ^_j_^            ^ ^    _J_    ^ ^          ^ ^   _-_   ^ ^            ^ ^
^^^^^^^^^^^^^^^^^^^................................................................................
    ^_p_revious^            ^^_n_ew^^               ^^_d_elete^^           ^_N_ext^ / ^_P_revious tab^
   ^_a_ce window^        ^^other _f_rame^^        ^^delete _o_ther^^        ^_t_:  tab-bar keys
" (nerd-fonts "fa-window-maximize"))
     ("h" evil-window-left)
     ("j" evil-window-down)
     ("k" evil-window-up)
     ("l" evil-window-right)
     ("a" ace-window)
     ("H" evil-window-move-far-left)
     ("J" evil-window-move-very-bottom)
     ("K" evil-window-move-very-top)
     ("L" evil-window-move-far-right)
     ("r" evil-window-rotate-downwards)
     ("R" evil-window-rotate-upwards)
     ("+" (evil-window-increase-height 5))
     ("-" (evil-window-decrease-height 5))
     (">" (evil-window-increase-width 5))
     ("<" (evil-window-decrease-width 5))
     ("=" balance-windows)
     ("v" evil-window-vsplit)
     ("s" evil-window-split)
     ("p" evil-window-mru)
     ("d" evil-window-delete)
     ("o" delete-other-windows :exit t)
     ("n" evil-window-new)
     ("f" other-frame)
     ("N" tab-bar-switch-to-next-tab)
     ("P" tab-bar-switch-to-prev-tab)
     ("t" hydra-tab-bar/body :exit t))

    :bind
    (:global-map
     ("<f4>" . hydra-manage-windows/body)
     ("M-4"  . hydra-manage-windows/body))
    :global-minor-mode evil-mode
    :config
    (leaf evil-surround
      :ensure t
      :global-minor-mode global-evil-surround-mode)
    (leaf evil-collection
      :after evil
      :ensure t
      :config
      (evil-collection-init)
      (define-key evil-normal-state-map (kbd "C-u") 'scroll-down-command)
      ;; enable emacs cursor movement in insert mode
      (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
      (define-key evil-insert-state-map (kbd "C-f") 'forward-char)
      (define-key evil-insert-state-map (kbd "C-b") 'backward-char)
      ;; (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
      ;; (define-key evil-insert-state-map (kbd "C-n") 'next-line)
      )
    (leaf evil-commentary :ensure t
      :doc "make easy to comment out."
      :url "https://github.com/linktohack/evil-commentary"
      :config (evil-commentary-mode))
    (leaf evil-goggles :ensure t
      :doc "visual hints on edit."
      :url "https://github.com/edkolev/evil-goggles"
      :custom
      (evil-goggles-pulse             . nil)
      (evil-goggles-duration          . 0.10)
      (evil-goggles-async-duration    . nil)
      (evil-goggles-blocking-duration . nil)
      :config
      (evil-goggles-mode)
      (evil-goggles-use-diff-faces)
      )
    (leaf evil-lion :ensure t
      :doc "Evil align operator."
      :url "https://github.com/edkolev/evil-lion"
      :config (evil-lion-mode))
    (leaf evil-matchit :ensure t
      :doc "Vim matchit.vim by Benji Fisher is ported into Emacs."
      :url "https://github.com/redguardtoo/evil-matchit"
      :global-minor-mode global-evil-matchit-mode)
    (leaf evil-numbers :ensure t
      :doc "Increment / Decrement binary, octal, decimal and hex literals"
      :url "https://github.com/cofi/evil-numbers"
      :config
      (evil-define-key 'motion 'global
        (kbd "C-c +") 'evil-numbers/inc-at-pt
        (kbd "C-c -") 'evil-numbers/dec-at-pt
        ))
    )

  (leaf editorconfig
    :doc "load editorconfig"
    :ensure t
    :config (editorconfig-mode))

  (leaf origami
    :doc "folding blocks"
    :ensure t
    :hook (prog-mode-hook . origami-mode))

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
       ("M-g M-s" . consult-yasnippet)))
    )

  (leaf tab-bar :emacs>= "27.1"
    :tag "builtin"
    :doc "configure tab-bar mode"
    :custom (tab-bar-show . 1)
    :hydra
    (hydra-tab-bar
     (:hint nil)
     (format "\
                       ^^%s tab-bar-mode^^
^^^^-----------------------------------------------------------------
 _n_:   new                       _j_:   next
 _c_:   close                     _k_:   previous
 _r_:   rename                    _s_:   select

 _SPC_: BACK
" (nerd-fonts "fa-window-maximize"))
     ("n"   tab-bar-new-tab)
     ("c"   tab-bar-close-tab)
     ("r"   tab-rename)
     ("j"   tab-bar-switch-to-next-tab)
     ("k"   tab-bar-switch-to-prev-tab)
     ("s"   tab-bar-select-tab-by-name)
     ("SPC" hydra-manage-windows/body :exit t))
    )

  (leaf quickrun :ensure t)

  (leaf tree-sitter
    :ensure t
    :commands (tree-sitter-hl-mode)
    :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
    :init
    (leaf tree-sitter-langs
      :ensure t)
    :global-minor-mode global-tree-sitter-mode
    )

  (leaf highlight-symbol
    :ensure t
    :doc "automatic and manual symbol highlighting for Emacs"
    :custom
    (highlight-symbol-idle-delay . 1.0)
    :config
    (leaf *patch-highlight-symbol :after doom-themes
      :hook
      (after-load-theme-hook . (lambda ()
                                 (custom-set-faces
                                  `(highlight-symbol-face  ((t (:background ,(doom-color 'dark-cyan))))))))
      )
    )
  )


(leaf *conf-appearance
  :custom
  (ring-bell-function . 'ignore)
  :config
  (display-time)
  (column-number-mode t)
  (blink-cursor-mode 0)
  (which-function-mode 1)
  (transient-mark-mode 1)

  (leaf *set-linespacing
    :doc "control line spacing"
    :custom (line-spacing . 0.40)
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
      (kill-local-variable 'line-spacing))
    )

  (leaf *config-custom-themes-path
    :doc "load custom theme from elisp dir"
    :custom (custom-theme-directory . "~/.emacs.d/elisp/"))

  (leaf *conf-theme-hook
    :doc "reconfigure appearance after `load-theme' fired"
    :url "https://emacs.stackexchange.com/a/28947"
    :config
    (defvar after-load-theme-hook nil
      "Hook run after color theme is loaded using `load-theme'.")
    (defadvice load-theme (after run-after-load-theme-hook activate)
      "Run `after-load-theme-hook'."
      (run-hooks 'after-load-theme-hook))
    )

  (leaf *config-doom-modeline
    :config
    (load "~/.emacs.d/elisp/doom.el")

    (leaf *patch-color-with-doom-themes :emacs>= "28.1"
      :after doom-themes
      :hook
      (after-load-theme-hook . (lambda ()
                                 (custom-set-faces
                                  `(help-key-binding  ((t (
                                                           :foreground ,(doom-color 'cyan)
                                                           :background ,(doom-color 'bg-alt)
                                                           :box (:line-width (1 . -1) :color ,(doom-color 'fg))
                                                           ))))
                                  ))))
    )

  (leaf *patch-childframe :emacs> "28" :after doom-themes
    :hook
    (after-load-theme-hook . (lambda ()
                               (custom-set-faces
                                `(child-frame-border  ((t (:foreground ,(doom-color 'bg))))))))
    )

  (leaf paren
    :doc "configure show-paren-mode"
    :tag "builtin"
    :config
    (leaf *patch-paren-face
      :hook (after-load-theme-hook . (lambda ()
                                       (custom-set-faces
                                        `(show-paren-match  ((t (:background ,(doom-color 'green))))))))
      )
    :global-minor-mode show-paren-mode)

  (leaf electric-pair-mode
    :doc "automatic pares parenthesis"
    :tag "builtin"
    :emacs>= "24"
    :hook
    ((conf-mode-hook
      prog-mode-hook) . electric-pair-mode))

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
    :config
    (defun rdm/display-line-numbers-toggle-rel ()
      "toggle relative"
      (interactive)
      (if (eq display-line-numbers-type 'relative)
          (customize-set-variable 'display-line-numbers-type  t)
        (customize-set-variable 'display-line-numbers-type  'relative))
      (display-line-numbers--turn-on)) ;; read config
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
    (leaf *patch-fill-column-indicator :after doom-themes
      :hook
      (after-load-theme-hook . (lambda ()
                                 (custom-set-faces
                                  `(fill-column-indicator ((t (:foreground ,(doom-color 'blue))))))
                                 ))
      )
    )

  (leaf hl-todo
    :ensure t
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


  (leaf nyan-mode
    :ensure t
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
    :require t
    :hook
    ((conf-mode-hook
      outline-mode-hook
      prog-mode-hook
      text-mode-hook
      git-timemachine-mode-hook) . highlight-indent-guides-mode)
    :custom
    (highlight-indent-guides-auto-enabled             . nil)
    (highlight-indent-guides-auto-odd-face-perc       . 10)
    (highlight-indent-guides-auto-even-face-perc      . 10)
    (highlight-indent-guides-method                   . 'column)
    (highlight-indent-guides-responsive               . 'top)
    (highlight-indent-guides-delay                    . 0)
    ;; (highlight-indent-guides-auto-character-face-perc . 90)
    :config
    (leaf *patch-highlight-indent-guides-color :after doom-themes cl-lib
      :doc "patch `indent-guides-color'"
      :hook
      (after-load-theme-hook . (lambda ()
                                 (let* ((mix-colors #'(lambda (x y)
                                                        (apply #'color-rgb-to-hex
                                                               (cl-mapcar #'(lambda (a b) (/ (+ a b) 2.2))
                                                                        (color-name-to-rgb x)
                                                                        (color-name-to-rgb y)))))
                                        (dimmc (funcall mix-colors
                                                        (doom-color 'bg) (doom-color 'dark-cyan)))
                                        (activec (doom-color 'dark-blue)))
                                   (custom-set-faces
                                    `(highlight-indent-guides-odd-face      ((nil (:background ,dimmc))))
                                    `(highlight-indent-guides-top-odd-face  ((nil (:background ,activec))))
                                    `(highlight-indent-guides-even-face     ((nil (:background ,dimmc))))
                                    `(highlight-indent-guides-top-even-face ((nil (:background ,activec))))
                                    ))))
      )
    )

  (leaf rainbow-delimiters
    :ensure t
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
                          ;;tab-mark tabs ;; conflicts highlight indent mode
                          ;;empty  ; empty line
                          ;;lines-tail
                          ;;spaces
                          ))
    (whitespace-display-mappings . '((tab-mark ?\t [?\xBB ?\t] [?\\ ?\t])
                                     (newline-mark ?\n [?\x21B2 ?\n])
                                     ))
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
    (leaf *patch-whitespace-face :after doom-themes
      :hook
      (after-load-theme-hook . (lambda () (custom-set-faces
                                           `(whitespace-trailing ((nil (:background ,(doom-color 'bg) ;:inherit 'default
                                                                                    :foreground ,(doom-color 'magenta)
                                                                                    :underline (:style wave)))))
                                           )))
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
    (pulsar-pulse-on-window-change . t)
    (pulsar-pulse-functions        . '(
                                       ;; recenter-top-bottom
                                       ;; move-to-window-line-top-bottom
                                       reposition-window
                                       ;; forward-page
                                       ;; backward-page
                                       ;; scroll-up-command
                                       ;; scroll-down-command
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
    ((
      prog-mode-hook
      conf-mode-hook
      ) . pulsar-mode)
    :config
    ;; (pulsar-global-mode 1)
    (leaf *patch-pulsar-color :after doom-themes
      :hook
      ((after-load-theme-hook
        pulsar-mode-hook) . (lambda ()
                                 (custom-set-faces
                                  `(pulsar-magenta ((nil (:background ,(doom-color 'magenta)))))
                                  `(pulsar-yellow  ((nil (:background ,(doom-color 'yellow)))))
                                  `(pulsar-red     ((nil (:background ,(doom-color 'red)))))
                                  `(pulsar-blue    ((nil (:background ,(doom-color 'blue)))))
                                  `(pulsar-cyan    ((nil (:background ,(doom-color 'cyan)))))
                                  `(pulsar-green   ((nil (:background ,(doom-color 'green)))))
                                  )))
      )

    (evil-define-key 'normal 'global
      (kbd "SPC") 'pulsar-pulse-line
      (kbd "S-SPC") 'pulsar-highlight-line)
    )
  )


(leaf ido :disabled t
  :doc "configure ido based completion"
  :tag "builtin"
  :init
  (ido-mode t)
  (ido-everywhere t)
  :custom
  (ido-enable-flex-matching . t)
  (ido-save-directory-list-file . "~/.emacs.d/.cache/ido.last")
  :config
  (leaf amx
    :ensure t
    :bind
    (:global-map ("M-x" . amx)
                 ("M-X" . amx-major-mode-commands))
    :custom
    (amx-save-file . "~/.emacs.d/.cache/amx-items")
    (amx-backend . 'ido)
    (amx-prompt-string . "⚡> ")
    :global-minor-mode amx-mode)
  (leaf ido-vertical-mode
    :ensure t
    :custom
    (ido-vertical-define-keys . 'C-n-C-p-up-down-left-right)
    (ido-vertical-show-count . t)
    (ido-vertical-indicator . " ▶")
    :config (ido-vertical-mode t))
  (leaf ido-completing-read+
    :ensure t
    :commands ido-ubiquitous-mode
    :custom
    (magit-completing-read-function . 'magit-ido-completing-read)
    (gnus-completing-read-function  . 'gnus-ido-completing-read)
    :config (ido-ubiquitous-mode 1))
  (leaf ido-yes-or-no
    :ensure t
    :commands ido-yes-or-no-mode
    :config (ido-yes-or-no-mode t))
  (leaf ido-complete-space-or-hyphen
    :ensure t
    :config (ido-complete-space-or-hyphen-mode t))

  (defun rdm/ido-recentf ()
    "works with recentf-mode"
    (interactive)
    (find-file (ido-completing-read "Find from recent: " recentf-list)))
  )


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
"  (nerd-fonts "fa-list-ul"))
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
" (nerd-fonts "fa-list-ul"))
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
" (nerd-fonts "fa-list-ul"))
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
" (nerd-fonts "fa-list-ul"))
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
" (nerd-fonts "fa-list-ul"))
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
" (nerd-fonts "fa-list-ul"))
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
" (nerd-fonts "fa-list-ul"))
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
" (nerd-fonts "fa-list-ul"))
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
 _a_:   apropos
 _m_:   man
 _f_:   file externally                    _t_:   theme
 _p_:   preview at point

 _SPC_: BACK
" (nerd-fonts "fa-list-ul"))
      ("a" consult-apropos                  :exit t)
      ("m" consult-man                      :exit t)
      ("f" consult-file-externally          :exit t)
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
     ("M-g b"   . consult-bookmark)
     ("M-g M-b" . consult-bookmark)
     ("M-y"     . consult-yank-from-kill-ring)
     ("C-x b"   . consult-buffer) ; replace switch-to-buffer. narrowing with b/f/m/p
     ("M-5"     . hydra-consult/body)
     ("<f5>"    . hydra-consult/body))
    :init
    (leaf *consult-use-fd :if (executable-find "fd")
      :doc "use fd for find if avalable"
      :custom (consult-find-command . "fd --color=never --full-path ARG OPTS"))
    (leaf *consult-use-rigrep :if (executable-find "rg")
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
    (leaf consult-dir :ensure t))

  (leaf orderless
    :ensure t
    :doc "Advanced completion style"
    :custom
    (completion-styles             . '(orderless))
    (completion-category-defaults  . nil)
    (completion-category-overrides . '((file (style partial-completion)))))

  (leaf marginalia
    :ensure t
    :doc "Rich annotations in the minibuffer"
    :config (marginalia-mode))

  (leaf embark
    :ensure t
    :doc "Emacs Mini-Buffer Actions Rooted in Keymaps"
    :bind
    (("C-." . embark-act)         ; pick some comfortable binding
     ("C-;" . embark-dwim)        ; good alternative: M-.
     ("C-h B" . embark-bindings)) ; alternative for `describe-bindings'
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
      :after (embark consult)
      ;; :demand t ; only necessary if you have the hook below
      :leaf-defer nil
      ;; if you want to have consult previews as you move around an
      ;; auto-updating embark collect buffer
      :hook (embark-collect-mode-hook . consult-preview-at-point-mode))
    )

  (leaf vertico
    :ensure t
    :doc "VERTical Interactive COmpletion"
    :init
    (vertico-mode)
    (leaf savehist
      :tag "builtin"
      :doc "Persist history over Emacs restarts. Vertico sorts by history position."
      :custom `((savehist-file . ,(cache-sub-file "history")))
      :init (savehist-mode))

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

  (leaf all-the-icons-completion :emacs>= "26.1"
    :after all-the-icons marginalia
    :doc "Add icons to completion candidates using the built in completion metadata functions."
    :ensure t
    :hook
    (conf-on-term-hook . (lambda () (all-the-icons-completion-mode 0)))
    (conf-on-gui-hook  . (lambda () (all-the-icons-completion-mode +1)))
    )
  )


(leaf *competion-linting
  :doc "completion and linting packages"
  :config
  (leaf auto-complete
    :tag "builtin"
    :setq-default
    (ac-sources  . '(ac-source-filename
                     ac-source-words-in-all-buffer)))

  (leaf ispell :if (executable-find "aspell")
    :tag "builtin"
    :setq-default
    (ispell-program-name     . "aspell")
    (ispell-local-dictionary . "en_US")
    )

  (leaf eldoc
    :tag "builtin"
    :config
    (leaf eldoc-box
      :ensure t
      :hook (eldoc-mode-hook . (lambda () (unless (display-graphic-p) (eldoc-box-hover-mode))))
      :custom
      (eldoc-box-only-multi-line    . nil)
      (eldoc-box-fringe-use-same-bg . t)
      (eldoc-box-clear-with-C-g     . t)
      (eldoc-box-cleanup-interval   . 0.5)
      :config
      (leaf *patch-eldoc-box-faces :after doom-themes
        :hook
        (after-load-theme-hook . (lambda ()
                                   (custom-set-faces ;; modify bg to eldoc-box-body
                                    `(eldoc-box-body   ((t (:inherit 'default :background ,(doom-color 'bg-alt)))))
                                    `(eldoc-box-border ((nil (:background ,(doom-color 'bg)))))
       ))))
      )
    )

  (leaf flymake :disabled t
    :tag "builtin"
    :emacs>= "26"
    :hook (prog-mode-hook . flymake-mode)
    :bind
    (:flymake-mode-map ("M-j"   . flymake-goto-next-error)
                       ("M-k"   . flymake-goto-previous-error)
                       ("C-M-l" . consult-flymake))
    )

  (leaf flycheck
    :ensure t
    :bind
    (:flycheck-mode-map ("M-j" . flycheck-next-error)
                        ("M-k" . flycheck-previous-error))
    :config
    (leaf *patch-flycheck-faces :after doom-themes
      :doc "on doom themes loaded"
      :hook
      (after-load-theme-hook . (lambda ()
                                 (custom-set-faces
                                  `(flycheck-warning ((t (:background ,(doom-color 'bg)))))
                                  `(flycheck-info    ((t (:background ,(doom-color 'bg)))))
                                  `(flycheck-error   ((t (:background ,(doom-color 'bg)))))
                                  )))
      )


    (leaf flycheck-posframe :if (not (emacs-works-on-term-p))
      :ensure t
      :hook (flycheck-mode-hook . flycheck-posframe-mode)
      :custom
      `(
        (flycheck-posframe-position       . 'window-bottom-right-corner)
        (flycheck-posframe-info-prefix    . ,(format "%s " (nerd-fonts "fa-info")))
        (flycheck-posframe-warning-prefix . ,(format "%s " (nerd-fonts "fa-exclamation-triangle")))
        (flycheck-posframe-error-prefix   . ,(format "%s " (nerd-fonts "fa-exclamation-circle")))
        )
      )

    (leaf consult-flycheck
      :ensure t
      :bind
      (:flycheck-mode-map
       :package flycheck
       ("C-M-l" . consult-flycheck)))
    )

  (leaf company
    :doc "company completion framework"
    :ensure t
    :custom
    (company-idle-delay . 0)
    (company-selection-wrap-around . t)
    (completion-ignore-case . t)
    (company-minimum-prefix-length . 1)
    (company-transformers . '(
                              company-sort-by-occurrence
                              company-sort-by-backend-importance
                              company-sort-prefer-same-case-prefix
                              ))
    :config
    (leaf *add-yasnippet-backend
      :after yasnippet
      :config
      (add-to-list 'company-backends 'company-yasnippet))
    (leaf company-box
      :ensure t
      :hook (company-mode-hook . company-box-mode))
    :global-minor-mode global-company-mode)

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
      (lsp-completion-provider                . ,(if (fboundp 'company-mode) :capf :none))
      )
    :bind (:lsp-mode-map ("C-c r"   . lsp-rename))
    :hook
    (lsp-mode-hook . (lambda ()
                       (eldoc-box-hover-at-point-mode -1)
                       (flycheck-posframe-mode -1))) ; disable flycheck-posframe
    :config
    (setq lsp-prefer-flymake nil)

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

    (leaf lsp-treemacs
      :ensure t
      :after (treemacs lsp-mode)
      :hook (lsp . lsp-treemacs)
      :bind (:global-map ("C-x t e" . lsp-treemacs-errors-list)
                         ("C-x t s" . lsp-treemacs-symbols))
      :config (lsp-treemacs-sync-mode 1))

    (leaf lsp-origami
      :ensure t
      :hook (lsp-after-open-hook . lsp-origami-try-enable))

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
" (nerd-fonts "fa-code"))
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
" (nerd-fonts "fa-code"))
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
    )

  (leaf eglot
    :ensure t
    :doc "another lsp client"
    :preface
    (leaf *eglot-with-eldoc-box
      :doc "use eldoc-box-mode if available"
      :hook
      (eglot-managed-mode-hook . (lambda ()
                                   (if (fboundp 'eldoc-box-hover-mode)
                                       (eldoc-box-hover-mode)))))
    )

  (leaf dap-mode
    :ensure t
    :after lsp-mode
    :doc "debug adapter protocol package"
    :require dap-hydra
    :bind
    (:global-map
     ("<f8>" . dap-hydra)
     ("M-8"  . dap-hydra))
    :custom
    (dap-auto-configure-features
     '(sessions locals breakpoints expressions repl controls tooltip))
    :config
    (dap-mode 1)
    (dap-auto-configure-mode 1)
    )
  )


(leaf *helpful-things
  :config

  (leaf rainbow-mode
    :ensure t
    :doc "preview colors from hex values")

  (leaf ace-window
    :ensure t
    :bind ("M-o" . ace-window)
    :custom (aw-keys . '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    :custom-face ;; larger leading char
    (aw-leading-char-face .
                          '((t (:inherit 'highlight :height 5.0 :width normal :underline t))))
    )

  (leaf ace-link
    :ensure t
    :doc "quickly follow links"
    :bind ("M-O" . ace-link)
    :config
    (ace-link-setup-default))

  (leaf which-key
    :ensure t
    :config
    (leaf which-key-posframe
      :ensure t
      :custom
      (which-key-posframe-poshandler . 'posframe-poshandler-frame-top-left-corner)
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

      (leaf *patch-which-key-posframe-faces :after doom-themes
        :hook
        (after-load-theme-hook . (lambda ()
                                   (custom-set-faces
                                    `(which-key-posframe-border ((nil (:background ,(doom-color 'fg-alt)))))
                                    ))))
      )
    (which-key-mode)
    )

  (leaf undo-tree
    :ensure t
    :custom
    `(
      (undo-tree-visualizer-timestamps . t)
      (undo-tree-auto-save-history . t)
      (undo-tree-history-directory-alist . `(("." . ,(cache-sub-dir "undo-tree"))))
      )
    :global-minor-mode (global-undo-tree-mode))

  (leaf dashboard
    :ensure t
    :custom
    `(
      (dashboard-banner-logo-title . ,(format "Welcome to Emacs %s! [on %s (%s)]"
                                              emacs-version
                                              system-name
                                              system-type)) ;; Set the title
      (dashboard-startup-banner     . 3)
      (dashboard-center-content     . t)
      (dashboard-show-shortcuts     . t)
      (dashboard-set-heading-icons  . t)
      (dashboard-set-file-icons     . t)
      (dashboard-set-init-info      . t)
      (dashboard-set-footer         . t)
      (dashboard-match-agenda-entry . "-agenda_ignore")
      (dashboard-items              . '((agenda    . 5)
                                        (recents   . 5)
                                        (bookmarks . 5)
                                        (projects  . 5)
                                        (registers . 5)))
      (initial-buffer-choice        . (lambda ()
                                        (dashboard-refresh-buffer)
                                        (get-buffer "*dashboard*")))
      )
    ;; choose banner before refreshing dashboard
    :advice
    (:before dashboard-refresh-buffer
             (lambda (&rest _args)
               (let ((banner-img "~/.emacs.d/banner.png"))
                 (if (and (file-regular-p banner-img)
                          (display-graphic-p))
                     (customize-set-variable 'dashboard-startup-banner banner-img)
                     (customize-set-variable 'dashboard-startup-banner 3)
                   ))))
    :bind ("<f12>" . dashboard-refresh-buffer))

  (leaf calendar
    :tag "builtin"
    :custom
    (
     (calendar-month-name-array   . ["01" "02" "03" "04" "05" "06"
                                     "07" "08" "09" "10" "11" "12" ])
     (calendar-week-start-day     . 0)
     (calendar-mark-holidays-flag . t)
     )
    :hook
    (calendar-today-visible-hook   . calendar-mark-today)
    :config
    (leaf *patch-calendar-faces :after doom-themes
      :hook
      (after-load-theme-hook . (lambda ()
                                 (custom-set-faces
                                  `(holiday
                                    ((nil (:foreground ,(doom-color 'orange) :background ,(doom-color 'bg)))))
                                  `(calendar-today
                                    ((nil (:foreground ,(doom-color 'base3) :background ,(doom-color 'green) :underline nil))))
                                  ))))
    )

  (leaf eshell
    :tag "builtin"
    :custom
    `((eshell-history-file-name . ,(expand-file-name
                                    "history"
                                    (cache-sub-dir "eshell"))))
    )

  (leaf vterm
    :ensure t
    :preface ; release keys for binding
    (global-unset-key [f2])
    (global-unset-key "\M-2")
    :hook (vterm-mode-hook . (lambda () (rdm/sw-lnsp 1)))
    :custom
    (vterm-max-scrollback . 10000)
    (vterm-buffer-name-string . "vterm: %s")
    :config
    (evil-set-initial-state 'vterm-mode 'emacs)
    (defun rdm/vterm-new-buffer-in-current-window()
      "new vterm on current window"
      (interactive)
      (let ((display-buffer-alist nil)) (vterm)))
    (leaf vterm-toggle
      :ensure t
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
    :bind
    (("M-2" . vterm-toggle)
     ("<f2>" . vterm-toggle)
     ("M-@" . rdm/vterm-new-buffer-in-current-window))
    (:vterm-mode-map
     ("C-u" . vterm-send-C-u)
     ("C-c C-[" . vterm-send-escape)
     ("M-2" . vterm-toggle)
     ("<f2>" . vterm-toggle))
    )

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
    (epa-pinentry-mode . 'loopback)
    )

  (leaf eww
    :tag "builtin"
    :doc "text base web browser"
    :custom
    `(
      (shr-width               . 70)
      (shr-indentation         . 4)
      (shr-use-fonts           . t)
      (shr-bullet              . "* ")
      (eww-search-prefix       . "https://www.google.com/search?q=")
      (eww-bookmarks-directory . ,(cache-sub-dir "eww"))
      )
    :hook
    (eww-mode-hook . (lambda () (rdm/sw-lnsp 0.75)))
    :config
    (leaf *eww-patch-faces :after doom-themes
      :hook
      ((after-load-theme-hook
        eww-mode-hook) . (lambda ()
        (custom-set-faces
         `(eww-form-text ((t :background ,(doom-color 'bg-alt)))))))
      )

    (leaf *eww-bind-nerd-icons
      :custom
      `(
        (shr-bullet                        . ,(format "%s " (nerd-fonts "oct-primitive-square")))
        (eww-form-checkbox-symbol          . ,(nerd-fonts "fa-square-o"))
        (eww-form-checkbox-selected-symbol . ,(nerd-fonts "fa-check-square-o"))
        )
      )

    (leaf *eww-toggle-inhibit-images :emacs>= "28.1"
      :doc "disable image by default"
      :custom
      (shr-inhibit-images . t)
      :config
      (evil-define-key 'normal eww-mode-map
        "i" 'eww-toggle-images
        "I" 'eww-toggle-images)
      )

    (leaf *eww-use-color-config
      :doc "config colors in eww"
      :custom
      (shr-use-colors . nil)
      :config
      (evil-define-key 'normal eww-mode-map
        "c" 'eww-toggle-colors
        "C" 'eww-toggle-colors)
      )

    (leaf *patch-eww-heading-size
      :hook
      (after-load-theme-hook . (lambda ()
                                 (set-face-attribute 'shr-h1 nil :height 1.4)
                                 (set-face-attribute 'shr-h2 nil :height 1.2)
                                 (set-face-attribute 'shr-h3 nil :height 1.1)
                                 (set-face-attribute 'shr-h4 nil :height 1.0)
                                 ))
      )
    )

  (leaf emacs-w3m :if (executable-find "w3m")
    :ensure t)

  (leaf pdf-tools
    :doc "require to execute `pdf-tools-install'"
    :ensure t
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :bind (:pdf-view-mode-map ("C-s" . ctrlf-forward-default))
    :setq-default (pdf-view-display-size . 'fit-page)
    :hook
    (pdf-view-mode-hook . (lambda() (line-number-mode -1)))
    (pdf-view-mode-hook . pdf-links-minor-mode)
    (pdf-view-mode-hook . pdf-annot-minor-mode)
    :init
    (leaf *switch-pdf-view-dark :after doom-themes
      :doc "detect whether doom-theme bg color seems dark (to avoid crush)"
      :preface
      (defun theme-seems-dark-p ()
        "Check `(doom-theme 'bg)'. If theme seems to be dark, returns t"
        (< (apply '+ (color-name-to-rgb (doom-color 'bg))) 1.5))
      :hook
      (pdf-view-mode-hook    . (lambda ()
                                 (if (theme-seems-dark-p)
                                     (pdf-view-dark-minor-mode +1)
                                   (pdf-view-dark-minor-mode -1))))
      (after-load-theme-hook . (lambda ()
                                 (dolist (buff (buffer-list))
                                   (with-current-buffer buff
                                     (when (eq major-mode #'pdf-view-mode)
                                       (if (theme-seems-dark-p)
                                           (pdf-view-dark-minor-mode +1)
                                         (pdf-view-dark-minor-mode -1)))))))
      )
    :config
    ;; setup package if not ready
    (pdf-tools-install)
    (pdf-loader-install)
    )

  (leaf go-translate
    :ensure t
    :setq
    (gts-translate-list     . '(("en" "ja")
                                ("ja" "en")))
    ;;(go-translate-buffer-follow-p . t)       ; focus the result window
    ;;(go-translate-buffer-source-fold-p . t)  ; fold the source text in the result window
    ;;(go-translate-buffer-window-config . ..) ; config the result window as your wish
    )

  (leaf *darwin-dictionary-integration :if (eq system-type 'darwin)
    :doc "dictionary app integration on macOS"
    :config
    (defun macos-dict-lookup (word)
      "Lookup word with dictionary.app by apple."
      (call-process "open" nil 0 nil (concat "dict://" word) "-g"))

    (defun macos-dict-lookup-word ()
      "Lookup the word at point with dictionary.app by apple."
      (interactive)
      (macos-dict-lookup (read-from-minibuffer "dictionary.app: " (current-word))))

    (defun monokakido-app-installed-p ()
      "if monokakido Dictionaries.app available, returns t"
      (string-match-p "jp.monokakido.Dictionaries"
                      (shell-command-to-string
                       "mdls '/Applications/Dictionaries.app' -name 'kMDItemCFBundleIdentifier'")))

    (leaf *monokakido-dict :if (monokakido-app-installed-p)
      :doc "use Disctionaries.app by monokakido (based on gist url)"
      :url "https://gist.github.com/skoji/aad5f66cbffc370e29888671e0801c6d"
      :config
      (defun monokakido-lookup (word)
        "Lookup word with Dictionaries.app by Monokakido."
        (call-process "open" nil 0 nil (concat "mkdictionaries:///?text=" word) "-g"))

      (defun monokakido-lookup-word ()
        "Lookup the word at point with Dictionaries.app by Monokakido."
        (interactive)
        (monokakido-lookup (read-from-minibuffer "Monokakido: " (current-word))))

      (evil-define-key '(normal visual) 'global
        (kbd "C-w f") 'monokakido-lookup-word
        (kbd "C-w F") 'macos-dict-lookup-word))

    (leaf *bind-macos-dict :unless (monokakido-app-installed-p)
      :doc "bind macos-dict-lookup-word to key with dictionary.app by apple"
      :config
      (evil-define-key '(normal visual) 'global
        (kbd "C-w f") 'macos-dict-lookup-word)))
  )


(leaf *git-related-packages
  :config
  (leaf git-gutter
    :ensure t
    :custom
    `(
      (git-gutter:modified-sign . "=")
      (git-gutter:added-sign    . "+")
      (git-gutter:deleted-sign  . ,(nerd-fonts "fa-minus"))
      (git-gutter:ask-p         . nil)
      )
    :config
    (leaf *patch-git-gutter-color :after doom-themes
      :hook
      (after-load-theme-hook . (lambda ()
                                 (custom-set-faces ;; modify bg fg
                                  `(git-gutter:modified ((t (:foreground ,(doom-color 'blue)  :background ,(doom-color 'blue)))))
                                  `(git-gutter:added    ((t (:foreground ,(doom-color 'base4) :background ,(doom-color 'green)))))
                                  `(git-gutter:deleted  ((t (:foreground ,(doom-color 'red))))))))
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
 _k_:   previous             _v_:   mark
 _G_:   end of hunk          _s_:   stage                _h_:   open git timemachine
 ^ ^                         _x_:   revert               _g_:   open magit mode
" (nerd-fonts "fa-code-fork"))
     ("j" git-gutter:next-hunk)
     ("k" git-gutter:previous-hunk)
     ("G" git-gutter:end-of-hunk)
     ("d" git-gutter:popup-hunk)
     ("v" git-gutter:mark-hunk)
     ("x" git-gutter:revert-hunk)
     ("s" git-gutter:stage-hunk)
     ("r" git-gutter:update-all-windows)
     ("h" git-timemachine :exit t)
     ("g" magit :exit t))

    :bind
    (:global-map
     ("<f3>" . hydra-git-gutter/body)
     ("M-3"  . hydra-git-gutter/body))
    :global-minor-mode (global-git-gutter-mode t))

  (leaf magit
    :ensure t
    ;:custom (magit-completing-read-function . 'magit-ido-completing-read)
    :hook (magit-section-mode-hook . (lambda () (whitespace-mode -1)))
    :config
    (leaf magit-todos
      :ensure t
      :custom ;; TODO: add grouping via category
      (magit-todos-max-items     . 15)
      (magit-todos-ignore-case   . nil)
      (magit-todos-rg-extra-args . '("--hidden" "--no-config"))
      (magit-todos-exclude-globs . '(".git/" ".node_modules/" ".venv/"))
      :hook (magit-section-mode-hook . magit-todos-mode))

    (leaf *magit-git-gutter-integration :if (fboundp 'git-gutter)
      :doc "refresh git-gutter on magit operation"
      :url "https://stackoverflow.com/questions/23344540/emacs-update-git-gutter-annotations-when-staging-or-unstaging-changes-in-magit"
      :hook
      (magit-post-refresh-hook . (lambda () ;; update visible buffer
                                   (dolist (buff (buffer-list))
                                     (with-current-buffer buff
                                       (when (and git-gutter-mode
                                                  (get-buffer-window buff))
                                         (git-gutter-mode t))))))
      )
    )

  (leaf git-timemachine :ensure t)
  )


(leaf *file-manage
  :doc "file management related config"
  :config
  (leaf dired
    :tag "builtin"
    :preface
    (leaf osx-trash :if (eq 'system-type 'darwin)
      :ensure t
      :config
      (osx-trash-setup))
    :custom
    `(
      ;; use gnu-ls if available on macOS
      (insert-directory-program . ,(if (and (eq system-type 'darwin)
                                             (not (executable-find "gls")))
                                       "ls" "gls"))
      (dired-listing-switches    . ,(if (and (eq system-type 'darwin)
                                             (not (executable-find "gls")))
                                        "-l" "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group"))
      (dired-dwim-target         . t)
      (delete-by-moving-to-trash . t)
      ;; NOTE: on macOS, require full disk access to use trash bin
      (trash-directory           . ,(when (eq system-type 'darwin) "~/.Trash"))
      )
    :config
    (ffap-bindings)
    ;; by selecting directory, do not creaete new dired buffer
    (put 'dired-find-alternate-file 'disabled nil)
    )

  (leaf dirvish
    :unless (and (equal system-type 'darwin)
                 (not (executable-find "gls")))
    :ensure t
    :after transient
    :custom
    `(
      ;; kill all session buffers on quit
      (dirvish-reuse-ssession       . nil)
      ;; cache
      (dirvish-cache-dir                  . ,(cache-sub-dir "dirvish"))
      (dirvish-media-auto-cache-threshold . '(500 . 4))
      ;; appearance
      (dirvish-default-layout       . '(0 0.2 0.8)) ;; no preview
      (dirvish-header-line-position . 'default)
      (dirvish-header-line-height   . '(25 . 25))
      (dirvish-header-line-format   . '(:left  (path)
                                        :right (free-space)))
      (dirvish-mode-line-position   . 'default)
      (dirvish-mode-line-format     . '(:left  (sort file-time " " file-size symlink)
                                        :right (omit yank index)))
      ;; bookmarks TODO: add config for windows
      (dirvish-quick-access-entries . `(("h" ,(getenv "HOME")   "Home")
                                        ("d" ,(expand-file-name "Dowwnloads" (getenv "HOME")) "Downloads")
                                        ("D" ,(expand-file-name "Documents"  (getenv "HOME")) "Documents")
                                        ("e" ,(expand-file-name ".emacs.d"   (getenv "HOME")) "Emacs directory")
                                        ("m" ,(cond ((eq system-type 'darwin)    "/Volumes/")
                                                    ((eq system-type 'gnu/linux) "/mnt/")
                                                    (t "(not available)")) "Drives")
                                        ("t" ,(cond ((eq system-type 'darwin)    "~/.Trash")
                                                    ((eq system-type 'gnu/linux) "~/.local/share/Trash/files/")
                                                    (t "(not available)")) "TrashCan")))
      )

    :init
    (leaf *dirvish-on-state
      :doc "switch dirvish attributes"
      :custom
      `(
        (dirvish-attributes           . `,(if (or (daemonp) (display-graphic-p))
                                             '(vc-state all-the-icons collapse git-msg file-time file-size)
                                           '(collapse git-msg file-time file-size)))
        (dirvish-side-attributes      . `,(if (or (daemonp) (display-graphic-p))
                                             '(vc-state all-the-icons collapse git-msg file-size)
                                           '(collapse git-msg file-size)))
        )
      :hook
      (dirvish-directory-view-mode-hook . (lambda ()
                                            (if (display-graphic-p)
                                                (progn
                                                  (customize-set-variable 'dirvish-attributes
                                                                          '(vc-state all-the-icons collapse git-msg file-time file-size))
                                                  (customize-set-variable 'dirvish-side-attributes
                                                                          '(vc-state all-the-icons collapse git-msg file-size)))
                                              (progn
                                                (customize-set-variable 'dirvish-attributes
                                                                        '(collapse git-msg file-time file-size))
                                                (customize-set-variable 'dirvish-side-attributes
                                                                        '(collapse git-msg file-size))
                                                ))))
      )
    ;; Let Dirvish take over Dired globally
    (dirvish-override-dired-mode)
    (add-to-list 'undo-tree-incompatible-major-modes 'dirvish-mode)

    :config
    (leaf *dirvish-preview-config
      :doc "configure dirvish preview dispathcer"
      :preface
      (dirvish-define-preview exa (file)
        "Use `exa' to generate directory preview."
        :require ("exa") ; tell Dirvish to check if we have the executable
        (when (file-directory-p file) ; we only interest in directories here
          `(shell . ("exa" "--color=always" "--icons" "--git" "--group-directories-first" "-al" ,file))))
      :custom
      `(
        (dirvish-preview-dispatchers  . `(exa image gif video audio epub archive
                                             ,(if (fboundp 'pdf-tools-install) 'pdf 'pdf-preface)))
        )
      )

    (leaf *dirvish-preview-hooks-mediainfo :if (executable-find "mediainfo")
      :hook
      ((dirvish-image-preview-dp-hook
        dirvish-video-preview-dp-hook
        dirvish-gif-preview-dp-hook
        dirvish-audio-preview-dp-hook) . dirvish-media-properties)
      )

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

  (leaf neotree
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

  (leaf projectile
    :ensure t
    :config (projectile-mode +1))

  (leaf treemacs
    :ensure t
    :init
    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))

    (leaf treemacs-all-the-icons
      :after treemacs-themes all-the-icons
      :require t
      :ensure t
      :hook
      (conf-on-term-hook  . (lambda () (treemacs-load-theme "Default")))
      (conf-on-gui-hook   . (lambda () (treemacs-load-theme "all-the-icons")))
      (treemacs-mode-hook . (lambda ()
                              (if (display-graphic-p)
                                  (treemacs-load-theme "all-the-icons")
                                (treemacs-load-theme "Default"))))
      )
    :bind
    (("M-0"       . treemacs-select-window)
     ("C-x t 1"   . treemacs-delete-other-windows)
     ("C-x t t"   . treemacs)
     ("C-x t B"   . treemacs-bookmark)
     ("C-x t C-t" . treemacs-find-file)
     ("C-x t M-t" . treemacs-find-tag))
    :config
    (leaf *treemacs-custom :after treemacs
      :doc "eval after treemacs load"
      :custom
      `(
        (treemacs-deferred-git-apply-delay      . 0.5)
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
        (treemacs-collapse-dirs                 . ,(if treemacs-python-executable 3 0))
        (treemacs-persist-file                  . ,(cache-sub-file "treemacs-persist"))
        ))
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode 0)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t) (treemacs-git-mode 'deferred))
      (`(t . _) (treemacs-git-mode 'simple)))

    ;; treemacs integrations
    (leaf treemacs-evil
      :ensure t
      :after (treemacs evil)
      :require treemacs-evil
      ;; fix tab action
      :bind (:evil-treemacs-state-map ("TAB" . treemacs-TAB-action)))

    (leaf treemacs-magit
      :ensure t
      :after (treemacs magit))

    (leaf treemacs-projectile
      :after treemacs projectile
      :ensure t
      :custom
      `(
        (projectile-cache-file          . ,(cache-sub-file "projectile.cache" "projectile"))
        (projectile-known-projects-file . ,(cache-sub-file "projectile-known-projects.eld" "projectile"))
        ))

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


(leaf *load-lang-settings
  :config (load "~/.emacs.d/elisp/langs.el"))


(leaf *conf-appearance-on-state
  :doc "apply theme and frame state hooks after init"
  :hook
  ((server-after-make-frame-hook
    after-init-hook) . (lambda ()
                         (if (display-graphic-p)
                             (conf-on-gui) (conf-on-term))))
  (after-init-hook   . (lambda () (load-theme 'doom-nord-aurora t)))
  )


(leaf *load-local-conf
  :config
  (let
      ((local-conf (expand-file-rec '("elisp" "local.el") user-emacs-directory)))
    (unless (file-exists-p local-conf)
      (with-temp-file local-conf nil))
    (load local-conf)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
