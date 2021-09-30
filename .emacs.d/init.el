;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; emacs configuration by radixM491VA

;; ---------------  load package ---------------
(load "~/.emacs.d/elisp/initpkg.el")


(leaf startup
  :config
  ;; mouse support on terminal
  (unless window-system
    (require 'mouse)
    (xterm-mouse-mode t)
    (defun track-mouse (e))
    (setq mouse-sel-mode t))
  (defun emacs-works-on-term-p ()
    "returns t if emacs seems running on term"
    (not (or (daemonp) (display-graphic-p))))

  (leaf exec-path-from-shell
    :doc "load path from shell at startup"
    :ensure t
    :custom (exec-path-from-shell-arguments . '("-l"))
    :config (when (daemonp) (exec-path-from-shell-initialize)))

  (leaf supress-cl-warning :emacs>= "27"
    :doc "supress cl warning"
    :config
    (setq byte-compile-warnings '(not cl-functions obsolete)))
  )


(leaf conf-appearance
  :custom
  (inhibit-splash-screen . t)
  (ring-bell-function . 'ignore)
  :config
  (if (not (emacs-works-on-term-p))
      (progn
        (tool-bar-mode -1)
        (scroll-bar-mode -1)))
  (menu-bar-mode -1)
  (display-time)
  (column-number-mode t)
  (blink-cursor-mode 0)
  (which-function-mode 1)
  (transient-mark-mode 1)

  (leaf font-configure
    :doc "font configure"
    :config
    (setq default-frame-alist (append (list '(font . "HackGen35Nerd Console-13.5")
                                            '(width . 80)
                                            '(height . 55)) default-frame-alist))
    (setq font-for-tables "HackGenNerd Console"))

  (leaf set-linespacing
    :doc "control line spacing"
    :config
    (setq-default line-spacing 0.40)
    (defun sw-lnsp (wdth)
      "switch line spacing"
      (interactive "nset line spacing: ")
      (setq line-spacing wdth))
    (defun dbl-lnsp ()
      "switch line spacing to double"
      (interactive)
      (setq line-spacing 2.0))
    )

  (leaf paren
    :doc "configure show-paren-mode"
    :tag "builtin"
    ;; :custom-face
    ;; (show-paren-match  . '((nil (:background "SpringGreen2"))))
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
      text-mode-hook) . display-line-numbers-mode)
    )

  (leaf display-fill-column-indicator
    :tag "builtin"
    :emacs>= "27"
    :custom
    (display-fill-column-indicator-column . 90)
    (display-fill-column-indicator-character . ?\N{U+22C5}) ; use '⋅' as indicator
    :custom-face
    (fill-column-indicator . '((t (:foreground "DeepSkyBlue3"))))
    :hook
    ((conf-mode-hook
      fundamental-mode-hook
      outline-mode-hook
      prog-mode-hook
      text-mode-hook) . display-fill-column-indicator-mode)
    )

  (leaf hl-todo
    :ensure t
    :hook
    (prog-mode-hook . hl-todo-mode))

  (leaf ov
    :ensure t
    :init
    (defun check-serif-available()
      "check Noto Serif font available"
      (if (member "Noto Serif CJK JP" (font-family-list))
          (setq serif-available-p t)
        (setq serif-available-p nil)))
    (setq ov-serif nil)
    (defun toggle-buffer-ov-serif (trigger-state-p)
      "temporary make buffer to serif, ov-clear if ov available at buffer"
      (interactive)
      (if serif-available-p
          (if trigger-state-p
              (progn (sw-lnsp 0.8)
                     (setq ov-serif
                           (ov (point-min) (point-max) 'face '(:family "Noto Serif CJK JP"))))
              (progn (sw-lnsp 0.25)
                     (ov-reset ov-serif)))))
    :hook (server-after-make-frame-hook . check-serif-available)
    :commands (ov-reset ov-clear ov))

  (leaf darkroom
    :ensure t
    :init
    (defun my-darkroom-init ()
      (toggle-buffer-ov-serif darkroom-mode))
    :hook (darkroom-mode-hook . my-darkroom-init)
    ;;(darkroom-mode . ov-clear)
    :bind ([f8] . darkroom-mode)
    :custom (darkroom-text-scale-increase . 1)
    ;;:config
    ;;(doom-modeline t) ;TODO:enable modeline
    )

  (leaf all-the-icons :ensure t) ; need installation by all-the-icons-install-fonts

  (leaf unicode-fonts
    :ensure t
    :config (unicode-fonts-setup))

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

  (leaf config-doom-modeline
    :config (load "~/.emacs.d/elisp/doom.el"))

  (leaf config-custom-thems-path
    :doc "load custom theme from elisp dir"
    :custom (custom-theme-directory . "~/.emacs.d/elisp/"))

  (leaf highlight-indent-guides
    :ensure t
    :hook
    ((conf-mode-hook
      outline-mode-hook
      prog-mode-hook
      text-mode-hook) . highlight-indent-guides-mode)
    :custom
    ;;(if (not (display-graphic-p))
    ;;    (progn
    ;;      (setq highlight-indent-guides-auto-enabled nil)
    ;;      (set-face-background 'highlight-indent-guides-odd-face "black")
    ;;      (set-face-background 'highlight-indent-guides-top-odd-face "green")
    ;;      (set-face-background 'highlight-indent-guides-even-face "black")
    ;;      (set-face-background 'highlight-indent-guides-top-even-face "green"))
    ;;    (setq highlight-indent-guides-auto-enabled t)
    ;;  )
    (highlight-indent-guides-method . 'column)
    (highlight-indent-guides-auto-odd-face-perc . 10)
    (highlight-indent-guides-auto-even-face-perc . 10)
    ;;(setq highlight-indent-guides-auto-character-face-perc 20)
    (highlight-indent-guides-responsive . 'top)
    (highlight-indent-guides-delay . 0))

  (leaf rainbow-delimiters
    :ensure t
    :hook
    ((conf-mode-hook
      prog-mode-hook) . rainbow-delimiters-mode))

  (leaf whitespace
    :doc "configure whitespace"
    :hook
    ((conf-mode-hook
      outline-mode-hook
      prog-mode-hook
      text-mode-hook) . whitespace-mode)
    :custom
    (whitespace-global-modes . '(not dired-mode tar-mode neotree magit-status-mode magit-revision-mode))
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
    ;; fix color
    (whitespace-trailing . '((nil (:inherit 'default :foreground "DeepPink" :underline t))))
    (whitespace-tab      . '((nil (:inherit 'default :foreground "gray22" :underline t))))
    (whitespace-newline  . '((nil (:inherit 'shadow))))
    ;;(set-face-attribute 'whitespace-newline nil
    ;;  :foreground "SlateGray"
    ;;  :background nil
    ;;  :underline nil)
    ;;(set-face-attribute 'whitespace-space nil
    ;;  :background my/bg-color
    ;;  :foreground "GreenYellow"
    ;;  :weight 'bold)
    ;;(set-face-attribute 'whitespace-empty nil
    ;;  :background my/bg-color)
    :global-minor-mode global-whitespace-mode
    )
  )


(leaf conf-cache-history
  :config
  (leaf set-cache-path
    :doc "check directory and set target"
    :config
    (let ((my/autosave-dir "~/.emacs.d/.cache/autosaved")
          (my/hist-dir "~/.emacs.d/.cache/hist"))
      (if (not (file-directory-p my/autosave-dir))
          (make-directory my/autosave-dir t))
      (if (not (file-directory-p my/hist-dir))
          (make-directory my/hist-dir t))))

  (leaf autosave
    :doc "configure auto save files"
    :tag "builtin"
    :custom
    (auto-save-file-name-transforms . '((".*" "~/.emacs.d/.cache/autosaved" t)))
    (delete-auto-save-files . t)
    (auto-save-default . t)
    (auto-save-timeout . 15)
    (auto-save-interval . 120)
    (auto-save-list-file-prefix . nil))

  (leaf backup-files
    :doc "make backup files"
    :tag "builtin"
    :custom
    (backup-directory-alist . '((".*" .  "~/.emacs.d/.cache/hist")))
    (make-backup-files . t)
    (version-control . t) ;; enable version control
    (kept-new-versions . 5)
    (kept-old-versions . 1)
    (delete-old-versions . t)
    (create-lockfiles . nil))

  (leaf history
    :doc "configure history, log"
    :tag "builtin"
    :custom
    (message-log-max . 10000)
    (history-length . 1000)
    (history-delete-duplicates . t))

  (leaf recentf
    :tag "builtin"
    :custom
    (recentf-save-file . "~/.emacs.d/.cache/recentf")
    (recentf-max-saved-items . 2000)
    (recentf-auto-cleanup . 'never)
    (recentf-exclude . '("/\\.cache/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:"
                         "/\\.emacs\\.d/games/*-scores" "/\\.emacs\\.d/\\.cask/"))
    :init
    (defmacro with-suppressed-message (&rest body)
      "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
      (declare (indent 0))
      (let ((message-log-max nil))
        `(with-temp-message (or (current-message) "") ,@body)))
    (setq recentf-auto-save-timer
          (run-with-idle-timer 30 t '(lambda ()
                                       (with-suppressed-message (recentf-save-list)))))
    :global-minor-mode recentf-mode
    )

  (leaf bookmark
    :tag "builtin"
    :config
    (custom-set-variables
     '(bookmark-default-file  (file-truename "~/.emacs.d/.cache/bookmarks")))
    )
  )


(leaf lang-locales
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
  (leaf skk
    :ensure ddskk
    :init
    (setq default-input-method "japanese-skk"
          skk-user-directory "~/.emacs.d/.cache/skk"
          skk-init-file "~/.emacs.d/elisp/initskk.el")))


(leaf editor-functions
  :init
  (setq-default tab-width 4
                indent-tabs-mode nil
                truncate-lines t)
  ;; configure indent tab to false as default
  (setq eol-mnemonic-dos "(CRLF)")
  (setq eol-mnemonic-mac "(CR)")
  (setq eol-mnemonic-unix "(LF)")
  (setq find-file-visit-truename t)
  ;; (setq line-move-visual t)
  ;; (setq word-wrap t)

  (leaf visual-line-mode
    :doc "visual line mode enables word wrap"
    :tag "builtin"
    :hook
    ((org-mode-hook
      markdown-mode-hook) . visual-line-mode)
    ;; (global-visual-line-mode nil)
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
    :hook (prog-mode-hook . yas-minor-mode)
    :config (yas-reload-all))

  (leaf evil
    :ensure t
    :init (setq evil-want-keybinding nil)
    :custom (evil-undo-system . 'undo-tree)
    :hydra
    (hydra-manage-frames
     nil
     "
                     ^managing frames^
----------------------------------------------------------
    select           move         resize          split
----------------------------------------------------------
       _k_         _R_⃔   _K_   ⃕_r_        _+_              _v_
       ↑              ↑              ↑              |
   _h_ ←   → _l_      _H_ ←   → _L_      _<_ ← _=_ → _>_      _s_ --+--
       ↓              ↓              ↓              |
       _j_              _J_              _-_
   _p_revious     delete(_c_)   delete other(_o_)   _n_ew
   _a_ce window
"
     ("h" evil-window-left)
     ("j" evil-window-down)
     ("k" evil-window-up)
     ("l" evil-window-right)
     ("a" ace-window)
     ("H" evil-window-move-far-left)
     ("J" evil-window-move-very-down)
     ("K" evil-window-move-very-up)
     ("L" evil-window-move-far-right)
     ("r" evil-window-rotate-downwards)
     ("R" evil-window-rotate-upwards)
     ("+" evil-window-increase-height)
     ("-" evil-window-decrease-height)
     (">" evil-window-increase-width)
     ("<" evil-window-decrease-width)
     ("=" balance-windows)
     ("v" evil-window-vsplit)
     ("s" evil-window-split)
     ("p" evil-window-mru)
     ("c" evil-window-delete)
     ("o" delete-other-windows :exit t)
     ("n" evil-window-new))
    :bind (:global-map ("M-6" . hydra-manage-frames/body))
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
    :global-minor-mode evil-mode
    )

  (leaf quickrun :ensure t)
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

  (defun my/ido-recentf ()
    "works with recentf-mode"
    (interactive)
    (find-file (ido-completing-read "Find from recent: " recentf-list)))
  )


(leaf minibuffer-extentions
  :doc "extend minibuffer functions"
  :config
  (leaf consult
    :ensure t
    :doc "Useful search and navigation commands"
    :hydra
    ((hydra-consult nil
      "
                 ^consult functions^
-----------------------------------------------------
 _v_irtual buffers   _s_earch        _m_odes
 _e_diting           _g_rep & find   _o_rg
 _r_egister          _c_ompilation   m_i_scellaneous
 _n_avigation        _h_istories
"
      ("v" hydra-consult:virtual-buffer/body :exit t)
      ("e" hydra-consult:editing/body        :exit t)
      ("r" hydra-consult:register/body       :exit t)
      ("n" hydra-consult:navigation/body     :exit t)
      ("s" hydra-consult:search/body         :exit t)
      ("g" hydra-consult:grep-find/body      :exit t)
      ("c" hydra-consult:compilation/body    :exit t)
      ("h" hydra-consult:histories/body      :exit t)
      ("m" hydra-consult:modes/body          :exit t)
      ("o" hydra-consult:org/body            :exit t)
      ("i" hydra-consult:miscellaneous/body  :exit t))

     (hydra-consult:virtual-buffer nil "
              ^consult virtual-buffer^
-----------------------------------------------------
 _b_uffer            other _f_rame     _r_ecent file
 _o_ther window      _B_ookmark
"
      ("b" consult-buffer       :exit t)
      ("o" consult-other-window :exit t)
      ("f" consult-other-frame  :exit t)
      ("B" consult-bookmark     :exit t)
      ("r" consult-recent-file  :exit t))

     (hydra-consult:editing nil "
                 ^consult editing^
-----------------------------------------------------
 _y_ank from kill ring            yank _r_eplace
 yank _p_op                       _k_macro
"
      ("y" consult-yank-from-kill-ring :exit t)
      ("p" consult-yank-pop            :exit t)
      ("r" consult-yank-replace        :exit t)
      ("k" consult-kmacro              :exit t))

     (hydra-consult:register nil "
                 ^consult register^
-----------------------------------------------------
 _r_egister        register _w_indow  register _s_tore
 register _l_oad   register _f_ormat
"
      ("r" consult-register        :exit t)
      ("l" consult-register-load   :exit t)
      ("w" consult-register-window :exit t)
      ("f" consult-register-format :exit t)
      ("s" consult-register-store  :exit t))

     (hydra-consult:navigation nil "
                ^consult navitation^
-----------------------------------------------------
 _g_oto line       global _M_ark      _i_menu
 _m_ark            _o_utline          _I_menu multi
"
      ("g" consult-goto-line   :exit t)
      ("m" consult-mark        :exit t)
      ("M" consult-global-mark :exit t)
      ("o" consult-outline     :exit t)
      ("i" consult-imenu       :exit t)
      ("I" consult-imenu-multi :exit t))

     (hydra-consult:search nil "
                  ^consult search^
-----------------------------------------------------
 _l_ine            i_s_earch          _k_eep lines
 line _m_ulti      _M_ulti occur      _f_ocus lines
"
      ("l" consult-line        :exit t)
      ("m" consult-line-multi  :exit t)
      ("s" consult-isearch     :exit t)
      ("M" consult-multi-occur :exit t)
      ("k" consult-keep-lines  :exit t)
      ("f" consult-focus-lines :exit t))

     (hydra-consult:grep-find nil "
                   ^consult find^
-----------------------------------------------------
 _g_rep            _G_it grep         _l_ocate
 _r_ipgrep         _f_ind
"
      ("g" consult-grep     :exit t)
      ("r" consult-ripgrep  :exit t)
      ("G" consult-git-grep :exit t)
      ("f" consult-find     :exit t)
      ("l" consult-locate   :exit t))

     (hydra-consult:compilation nil "
               ^consult compilation^
-----------------------------------------------------
 compile _e_rror       fly_m_ake             _x_ref
                       fly_c_heck
"
      ("e" consult-compile-error :exit t)
      ("m" consult-flymake       :exit t)
      ("c" consult-flycheck      :exit t)
      ("x" consult-xref          :exit t))

     (hydra-consult:histories nil "
                 ^consult histories^
-----------------------------------------------------
 _c_omplex command                  _h_istory
"
      ("c" consult-complex-command :exit t)
      ("h" consult-history         :exit t))

     (hydra-consult:modes nil "
                   ^consult modes^
-----------------------------------------------------
 _m_inor mode menu                  mode _c_ommand
                      [prefix]
 [i]on/[o]ff/[l]ocal/[g]lobal    [l]ocal-minor/[g]lobal-minor/[m]ajor
"
      ("m" consult-minor-mode-menu :exit t)
      ("c" consult-mode-command    :exit t))

     (hydra-consult:org nil
      "
                   ^consult modes^
-----------------------------------------------------
 org _h_eading                         org _a_genda
"
      ("h" consult-org-heading :exit t)
      ("a" consult-org-agenda  :exit t))

     (hydra-consult:miscellaneous nil "
                      ^consult cellaneous^
--------------------------------------------------------------------
 _a_propos          _t_heme             _c_ompletion in region
 _m_an              _p_review at point  completion read _M_ultiple
 _f_ile externally
"
      ("a" consult-apropos                  :exit t)
      ("m" consult-man                      :exit t)
      ("f" consult-file-externally          :exit t)
      ("t" consult-theme                    :exit t)
      ("p" consult-preview-at-point         :exit t)
      ("c" consult-completion-in-region     :exit t)
      ("M" consult-completion-read-multiple :exit t))
     )

    :bind
    (("M-g g"   . consult-line)
     ("M-g M-g" . consult-line) ;; replace native goto-line
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
     ("M-y"     . consult-yank-from-kill-ring)
     ("C-x b"   . consult-buffer) ; replace switch-to-buffer. narrowing with b/f/m/p
     ("<f5>"    . hydra-consult/body))
    :init
    (leaf consult-use-fd :if (executable-find "fd")
      :doc "use fd for find if avalable"
      :custom (consult-find-command . "fd --color=never --full-path ARG OPTS"))
    (leaf consult-use-rigrep :if (executable-find "rg")
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
    (leaf consult-dir :ensure t))

  (leaf orderless
    :ensure t
    :doc "Advanced completion style"
    :custom
    (completion-styles . '(orderless))
    (completion-category-defaults . nil)
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
    :init
    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)
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
      :custom (savehist-file . "~/.emacs.d/.cache/history")
      :init (savehist-mode))

    ;; Add prompt indicator to `completing-read-multiple'.
    ;; Alternatively try `consult-completing-read-multiple'.
    (defun crm-indicator (args)
      (cons (concat "[CRM] " (car args)) (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    (leaf hide-commads-unnecessary :emacs>= "28"
      :doc
      "Emacs 28: Hide commands in M-x which do not work in the current mode.
       Vertico commands are hidden in normal buffers."
      :config
      (setq read-extended-command-predicate
            #'command-completion-default-include-p))

    :custom
    (vertico-count . 15)
    (vertico-cycle . t)
    ;; Enable recursive minibuffers
    (enable-recursive-minibuffers . t)
    ;; Do not allow the cursor in the minibuffer prompt
    (minibuffer-prompt-properties . '(read-only t cursor-intangible t face minibuffer-prompt))
    :hook (minibuffer-setup-hook . cursor-intangible-mode))
  )


(leaf competion-linting
  :doc "completion and linting packages"
  :config
  (leaf auto-completion
    :tag "builtin"
    :config
    (setq-default ac-sources '(ac-source-filename
                               ac-source-words-in-all-buffer)))

  (leaf eldoc
    :ensure t
    :tag "builtin"
    :config
    (leaf eldoc-box :if (not (emacs-works-on-term-p))
      :ensure t
      :hook (eldoc-mode-hook . eldoc-box-hover-at-point-mode)
      :config
      (custom-set-faces ;; modify bg og eldoc-box-body
       `(eldoc-box-body ((t (:inherit 'default :background ,(doom-color 'bg-alt)))))))
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
    (leaf flycheck-posframe :if (not (emacs-works-on-term-p))
      :ensure t
      :hook (flycheck-mode-hook . flycheck-posframe-mode)
      :custom
      (flycheck-posframe-position       . 'window-bottom-right-corner)
      (flycheck-posframe-info-prefix    . "\x2139 ")
      (flycheck-posframe-warning-prefix . "\x26a0 ")
      (flycheck-posframe-error-prefix   . "\x274c "))

    (leaf consult-flycheck
      :ensure t
      :bind
      (:flycheck-mode-map
       :package flycheck
       ("C-M-l" . consult-flycheck))))

  (leaf company
    ;; company completion framework
    :ensure t
    :custom
    (company-idle-delay . 0)
    (company-selection-wrap-around . t)
    (completion-ignore-case . t)
    :config
    (leaf company-box
      :ensure t
      :hook (company-mode-hook . company-box-mode))
    :global-minor-mode global-company-mode)

  (leaf lsp-mode
    :doc "configure lsp"
    :ensure t
    :init (setq lsp-keymap-prefix "C-c C-l")
    ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l", "s-l" )
    :commands (lsp lsp-deferred)
    :custom
    (lsp-document-sync-method . nil) ;; always send incremental document
    (lsp-response-timeout . 5)
    (lsp-eldoc-enable-hover . nil)
    (lsp-auto-configure . t)
    (lsp-headerline-breadcrumb-icons-enable . nil)
    :bind (:lsp-mode-map ("C-c r"   . lsp-rename))
    :config
    (setq lsp-prefer-flymake nil)
    (custom-set-variables
     '(lsp-session-file
       (expand-file-name ".cache/.lsp-session-v1" user-emacs-directory)))

    (leaf lsp-ui
      :doc "LSP UI tools"
      :ensure t
      :custom
      ;; lsp-ui-doc
      (lsp-ui-doc-enable . t)
      (lsp-ui-doc-header . t)
      (lsp-ui-doc-include-signature . t)
      (lsp-ui-doc-position . 'top) ;; top, bottom, or at-point
      (lsp-ui-doc-max-width . 150)
      (lsp-ui-doc-max-height . 30)
      (lsp-ui-doc-use-childframe . t)
      (lsp-ui-doc-use-webkit . nil)
      ;; lsp-ui-flycheck
      (lsp-ui-flycheck-enable . t)
      ;; lsp-ui-sideline
      (lsp-ui-sideline-enable . t)
      (lsp-ui-sideline-ignore-duplicate . t)
      (lsp-ui-sideline-show-symbol . t)
      (lsp-ui-sideline-show-hover . t)
      (lsp-ui-sideline-show-diagnostics . t)
      (lsp-ui-sideline-show-code-actions . t)
      ;; lsp-ui-imenu
      (lsp-ui-imenu-enable . nil)
      (lsp-ui-imenu-kind-position . 'top)
      ;; lsp-ui-peek
      (lsp-ui-peek-enable . t)
      (lsp-ui-peek-peek-height . 20)
      (lsp-ui-peek-list-width . 50)
      (lsp-ui-peek-fontify . 'on-demand) ;; never, on-demand, or always
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
      (lsp-ui-mode-hook . (lambda () (flycheck-posframe-mode -1))) ; disable flycheck-posframe
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
      :require t
      :bind (:lsp-mode-map ("C-M-l" . consult-lsp-diagnostics)))
    ;; optionally
    ;; if you are helm user
    ;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
    ;; if you are ivy user
    ;;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
    ;; optionally if you want to use debugger
    ;;(use-package dap-mode)
    ;; (use-package dap-LANGUAGE) to load the dap adapter for your language
    )

  (leaf eglot
    :ensure t
    :doc "another lsp client"
    :preface
    (leaf eldoc-box-mode :if (not (emacs-works-on-term-p))
      :doc "use eldoc-box-mode if not on term"
      :hook (eglot--managed-mode-hook . eldoc-box-hover-mode)))
  )


(leaf helpful-things
  :config
  (leaf bind-key
    :doc "move frames with function-key"
    :bind ([f9] . other-frame))

  (leaf ace-window
    :ensure t
    :bind ("M-o" . ace-window))

  (leaf which-key
    :ensure t
    :config
    (leaf which-key-posframe
      :ensure t
      :custom
      (which-key-posframe-poshandler . 'posframe-poshandler-frame-top-left-corner))
    (which-key-mode)
    (if (emacs-works-on-term-p)
	    (which-key-setup-side-window-right-bottom)(which-key-posframe-mode))
    )

  (leaf undo-tree
    :ensure t
    :custom (undo-tree-visualizer-timestamps . t)
    :global-minor-mode (global-undo-tree-mode))

  (leaf vterm
    :ensure t
    :init
    (add-to-list 'evil-emacs-state-modes 'vterm-mode)
    :hook (vterm-mode-hook . (lambda () (sw-lnsp 1)))
    :bind (:vterm-mode-map ("C-u" . vterm-send-C-u)))
  )


(leaf git-related-packages
  :config
  (leaf git-gutter
    :ensure t
    :hook (prog-mode-hook . git-gutter-fix-init)
    :custom
    (git-gutter:modified-sign . " ")
    (git-gutter:added-sign    . "+")
    (git-gutter:deleted-sign  . "▶")
    (git-gutter:ask-p         . nil)
    :custom-face
    (git-gutter:modified . '((t (:foreground "DodgerBlue2"
                                 :background "DodgerBlue2"))))
    (git-gutter:added    . '((t (:foreground "dark slate"
                                 :background "SpringGreen2"))))
    (git-gutter:deleted  . '((t (:foreground  "tomato2"))))
    :config
    (defun git-gutter-fix-init ()
      (interactive)
      (if (eq git-gutter-mode nil)
          (git-gutter))
      (git-gutter))
    :hydra (hydra-git-gutter nil
            "git-gutter"
            ("j" git-gutter:next-hunk           "next")
            ("k" git-gutter:previous-hunk       "previous")
            ("G" git-gutter:end-of-hunk         "end")
            ("d" git-gutter:popup-hunk          "popup")
            ("v" git-gutter:mark-hunk           "mark")
            ("x" git-gutter:revert-hunk         "revert")
            ("s" git-gutter:stage-hunk          "stage")
            ("g" git-gutter-fix-init            "fix init")
            ("r" git-gutter:update-all-windows  "update all windows")
            ("M" magit                          "enter magit" :exit t))
    :bind (:global-map
           ("C-x C-g" . hydra-git-gutter/body))
    :global-minor-mode (global-git-gutter-mode t))

  (leaf magit
    :ensure t
    ;:custom (magit-completing-read-function . 'magit-ido-completing-read)
    :config (add-hook 'magit-section-mode-hook #'(lambda () (whitespace-mode -1))))
  )


(leaf file-manage
  :doc "file management related config"
  :config
  (leaf dired
    :tag "builtin"
    :config (put 'dired-find-alternate-file 'disabled nil))

  (leaf neotree
    :ensure t
    :bind ([f7] . neotree-toggle)
    :custom (neo-window-fixed-size . nil)
    :config
    ;; keymap for using with evil mode
    (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
    (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
    (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
    (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle))

  (leaf projectile
    :ensure t
    :config (projectile-mode +1))

  (leaf treemacs
    :ensure t
    ;; :defer t
    ;; :require treemacs
    :init
    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
    :bind
    (("M-0"       . treemacs-select-window)
     ("C-x t 1"   . treemacs-delete-other-windows)
     ("C-x t t"   . treemacs)
     ("C-x t B"   . treemacs-bookmark)
     ("C-x t C-t" . treemacs-find-file)
     ("C-x t M-t" . treemacs-find-tag))
    :config
    (leaf treemacs-custom :after treemacs
      :doc "eval after treemacs load"
      :custom
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
      :config ;; use expression to set variables
      (custom-set-variables
       '(treemacs-collapse-dirs                 (if treemacs-python-executable 3 0))
       '(treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory))))
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
      :init
      (custom-set-variables
       '(projectile-cache-file
         (expand-file-name ".cache/projectile.cache" user-emacs-directory))
       '(projectile-known-projects-file
         (expand-file-name ".cache/projectile-known-projects.eld" user-emacs-directory))
       ))

    (leaf treemacs-all-the-icons
      :after treemacs all-the-icons
      :ensure t :require t)

    (leaf treemacs-icons-dired
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


(leaf conf-appearance-on-state
  :doc "switch appearance when on gui or term"
  :require treemacs doom-modeline
  :config
  (defun emacs-on-term ()
    "switch emacs appearance for term with doom-spacegrey"
    (interactive)
    (load-theme 'doom-monokai-one t)
    (custom-set-variables '(doom-modeline-icon nil))
    (treemacs-load-theme "Default"))

  (defun emacs-on-gui ()
    "switch emacs appearance for gui with doom-molokai and modeline icons"
    (interactive)
    (load-theme 'doom-monokai-one t)
    (custom-set-variables '(doom-modeline-icon t))
    (treemacs-load-theme "all-the-icons"))

  (if (emacs-works-on-term-p)
      (emacs-on-term) (emacs-on-gui))
  )


(leaf load-lang-settings
  :config (load "~/.emacs.d/elisp/langs.el"))


(leaf load-local-conf
  :config (if (file-exists-p "~/.emacs.d/elisp/local.el")
              (load "~/.emacs.d/elisp/local.el")
              ;; create elisp/local.el if not exists
              (with-temp-file "~/.emacs.d/elisp/local.el" nil)))
