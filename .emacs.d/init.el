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
    :init
    (setq exec-path-from-shell-arguments (list "-l"))
    :config
    (when (daemonp) (exec-path-from-shell-initialize)))
  )

(leaf set-appearance
  :custom
  (inhibit-splash-screen . t)
  (ring-bell-function . 'ignore)
  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (display-time)
  (column-number-mode t)
  (blink-cursor-mode 0)
  (which-function-mode 1)
  (transient-mark-mode 1)
  (setq default-frame-alist (append (list '(font . "HackGen35Nerd Console-14.1")
                                          '(width . 80)
                                          '(height . 55)) default-frame-alist))
  (setq font-for-tables "HackGenNerd Console")

  (leaf set-linespacing
    :doc "control line spacing"
    :config
    (setq-default line-spacing 0.25)
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
    :config
    (set-face-attribute 'show-paren-match nil
                        :background "SpringGreen2")
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

  (leaf all-the-icons :ensure t) ;; need installation by all-the-icons-install-fonts

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

  ;; configure whitespace
  (leaf whitespace
    :hook
    ((conf-mode-hook
      outline-mode-hook
      prog-mode-hook
      text-mode-hook) . whitespace-mode)
    :custom
    (whitespace-global-modes . '(not dired-mode tar-mode neotree))
    (whitespace-line-column . 80)
    (whitespace-style . '(face ;; enable
                          trailing
                          tabs ;; conflicts highlight indent mode
                          space-mark
                          tab-mark
                          ;;newline
                          ;;newline-mark
                          ;;empty  ; empty line
                          ;;lines-tail
                          ;;spaces
                          ))
    (whitespace-display-mappings . '((tab-mark ?\t [?\xBB ?\t] [?\\ ?\t])
                                     (newline-mark ?\n [?\x21B2 ?\n]) ;; display when in some major mode
                                     ))
    :config
    ;; fix color
    (set-face-attribute whitespace-trailing nil
                        :foreground "DeepPink"
                        :background (face-attribute 'default :background)
                        :underline t)
    (set-face-attribute whitespace-tab nil
                        :foreground "gray22"
                        :background nil
                        :underline t)
    ;;(global-whitespace-mode t)
    :global-minor-mode global-whitespace-mode
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
    )

  ;; ---------- loading doom theme  ----------
  (load "~/.emacs.d/elisp/doom.el")
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
    :custom
    (auto-save-file-name-transforms . '((".*" "~/.emacs.d/.cache/autosaved" t)))
    (delete-auto-save-files . t)
    (auto-save-default . t)
    (auto-save-timeout . 15)
    (auto-save-interval . 120)
    (auto-save-list-file-prefix . nil))

  (leaf backup-files
    :doc "make backup files"
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
    :custom
    (message-log-maxa . 10000)
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


(leaf editor-defaults
  :bind ([f9] . other-frame)
  :config
  ;; configure indent tab to false as default
  (setq-default tab-width 4 indent-tabs-mode nil)
  (setq eol-mnemonic-dos "(CRLF)")
  (setq eol-mnemonic-mac "(CR)")
  (setq eol-mnemonic-unix "(LF)")
  (setq find-file-visit-truename t)
  (setq-default truncate-lines t)
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
  )

(leaf ido
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


;; --------------- minor modes ---------------
(load "~/.emacs.d/elisp/minors.el")

;; --------------- lsp package ---------------
(load "~/.emacs.d/elisp/lsp.el")

;; --------------- major modes ---------------
(load "~/.emacs.d/elisp/majors-s.el")

;; --------------- major modes ---------------
(load "~/.emacs.d/elisp/majors.el")

;; ------------ loading local.el  ------------
(if (file-exists-p "~/.emacs.d/elisp/local.el")
  (load "~/.emacs.d/elisp/local.el")
  ;; create elisp/local.el if not exists
  (with-temp-file "~/.emacs.d/elisp/local.el" nil))
