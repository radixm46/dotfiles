;;; conf-org.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;
;; org-mode configurations
;;

;;; Code:

(leaf org :after nerd-icons
  :doc "org-mode config"
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :custom
  ;; priority
  (org-priority-highest               . 1)
  (org-priority-lowest                . 4)
  (org-priority-default               . 3)
  ;; keywords
  (org-refile-targets                 . '((org-agenda-files :maxlevel . 3)))
  (org-todo-keywords                  . '((sequence "TODO(t)" "NEXT(n)" "SOMEDAY(s)" "WAITING(w@/!)" "PROJ(p)" "|"
                                                    "DONE(d!)" "CANCELLED(c@)")))
  (org-capture-templates              . `( ;; NOTE: require material icons
                                          ("t" ,(format "%s  Task to Inbox" (nerd-icons-faicon "nf-fa-check"))
                                           entry (file+headline org-todofile "Inbox")
                                           "-*- TODO %?\nEntered on %U\n%a"
                                           :empty-lines-before 1) ; %u->%t
                                          ("n" ,(format "%s  Note to Inbox" (nerd-icons-faicon "nf-fa-sticky_note"))
                                           entry (file+headline "" "Inbox")
                                           "-*- %?\nEntered on %U\n %i\n%a"
                                           :empty-lines-before 1)
                                          ))
  (org-log-done                       . 'time)
  (org-clock-clocked-in-display       . 'frame-title)
  (org-list-allow-alphabetical        . t)
  (org-startup-indented               . t)
  ;; for org src
  (org-src-preserve-indentation       . t)
  ;; Edit settings
  (org-auto-align-tags                . t)
  (org-tags-column                    . 75)
  (org-catch-invisible-edits          . 'show-and-error)
  (org-special-ctrl-a/e               . nil)
  (org-insert-heading-respect-content . nil)
  ;; agenda filter preset
  (org-agenda-tag-filter-preset       . '("-agenda_ignore"))
  (org-agenda-include-diary           . t)
  :init
  (leaf *font-remap-org-mode
    :hook (org-mode-hook . remap-font-to-doc)
    :preface (push #'org-mode  remap-font-to-doc-modes-list)
    )

  (leaf *font-remap-org-agenda-mode
    :doc "for better alignment"
    :hook (org-agenda-mode-hook . remap-font-to-term)
    :preface (push #'org-mode  remap-font-to-term-modes-list))

  (leaf evil-org
    :ensure t
    :hook
    (org-mode-hook . evil-org-mode)
    :preface
    (evil-set-initial-state 'org-agenda-mode 'motion)
    :init
    (require 'evil-org-agenda)
    (add-hook 'org-agenda-mode-hook #'evil-org-agenda-set-keys)
    :config
    ;; enable evil-org features
    (evil-org-set-key-theme
     '(textobjects insert navigation additional shift todo heading))
    (leaf *patch-evil-org-ctrl-behavior
      :hook
      (conf-on-term-hook . (lambda () (setq evil-want-C-i-jump nil)))
      (conf-on-gui-hook  . (lambda () (setq evil-want-C-i-jump t)))
      )
    )

  :config
  (leaf *org-config-directory
    :if (file-directory-p (expand-file-rec '("org" "orgfiles") (getenv "HOME")))
    :custom
    `(
      (org-directory              . ,(expand-file-rec '("org" "orgfiles") (getenv "HOME")))
      (org-default-notes-file     . ,(expand-file-name
                                      "notes.org" (expand-file-rec '("org" "orgfiles") (getenv "HOME"))))
      (org-agenda-files           . `,(directory-files
                                       (expand-file-rec '("org" "orgfiles") (getenv "HOME")) t ".org$" t))
      )
    :config
    (defvar org-todofile
      (expand-file-name "todo.org" org-directory)
      "default org todo file path"))

  (leaf *org-habits
    :doc "enable `org-habit'"
    :preface (add-to-list 'org-modules 'org-habit)
    :custom
    `((org-habit-completed-glyph . ,(string-to-char "-"))
      (org-habit-today-glyph     . ,(string-to-char "O"))
      (org-habit-following-days  . 7)
      (org-habit-preceding-days  . 28)
      (org-habit-show-all-today  . t)))

  (leaf *org-num-mode
    :doc "configure org-num minor mode"
    :custom
    (org-num-skip-tags       . nil)
    (org-num-skip-commented  . t)
    (org-num-skip-footnotes  . t)
    (org-num-skip-unnumbered . t))

  (defun rdm/org-goto-dir ()
    "open dir '~/org' with dired"
    (interactive)
    (find-file "~/org"))

  (leaf *org-appearance
    :custom
    (org-startup-truncated          . t)
    (org-startup-folded             . nil)
    (org-image-actual-width         . nil)
    (org-hide-emphasis-markers      . t)
    (org-pretty-entities            . nil)
    (org-ellipsis                   . "…")
    (org-agenda-block-separator     . ?─)
    (org-agenda-time-grid           . '((daily today require-timed)
                                        (800 1000 1200 1400 1600 1800 2000)
                                        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
    (org-agenda-current-time-string . "⭠ now ─────────────────────────────────────────────────")
    (org-agenda-prefix-format       . '((agenda . " %i %-12:c%?-12t%-6e% s")
                                        (todo   . " %i %-12:c %-6e")
                                        (tags   . " %i %-12:c")
                                        (search . " %i %-12:c")))
    (org-columns-default-format     . "%60ITEM(Task) %TODO %TAGS %6Effort(Estim){:} %CLOCKSUM(ActTime)")
    (org-agenda-columns-add-appointments-to-effort-sum . t)

    :config
    (leaf *org-latex-preview-svg :if (executable-find "dvisvgm") :disabled t
      :doc "if dvisvgm available, startup with latex preview"
      :custom
      (org-preview-latex-default-process . 'dvisvgm)
      (org-startup-with-latex-preview    . nil)
      :config
      (customize-set-variable
       'org-format-latex-options (plist-put org-format-latex-options :scale 1.4))
      )

    (leaf *org-latex-preview-imagemagick :if (executable-find "imagemagick")
      :doc "use imagemagick for latex preview"
      :require ob-latex
      :after org
      :custom
      (org-latex-create-formula-image-program . 'imagemagick)
      (org-startup-with-latex-preview         . nil)
      :config
      (customize-set-variable
       'org-format-latex-options (plist-put org-format-latex-options :scale 1.6))
      (customize-set-variable
       'org-format-latex-options (plist-put org-format-latex-options :background "Transparent"))
      (add-to-list 'org-babel-load-languages '(latex . t)))

    (leaf *patch-org-mode-header-size
      :doc "set larger face for org-level-1 to 5"
      :preface
      (defun patch-org-mode-header-size ()
        (set-face-attribute 'org-document-title nil :height 2.0)
        (set-face-attribute 'org-level-1 nil :height 1.75)
        (set-face-attribute 'org-level-2 nil :height 1.50)
        (set-face-attribute 'org-level-3 nil :height 1.25)
        (set-face-attribute 'org-level-4 nil :height 1.1)
        (set-face-attribute 'org-level-5 nil :height 1.0))
      :hook
      (after-load-theme-hook . patch-org-mode-header-size)
      )

    (leaf *patch-org-mode-faces :after doom-themes
      :preface
      (defun patch-org-mode-face ()
        (custom-set-faces
         `(org-hide             ((t (:foreground ,(doom-color 'bg))))) ; org-hide
         `(org-table            ((t :family ,font-for-tables)))       ; use 1:2 font for table
         `(org-column           ((t :family ,font-for-tables)))       ; use 1:2 font for column
         `(org-ellipsis         ((t (:background ,(doom-color 'bg)))))
         `(org-block-begin-line ((t (:background ,(doom-color 'bg)))))
         `(org-block-end-line   ((t (:background ,(doom-color 'bg)))))
         ))
      :hook ;; without `doom-color': (face-attribute 'default :background)
      ((after-load-theme-hook
        after-load-theme-hook) . patch-org-mode-face)
      )

    (leaf org-bullets :disabled t
      :ensure t
      :doc "pretty looking bullet list"
      :hook (org-mode-hook . org-bullets-mode))

    (leaf *patch-org-todo-faces :after doom-themes
      :doc  "prettify todo keywords"
      :preface
      (defun patch-org-todo-faces ()
        (customize-set-variable
         'org-todo-keyword-faces
         `(("TODO"    . ((t (:foreground ,(doom-color 'bg) :box '(:line-width (0 . -4)) :background ,(doom-color 'green)))))
           ("NEXT"    . ((t (:foreground ,(doom-color 'bg) :box '(:line-width (0 . -4)) :background ,(doom-color 'blue)))))
           ("SOMEDAY" . ((t (:foreground ,(doom-color 'bg) :box '(:line-width (0 . -4)) :background ,(doom-color 'orange)))))
           ("WAITING" . ((t (:foreground ,(doom-color 'bg) :box '(:line-width (0 . -4)) :background ,(doom-color 'yellow)))))
           ("PROJ"    . ((t (:foreground ,(doom-color 'bg) :box '(:line-width (0 . -4)) :background ,(doom-color 'magenta))))))))
      :hook
      (after-load-theme-hook . patch-org-todo-faces))

    (leaf org-modern
      :straight
      (org-modern :type git :host github
                  :repo "minad/org-modern" :branch "main")
      :doc "better looking org mode"
      :hook
      (org-mode-hook . org-modern-mode)
      :custom
      ;;org-modern custom
      (org-modern-todo            . nil)
      (org-modern-label-border    . 4)
      (org-modern-table           . nil)
      (org-modern-horizontal-rule . nil)
      )
    )

  ;; (setq system-time-locale "C") ; dates written in eng

  (leaf ob-mermaid
    :doc "configure mermaid for org babel"
    :ensure t
    :custom `(ob-mermaid-cli-path . ,(executable-find "mmdc"))
    :config (add-to-list 'org-babel-load-languages '(mermaid . t)))

  (leaf org-pomodoro
    :doc "configure org-pomodoro"
    :ensure t
    :custom
    (org-pomodoro-ask-upon-killing   . t)
    (org-pomodoro-format             . "🍅 %s")
    (org-pomodoro-short-break-format . "☕ %s")
    (org-pomodoro-long-break-format  . "🥞 %s")
    )

  (leaf org-sidebar
    :doc "sidebar for org-mode"
    :ensure t
    )

  (leaf *configure-bibtex
    :defun reftex-parse-all
    :config
    (defun org-mode-reftex-setup ()
      "setup reftex on org-mode doc"
      (load-library "reftex")
      (and (buffer-file-name)
           (file-exists-p (buffer-file-name))
           (reftex-parse-all))
      (define-key org-mode-map (kbd "C-c )") 'reftex-citation))
   )
  ;; (use-package ox-bibtex)

  (leaf org-ref :ensure t)

  (leaf org-roam
    :ensure t
    :pre-setq (org-roam-v2-ack . t)
    :bind
    (("C-c n c" . org-roam-capture)
     ("C-c n f" . org-roam-find-file)
     ("C-c n g" . org-roam-ui-mode)
     (:org-mode-map
      ("C-c n n" . org-roam-node-insert)
      ("C-c n i" . org-roam-insert)
      ("C-c n I" . org-roam-insert-immediate)
      ("C-c n a" . org-roam-alias-add)
      ("C-c n A" . org-roam-alias-remove)
      ("C-c n t" . org-roam-tag-add)
      ("C-c n T" . org-roam-tag-remove)))
    :hydra
    (hydra-org-roam
     (:hint nil)
     (format "\
^^^                           %s org roam^^^
^^^^^^--------------------------------------------------------------------------
 _c_:   capture              _f_:   find file            _g_:   ui mode
^^^^^^..........................................................................
 _i_:   insert               _I_:   insert immediate

 ^node^                      ^alias^                     ^tag^
 _n i_: insert               _a a_: add                  _t a_: add
 _n f_: find                 _a r_: remove               _t r_: remove
 _n v_: visit
 _n r_: random

 _SPC_: BACK
" (nerd-icons-faicon "nf-fa-file_text_o"))
     ("c"   org-roam-capture)
     ("f"   org-roam-find-file)
     ("g"   org-roam-ui-mode)
     ("i"   org-roam-insert)
     ("I"   org-roam-insert-immediate)
     ("n i" org-roam-node-insert)
     ("n f" org-roam-node-find)
     ("n v" org-roam-node-visit)
     ("n r" org-roam-node-random)
     ("a a" org-roam-alias-add)
     ("a r" org-roam-alias-remove)
     ("t a" org-roam-tag-add)
     ("t r" org-roam-tag-remove)
     ("SPC" hydra-org/body :exit t))
    :custom
    `(
      (org-roam-capture-templates . '(("d" "default" plain
                                       "%?"
                                       :if-new
                                       (file+head "${slug}.org" "#+title: ${title}\n")
                                       :immediate-finish t
                                       :unnarrowed t)
                                      ;; ("r" "bibliography reference" plain "%?"
                                      ;;  :if-new
                                      ;;  (file+head "references/${citekey}.org" "#+title: ${title}\n")
                                      ;;  :unnarrowed t)
                                      ;;("r" "Roam" plain (function org-roam--capture-get-point)
                                      ;;"%?"
                                      ;;:file-name "%<%Y%m%d%H%M%S>-${slug}.org"
                                      ;;:head "#+title: ${title}\n"
                                      ;;:unnarrowed t)
                                      ))
      (org-roam-directory         . ,(expand-file-rec '("org" "roam") (getenv "HOME")))
      (org-roam-index-file        . ,(expand-file-rec '("org" "roam" "Index.org") (getenv "HOME")))
      (org-roam-db-location       . ,(cache-sub-file  "org-roam.db"))
      )
    :hook (org-roam-capture-new-node-hook . org-roam-db-sync)
    :config
    (org-roam-db-autosync-mode)

    (leaf org-roam-ui :after org-roam
      :doc "org-roam frontend package"
      :straight
      (org-roam-ui :type git :host github
                   :repo "org-roam/org-roam-ui" :branch "main"
                   :files ("*.el" "out"))
      :custom
      (org-roam-ui-sync-theme     . t)
      (org-roam-ui-follow         . t)
      (org-roam-ui-update-on-save . t)
      (org-roam-ui-open-on-start  . t)
      )
    )

  (leaf org-pdftools :after pdf-tools
    :doc "org mode for pdf-tools integration (contains org-noter)"
    :ensure t
    :hook (org-mode-hook . org-pdftools-setup-link)
    :custom
    `(
      (org-noter-notes-search-path . `(,(expand-file-rec '("org" "roam" "noter") (getenv "HOME"))))
      )
    )

  :hook
  ;; (org-mode-hook . org-mode-reftex-setup)

  :hydra
  ((hydra-org
    (:hint nil)
    (format "\
^^^                              %s org functions^^^
^^^^^^--------------------------------------------------------------------------------
 ^ ^                         ^link^                      ^agenda^
 _c_:   capture              _l s_: store                _a_:  ^^agenda (consult)
 ^ ^                         _l i_: insert (last)        _A_:  ^^agenda

^^^^^^............................... (in org mode) ..................................
 ^time stamp^                ^sidebar^                   ^move^ ^around^
 _t a_: active               _s t_: toggle               _h_:^ ^  heading (consult)
 _t A_: active (+time)       _s T_: toggle (tree)        _j_/_k_: visible heading
 _t i_: inactive             _s b_: with backlinks       _i_/_I_: item
 _t I_: inactive (+time)     ^   ^                       _b_/_B_: block
 ^   ^                       ^   ^                       _l_/_L_: link

 _m t_: timer                _m r_: roam                 _o_:^ ^  open org dir
" (nerd-icons-faicon "nf-fa-file_text_o"))
    ("c"   org-capture :exit t)
    ("A"   org-agenda :exit t)
    ("a"   consult-org-agenda :exit t)
    ("l s" org-store-link)
    ("l i" org-insert-last-stored-link)
    ("t a" (org-time-stamp nil) :exit t)
    ("t A" (org-time-stamp t) :exit t) ; with time
    ("t i" (org-time-stamp-inactive nil) :exit t)
    ("t I" (org-time-stamp-inactive t) :exit t) ; with time
    ("s t" org-sidebar-toggle)
    ("s T" org-sidebar-tree-toggle)
    ("s b" org-sidebar-backlinks)
    ("h"   consult-org-heading)
    ("o"   rdm/org-goto-dir)
    ("j"   org-next-visible-heading)
    ("k"   org-previous-visible-heading)
    ("i"   org-next-item)
    ("I"   org-previous-item)
    ("b"   org-next-block)
    ("B"   org-previous-block)
    ("l"   org-next-link)
    ("L"   org-previous-link)
    ("m t" hydra-org-timers/body :exit t)
    ("m r" hydra-org-roam/body :exit t)
    ;; enable evil folding on current hydra
    ("z a" evil-toggle-fold)
    ("z o" evil-open-fold)
    ("z O" evil-open-fold-rec)
    ("z m" evil-close-folds)
    ("z r" evil-open-folds)
    ("z c" evil-close-fold)
    ;; enable evil scroll line binding
    ("z t" evil-scroll-line-to-top)
    ("z z" evil-scroll-line-to-center)
    ("z b" evil-scroll-line-to-bottom))

   (hydra-org-timers
    (:hint nil) "
^^^                               org timer^^^
^^^^^^--------------------------------------------------------------------------------
 _t_: timer                  _c_: set timer              _p_: pomodoro
 _s_: start                  _I_: timer item             _P_: extend last pomodoro
 _e_: stop                   _C_: change (region)
 _i_: pause or continue
 _S_: show remaining

 _SPC_: BACK
"
    ("t" org-timer)
    ("c" org-timer-set-timer)
    ("s" org-timer-start)
    ("e" org-timer-stop)
    ("i" org-timer-pause-or-continue)
    ("S" org-timer-show-remaining-time)
    ("C" org-timer-change-times-in-region)
    ("I" org-timer-item)
    ("p" org-pomodoro)
    ("P" org-pomodoro-extend-last-clock)
    ("SPC" hydra-org/body :exit t))
   )
  :bind
  (("C-c l"     . org-store-link)
   ("C-c c"     . org-capture)
   ("C-c a"     . org-agenda)
   ("C-c b"     . org-iswitchb)
   ("C-c C-o 0" . rdm/org-goto-dir)
   ("M-6"       . hydra-org/body)
   ("<f6>"      . hydra-org/body))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conf-org.el ends here
