;;; conf-elfeed.el --- elfeed conf -*- lexical-binding: t -*-
;;; Commentary:
;;
;; configure elfeed
;;

;;; Code:


(leaf elfeed
  :ensure t
  :url "https://github.com/skeeto/elfeed"
  :preface (defconst elfeed-dir-path "~/.config/elfeed/" "elfeed config path")
  :hook
  ((elfeed-show-mode-hook
    elfeed-search-mode-hook) . rdm/text-scale-adjust)
  (elfeed-search-mode-hook . (lambda () (rdm/sw-lnsp 0.75)))
  :init
  (leaf elfeed-org :if (file-exists-p (expand-file-name "elfeed.org" elfeed-dir-path))
    :ensure t
    :url "https://github.com/remyhonig/elfeed-org"
    :init (elfeed-org)
    :custom
    `((rmh-elfeed-org-files . `,(list (expand-file-name "elfeed.org" elfeed-dir-path)))))

  (leaf *patch-elfeed-faces :after doom-themes
    :hook
    ((elfeed-search-update-hook
      after-load-theme-hook) . (lambda ()
                                 (face-remap-add-relative 'hl-line
                                                          `(:background ,(doom-blend 'highlight 'bg-alt 0.30)))
                                 (set-face-attribute 'elfeed-search-tag-face nil :foreground `,(doom-color 'green))
                                 (dolist (face '(elfeed-search-tag-face
                                                 elfeed-search-date-face
                                                 elfeed-search-feed-face
                                                 elfeed-search-title-face
                                                 elfeed-search-unread-title-face))
                                   (set-face-attribute
                                    face nil :family font-for-tables :height 1.0)))))

  :config
  (leaf *elfeed-patch-entry-switch
    :doc "patch entry-switch behavior"
    :custom
    (elfeed-show-entry-switch . #'display-buffer)
    :config
    (defun rdm/elfeed-show-next ()
      (interactive)
      (let ((elfeed-show-entry-switch #'switch-to-buffer))
        (elfeed-show-next)))

    (defun rdm/elfeed-show-prev ()
      (interactive)
      (let ((elfeed-show-entry-switch #'switch-to-buffer))
        (elfeed-show-prev)))

    (evil-define-key 'normal elfeed-show-mode-map
      (kbd "C-j") 'rdm/elfeed-show-next
      "]]"        'rdm/elfeed-show-next
      "gj"        'rdm/elfeed-show-next
      (kbd "C-k") 'rdm/elfeed-show-prev
      "[["        'rdm/elfeed-show-prev
      "gk"        'rdm/elfeed-show-prev)
    )

  (leaf *elfeed-func-def-bind
    :doc "custom defined func"
    :config
    (defun rdm/elfeed-search-toggle-star ()
      "add/remove star tag on elfeed entry"
      (interactive)
      (apply 'elfeed-search-toggle-all '(star)))

    (defun rdm/elfeed-search-tag-later-unread (entry)
      "add later and unread tag on elfeed entry"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (elfeed-tag entry 'unread 'later)
        (elfeed-search-update-entry entry)
        (unless elfeed-search-remain-on-entry (forward-line))))

    (defun rdm/elfeed-search-tag-junk (entry)
      "add/remove junk tag on elfeed entry"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (elfeed-untag entry 'unread)
        (elfeed-tag entry 'junk)
        (elfeed-search-update-entry entry)
        (unless elfeed-search-remain-on-entry (forward-line))))

    (defun rdm/elfeed-show-entry-share (&optional use-generic-p)
      "Copy the entry title and URL as org link to the clipboard."
      (interactive "P")
      (let* ((link (elfeed-entry-link elfeed-show-entry))
             (title (elfeed-entry-title elfeed-show-entry))
             (feed-info (concat title "\n" link)))
        (when feed-info
          (kill-new feed-info)
          (x-set-selection 'PRIMARY feed-info)
          (message "Yanked: %s" feed-info))))

    (defun rdm/elfeed-search-entry-share (entry)
      "Copy the entry title and URL as org link to the clipboard."
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (let* ((link (elfeed-entry-link entry))
               (title (elfeed-entry-title entry))
               (feed-info (concat title "\n" link)))
          (when feed-info
            (kill-new feed-info)
            (x-set-selection 'PRIMARY feed-info)
            (message "Yanked: %s" feed-info)))))

    (defun rdm/elfeed-unread-p (entry)
      "returns t if selected entry has unread tag"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (elfeed-tagged-p 'unread entry))

    (defun rdm/elfeed-search-mark-read (entry)
      "mark entry read and remove later tag, then update elfeed-search"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (elfeed-untag entry 'unread 'later)
      (elfeed-search-update-entry entry))

    (defun rdm/elfeed-search-mark-read-show (entry)
      "Mark entry as read and show"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (elfeed-show-entry entry)
      (rdm/elfeed-search-mark-read entry))

    (defun rdm/elfeed-search-untag-later-unread (entry)
      "Remove the 'unread' and 'later' tag from entry.
based on elfeed-search-show-entry"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (rdm/elfeed-search-mark-read entry)
        (unless elfeed-search-remain-on-entry (forward-line))))

    (defun rdm/elfeed-search-untag-unread (entry)
      "Remove the 'unread' tag from entry and recenter.
based on elfeed-search-show-entry"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (rdm/elfeed-search-mark-read entry)
        (forward-line) (recenter)))

    (defun rdm/elfeed-search-show-entry (entry)
      "Display the currently selected item on elfeed-show buffer.
without forwarding line and mark as read"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (require 'elfeed-show)
      (when (elfeed-entry-p entry)
        (elfeed-search-update-entry entry)
        (elfeed-show-entry entry)))

    (defun rdm/elfeed-search-show-read-entry ()
      "Display the currently selected item in a buffer.
based on elfeed-search-show-entry.
  open first line if unread, if not show next and mark unrerad.
"
      (interactive)
      (if (and (eq (line-number-at-pos (point)) 1)
               (call-interactively #'rdm/elfeed-unread-p))
          (call-interactively #'rdm/elfeed-search-mark-read-show)
        (progn
          (call-interactively #'rdm/elfeed-search-mark-read)
          (forward-line) (recenter)
          (call-interactively #'rdm/elfeed-search-show-entry))))

    (defun rdm/elfeed-search-show-read-prev-entry ()
      "Display currently selected item in buffer.
based on elfeed-search-show-entry"
      (interactive)
      (forward-line -1) (recenter)
      (call-interactively #'rdm/elfeed-search-mark-read-show))

    (defun rdm/elfeed-search-browse-url (&optional use-generic-p)
      "open with browser, untag unread and later.
based on elfeed-search-browse-url"
      (interactive "P")
      (let ((buffer (current-buffer))
            (entries (elfeed-search-selected)))
        (cl-loop for entry in entries
                 do (elfeed-untag entry 'unread 'later)
                 when (elfeed-entry-link entry)
                 do (if use-generic-p
                        (browse-url-generic it)
                      (browse-url it)))
        ;; `browse-url' could have switched to another buffer if eww or another
        ;; internal browser is used, but the remainder of the functions needs to
        ;; run in the elfeed buffer.
        (with-current-buffer buffer
          (mapc #'elfeed-search-update-entry entries)
          (unless (or elfeed-search-remain-on-entry (use-region-p))
            (forward-line)))))

    (defun rdm/elfeed-search-filter-feed-name (entry)
      "filter unread via highlighted feed address"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (let* ((meta (elfeed-entry-feed entry))
               (title (elfeed-meta meta :title))
               (title-esc (replace-regexp-in-string " " "\\s-" title t t))
               (filter-str (concat "=" title-esc)))
          (when filter-str
            (elfeed-search-set-filter (concat filter-str " @3-months-ago +unread -later"))))))

    ;; eww
    (defun rdm/elfeed-show-eww-open (&optional use-generic-p)
      "open with eww"
      (interactive "P")
      (let ((browse-url-browser-function #'eww-browse-url))
        (elfeed-show-visit use-generic-p)))

    (defun rdm/elfeed-search-eww-open (&optional use-generic-p)
      "open with eww"
      (interactive "P")
      (let ((browse-url-browser-function #'eww-browse-url))
        (elfeed-search-browse-url use-generic-p)))

    ;; w3m
    (defun rdm/elfeed-search-w3m-open (&optional use-generic-p)
      "open with w3m browser"
      (interactive "P")
      (if (fboundp 'w3m-browse-url)
          (let ((browse-url-browser-function #'w3m-browse-url))
            (elfeed-search-browse-url use-generic-p))
        (warn "w3m-mode not found")))

    (defun rdm/elfeed-show-w3m-open (&optional use-generic-p)
      "open with eww"
      (interactive "P")
      (if (fboundp 'w3m-browse-url)
          (let ((browse-url-browser-function #'w3m-browse-url))
            (elfeed-show-visit use-generic-p))
        (warn "w3m-mode not found")))

    ;; xwidget-webkit
    (defun rdm/elfeed-search-webkit-open (&optional use-generic-p)
      "open with xwidget webkit browser"
      (interactive "P")
      (if (fboundp 'xwidget-webkit-browse-url)
          (progn
            (let ((browse-url-browser-function #'xwidget-webkit-browse-url))
              (elfeed-search-browse-url use-generic-p))
            ;; switch buffer
            (dolist (buff (buffer-list))
              (with-current-buffer buff
                (when (eq major-mode
                          #'xwidget-webkit-mode)
                  (switch-to-buffer buff)))))
        (warn "xwidget-webkit-mode seems not available")
        ))

    (defun rdm/elfeed-show-webkit-open (&optional use-generic-p)
      "open with xwidget webkit browser"
      (interactive "P")
      (if (fboundp 'xwidget-webkit-browse-url)
          (progn
            (let ((browse-url-browser-function #'xwidget-webkit-browse-url))
              (elfeed-show-visit use-generic-p))
            ;; switch buffer
            (dolist (buff (buffer-list))
              (with-current-buffer buff
                (when (eq major-mode
                          #'xwidget-webkit-mode)
                  (switch-to-buffer buff)))))
        (warn "xwidget-webkit-mode seems not available")))

    (evil-define-key 'normal elfeed-search-mode-map
      "m"  'rdm/elfeed-search-toggle-star
      "l"  'rdm/elfeed-search-tag-later-unread
      "b"  'rdm/elfeed-search-eww-open
      "B"  'rdm/elfeed-search-webkit-open
      "u"  'rdm/elfeed-search-untag-later-unread
      "Y"  'rdm/elfeed-search-entry-share
      "F"  'rdm/elfeed-search-filter-feed-name
      "f"  'hydra-elfeed-search-filter/body
      "ta" 'elfeed-search-tag-all
      "tr" 'elfeed-search-untag-all
      "tj" 'rdm/elfeed-search-tag-junk
      "go" 'elfeed-search-browse-url
      "oo" 'rdm/elfeed-search-mark-read-show
      (kbd "RET") 'rdm/elfeed-search-show-read-entry
      (kbd "SPC") 'rdm/elfeed-search-show-read-entry
      (kbd "S-SPC") 'rdm/elfeed-search-show-read-prev-entry
      (kbd "C-j") 'rdm/elfeed-search-show-read-entry
      (kbd "M-j") 'rdm/elfeed-search-show-read-entry
      "]]"        'rdm/elfeed-search-show-read-entry
      "gj"        'rdm/elfeed-search-show-read-entry
      (kbd "C-k") 'rdm/elfeed-search-show-read-prev-entry
      (kbd "M-k") 'rdm/elfeed-search-show-read-prev-entry
      "[["        'rdm/elfeed-search-show-read-prev-entry
      "gk"        'rdm/elfeed-search-show-read-prev-entry
      )

    (evil-define-key 'normal elfeed-show-mode-map
      "b"  'rdm/elfeed-show-eww-open
      "Y"  'rdm/elfeed-show-entry-share
      "B"  'rdm/elfeed-show-webkit-open
      "go" 'elfeed-show-visit
      "ta" 'elfeed-show-tag
      "tr" 'elfeed-show-untag)
    )

  :custom
  (elfeed-db-directory           . "~/.config/elfeed/.db")
  (elfeed-enclosure-default-dir  . elfeed-dir-path)
  (elfeed-search-filter          . "@1-months-ago +unread -later -junk")
  (elfeed-search-title-max-width . 95)
  (elfeed-search-date-format     . '("%Y-%m-%d (%a) %k:%M" 22 :left))

  :hydra
  (hydra-elfeed-search-filter (nil nil)
                              "elfeed filters"
                              ("u" (elfeed-search-set-filter "@4-weeks-ago +unread -junk")  "all unread")
                              ("a" (elfeed-search-set-filter "@4-weeks-ago -junk")  "all entries")
                              ("n" (elfeed-search-set-filter "@4-weeks-ago +news +unread -later -junk")  "news")
                              ("N" (elfeed-search-set-filter "@4-weeks-ago +news -junk")  "news(all)")
                              ("l" (elfeed-search-set-filter "+later +unread")  "later")
                              ("L" (elfeed-search-set-filter "+later -unread")  "later(read)")
                              ("c" (elfeed-search-set-filter "@6-weeks-ago +comic +unread")  "comic")
                              ("C" (elfeed-search-set-filter "@6-weeks-ago +comic -unread")  "comic(all)")
                              ("v" (elfeed-search-set-filter "@4-weeks-ago +YouTube +unread -later") "YouTube")
                              ("V" (elfeed-search-set-filter "@4-weeks-ago +YouTube") "YouTube(all)")
                              ("s" (elfeed-search-set-filter "star")  "starred")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conf-elfeed.el ends here
