;;; conf-elfeed.el --- elfeed conf -*- lexical-binding: t -*-
;;; Commentary:
;;
;; configure elfeed
;;

;;; Code:
(eval-when-compile
  (load (expand-file-name "elisp/initpkg" user-emacs-directory))
  (!el-load "elisp/conf-evil"
            "elisp/util"
            "elisp/doom"
            "elisp/conf-fonts"
            "elisp/conf-ext-fronts")
  (defconst elfeed-dir-path (expand-file-name "~/.config/elfeed/")
    "Path of elfeed user dir."))
(require 'straight)
(require 'evil-core) ; for evil-define-key
(require 'hydra)
(require 'rdm/util)


(leaf elfeed-org
  :ensure t
  :url "https://github.com/remyhonig/elfeed-org"
  :commands elfeed-org
  :custom
  `((rmh-elfeed-org-files . `,(list (!expand-file-name "elfeed.org" elfeed-dir-path)))))

(leaf elfeed
  :ensure t
  :url "https://github.com/skeeto/elfeed"
  :when (file-exists-p (!expand-file-name elfeed-dir-path))
  :commands elfeed
  :hook
  (elfeed-show-mode-hook   . (lambda () (rdm/text-scale-adjust)))
  (elfeed-search-mode-hook . (lambda ()
                               (rdm/text-scale-adjust)
                               (rdm/sw-lnsp 0.75)))
  :config
  (elfeed-org)
  (leaf *patch-elfeed-faces
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
  )

(leaf *elfeed-conf :after elfeed
  :defun elfeed-search-set-filter
  :defvar elfeed-dir-path
  :custom
  `((elfeed-db-directory           . ,(!expand-file-name ".db" elfeed-dir-path))
    (elfeed-enclosure-default-dir  . ,(!expand-file-name elfeed-dir-path))
    (elfeed-search-filter          . "@1-months-ago +unread -later -junk")
    (elfeed-search-title-max-width . 95)
    (elfeed-search-date-format     . '("%Y-%m-%d (%a) %k:%M" 22 :left)))

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

    :defun elfeed-show-next elfeed-show-prev
    :defvar elfeed-show-mode-map elfeed-show-entry-switch)

  (leaf *elfeed-func-def
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

    (defun rdm/elfeed-show-entry-share (&optional _use-generic-p)
      "Copy the entry title and URL as org link to the clipboard."
      (interactive "P")
      (let* ((link (elfeed-entry-link elfeed-show-entry))
             (title (elfeed-entry-title elfeed-show-entry))
             (feed-info (concat title "\n" link)))
        (when feed-info
          (kill-new feed-info)
          (gui-set-selection 'PRIMARY feed-info)
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
            (gui-set-selection 'PRIMARY feed-info)
            (message "Yanked: %s" feed-info)))))

    (defsubst rdm/elfeed-unread-p (entry)
      "returns t if selected entry has unread tag"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (elfeed-tagged-p 'unread entry))

    (defsubst rdm/elfeed-search-mark-read (entry)
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
      "Remove the \\='unread\\=' and \\='later\\=' tag from entry.
based on elfeed-search-show-entry"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (rdm/elfeed-search-mark-read entry)
        (unless elfeed-search-remain-on-entry (forward-line))))

    (defun rdm/elfeed-search-untag-unread (entry)
      "Remove the \\='unread\\=' tag from entry and recenter.
based on elfeed-search-show-entry"
      (interactive (list (elfeed-search-selected :ignore-region)))
      (when (elfeed-entry-p entry)
        (rdm/elfeed-search-mark-read entry)
        (forward-line) (recenter)))

    (defun rdm/elfeed-search-show-entry (entry)
      "Display the currently selected item on elfeed-show buffer.
without forwarding line and mark as read"
      (interactive (list (elfeed-search-selected :ignore-region)))
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

    :defun
    elfeed-entry-feed elfeed-entry-link elfeed-entry-p elfeed-entry-title
    elfeed-search-selected elfeed-search-toggle-all elfeed-search-update-entry
    elfeed-meta
    elfeed-show-entry
    elfeed-tag elfeed-tagged-p elfeed-untag
    rdm/elfeed-search-mark-read
    rdm/elfeed-unread-p
    rdm/elfeed-search-mark-read-show
    rdm/elfeed-search-show-entry
    :defvar
    elfeed-show-entry
    elfeed-search-remain-on-entry)

  (leaf *elfeed-with-eww
    :defun
    elfeed-show-visit
    elfeed-search-browse-url
    :config
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

    (evil-define-key 'normal elfeed-show-mode-map
      "b" 'rdm/elfeed-show-eww-open)
    (evil-define-key 'normal elfeed-search-mode-map
      "b" 'rdm/elfeed-search-eww-open))

  (leaf *elfeed-with-xwidgets
    :doc "elfeed with `xwidget-webkit'"
    :defun xwidget-webkit-mode
    :config
    (defun rdm/elfeed-search-webkit-open (&optional use-generic-p)
      "open with xwidget webkit browser"
      (interactive "P")
      (if (featurep 'xwidget-internal)
          (progn
            (let ((browse-url-browser-function #'xwidget-webkit-browse-url))
              (elfeed-search-browse-url use-generic-p))
            ;; switch buffer
            (dolist (buff (buffer-list))
              (with-current-buffer buff
                (when (eq major-mode
                          #'xwidget-webkit-mode)
                  (switch-to-buffer buff)))))
        (message "xwidget-webkit-mode seems not available")))

    (defun rdm/elfeed-show-webkit-open (&optional use-generic-p)
      "open with xwidget webkit browser"
      (interactive "P")
      (if (featurep 'xwidget-internal)
          (progn
            (let ((browse-url-browser-function #'xwidget-webkit-browse-url))
              (elfeed-show-visit use-generic-p))
            ;; switch buffer
            (dolist (buff (buffer-list))
              (with-current-buffer buff
                (when (eq major-mode
                          #'xwidget-webkit-mode)
                  (switch-to-buffer buff)))))
        (message "xwidget-webkit-mode seems not available")))

    (evil-define-key 'normal elfeed-show-mode-map
      "B" 'rdm/elfeed-show-webkit-open)
    (evil-define-key 'normal elfeed-search-mode-map
      "B" 'rdm/elfeed-search-webkit-open))

  (leaf *elfeed-with-w3m
    :config
    (defun rdm/elfeed-search-w3m-open (&optional use-generic-p)
      "open with w3m browser"
      (interactive "P")
      (when (boundp 'w3m)
        (let ((browse-url-browser-function w3m-browse-url))
          (elfeed-search-browse-url use-generic-p))))

    (defun rdm/elfeed-show-w3m-open (&optional use-generic-p)
      "open with eww"
      (interactive "P")
      (when (boundp 'w3m)
        (let ((browse-url-browser-function #'w3m-browse-url))
          (elfeed-show-visit use-generic-p)))))

  (leaf *elfeed-mpv-playback
    :doc "integrate with mpv"
    :defun
    mpv-get-property mpv-live-p
    mpv-playlist-append-url mpv-run-command mpv-start
    rdm/elfeed-search--mpv-play
    :init
    (eval-when-compile (leaf mpv :ensure t))
    (defsubst rdm/elfeed-search--mpv-play (fmt &optional _use-generic-p)
      "Playback url with mpv and yt-dlp"
      (catch 'quit
        ;; first, check dependency
        (unless (or (fboundp 'mpv-play)
                    (or (!executable-find "yt-dlp")
                        (!executable-find "youtube-dl")))
          (message "Cannot playback media, deps not satisfied (mpv and yt-dlp). Abort.")
          (throw 'quit nil))

        (if (mpv-live-p)          ; if mpv instance already exists, check options
            (unless (string-equal
                     (cdr (assoc 'format
                                 (mpv-get-property "options/ytdl-raw-options")))
                     fmt)
              (message "Current mpv instance has different format options. Abort.")
              (throw 'quit nil))
          (mpv-start "--idle=once"
                     (format "--ytdl-raw-options=format=%s,prefer-free-formats=" fmt)))

        (let ((entries (elfeed-search-selected))
              (async-shell-command-buffer 'rename))
          (cl-loop for entry in entries
                   do (elfeed-untag entry 'unread 'later)
                   when (elfeed-entry-link entry)
                   do (mpv-playlist-append-url it))
          ;; start playing if playing-pos eq -1
          (when (eq (mpv-get-property "playlist-playing-pos") -1)
            (mpv-run-command "playlist-play-index" 0))
          (mapc #'elfeed-search-update-entry entries)
          (unless (use-region-p) (forward-line)))))
    ;; playback functions
    (defun rdm/elfeed-search-mpv-play-ultra (&optional use-generic-p)
      "playback with Youtube-DL and mpv link (best vid+aud)"
      (interactive "P")
      (rdm/elfeed-search--mpv-play
       "bv*[width>=1920]+ba/b[width>=1920]" use-generic-p))

    (defun rdm/elfeed-search-mpv-play-high (&optional use-generic-p)
      "playback with Youtube-DL and mpv link (best vid+aud)"
      (interactive "P")
      (rdm/elfeed-search--mpv-play
       "bv*[width<=1920]+ba/b[width<=1920]" use-generic-p))

    (defun rdm/elfeed-search-mpv-play-mid (&optional use-generic-p)
      "playback with Youtube-DL and mpv link (720p)"
      (interactive "P")
      (rdm/elfeed-search--mpv-play
       "bv*[width<720]+ba/b[width<720] / wv*+ba/w" use-generic-p))

    (defun rdm/elfeed-search-mpv-play-low (&optional use-generic-p)
      "playback with Youtube-DL and mpv link"
      (interactive "P")
      (rdm/elfeed-search--mpv-play
       "bv*[width<=480]+ba/b[width<=480] / wv*+ba/w" use-generic-p))

    (defun rdm/elfeed-search-mpv-play-audio (&optional use-generic-p)
      "playback with Youtube-DL and mpv link (best aud)"
      (interactive "P")
      (rdm/elfeed-search--mpv-play "bestaudio" use-generic-p))

    (defun rdm/yt-dl (url)
      "Downloads the URL in an async shell"
      (let ((yt-dl-exec (or (!executable-find "yt-dlp") ;; prefer yt-dlp if found
                            (!executable-find "youtube-dl")))
            (default-directory "~/Downloads"))
        (catch 'quit
          (unless yt-dl-exec
            (message "Cannot download media, deps not satisfied (yt-dlp). Abort.")
            (throw 'quit nil)))
        (async-shell-command
         (format (concat yt-dl-exec " '%s' "
                         "--format='bestvideo[height>=720]+bestaudio/best[height>=720]' "
                         "--prefer-free-formats") url))))

    (defun rdm/elfeed-search-yt-dl (&optional use-generic-p)
      "Youtube-DL link"
      (interactive "P")
      (let ((entries (elfeed-search-selected))
            (async-shell-command-buffer 'rename))
        (cl-loop for entry in entries
                 do (elfeed-untag entry 'unread 'later)
                 when (elfeed-entry-link entry)
                 do (rdm/yt-dl it))
        (mapc #'elfeed-search-update-entry entries)
        (unless (use-region-p) (forward-line))))

    (evil-define-key '(normal visual) elfeed-search-mode-map
      "oh"  'rdm/elfeed-search-mpv-play-high
      "om"  'rdm/elfeed-search-mpv-play-mid
      "ol"  'rdm/elfeed-search-mpv-play-low
      "oa"  'rdm/elfeed-search-mpv-play-audio))

  (evil-define-key 'normal elfeed-show-mode-map
    (kbd "C-j") 'rdm/elfeed-show-next
    "]]"        'rdm/elfeed-show-next
    "gj"        'rdm/elfeed-show-next
    (kbd "C-k") 'rdm/elfeed-show-prev
    "[["        'rdm/elfeed-show-prev
    "gk"        'rdm/elfeed-show-prev
    "b"         'rdm/elfeed-show-eww-open
    "Y"         'rdm/elfeed-show-entry-share
    "go"        'elfeed-show-visit
    "ta"        'elfeed-show-tag
    "tr"        'elfeed-show-untag
    "O"         'hydra-elfeed-show-open/body)

  (evil-define-key 'normal elfeed-search-mode-map
    "m"           'rdm/elfeed-search-toggle-star
    "l"           'rdm/elfeed-search-tag-later-unread
    "u"           'rdm/elfeed-search-untag-later-unread
    "Y"           'rdm/elfeed-search-entry-share
    "F"           'rdm/elfeed-search-filter-feed-name
    "f"           'hydra-elfeed-search-filter/body
    "ta"          'elfeed-search-tag-all
    "tr"          'elfeed-search-untag-all
    "tj"          'rdm/elfeed-search-tag-junk
    "go"          'elfeed-search-browse-url
    "oo"          'rdm/elfeed-search-mark-read-show
    (kbd "RET")   'rdm/elfeed-search-show-read-entry
    (kbd "SPC")   'rdm/elfeed-search-show-read-entry
    (kbd "C-j")   'rdm/elfeed-search-show-read-entry
    (kbd "M-j")   'rdm/elfeed-search-show-read-entry
    "]]"          'rdm/elfeed-search-show-read-entry
    "gj"          'rdm/elfeed-search-show-read-entry
    (kbd "S-SPC") 'rdm/elfeed-search-show-read-prev-entry
    (kbd "C-k")   'rdm/elfeed-search-show-read-prev-entry
    (kbd "M-k")   'rdm/elfeed-search-show-read-prev-entry
    "[["          'rdm/elfeed-search-show-read-prev-entry
    "gk"          'rdm/elfeed-search-show-read-prev-entry
    "O"           'hydra-elfeed-search-open/body)
  :hydra
  (hydra-elfeed-search-open (nil nil)
                            "open..."
                            ("d"  rdm/elfeed-search-yt-dl     "download" :exit t)
                            ("oh" rdm/elfeed-search-mpv-play-high  "play (high)" :exit t)
                            ("om" rdm/elfeed-search-mpv-play-mid   "play (mid)" :exit t)
                            ("ol" rdm/elfeed-search-mpv-play-low   "play (low)" :exit t)
                            ("oa" rdm/elfeed-search-mpv-play-audio "play (audio)" :exit t)
                            ("be" rdm/elfeed-search-eww-open       "browser (eww)" :exit t)
                            ("bm" rdm/elfeed-search-w3m-open       "browser (w3m)" :exit t)
                            ("bw" rdm/elfeed-search-webkit-open    "browser (webkit)" :exit t)
                            ("bb" rdm/elfeed-search-browse-url     "browser (default)" :exit t))

  (hydra-elfeed-show-open (nil nil)
                          "open..."
                          ("be" rdm/elfeed-show-eww-open       "browser (eww)" :exit t)
                          ("bm" rdm/elfeed-show-w3m-open      "browser (w3m)"     :exit t)
                          ("bw" rdm/elfeed-show-webkit-open      "browser (webkit)"  :exit t)
                          ("bb" rdm/elfeed-show-default-open     "browser (default)" :exit t))

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

(leaf *elfeed-with-pocket
  :doc "pocket-reader integration"
  :config
  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "p") 'pocket-reader-add-link)
  (evil-define-key 'normal elfeed-show-mode-map
    (kbd "p") 'pocket-reader-add-link))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conf-elfeed.el ends here
