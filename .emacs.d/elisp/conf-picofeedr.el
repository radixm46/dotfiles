;;; conf-picofeedr.el --- picofeedr ui conf -*- lexical-binding: t -*-
;;; Commentary:
;;
;; configure picofeedr
;;

;;; Code:
(eval-when-compile
  (load (expand-file-name "elisp/initpkg" user-emacs-directory))
  (!el-load "elisp/conf-evil"
            "elisp/util"
            "elisp/doom"
            "elisp/conf-fonts"
            "elisp/conf-ext-fronts"))
(require 'straight)
(require 'evil-core) ; for evil-define-key
(require 'hydra)
(require 'rdm/util)



(leaf picofeedr
  :doc "picofeedr emacs client (testing)"
  :straight
  (picofeedr-emacs
   :type git :host github
   :repo "radixm46/picofeedr-emacs"
   :branch "main")
  :require t
  :custom
  (picofeedr-cli-args                           . '("--output" "json"))
  (picofeedr-list-limit                         . 500)
  (picofeedr-search-default-query               . "after:1m -tag:news|later|junk|YouTube|github tag:unread")
  (picofeedr-search-open-restore-strategy       . 'immediate)
  (picofeedr-search-column-specs                . '((date :min-width 24
                                                          :format "%Y-%m-%d (%a) %k:%M")
                                                    (title :min-width 85)
                                                    (feed :min-width 30)
                                                    (tags :min-width 30)))
  (picofeedr-show-enclosure-image-preview-count . 2)
  (picofeedr-show-meta-order                    . '(title author date feed tags link enclosures))
  (picofeedr-show-date-format                   .  "%Y-%m-%d (%a) %k:%M")
  :config
  (leaf *patch-picofeedr-faces
    :hook
    ((picofeedr-search-mode-hook
      after-load-theme-hook) . (lambda ()
      (face-remap-add-relative 'hl-line
                               `(:background ,(doom-blend 'highlight 'bg-alt 0.30)))
      (rdm/text-scale-adjust)
      (rdm/sw-lnsp 0.75)))
    ((picofeedr-show-mode-hook . (lambda () (rdm/text-scale-adjust)))))
  ;; Tag helpers
  (defun rdm/picofeedr-search-toggle-star ()
    "Toggle the star tag on the current entry."
    (interactive)
    (picofeedr-search-entry-toggle-tag
     (picofeedr-search-current-entry-id) "star"))

  (picofeedr-search-register-tag-action
   'rdm/picofeedr-search-mark-later-unread
   "Add later and unread tags to selected targets."
   :add (list "later" picofeedr-unread-tag)
   )

  (picofeedr-search-register-tag-action
   'rdm/picofeedr-search-entry-mark-read
   "Mark selected entries as read by removing unread/later tags."
   :remove (list picofeedr-unread-tag "later")
   :on-error (lambda (_ids updated error)
               (picofeedr-search-handle-bulk-operation-error
                "picofeedr mark error" updated error)))

  ;; Read state helpers
  (defun rdm/picofeedr-search--show-after-move (delta)
    "Move by DELTA when possible, then show current entry."
    (when (picofeedr-search-forward-cursor delta)
      (recenter))
    (picofeedr-search-entry-show))

  (defun rdm/picofeedr-search-unread-p ()
    "Return non-nil if the current entry has the unread tag."
    (picofeedr-search-entry-unread-p
     (picofeedr-search-current-entry-item)))

  (picofeedr-search-register-tag-action
   'rdm/picofeedr-search-unmark-later-unread
   "Remove unread/later tags from selected targets."
   :remove (list picofeedr-unread-tag "later")
   :on-error (lambda (_ids updated error)
               (picofeedr-search-handle-bulk-operation-error
                "picofeedr tag error" updated error)))

  ;; Show / navigation helpers
  (defun rdm/picofeedr-search-show-read-entry ()
    "Show with preview-first behavior on an unread row.

When the current entry is unread, preview it in place.
Otherwise move forward and preview there."
    (interactive)
    (if (rdm/picofeedr-search-unread-p)
        (picofeedr-search-entry-show)
      (rdm/picofeedr-search--show-after-move 1)))

  (defun rdm/picofeedr-search-show-read-prev-entry ()
    "Move to previous entry and preview it."
    (interactive)
    (rdm/picofeedr-search--show-after-move -1))

;;; Share / clipboard

  (defun rdm/picofeedr-search-entry-share ()
    "Copy current entry title and URL as text to the kill ring."
    (interactive)
    (let* ((item (picofeedr-search-current-entry-item))
           (text (concat (picofeedr-entry-item-title item) "\n"
                          (picofeedr-entry-item-link item))))
      (kill-new text)
      (message "Yanked: %s" text)))


  (defun rdm/picofeedr-search-entry-url ()
    "Copy current entry URL as text to the kill ring."
    (interactive)
    (let ((url (picofeedr-entry-item-link (picofeedr-search-current-entry-item))))
      (kill-new url)
      (message "Yanked: %s" url)))

;;; Browser open

  (picofeedr-search-register-open-action
   'rdm/picofeedr-search-browse-open
   "Open current entry URL in the default browser."
   :remove (list picofeedr-unread-tag "later"))

;;; Filter helpers

  (defun rdm/picofeedr-search-filter-feed-name ()
    "Narrow the search list to unread entries from the current entry's feed."
    (interactive)
    (let* ((summary (picofeedr-search-current-entry-item))
           (label   (picofeedr-search-entry-feed-label summary)))
      (unless (and (stringp label) (not (string-empty-p label)))
        (user-error "No feed label for current entry"))
      (picofeedr-search-set-filter
       (format "after:3m feed:\"%s\" tag:%s" label picofeedr-unread-tag))))

  (defun rdm/picofeedr-search-default ()
    (interactive)
    (picofeedr-search-set-filter picofeedr-search-default-query))

  ;; (evil-define-key 'normal picofeedr-search-mode-map
  ;;   "b" 'rdm/elfeed-search-eww-open)
  (evil-define-key '(visual normal) picofeedr-search-mode-map
    "v"           'rdm/picofeedr-search-toggle-star
    "m"           'picofeedr-search-entry-toggle-marks
    "l"           'rdm/picofeedr-search-mark-later-unread
    "u"           'rdm/picofeedr-search-entry-mark-read
    "U"           'picofeedr-search-entry-mark-unread
    "Y"           'rdm/picofeedr-search-entry-share
    "c"           'rdm/picofeedr-search-default
    "y"           'rdm/picofeedr-search-entry-url
    "F"           'rdm/picofeedr-search-filter-feed-name
    "f"           'hydra-picofeedr-search-filter/body
    "ta"          'picofeedr-search-entry-add-tags
    "tr"          'picofeedr-search-entry-remove-tags
    ;; "tj"          'rdm/elfeed-search-tag-junk
    "+"           'picofeedr-search-entry-add-tags
    "-"           'picofeedr-search-entry-remove-tags
    "go"          'rdm/picofeedr-search-browse-open
    "gr"          'picofeedr-search-refresh
    "gR"          'picofeedr-search-sync
    "S"           'picofeedr-search-set-filter
    "oo"          'picofeedr-search-entry-show
    ;; "os"          'rdm/elfeed-search-entry-sum
    (kbd "RET")   'rdm/picofeedr-search-show-read-entry
    (kbd "SPC")   'rdm/picofeedr-search-show-read-entry
    (kbd "C-j")   'rdm/picofeedr-search-show-read-entry
    (kbd "M-j")   'rdm/picofeedr-search-show-read-entry
    "]]"          'rdm/picofeedr-search-show-read-entry
    "gj"          'rdm/picofeedr-search-show-read-entry
    (kbd "S-RET") 'rdm/picofeedr-search-show-read-prev-entry
    (kbd "S-SPC") 'rdm/picofeedr-search-show-read-prev-entry
    (kbd "C-k")   'rdm/picofeedr-search-show-read-prev-entry
    (kbd "M-k")   'rdm/picofeedr-search-show-read-prev-entry
    "[["          'rdm/picofeedr-search-show-read-prev-entry
    "gk"          'rdm/picofeedr-search-show-read-prev-entry
    "O"           'hydra-picofeedr-search-open/body)

  (leaf *picofeedr-with-xwidgets
    :doc "elfeed with `xwidget-webkit'"
    :defun xwidget-webkit-mode
    :config
    (picofeedr-search-register-open-action
     'rdm/picofeedr-search-webkit-open
     "Open current entry with xwidget webkit browser."
     :remove (list picofeedr-unread-tag "later")
     :browser (lambda (url)
                (if (featurep 'xwidget-internal)
                    (progn
                      (if (fboundp 'xwidget-webkit-browse-url)
                          (xwidget-webkit-browse-url url)
                        (browse-url url))
                      (dolist (buff (buffer-list))
                        (with-current-buffer buff
                          (when (eq major-mode 'xwidget-webkit-mode)
                            (switch-to-buffer buff)))))
                  (message "xwidget-webkit-mode seems not available"))))

    (evil-define-key 'normal picofeedr-search-mode-map
      "B" 'rdm/picofeedr-search-webkit-open)
    ;; (defun rdm/elfeed-show-webkit-open (&optional use-generic-p)
    ;;   "open with xwidget webkit browser"
    ;;   (interactive "P")
    ;;   (if (featurep 'xwidget-internal)
    ;;       (let ((browse-url-browser-function #'xwidget-webkit-browse-url)
    ;;             (entry (elfeed-search-selected t)))
    ;;         (elfeed-untag entry 'later)
    ;;         (elfeed-show-visit use-generic-p)
    ;;         ;; switch buffer
    ;;         (dolist (buff (buffer-list)
    ;;                       (with-current-buffer buff
    ;;                         (when (eq major-mode
    ;;                                   #'xwidget-webkit-mode)
    ;;                           (switch-to-buffer buff))))))
    ;;     (message "xwidget-webkit-mode seems not available")))

    ;; (evil-define-key 'normal elfeed-show-mode-map
    ;;   "B" 'rdm/elfeed-show-webkit-open)
    )

  (leaf *picofeedr-with-eww
    :defun rdm/picofeedr-search-eww-open
    :config
    ;; eww
    ;; (defun rdm/elfeed-show-eww-open (&optional use-generic-p)
    ;;   "open with eww, untag \\='later\\='"
    ;;   (interactive "P")
    ;;   (let ((browse-url-browser-function #'eww-browse-url)
    ;;         (entry (elfeed-search-selected t)))
    ;;     (elfeed-untag entry 'later)
    ;;     (elfeed-show-visit use-generic-p)))
    ;; (evil-define-key 'normal elfeed-show-mode-map
    ;;   "b" 'rdm/elfeed-show-eww-open)

    (picofeedr-search-register-open-action
     'rdm/picofeedr-search-eww-open
     "Open current entry URL with eww browser."
     :browser (lambda (url)
                (let ((browse-url-browser-function #'eww-browse-url))
                  (browse-url url)))
     :remove (list picofeedr-unread-tag "later"))
    (evil-define-key 'normal picofeedr-search-mode-map
      "b" 'rdm/picofeedr-search-eww-open))

  (leaf *picofeedr-with-w3m
    :config
    (picofeedr-search-register-open-action
     'rdm/picofeedr-search-w3m-open
     "Open current entry URL with w3m browser."
     :remove (list picofeedr-unread-tag "later")
     :browser (lambda (url)
                (if (fboundp 'w3m-browse-url)
                    (let ((browse-url-browser-function #'w3m-browse-url))
                      (browse-url url))
                  (message "w3m-browse-url seems not available")))))

  (leaf *picofeedr-mpv-playback
    :doc "Open current entry URL with mpv using quality presets."
    :config
    (eval-when-compile (leaf mpv :ensure t))

    (defun rdm/picofeedr-search--mpv-selected-target ()
      "Return selected target payload as plist.

Returned keys are:
- `:ids': selected entry IDs in visible order
- `:urls': selected entry URLs in visible order (nil URLs removed)"
      (when-let* ((target-ids (picofeedr-search-target-entry-ids t)))
        (list :ids target-ids
              :urls (delq nil (picofeedr-search-target-entry-urls t)))))

    (defun rdm/picofeedr-search--mpv-open-url (url ytdl-format)
      "Open URL with mpv using YTDL-FORMAT."
      ;; first, check dependency
      (unless (or (fboundp 'mpv-play)
                  (or (!executable-find "yt-dlp")
                      (!executable-find "youtube-dl")))
        (user-error "Cannot playback media, deps not satisfied (mpv and yt-dlp)"))

      (if (mpv-live-p) ;; if mpv instance already exists, check options
          (unless (string-equal
                   (cdr (assoc 'format
                               (mpv-get-property "options/ytdl-raw-options")))
                   ytdl-format)
            (uesr-error "Current mpv instance has different format options"))
        (mpv-start "--idle=once"
                   (format "--ytdl-raw-options=format=%s,prefer-free-formats="
                           ytdl-format)))

      (let* ((selected-target (rdm/picofeedr-search--mpv-selected-target))
             (selected-ids (plist-get selected-target :ids))
             (urls (or (plist-get selected-target :urls)
                       (list url))))
        ;; Keep read-state behavior consistent with queue-based playback.
        (when selected-ids
          (picofeedr-search-apply-tags-to-entry-ids
           selected-ids nil
           (list picofeedr-unread-tag "later")))
        (dolist (queue-url urls)
          (mpv-playlist-append-url queue-url))
        ;; start playing if playing-pos eq -1
        (when (eq (mpv-get-property "playlist-playing-pos") -1)
          (mpv-run-command "playlist-play-index" 0))))

    ;; generate mpv playback functions
    (pcase-dolist (`(,k . ,v) '((ultra . "bv*[width>=1920]+ba/b[width>=1920]")
                                (high  . "bv*[width<=1920]+ba/b[width<=1920]")
                                (mid   . "bv*[width<720]+ba/b[width<720] / wv*+ba/w")
                                (low   . "bv*[width<=480]+ba/b[width<=480] / wv*+ba/w")
                                (audio . "bestaudio")))
      (picofeedr-search-register-open-action
       ;; function name as symbol
       (intern (format "rdm/picofeedr-search-mpv-play-%s"
                       (symbol-name k)))
       (format "Open current entry in mpv as quality %s.\n`%s'."
               (symbol-name k) v)
       :remove (list picofeedr-unread-tag "later")
       :browser (lambda (url)
                  (rdm/picofeedr-search--mpv-open-url url v))))

    (evil-define-key '(normal visual) picofeedr-search-mode-map
      "oh" 'rdm/picofeedr-search-mpv-play-high
      "om" 'rdm/picofeedr-search-mpv-play-mid
      "ol" 'rdm/picofeedr-search-mpv-play-low
      "oa" 'rdm/picofeedr-search-mpv-play-audio))
  :hydra
  (hydra-picofeedr-search-open (nil nil)
                            "open..."
                            ;; ("d"  rdm/picofeedr-search-yt-dl     "download" :exit t)
                            ("oh" rdm/picofeedr-search-mpv-play-high  "play (high)" :exit t)
                            ("om" rdm/picofeedr-search-mpv-play-mid   "play (mid)" :exit t)
                            ("ol" rdm/picofeedr-search-mpv-play-low   "play (low)" :exit t)
                            ("oa" rdm/picofeedr-search-mpv-play-audio "play (audio)" :exit t)
                            ("be" rdm/picofeedr-search-eww-open       "browser (eww)" :exit t)
                            ("bm" rdm/picofeedr-search-w3m-open       "browser (w3m)" :exit t)
                            ("bw" rdm/picofeedr-search-webkit-open    "browser (webkit)" :exit t)
                            ("bb" rdm/picofeedr-search-browse-open    "browser (default)" :exit t))

  (hydra-picofeedr-search-filter (nil nil)
                              "elfeed filters"
                              ("u" (picofeedr-search-set-filter "after:4w tag:unread -tag:junk")  "all unread")
                              ("a" (picofeedr-search-set-filter "after:4w -tag:junk")  "all entries")
                              ("n" (picofeedr-search-set-filter "after:4w tag:news&unread -tag:later|junk")  "news")
                              ("N" (picofeedr-search-set-filter "after:4w tag:news -tag:junk")  "news(all)")
                              ("l" (picofeedr-search-set-filter "tag:later&unread")  "later")
                              ("L" (picofeedr-search-set-filter "tag:later -tag:unread")  "later(read)")
                              ("c" (picofeedr-search-set-filter "after:6w tag:comic&unread -tag:later")  "comic")
                              ("C" (picofeedr-search-set-filter "after:6w tag:comic -tag:unread")  "comic(all)")
                              ("v" (picofeedr-search-set-filter "after:4w tag:YouTube&unread -tag:later") "YouTube")
                              ("V" (picofeedr-search-set-filter "after:4w tag:YouTube") "YouTube(all)")
                              ("s" (picofeedr-search-set-filter "tag:star")  "starred"))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conf-picofeedr.el ends here
