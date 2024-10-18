;;; conf-ext-fronts.el --- ext apps frontends -*- lexical-binding: t -*-

;;; Commentary:
;; frontend modes for external commands

;;; Code:
(eval-when-compile
  (load (expand-file-name "elisp/initpkg" user-emacs-directory))
  (!el-load "elisp/doom"
            "elisp/conf-evil"))
(require 'straight)

(leaf rg :if (!executable-find "rg")
    :doc "Use ripgrep in Emacs. (binaries included)"
    :ensure t
    :defun rg-enable-default-bindings
    :commands rg rg-menu
    :config
    (rg-enable-default-bindings))

(leaf emacs-w3m :if (!executable-find "w3m")
  :doc "w3m interface for emacs"
  :commands w3m w3m-browse-url
  :straight
  (emacs-w3m
   :type git :host github
   :repo "emacs-w3m/emacs-w3m")
  :custom
  `(
    (w3m-fill-column                   . 90)
    (w3m-default-display-inline-images . t)
    (w3m-default-coding-system         . 'utf-8)
    (w3m-default-directory             . "~/Downloads")
    (w3m-bookmark-file-coding-system   . 'utf-8)
    ;; set w3m path for emacs to separate shell env
    (w3m-profile-directory             . ,(cache-sub-dir "w3m"))
    (w3m-bookmark-file                 . ,(!expand-file-name "bookmark.html"             (cache-sub-dir "w3m")))
    (w3m-arrived-file                  . ,(!expand-file-name ".arrived"                  (cache-sub-dir "w3m")))
    (w3m-cookie-file                   . ,(!expand-file-name ".cookie"                   (cache-sub-dir "w3m")))
    (w3m-session-file                  . ,(!expand-file-name ".sessions"                 (cache-sub-dir "w3m")))
    (w3m-queries-log-file              . ,(!expand-file-name ".sessions"                 (cache-sub-dir "w3m")))
    (w3m-queries-log-file              . ,(!expand-file-name "emacs-w3m-queries_log.txt" (cache-sub-dir "w3m")))
    )
  :defun rdm/text-scale-adjust
  :mode-hook
  (w3m-fontify-after-hook . ((when (display-graphic-p)
                               (rdm/text-scale-adjust)
                               (face-remap-add-relative
                                'default `(:family  ,(face-attribute 'variable-pitch :family))))))
  :init
  (leaf *patch-w3m-face
    :doc "patch w3m faces with doom-colors"
    :preface
    (defsubst patch-w3m-face ()
      (custom-set-faces
       '(w3m-bold                   ((t (:inherit variable-pitch :bold t))))
       '(w3m-current-anchor         ((t (:inherit variable-pitch  :underline  (:style wave)))))
       '(w3m-error                  ((t (:inherit error))))
       '(w3m-italic                 ((t (:inherit variable-pitch :italic t))))
       '(w3m-strike-through         ((t (:inherit variable-pitch :strike-through t))))
       '(w3m-underline              ((t (:inherit variable-pitch :underline t))))
       `(w3m-anchor                 ((t (:inherit variable-pitch :foreground ,(doom-color 'cyan)))))
       `(w3m-arrived-anchor         ((t (:inherit variable-pitch :foreground ,(doom-color 'cyan)   :underline t))))
       `(w3m-header-line-background ((t (:inherit variable-pitch :background ,(doom-color 'bg-alt)))))
       `(w3m-header-line-content    ((t (:inherit variable-pitch :foreground ,(doom-color 'yellow) :background ,(doom-color 'bg-alt)))))
       `(w3m-header-line-title      ((t (:inherit variable-pitch :foreground ,(doom-color 'cyan)))))
       `(w3m-history-current-url    ((t (:inherit variable-pitch :foreground ,(doom-color 'cyan)   :background ,  (doom-color 'bg-alt)))))
       `(w3m-image                  ((t (:inherit variable-pitch :foreground ,(doom-color 'cyan)))))
       `(w3m-image                  ((t (:inherit variable-pitch :foreground ,(doom-color 'cyan)))))
       `(w3m-image-anchor           ((t (:inherit variable-pitch :foreground ,(doom-color 'bg)     :background ,    (doom-color 'bg)))))
       `(w3m-insert                 ((t (:inherit default     :foreground ,(doom-color 'magenta)))))
       ))
    :hook
    ((after-load-theme-hook
      w3m-load-hook) . patch-w3m-face))
  )

(leaf mpv :when (!executable-find "mpv")
  :ensure t
  :commands mpv-play mpv-play-url mpv-live-p; used to detect mpv running
  :custom
  `((mpv-default-options   . `(,(if-let ((mpvconf (!expand-file-name "mpv" "~/.config")))
                                    (format "--config-dir=%s" mpvconf) "")
                               "--save-position-on-quit"
                               "--volume=80"))
    (mpv-start-timeout     . 5.0)
    (mpv-current-indicator . ,(format "  -- (now playing)" (nerd-icons-faicon "nf-fa-play")))))

(leaf tldr :if (!executable-find "tldr")
  :doc "elegant tldr frontend"
  :ensure t
  :commands tldr
  :custom
  `((tldr-directory-path . ,(cache-sub-dir "tldr"))))

(leaf difftastic :when (!executable-find "difft")
  :doc "`difftastic' integration"
  :ensure t :leaf-defer t
  :config
  (leaf *magit-diff-integration :after 'magit-diff
    :doc "config for `magit'"
    :bind (:magit-blame-read-only-mode-map
           ("D" . difftastic-magit-show)
           ("S" . difftastic-magit-show))
    :config
    (transient-append-suffix 'magit-diff '(-1 -1)
      [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
       ("S" "Difftastic show" difftastic-magit-show)])))

(leaf pocket-reader
  :ensure t
  :commands pocket-reader
  :custom
  `((pocket-lib-token-file         . ,(cache-sub-file "emacs-pocket-lib-token.json"))
    (pocket-reader-show-count      . 200)
    (pocket-reader-url-priorities  . '(amp_url resolved_url given_url))
    (pocket-reader-default-queries . '(":unread")))
  :defvar pocket-reader-mode-map elfeed-search-mode-map elfeed-show-mode-map
  :config
  (evil-define-key 'normal pocket-reader-mode-map
    (kbd "r")   'pocket-reader-resort
    (kbd "D")   'pocket-reader-delete
    (kbd "/")   'pocket-reader-search
    (kbd "y")   'pocket-reader-copy-url
    (kbd "RET") 'pocket-reader-open-url
    (kbd "o")   'pocket-reader-open-url
    (kbd "p") 'pocket-reader-pop-to-url
    (kbd "TAB") 'pocket-reader-excerpt
    (kbd "a")   'pocket-reader-toggle-archived
    (kbd "f")   'pocket-reader-toggle-favorite
    (kbd "b")   'pocket-reader-open-in-external-browser
    ;; mark
    (kbd "m")   'pocket-reader-toggle-mark
    (kbd "M")   'pocket-reader-mark-all
    (kbd "U")   'pocket-reader-unmark-all
    ;; view
    (kbd "g m") 'pocket-reader-more
    (kbd "g l") 'pocket-reader-limit
    (kbd "g r") 'pocket-reader-refresh
    (kbd "g e") 'pocket-reader-excerpt
    (kbd "g E") 'pocket-reader-excerpt-all
    (kbd "g R") 'pocket-reader-random-item
    (kbd "g f") 'pocket-reader-show-unread-favorites
    ;; tags
    (kbd "t a") 'pocket-reader-add-tags
    (kbd "t t") 'pocket-reader-set-tags
    (kbd "t r") 'pocket-reader-remove-tags
    (kbd "t s") 'pocket-reader-tag-search)
  )

(leaf mastodon
  :preface
  (leaf emojify
    :ensure t
    :custom `((emojify-emojis-dir . ,(cache-sub-dir "emojify"))))
  :ensure t
  :custom
  `((mastodon-media--enable-image-caching . t)
    (mastodon-client--token-file          . ,(cache-sub-file "mastodon.plstore")))
  :init (when (fboundp 'evil-lion-mode) (evil-lion-mode nil))
  :commands mastodon mastodon-toot
  :defvar
  (mastodon-mode-map mastodon-toot-mode-map
   mastodon-profile-mode-map mastodon-search-mode-map)
  :config
  ;; Timeline Mode Keybindings
  (evil-define-key 'normal mastodon-mode-map
    (kbd "C-j") 'evil-next-line
    (kbd "C-k") 'evil-previous-line
    (kbd "j") 'mastodon-tl--goto-next-item
    (kbd "k") 'mastodon-tl--goto-prev-item
    ;; operate on toot
    (kbd "t") 'mastodon-toot
    (kbd "e") 'mastodon-toot--edit-toot-at-point
    (kbd "r") 'mastodon-toot--reply
    (kbd "d") 'mastodon-toot--delete-toot
    (kbd "y") 'mastodon-toot--copy-toot-url
    (kbd "o") 'mastodon-toot--open-toot-url
    (kbd "f") 'mastodon-toot--toggle-favourite
    (kbd "b") 'mastodon-toot--toggle-boost
    (kbd "B") 'mastodon-toot--toggle-bookmark
    (kbd "P") 'mastodon-toot--pin-toot-toggle
    ;; threads
    (kbd "RET") 'mastodon-tl--thread
    (kbd "T t") 'mastodon-tl--thread
    (kbd "T T") 'mastodon-tl--view-whole-thread
    (kbd "T m") 'mastodon-tl--mute-thread
    (kbd "T M") 'mastodon-tl--unmute-thread
    (kbd "T s") 'mastodon-tl--toggle-spoiler-in-thread
    ;; view
    (kbd "s") 'mastodon-tl--toggle-spoiler-text-in-toot
    (kbd "V") 'mastodon-tl--poll-vote
    (kbd "/") 'mastodon-search--query
    ;; operate on user
    (kbd "u p") 'mastodon-profile--show-user
    (kbd "u d") 'mastodon-tl--dm-user
    (kbd "u f") 'mastodon-tl--follow-user
    (kbd "u F") 'mastodon-tl--unfollow-user
    (kbd "u M") 'mastodon-tl--mute-user
    (kbd "u m") 'mastodon-tl--unmute-user
    (kbd "u B") 'mastodon-tl--block-user
    (kbd "u b") 'mastodon-tl--unblock-user
    ;; timeline, lists
    (kbd "g r") 'mastodon-tl--update
    (kbd "g h") 'mastodon-tl--get-home-timeline
    (kbd "g L") 'mastodon-tl--get-local-timeline
    (kbd "g S") 'mastodon-tl--get-federated-timeline
    (kbd "g #") 'mastodon-tl--get-tag-timeline
    (kbd "g n") 'mastodon-notifications-get
    (kbd "g m") 'mastodon-notifications--get-mentions
    (kbd "g l") 'mastodon-views--view-lists
    (kbd "g f") 'mastodon-views--view-follow-suggestions
    (kbd "g p") 'mastodon-profile--my-profile
    (kbd "g b") 'mastodon-profile--view-bookmarks
    (kbd "g v") 'mastodon-profile--view-favourites
    ;; etc
    (kbd "q") 'mastodon-kill-window
    (kbd "Q") 'mastodon-kill-all-buffers
    ;; Leader key mappings
    (kbd "z r") 'mastodon-tl--report-to-mods
    (kbd "z z") 'mastodon-views--view-instance-description
    )

  ;; Mastodon Profile Mode
  (evil-define-key 'normal mastodon-profile-mode-map
    (kbd "TAB") 'mastodon-profile--account-view-cycle
    (kbd "/") 'mastodon-profile--account-search
    (kbd ", #") 'mastodon-profile--open-statuses-tagged
    )

  ;; Mastodon Search Mode
  (evil-define-key 'normal mastodon-search-mode-map
    (kbd "n") 'mastodon-search--next-result
    (kbd "N") 'mastodon-search--previous-result
    (kbd "TAB") 'mastodon-search--query-cycle
    (kbd "/") 'mastodon-search--query
    )
  )

(leaf go-translate
  :ensure t
  :commands gt-do-translate
  :defun request rdm/deepl-api-key
  :config
  (defun show-deepl-api-usage ()
    "Show DeepL usage statistics on echo area."
    (interactive)
    (if-let (key (rdm/deepl-api-key))
        (request
          "https://api-free.deepl.com/v2/usage"
          :type "GET"
          :headers `(("Authorization" . ,(format "DeepL-Auth-Key %s" key)))
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (let* ((usage (json-parse-string data))
                             (used  (gethash "character_count" usage))
                             (limit (gethash "character_limit" usage)))
                        (message "DeepL API usage: %g %%  [ %s / %s chars ]"
                                 (* 100 (/ (float used) limit))
                                 used limit))))
          :error (cl-function
                  (lambda (&key error-thrown &allow-other-keys)
                    (user-error "Got error during ruquest: %S" error-thrown))))
      (user-error "DeepL API key not set")))
  :defvar gt-preset-translators
  :defun
  gt-translator gt-taker gt-overlay-render gt-insert-render
  gt-google-engine gt-deepl-engine gt-bing-engine gt-chatgpt-engine
  :setq
  ((gt-preset-translators . `((deepl   . ,(gt-translator
                                           :taker (gt-taker :langs '(en ja) :text 'word)
                                           :engines (gt-deepl-engine)
                                           :render (gt-overlay-render)))
                              (google  . ,(gt-translator
                                           :taker (gt-taker :langs '(en ja) :text 'word)
                                           :engines (gt-google-engine)
                                           :render (gt-insert-render)))
                              (bing    . ,(gt-translator
                                           :taker (gt-taker :langs '(en ja) :text 'word)
                                           :engines (gt-bing-engine)
                                           :render (gt-overlay-render)))
                              (chatgpt . ,(gt-translator
                                           :taker (gt-taker :langs '(en ja) :text 'word)
                                           :engines (gt-chatgpt-engine)
                                           :render (gt-overlay-render))))))
  )

(leaf *openai-things
  :defun auth-source-pick-first-password
  :config
  (leaf chatgpt-shell
    :ensure t
    :commands chatgpt-shell
    :defvar chatgpt-shell-openai-key
    :custom
    `((shell-maker-root-path                    . ,(cache-sub-dir "shell-maker"))
      (chatgpt-shell-model-temperature          . 0.75)
      (chatgpt-shell-streaming                  . t)
      (chatgpt-shell-transmitted-context-length . 8)
      ;; switch via `chatgpt-shell-swap-model-version'
      (chatgpt-shell-model-version              . "chatgpt-4o-latest")
      (chatgpt-shell-openai-key                 . #'rdm/openai-api-key))
    :init
    (leaf *ob-openai-things-setup :after org-mode
      :load-path
      `(,(!expand-file-name "straight/repos/chatgpt-shell" straight-base-dir))
      :defun ob-chatgpt-shell-setup ob-dall-e-shell-setup
      :require ob-chatgpt-shell ob-dall-e-shell
      :config
      (ob-chatgpt-shell-setup)
      (ob-dall-e-shell-setup))
    )

  (leaf gptel :ensure t))

;;; conf-ext-fronts.el ends here
