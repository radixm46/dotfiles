;;; conf-evil.el --- evil mode conf -*- lexical-binding: t -*-

;;; Commentary:
;; configure evil mode

;;; Code:

(eval-when-compile
  (load (expand-file-name "elisp/initpkg"    user-emacs-directory))
  (load (expand-file-name "elisp/conf-fonts" user-emacs-directory)))
(require 'straight)
(require 'hydra)

(leaf evil
  :ensure t
  :defvar evil-want-keybinding
  :pre-setq (evil-want-keybinding . nil)
  :custom (evil-undo-system . 'undo-tree)
  :defun
  evil-window-increase-height evil-window-decrease-height
  evil-window-increase-width evil-window-decrease-width
  evil-define-minor-mode-key
  evil-global-set-key
  :commands evil-define-key
  :defvar evil-normal-state-map evil-insert-state-map
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
    :defun evil-collection-init
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
    :defun evil-commentary-mode
    :config (evil-commentary-mode))
  (leaf evil-goggles :ensure t
    :doc "visual hints on edit."
    :url "https://github.com/edkolev/evil-goggles"
    :custom
    (evil-goggles-pulse             . nil)
    (evil-goggles-duration          . 0.10)
    (evil-goggles-async-duration    . nil)
    (evil-goggles-blocking-duration . nil)
    :global-minor-mode evil-goggles-mode
    :defun evil-goggles-use-diff-faces
    :config (evil-goggles-use-diff-faces))
  (leaf evil-lion :ensure t
    :doc "Evil align operator."
    :url "https://github.com/edkolev/evil-lion"
    :global-minor-mode evil-lion-mode)
  (leaf evil-matchit :ensure t
    :doc "Vim matchit.vim by Benji Fisher is ported into Emacs."
    :url "https://github.com/redguardtoo/evil-matchit"
    :global-minor-mode global-evil-matchit-mode)
  (leaf evil-cleverparens
    :doc "edit for lispy modes"
    :ensure t
    :commands evil-cleverparens-mode
    :hook (smartparens-enabled-hook . evil-cleverparens-mode)
    :defvar evil-cleverparens-mode-map
    :config
    (evil-define-minor-mode-key
      '(normal visual) 'evil-cleverparens-mode
      "gj" 'sp-next-sexp
      "gk" 'sp-previous-sexp
      ;; TODO: もうちょっとsexp追加したいな……いっそhydraか何かで作ったほうがいいかも
      ;; "gh" 'sp-up-sexp
      ;; "gl" 'sp-down-sexp
      "]"  'evil-cp-next-opening
      "["  'evil-cp-previous-closing
      "{"  'evil-cp-previous-opening
      "}"  'evil-cp-next-closing))
  (leaf evil-numbers
    :ensure t
    :doc "Increment / Decrement binary, octal, decimal and hex literals"
    :url "https://github.com/cofi/evil-numbers"
    :config
    ;; use `evil-global-set-key' instead of `evil-define-key'
    (evil-global-set-key
     'normal (kbd "C-c +") 'evil-numbers/inc-at-pt)
    (evil-global-set-key
     'normal (kbd "C-c -") 'evil-numbers/dec-at-pt)

    ;; (evil-define-key '(motion global) ;; FIXME: どのみち読めてない, 外に出しても中に入れてもダメ
    ;;   (kbd "C-c +") 'evil-numbers/inc-at-pt
    ;;   (kbd "C-c -") 'evil-numbers/dec-at-pt))
    )

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
   ^(_<f4>_/_M-4_)       ^^buf2other _F_rame
" (nerd-icons-faicon "nf-fa-window_maximize"))
   ("h" evil-window-left)
   ("j" evil-window-down)
   ("k" evil-window-up)
   ("l" evil-window-right)
   ("a" ace-window)
   ("<f4>" ace-window :exit t)
   ("M-4"  ace-window :exit t)
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
   ("F" consult-buffer-other-frame :exit t)
   ("N" tab-bar-switch-to-next-tab)
   ("P" tab-bar-switch-to-prev-tab)
   ("t" hydra-tab-bar/body :exit t))
  )
;;; conf-evil.el ends here
