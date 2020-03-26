(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package general
  :ensure t
  :config
  (general-create-definer spc-leader-def
    :states '(normal insert visual emacs)
    :prefix "<SPC>"
    :non-normal-prefix "C-,")
  (spc-leader-def
    "SPC" 'counsel-M-x
    "yv" '(youdao-dictionary-play-voice-at-point :wk "pronounce")
    "yy" 'my-youdao-search-at-point
    "ys" 'ivy-yasnippet
    "fr" 'counsel-recentf
    "bb" 'counsel-switch-buffer
    "bl" 'evil-switch-to-windows-last-buffer
    "ff" 'find-file
    "fs" 'swiper
    "bk" 'kill-current-buffer
    "gs" 'magit-status
    "gd" 'magit-dispatch
    "ar" 'anzu-replace-at-cursor-thing
    "aa" 'anzu-query-replace-at-cursor
    "pf" 'counsel-projectile-find-file
    "pp" 'counsel-projectile-switch-project
    "pa" 'projectile-add-known-project
    "pr" 'counsel-projectile-rg
    "op" '+treemacs/toggle
    "jj" 'dumb-jump-go
    "jb" 'dumb-jump-back
    "sw" 'shell-pop
    "cr" 'counsel-rg
    )
  (global-set-key (kbd "s-/") 'comment-line)
  (global-set-key (kbd "s-;") 'yas-expand)
  (global-set-key (kbd "C-c C-c") 'er/expand-region))

(provide 'init-evil)
