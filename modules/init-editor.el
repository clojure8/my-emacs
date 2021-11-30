;;; code:

(use-package ivy
  :defer t)

(use-package counsel
  :defer t)

(use-package company
  :hook (prog-mode . global-company-mode))

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (csetq evil-want-keybinding nil)
  )

(use-package evil-collection
  :after evil
  :init
  (csetq evil-want-keybinding nil)
  :config
  (add-hook 'after-init-hook 'evil-collection-init))

(use-package general
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
	"oc" 'org-capture
    "fr" 'counsel-recentf
    "bb" 'counsel-switch-buffer
    "bl" 'evil-switch-to-windows-last-buffer
    "ff" 'counsel-find-file
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
    "cr" 'counsel-rg
    )
  (global-set-key (kbd "s-/") 'comment-line)
  (global-set-key (kbd "M-f") 'swiper)
  (global-set-key (kbd "C-S-p") 'counsel-switch-buffer)
  (global-set-key (kbd "M-s") 'save-buffer)
  (global-set-key (kbd "s-;") 'yas-expand)
  (global-set-key (kbd "C-x p") '+treemacs/toggle)
  (global-set-key (kbd "C-x k") 'kill-current-buffer)
  (global-set-key (kbd "C-c C-c") 'er/expand-region))


(provide 'init-editor)
