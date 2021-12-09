;; -*- coding: utf-8; lexical-binding: t; -*-

;;; code:

(use-package general
  :config
  (general-define-key
   "M-p" 'counsel-switch-buffer
   "M-z" 'evil-undo
   "M-x" 'counsel-M-x
   "s-x" 'counsel-M-x
   "M-v" 'evil-paste-after
   "s-/" 'comment-line
   "M-/" 'comment-line
   "M-f" 'swiper
   "C-S-p" 'counsel-switch-buffer
   "M-s" 'save-buffer
   "s-;" 'yas-expand
   "C-x p" '+treemacs/toggle
   "C-x k" 'kill-current-buffer
   "C-c C-c" 'er/expand-region)
  (general-create-definer g-leader-def
    :states '(normal insert visual emacs)
    :prefix "g"
    :non-normal-prefix "C-;")
  (g-leader-def
    "l" 'centaur-tabs-forward-tab
    "h" 'centaur-tabs-backward-tab
    "k" 'centaur-tabs-forward-group
    "j" 'centaur-tabs-backward-group)
  (general-create-definer spc-leader-def
    :states '(normal insert visual emacs)
    :prefix "<SPC>"
    :non-normal-prefix "C-,")
  (spc-leader-def
    "SPC" 'counsel-M-x
    "ht" 'counsel-load-theme
    "ep" 'treemacs-display-current-project-exclusively
    "op" 'treemacs
    "br" 'revert-buffer
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
    "fp" 'counsel-projectile-find-file
    "pp" 'counsel-projectile-switch-project
    "pa" 'projectile-add-known-project
    "pr" 'counsel-projectile-rg
    "jj" 'dumb-jump-go
    "jb" 'dumb-jump-back
    "cr" 'counsel-rg
    "bu" 'xwidget-webkit-browse-url
    ))
