;; -*- coding: utf-8; lexical-binding: t; -*-


(use-package exec-path-from-shell
  :defer t
  :hook (after-init . exec-path-from-shell-initialize))

  (use-package magit
	:defer t
	:init (setq magit-diff-refine-hunk t))

(use-package counsel-projectile
  :defer t)

(use-package projectile
  :hook (prog-mode . projectile-mode))

(use-package esup
  :defer t)



(provide 'init-tools)
