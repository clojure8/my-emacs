
(use-package magit
  :defer t
  :init (setq magit-diff-refine-hunk t))

(use-package counsel-projectile
  :defer t)

(use-package projectile
  :hook (prog-mode . projectile-mode))

(use-package esup
  :defer t)

(require 'wpt-proxy)


(provide 'init-tools)
