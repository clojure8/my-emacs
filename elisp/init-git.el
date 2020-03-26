
(use-package magit)

;; ;; (use-package git-gutter
;; ;;   :config
;; ;;   (setq git-gutter-map nil))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'init-git)
