
(use-package magit)

;; ;; (use-package git-gutter
;; ;;   :config
;; ;;   (setq git-gutter-map nil))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;==================================================
;; diff比较工具
;;==================================================
(use-package vdiff
  :after evil
  :config
  (setq vdiff-lock-scrolling t)
  ;; diff program/algorithm to use. Allows choice of diff or git diff along with
  ;; the various algorithms provided by these commands. See
  ;; `vdiff-diff-algorithms' for the associated command line arguments.
  (setq vdiff-diff-algorithm 'diff)
  (setq vdiff-auto-refine nil)
  ;; Unchanged lines to leave unfolded around a fold
  (setq vdiff-fold-padding 6)
  (evil-define-key 'normal vdiff-mode-map "," vdiff-mode-prefix-map)
  (add-hook 'vdiff-mode (lambda () (vdiff-hydra/body))))

(use-package vdiff-magit
  :after (vdiff magit)
  :config
  (define-key magit-mode-map "e" 'vdiff-magit-dwim)
  (define-key magit-mode-map "E" 'vdiff-magit)
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))

 (provide 'init-git)
