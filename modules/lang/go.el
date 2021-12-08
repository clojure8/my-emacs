;; -*- coding: utf-8; lexical-binding: t; -*-


(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  (use-package s :defer t)
  (add-hook 'after-save-hook
            (lambda () (when (s-ends-with? ".go" (buffer-name))
						 (progn (lsp-format-buffer)
								(save-buffer))))))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-headerline-breadcrumb-enable nil)
  :hook ((go-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package which-key
  :config
  :hook (go-mode . which-key-mode))

(use-package quickrun
  :commands quickrun-shell)

(use-package popwin
  :hook (after-init . popwin-mode)
  :config
  (push '("*eshell-quickrun*" :height 15) popwin:special-display-config)
  (push '("*quickrun*"  :height 15) popwin:special-display-config)
  (push '("*eshell*" :height 15) popwin:special-display-config)
  )

