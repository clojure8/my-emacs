
(use-package go-mode)

(use-package lsp-mode
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :init (setq lsp-keymap-prefix "s-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (java-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-java)

;; optionallyq
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp
  :config
  (global-company-mode 1))

;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :bind
  (("<f7>" . dap-step-in)
   ("<M-f7>" . dap-step-out)
   ("<f8>" . dap-next)
   ("<f9>" . dap-continue))
  :hook
  ((java-mode . dap-mode)
   (java-mode . dap-ui-mode))
  :config
  (require 'dap-java)
  (require 'dap-go))

(use-package which-key
  :config
  (which-key-mode))

(use-package flycheck
  :config
  (require 'lsp-ui-flycheck)
   (lsp-ui-flycheck-enable nil)
        (flycheck-mode))


(provide 'init-lsp-java)
