;; -*- coding: utf-8; lexical-binding: t; -*-


(use-package evil
  :hook (after-init . evil-mode)
  :init
  (csetq evil-want-keybinding nil)
  )

(use-package evil-collection
  :after evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (setq evil-collection-mode-list '(dired magit org))
  (evil-collection-init))

(use-package ivy-rich
  :after (ivy counsel)
  :config
  (ivy-rich-mode +1)
  (ivy-rich-project-root-cache-mode +1))

(use-package ivy
  :defer t
  :config
  (use-package prescient :defer t)
  (use-package ivy-prescient :after ivy
	:config
	(ivy-prescient-mode))
  (use-package company-prescient :defer t)
  (use-package orderless
    :defer t
    :init
    (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))))

(use-package all-the-icons)

(use-package all-the-icons-ivy
  :after all-the-icons
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  :config
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir)))


(use-package hydra)

(use-package counsel
  :defer t)

(use-package company
  :hook (prog-mode . global-company-mode))


(use-package flycheck
  :hook (after-init . global-flycheck-mode))

(use-package savehist)

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode))

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package anzu
  :hook (after-init . global-anzu-mode)
  :config
  (use-package evil-anzu
	:defer t
	:requires (evil anzu)))

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets
	:defer t)
  (use-package ivy-yasnippet
	:after yasnippet
	:defer t))

  
