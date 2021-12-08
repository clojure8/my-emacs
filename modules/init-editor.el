;; -*- coding: utf-8; lexical-binding: t; -*-


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
	:hook (after-init . ivy-prescient-mode))
  (use-package company-prescient :defer t)
  (use-package orderless
	:defer t
	:init
	(setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))))

(use-package counsel
  :defer t)

(use-package company
  :hook (prog-mode . global-company-mode))


