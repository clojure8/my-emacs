;; -*- coding: utf-8; lexical-binding: t; -*-


(use-package ivy
  :defer t)

(use-package counsel
  :defer t)

(use-package company
  :hook (prog-mode . global-company-mode))


(provide 'init-editor)
