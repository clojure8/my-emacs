;; -*- coding: utf-8; lexical-binding: t; -*-

;;; code:

(use-package eclipse-theme :defer t)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'doom-one t)
  (add-hook 'after-init-hook (lambda () (load-theme 'leuven t)))
  ;; (load-theme 'doom-fairy-floss t)
  ;;(load-theme 'doom-acario-dark t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") 
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package apropospriate-theme :defer t)


(use-package sublime-themes :defer t)

(use-package kaolin-themes :defer t)

(use-package nimbus-theme :defer t)
(use-package modus-themes :defer t)
