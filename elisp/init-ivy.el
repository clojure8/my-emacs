;; 	-*- lexical-binding: t -*-
(use-package ivy)

(use-package fuz)

;; (use-package ivy-fuz
;;   :ensure t
;;   :demand t
;;   :after ivy
;;   :custom
;;   (ivy-sort-matches-functions-alist '((t . ivy-fuz-sort-fn)))
;;   (ivy-re-builders-alist '((t . ivy-fuz-regex-fuzzy)))
;;   :config
;;   (add-to-list 'ivy-highlight-functions-alist '(ivy-fuz-regex-fuzzy . ivy-fuz-highlight-fn)))

(use-package ivy-rich
  :after ivy
  :hook
  (ivy-mode . ivy-rich-mode))

;; (use-package ivy-explorer
;;   :after ivy
;;   :config
;;   (ivy-explorer-mode +1)
;;   (counsel-mode +1))

(use-package ivy-xref
  :after ivy
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package swiper
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package counsel
  :bind
  (("C-c f" . counsel-recentf))
  :init
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  :config
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable))

(use-package counsel-etags
  :ensure t
  :bind (("s-." . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories)
  (eval-after-load 'counsel-etags
    '(progn
       ;; counsel-etags-ignore-directories does NOT support wildcast
       (push "build_clang" counsel-etags-ignore-directories)
       ;; counsel-etags-ignore-filenames supports wildcast
       (push "TAGS" counsel-etags-ignore-filenames)
       (push "*.json" counsel-etags-ignore-filenames)))
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; Setup auto update now
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local))))

(use-package company
  :config
  (global-company-mode t))

(use-package company-ctags
  :after (company general)
  :init
  (setq company-ctags-extra-tags-files '("$HOME/TAGS" "/usr/include/TAGS"))
  :config
  (company-ctags-auto-setup)
  (defun my-counsel-company ()
    "Input code from company backend using fuzzy matching."
    (interactive)
    (company-abort)
    (let* ((company-backends '(company-ctags))
           (company-ctags-fuzzy-match-p t))
      (counsel-company)))
  ;; (general-imap "r"
  ;;               (general-key-dispatch 'self-insert-command
  ;;                 :timeout 0.2
  ;;                 "r" 'my-counsel-company))
  )

(use-package counsel-gtags
  :after company
  :defer t
  :hook
  ((c-mode . counsel-gtags-mode)
   (c++-mode . counsel-gtags-mode))
  :config
  (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
  (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
  (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
  (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward))



(provide 'init-ivy)

