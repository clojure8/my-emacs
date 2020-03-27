;; init-treemacs.el --- Initialize treemacs.	-*- lexical-binding: t -*-

;;
;; Treemacs: A tree layout file explorer.
;;

;;; Code:


(defvar +treemacs-git-mode 'simple)

;; A tree layout file explorer
(use-package treemacs
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :bind (([f8]        . treemacs)
         ("M-0"       . treemacs-select-window)
         ("C-x 1"     . treemacs-delete-other-windows)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t b"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-is-never-other-window   t
        treemacs-silent-filewatch        t
        treemacs-silent-refresh          t
        treemacs-width                   30
        treemacs-no-png-images           t)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  ;; Projectile integration
  (use-package treemacs-projectile
    :after projectile
    :bind (:map projectile-command-map
                ("h" . treemacs-projectile)))

  (use-package treemacs-magit
    :after magit
    :commands treemacs-magit--schedule-update
    :hook ((magit-post-commit
            git-commit-post-finish
            magit-post-stage
            magit-post-unstage)
           . treemacs-magit--schedule-update))

  (use-package perspective)
  (use-package treemacs-persp
    :after persp-mode
    :commands treemacs-set-scope-type
    :init (treemacs-set-scope-type 'Frames)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun +treemacs--init ()
  (require 'treemacs)
  (let ((origin-buffer (current-buffer)))
    (cl-letf (((symbol-function 'treemacs-workspace->is-empty?)
               (symbol-function 'ignore)))
      (treemacs--init))
    (dolist (project (treemacs-workspace->projects (treemacs-current-workspace)))
      (treemacs-do-remove-project-from-workspace project))
    (with-current-buffer origin-buffer
      (let ((project-root (or default-directory)))
        (treemacs-do-add-project-to-workspace
         (treemacs--canonical-path project-root)))
      (setq treemacs--ready-to-follow t)
      (when (or treemacs-follow-after-init treemacs-follow-mode)
        (treemacs--follow)))))

;;;###autoload
(defun +treemacs/toggle ()
  "Initialize or toggle treemacs.

Ensures that only the current project is present and all other projects have
been removed.

Use `treemacs' command for old functionality."
  (interactive)
  (require 'treemacs)
  (pcase (treemacs-current-visibility)
    (`visible (delete-window (treemacs-get-local-window)))
    (_ (+treemacs--init))))

;;;###autoload
(defun +treemacs/find-file (arg)
  "Open treemacs (if necessary) and find current file."
  (interactive "P")
  (let ((origin-buffer (current-buffer)))
    (+treemacs--init)
    (with-current-buffer origin-buffer
      (treemacs-find-file arg))))


(provide 'init-treemacs)
