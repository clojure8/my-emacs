
(defvar +treemacs-git-mode 'simple
  "Type of git integration for `treemacs-git-mode'.

There are 3 possible values:

  1) `simple', which highlights only files based on their git status, and is
     slightly faster,
  2) `extended', which highlights both files and directories, but requires
     python,
  3) `deferred', same as extended, but highlights asynchronously.

This must be set before `treemacs' has loaded.")


(use-package treemacs
  :defer t
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-persist-file (concat *my/emacs-cache* "treemacs-persist")
        treemacs-last-error-persist-file (concat *my/emacs-cache* "treemacs-last-error-persist"))
  :config
  (treemacs-resize-icons 13)
  ;; Don't follow the cursor
  (treemacs-follow-mode -1)

  (when +treemacs-git-mode
    ;; If they aren't supported, fall back to simpler methods
    (when (and (memq +treemacs-git-mode '(deferred extended))
               (not (executable-find "python3")))
      (setq +treemacs-git-mode 'simple))
    (treemacs-git-mode +treemacs-git-mode)
    (setq treemacs-collapse-dirs
          (if (memq treemacs-git-mode '(extended deferred))
              3
            0))))


(use-package treemacs-evil
  :after treemacs)


(use-package treemacs-projectile
  :after treemacs)


(use-package treemacs-magit
  :after treemacs magit)


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
