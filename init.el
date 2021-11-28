;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-

;; Author: wpt
;; Version: 1.0

;;; Code:
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

;; don't forget to load custom file at last
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file nil t))


;; set the startup default directory, not essential but recommended.
(setq default-directory "~/")

;; Load path
;; Optimize: Force "elisp" "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("elisp"))
	(push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "elisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; emacs deamon
(server-start)


(require 'init-package-manager)
(require 'init-system)

(require 'init-ui)
(require 'init-editor)
(require 'init-lang)
(require 'init-term)
(require 'init-tools)


;;; Init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not unresolved obsolete)
;; End:
