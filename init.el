;; -*- coding: utf-8; lexical-binding: t; -*-

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
(setq default-directory "~/.emacs.d")

(add-hook 'after-init-hook #'server-start)

;; ----------------------------------------------------------------------------------------------------
;; package manager
;; ----------------------------------------------------------------------------------------------------
(setq package-archives '(
                         ("melpa" . "https://mirrors.163.com/elpa/melpa/")
                         ("melpa-stable" . "https://mirrors.163.com/elpa/melpa-stable/")))

(package-initialize)

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
     (bootstrap-version 5))
 (unless (file-exists-p bootstrap-file)
   (with-current-buffer
       (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
     (goto-char (point-max))
     (eval-print-last-sexp)))
 (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom (straight-use-package-by-default t))

(straight-use-package 'el-patch)


;; ----------------------------------------------------------------------------------------------------
;; load all modules
;; ----------------------------------------------------------------------------------------------------
(use-package f)

(defun load-dir (f libs-loaded)
  (dolist (file (directory-files f t ".+\\.elc?$"))
    (let ((library (file-name-sans-extension file)))
      (unless (member library libs-loaded)
        (load library nil t)
		(message "loaded %s" library)
        (push library libs-loaded)))))

(cl-defun load-modules (&optional (modules-dir "modules"))
  (setq libs-loaded (mapcar #'file-name-sans-extension (delq nil (mapcar #'car load-history))))
  (let ((dir (expand-file-name modules-dir user-emacs-directory)))
	(load-dir dir libs-loaded)
	(f-directories dir (lambda (d) (load-dir d libs-loaded)))))

(load-modules)

