(setq package-archives '(
                         ("melpa" . "https://mirrors.163.com/elpa/melpa/")
                         ("melpa-stable" . "https://mirrors.163.com/elpa/melpa-stable/")))

(package-initialize)

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
     (bootstrap-version 5))
 ; (unless (file-exists-p bootstrap-file)
 ;   (with-current-buffer
 ;       (url-retrieve-synchronously
 ;        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
 ;        'silent 'inhibit-cookies)
 ;     (goto-char (point-max))
 ;     (eval-print-last-sexp)))
 (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom (straight-use-package-by-default t))

(straight-use-package 'el-patch)


(provide 'init-package-manager)
