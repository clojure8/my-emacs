;;; init.el --- -*- lexical-binding: t -*-
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set) 'set-default) ',variable ,value))

(defvar gc-cons-threshold-up-limit (* 100 1024 1024))
(defvar gc-cons-threshold-default (* 20 1024 1024))

;; CheckVer
(cond ((version< emacs-version "26.1")
       (warn "M-EMACS requires Emacs 26.1 and above!"))
      ((let* ((early-init-f (expand-file-name "early-init.el" user-emacs-directory))
              (early-init-do-not-edit-d (expand-file-name "early-init-do-not-edit/" user-emacs-directory))
              (early-init-do-not-edit-f (expand-file-name "early-init.el" early-init-do-not-edit-d)))
         (and (version< emacs-version "27")
              (or (not (file-exists-p early-init-do-not-edit-f))
                  (file-newer-than-file-p early-init-f early-init-do-not-edit-f)))
         (make-directory early-init-do-not-edit-d t)
         (copy-file early-init-f early-init-do-not-edit-f t t t t)
         (add-to-list 'load-path early-init-do-not-edit-d)
         (require 'early-init))))
;; -CheckVer

;; BetterGC
(defvar better-gc-cons-threshold (* 1024 1024 1024) ;;1G
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))
;; -BetterGC

;; AutoGC
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
;; -AutoGC


;;========================================================================================
;; Package
;;========================================================================================
(setq package-archives '(
                         ("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))
                         ;; ("melpa" . "https://mirrors.163.com/elpa/melpa/")
                         ;; ("melpa-stable" . "https://mirrors.163.com/elpa/melpa-stable/")))

                         (package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)


;;==================================================
;; straight初始化
;;==================================================
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (load bootstrap-file nil 'nomessage))

;;==================================================
;; 引入一些基础包
;;==================================================
(use-package s)
(use-package f)
(use-package dash)
(use-package language-id)
(use-package el-patch)

;;================================================================================
;;; LoadPath
;;================================================================================
(defconst my-elisp-dir "~/.emacs.d/elisp")
(defconst my-autoload-file "~/.emacs.d/loaddefs.el")

(require 'cl-lib)
(require 'autoload)
(with-eval-after-load 'gnutls
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem"))

(defun refresh-load-path (&optional dir)
  "Refresh the load path of `site-lisp'."
  (interactive)
  (let ((default-directory (file-name-as-directory (or dir *site-lisp-directory*))))
    (push default-directory load-path)
    (normal-top-level-add-subdirs-to-load-path)))

(defun generate-autoloads (&optional dir target)
  "Generate autoload files for dir"
  (interactive)
  (with-temp-file (or target *extra-autoloads-file*)
    (cl-loop with generated-autoload-file = nil
             with dir = (or dir *site-lisp-directory*)
             with inhibit-message = t
             for f in (directory-files-recursively
                       dir
                       "\\.el$")
             for file = (file-truename f)
             for generated-autoload-load-name = (file-name-sans-extension
                                                 file)
             do (autoload-generate-file-autoloads file (current-buffer)))
    (cl-loop with cache = nil
             with load-path = (refresh-load-path dir)
             while (re-search-forward "^\\s-*(autoload\\s-+'[^ ]+\\s-+\"\\([^\"]*\\)\"" nil :no-error)
             for path = (match-string 1)
             do (replace-match
                 (or (cdr (assoc path cache))
                     (when-let* ((libpath (locate-library path))
                                 (libpath (file-name-sans-extension libpath)))
                       (push (cons path (abbreviate-file-name libpath)) cache)
                       libpath)
                     path)
                 :fixed-case :literal nil 1))))

(generate-autoloads my-elisp-dir my-autoload-file)
(load (expand-file-name my-autoload-file) t t)

;;==================================================
;; 在mac中使用代理
;;==================================================
(defun gen-autoloads ()
  (interactive)
  (generate-autoloads my-elisp-dir my-autoload-file))

(defun use-proxy ()
  (interactive)
  (setq url-gateway-method 'socks)
  (setq socks-server '("Default server" "127.0.0.1" 1080 5)))

;;=============================================
;; 自定义的一些配置导入
;;=============================================
(require 'init-base)
(require 'init-funcs)
(require 'init-ui)
(require 'init-pkgs)
(require 'init-org)
(require 'init-lsp-java)
;;(require 'init-nox-java)
(require 'init-treemacs)
(require 'init-git)
(require 'init-ivy)
(require 'init-helm)
(require 'init-evil)
