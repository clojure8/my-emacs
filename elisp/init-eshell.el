;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package eshell-git-prompt
  :config
  ;; (eshell-git-prompt-use-theme 'powerline)
  (eshell-git-prompt-use-theme 'robbyrussell)
  )

(use-package eshell-z)

;;;###autoload
(defun aweshell-unpack (file &rest args)
  "Unpack FILE with ARGS."
  (let ((command (some (lambda (x)
                         (if (string-match-p (car x) file)
                             (cadr x)))
                       '((".*\.tar.bz2" "tar xjf")
                         (".*\.tar.gz" "tar xzf")
                         (".*\.bz2" "bunzip2")
                         (".*\.rar" "unrar x")
                         (".*\.gz" "gunzip")
                         (".*\.tar" "tar xf")
                         (".*\.tbz2" "tar xjf")
                         (".*\.tgz" "tar xzf")
                         (".*\.zip" "unzip")
                         (".*\.Z" "uncompress")
                         (".*" "echo 'Could not unpack the file:'")))))
    (let ((unpack-command(concat command " " file " " (mapconcat 'identity args " "))))
      (eshell/printnl "Unpack command: " unpack-command)
      (eshell-command-result unpack-command))
    ))
(defalias 'eshell/unpack 'aweshell-unpack)

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell)
  :bind
  ("s-`" . eshell-toggle))

(defalias 'eshell/ll (lambda () (eshell/ls "-l")))

(add-hook 'eshell-mode-hook (lambda ()
                              (eshell/alias "e" "find-file $1")
                              (eshell/alias "ff" "find-file $1")
                              (eshell/alias "emacs" "find-file $1")
                              (eshell/alias "ee" "find-file-other-window $1")

                              (eshell/alias "gss" "magit-status")
                              (eshell/alias "gd" "magit-diff-unstaged")
                              (eshell/alias "gds" "magit-diff-staged")
                              (eshell/alias "d" "dired $1")

                              ;; The 'ls' executable requires the Gnu version on the Mac
                              (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                                            "/usr/local/bin/gls"
                                          "/bin/ls")))
                                (eshell/alias "ll" (concat ls " -AlohG --color=always")))))

(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(eshell-smart-initialize)

(provide 'init-eshell)
