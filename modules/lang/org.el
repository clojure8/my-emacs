;; -*- coding: utf-8; lexical-binding: t; -*-

;;;;;;;;;;; org相关配置
(setq org-directory "~/orgs/")
(setq org-agenda-files '("~/orgs/agenda"))
(setq org-capture-templates '(("d" "工作日报" entry (file+olp+datetree "~/orgs/zeyi/daily.org")
                               "* 日报\n%?\nEntered on %U\n  %i\n")
                              ("a" "添加到TODOs" entry (file+olp+datetree "~/orgs/zeyi/todos.org")
                               "* 每日TODOs\n%?\nEntered on %U\n  %i\n")
                              ("T" "Tickler" entry
                               (file+headline "~/orgs/agenda/tickler.org" "Tickler")
                               "* %i%? \n %U")))
(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode)
  (org-mode . org-indent-mode))

(use-package org-download
  :ensure t
  :hook ((org-mode dired-mode) . org-download-enable)
  :config
  (setq-default org-download-image-dir "~/Pictures/foo/")
  (setq-default org-download--dir "~/Pictures/foo/")

  (defun +org-download-method (link)
    (org-download--fullname (org-link-unescape link)))
  (setq org-download-method '+org-download-method)

  (setq org-download-annotate-function (lambda (_link) "")
        org-download-method 'attach
        org-download-screenshot-method "screencapture -i %s"))



