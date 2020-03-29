
;;custom org mode
(csetq org-directory "~/orgs")
(csetq org-table '((t (:foreground "#6c71c4" :family "Ubuntu Mono"))))
(csetq org-default-notes-file (concat org-directory "/notes.org"))
(csetq org-download-image-dir "~/orgs/downloads")
(csetq org-agenda-files
       (list "~/orgs/agenda/gcal.org"
             "~/orgs/agenda/i.org"
             "~/orgs/agenda/schedule.org"))

(use-package org-roam
  :hook 
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/orgs")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert)))
  :init
  (setq org-roam-link-title-format "R:%s")
  (setq org-roam-buffer-width 0.4)
  (setq org-roam-buffer "*my-roam-buffer*")
  (setq org-roam-completion-system 'ivy)
  (setq org-roam-graphviz-executable "/usr/local/bin/dot")
  (setq org-roam-graphviz-executable "/usr/local/bin/neato")
  (setq org-roam-graphviz-extra-options '(("overlap" . "false")))
  ;; (setq org-roam-graph-viewer "/usr/local/bin/image-viewer")
  )

(use-package org-download)

(use-package org-bullets
  :hook ((org-mode . org-bullets-mode)
         (org-mode . org-indent-mode)))

(use-package noflet)

(setq org-capture-templates
      '(("a" "Appointment" entry (file  "~/orgs/agenda/gcal.org" "Appointments")
         "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n")
        ("n" "Note" entry (file+headline "~/orgs/agenda/notes.org" "Notes")
         "* Note %?\n%T")
        ("l" "Link" entry (file+headline "~/orgs/agenda/links.org" "Links")
         "* %? %^L %^g \n%T" :prepend t)
        ("b" "Blog idea" entry (file+headline "~/orgs/agenda/i.org" "Blog Topics:")
         "* %?\n%T" :prepend t)
        ("t" "To Do Item" entry (file+headline "~/orgs/agenda/i.org" "To Do Items")
         "* %?\n%T" :prepend t)
        ("j" "Journal" entry (file+datetree "~/orgs/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("s" "Screencast" entry (file "~/orgs/agenda/screencastnotes.org")
         "* %?\n%i\n")))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))


(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    (org-capture)))

(define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
(define-key org-mode-map (kbd "M-h") nil)
(define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))
(setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar")
(setq org-roam-ditaa-executable "/usr/local/bin/ditaa")
(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
;; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")
(setq org-confirm-babel-evaluate nil)
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

(org-babel-do-load-languages
 'org-babel-load-languages
 `((R . t)
   (ditaa . t)
   (dot . t);;Graphviz
   (emacs-lisp . t)
   (plantuml . t)
   (python . t)))

(provide 'init-org)
