;;; me-notes.el --- Notes management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package org-roam
  :straight t
  :demand t
  :after org-agenda
  :hook (org-roam-mode . org-roam-db-autosync-enable)
  :init
  (+map! :infix "n"
    "f" #'org-roam-node-find
    "p" #'my/org-roam-find-project
    "t" #'my/org-roam-capture-task
    "b" #'my/org-roam-capture-inbox
    "dY" #'org-roam-dailies-capture-yesterday
    "dT" #'org-roam-dailies-capture-tomorrow
    "r" #'org-roam-ref-find
    "i" #'org-roam-node-insert
    "I" #'org-roam-node-insert-immediate
    "R" #'org-roam-node-random
    "B" #'org-roam-buffer-display-dedicated)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)
  :custom
  (org-roam-directory "~/Roam/")
  (org-roam-node-display-template "${title} ${tags}")
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t))))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/set-org-agenda-files ()
       '("~/Org/inbox.org"
         "~/Org/agenda.org"
         "~/Org/todo.org"
         "~/Org/calendar-personal.org"
         "~/Org/calendar-work.org"
         "~/Org/projects.org"))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (append (my/set-org-agenda-files) (my/org-roam-list-notes-by-tag "Project"))))

;; Build the agenda list the first time for the session
(my/org-roam-refresh-agenda-list)

(defun my/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Project")
   :templates
   '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "tasks/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t))))

(defun my/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?")
                                  :if-new (file+head "inbox.org" "#+title: Inbox\n"))))

(defun my/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tag "Project"))
                     :templates '(("p" "project" plain "** TODO %?"
                                   :if-new (file+head+olp "tasks/%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                          ("Tasks"))))))

(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep nil) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
          '(("t" "tasks" entry "%?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (my/org-roam-copy-todo-to-today))))

(use-package org-roam-protocol
  :after org-roam
  :demand t
  :custom
  (org-roam-protocol-store-links t)
  ;; Add this as bookmarklet in your browser
  ;; javascript:location.href='org-protocol://roam-ref?template=r&ref=%27+encodeURIComponent(location.href)+%27&title=%27+encodeURIComponent(document.title)+%27&body=%27+encodeURIComponent(window.getSelection())
  (org-roam-capture-ref-templates
   '(("r" "ref" plain "%?"
      :if-new (file+head "web/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+created: %U\n\n${body}\n")
      :unnarrowed t)))
  :config
  ;; Save a local snapshot of the captured web page using "single-file-cli"
  (advice-add
   'org-roam-protocol-open-ref :after
   (defun +org-roam-protocol--single-file-snapshot-a (info)
     (+single-file
      (plist-get info :ref)
      (+file-name-incremental
       (expand-file-name
        (concat "web/snapshots/" (+clean-file-name (plist-get info :title)) ".html")
        org-roam-directory))))))

(use-package org-roam-ui
  :straight t
  :init
  (+map! "nu" #'org-roam-ui-open))

(use-package consult-org-roam
  :straight t
  :init
  (+map! :infix "n"
    "s" #'consult-org-roam-search
    "l" #'consult-org-roam-forward-links
    "b" #'consult-org-roam-backlinks
    "F" #'consult-org-roam-file-find)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r) ; custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-after-buffers t)
  :config
  (consult-org-roam-mode 1)
  ;; Eventually suppress previewing for certain functions
  (consult-customize consult-org-roam-forward-links :preview-key (kbd "M-.")))


(provide 'me-notes)

;;; me-notes.el ends here
