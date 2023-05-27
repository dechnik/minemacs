;;; me-files.el --- File management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package dirvish
  :straight t
  :hook (minemacs-after-startup . dirvish-override-dired-mode)
  :custom
  (dirvish-attributes '(subtree-state nerd-icons file-size vc-state git-msg))
  (dirvish-cache-dir (+directory-ensure minemacs-cache-dir "dirvish/"))
  (dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  (dirvish-side-width 30)
  (dirvish-fd-default-dir "~/")
  (dirvish-use-header-line t) ; 'global make header line span all panes
  (dirvish-use-mode-line t)
  :init
  (+map!
    ;; Open
    "o-" '(dirvish :wk "Dirvish")
    "oq" '(dirvish-quick-access :wk "Dirvish quick access")
    ;; Search
    "sd" '(dirvish-fd :wk "Dirvish fd"))
  :config
  (+nvmap! :keymaps 'dirvish-mode-map
    "q" #'dirvish-quit
    "s" #'dirvish-subtree-toggle
    "y" #'dirvish-yank-menu))

(use-package vlf-setup
  :straight vlf
  :after minemacs-loaded
  :demand t)

(use-package treemacs
  :straight t
  :init
  (+map!
    "op" '(treemacs :wk "Side panel"))
  :custom
  (treemacs-persist-file (concat minemacs-local-dir "treemacs/persist.el"))
  (treemacs-last-error-persist-file (concat minemacs-local-dir "treemacs/last-error-persist.el"))
  (treemacs-width 30))

(use-package treemacs-evil
  :straight t
  :after treemacs evil
  :demand t)

(use-package treemacs-nerd-icons
  :straight t
  :after treemacs nerd-icons
  :demand t
  :config
  (treemacs-load-theme "nerd-icons"))


(provide 'me-files)

;;; me-files.el ends here
