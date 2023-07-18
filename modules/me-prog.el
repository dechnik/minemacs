;;; me-prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(if (not (+emacs-features-p 'tree-sitter))
    ;; Use the external `tree-sitter' module
    (+load minemacs-modules-dir "obsolete/me-tree-sitter.el")
  (use-package treesit-auto
    :straight (:host github :repo "renzmann/treesit-auto")
    :hook (minemacs-after-startup . global-treesit-auto-mode)
    :hook (minemacs-build-functions . treesit-auto-install-all)
    :custom
    (treesit-auto-install 'prompt)
    :config
    ;; Install all languages when calling `treesit-auto-install-all'
    (setq treesit-language-source-alist (treesit-auto--build-treesit-source-alist)))

  ;; To avoid installing `tree-sitter' as the used `ts-fold' fork uses the
  ;; built-in `treesit'
  (push 'tree-sitter straight-built-in-pseudo-packages)

  (use-package ts-fold
    :straight (:host github :repo "abougouffa/ts-fold" :branch "andrew-sw/treesit-el-support")
    :after treesit treesit-auto
    :demand t
    :init
    (global-ts-fold-mode 1))

  (use-package combobulate
    :straight t
    :hook ((python-ts-mode js-ts-mode css-ts-mode yaml-ts-mode typescript-ts-mode tsx-ts-mode) . combobulate-mode)
    :custom
    (combobulate-key-prefix "C-c o"))

  ;; From: github.com/MaskRay/ccls/wiki/eglot#misc
  (defun +eglot-ccls-inheritance-hierarchy (&optional derived)
    "Show inheritance hierarchy for the thing at point.
If DERIVED is non-nil (interactively, with prefix argument), show
the children of class at point."
    (interactive "P")
    (if-let* ((res (jsonrpc-request
                    (eglot--current-server-or-lose)
                    :$ccls/inheritance
                    (append (eglot--TextDocumentPositionParams)
                            `(:derived ,(if derived t :json-false))
                            '(:levels 100) '(:hierarchy t))))
              (tree (list (cons 0 res))))
        (with-help-window "*ccls inheritance*"
          (with-current-buffer standard-output
            (while tree
              (pcase-let ((`(,depth . ,node) (pop tree)))
                (cl-destructuring-bind (&key uri range) (plist-get node :location)
                  (insert (make-string depth ?\ ) (plist-get node :name) "\n")
                  (make-text-button
                   (+ (pos-bol 0) depth) (pos-eol 0)
                   'action (lambda (_arg)
                             (interactive)
                             (find-file (eglot--uri-to-path uri))
                             (goto-char (car (eglot--range-region range)))))
                  (cl-loop for child across (plist-get node :children)
                           do (push (cons (1+ depth) child) tree)))))))
      (eglot--error "Hierarchy unavailable"))))

(use-package consult-eglot
  :straight t
  :after consult eglot
  :init
  (+map! :keymaps 'eglot-mode-map
    "cs" '(consult-eglot-symbols :wk "Symbols"))
  :config
  ;; Provide `consult-lsp' functionality from `consult-eglot', useful for
  ;; packages that relays on `consult-lsp' (like `dirvish-subtree').
  (unless (or (memq 'me-lsp minemacs-modules)
              (fboundp 'consult-lsp-file-symbols))
    (defalias 'consult-lsp-file-symbols #'consult-eglot-symbols)))

(use-package eldoc-box
  :straight t
  :hook (prog-mode . eldoc-box-hover-at-point-mode)
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

(use-package compile-multi
  :straight t
  :commands +project-compile-multi
  :init
  (+map! "pC" #'+project-compile-multi)
  :config
  (defun +project-compile-multi ()
    "Like `project-compile', but uses `compile-multi'."
    (declare (interactive-only compile))
    (interactive)
    (let ((default-directory (project-root (project-current t)))
          (compilation-buffer-name-function
           (or project-compilation-buffer-name-function
               compilation-buffer-name-function)))
      (call-interactively #'compile-multi))))

(use-package apheleia
  :straight t
  :init
  (+map! "cff" #'apheleia-format-buffer)
  :config
  (dolist (alist '((lisp-data-mode . lisp-indent)
                   (emacs-lisp-mode . lisp-indent)
                   (sh-mode . shfmt)))
    (add-to-list 'apheleia-mode-alist alist)))

(use-package editorconfig
  :straight t
  :hook (prog-mode . editorconfig-mode)
  :init
  (+map!
    "fc" '(editorconfig-find-current-editorconfig :wk "Find current EditorConfig")
    "cfe" #'editorconfig-format-buffer))

(use-package clang-format
  :straight t
  :init
  (+map! :keymaps '(c-mode-map c++-mode-map c-ts-mode-map c++-ts-mode-map
                    cuda-mode-map scad-mode-map)
    "cfc" #'clang-format-buffer))

;;; Modes
(use-package vimrc-mode
  :straight t
  :mode "\\.vim\\(rc\\)?\\'")

(unless (+emacs-features-p 'tree-sitter)
  (+load minemacs-modules-dir "obsolete/me-cmake.el"))

(use-package rust-mode
  :straight t
  :commands
  rust-compile rust-compile-release
  rust-check rust-test
  rust-run rust-run-release rust-run-clippy
  rust-format-buffer rust-goto-format-problem
  rust-enable-format-on-save
  :init
  (+map-local! :keymaps '(rust-mode-map rust-ts-mode-map)
    "c" #'rust-compile
    "C" #'rust-compile-release
    "k" #'rust-check
    "t" #'rust-test
    "r" #'rust-run
    "R" #'rust-run-release
    "y" #'rust-run-clippy
    "f" #'rust-format-buffer
    "F" #'rust-goto-format-problem
    "S" #'rust-enable-format-on-save))

(use-package cuda-mode
  :straight t
  :hook (cuda-mode . display-line-numbers-mode)
  :hook (cuda-mode . hs-minor-mode))

(use-package opencl-mode
  :straight t
  :mode "\\.cl\\'")

(use-package dumb-jump
  :straight t
  :commands +dumb-jump-hydra/body
  :custom
  (dumb-jump-selector 'completing-read)
  :init
  (+map!
    "cj" '(+dumb-jump-hydra/body :wk "+dumb-jump-hydra"))
  ;; Use as xref backend
  (with-eval-after-load 'xref
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
  :config
  ;; Define Hydra keybinding (from the repo's examples)
  (defhydra +dumb-jump-hydra (:color blue :hint nil :foreign-keys warn)
    "
[Dumb Jump]                                                                         [_q_] quit
  ├─────────────────────────────────────────────────────────────────────────────────────────╮
  │  [_j_] Go          [_o_] Go other window    [_e_] Go external   [_x_] Go external other window  │
  │  [_i_] Go prompt   [_l_] Quici look         [_b_] Back                                        │
  ╰─────────────────────────────────────────────────────────────────────────────────────────╯
"
    ("j" dumb-jump-go)
    ("o" dumb-jump-go-other-window)
    ("e" dumb-jump-go-prefer-external)
    ("x" dumb-jump-go-prefer-external-other-window)
    ("i" dumb-jump-go-prompt)
    ("l" dumb-jump-quick-look)
    ("b" dumb-jump-back)
    ("q" nil :color blue)))

(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo")
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        (append
         hl-todo-keyword-faces
         '(("BUG" . "#ee5555")
           ("PROJ" . "#447f44")
           ("IDEA" . "#0fa050")
           ("INFO" . "#0e9030")
           ("TWEAK" . "#fe9030")
           ("PERF" . "#e09030")))))

(use-package rainbow-mode
  :straight t
  :init
  (+map! :keymaps '(prog-mode-map conf-mode-map text-mode-map)
    "tR" #'rainbow-mode))

(use-package lua-mode
  :straight t
  :custom
  (lua-indent-level 2))

(use-package franca-idl
  :straight (:host github :repo "zeph1e/franca-idl.el"))

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'"
  :config
  (+eglot-register 'nix-mode "rnix-lsp"))

(provide 'me-prog)

;;; me-prog.el ends here
