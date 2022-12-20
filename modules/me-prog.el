;;; me-prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


;;; Tree sitter
(use-package tree-sitter
  :straight t
  :after minemacs-loaded
  :hook (tree-sitter-mode . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode 1))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(use-package evil-textobj-tree-sitter
  :straight t
  :after evil tree-sitter
  :config
  ;; evil-textobj-tree-sitter comes with no default keybindings,
  ;; Here is a keybindings (vaX) stolen from here:
  ;; https://github.com/meain/dotfiles/blob/master/emacs/.config/emacs/init.el
  (define-key
   evil-outer-text-objects-map
   "m" (evil-textobj-tree-sitter-get-textobj "import"
         '((python-mode . [(import_statement) @import])
           (go-mode . [(import_spec) @import])
           (rust-mode . [(use_declaration) @import]))))
  (define-key
   evil-outer-text-objects-map
   "f" (cons "evil-outer-function" (evil-textobj-tree-sitter-get-textobj "function.outer")))
  (define-key
   evil-inner-text-objects-map
   "f" (cons "evil-inner-function" (evil-textobj-tree-sitter-get-textobj "function.inner")))
  (define-key
   evil-outer-text-objects-map
   "c" (cons "evil-outer-class" (evil-textobj-tree-sitter-get-textobj "class.outer")))
  (define-key
   evil-inner-text-objects-map
   "c" (cons "evil-inner-class" (evil-textobj-tree-sitter-get-textobj "class.inner")))
  (define-key
   evil-outer-text-objects-map
   "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key
   evil-inner-text-objects-map
   "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key
   evil-outer-text-objects-map
   "o" (cons "evil-outer-loop" (evil-textobj-tree-sitter-get-textobj "loop.outer")))
  (define-key
   evil-inner-text-objects-map
   "o" (cons "evil-inner-loop" (evil-textobj-tree-sitter-get-textobj "loop.inner")))
  (define-key
   evil-outer-text-objects-map
   "v" (cons "evil-outer-conditional" (evil-textobj-tree-sitter-get-textobj "conditional.outer")))
  (define-key
   evil-inner-text-objects-map
   "v" (cons "evil-inner-conditional" (evil-textobj-tree-sitter-get-textobj "conditional.inner")))
  (define-key
   evil-inner-text-objects-map
   "a" (cons "evil-inner-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.inner")))
  (define-key
   evil-outer-text-objects-map
   "a" (cons "evil-outer-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.outer")))

  (define-key
   evil-normal-state-map
   (kbd "]a") (cons "goto-parameter-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "parameter.inner"))))
  (define-key
   evil-normal-state-map
   (kbd "[a") (cons "goto-parameter-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "parameter.inner" t))))
  (define-key
   evil-normal-state-map
   (kbd "]A") (cons "goto-parameter-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "parameter.inner" nil t))))
  (define-key
   evil-normal-state-map
   (kbd "[A") (cons "goto-parameter-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "parameter.inner" t t))))
  (define-key
   evil-normal-state-map
   (kbd "]v") (cons "goto-conditional-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "conditional.outer"))))
  (define-key
   evil-normal-state-map
   (kbd "[v") (cons "goto-conditional-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "conditional.outer" t))))
  (define-key
   evil-normal-state-map
   (kbd "]V") (cons "goto-conditional-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "conditional.outer" nil t))))
  (define-key
   evil-normal-state-map
   (kbd "[V") (cons "goto-conditional-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "conditional.outer" t t))))
  (define-key
   evil-normal-state-map
   (kbd "]c") (cons "goto-class-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "class.outer"))))
  (define-key
   evil-normal-state-map
   (kbd "[c") (cons "goto-class-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "class.outer" t))))
  (define-key
   evil-normal-state-map
   (kbd "]C") (cons "goto-class-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "class.outer" nil t))))
  (define-key
   evil-normal-state-map
   (kbd "[C") (cons "goto-class-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "class.outer" t t))))
  (define-key
   evil-normal-state-map
   (kbd "]n") (cons "goto-comment-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "comment.outer"))))
  (define-key
   evil-normal-state-map
   (kbd "[n") (cons "goto-comment-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "comment.outer" t))))
  (define-key
   evil-normal-state-map
   (kbd "]N") (cons "goto-comment-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "comment.outer" nil t))))
  (define-key
   evil-normal-state-map
   (kbd "[N") (cons "goto-comment-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "comment.outer" t t))))
  (define-key
   evil-normal-state-map
   (kbd "]f") (cons "goto-function-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer") (reposition-window))))
  (define-key
   evil-normal-state-map
   (kbd "[f") (cons "goto-function-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" t) (reposition-window))))
  (define-key
   evil-normal-state-map
   (kbd "]F") (cons "goto-function-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t) (reposition-window))))
  (define-key
   evil-normal-state-map
   (kbd "[F") (cons "goto-function-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" t t) (reposition-window)))))

;;; Eglot + LSP
(use-package eglot
  :straight t
  :hook ((c++-mode
          c++-ts-mode
          c-mode c-ts-mode
          python-mode python-ts-mode
          rust-mode
          cmake-mode) . eglot-ensure)
  :init
  (+map
    "cfF" #'eglot-format-buffer
    "cd"  '(eglot-find-declaration :wk "Find declaration")
    "ci"  '(eglot-find-implementation :wk "Find implementation")
    "ct"  '(eglot-find-typeDefinition :wk "Find type definition")
    "ca"  '(eglot-code-actions :wk "Code actions")
    "cr"  '(nil :wk "refactor")
    "crr" '(eglot-rename :wk "Rename")
    "crR" '(eglot-code-action-rewrite :wk "Rewrite")
    "crf" '(eglot-code-action-quickfix :wk "Quick fix")
    "cri" '(eglot-code-action-inline :wk "Inline")
    "cre" '(eglot-code-action-extract :wk "Extract")
    "cro" '(eglot-code-action-organize-imports :wk "Organize imports")
    "cs"  '(nil :wk "eglot session")
    "css" '(eglot :wk "Start")
    "csq" '(eglot-shutdown :wk "Shutdown")
    "csr" '(eglot-reconnect :wk "Reconnect")
    "csQ" '(eglot-shutdown-all :wk "Shutdown all")
    "cw"  '(eglot-show-workspace-configuration :wk "Eglot workspace config"))
  :custom
  (eglot-autoshutdown t) ;; shutdown after closing the last managed buffer
  (eglot-sync-connect 0) ;; async, do not block
  (eglot-extend-to-xref t) ;; can be interesting!
  :config
  ;; Prioritize ccls
  (add-to-list
   'eglot-server-programs
   (cons
    '(c++-mode c++-ts-mode c-mode c-ts-mode)
    (eglot-alternatives '("ccls" ("clangd"
                                  "--background-index"
                                  "-j=12"
                                  "--query-driver=/usr/bin/**/clang-*,/bin/clang,/bin/clang++,/usr/bin/gcc,/usr/bin/g++"
                                  "--clang-tidy"
                                  ;; "--clang-tidy-checks=*"
                                  "--all-scopes-completion"
                                  "--cross-file-rename"
                                  "--completion-style=detailed"
                                  "--header-insertion-decorators"
                                  "--header-insertion=iwyu"
                                  "--pch-storage=memory"))))))

(use-package eldoc-box
  :straight t
  :hook (prog-mode . eldoc-box-hover-at-point-mode))

;;; Formatting
(use-package apheleia
  :straight t
  :general
  (+map "cff" #'apheleia-format-buffer)
  :config
  (dolist (mode '(emacs-lisp-mode lisp-data-mode scheme-mode))
    (push (cons mode 'lisp-indent) apheleia-mode-alist)))

(use-package editorconfig
  :straight t
  :general
  (+map
    "fc" '(editorconfig-find-current-editorconfig :wk "Find current EditorConfig")
    "cfe" #'editorconfig-format-buffer)
  :hook (prog-mode . editorconfig-mode))

(use-package clang-format
  :straight t
  :commands clang-format clang-format-region clang-format-buffer
  :general
  (+map "cfc" #'clang-format-buffer))

(use-package flymake
  :straight t
  :defer t
  :init
  (+map "tf" #'flymake-mode))

;;; Modes
(use-package vimrc-mode
  :straight t
  :mode "\\.vim\\(rc\\)?\\'")

(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'"
  :straight (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*")))

(use-package cmake-font-lock
  :straight (:host github :repo "Lindydancer/cmake-font-lock" :files (:defaults "*"))
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package plantuml-mode
  :straight t
  :defer t
  :mode "\\.plantuml\\'"
  :hook (plantuml-mode . +plantuml-mode-setup)
  :custom
  (plantuml-jar-path (+expand 'local "plantuml/plantuml.jar"))
  (plantuml-indent-level 2)
  :config
  (setq
   plantuml-default-exec-mode
   ;; Prefer the system executable
   (if (executable-find "plantuml")
       'executable
     ;; Then, if a JAR exists, use it
     (or (and (file-exists-p plantuml-jar-path) 'jar)
         ;; otherwise, try to download a JAR in interactive mode
         (and (not noninteractive) (plantuml-download-jar) 'jar)
         ;; Fall back to server
         'server)))

  ;; Add support fot capf, rather than the builtin `plantuml-complete-symbol'
  (defun +plantuml-mode-setup ()
    (add-to-list
     'completion-at-point-functions
     (defun +plantuml-complete-at-point ()
       "Perform symbol-at-pt completion on word before cursor."
       (let* ((end-pos (point))
              (sym-at-pt (or (thing-at-point 'symbol) ""))
              (max-match (try-completion sym-at-pt plantuml-kwdList)))
         (unless (null max-match)
           (list (- end-pos (length sym-at-pt))
                 end-pos
                 (if (eq max-match t)
                     (list keyword)
                   (all-completions sym-at-pt plantuml-kwdList))))))))

  (+map-local :keymaps 'plantuml-mode-map
    "p" #'plantuml-preview-buffer
    "P" #'plantuml-preview
    "d" `(,(+cmdfy!
            (if plantuml-mode-debug-enabled
                (plantuml-disable-debug)
              (plantuml-enable-debug)))
          :wk "Toggle debug")))

(use-package rust-mode
  :straight t
  :mode "\\.rs\\'"
  :config
  (+map-local :keymaps 'rust-mode-map
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

(use-package dumb-jump
  :straight t
  :defer t
  :custom
  (dumb-jump-selector 'completing-read)
  :init
  ;; Use as xref backend
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

  ;; Define Hydra keybinding (from the repo's examples)
  (defhydra +dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump."
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))

  (+map
    "cj" '(+dumb-jump-hydra/body :wk "+dumb-jump-hydra")))

(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo")
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq
   hl-todo-keyword-faces
   (append
    hl-todo-keyword-faces
    '(("BUG" . "#ee5555")
      ("PROJ" . "#447f44")))))

(use-package rainbow-mode
  :straight t
  :defer t
  :hook (prog-mode . rainbow-mode))

(use-package lua-mode
  :straight t
  :custom
  (lua-indent-level 2)
  :config
  (with-eval-after-load 'eglot
    (when (executable-find "lua-language-server")
      (add-to-list
       'eglot-server-programs
       `(lua-mode . ,(eglot-alternatives '(("lua-language-server") ("lua-lsp"))))))))


(provide 'me-prog)

;;; me-prog.el ends here
