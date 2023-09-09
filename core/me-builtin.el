;;; me-builtin.el --- Emacs built-in packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package transient
  :straight (:type built-in)
  :config
  ;; Map ESC and q to quit transient
  (keymap-set transient-map "<escape>" 'transient-quit-one)
  (keymap-set transient-map "q" 'transient-quit-one))

(use-package map
  :straight (:type built-in))

(use-package let-alist
  :straight (:type built-in))

(use-package password-cache
  :straight (:type built-in)
  :custom
  (password-cache t) ; Enable password caching
  (password-cache-expiry 60)) ; One minute, default is 16

(use-package auth-source
  :straight (:type built-in)
  :custom
  (auth-sources '("~/.authinfo.gpg")) ; Default auth-sources to GPG
  (auth-source-do-cache t) ; Enable caching, do not keep asking about GPG key
  (auth-source-cache-expiry 86400)) ; All day, default is 2h (7200)

(use-package epa
  :straight (:type built-in)
  :custom
  ;; Force gpg-agent to use minibuffer to prompt for passphrase (GPG 2.1+).
  (epg-pinentry-mode 'loopback))

(use-package epa-file
  :straight (:type built-in)
  :after minemacs-loaded
  :demand t
  :config
  (+shutup! (epa-file-enable)))

(use-package dired
  :straight (:type built-in)
  ;; Enable adding mail attachements from dired "C-c RET C-a" for
  ;; `gnus-dired-attach'
  :hook (dired-mode . turn-on-gnus-dired-mode)
  :custom
  (dired-dwim-target t)
  (dired-auto-revert-buffer t))

(use-package doc-view
  :straight (:type built-in)
  :custom
  (doc-view-continuous t)
  (doc-view-mupdf-use-svg (+emacs-features-p 'rsvg)))

(use-package project
  :straight (:type built-in)
  :after minemacs-loaded
  :demand t
  :custom
  (project-list-file (concat minemacs-local-dir "project-list.el"))
  (project-vc-extra-root-markers '(".projectile.el" ".project.el" ".project"))
  :init
  (+map! ":"  #'project-find-file)
  (+map! :infix "p" ;; project
    "w"  #'project-switch-project
    "c"  #'project-compile
    "d"  #'project-find-dir
    "f"  #'project-find-file
    "k"  #'project-kill-buffers
    "b"  #'project-switch-to-buffer
    "a"  #'+project-add-project
    "D"  #'+dir-locals-open-or-create
    "-"  #'project-dired
    "x"  #'project-execute-extended-command
    ;; compile/test
    "c" #'project-compile
    ;; run
    "r"  '(nil :wk "run")
    "re" #'project-eshell
    "rg" #'+project-gdb
    "rs" #'project-shell
    "rc" #'project-shell-command
    "rC" #'project-async-shell-command
    ;; forget
    "F"  '(nil :wk "forget/cleanup")
    "Fz" '(project-forget-zombie-projects :wk "Zombie projects")
    "Fp" '(project-forget-project :wk "Project")
    "Fu" '(project-forget-projects-under :wk "Projects under...")
    ;; search/replace
    "s"  '(nil :wk "search/replace")
    "ss" #'project-search
    "sn" '(fileloop-continue :wk "Next match")
    "sr" #'project-query-replace-regexp
    "sf" #'project-find-regexp))

(use-package tab-bar
  :straight (:type built-in)
  :hook (minemacs-after-startup . tab-bar-mode)
  :custom
  (tab-bar-format '(tab-bar-format-history
                    tab-bar-format-tabs
                    tab-bar-separator))
  (tab-bar-tab-hints t)
  (tab-bar-tab-name-format-function #'+tab-bar-tab-spaced-name-format)
  (tab-bar-close-button-show nil)
  (tab-bar-show nil)
  :config
  (defun +tab-bar-tab-spaced-name-format (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat (if tab-bar-tab-hints (format " %c " (+ ?① (1- i)) " "))
               (alist-get 'name tab)
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   ""))
       'face (funcall tab-bar-tab-face-function tab)))))

(use-package flymake
  :straight (:type built-in)
  :init
  (+map! "tf" #'flymake-mode)
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-error-bitmap '(+flymake-bitmap-left-arrow-hi-res compilation-error))
  (flymake-warning-bitmap '(+flymake-bitmap-left-arrow-hi-res compilation-warning))
  (flymake-note-bitmap '(+flymake-bitmap-left-arrow-hi-res compilation-info))
  :config
  (+map-local! :keymaps 'flymake-mode-map
    "f"  '(nil :wk "flymake")
    "fn" #'flymake-goto-next-error
    "fN" #'flymake-goto-prev-error
    "fs" #'flymake-start
    "fb" #'flymake-show-buffer-diagnostics
    "fp" #'flymake-show-project-diagnostics)

  ;; Use the session's load-path with flymake
  (setq elisp-flymake-byte-compile-load-path
        (append elisp-flymake-byte-compile-load-path load-path))

  ;; Larger right frings
  (set-fringe-style '(8 . 13))

  ;; Better fringe bitmaps
  (define-fringe-bitmap '+flymake-bitmap-arrow
    [#b11111000
     #b01111100
     #b00111110
     #b00011111
     #b00111110
     #b01111100
     #b11111000])
  (define-fringe-bitmap '+flymake-bitmap-arrow-hi-res
    [#b01111000000
     #b00111100000
     #b00011110000
     #b00001111000
     #b00000111100
     #b00000011110
     #b00000011110
     #b00000111100
     #b00001111000
     #b00011110000
     #b00111100000
     #b01111000000]
    nil 13)
  (define-fringe-bitmap '+flymake-bitmap-left-arrow-hi-res
    [#b00000011110
     #b00000111100
     #b00001111000
     #b00011110000
     #b00111100000
     #b01111000000
     #b01111000000
     #b00111100000
     #b00011110000
     #b00001111000
     #b00000111100
     #b00000011110]
    nil 13))

(use-package xt-mouse
  :straight (:type built-in)
  :hook (tty-setup . xterm-mouse-mode))

(use-package tramp
  :straight (:type built-in)
  :init
  ;; This is faster than the default "scp"
  (unless os/win
    (setq tramp-default-method "ssh"))
  :custom
  (tramp-auto-save-directory (concat minemacs-local-dir "tramp/auto-save/"))
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-persistency-file-name (concat minemacs-local-dir "tramp/persistency.el"))
  (tramp-default-remote-shell "/bin/bash"))

(use-package eshell
  :straight (:type built-in)
  :custom
  (eshell-aliases-file (concat minemacs-local-dir "eshell/aliases"))
  (eshell-directory-name (+directory-ensure minemacs-local-dir "eshell/"))
  (eshell-history-file-name (concat minemacs-local-dir "eshell/history.el"))
  (eshell-last-dir-ring-file-name (concat minemacs-local-dir "eshell/last-dir-ring.el"))
  (eshell-login-script (concat minemacs-local-dir "eshell/login"))
  (eshell-rc-script (concat minemacs-local-dir "eshell/rc"))
  (eshell-scroll-to-bottom-on-input 'this))

(use-package reftex ;; Inspired by Doom Emacs
  :straight (:type built-in)
  :hook (LaTeX-mode . turn-on-reftex)
  :hook (reftex-toc-mode . reftex-toc-rescan)
  :custom
  ;; Get RefTeX working with BibLaTeX. See: tex.stackexchange.com/a/31992/43165
  (reftex-cite-format
   '((?a . "\\autocite[]{%l}")
     (?b . "\\blockcquote[]{%l}{}")
     (?c . "\\cite[]{%l}")
     (?f . "\\footcite[]{%l}")
     (?n . "\\nocite{%l}")
     (?p . "\\parencite[]{%l}")
     (?s . "\\smartcite[]{%l}")
     (?t . "\\textcite[]{%l}"))
   ;; This is needed when `reftex-cite-format' is set. See:
   ;; superuser.com/a/1386206
   (LaTeX-reftex-cite-format-auto-activate nil)
   (reftex-plug-into-AUCTeX t)
   (reftex-toc-split-windows-fraction 0.3))
  :config
  (+map-local! :keymaps 'reftex-mode-map
    ";" 'reftex-toc)
  (+nvmap! :keymaps 'reftex-toc-mode-map
    "j"   #'next-line
    "k"   #'previous-line
    "q"   #'kill-buffer-and-window
    "ESC" #'kill-buffer-and-window)
  (with-eval-after-load 'evil
    (add-hook 'reftex-mode-hook #'evil-normalize-keymaps)))

(use-package bibtex
  :straight (:type built-in)
  :hook (bibtex-mode . display-line-numbers-mode)
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-align-at-equal-sign t)
  (bibtex-text-indentation 20)
  :config
  (+map-local! :keymaps 'bibtex-mode-map
    "l" #'bibtex-fill-entry
    "r" #'bibtex-reformat))

(use-package oc
  :straight (:type built-in)
  :after org
  :demand t
  :custom
  (org-cite-export-processors '((latex biblatex) (t csl)))
  (org-support-shift-select t)
  :config
  (+map-local! :keymaps 'org-mode-map
    "C" #'org-cite-insert))

(use-package oc-csl
  :straight (:type built-in)
  :after oc
  :demand t)

(use-package oc-natbib
  :straight (:type built-in)
  :after oc
  :demand t)

(use-package oc-biblatex
  :straight (:type built-in)
  :after oc
  :demand t)

(when (+emacs-features-p 'tree-sitter)
  (use-package treesit
    :straight (:type built-in)
    :custom
    (treesit-font-lock-level 4))

  (use-package dockerfile-ts-mode
    :straight (:type built-in)
    :mode "/Dockerfile\\'")

  (use-package cmake-ts-mode
    :straight (:type built-in)
    :mode "CMakeLists\\.txt\\'"
    :mode "\\.cmake\\'"))

(use-package autoinsert
  ;; NOTE: When prompting (like in Keywords), hit M-RET when finished
  :straight (:type built-in)
  :hook (minemacs-after-startup . auto-insert-mode)
  :custom
  (auto-insert-directory (+directory-ensure minemacs-local-dir "auto-insert/")))

(use-package hideif
  :straight (:type built-in)
  :init
  (defun +hide-ifdef-mode-maybe-h ()
    ;; If `me-lsp' is enabled, `lsp-semantic-tokens-mode' should do a better job,
    ;; so we don't enable `hide-ifdef-mode'.
    (unless (or (bound-and-true-p lsp-semantic-tokens-mode)
                (bound-and-true-p lsp-semantic-tokens-enable))
      (hide-ifdef-mode 1)))
  (defun +hide-ifdef-auto-enable ()
    (interactive)
    (if prefix-arg
        (+remove-hook! (c-mode c-ts-mode c++-mode c++-ts-mode cuda-mode opencl-mode)
          #'+hide-ifdef-mode-maybe-h)
      (+add-hook! (c-mode c-ts-mode c++-mode c++-ts-mode cuda-mode opencl-mode)
                  :depth 101 #'+hide-ifdef-mode-maybe-h)))
  :custom
  (hide-ifdef-shadow t)
  (hide-ifdef-initially t))

(use-package hl-line
  :straight (:type built-in)
  ;; Highlight the current line
  :hook ((prog-mode conf-mode text-mode) . hl-line-mode))

(use-package hideshow
  :straight (:type built-in)
  ;; Hide/show code blocks, a.k.a. code folding
  :hook ((prog-mode conf-mode) . hs-minor-mode))

(use-package xref
  :straight (:type built-in)
  :custom
  ;; Use completion in the minibuffer instead of definitions buffer
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package eglot
  :straight `(:type ,(if (< emacs-major-version 29) 'git 'built-in))
  :hook (eglot-managed-mode . eglot-inlay-hints-mode)
  :custom
  (eglot-autoshutdown t) ; shutdown after closing the last managed buffer
  (eglot-sync-connect 0) ; async, do not block
  (eglot-extend-to-xref t) ; can be interesting!
  (eglot-report-progress nil) ; disable annoying messages in echo area!
  :init
  ;; Register global keybinding
  (+map! :infix "c"
    "e"  '(nil :wk "eglot session")
    "ee" #'eglot
    "eA" #'+eglot-auto-enable)
  (defcustom +eglot-auto-enable-modes
    '(c++-mode c++-ts-mode c-mode c-ts-mode
      python-mode python-ts-mode nix-mode
      rust-mode rust-ts-mode cmake-mode
      js-mode js-ts-mode typescript-mode typescript-ts-mode
      json-mode json-ts-mode js-json-mode)
    "Modes for which Eglot can be automatically enabled by `+eglot-auto-enable'."
    :group 'minemacs-prog
    :type '(repeat symbol))
  (defun +eglot-auto-enable ()
    "Auto-enable Eglot in configured modes in `+eglot-auto-enable-modes'."
    (interactive)
    (dolist (mode +eglot-auto-enable-modes)
      (let ((hook (intern (format "%s-hook" mode))))
        (add-hook hook #'eglot-ensure)
        (remove-hook hook #'lsp-deferred))))
  :config
  ;; Modified from Crafted Emacs, pass `eglot-server-programs' to this function
  ;; to fill `+eglot-auto-enable-modes' with all supported modes.
  (defun +eglot-use-on-all-supported-modes (mode-list)
    (dolist (mode-def mode-list)
      (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
        (cond
         ((listp mode) (+eglot-use-on-all-supported-modes mode))
         (t
          (when (and (not (eq 'clojure-mode mode)) ; prefer cider
                     (not (eq 'lisp-mode mode))    ; prefer sly
                     (not (eq 'scheme-mode mode))) ; prefer geiser
            (add-to-list '+eglot-auto-enable-modes mode)))))))
  (+map! :keymaps 'eglot-mode-map
    :infix "c"
    "fF" #'eglot-format-buffer
    "d"  '(eglot-find-declaration :wk "Find declaration")
    "i"  '(eglot-find-implementation :wk "Find implementation")
    "t"  '(eglot-find-typeDefinition :wk "Find type definition")
    "a"  '(eglot-code-actions :wk "Code actions")
    "r"  '(nil :wk "refactor")
    "rr" '(eglot-rename :wk "Rename")
    "rR" '(eglot-code-action-rewrite :wk "Rewrite")
    "rf" '(eglot-code-action-quickfix :wk "Quick fix")
    "ri" '(eglot-code-action-inline :wk "Inline")
    "re" '(eglot-code-action-extract :wk "Extract")
    "ro" '(eglot-code-action-organize-imports :wk "Organize imports")
    "eq" '(eglot-shutdown :wk "Shutdown")
    "er" '(eglot-reconnect :wk "Reconnect")
    "eQ" '(eglot-shutdown-all :wk "Shutdown all")
    "w"  '(eglot-show-workspace-configuration :wk "Eglot workspace config"))

  (+eglot-register
    '(c++-mode c++-ts-mode c-mode c-ts-mode)
    '("clangd"
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
      "--pch-storage=memory")
    "ccls"))

(use-package eldoc
  :straight (:type built-in)
  :custom
  (eldoc-documentation-strategy #'eldoc-documentation-compose))

(use-package compile
  :straight (:type built-in)
  :commands +toggle-bury-compilation-buffer-if-successful
  ;; Enable ANSI colors in compilation buffer
  :hook (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-scroll-output t) ; Keep scrolling the compilation buffer, `first-error' can be interesting
  (compilation-always-kill t) ; Always kill current compilation process before starting a new one
  (compilation-skip-visited t) ; Skip visited messages on compilation motion commands
  (compilation-window-height 12) ; Keep it readable
  :init
  (defcustom +compilation-auto-bury-msg-level "warning"
    "Level of messages to consider OK to auto-bury the compilation buffer."
    :group 'minemacs-prog
    :type '(choice (const "warning") (const "error") string))
  (defcustom +compilation-auto-bury-delay 3.0
    "The delay in seconds after which the compilation buffer is buried."
    :group 'minemacs-prog
    :type 'number)
  :config
  ;; Integration of `compile' with `savehist'
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'compile-history))

  ;; Auto-close the compilation buffer if succeeded without warnings.
  ;; Adapted from: stackoverflow.com/q/11043004/3058915
  (defun +compilation--bury-if-successful-h (buf str)
    "Bury the compilation buffer if it succeeds without warnings."
    (when (and
           (string-match "compilation" (buffer-name buf))
           (string-match "finished" str)
           (not (with-current-buffer buf
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward +compilation-auto-bury-msg-level nil t)))))
      (run-at-time
       +compilation-auto-bury-delay nil
       (lambda (b)
         (with-selected-window (get-buffer-window b)
           (kill-buffer-and-window))
         (unless (current-message)
           (message "Compilation finished without warnings.")))
       buf)))

  (defun +toggle-bury-compilation-buffer-if-successful ()
    "Toggle auto-burying the successful compilation buffer."
    (interactive)
    (if (memq '+compilation--bury-if-successful-h compilation-finish-functions)
        (progn
          (message "Disabled burying compilation buffer.")
          (remove-hook 'compilation-finish-functions #'+compilation--bury-if-successful-h))
      (message "Enabled burying compilation buffer.")
      (add-hook 'compilation-finish-functions #'+compilation--bury-if-successful-h))))

(use-package vhdl-mode
  :straight (:type built-in)
  :config
  ;; Setup vhdl_ls from rust_hdl (AUR: rust_hdl-git)
  (+eglot-register 'vhdl-mode "vhdl_ls"))

(use-package verilog-mode
  :straight (:type built-in)
  :config
  ;; Setup Verilog/SystemVerilog LSP servers
  (+eglot-register 'verilog-mode "svls" "verible-verilog-ls" "svlangserver"))

(use-package nxml-mode
  :straight (:type built-in)
  :mode "\\.xmpi\\'"
  :config
  (+eglot-register '(nxml-mode xml-mode) "lemminx"))

(use-package elisp-mode
  :straight (:type built-in)
  :hook (emacs-lisp-mode . (lambda () (setq-local tab-width 8))) ;; to view built-in packages correctly
  :after minemacs-loaded ; prevent elisp-mode from being loaded too early
  :init
  (+map-local! :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map ielm-map lisp-mode-map racket-mode-map scheme-mode-map)
    "p" #'check-parens)
  :config
  (+map-local! :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "d"   '(nil :wk "edebug")
    "df"  'edebug-defun
    "dF"  'edebug-all-forms
    "dd"  'edebug-all-defs
    "dr"  'edebug-remove-instrumentation
    "do"  'edebug-on-entry
    "dO"  'edebug-cancel-on-entry
    "db"  '(nil :wk "breakpoints")
    "dbb" 'edebug-set-breakpoint
    "dbr" 'edebug-unset-breakpoint
    "dbn" 'edebug-next-breakpoint
    "e"   '(nil :wk "eval")
    "eb"  'eval-buffer
    "ed"  'eval-defun
    "ee"  'eval-last-sexp
    "er"  'eval-region
    "eR"  'elisp-eval-region-or-buffer
    "el"  'load-library
    "g"   '(nil :wk "goto/find")
    "gf"  'find-function-at-point
    "gR"  'find-function
    "gv"  'find-variable-at-point
    "gV"  'find-variable
    "gL"  'find-library
    "c"   '(nil :wk "compile")
    "cc"  #'elisp-byte-compile-buffer
    "cf"  #'elisp-byte-compile-file
    "cn"  #'emacs-lisp-native-compile-and-load
    "cb"  #'emacs-lisp-byte-compile-and-load)
  (+map-local! :keymaps '(edebug-mode-map)
    "e"   '(nil :wk "eval")
    "ee"  'edebug-eval-last-sexp
    "eE"  'edebug-eval-expression
    "et"  'edebug-eval-top-level-form))

(use-package scheme
  :straight (:type built-in)
  :custom
  (scheme-program-name "guile"))

(use-package gdb-mi
  :straight (:type built-in)
  :custom
  (gdb-show-main t) ; display source file containing main routine at startup
  (gdb-many-windows t) ; start in gdb-many-windows mode
  (gdb-debug-log-max 1024) ; default 128
  (gdb-restore-window-configuration-after-quit t)
  (gdb-thread-buffer-verbose-names nil)
  (gdb-window-configuration-directory (+directory-ensure minemacs-local-dir "gdb/"))
  (gdb-max-source-window-count 1) ; IDEA: maybe increase it!
  (gdb-display-io-nopopup nil) ; IDEA: maybe change it!
  :config
  ;; Add an overlay for the current line (mimics dap-mode)
  (defvar +gud-overlay
    (let* ((overlay (make-overlay (point-min) (point-min))))
      (overlay-put overlay 'face 'highlight)
      overlay)
    "Overlay variable for GUD highlighting.")

  (advice-add
   'gud-display-line :after
   (defun +gud--display-overlay-a (true-file _line)
     (let* ((overlay +gud-overlay)
            (buffer (gud-find-file true-file)))
       (with-current-buffer buffer
         (move-overlay overlay (line-beginning-position) (line-end-position) (current-buffer))))))

  (add-hook
   'kill-buffer-hook
   (defun +gud--delete-overlay-h ()
     (when (derived-mode-p 'gud-mode)
       (delete-overlay +gud-overlay)))))

(use-package org
  :straight (:type built-in)
  :after minemacs-loaded
  :preface
  ;; Set to nil so we can detect user changes (in config.el)
  (setq org-directory nil)
  :custom
  (org-clock-persist-file (concat minemacs-cache-dir "org/clock-persist.el"))
  (org-id-locations-file (concat minemacs-cache-dir "org/id-locations.el"))
  (org-persist-directory (+directory-ensure minemacs-cache-dir "org/persist/"))
  (org-preview-latex-image-directory (+directory-ensure minemacs-cache-dir "org/preview/latex-image/"))
  (org-publish-timestamp-directory (+directory-ensure minemacs-cache-dir "org/publish/timestamps/"))
  (org-tags-column 0)
  (org-auto-align-tags nil)
  (org-startup-indented nil)
  (org-cycle-hide-block-startup t)
  (org-return-follows-link t) ; RET follows link (a key bind has to be defined for Evil, see below)
  (org-fold-catch-invisible-edits 'smart) ; try not to accidently do weird stuff in invisible regions
  (org-fold-core-style 'overlays) ; Fix `evil' search problem (to be used with `evil-search')
  (org-fontify-quote-and-verse-blocks t)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-use-property-inheritance t) ; it's convenient to have properties inherited
  (org-ellipsis " ↩")
  (org-log-done 'time) ; having the time an item is done sounds convenient
  (org-list-allow-alphabetical t) ; have a. A. a) A) list bullets
  (org-export-in-background t) ; run export processes in external emacs process
  (org-export-async-init-file (expand-file-name (concat minemacs-modules-dir "extras/me-org-export-async-init.el")))
  (org-export-with-smart-quotes t) ; convert "this" to « this »
  (org-export-with-sub-superscripts '{}) ; Only explicit _{} ^{} are interpreted as sub/superscripts
  (org-export-with-broken-links 'mark) ; Do not rise error on broken links, but mark them in the output file
  (org-highlight-latex-and-related '(native script entities))
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-use-sub-superscripts '{}) ; Do the same when rendering the Org buffer
  (org-edit-src-content-indentation 0) ; do not indent the content of src blocks
  (org-edit-src-turn-on-auto-save t) ; auto-save org-edit-src
  (org-edit-src-auto-save-idle-delay auto-save-timeout) ; use the defaults
  :config
  (+map-local! :keymaps 'org-mode-map
    "l"  '(nil :wk "link")
    "ll" #'org-insert-link
    "e"  #'org-export-dispatch
    "c"  #'org-edit-src-code
    "s"  '(nil :wk "babel-session")
    "sc" #'org-babel-switch-to-session-with-code
    "ss" #'org-babel-switch-to-session
    "sp" #'org-babel-pop-to-session
    "sP" #'org-babel-pop-to-session-maybe
    "sl" #'org-babel-load-in-session
    "sL" #'org-babel-load-in-session-maybe
    "si" #'org-babel-initiate-session
    "b"  '(nil :wk "babel")
    "bt" #'org-babel-tangle
    "bd" #'org-babel-detangle
    "bf" #'org-babel-tangle-file)
  (+map-local! :keymaps 'org-src-mode-map
    "s" #'org-edit-src-save
    "q" #'org-edit-src-abort
    "e" #'org-edit-src-exit)
  (+nmap! :keymaps 'org-mode-map
    "RET" #'org-open-at-point)

  (setq org-export-async-debug minemacs-debug) ;; Can be useful!

  ;; Dynamically change font size for Org heading levels, starting from
  ;; `+org-level-base-size', and shrinking by a factor of 0.9 at each level.
  (defvar +org-level-base-size 1.3)

  (dotimes (level 8)
    (let ((size (max 1.0 (* +org-level-base-size (expt 0.9 level)))))
      (set-face-attribute
       (intern (format "org-level-%d" (1+ level))) nil
       :weight 'bold
       :height size)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   (cl-loop
    for lang in '(C R js dot awk sed sql org shell ditaa latex julia sqlite octave
                  maxima eshell scheme python fortran gnuplot plantuml makefile)
    collect (cons lang t)))

  (with-eval-after-load 'org-src
    (setq org-src-lang-modes
          (append
           '(("dot" . graphviz-dot))
           (delete (assoc "dot" org-src-lang-modes #'equal) org-src-lang-modes))))

  (with-eval-after-load 'plantuml-mode
    (setq org-plantuml-jar-path plantuml-jar-path
          org-plantuml-exec-mode plantuml-default-exec-mode
          org-plantuml-executable-path plantuml-executable-path))

  ;; From Doom Emacs
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel))))

;; (use-package org
;;   :straight (:type built-in)
;;   :demand t
;;   :after minemacs-loaded
;;   :preface
;;   ;; Set to nil so we can detect user changes (in config.el)
;;   (setq org-directory nil)
;;   :custom
;;   (org-tags-column 0)
;;   (org-auto-align-tags nil)
;;   (org-startup-indented nil)
;;   (org-cycle-hide-block-startup t)
;;   (org-return-follows-link t) ; RET follows link (a key bind has to be defined for Evil, see below)
;;   (org-fold-catch-invisible-edits 'smart) ; try not to accidently do weird stuff in invisible regions
;;   (org-fold-core-style 'overlays) ; Fix `evil' search problem (to be used with `evil-search')
;;   (org-fontify-quote-and-verse-blocks t)
;;   (org-special-ctrl-a/e t)
;;   (org-insert-heading-respect-content t)
;;   (org-hide-emphasis-markers t)
;;   (org-use-property-inheritance t) ; it's convenient to have properties inherited
;;   (org-ellipsis " ↩")
;;   (org-log-done 'time) ; having the time an item is done sounds convenient
;;   (org-list-allow-alphabetical t) ; have a. A. a) A) list bullets
;;   (org-export-in-background t) ; run export processes in external emacs process
;;   (org-export-async-init-file (expand-file-name (concat minemacs-modules-dir "extras/me-org-export-async-init.el")))
;;   (org-export-with-smart-quotes t) ; convert "this" to « this »
;;   (org-export-with-sub-superscripts '{}) ; Only explicit _{} ^{} are interpreted as sub/superscripts
;;   (org-export-with-broken-links 'mark) ; Do not rise error on broken links, but mark them in the output file
;;   (org-highlight-latex-and-related '(native script entities))
;;   (org-pretty-entities t)
;;   (org-pretty-entities-include-sub-superscripts t)
;;   (org-use-sub-superscripts '{}) ; Do the same when rendering the Org buffer
;;   (org-edit-src-content-indentation 0) ; do not indent the content of src blocks
;;   (org-edit-src-turn-on-auto-save t) ; auto-save org-edit-src
;;   (org-edit-src-auto-save-idle-delay auto-save-timeout) ; use the defaults
;;   :config
;;   (+map-local! :keymaps 'org-mode-map
;;     "l"  '(nil :wk "link")
;;     "ll" #'org-insert-link
;;     "e"  #'org-export-dispatch
;;     "c"  #'org-edit-src-code
;;     "s"  '(nil :wk "babel-session")
;;     "sc" #'org-babel-switch-to-session-with-code
;;     "ss" #'org-babel-switch-to-session
;;     "sp" #'org-babel-pop-to-session
;;     "sP" #'org-babel-pop-to-session-maybe
;;     "sl" #'org-babel-load-in-session
;;     "sL" #'org-babel-load-in-session-maybe
;;     "si" #'org-babel-initiate-session
;;     "b"  '(nil :wk "babel")
;;     "bt" #'org-babel-tangle
;;     "bd" #'org-babel-detangle
;;     "bf" #'org-babel-tangle-file)
;;   (+map-local! :keymaps 'org-src-mode-map
;;     "s" #'org-edit-src-save
;;     "q" #'org-edit-src-abort
;;     "e" #'org-edit-src-exit)
;;   (+nmap! :keymaps 'org-mode-map
;;     "RET" #'org-open-at-point)
;;
;;   (setq org-export-async-debug minemacs-debug) ;; Can be useful!
;;
;;   ;; Dynamically change font size for Org heading levels, starting from
;;   ;; `+org-level-base-size', and shrinking by a factor of 0.9 at each level.
;;   (defvar +org-level-base-size 1.3)
;;
;;   (dotimes (level 8)
;;     (let ((size (max 1.0 (* +org-level-base-size (expt 0.9 level)))))
;;       (set-face-attribute
;;        (intern (format "org-level-%d" (1+ level))) nil
;;        :weight 'bold
;;        :height size)
;;
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    (cl-loop
;;     for lang in '(C R js dot awk sed sql org shell ditaa latex julia sqlite octave
;;                   maxima eshell scheme python fortran gnuplot plantuml makefile))
;;     collect (cons lang t))
;;
;;   (with-eval-after-load 'org-src
;;     (setq org-src-lang-modes
;;           (append
;;            '(("dot" . graphviz-dot))
;;            (delete (assoc "dot" org-src-lang-modes #'equal) org-src-lang-modes))))
;;
;;   (with-eval-after-load 'plantuml-mode
;;     (setq org-plantuml-jar-path plantuml-jar-path
;;           org-plantuml-exec-mode plantuml-default-exec-mode
;;           org-plantuml-executable-path plantuml-executable-path))
;;
;;   ;; From Doom Emacs
;;   (with-no-warnings
;;     (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
;;     (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
;;     (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
;;     (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))
;;
;;   (setq org-todo-keywords
;;         '((sequence
;;            "TODO(t)"  ; A task that needs doing & is ready to do
;;            "PROJ(p)"  ; A project, which usually contains other tasks
;;            "LOOP(r)"  ; A recurring task
;;            "STRT(s)"  ; A task that is in progress
;;            "WAIT(w)"  ; Something external is holding up this task
;;            "HOLD(h)"  ; This task is paused/on hold because of me
;;            "IDEA(i)"  ; An unconfirmed and unapproved task or notion
;;            "|"
;;            "DONE(d)"  ; Task successfully completed
;;            "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
;;           (sequence
;;            "[ ](T)"   ; A task that needs doing
;;            "[-](S)"   ; Task is in progress
;;            "[?](W)"   ; Task is being held up or paused
;;            "|"
;;            "[X](D)")  ; Task was completed
;;           (sequence
;;            "|"
;;            "OKAY(o)"
;;            "YES(y)"
;;            "NO(n)"))
;;         org-todo-keyword-faces
;;         '(("[-]"  . +org-todo-active)
;;           ("STRT" . +org-todo-active)
;;           ("[?]"  . +org-todo-onhold)
;;           ("WAIT" . +org-todo-onhold)
;;           ("HOLD" . +org-todo-onhold)
;;           ("PROJ" . +org-todo-project)
;;           ("NO"   . +org-todo-cancel)
;;           ("KILL" . +org-todo-cancel))))

(use-package org-agenda
  :straight (:type built-in)
  :demand t
  :custom
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────"))

(use-package citeproc
  :straight t
  :ensure t
  :defer t)

(use-package org-caldav
  :straight t
  :after (org org-agenda)
  :ensure t
  :config
  (setq org-caldav-url "https://dav.dechnik.net/lukasz%40dechnik.net"
        org-caldav-calendars
        '((:calendar-id "Work"
           :files ("~/Org/calendar-work.org")
           :inbox "~/Org/calendar-work.org")
          (:calendar-id "Personal"
           :files ("~/Org/calendar-personal.org")
           :inbox "~/Org/calendar-personal.org"))
        org-caldav-sync-changes-to-org 'all
        org-caldav-save-directory "~/Org"
        org-icalendar-alarm-time 20
        org-icalendar-use-deadline '(event-if-not-todo todo-due)
        org-icalendar-use-scheduled '(event-if-not-todo)
        org-icalendar-include-todo 'all
        org-caldav-sync-todo t
        org-icalendar-categories '(local-tags)
        org-icalendar-timezone "Europe/Warsaw"))

(use-package org-super-agenda
  :straight t
  :commands org-super-agenda-mode
  :after org-agenda
  :init (let ((inhibit-message t))
          (org-super-agenda-mode))
  :config
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t)
  (setq org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :todo "TODAY"
                                  :scheduled today
                                  :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Next to do"
                                   :todo "NEXT"
                                   :order 1)
                            (:name "Important"
                                   :tag "Important"
                                   :priority "A"
                                   :order 6)
                            (:name "Due Today"
                                   :deadline today
                                   :order 2)
                            (:name "Due Soon"
                                   :deadline future
                                   :order 8)
                            (:name "Overdue"
                                   :deadline past
                                   :face error
                                   :order 7)
                            (:name "Assignments"
                                   :tag "Assignment"
                                   :order 10)
                            (:name "Issues"
                                   :tag "Issue"
                                   :order 12)
                            (:name "Emacs"
                                   :tag "Emacs"
                                   :order 13)
                            (:name "Projects"
                                   :tag "Project"
                                   :order 14)
                            (:name "Research"
                                   :tag "Research"
                                   :order 15)
                            (:name "To read"
                                   :tag "Read"
                                   :order 30)
                            (:name "Waiting"
                                   :todo "WAITING"
                                   :order 20)
                            (:name "Work"
                                   :tag "work"
                                   :order 32)
                            (:name "Trivial"
                                   :priority<= "E"
                                   :tag ("Trivial" "Unimportant")
                                   :todo ("SOMEDAY")
                                   :order 90)
                            (:discard (:tag ("Chore" "Routine" "Daily"))))))))))))

(use-package doct
  :straight t
  :commands doct)

(use-package org-capture
  :straight (:type built-in)
  :after (org
          org-agenda)
  :config
  (require 'all-the-icons)
  (defun +org--capture-local-root (path)
    (let ((filename (file-name-nondirectory path)))
      (expand-file-name
       filename
       (or (locate-dominating-file (file-truename default-directory)
                                   filename)
           (doom-modeline--project-root)
           (user-error "Couldn't detect a project")))))
  (defun +org-capture-project-todo-file ()
    "Find the nearest `+org-capture-todo-file' in a parent directory, otherwise,
      opens a blank one at the project root. Throws an error if not in a project."
    (+org--capture-local-root +org-capture-todo-file))

  (defun +org-capture-project-notes-file ()
    "Find the nearest `+org-capture-notes-file' in a parent directory, otherwise,
      opens a blank one at the project root. Throws an error if not in a project."
    (+org--capture-local-root +org-capture-notes-file))

  (defun +org-capture-project-changelog-file ()
    "Find the nearest `+org-capture-changelog-file' in a parent directory,
      otherwise, opens a blank one at the project root. Throws an error if not in a
      project."
    (+org--capture-local-root +org-capture-changelog-file))
  (defun org-capture-select-template-prettier (&optional keys)
    "Select a capture template, in a prettier way than default
                    Lisp programs can force the template by setting KEYS to a string."
    (let ((org-capture-templates
           (or (org-contextualize-keys
                (org-capture-upgrade-templates org-capture-templates)
                org-capture-templates-contexts)
               '(("t" "Task" entry (file+headline "" "Tasks")
                  "* TODO %?\n  %u\n  %a")))))
      (if keys
          (or (assoc keys org-capture-templates)
              (error "No capture template referred to by \"%s\" keys" keys))
        (org-mks org-capture-templates
                 "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                 "Template key: "
                 `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
  (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

  (defun org-mks-pretty (table title &optional prompt specials)
    "Select a member of an alist with multiple keys. Prettified.

                    TABLE is the alist which should contain entries where the car is a string.
                    There should be two types of entries.

                    1. prefix descriptions like (\"a\" \"Description\")
                       This indicates that `a' is a prefix key for multi-letter selection, and
                       that there are entries following with keys like \"ab\", \"ax\"…

                    2. Select-able members must have more than two elements, with the first
                       being the string of keys that lead to selecting it, and the second a
                       short description string of the item.

                    The command will then make a temporary buffer listing all entries
                    that can be selected with a single key, and all the single key
                    prefixes.  When you press the key for a single-letter entry, it is selected.
                    When you press a prefix key, the commands (and maybe further prefixes)
                    under this key will be shown and offered for selection.

                    TITLE will be placed over the selection in the temporary buffer,
                    PROMPT will be used when prompting for a key.  SPECIALS is an
                    alist with (\"key\" \"description\") entries.  When one of these
                    is selected, only the bare key is returned."
    (save-window-excursion
      (let ((inhibit-quit t)
            (buffer (org-switch-to-buffer-other-window "*Org Select*"))
            (prompt (or prompt "Select: "))
            case-fold-search
            current)
        (unwind-protect
            (catch 'exit
              (while t
                (setq-local evil-normal-state-cursor (list nil))
                (erase-buffer)
                (insert title "\n\n")
                (let ((des-keys nil)
                      (allowed-keys '("\C-g"))
                      (tab-alternatives '("\s" "\t" "\r"))
                      (cursor-type nil))
                  ;; Populate allowed keys and descriptions keys
                  ;; available with CURRENT selector.
                  (let ((re (format "\\`%s\\(.\\)\\'"
                                    (if current (regexp-quote current) "")))
                        (prefix (if current (concat current " ") "")))
                    (dolist (entry table)
                      (pcase entry
                        ;; Description.
                        (`(,(and key (pred (string-match re))) ,desc)
                         (let ((k (match-string 1 key)))
                           (push k des-keys)
                           ;; Keys ending in tab, space or RET are equivalent.
                           (if (member k tab-alternatives)
                               (push "\t" allowed-keys)
                             (push k allowed-keys))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                        ;; Usable entry.
                        (`(,(and key (pred (string-match re))) ,desc . ,_)
                         (let ((k (match-string 1 key)))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                           (push k allowed-keys)))
                        (_ nil))))
                  ;; Insert special entries, if any.
                  (when specials
                    (insert "─────────────────────────\n")
                    (pcase-dolist (`(,key ,description) specials)
                      (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                      (push key allowed-keys)))
                  ;; Display UI and let user select an entry or
                  ;; a sub-level prefix.
                  (goto-char (point-min))
                  (unless (pos-visible-in-window-p (point-max))
                    (org-fit-window-to-buffer))
                  (let ((pressed (org--mks-read-key allowed-keys
                                                    prompt
                                                    (not (pos-visible-in-window-p (1- (point-max)))))))
                    (setq current (concat current pressed))
                    (cond
                     ((equal pressed "\C-g") (user-error "Abort"))
                     ;; Selection is a prefix: open a new menu.
                     ((member pressed des-keys))
                     ;; Selection matches an association: return it.
                     ((let ((entry (assoc current table)))
                        (and entry (throw 'exit entry))))
                     ;; Selection matches a special entry: return the
                     ;; selection prefix.
                     ((assoc current specials) (throw 'exit current))
                     (t (error "No entry available")))))))
          (when buffer (kill-buffer buffer))))))
  (advice-add 'org-mks :override #'org-mks-pretty)
  (defun +doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
    (let ((name (pop declaration))
          (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
          (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
          (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
      (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (defun +doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
      (setq doct-templates (mapcar (lambda (template)
                                     (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                 (spec (plist-get (plist-get props :doct) :icon)))
                                       (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                      "\t"
                                                                      (nth 1 template))))
                                     template)
                                   templates))))

  (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

  (defvar +org-capture-recipies  "~/Org/recipies.org")
  (defvar +org-capture-todo-file  "~/Org/todo.org")
  (defvar +org-capture-notes-file  "~/Org/inbox.org")
  (defvar +org-capture-changelog-file  "~/Org/changelog.org")

  (defun set-org-capture-templates ()
    (setq org-capture-templates
          (doct `(("Personal todo" :keys "t"
                   :icon ("checklist" :set "octicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a"))
                  ("Personal note" :keys "n"
                   :icon ("sticky-note-o" :set "faicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* %?"
                              "%i %a"))
                  ("Email" :keys "e"
                   :icon ("envelope" :set "faicon" :color "blue")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                              "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                              "about %^{topic}"
                              "%U %i %a"))
                  ("Interesting" :keys "i"
                   :icon ("eye" :set "faicon" :color "lcyan")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Interesting"
                   :type entry
                   :template ("* [ ] %{desc}%? :%{i-type}:"
                              "%i %a")
                   :children (("Webpage" :keys "w"
                               :icon ("globe" :set "faicon" :color "green")
                               :desc "%(org-cliplink-capture) "
                               :i-type "read:web")
                              ("Article" :keys "a"
                               :icon ("file-text" :set "octicon" :color "yellow")
                               :desc ""
                               :i-type "read:reaserch")
                              ("\tRecipie" :keys "r"
                               :icon ("spoon" :set "faicon" :color "dorange")
                               :file +org-capture-recipies
                               :headline "Unsorted"
                               :template "%(org-chef-get-recipe-from-url)")
                              ("Information" :keys "i"
                               :icon ("info-circle" :set "faicon" :color "blue")
                               :desc ""
                               :i-type "read:info")
                              ("Idea" :keys "I"
                               :icon ("bubble_chart" :set "material" :color "silver")
                               :desc ""
                               :i-type "idea")))
                  ("Project" :keys "p"
                   :icon ("repo" :set "octicon" :color "silver")
                   :prepend t
                   :type entry
                   :headline "Inbox"
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :file ""
                   :custom (:time-or-todo "")
                   :children (("Project-local todo" :keys "t"
                               :icon ("checklist" :set "octicon" :color "green")
                               :time-or-todo "TODO"
                               :file +org-capture-project-todo-file)
                              ("Project-local note" :keys "n"
                               :icon ("sticky-note" :set "faicon" :color "yellow")
                               :time-or-todo "%U"
                               :file +org-capture-project-notes-file)
                              ("Project-local changelog" :keys "c"
                               :icon ("list" :set "faicon" :color "blue")
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-project-changelog-file)))
                  ("Tasks" :keys "k"
                   :icon ("inbox" :set "octicon" :color "yellow")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Tasks"
                   :type entry
                   :template ("* TODO %? %^G%{extra}"
                              "%i %a")
                   :children (("General Task" :keys "k"
                               :icon ("inbox" :set "octicon" :color "yellow")
                               :extra "")
                              ("Task with deadline" :keys "d"
                               :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                               :extra "\nDEADLINE: %^{Deadline:}t")
                              ("Scheduled Task" :keys "s"
                               :icon ("calendar" :set "octicon" :color "orange")
                               :extra "\nSCHEDULED: %^{Start time:}t")))))))

  (set-org-capture-templates)
  (add-hook 'server-after-make-frame-hook
            (defun org-capture-reinitialise-hook ()
                (set-org-capture-templates)
                (remove-hook 'server-after-make-frame-hook
                             #'org-capture-reinitialise-hook))))

(use-package ox-latex
  :straight (:type built-in)
  :after ox
  :custom
  (org-latex-src-block-backend 'engraved)
  (org-latex-prefer-user-labels t)
  (org-latex-tables-booktabs t)
  ;; Default `minted` options, can be overwritten in file/dir locals
  (org-latex-minted-options
   '(("frame"         "lines")
     ("fontsize"      "\\footnotesize")
     ("tabsize"       "2")
     ("breaklines"    "true")
     ("breakanywhere" "true") ;; break anywhere, no just on spaces
     ("style"         "default")
     ("bgcolor"       "GhostWhite")
     ("linenos"       "true")))
  :config
  ;; (setq org-latex-line-break-safe "\\\\")
  ;; Add this to your config to be able to export with minted:
  ;; (with-eval-after-load 'ox-latex
  ;;   (add-to-list 'org-latex-packages-alist '("" "minted"))
  ;;   (add-to-list 'org-latex-packages-alist '("svgnames" "xcolor"))
  ;;   (setq org-latex-src-block-backend 'minted
  ;;         org-latex-pdf-process '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f")))

  ;; Map some org-mode blocks' languages to lexers supported by minted
  ;; you can see supported lexers by running this command in a terminal:
  ;; 'pygmentize -L lexers'
  (dolist (pair '((ipython    "python")
                  (jupyter    "python")
                  (scheme     "scheme")
                  (lisp-data  "lisp")
                  (conf-unix  "unixconfig")
                  (conf-space "unixconfig")
                  (authinfo   "unixconfig")
                  (gdb-script "unixconfig")
                  (conf-toml  "yaml")
                  (conf       "ini")
                  (gitconfig  "ini")
                  (systemd    "ini")))
    (unless (member pair org-latex-minted-langs)
      (add-to-list 'org-latex-minted-langs pair)))

  (cond
   ((executable-find "latexmk")
    (setq
     org-latex-pdf-process
     '("latexmk -c -bibtex-cond1 %f" ; ensure cleaning ".bbl" files
       "latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f")))
   ;; NOTE: Tectonic might have some issues with some documents (sagej + natbib)
   ((executable-find "tectonic")
    (setq
     org-latex-pdf-process
     '("tectonic -X compile --outdir=%o -Z shell-escape -Z continue-on-errors %f")))))

(use-package ox-koma-letter
  :after ox
  :demand t)

(use-package ox-odt
  :after ox
  :demand t)

(use-package ox-beamer
  :after ox
  :demand t)

(use-package org-agenda
  :straight (:type built-in)
  :custom
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────"))

(use-package ediff
  :straight (:type built-in)
  :hook (ediff-before-setup . +ediff--save-window-config-h)
  :custom
  ;; Split horizontally
  (ediff-split-window-function #'split-window-horizontally)
  ;; Setup all windows in one frame
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  :config
  (defvar +ediff--saved-window-config nil)

  ;; Save the current window configuration (hooked to `ediff-before-setup-hook')
  (defun +ediff--save-window-config-h ()
    (setq +ediff--saved-window-config (current-window-configuration)))

  ;; Restore the saved window configuration on quit or suspend
  (+add-hook! (ediff-quit ediff-suspend) :depth 101
    (defun +ediff--restore-window-config-h ()
      (when (window-configuration-p +ediff--saved-window-config)
        (set-window-configuration +ediff--saved-window-config)))))

(use-package smerge-mode
  :straight (:type built-in)
  :commands +smerge-hydra/body
  :init
  (+map! "gm" '(+smerge-hydra/body :wk "sMerge"))
  :config
  (with-eval-after-load 'hydra
    (defhydra +smerge-hydra (:hint nil
                                   :pre (if (not smerge-mode) (smerge-mode 1))
                                   ;; Disable `smerge-mode' when quitting hydra if
                                   ;; no merge conflicts remain.
                                   :post (smerge-auto-leave))
      "
                                                         [smerge]
  Movement   Keep           Diff              Other         │
  ╭─────────────────────────────────────────────────────────╯
  │  ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
  │  ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
  │  ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
  │  ^_j_ ↓^     [_a_] all        [_H_] hightlight    [_n_] next in project
  │  ^_C-j_^     [_RET_] current  [_E_] ediff
  │  ^_G_^                                                 [_q_] quit
  ╰─────────────────────────────────────────────────────╯
"
      ("g" (progn (goto-char (point-min)) (smerge-next)))
      ("G" (progn (goto-char (point-max)) (smerge-prev)))
      ("C-j" smerge-next)
      ("C-k" smerge-prev)
      ("j" next-line)
      ("k" previous-line)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("H" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("R" smerge-kill-current)
      ;; Often after calling `smerge-vc-next-conflict', the cursor will land at
      ;; the bottom of the window
      ("n" (progn (smerge-vc-next-conflict) (recenter-top-bottom (/ (window-height) 8))))
      ("q" nil :color blue))))

(use-package octave
  :straight (:type built-in)
  :mode ("\\.m\\'" . octave-mode)
  :config
  (defun +octave-eval-last-sexp ()
    "Evaluate Octave sexp before point and print value into current buffer."
    (interactive)
    (inferior-octave t)
    (let ((print-escape-newlines nil)
          (opoint (point)))
      (prin1
       (save-excursion
         (forward-sexp -1)
         (inferior-octave-send-list-and-digest
          (list (concat (buffer-substring-no-properties (point) opoint) "\n")))
         (mapconcat 'identity inferior-octave-output-list "\n"))))))

(use-package abbrev
  :straight (:type built-in)
  :custom
  (abbrev-file-name (concat minemacs-local-dir "abbrev.el")))

(use-package bookmark
  :straight (:type built-in)
  :custom
  (bookmark-default-file (concat minemacs-local-dir "bookmark.el"))
  ;; Save the bookmarks every time a bookmark is made
  (bookmark-save-flag 1))

(use-package calc
  :straight (:type built-in)
  :init
  (+map! "o=" #'calc)
  :custom
  (calc-settings-file (concat minemacs-local-dir "calc-settings.el")))

(use-package desktop
  :straight (:type built-in)
  :custom
  ;; File name to use when saving desktop
  (desktop-base-file-name "emacs-session.el")
  ;; File name to use as a lock
  (desktop-base-lock-name (concat desktop-base-file-name ".lock"))
  ;; Load only 5 buffers immediately, the remaining buffers will be loaded lazily
  (desktop-restore-eager 5)
  ;; Avoid writing contents unchanged between auto-saves
  (desktop-file-checksum t)
  ;; Save buffer status
  (desktop-save-buffer t))

(use-package recentf
  :straight (:type built-in)
  :after minemacs-loaded
  :demand t
  :custom
  (recentf-save-file (concat minemacs-local-dir "recentf-save.el"))
  ;; Increase the maximum number of saved items
  (recentf-max-saved-items 100)
  ;; Ignore case when searching recentf files
  (recentf-case-fold-search t)
  ;; Exclude some files from being remembered by recentf
  (recentf-exclude
   `(,(rx (* any)
       (or
        "elfeed-db"
        "eln-cache"
        "/cache/"
        ".maildir/"
        ".cache/")
       (* any)
       (? (or "html" "pdf" "tex" "epub")))
     ,(rx "/"
       (or "rsync" "ssh" "tmp" "yadm" "sudoedit" "sudo")
       (* any))))
  :config
  ;; Enable `recentf-mode' to remember recent files
  (+shutup! (recentf-mode 1)))

(use-package url
  :straight (:type built-in)
  :custom
  (url-cache-directory (+directory-ensure minemacs-cache-dir "url/"))
  (url-configuration-directory (+directory-ensure minemacs-local-dir "url/"))
  (url-cookie-file (concat minemacs-local-dir "url/cookie.el"))
  (url-history-file (concat minemacs-local-dir "url/history.el")))

(use-package webjump
  :straight (:type built-in)
  :custom
  (webjump-sites
   '(("Emacs Wiki"    . [simple-query "www.emacswiki.org" "www.emacswiki.org/cgi-bin/wiki/" ""])
     ("DuckDuckGo"    . [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
     ("Qwant"         . [simple-query "www.qwant.com" "www.qwant.com/?q=" ""])
     ("Ecosia"        . [simple-query "www.ecosia.org" "www.ecosia.org/search?q=" ""])
     ("Brave"         . [simple-query "search.brave.com" "search.brave.com/search?q=" ""])
     ("Bing"          . [simple-query "www.bing.com" "www.bing.com/search?q=" ""])
     ("Yahoo"         . [simple-query "www.yahoo.com" "search.yahoo.com/search?p=" ""])
     ("Google"        . [simple-query "www.google.com" "www.google.com/search?q=" ""])
     ("Google Maps"   . [simple-query "www.google.com" "www.google.com/maps?q=" ""])
     ("Google Images" . [simple-query "www.google.com" "www.google.com/images?q=" ""])
     ("Google Groups" . [simple-query "groups.google.com" "groups.google.com/groups?q=" ""])
     ("StackOverflow" . [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" ""])
     ("GitHub Repo"   . [simple-query "github.com" "github.com/search?type=repositories&q=" ""])
     ("GitHub Code"   . [simple-query "github.com" "github.com/search?type=code&q=" ""])
     ("WolframAlpha"  . [simple-query "wolframalpha.com" "wolframalpha.com/input/?i=" ""])
     ("MDN"           . [simple-query "developer.mozilla.org" "developer.mozilla.org/search?q=" ""])
     ("Youtube"       . [simple-query "www.youtube.com" "www.youtube.com/results?search_query=" ""])
     ("Reddit"        . [simple-query "www.reddit.com" "www.reddit.com/search/?q=" ""])
     ("Wikipedia"     . [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""]))))

(use-package time-stamp
  :straight (:type built-in)
  ;; Update time stamp (if available) before saving a file.
  :hook (before-save . time-stamp)
  :custom
  ;; Do enable time-stamps
  (time-stamp-active t)
  ;; Check the first 12 buffer lines for Time-stamp: <>
  (time-stamp-line-limit 12)
  ;; Timestamp format
  (time-stamp-format "%04Y-%02m-%02d %02H:%02M:%02S"))

(use-package whitespace
  :straight (:type built-in)
  :hook (before-save . +save--whitespace-cleanup-h)
  :custom
  ;; Default behavior for `whitespace-cleanup'
  (whitespace-action '(cleanup auto-cleanup))
  :init
  (defcustom +whitespace-auto-cleanup-modes
    '(prog-mode conf-mode org-mode markdown-mode
      latex-mode tex-mode bibtex-mode)
    "Enable auto white space cleanup before saving for these derived modes."
    :group 'minemacs-edit
    :type '(repeat symbol))
  :config
  ;; Auto-remove trailing white spaces before saving for modes defined in
  ;; `+whitespace-auto-cleanup-modes'.
  (defun +save--whitespace-cleanup-h ()
    (when (cl-some #'derived-mode-p +whitespace-auto-cleanup-modes)
      (whitespace-cleanup))))

(use-package autorevert
  :straight (:type built-in)
  ;; Auto load files changed on disk
  :hook (minemacs-after-startup . global-auto-revert-mode)
  :custom
  ;; Revert non-file buffers like dired
  (global-auto-revert-non-file-buffers t))

(use-package savehist
  :straight (:type built-in)
  :hook (minemacs-after-startup . savehist-mode)
  :custom
  (savehist-file (concat minemacs-local-dir "savehist.el")))

(use-package saveplace
  :straight (:type built-in)
  ;; Save place in files
  :hook (minemacs-after-startup . save-place-mode)
  :custom
  (save-place-file (concat minemacs-local-dir "save-place.el")))

(use-package x-win
  :straight (:type built-in)
  :config
  ;; Advice `emacs-session-filename' to ensure creating "session.ID" files in
  ;; a sub-directory
  (advice-add
   #'emacs-session-filename :filter-return
   (defun +emacs-session-filename--in-subdir-a (session-filename)
     "Put the SESSION-FILENAME in the \"x-win/\" sub-directory."
     (concat (+directory-ensure minemacs-local-dir "x-win/")
             (file-name-nondirectory session-filename)))))

(use-package term
  :straight (:type built-in)
  :config
  ;; Kill `term' buffer on exit (reproduce a similar behavior to `shell's
  ;; `shell-kill-buffer-on-exit').
  (advice-add
   'term-sentinel :around
   (defun +term--kill-after-exit-a (orig-fn proc msg)
     (if (memq (process-status proc) '(signal exit))
         (let ((buffer (process-buffer proc)))
           (apply orig-fn (list proc msg))
           (kill-buffer buffer))
       (apply orig-fn (list proc msg))))))

(use-package executable
  :straight (:type built-in)
  ;; Make scripts (files starting wiht shebang "#!") executable when saved
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package display-line-numbers
  :straight (:type built-in)
  ;; Show line numbers
  :hook ((prog-mode conf-mode text-mode) . display-line-numbers-mode)
  :custom
  ;; Relative line numbering
  (display-line-numbers-type 'relative)
  ;; Width for line numbers
  (display-line-numbers-width 4)
  ;; Display absolute line numbers in narrowed regions
  (display-line-numbers-widen t))

(use-package pixel-scroll
  :straight (:type built-in)
  :after minemacs-loaded
  :demand t
  :custom
  ;; Better scrolling on Emacs29+, specially on a touchpad
  (pixel-scroll-precision-use-momentum t)
  :config
  ;; Scroll pixel by pixel, in Emacs29+ there is a more pricise mode way to scroll
  (if (>= emacs-major-version 29)
      (pixel-scroll-precision-mode 1)
    (pixel-scroll-mode 1)))

(use-package mouse
  :straight (:type built-in)
  :custom
  ;; Enable Drag-and-Drop of regions
  (mouse-drag-and-drop-region t)
  ;; Enable Drag-and-Drop of regions from Emacs to external programs
  (mouse-drag-and-drop-region-cross-program t))

(use-package wmouse
  :straight (:type built-in)
  :custom
  ;; Make mouse scroll a little faster
  (mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((meta) . nil) ((control meta) . global-text-scale) ((control) . text-scale)))
  ;; Make mouse scroll a little faster horizontally
  (mouse-wheel-scroll-amount-horizontal 2))

(use-package gnus
  :straight (:type built-in)
  :custom
  (gnus-dribble-directory (+directory-ensure minemacs-local-dir "gnus/dribble/"))
  (gnus-init-file (concat minemacs-config-dir "gnus/init.el"))
  (gnus-startup-file (concat minemacs-config-dir "gnus/newsrc")))

(use-package image-dired
  :straight (:type built-in)
  :custom
  (image-dired-dir (+directory-ensure minemacs-local-dir "image-dired/"))
  (image-dired-tags-db-file (concat minemacs-local-dir "image-dired/tags-db.el"))
  (image-dired-temp-rotate-image-file (concat minemacs-cache-dir "image-dired/temp-rotate-image")))

(use-package time
  :straight (:type built-in)
  ;; Display time in mode-line
  :hook (minemacs-after-startup . display-time-mode)
  :custom
  ;; Enable time in the mode-line
  (display-time-string-forms '((propertize (concat 24-hours ":" minutes)))))

(use-package frame
  :straight (:type built-in)
  ;; Display divider between windows
  :hook (minemacs-after-startup . window-divider-mode)
  :custom
  ;; Set line width for the divider in `window-divider-mode' to 2px
  (window-divider-default-bottom-width 2)
  (window-divider-default-right-width 2))


(provide 'me-builtin)

;;; me-builtin.el ends here
