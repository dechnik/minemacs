;;; config.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Abdelhak Bougouffa

;; Personal info
(setq user-full-name "Lukasz Dechnik"
      user-mail-address (concat "lukasz" "@" "dechnik" "." "net"))

;; Set the default GPG key ID, see "gpg --list-secret-keys"
;; (setq-default epa-file-encrypt-to '("XXXX"))

(use-package password-store
  :straight t
  :demand t)
(use-package password-store-otp
  :straight t
  :demand t)
(use-package pass
  :straight t
  :demand t
  :after password-store
  :config
  (auth-source-pass-enable))
(use-package auth-source-pass
  :straight (:type built-in)
  :demand t
  :init
  (auth-source-pass-enable)
  :after password-store)
(setq auth-sources '(password-store "~/.authinfo.gpg"))

(use-package robot-mode
  :straight t
  :mode (("\\.robot" . robot-mode)))

(setq
 ;; Set a theme for MinEmacs, supported themes include these from `doom-themes'
 ;; and `modus-themes'.
 minemacs-theme 'doom-gruvbox ; `doom-one' is a dark theme, `doom-one-light' is the light one
 ;; Set Emacs fonts, some good choices include:
 ;; - Cascadia Code
 ;; - Fira Code, FiraCode Nerd Font
 ;; - Iosevka, Iosevka Fixed Curly Slab
 ;; - IBM Plex Mono
 ;; - JetBrains Mono
 minemacs-fonts
 '(:font-family "JetBrainsMono Nerd Font"
   :font-size 10
   :variable-pitch-font-family "Overpass"
   :variable-pitch-font-size 10))

(+deferred!
 ;; Auto enable Eglot in supported modes using `+eglot-auto-enable' (from the
 ;; `me-prog' module). You can use `+lsp-auto-enable' instead to automatically
 ;; enable LSP mode in supported modes (from the `me-lsp' module).
 (+eglot-auto-enable))

;; If you installed Emacs from source, you can add the source code
;; directory to enable jumping to symbols defined in Emacs' C code.
;; (setq source-directory "~/Sources/emacs-git/")

;; I use Brave, and never use Chrome, so I replace chrome program with "brave"
(setq browse-url-chrome-program (or (executable-find "firefox") (executable-find "chromium")))

;; Module: `me-natural-langs' -- Package: `spell-fu'
(with-eval-after-load 'spell-fu
  ;; We can use MinEmacs' helper macro `+spell-fu-register-dictionaries'
  ;; to enable multi-language spell checking.
  (+spell-fu-register-dictionaries "en" "pl"))

;; Module: `me-rss' -- Package: `elfeed'
(with-eval-after-load 'elfeed
  ;; Add news feeds for `elfeed'
  (setq elfeed-feeds
        '("https://itsfoss.com/feed"
          "https://planet.emacslife.com/atom.xml")))

;; Module: `me-email' -- Package: `mu4e'
(with-eval-after-load 'mu4e
  ;; Load personal aliases, a file containing aliases, for example:
  ;; alias gmail "Firstname Lastname <some.user.name@gmail.com>"
  ;; alias work  "Firstname Lastname <some.user.name@work.com>"
  (add-to-list 'mu4e-bookmarks
               '(:name "Yesterday's messages" :query "date:2d..1d" :key ?y) t)

  (setq mm-sign-option 'guided)

  (defun sign-or-encrypt-message ()
    (let ((answer (read-from-minibuffer "Sign or encrypt?\nEmpty to do nothing.\n[s/e]: ")))
      (cond
       ((string-equal answer "s") (progn
                                    (message "Signing message.")
                                    (mml-secure-message-sign-pgpmime)))
       ((string-equal answer "e") (progn
                                    (message "Encrypt and signing message.")
                                    (mml-secure-message-encrypt-pgpmime)))
       (t (progn
            (message "Dont signing or encrypting message.")
            nil)))))

  (add-hook 'message-send-hook 'sign-or-encrypt-message)

  ;; (setq mail-personal-alias-file (concat minemacs-config-dir "private/mail-aliases.mailrc"))
  (when (file-exists-p "~/.local/share/mail/.emacs/contexts.el")
    (load-file "~/.local/share/mail/.emacs/contexts.el"))
  (setq mu4e-context-policy 'pick-first)
  (when (file-exists-p "~/.local/share/mail/.emacs/send.el")
    (load-file "~/.local/share/mail/.emacs/send.el")))


;; Module: `me-org' -- Package: `org'
(with-eval-after-load 'org
  ;; Set Org-mode directory
  (setq org-directory "~/Org/" ; let's put files here
        org-default-notes-file (concat org-directory "inbox.org"))
  ;; Customize Org stuff
  (setq org-todo-keywords
        '((sequence "IDEA(i)" "TODO(t)" "NEXT(n)" "PROJ(p)" "STRT(s)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

  (setq org-export-headline-levels 5))
