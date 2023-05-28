;;; me-loaddefs.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:



;;; Generated autoloads from ../elisp/+binary.el

(autoload '+binary-objdump-buffer-p "../elisp/+binary" "\
Can the BUFFER be viewed as a disassembled code with objdump.

(fn &optional BUFFER)")
(autoload '+binary-hexl-buffer-p "../elisp/+binary" "\
Is the current buffer should be viewed using `hexl-mode'.

(fn &optional BUFFER)")
(autoload 'objdump-disassemble-mode "../elisp/+binary" "\
Major mode for viewing executable files disassembled using objdump.

(fn)" t)
(autoload '+binary-hexl-mode-maybe "../elisp/+binary" "\
If `hexl-mode' is not already active, and the current buffer
is binary, activate `hexl-mode'." t)
(autoload '+binary-setup-modes "../elisp/+binary")
(register-definition-prefixes "../elisp/+binary" '("+binary-"))


;;; Generated autoloads from ../elisp/+buffer.el

(autoload '+kill-buffer-and-its-windows "../elisp/+buffer" "\
Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string).

(fn BUFFER &optional MSGP)" t)
(autoload '+region-to-buffer "../elisp/+buffer" "\
Copy region to BUFFER: At beginning (prefix >= 0), end (< 0), or replace.
START and END are the region boundaries.
BUFFER is a buffer or its name (a string).
With prefix ARG >= 0: `append-to-buffer':
  Append contents of region to end of BUFFER.
  (Point is moved to end of BUFFER first.)
With prefix ARG < 0:  `prepend-to-buffer':
  Prepend contents of region to beginning of BUFFER.
  (Point is moved to beginning of BUFFER first.)
With no prefix ARG (nil): `copy-to-buffer'.
  Write region to BUFFER, replacing any previous contents.

(fn START END BUFFER ARG)" t)
(autoload '+region-to-file "../elisp/+buffer" "\
With prefix arg, this is `append-to-file'.  Without, it is `write-region'.
START and END are the region boundaries.
Prefix ARG non-nil means append region to end of file FILENAME.
Prefix ARG nil means write region to FILENAME, replacing contents.

(fn START END FILENAME ARG)" t)
(autoload '+kill-some-buffers "../elisp/+buffer" "\
Kill some buffers.  Asks the user whether to kill the modified ones.
Non-interactively, if optional argument LIST is non-nil, it
specifies the list of buffers to kill, asking for approval for each one.
See `kill-some-buffers'.

(fn &optional LIST)" t)
(autoload '+kill-buffer-ask-if-modified "../elisp/+buffer" "\
Like `kill-buffer-ask', but kills BUFFER without confirmation if buffer is unmodified.
Kill without asking for buffer names in `+kill-buffer-no-ask-list'.

(fn BUFFER)")
(autoload '+delete-extra-windows-for-buffer "../elisp/+buffer" "\
Delete all other windows showing the selected window's buffer." t)
(autoload '+delete-window-maybe-kill-buffer "../elisp/+buffer" "\
Delete selected window.
If no other window shows its buffer, kill the buffer too." t)
(autoload '+fill-scratch-buffer "../elisp/+buffer" "\
Fill the `initial-scratch-message'.
When available, use \"fortune\" to add a random quote.")
(register-definition-prefixes "../elisp/+buffer" '("+kill-buffer-no-ask-list"))


;;; Generated autoloads from ../elisp/+eglot.el

(autoload '+eglot-register "../elisp/+eglot" "\
Register MODES with LSP SERVERS.
Examples:
  (+eglot-register 'vhdl-mode \"vhdl_ls\")
  (+eglot-register 'lua-mode \"lua-language-server\" \"lua-lsp\")
  (+eglot-register '(c-mode c++-mode) '(\"clangd\" \"--clang-tidy\" \"-j=12\") \"ccls\")

(fn MODES &rest SERVERS)")
(function-put '+eglot-register 'lisp-indent-function 0)


;;; Generated autoloads from ../elisp/+emacs.el

(autoload '+dir-locals-reload-for-this-buffer "../elisp/+emacs" "\
Reload directory-local for the current buffer" t)
(autoload '+dir-locals-reload-for-all-buffers-in-this-directory "../elisp/+emacs" "\
For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals." t)
(autoload '+dir-locals-toggle-autoreload "../elisp/+emacs" "\
Toggle autoloading directory-local variables after editing the \".dir-locals\" file.
If ENABLE is non-nil, force enabling autoreloading.

(fn &optional ENABLE)" t)
(autoload '+dir-locals-open-or-create "../elisp/+emacs" "\
Open or create the dir-locals.el for the current project." t)
(autoload '+toggle-auto-whitespace-cleanup "../elisp/+emacs" "\
Toggle auto-deleting trailing whitespaces." t)
(autoload '+what-faces "../elisp/+emacs" "\
Get the font faces at POS.

(fn POS)" t)
(autoload '+screenshot-svg "../elisp/+emacs" "\
Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring. If launched with a
prefix or universal argument, it waits for a moment (defined by
`+screenshot-delay') before taking the screenshot." t)
(autoload '+region-or-thing-at-point "../elisp/+emacs" "\
Return the region or the thing at point.")
(autoload '+webjump "../elisp/+emacs" "\
Like `webjump', with initial query filled from `+region-org-thing-at-point'." t)
(register-definition-prefixes "../elisp/+emacs" '("+dir-locals--autoreload-" "+screenshot-" "+webjump-read-string-"))


;;; Generated autoloads from ../elisp/+io.el

(autoload '+file-mime-type "../elisp/+io" "\
Get MIME type for FILE based on magic codes provided by the 'file' command.
Return a symbol of the MIME type, ex: `text/x-lisp', `text/plain',
`application/x-object', `application/octet-stream', etc.

(fn FILE)")
(autoload '+file-name-incremental "../elisp/+io" "\
Return an unique file name for FILENAME.
If \"file.ext\" exists, returns \"file-0.ext\".

(fn FILENAME)")
(autoload '+file-read-to-string "../elisp/+io" "\
Return a string with the contents of FILENAME.

(fn FILENAME)")
(autoload '+directory-subdirs "../elisp/+io" "\
Return a list of sub-directories in DIR.

(fn DIR)")
(autoload '+directory-ensure "../elisp/+io" "\
Concatenate PATH-PARTS to construct a path and return it.

Ensure the path exists, if not create it. The exact behavior is to create the
parent directory if the path is a file, and if the path is a directory, create
that directory.

(fn &rest PATH-PARTS)")
(autoload '+delete-this-file "../elisp/+io" "\
Delete PATH.

If PATH is not specified, default to the current buffer's file.

If FORCE-P, delete without confirmation.

(fn &optional PATH FORCE-P)" t)
(autoload '+move-this-file "../elisp/+io" "\
Move current buffer's file to NEW-PATH.

If FORCE-P, overwrite the destination file if it exists, without confirmation.

(fn NEW-PATH &optional FORCE-P)" t)
(autoload '+tramp-sudo-file-path "../elisp/+io" "\
Construct a Tramp sudo path to FILE. Works for both local and remote files.

(fn FILE)")
(autoload '+sudo-find-file "../elisp/+io" "\
Open FILE as root.

(fn FILE)" t)
(autoload '+sudo-this-file "../elisp/+io" "\
Open the current file as root." t)
(autoload '+sudo-save-buffer "../elisp/+io" "\
Save this file as root." t)
(autoload '+yank-this-file-name "../elisp/+io" "\
Yank the file name of this buffer." t)
(autoload '+clean-file-name "../elisp/+io" "\
Clean file name.

(fn FILENAME &optional DOWNCASE-P)")
(autoload '+html2pdf "../elisp/+io" "\
Convert HTML file INFILE to PDF and save it to OUTFILE.
When BACKEND is provided, the corresponding program is used, otherwise, the
value of `+html2pdf-default-backend' is used.

(fn INFILE OUTFILE &optional BACKEND)")
(autoload '+txt2html "../elisp/+io" "\
Convert plain-text file INFILE to HTML and save it to OUTFILE.
When MAIL-MODE-P is non-nil, --mailmode is passed to \"txt2html\".

(fn INFILE OUTFILE &optional MAIL-MODE-P)")
(autoload '+save-as-pdf "../elisp/+io" "\
Save URL as PDF.
This function's signature is compatible with `browse-url-browser-function'
so it can be used to save HTML pages or emails to PDF.
When MAIL-MODE-P is non-nil, treat INFILE as a mail.

(fn INFILE &optional MAIL-MODE-P)")
(defvar +single-file-executable (executable-find "single-file") "\
The executable for \"single-file\" which is used archive HTML pages.")
(custom-autoload '+single-file-executable "../elisp/+io" t)
(autoload '+single-file "../elisp/+io" "\
Save URL into OUT-FILE as a standalone HTML file.

(fn URL OUT-FILE)")
(register-definition-prefixes "../elisp/+io" '("+html2pdf-default-backend" "+save-as-pdf-filename"))


;;; Generated autoloads from ../elisp/+keybinding.el

(autoload '+map! "../elisp/+keybinding" "\
A wrapper around `+minemacs--internal-map!'.
It is deferred until `general' gets loaded and configured.

(fn &rest ARGS)" nil t)
(function-put '+map! 'lisp-indent-function 'defun)
(autoload '+map-local! "../elisp/+keybinding" "\
A wrapper around `+minemacs--internal-map-local!'.
It is deferred until `general' gets loaded and configured.

(fn &rest ARGS)" nil t)
(function-put '+map-local! 'lisp-indent-function 'defun)
(autoload '+nmap! "../elisp/+keybinding" "\
A wrapper around `general-nmap'.
It is deferred until `general' gets loaded and configured.

(fn &rest ARGS)" nil t)
(function-put '+nmap! 'lisp-indent-function 'defun)
(autoload '+vmap! "../elisp/+keybinding" "\
A wrapper around `general-vmap'.
It is deferred until `general' gets loaded and configured.

(fn &rest ARGS)" nil t)
(function-put '+vmap! 'lisp-indent-function 'defun)
(autoload '+mmap! "../elisp/+keybinding" "\
A wrapper around `general-mmap'.
It is deferred until `general' gets loaded and configured.

(fn &rest ARGS)" nil t)
(function-put '+mmap! 'lisp-indent-function 'defun)
(autoload '+imap! "../elisp/+keybinding" "\
A wrapper around `general-imap'.
It is deferred until `general' gets loaded and configured.

(fn &rest ARGS)" nil t)
(function-put '+imap! 'lisp-indent-function 'defun)
(autoload '+emap! "../elisp/+keybinding" "\
A wrapper around `general-emap'.
It is deferred until `general' gets loaded and configured.

(fn &rest ARGS)" nil t)
(function-put '+emap! 'lisp-indent-function 'defun)
(autoload '+omap! "../elisp/+keybinding" "\
A wrapper around `general-omap'.
It is deferred until `general' gets loaded and configured.

(fn &rest ARGS)" nil t)
(function-put '+omap! 'lisp-indent-function 'defun)
(autoload '+rmap! "../elisp/+keybinding" "\
A wrapper around `general-rmap'.
It is deferred until `general' gets loaded and configured.

(fn &rest ARGS)" nil t)
(function-put '+rmap! 'lisp-indent-function 'defun)
(autoload '+iemap! "../elisp/+keybinding" "\
A wrapper around `general-iemap'.
It is deferred until `general' gets loaded and configured.

(fn &rest ARGS)" nil t)
(function-put '+iemap! 'lisp-indent-function 'defun)
(autoload '+nvmap! "../elisp/+keybinding" "\
A wrapper around `general-nvmap'.
It is deferred until `general' gets loaded and configured.

(fn &rest ARGS)" nil t)
(function-put '+nvmap! 'lisp-indent-function 'defun)


;;; Generated autoloads from ../elisp/+minemacs.el

(autoload '+error! "../elisp/+minemacs" "\
Log error MSG and VARS using `message'.

(fn MSG &rest VARS)" nil t)
(autoload '+info! "../elisp/+minemacs" "\
Log info MSG and VARS using `message'.

(fn MSG &rest VARS)" nil t)
(autoload '+log! "../elisp/+minemacs" "\
Log MSG and VARS using `message' when `minemacs-verbose' is non-nil.

(fn MSG &rest VARS)" nil t)
(autoload '+debug! "../elisp/+minemacs" "\
Log error MSG and VARS using `message'.

(fn MSG &rest VARS)" nil t)
(autoload '+emacs-features-p "../elisp/+minemacs" "\
Is features FEATS are enabled in this Emacs build.

(fn &rest FEATS)")
(autoload '+fn-inhibit-messages! "../elisp/+minemacs" "\
Add an advice around the function FN to suppress messages in echo area.
If NO-MESSAGE-LOG is non-nil, do not print any message to *Messages* buffer.

(fn FN &optional NO-MESSAGE-LOG)" nil t)
(autoload '+shutup! "../elisp/+minemacs" "\
Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated.

(fn &rest BODY)" nil t)
(autoload '+suppress! "../elisp/+minemacs" "\
Suppress new messages temporarily in the echo area while BODY is evaluated.

(fn &rest BODY)" nil t)
(autoload '+cmdfy! "../elisp/+minemacs" "\
Convert BODY to an interactive command.

(fn &rest BODY)" nil t)
(autoload '+set-fonts "../elisp/+minemacs" "\
Set Emacs' fonts from `minemacs-fonts'." t)
(autoload '+load-theme "../elisp/+minemacs" "\
Load Emacs' theme from `minemacs-theme'." t)
(autoload '+eval-when-idle "../elisp/+minemacs" "\
Queue FNS to be processed when Emacs becomes idle.

(fn DELAY &rest FNS)")
(autoload '+eval-when-idle! "../elisp/+minemacs" "\
Evaluate BODY when Emacs becomes idle.

(fn &rest BODY)" nil t)
(function-put '+eval-when-idle! 'lisp-indent-function 0)
(autoload '+eval-when-idle-for! "../elisp/+minemacs" "\
Evaluate BODY after DELAY seconds from Emacs becoming idle.

(fn DELAY &rest BODY)" nil t)
(function-put '+eval-when-idle-for! 'lisp-indent-function 1)
(autoload '+deferred! "../elisp/+minemacs" "\
Run BODY after Emacs gets loaded, a.k.a. after `minemacs-loaded'.

(fn &rest BODY)" nil t)
(autoload '+deferred-when! "../elisp/+minemacs" "\
Like `+deferred!', with BODY executed only if CONDITION is non-nil.

(fn CONDITION &rest BODY)" nil t)
(function-put '+deferred-when! 'lisp-indent-function 1)
(autoload '+deferred-unless! "../elisp/+minemacs" "\
Like `+deferred!', with BODY executed only if CONDITION is nil.

(fn CONDITION &rest BODY)" nil t)
(function-put '+deferred-unless! 'lisp-indent-function 1)
(autoload '+deferred-or-immediate! "../elisp/+minemacs" "\
Like `+deferred!', with BODY deferred if CONDITION is non-nil, otherwise it acts like `progn'.

(fn CONDITION &rest BODY)" nil t)
(function-put '+deferred-or-immediate! 'lisp-indent-function 1)
(autoload '+lazy! "../elisp/+minemacs" "\
Run BODY as a lazy block (see `minemacs-lazy').

(fn &rest BODY)" nil t)
(autoload '+lazy-when! "../elisp/+minemacs" "\
Like `+lazy!', with BODY executed only if CONDITION is non-nil.

(fn CONDITION &rest BODY)" nil t)
(function-put '+lazy-when! 'lisp-indent-function 1)
(autoload '+lazy-unless! "../elisp/+minemacs" "\
Like `+lazy!', with BODY executed only if CONDITION is nil.

(fn CONDITION &rest BODY)" nil t)
(function-put '+lazy-unless! 'lisp-indent-function 1)
(autoload '+lazy-or-immediate! "../elisp/+minemacs" "\
Like `+lazy!', with BODY deferred if CONDITION is non nil, otherwise it acts like `progn'.

(fn CONDITION &rest BODY)" nil t)
(function-put '+lazy-or-immediate! 'lisp-indent-function 1)
(autoload '+after-load! "../elisp/+minemacs" "\
Execute BODY after FEATURES have been loaded.

(fn FEATURES &rest BODY)" nil t)
(function-put '+after-load! 'lisp-indent-function 1)
(autoload '+hook-with-delay! "../elisp/+minemacs" "\
Add the FUNCTION to the value of HOOK.
The FUNCTION is delayed to be evaluated in SECS once HOOK is
triggered.
DEPTH and LOCAL are passed as is to `add-hook'.

(fn HOOK SECS FUNCTION &optional DEPTH LOCAL)" nil t)
(autoload '+hook-once! "../elisp/+minemacs" "\
Hook BODY in HOOK, it runs only once.

(fn HOOK &rest BODY)" nil t)
(function-put '+hook-once! 'lisp-indent-function 1)
(autoload '+compile-functions "../elisp/+minemacs" "\
Queue FNS to be byte/natively-compiled after a brief delay.

(fn &rest FNS)")
(autoload '+env-save "../elisp/+minemacs" "\
Load environment variables of the current session to the file
  \".emacs.d/local/system-env.el\"." t)
(autoload '+env-load "../elisp/+minemacs" "\
Load environment variables from the file saved in
  \".emacs.d/local/system-env.el\" if available." t)
(autoload '+ignore-root "../elisp/+minemacs" "\
Add ROOTS to ignored projects, recentf, etc.

(fn &rest ROOTS)")
(autoload '+register-build-function! "../elisp/+minemacs" "\
Register build function FN to be called at the end of `minemacs-update'.

(fn FN)" nil t)
(autoload 'minemacs-update "../elisp/+minemacs" "\
Update MinEmacs packages." t)
(register-definition-prefixes "../elisp/+minemacs" '("+eval-when-idle-" "+hook-once-num" "minemacs--build-functions"))


;;; Generated autoloads from ../elisp/+primitives.el

(autoload '+plist-keys "../elisp/+primitives" "\
Return the keys of PLIST.

(fn PLIST)")
(autoload '+plist-push! "../elisp/+primitives" "\
Push KEY-VALS to PLIST.

(fn PLIST &rest KEY-VALS)" nil t)
(function-put '+plist-push! 'lisp-indent-function 1)
(autoload '+plist-combine "../elisp/+primitives" "\
Create a single property list from all plists in PLISTS.
Modified from `org-combine-plists'. This supposes the values to be vectors,
and concatenate them.

(fn &rest PLISTS)")
(autoload '+plist-delete "../elisp/+primitives" "\
Delete property PROP from PLIST.
Adapted from `org-plist-delete'.

(fn PLIST PROP)")
(autoload '+plist-to-alist "../elisp/+primitives" "\


(fn PLIST &optional TRIM-COL)")
(autoload '+alist-to-plist "../elisp/+primitives" "\


(fn ALIST &optional ADD-COL)")
(autoload '+alist-set "../elisp/+primitives" "\
Set property KEY to VAL in ALIST. Return new alist.
This creates the association if it is missing, and otherwise sets
the cdr of the first matching association in the list. It does
not create duplicate associations. By default, key comparison is
done with `equal'. However, if SYMBOL is non-nil, then `eq' is
used instead.

This method may mutate the original alist, but you still need to
use the return value of this method instead of the original
alist, to ensure correct results.

(fn KEY VAL ALIST &optional SYMBOL)")
(autoload '+serialize-sym "../elisp/+primitives" "\
Serialize SYM to DIR.
If FILENAME-FORMAT is non-nil, use it to format the file name (ex. \"file-%s.el\").
Return the written file name, or nil if SYM is not bound.

(fn SYM &optional DIR FILENAME-FORMAT)")
(autoload '+deserialize-sym "../elisp/+primitives" "\
Deserialize SYM from DIR, if MUTATE is non-nil, assign the object to SYM.
If FILENAME-FORMAT is non-nil, use it to format the file name (ex. \"file-%s.el\").
Return the deserialized object, or nil if the SYM.el file dont exist.

(fn SYM &optional DIR MUTATE FILENAME-FORMAT)")
(autoload '+reset-sym "../elisp/+primitives" "\
Reset SYM to its standard value.

(fn SYM)")
(autoload '+reset-var! "../elisp/+primitives" "\
Reset VAR to its standard value.

(fn VAR)" nil t)
(autoload '+unquote "../elisp/+primitives" "\
Return EXP unquoted.

(fn EXPR)")
(function-put '+unquote 'pure 't)
(function-put '+unquote 'side-effect-free 't)
(autoload '+quoted "../elisp/+primitives" "\
Retrun t when EXP is quoted.

(fn EXPR)")
(autoload '+apply-partially-right "../elisp/+primitives" "\
Like `apply-partially', but applies the ARGS to the right of FUN.

(fn FUN &rest ARGS)")
(register-definition-prefixes "../elisp/+primitives" '("+serialized-symbols-directory"))


;;; Generated autoloads from ../elisp/+project.el

(autoload '+project-scan-for-projects "../elisp/+project" "\
Scan and remember projects under `+project-scan-dir-paths'." t)
(autoload '+project-add-project "../elisp/+project" "\
Switch to another project at DIR.
When DIR is not detected as a project, ask to force it to be by adding a
\".project.el\" file.

(fn DIR &optional DONT-ASK)" t)
(autoload '+project-gdb "../elisp/+project" "\
Invoke `gdb' in the project's root." t)
(register-definition-prefixes "../elisp/+project" '("+project-scan-dir-paths"))


;;; Generated autoloads from ../elisp/+systemd.el

(autoload '+systemd-running-p "../elisp/+systemd" "\
Check if the systemd SERVICE is running.

(fn SERVICE)")
(autoload '+systemd-command "../elisp/+systemd" "\
Call systemd with COMMAND and SERVICE.

(fn SERVICE COMMAND &optional PRE-FN POST-FN)")
(autoload '+systemd-start "../elisp/+systemd" "\
Start systemd SERVICE.

(fn SERVICE &optional PRE-FN POST-FN)")
(autoload '+systemd-stop "../elisp/+systemd" "\
Stops the systemd SERVICE.

(fn SERVICE &optional PRE-FN POST-FN)")


;;; Generated autoloads from ../elisp/+unix.el

(autoload '+chmod-this-file "../elisp/+unix" "\
Execute Unix command `chmod'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chmod').

(fn CMD)" t)
(autoload '+chgrp-this-file "../elisp/+unix" "\
Execute Unix command `chgrp'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chgrp').

(fn CMD)" t)
(autoload '+chown-this-file "../elisp/+unix" "\
Execute Unix command `chown'.  Current buffer's file is default arg.
CMD is the command to execute (interactively, `chown').

(fn CMD)" t)
(register-definition-prefixes "../elisp/+unix" '("+read-shell-file-command"))


;;; Generated autoloads from ../elisp/ecryptfs.el

(autoload 'ecryptfs-toggle-mount-private "../elisp/ecryptfs" "\
Mount/Unmount eCryptfs' private directory." t)
(autoload 'ecryptfs-mount-private "../elisp/ecryptfs" "\
Mount eCryptfs' private directory." t)
(autoload 'ecryptfs-umount-private "../elisp/ecryptfs" "\
Unmount eCryptfs' private directory." t)
(register-definition-prefixes "../elisp/ecryptfs" '("ecryptfs-"))


;;; Generated autoloads from me-backports-29.el

(register-definition-prefixes "me-backports-29" '("loaddefs-generate" "messages-buffer-name" "native-compile-prune-cache" "scratch-buffer" "setopt" "with-memoization"))


;;; Generated autoloads from me-core-ui.el

(register-definition-prefixes "me-core-ui" '("+theme--tweaks-h"))


;;; Generated autoloads from ../modules/extras/me-dap-utils.el

(autoload '+github-latest-release "../modules/extras/me-dap-utils" "\
Get the latest release of USER/REPO. Strips the \"v\" at left.

Fallback to FALLBACK-RELEASE when it can't get the last one.

(fn USER REPO &optional FALLBACK-RELEASE)")


;;; Generated autoloads from me-defaults.el

(register-definition-prefixes "me-defaults" '("+show-trailing-whitespace-h" "+whitespace-auto-cleanup-modes"))


;;; Generated autoloads from ../modules/extras/me-eglot-ltex.el

(register-definition-prefixes "../modules/extras/me-eglot-ltex" '("eglot-ltex-"))


;;; Generated autoloads from ../modules/extras/me-elisp-extras.el

(register-definition-prefixes "../modules/extras/me-elisp-extras" '("+elisp-" "+emacs-lisp--"))


;;; Generated autoloads from ../modules/extras/me-gdb.el

(autoload '+gdb-set-layout "../modules/extras/me-gdb" "\
Enable custom window layout for gdb." t)
(autoload '+emacs-gdb-enable "../modules/extras/me-gdb" "\
Load a faster \"gdb\" command from \"emacs-gdb\".
This will overwrite the built-in \"gdb-mi\" for this session." t)
(register-definition-prefixes "../modules/extras/me-gdb" '("+gdb-"))


;;; Generated autoloads from me-modules.el

(register-definition-prefixes "me-modules" '("minemacs-"))


;;; Generated autoloads from ../modules/extras/me-mu4e-extras.el

(autoload '+mu4e-extras-setup "../modules/extras/me-mu4e-extras")
(register-definition-prefixes "../modules/extras/me-mu4e-extras" '("+mu4e-" "+org-msg-make-signature"))


;;; Generated autoloads from ../modules/extras/me-mu4e-gmail.el

(register-definition-prefixes "../modules/extras/me-mu4e-gmail" '("+mu4e-"))


;;; Generated autoloads from ../modules/extras/me-mu4e-ui.el

(register-definition-prefixes "../modules/extras/me-mu4e-ui" '("+mu4e-"))


;;; Generated autoloads from ../modules/extras/me-org-extras.el

(register-definition-prefixes "../modules/extras/me-org-extras" '("+org-"))


;;; Generated autoloads from ../modules/extras/me-realgud.el

(autoload '+realgud:start "../modules/extras/me-realgud" "Start the RealGUD debugger suitable for the current mode." t)
(autoload '+realgud:toggle-breakpoint "../modules/extras/me-realgud" "Toggle break point." t)
(autoload '+realgud-hydra/body "../modules/extras/me-realgud" "Hydra keys for RealGUD." t)
(register-definition-prefixes "../modules/extras/me-realgud" '("+realgud:cmd-"))


;;; Generated autoloads from ../modules/extras/me-spell-fu.el

(autoload '+spell-fu-correct "../modules/extras/me-spell-fu" "\
Correct spelling of word at point." t)
(autoload '+spell-fu-register-dictionaries! "../modules/extras/me-spell-fu" "\
Register dictionaries for `LANGS` to spell-fu's multi-dict.

(fn &rest LANGS)" nil t)
(define-obsolete-function-alias '+spell-fu-register-dictionaries '+spell-fu-register-dictionaries! "2023-05-27")
(register-definition-prefixes "../modules/extras/me-spell-fu" '("+spell-fu--"))


;;; Generated autoloads from me-splash.el

(register-definition-prefixes "me-splash" '("minemacs-splash"))


;;; Generated autoloads from me-vars.el

(register-definition-prefixes "me-vars" '("+env-save-vars" "emacs/features" "minemacs-" "os/" "sys/arch"))


;;; Generated autoloads from ../modules/extras/me-writing-mode.el

(autoload '+writing-mode "../modules/extras/me-writing-mode" "\
A mode for writing without distraction.

This is a minor mode.  If called interactively, toggle the
`+Writing mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `+writing-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "../modules/extras/me-writing-mode" '("+writing-"))


;;; Generated autoloads from ../elisp/netextender.el

(autoload 'netextender-start "../elisp/netextender" "\
Launch a NetExtender VPN session." t)
(autoload 'netextender-toggle "../elisp/netextender" "\
Toggle connection to NetExtender." t)
(register-definition-prefixes "../elisp/netextender" '("netextender-"))


;;; Generated autoloads from ../elisp/valgrind.el

(autoload 'valgrind "../elisp/valgrind" "\
Run valgrind.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*valgrind*'.
You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

(fn COMMAND)" t)
(register-definition-prefixes "../elisp/valgrind" '("valgrind-"))

;;; End of scraped data

(provide 'me-loaddefs)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8
;; End:

;;; me-loaddefs.el ends here
