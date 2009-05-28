;;; .emacs --- Emacs configuration file

;; Copyright (C) 2003 Samuel Tesla

;; Author: Samuel Tesla <samuel@alieniloquent.com>

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; type C-h C-c inside GNU Emacs to view the license.  Otherwise,
;; write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is my .emacs file, based heavily on Tyler Berry's which was
;; based heavily on Ted O'Connor's with additions from the Emacs Wiki
;; and other sources.  Feel free to do with it as you like and the GPL
;; permits, and please let me know of any problems or bugs you find.

;;; Debugging:

;; These are only active when I'm working on this file.

; (setq debug-on-error t
;      debug-on-quit  t)
;
; (defmacro stesla-no-op (&rest args) nil)

;; This is only here while I'm porting stuff from Tyler's .emacs
;
;(defun stesla-replace-stesla-string (start end)
;  "replace the string 'stesla' with the string 'stesla'"
;  (interactive "*r")
;  (replace-string "tyler" "stesla" nil start end))
;(global-set-key (kbd "C-c C-t") 'stesla-replace-stesla-string)

;;; Setup:

;; `when' and `unless' are undefined in older versions of GNU Emacs.

(if (not (fboundp 'when))
    (progn
      (defmacro when (test &rest body)
    "If TEST is non-nil, evaluate the forms in BODY.
If TEST is nil, return nil."
    `(if ,test
         (progn ,@body)
       nil))))

(when (not (fboundp 'unless))
  (defmacro unless (test &rest body)
    "If TEST is nil, evaluate the forms in BODY.
If TEST is non-nil, return nil."
    `(when (not ,test)
       ,@body)))

;; If we don't have the `mapc' function, alias to `mapcar'.

(unless (fboundp 'mapc)
  (defalias 'mapc 'mapcar))

;; If we don't have the `kbd' macro, provide it.

(unless (fboundp 'kbd)
  (defmacro kbd (key-sequence)
    (read-kbd-macro key-sequence)))

;; Frob `load-path' to include ~/.elisp.

(defconst elisp-dir (expand-file-name "~/.elisp"))

(when (file-directory-p elisp-dir)
  (add-to-list 'load-path elisp-dir)
  ;; ~/elisp/subdirs.el should load any subdirectories.
  (load (concat elisp-dir "/subdirs.el") t))

;; A bunch of version and feature detection stuff.

(defconst stesla-xemacs-p (featurep 'xemacs)
  "Is this XEmacs?")

(defconst stesla-gnu-emacs-p (not stesla-xemacs-p)
  "Is this GNU Emacs?")

(defconst stesla-emacs-20-or-later-p (>= emacs-major-version 20))
(defconst stesla-emacs-21-or-later-p (>= emacs-major-version 21))
(defconst stesla-emacs-20-p          (= emacs-major-version 20))
(defconst stesla-emacs-19-p          (= emacs-major-version 19))
(defconst stesla-emacs-21.1-p
  (and stesla-gnu-emacs-p
       (= emacs-major-version 21)
       (= emacs-minor-version 1)))
(defvar stesla-oort-or-later-p t
  "If non-nil, this Emacs is equipped with Oort Gnus.")

(defconst stesla-display-graphic-p (if (fboundp 'display-graphic-p)
                                      (display-graphic-p)
                                    window-system)
  "Should we display graphics?")

(defconst stesla-use-my-font-p stesla-display-graphic-p
  "Should we set the font in frame alists?")

(defconst stesla-tty-p (if (fboundp 'console-type) ; XEmacs
              (eq (console-type) 'tty)
            (not stesla-display-graphic-p))
  "Are we using a TTY?")

(defconst stesla-use-colors-p
  (cond ((fboundp 'display-color-p)
         (display-color-p))
        (stesla-xemacs-p t)
        (t stesla-display-graphic-p))
  "Should we use colors?")

(defconst stesla-use-menu-bar-p nil
  "Should we use the menu bar?")

(defconst stesla-use-mouse-p (if (fboundp 'display-mouse-p)
                                (display-mouse-p)
                              stesla-display-graphic-p)
  "Should we load mouse customizations?")

(defconst stesla-use-popup-menus-p
  (if (fboundp 'display-popup-menus-p)
      (display-popup-menus-p)
    stesla-display-graphic-p)
  "Should we load popup menu customations?")

(defconst stesla-w32-window-system-p (memq window-system '(w32 win32))
  "Are we running graphically under Microsoft Windows?")

(defconst stesla-mac-window-system-p (eq window-system 'mac)
  "Are we running graphically under the Mac OS?")

(defconst stesla-x-window-system-p (eq window-system 'x)
  "Are we running graphically under the X Window System?")

(defconst stesla-w32-p (or stesla-w32-window-system-p
                          (eq system-type 'windows-nt))
  "Are we running under Microsoft Windows?")

(defconst stesla-mac-p (or stesla-mac-window-system-p
                          (eq system-type 'darwin))
  "Are we running on a Macintosh?")

(defconst stesla-most-positive-fixnum (lsh -1 -1))

(when stesla-mac-p
  (setq mac-command-modifier nil)
  (setq mac-option-modifier 'meta))

(if stesla-w32-p
    (defconst stesla-dotemacs-file "~/config/dot.emacs")
  (defconst stesla-dotemacs-file "~/.emacs"))

(defconst tyler-dotemacs-file "~/.elisp/tyler.emacs.el")

(defconst stesla-emacs-name
  (let ((version-int (number-to-string emacs-major-version)))
    (cond (stesla-xemacs-p    (concat "xemacs-" version-int))
          (stesla-gnu-emacs-p (concat "emacs-" version-int))
          (t                 "unknown-emacs")))
  "The name of this Emacs.")

(defconst stesla-emacs-pretty-name
  (let ((version-int (concat
                      (number-to-string emacs-major-version)
                      "."
                      (number-to-string emacs-minor-version))))
    (cond (stesla-xemacs-p    (concat "XEmacs " version-int))
          (stesla-gnu-emacs-p (concat "GNU Emacs " version-int))
          (t                 "Emacs")))
  "The name of this Emacs, formatted prettily.")

;; (setq initial-scratch-message
;;       (concat ";; Welcome to "
;;               stesla-emacs-name
;;               ", as (mis)configured by Ted O'Connor.\n"
;;               ";; Share and Enjoy!\n\n"))

(defun load-no-error (package)
  (condition-case nil (load package) (error nil)))

;; Ted's conditional loading functions.

(defun require-no-error (package)
  "This is Ted O'Connor's non-erroring version of (require PACKAGE)."
  (condition-case nil (require package) (error nil)))

;;; Basic functionality:

;; These functions convert files between DOS/UNIX/Mac formats.

(defun dos-line-endings ()
  "Sets the buffer-file-coding-system to undecided-dos; changes the buffer
by invisibly adding carriage returns."
  (interactive)
  (set-buffer-file-coding-system `undecided-dos nil))

(defun unix-line-endings ()
  "Sets the buffer-file-coding-system to undecided-unix; changes the buffer
by invisibly removing carriage returns."
  (interactive)
    (set-buffer-file-coding-system 'undecided-unix nil))

(defun mac-line-endings ()
  "Sets the buffer-file-coding-system to undecided-mac; may change the buffer
by invisibly removing carriage returns."
  (interactive)
  (set-buffer-file-coding-system `undecided-mac nil))


;; A function to correct the `backspace = C-h' issue.

(when stesla-tty-p
  (defun stesla-fix-stupid-backspace-key-issue ()
    "Fix this stupid terminal."
    (interactive)
    (keyboard-translate ?\C-h ?\C-?)
    (message "Backspace has been fixed.")))

;; GNU Emacs 20:

(when (and stesla-use-colors-p stesla-emacs-20-p)
  (defmacro highlight-trailing-whitespace (m)
    "Highlight trailing whitespace in mode M."
    `(font-lock-add-keywords ,m '(("[ \t]+$" .
                                   show-paren-match-face))))

  ;; To get various Emacs 20s to DTRT.

  (setq show-paren-match-face default)

  (mapc (lambda (mode)
          (highlight-trailing-whitespace mode))
        '(c-mode c++-mode java-mode lisp-mode emacs-lisp-mode
          latex-mode cperl-mode python-mode)))

;; GNU Emacs 21:

(when (and stesla-gnu-emacs-p stesla-emacs-21-or-later-p)
  (setq-default show-trailing-whitespace t)

  (defun stesla-hide-trailing-whitespace ()
    "Turn off trailing whitespace highlighting in this buffer."
    (interactive)
    (setq show-trailing-whitespace nil))

  (mapc (lambda (mode-hook)
          (add-hook mode-hook 'stesla-hide-trailing-whitespace))
        '(Buffer-menu-mode-hook custom-mode-hook term-mode-hook Info-mode-hook
          comint-mode-hook buffer-menu-mode-hook apropos-mode-hook
          tooltip-show-hook gnus-article-mode-hook mail-mode-hook
          gnus-summary-mode-hook message-mode-hook gnus-group-mode-hook
          eshell-mode-hook w3-mode-hook w3m-mode-hook help-modeq))

  (mapc (lambda (mode-hook)
          (add-hook mode-hook
                    (lambda () (setq show-trailing-whitespace t))))
        '(latex-mode-hook html-mode-hook)))

(unless (fboundp 'stesla-hide-trailing-whitespace)
  (defun stesla-hide-trailing-whitespace ()
    "Placeholder for Emacsen which don't use this function."
    (interactive)
    nil))

;; Don't warn about nonsense X11 modifier issues.

(when stesla-xemacs-p
  (setq display-warning-minimum-level 'error)
  (setq log-warning-minimum-level 'info))

;; Ensure that `turn-off-auto-fill' is defined.

(unless (fboundp 'turn-off-auto-fill)
  (defun turn-off-auto-fill ()
    "Unconditionally turn off Auto Fill mode."
    (interactive)
    (auto-fill-mode -1)))

;; "Clear the screen" losslessly using `recenter'.

(defun stesla-clear ()
  "\"Clear the screen\" (recenter 0)."
  (interactive)
  (recenter 0))

;; A quick function to open a library.

(defun stesla-find-library (library)
  "Open LIBRARY."
  (interactive "sLibrary: ")
  (let ((filename (locate-library (concat library ".el"))))
    (if (stringp filename)
        (find-file filename)
      (message "Library %s not found." library))))

;; Allow the *Messages* buffer to grow indefinitely.  (Or close.)

(setq message-log-max stesla-most-positive-fixnum)

;; Switch from an audible to a visible bell.
(setq ring-bell-function nil
      visible-bell       t)

;; Always add final newlines to buffers without them.

(setq require-final-newline t)

;; Auto-fill by default.

(setq-default auto-fill-function 'do-auto-fill)

;; ...except in these major modes.

(mapc (lambda (mode-hook)
    (add-hook mode-hook 'turn-off-auto-fill))
      '(sh-mode-hook comint-mode-hook shell-mode-hook erc-mode-hook))

;; Make sure that yanks are inserted at point, not at the location of the
;; mouse.

(setq mouse-yank-at-point t)

;; Arrange for a sane, non-cluttered backup system.

(when stesla-emacs-21-or-later-p
  (setq backup-by-copying      t
        backup-directory-alist '(("." . "~/.backups"))
        kept-new-versions      6
        kept-old-versions      2
        delete-old-versions    t
        version-control        t))

;; Follow symlinks to version-controlled files without asking.

(setq vc-follow-symlinks t)

;; Avoid scrolling by large amounts.

(setq scroll-step              1
      scroll-conservatively    10
      scroll-up-aggressively   .00000001
      scroll-down-aggressively .00000001)
;; Don't ask for confirmation on yes-or-no questions.

(defalias 'yes-or-no-p 'y-or-n-p)

;; When using isearch, highlight the current match.

(if stesla-xemacs-p
    (setq isearch-highlight t)
  (setq search-highlight t))

;; Make sure that mark gets set when I set it in XEmacs.

(when stesla-xemacs-p
  (setq zmacs-regions nil))

;; Turn on line and column numbers in the mode line.

(line-number-mode 1)
(when (fboundp 'column-number-mode)
  (column-number-mode 1))

;; Turn on abbrevs.

(setq default-abbrev-mode t)
(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))


;; Turn on paren matching.

(when (fboundp 'show-paren-mode) ; GNU Emacs
  (show-paren-mode 1))
(when stesla-xemacs-p             ; XEmacs
  (setq paren-mode 'paren))

;; Keep temporary buffers down to a reasonable size.

(when (fboundp 'temp-buffer-resize-mode)
  (temp-buffer-resize-mode 1))


;; Resize the minibuffer appropriately in different Emacsen.

(cond (stesla-xemacs-p
       (progn
     (autoload 'resize-minibuffer-mode "rsz-minibuf" nil t)
     (setq resize-minibuffer-window-exactly t)))
      ((not stesla-emacs-21-or-later-p) ; GNU Emacs 20 or older
       (resize-minibuffer-mode 1))
      (t (progn                        ; GNU Emacs 21 or newer
       (setq max-mini-window-height 0.30)
       (setq resize-mini-window t))))

;; Make Emacs always use spaces.  Never ever use tabs.

(setq-default indent-tabs-mode nil)

;; Indent to mod 2 when the <TAB> key is hit.

(setq-default c-basic-indent 2)

;; Display four spaces when encountering a \t character in a file.

(setq-default tab-width 4)

;; A function to untabify the current buffer.  This comes from jwz.

(defun stesla-untabify-buffer ()
  "Untabify the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[\t ]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
        (untabify (1- (point)) (point-max))))
  nil)

;; Automatically execute this on certain types of source code.

(mapc (lambda (mode-hook)
    (add-hook mode-hook 'stesla-untabify-buffer))
      '(c-mode-hook cperl-mode-hook emacs-lisp-mode-hook))

;; Set the default fill column.

(setq-default fill-column 79)

;; enable visual feedback on selections

(setq-default transient-mark-mode t)

;; stop at the end of the file, not just add lines

(setq next-line-add-newlines nil)

;; Uniquely identify buffers that happen to have the same name

(when (require-no-error 'uniquify)
  (setq-default uniquify-buffer-name-style 'forward))

;; Use `iswitchb' to switch between buffers.  What a massive improvement!

(cond ((fboundp 'iswitchb-mode)                ; GNU Emacs 21
       (iswitchb-mode 1))
      ((fboundp 'iswitchb-default-keybindings) ; Old-style
       (iswitchb-default-keybindings)))

;; Use `partial-completion-mode'.  Very cool.

(when (fboundp 'partial-completion-mode)
  (partial-completion-mode 1))

;; Time stamp files on write.

(add-hook 'write-file-hooks 'time-stamp)

;; Set up e-mail miscellany.

(add-hook 'mail-setup-hook 'mail-abbrevs-setup)

(setq user-mail-address (concat (getenv "USER") "@alieniloquent.com")
      user-full-name    "Samuel Tesla"
      mail-signature    t
      mail-yank-prefix  "> "
      mail-from-style   'angles)

(if (boundp 'my-copyright-holder)
    (setq auto-insert-copyright my-copyright-holder)
  (setq auto-insert-copyright (user-full-name)))

(add-hook 'mail-mode-hook
          (lambda ()
            ;; Kill quoted signatures.
            (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*")
            (setq make-backup-files nil)  ; None necessary.
            (setq fill-column 72)
            (not-modified)))

;; Turn on `truncate-lines' - continuation lines are ugly.

;(setq truncate-partial-width-windows nil)
;(setq-default truncate-lines t)
;(let ((foo '(lambda ()
;              (setq truncate-lines nil))))
;  (add-hook 'term-mode-hook foo)
;  (add-hook 'eshell-mode-hook foo))

;; My home page

(defconst stesla-home-page "http://www.alieniloquent.com/")

;; My current physical location characteristics.

;(setq calendar-latitude 40.416667
;      calendar-longitude -104.683333
;      calendar-location-name "Greeley, CO")

;; Activate links in text.

(when (fboundp 'goto-address)
  (setq goto-address-fontify-maximum-size stesla-most-positive-fixnum)
  (add-hook 'find-file-hooks 'goto-address))

;; Default browser for TTYs; for a windowing system, the default is usually
;; sensible.  This gets overriden later if emacs-w3m is present.

(when stesla-tty-p
  (setq browse-url-browser-function 'browse-url-lynx-emacs))

;; Turn on or off the menu bar, as appropriate.

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode (if stesla-use-menu-bar-p 1 -1)))

;; Set up a default terminal coding.  FIXME: Set this up for Unicode.

(when (and stesla-tty-p (fboundp 'set-terminal-coding-system))
  (set-terminal-coding-system 'iso-8859-1))

;; Group the C-<down-mouse-1> buffer menu by major mode.

(setq mouse-buffer-menu-mode-mult 1)

;; I do not want to assign my copyrights to the FSF, thank you.

(setenv "ORGANIZATION" "Samuel Tesla")

;; Keybindings for `stesla-fix-stupid-backspace-key-issue'.

(when stesla-tty-p
  (global-set-key (kbd "C-c B") 'stesla-fix-stupid-backspace-key-issue)

  ;; Since that will nuke C-h, bind help to a different chord.
  (global-set-key (kbd "C-c H") 'help-command))

;; Support functions for `stesla-rotate-buffers'.  FromD the EmacsWiki.

(defvar stesla-hated-buffers '("KILL" "*Apropos*" "*Completions*" "*grep*"
                               ".newsrc-dribble" ".bbdb" "sent-mail" "*vc*"
                              "*Compile-Log*" "*Help*" "*Messages*"
                              "*compilation*"))


(defvar stesla-hated-buffer-regexps '("^ " "*Buffer" "^\\*trace" "^\\*tramp"))

(setq iswitchb-buffer-ignore (append stesla-hated-buffer-regexps stesla-hated-buffers))

(defmacro stesla-buffer-regexp-mapcar (regexp buffers)
  "Find BUFFERS whose name matches REGEXP"
  `(mapcar (lambda (this-buffer)
             (if (string-match ,regexp (buffer-name this-buffer))
                 this-buffer))
           ,(if (symbolp buffers) (symbol-value buffers) buffers)))

(defmacro stesla-hated-buffer-from-regexps (regexps)
  "Generate a one-dimensional list of buffers that match REGEXPS"
  (append
   '(append)
   (mapcar (lambda (regexp)
             `(delete nil (stesla-buffer-regexp-mapcar ,regexp
                                                       (buffer-list))))
           (if (symbolp regexps) (symbol-value regexps) regexps))))

(defun stesla-delete-from-list (delete-these from-list)
  "Delete DELETE-THESE from FROM-LIST."
  (cond
   ((car delete-these)
      (if (member (car delete-these) from-list)
        (stesla-delete-from-list (cdr delete-these)
                                (delete (car delete-these) from-list))
      (stesla-delete-from-list (cdr delete-these) from-list)))
   (t from-list)))

(defun stesla-hated-buffers ()
  "List of buffers I never want to see."
  (delete nil
          (append
           (mapcar 'get-buffer stesla-hated-buffers)
           (stesla-hated-buffer-from-regexps stesla-hated-buffer-regexps))))

;; `stesla-rotate-buffers': Like `bury-buffer' but with the capability to
;; exclude certain specified buffers.

(defun stesla-rotate-buffers (&optional n)
  "Switch to the Nth next buffer.  Negative arguments move backwards."
  (interactive)
  (unless n
    (setq n 1))
  (let ((my-buffer-list
         (stesla-delete-from-list (stesla-hated-buffers)
                                 (buffer-list (selected-frame)))))
    (switch-to-buffer
     (if (< n 0)
         (nth (+ (length my-buffer-list) n)
              my-buffer-list)
       (bury-buffer)
       (nth n my-buffer-list)))))

;; Windows-style C-TAB and C-M-TAB to switch buffers.

(global-set-key (kbd "C-<tab>") 'stesla-rotate-buffers)
(global-set-key (kbd "C-M-<tab>") (lambda ()
                                    (interactive)
                                    (stesla-rotate-buffers -1)))

;; This is C-TAB and C-M-TAB for the Linux console.  This requires special
;; setup; namely, you need to load a keymap file with /usr/bin/loadkeys
;; containing the following lines:
;;
;; control keycode 15 = Macro
;; control alt keycode 15 = Pause
;;
;; If you actually -have- a key that generates the Macro or Pause keysyms, you
;; have a better keyboard than I.  For me, this makes Emacs DWIW.  Credit for
;; this hack goes to Alex Schroeder.

(global-set-key (kbd "ESC [ M") 'stesla-rotate-buffers)
(global-set-key (kbd "ESC [ P") (lambda ()
                                  (interactive)
                                  (stesla-rotate-buffers -1)))

;; Various function bindings.

(global-set-key [kp-enter] 'eval-last-sexp)
(global-set-key (kbd "C-c l") 'goto-line)
(global-set-key (kbd "C-c c") 'stesla-clear)
(global-set-key (kbd "C-c p") 'list-text-properties-at)
(global-set-key (kbd "C-c t") 'text-mode)
(global-set-key (kbd "C-c C-r") 'redraw-display)
(global-set-key (kbd "C-h A") 'apropos-variable)
(global-set-key (kbd "C-h F") 'find-function)
(global-set-key (kbd "C-c L") 'stesla-find-library)
(global-set-key (kbd "C-h V") 'find-variable)
(when (fboundp 'find-file-at-point)
  (global-set-key (kbd "C-c f") 'find-file-at-point))

;; Fix the behavior of RET in `help-mode' in XEmacs.

(when stesla-xemacs-p
  (setq apropos-do-all t))

;; PuTTY generates this keysym.

(global-set-key [insertchar] 'overwrite-mode)

;; This chord is almost certainly an accident.

(global-set-key (kbd "C-x f") 'find-file)

;; I prefer `buffer-menu' to `list-buffers'.

(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; In addition to C-c chords, F5-F9 are reserved for the user.  Kai Groﬂjohann
;; had the idea to chain the F-keys together, expanding the usefulness of these
;; keys.  This implementation of that concept is Ted O'Connor's, although it's
;; currently sparsely populated.

(defvar stesla-f5-prefix-map (make-sparse-keymap))
(defvar stesla-f6-prefix-map (make-sparse-keymap))
(defvar stesla-f7-prefix-map (make-sparse-keymap))
(defvar stesla-f8-prefix-map (make-sparse-keymap))
(defvar stesla-f9-prefix-map (make-sparse-keymap))

(global-set-key [f5] stesla-f5-prefix-map)
(global-set-key [f6] stesla-f6-prefix-map)
(global-set-key [f7] stesla-f7-prefix-map)
(global-set-key [f8] stesla-f8-prefix-map)
(global-set-key [f9] stesla-f9-prefix-map)

;; Also from Ted: F9 F9 records or executes a previously recorded keyboard
;; macro; F9 F10 clears a previously recorded macro; F9 F11 gives a macro a
;; permanent name.

(defun stesla-macro-dwim (arg)
  "DWIM keyboard macro recording and executing."
  (interactive "P")
  (if defining-kbd-macro
      (if arg
          (end-kbd-macro arg)
        (end-kbd-macro))
    (if last-kbd-macro
        (call-last-kbd-macro arg)
      (start-kbd-macro arg))))

(defun stesla-macro-clear ()
  "Clear last keyboard macro."
  (interactive)
  (setq last-kbd-macro nil)
  (message "Last keyboard macro cleared."))

(define-key stesla-f9-prefix-map [(f9)] 'stesla-macro-dwim)
(define-key stesla-f9-prefix-map [(f10)] 'stesla-macro-clear)
(define-key stesla-f9-prefix-map [(f11)] 'name-last-kbd-macro)

;; Martin Cracauer wrote some code which emulates the SELECT key from a
;; Symbolics Lisp machine.  This is Ted's adaptation.
;;
;; FIXME: Tweak.

(defvar stesla-select-prefix-map stesla-f8-prefix-map)
(global-set-key [menu] stesla-select-prefix-map)
(global-set-key [apps] stesla-select-prefix-map)

(defun stesla-display-select-bindings ()
  (interactive)
  (describe-bindings [f8]))

(define-key stesla-select-prefix-map "?" 'stesla-display-select-bindings)

(defmacro stesla-define-select-key (fname-base key &optional buf-form else-form)
  "Define a select-key function FNAME-BASE bound on KEY.

If provided, BUF-FORM should be a form which will attempt to return
a buffer to switch to.  If it returns nil, ELSE-FORM is evaluated."
  (let ((fname (intern (concat "stesla-select-" (symbol-name fname-base)))))
    `(progn
       (defun ,fname (arg)
         (interactive "P")
         (let ((buf ,buf-form))
           (if buf
               (switch-to-buffer buf)
             ,else-form)))
       (define-key stesla-select-prefix-map ,key ',fname))))

(put 'stesla-define-select-key 'lisp-indent-function 2)

(defmacro stesla-define-select-key-class (fname-base key extension &optional default-dir)
  `(stesla-define-select-key ,(intern (concat (symbol-name fname-base) "-file")) ,key
     (let ((buffers (buffer-list))
           (buffer t))
       (while (and buffers
                   (listp buffers))
         (setq buffer (car buffers))
         (setq buffers (cdr buffers))
         (if (string-match ,extension (buffer-name buffer))
             (setq buffers nil)
           (setq buffer nil)))
       buffer)
     (find-file
      (read-file-name ,(concat "Find " (symbol-name fname-base) " file: ")
                      ,default-dir))))

;; These are the file types I use at least semi-regularly.

(stesla-define-select-key-class C        "c" "\\.c$" "~/Projects/")
(stesla-define-select-key-class Emacs-Lisp "e" "\\.el$"     "~/.elisp/")
(stesla-define-select-key-class C-Header "h" "\\.h$" "~/Projects/")
(stesla-define-select-key-class Lisp       "l" "\\.\\(lisp\\|lsp\\)$")
(stesla-define-select-key-class LaTeX      "t" "\\.tex$")
(stesla-define-select-key-class Makefile   "M" "\\(GNU\\)?[Mm]akefile")
(stesla-define-select-key-class m4         "4" "\\.m4$")

;; For easy access to a few commonly accessed files/buffers.

(stesla-define-select-key dotemacs-file "."
  (find-buffer-visiting stesla-dotemacs-file)
  (find-file stesla-dotemacs-file))

(stesla-define-select-key home-directory "~"
  (find-buffer-visiting "~")
  (dired "~"))
;; That ~ key is impossible to type...
(define-key stesla-select-prefix-map "`" 'stesla-select-home-directory)

(stesla-define-select-key info "i"
  (find-buffer-visiting "*info*")
  (info))

(stesla-define-select-key shell "!"
  (find-buffer-visiting "*eshell*")
  (eshell))

(stesla-define-select-key gnus "g"
  (gnus))


;; I like to use uuids, and it's tedious to generate them.
(defcustom stesla-uuidgen-program "uuidgen" "Program to generate uuids")

(defun stesla-uuidgen ()
  (interactive)
  (save-excursion
    (save-restriction
      (shell-command stesla-uuidgen-program)
      (let ((buffer (current-buffer))
            (output (get-buffer "*Shell Command Output*")))
        (set-buffer output)
        (let ((start (point-min))
              (end (- (point-max) 1)))
          (set-buffer buffer)
          (insert-buffer-substring output start end))))))

(stesla-define-select-key uuid "u"
  (stesla-uuidgen))

;;;; I want a more intelligent mode selection for some files

(mapcar (lambda (mapping) (add-to-list 'auto-mode-alist mapping))
        '(("\\.dtd$" . xml-mode)
          ("\\.xml$" . xml-mode)
          ("\\.yml$" . conf-mode)
          ("bash_profile$" . sh-mode)
          ("bashrc$" . sh-mode)
          ("\\.ebuild$" . sh-mode)))

;;;; Application configuration

;;; Load the local init file for paths and whatnot

(load "~/.emacs.local" t)

;;; Code not specific to one mode

;; This is necessary for ruby-mode to define it's font-locking stuff.  I'm
;; going to define it here, though, before all of my modes.
(require-no-error 'font-lock)

;; This is a gem I found on the Ruby Garden Wiki.  It automatically saves files
;; with a she-bang as executable files.  Thanks Mark Slagell!

(add-hook 'after-save-hook
          '(lambda ()
             (progn
               (and (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (save-match-data
                          (looking-at "^#!"))))
                    (shell-command (concat "chmod u+x " buffer-file-name))
                    (message (concat "Saved as script: " buffer-file-name))
                    ))))

;;; BBDB - the Insidious Big Brother Database.

(when (require-no-error 'bbdb)
  (when (and (boundp 'coding-system-p)
             (coding-system-p 'utf-8))
    (setq bbdb-file-coding-system 'utf-8))

  ;; Ted noticed that certain versions of the BBDB don't autoload this.

  (require 'bbdb-sc)
  (bbdb-initialize 'sendmail 'gnus 'message 'sc)

  (when (locate-library "eshell")
    (defun eshell/bbdb (&optional (regex ".*"))
      (bbdb regex nil)))

  (stesla-define-select-key bbdb "b"
    (get-buffer "*BBDB*")
    (bbdb ".*" nil))

  (add-hook 'message-setup-hook 'bbdb-define-all-aliases))

(defconst stesla-bbdb-p (featurep 'bbdb))

;;; caml-mode
(when (require-no-error 'caml)
  (require-no-error 'caml-font)
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . caml-mode))
  (autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
  (define-key caml-mode-map (kbd "C-c l") 'goto-line)
  (setq inferior-caml-program "myocaml"))

;;; compile customizations.
(when (require-no-error 'compile)
  (defun stesla-compile ()
    (interactive)
    (compile compile-command))
  (global-set-key (kbd "C-c r") 'stesla-compile))

;;; cc-mode customizations.
(defconst stesla-c-style
  '((c-basic-offset . 2)
    (c-comment-only-line-offset 0 . 0)
    (c-offsets-alist (statement-block-intro . +)
                     (knr-argdecl-intro . 5)
                     (substatement-open . +)
                     (label . 0)
                     (statement-case-open . +)
                     (statement-cont . +)
                     (arglist-intro . c-lineup-arglist-intro-after-paren)
                     (arglist-close . c-lineup-arglist)
                     (inline-open . 0)
                     (brace-list-open . +))
    (c-special-indent-hook . c-gnu-impose-minimum)
    (c-block-comment-prefix . "*")
    (c-hanging-semi&comma-criteria .
     '(c-semi&comma-no-newlines-before-nonblanks))))

(defun stesla-c-mode-common-hook ()
  (c-add-style "PERSONAL" stesla-c-style t))

(add-hook 'c-mode-common-hook 'stesla-c-mode-common-hook)

;; objc-mode customizations

(defun stesla-find-xcode-project-name ()
  "Determines xcode project name.  Currently, only works in current directory."
  (let* ((current-dir (file-name-directory (buffer-file-name)))
         (glob (concat current-dir "*.xcode"))
         (project-list (file-expand-wildcards glob))
         (project-file (car project-list)))
    (string-match "/\\([^/]+\\)\\.xcode$" project-file)
    (match-string 1 project-file)))

(defun stesla-find-xcode-project-program-name ()
  "Where, oh where, is our application folder at?  This function finds the
build folder and the .app folder underneath it and returns the full path to the
executable in Contents/MacOS/ProjectName

Only works for projects where build and the project are in the current
directory.  It uses `stesla-find-xcode-project-name' to get the project name."
  (let* ((current-dir (file-name-directory (buffer-file-name)))
         (project-name (stesla-find-xcode-project-name)))
    (concat current-dir
            "build/"
            project-name
            ".app/Contents/MacOS/"
            project-name)))

(defun stesla-activate-via-applescript (program)
  (let ((command
         (concat "osascript -e 'tell application \""
                 program
                 "\"\nactivate\nend tell'")))
    (shell-command command)))

(defun stesla-run-xcode-project-program ()
  (interactive)
  (let* ((project-name (stesla-find-xcode-project-name))
         (output-buffer (get-buffer-create
                         (concat "*" project-name " Output*")))
         (program-name (stesla-find-xcode-project-program-name)))

    (pop-to-buffer output-buffer)
    (start-process project-name output-buffer program-name)
    (stesla-activate-via-applescript project-name)))


;;; chess

(when (locate-library "chess")
  (autoload 'chess "chess" nil t))


;;; css-mode.el - editing cascading style sheets.

(when (locate-library "css-mode")
  (autoload 'css-mode "css-mode" nil t)
  (autoload 'cssm-c-style-indenter "css-mode" nil nil)
  (add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
  (setq cssm-indent-function 'cssm-c-style-indenter))

;;; delphi.el - editing Delphi source

(when (require-no-error 'delphi)
  (setq delphi-indent-level 2))

;;; delphi-extra.el - Good additions for editing delphi code

(when (require-no-error 'delphi-extra))

;;; Dired - a full-featured file manager for Emacs.

;; Copy things correctly between two directories.

(setq dired-dwim-target t)

;; Confirm recursive deletes, but only on the top level.))

(setq dired-recursive-deletes 'top)

;; Advice around `find-file' for Dired - use the current directory instead of
;; the default directory when executing `find-file'.

(defadvice find-file (around dired-x-default-directory activate)
  "Happy advice around `find-file'.\n
In Dired, use dired-x.el's `default-directory' function instead of the
`default-directory' variable.
From Kevin Rodgers <kevin@ihs.com>"
  (interactive
   (let ((default-directory
           (if (and (eq major-mode 'dired-mode)
                    (fboundp 'default-directory))
               (default-directory)
             default-directory)))
     (list (read-file-name "Find file: " nil nil nil nil))))
  ad-do-it)

;;; eldoc - I found this just recently in Ted's .emacs.  This is the coolest
;;; thing ever!  It shows the syntax of the current Emacs Lisp command in the
;;; echo area.

;; (when (locate-library "eldoc")
;;   (mapc (lambda (mode-hook)
;;           (add-hook mode-hook 'turn-on-eldoc-mode))
;;         '(emacs-lisp-mode-hook lisp-interaction-mode-hook ielm-mode-hook))

;;   (when (fboundp 'propertize)
;;     (defun ted-frob-eldoc-argument-list (string)
;;       "Upcase and fontify STRING for use with `eldoc-mode'."
;;       (let ((upcased (upcase string)))
;;         (if font-lock-mode
;;             (propertize upcased
;;                         'face 'font-lock-variable-name-face)
;;           upcased)))
;;     (setq eldoc-argument-case 'ted-frob-eldoc-argument-list)))

;;; emacs-lisp-mode

(put 'shell-command-on-region 'lisp-indent-function 2)

;;; ERC - the Emacs IRC Client (don't forget .ercrc too)

(when (require-no-error 'erc)

  (defun stesla-erc-connect (server port nick full-name)
    "Connect to an IRC server without the prompts of erc-select"
    (run-hook-with-args 'erc-before-connect server port nick)
    (erc server port nick full-name t nil))


  (defun stesla-erc-join-channel (channel &optional invite)
    (when invite
      (progn
        (erc-cmd-MSG (concat "ChanServ invite " channel))
        (with-temp-message "Waiting for ChanServ..." (sleep-for 5))))
    (erc-cmd-JOIN channel))

  (defun stesla-erc-join-or-switch-with-connect (server port nick full-name
                                                        channel &optional invite)
    "If we are not currently in CHANNEL on SERVER, join it.  If we are not
currently on SERVER:PORT as NICK, connect first.  Otherwise, just switch to the
buffer"
    (let ((server-buffer (car (erc-buffer-list
                               (lambda () (and (string= server erc-session-server)
                                               (erc-server-buffer-p)
                                               (erc-process-alive)))))))
      (if server-buffer
          (let ((buffer (car (erc-buffer-list
                              (lambda () (and (string= channel (erc-default-target))
                                              (string= server erc-session-server)
                                              (erc-process-alive)))))))
            (if buffer
                (switch-to-buffer buffer)
              (progn
                (set-buffer server-buffer)
                (stesla-erc-join-channel channel invite))))
        (progn
          (stesla-erc-connect server port nick full-name)
          (with-temp-message "Waiting for NickServ..." (sleep-for 5))
          (stesla-erc-join-channel channel invite)))))

  (defvar stesla-erc-channel-map stesla-f5-prefix-map)

  (defun stesla-display-erc-channel-bindings ()
    (interactive)
    (describe-bindings [f5]))

  (define-key stesla-erc-channel-map "?" 'stesla-display-erc-channel-bindings)

  (defmacro stesla-define-erc-channel-key (fname-base key server port nick full-name channel &optional
                                              invite)
    "Define an erc-key function FNAME-BASE bound on KEY that will execute
stesla-erc-join-or-switch-with-connect on the remaining params"
    (let ((fname (intern (concat "stesla-erc-channel-" (symbol-name fname-base)))))
      `(progn
         (defun ,fname ()
           (interactive)
           (stesla-erc-join-or-switch-with-connect
            ,server ,port ,nick ,full-name ,channel ,invite))
         (define-key stesla-erc-channel-map ,key ',fname))))

  (put 'stesla-define-erc-channel-key 'lisp-indent-function 2)

  (setq stesla-irc-nick "Manwe"
        stesla-irc-full-name "Samuel Tesla")

  (stesla-define-erc-channel-key localhost-test "t"
    "localhost" "ircd" "Manwe" "Samuel Tesla" "#test")

  (stesla-define-erc-channel-key newatlantis "n"
    "irc.lucifer.com" "ircd" stesla-irc-nick stesla-irc-full-name "#newatlantis")

  (defmacro stesla-define-freenet-channel-key (symbol key channel &optional
                                                      invite)
    "I define so many channels on freenet, I don't want to have to type it in
every time"
    `(stesla-define-erc-channel-key ,symbol ,key
       "irc.freenode.net" "ircd" ,stesla-irc-nick ,stesla-irc-full-name
       ,channel ,invite))

  (stesla-define-freenet-channel-key citylights "c" "#citylights" t)
  (stesla-define-freenet-channel-key cityops "o" "#cityops" t)
  (stesla-define-freenet-channel-key citygeek "g" "#citygeek" t)
  (stesla-define-freenet-channel-key cityshadows "s" "#cityshadows")
  (stesla-define-freenet-channel-key emacs "e" "#emacs")

  (when (require-no-error 'erc-nickserv)
    (erc-nickserv-mode 1)
    (load "~/.elisp/nickserv-list.el" t)))





;;; Eshell - who needs binaries other than Emacs?

;; Make sure `eshell' is autoloaded.

(when (locate-library "eshell")
  (when (not (fboundp 'eshell))
    (autoload 'eshell "eshell" nil t))

  ;; Always save command history.

  (setq eshell-ask-to-save-history 'always)

  ;; Fix C-a to be prompt-aware.

  (add-hook 'eshell-mode-hook
            '(lambda () (local-set-key (kbd "C-a") 'eshell-bol)))

  ;; Add to the select-key list.

  (stesla-define-select-key eshell "s"
    (get-buffer "*eshell*")
    (eshell))

  ;; Set up the same prompt that I use in bash.

  (setq eshell-prompt-function
        (lambda ()
          (concat "[" (user-login-name)
                  "@" (car (split-string (system-name) "[.]"))
                  ":" (eshell/pwd) "]"
                  (if (= (user-uid) 0) "# " "$ "))))
  (setq eshell-prompt-regexp "^[^#$\n]*[#$] ")

  ;; Move to the correct place after printing the prompt.

  (add-hook 'eshell-after-prompt-hook 'eshell-bol)

  ;; Provide a sane clear command.

  (defalias 'eshell/clear 'stesla-clear)

  ;; Keep info from eating the screen when called.

  (defun eshell/info (&rest args)
    "Invoke `info' on a file."
    (if (null args)
        (info)
      (while args
      (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
          (let* ((line (string-to-number (match-string 1 (pop args))))
                 (file (pop args)))
            (info file))
        (info (pop args))))))

  ;; Kai Groﬂjohann's version of eshell/less.

  (defun eshell/less (&rest args)
    "Invoke `view-file' on a file.
\"less +42 foo\" will go to line 42 in the buffer."
    (while args
      (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
          (let* ((line (string-to-number (match-string 1 (pop args))))
                 (file (pop args)))
            (view-file file)
            (goto-line line))
        (view-file (pop args))))
    (eshell-bol))

  (defalias 'eshell/more 'eshell/less)

  ;; Ted's version of eshell/emacs.

  (defun eshell/emacs (&rest args)
    "Open a file in Emacs.  Some habits die hard."
    (if (null args)

        ;; Pretend to do what I asked.

        (switch-to-buffer "*scratch*")

      ;; The flattening is necessary if we try to open a bunch of files in many
      ;; different places in the filesystem.

      (mapc 'find-file (mapcar 'expand-file-name
                               (eshell-flatten-list args)))))

  ;; eshell/vi, based on both Ted's and Kai Groﬂjohann's.

  (defun eshell/vi (&rest args)
    "Open a file in Viper mode."
    (while args
      (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
          (let* ((line (string-to-number (match-string 1 (pop args))))
                 (file (pop args)))
            (with-current-buffer (find-file file)
              (goto-line line)))
        (find-file (pop args)))
      (setq viper-mode t)
      (viper-mode)))
  (defalias 'eshell/vim 'eshell/vi)

  ;; Make Eshell prettier in general.

  (require 'ansi-color)
  (add-hook 'eshell-preoutput-filter-functions
            'ansi-color-filter-apply)

  ;; Make the shell illusion even better by hiding the modeline.

  ;(let ((stesla-no-mode-line '(lambda ()
  ;                             (setq mode-line-format nil))))
  ;  (add-hook 'eshell-mode-hook stesla-no-mode-line)
  ;  (add-hook 'sql-interactive-mode-hook stesla-no-mode-line)
  ;  (add-hook 'term-mode-hook stesla-no-mode-line))

  ;; Make sure that mutt and other curses apps stick to ansi-term.

  (add-hook 'eshell-mode-hook
            '(lambda ()
               (add-to-list 'eshell-visual-commands "mutt")
               (add-to-list 'eshell-visual-commands "links")
               (add-to-list 'eshell-visual-commands "notes"))))

;;; igrep.el
(when (locate-library "igrep")
  (autoload 'igrep "igrep"
      "*Run `grep` PROGRAM to match REGEX in FILES..." t)
   (autoload 'igrep-find "igrep"
      "*Run `grep` via `find`..." t)
   (autoload 'igrep-visited-files "igrep"
      "*Run `grep` ... on all visited files." t)
   (autoload 'dired-do-igrep "igrep"
      "*Run `grep` on the marked (or next prefix ARG) files." t)
   (autoload 'dired-do-igrep-find "igrep"
      "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
   (autoload 'Buffer-menu-igrep "igrep"
     "*Run `grep` on the files visited in buffers marked with '>'." t)
   (autoload 'igrep-insinuate "igrep"
     "Define `grep' aliases for the corresponding `igrep' commands." t)
   (autoload 'grep "igrep"
      "*Run `grep` PROGRAM to match REGEX in FILES..." t)
   (autoload 'egrep "igrep"
      "*Run `egrep`..." t)
   (autoload 'fgrep "igrep"
      "*Run `fgrep`..." t)
   (autoload 'agrep "igrep"
      "*Run `agrep`..." t)
   (autoload 'grep-find "igrep"
      "*Run `grep` via `find`..." t)
   (autoload 'egrep-find "igrep"
      "*Run `egrep` via `find`..." t)
   (autoload 'fgrep-find "igrep"
      "*Run `fgrep` via `find`..." t)
   (autoload 'agrep-find "igrep"
      "*Run `agrep` via `find`..." t)
   (igrep-insinuate)
   (setq igrep-find-use-xargs 'gnu))

;;; ruby-mod.el - For editing ruby files.  A lot of this is from rubygarden.org
(when (require-no-error 'ruby-mode)

  (mapcar (lambda (mapping) (add-to-list 'auto-mode-alist mapping))
          '(("Rakefile" . ruby-mode)
            ("\\.rake$" . ruby-mode)
            ("\\.rb$" . ruby-mode)))

  ;; This fixes the probelm with M-q and comment.  Thanks to Jim Weirich.

  (defvar stesla-rb-para-begin-re "\\(^\\s-*#*\\s-*$\\)\\|\\(^\\s-*[^# ]\\)")

  (defun stesla-rb-goto-para-begin ()
    (search-backward-regexp stesla-rb-para-begin-re)
    (beginning-of-line)
    (next-line 1) )

  (defun stesla-rb-goto-para-end ()
    (search-forward-regexp stesla-rb-para-begin-re)
    (beginning-of-line) )

  (defun stesla-rb-fill-comment-region ()
    (interactive)
    (save-excursion
      (stesla-rb-goto-para-begin)
      (let ((start (point)))
        (stesla-rb-goto-para-end)
        (narrow-to-region start (point))
        (fill-region start (point))
        (widen)
        )
      ))

  ;; This makes M-x compile work happy for me when I'm playing in ruby.  Based
  ;; on code from Steve Molitor and the regexp is from Avdi Grimm.  Both off
  ;; the Ruby Garden wiki.

  (defun stesla-rb-compile-hook ()
    (make-variable-buffer-local 'compilation-error-regexp-alist)
    (add-to-list 'compilation-error-regexp-alist
                 '((concat "test[a-zA-Z0-9_]*([A-Z][a-zA-Z0-9_]*) "
                           "\\[\\(.*\\):\\([0-9]+\\)\\]:")
                   1 2))
    (make-variable-buffer-local 'compile-command)
    (setq compile-command "rake"))

  (add-hook 'ruby-mode-hook 'stesla-rb-compile-hook)

  (add-hook 'ruby-mode-hook
            '(lambda ()
               (make-local-variable 'font-lock-defaults)
               (setq font-lock-defaults '((ruby-font-lock-keywords) nil nil))
               (setq font-lock-keywords ruby-font-lock-keywords)
               (setq font-lock-syntactic-keywords
                     ruby-font-lock-syntactic-keywords)
               (font-lock-mode)))

  ;; Here are our key bindings for ruby-mode

  (add-hook 'ruby-mode-hook
            '(lambda ()
               (define-key ruby-mode-map (kbd "C-#") 'stesla-ruby-hash-header)
               (define-key ruby-mode-map (kbd "C-<f10>")
                 'stesla-ruby-xmp-region)
               (define-key ruby-mode-map
                 "\M-q" 'stesla-rb-fill-comment-region))))

;;; inf-ruby.el - Inferior Ruby for a command line way of running ruby code
(when (require-no-error 'inf-ruby))

;;; javascript-mode.el
(when (require-no-error 'javascript-mode)
  (add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
  (setq javascript-indent-level 2))


;;; SLIME - for editing CL

(when (require-no-error 'slime)
  (let* ((slime-dir (concat elisp-dir "/slime"))
         (core-file (concat slime-dir "/sbcl-with-slime.core")))
    (setq inferior-lisp-program (concat "sbcl --core " core-file))))

;;; SML

(when (load-no-error "sml-mode-startup")
  (defun stesla-sml-mode-hook ()
    (setq abbrev-mode nil)
    (setq sml-indent-level 2))

  (add-hook 'inferior-sml-mode-hook 'stesla-sml-mode-hook)
  (add-hook 'sml-mode-hook 'stesla-sml-mode-hook))

;;; Emacs-w3 - a browser written in elisp

(when (require-no-error 'w3))

;;; Emacs-w3m - a match made in lisp heaven.

(when (locate-library "w3m")
  (setq w3m-use-toolbar            nil
        w3m-use-tab                nil
        w3m-key-binding            'info
        w3m-search-default-engine  "google"
        w3m-default-save-directory "~"
        w3m-home-page              stesla-home-page
        w3m-mailto-url-function    (symbol-function 'compose-mail))
  (autoload 'w3m "w3m" nil t)
  (autoload 'w3m-region "w3m")
  (add-hook 'w3m-mode-hook 'stesla-hide-trailing-whitespace)
  (add-hook 'w3m-mode-hook (lambda ()
                             (local-set-key (kbd "C-x f") 'w3m-find-file)))
  (defalias 'eshell/w3m 'w3m)
  (stesla-define-select-key w3m "w" (get-buffer "*w3m*") (w3m))

  ;; Ted has misgivings about this, but it suits me just fine.

  (setq browse-url-browser-function 'w3m)

  ;; Gnus should use w3m to render its HTML text.

  (setq mm-text-html-renderer 'w3m))

;; This command allows us to follow links in regions rendered by w3-region or
;; w3m-region, such as in Gnus articles.

(defun stesla-follow-link-at-point (point)
  "Try to follow HTML link at point.
This works for links created by w3-region or w3m-region."
  (interactive "d")
  (let* ((props (text-properties-at-point))
         (w3-h-i (plist-get props 'w3-hyperlink-info))
         (w3m-h-a (plist-get props 'w3m-href-anchor)))
    (cond (w3-h-i
           (browse-url (plist-get w3-h-i :href)))
          (w3m-h-a
           (browse-url w3m-h-a))
          (t
           (error "Couldn't determine link at point.")))))

(add-hook 'gnus-article-mode-hook
          (lambda ()

            ;; `a' as in `<a href...'.

            (local-set-key (kbd "C-c a") 'stesla-follow-link-at-point)))

;;; erlang-mode
(when (and (require-no-error 'erlang-start)
           (require-no-error 'erlang))
  (setq erlang-indent-level 2)
  (let* ((mnesia-dir (concat (expand-file-name "~/.emacs.d/") "mnesia"))
         (quoted-mnesia-dir (concat "\"" mnesia-dir "\""))
         (options (list "-sname" "emacs" "-mnesia" "dir" quoted-mnesia-dir)))
    (setq inferior-erlang-machine-options options)))

;;; mt.el -- Posting to Movable Type from Emacs

(when (require-no-error 'mt)
  (setq weblog-id "2"
        weblog-url "http://www.alieniloquent.com/mt/mt-xmlrpc.cgi"
        weblog-post-count 5
        weblog-publish-on-save t)

 (global-set-key (kbd "C-c w c") 'weblog-create-post)
 (global-set-key (kbd "C-c w r") 'weblog-retrieve-recent-posts)
 (global-set-key (kbd "C-c w g") 'weblog-retrieve-post)

 (add-hook 'weblog-mode-hook 'turn-off-auto-fill)
 (add-hook 'weblog-new-post-hook 'turn-off-auto-fill))

;; tex-mode customizations

(when (require-no-error 'tex-mode)
  (defun stesla-latex-compile-command (filename)
    "Returns the rake command to build a PDF with the same basename as this file"
    (concat "rake clobber " (file-name-sans-extension filename) ".pdf"))

  (defun stesla-latex-compile-hook ()
    (make-variable-buffer-local 'compile-command)
    (setq compile-command (stesla-latex-compile-command (buffer-file-name))))

  (add-hook 'latex-mode-hook 'stesla-latex-compile-hook))


;;; GUI Emacs setup:

;; Set up default frame configuration.

(setq default-frame-alist
      '((background-mode . dark)
        ;(top . 24)
        ;(left . 0)
        ))

;; Set default colors.

;; colortheme.el from Alex Schroeder, for Emacs beautification.

(when (and stesla-use-colors-p
           (require-no-error 'color-theme))
  (add-hook 'after-init-hook 'color-theme-hober))

;; Set up menu size and finish initial setup.

(when (not stesla-xemacs-p)
  (add-to-list 'default-frame-alist
               '(wait-for-wm . nil))
  (add-to-list 'default-frame-alist
               (cons 'menu-bar-lines
                     (if stesla-use-menu-bar-p 1 0))))

(setq initial-frame-alist default-frame-alist)

;; Set up appearances for the frame.

(when stesla-display-graphic-p
  (setq frame-title-format
        (concat "%b - " stesla-emacs-pretty-name))

  ;; Cursor type.

  (setq-default cursor-type 'block)

  ;; Turn off scrollbars.

  (cond (stesla-xemacs-p
         (setq scrollbars-visible-p nil)
         (set-specifier horizontal-scrollbar-visible-p nil)
         (set-specifier vertical-scrollbar-visible-p nil))
        (t (when (fboundp 'scroll-bar-mode)
             (scroll-bar-mode -1))))

  ;; Fix scrollbars in new frames in NTEmacs.

  (when (and stesla-w32-window-system-p
             stesla-gnu-emacs-p
             stesla-emacs-21-or-later-p)
    (add-hook 'after-make-frame-functions
              '(lambda (frame)
                 (scroll-bar-mode -1)))))

;; Turn off toolbars in XEmacs and GNU Emacs 21.

(when stesla-display-graphic-p
  (cond (stesla-xemacs-p
         (setq toolbar-visible-p nil)
         (set-specifier default-toolbar-visible-p nil))
        ((and stesla-gnu-emacs-p stesla-emacs-21-or-later-p)
         (tool-bar-mode -1)
         (add-to-list 'default-frame-alist
                      '(tool-bar-lines . 0)))))

;; Remove the default gutter in XEmacs.

(when stesla-xemacs-p
  (when (boundp 'default-gutter-visible-p)
    (set-specifier default-gutter-visible-p nil))

  ;; And make the progress bar go away.

  (setq progress-feedback-use-echo-area t))

;; Customizations for Emacs 19.

(when (and stesla-display-graphic-p stesla-emacs-19-p) ; FIXME? - Ted
  (set-background-color "black")
  (set-foreground-color "#c0c0c0")
  (set-cursor-color "blue")
  (setq font-lock-support-mode '((t .lazy-lock-mode)))
  (setq font-lock-maximum-decoration t))

;; Something like vi's ~ characters...

(setq default-indicate-empty-lines t)

;; ...but not in every mode.

(let ((hook '(lambda ()
               (setq indicate-empty-lines nil)))
      (mode-hooks (list 'shell-mode-hook
                        'term-mode-hook
                        'gnus-article-mode-hook
                        'gnus-summary-mode-hook
                        'gnus-group-mode-hook
                        'erc-mode-hook
                        'eshell-mode-hook)))

  (mapc (lambda (mode-hook)
          (add-hook mode-hook hook))
        mode-hooks))

;; Handle ANSI color sequences correctly.

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Be able to open images.

(when (and (fboundp 'display-images-p)
           (display-images-p)
           (fboundp 'auto-image-file-mode))
  (auto-image-file-mode 1))

;; Configure Tooltips in Emacs 21.

(when (featurep 'tooltip)
  (setq tooltip-gud-tips-p t))

;; Font-locking
;;
;; Wisdom from Eli Zaretskii (via Ted O'Connor's .emacs via Jean-Phillipe
;; Theberge's .emacs):
;;
;; "I think it is generally a bad idea to load font-lock directly from .emacs,
;; especially if you do that before setting `default-frame-alist'.  It is
;; better to do it from a major-mode-specific hook.  If you want to turn on
;; `global-font-lock-mode', do it from a function that is on the
;; `after-init-hook' list.  Emacs calls `after-init-hook' after it reads
;; .emacs, so when font-lock is loaded, `default-frame-alist' is already set."

(when stesla-use-colors-p
  (cond
   (stesla-emacs-19-p
    (setq hilit-mode-enable-list  '(not text-mode)
          hilit-background-mode   'dark
          hilit-inhibit-hooks     nil
          hilit-inhibit-rebinding nil)
    (add-hook 'after-init-hook
              (lambda () (require 'hilit19))))
   (stesla-gnu-emacs-p
    (if stesla-emacs-20-p
        (when (fboundp 'toggle-global-lazy-font-lock-mode)
          (add-hook 'after-init-hook
                    (lambda () (toggle-global-lazy-font-lock-mode))))
      (when (fboundp 'global-font-lock-mode)
        (add-hook 'after-init-hook
                  (lambda () (global-font-lock-mode 1))))))

   ;; Make XEmacs highlight lisp interaction mode.

   (stesla-xemacs-p
    (add-hook 'list-interaction-mode-hook
              '(lambda ()
                 (font-lock-mode 1)))))

  ;; Font lock additions.

  (put 'font-lock-add-keywords 'lisp-indent-function 1)

  (when (fboundp 'font-lock-add-keywords)

    ;; C mode font lock additions/

    (font-lock-add-keywords 'c-mode
      '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
        ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)))))


;; X-specific configuration.

(when stesla-x-window-system-p
  (setq focus-follows-mouse t)

  ;; Don't echo passwords when in comint.

  (add-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt))

;;; Provide for using multiple Emacsen and boxen:

;; Prepare the customization system for this Emacs

(when (featurep 'custom)
  (setq custom-file
        (concat "~/.elisp/" stesla-emacs-name "-custom.el"))

  ;; Load it, if it's there.

  (load custom-file t))

;;; .emacs ends here
