;;; init.el -- Emacs Initialization

;; Copyright (C) 2009 Samuel Tesla

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
;;;;

;; do this first so if something else breaks, at least I can stand to
;; look at my editor.
(setq inhibit-startup-screen t)
(setq transient-mark-mode t)
(setq visible-bell t)
(setq kill-whole-line t)
(set-face-background 'default "black")
(set-face-foreground 'default "white")
(set-cursor-color "white")
(global-font-lock-mode 1)
(setq confirm-kill-emacs 'yes-or-no-p)
(random t)                              ; reseed
;(server-start)
(global-auto-revert-mode 1)

(setq make-backup-files t)
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))

(add-to-list 'load-path "~/.emacs.d/elisp")

;; Martin Cracauer wrote some code which emulates the SELECT key from a
;; Symbolics Lisp machine.  This is Ted's adaptation.

(defvar stesla-select-prefix-map (make-sparse-keymap))
(defvar stesla-select-prefix [f8])

(defun stesla-select-set-prefix (prefix)
  (global-unset-key stesla-select-prefix)
  (setq stesla-select-prefix prefix)
  (global-set-key stesla-select-prefix stesla-select-prefix-map))
(stesla-select-set-prefix stesla-select-prefix)

(defun stesla-select-display-bindings ()
  (interactive)
  (describe-bindings stesla-select-prefix))

(define-key stesla-select-prefix-map "?" 'stesla-select-display-bindings)


(defmacro stesla-select-define-key (fname-base key &optional buf-form else-form)
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

(put 'stesla-select-define-key 'lisp-indent-function 2)

;; For easy access to a few commonly accessed files/buffers.

(defconst stesla-dotemacs-file "~/.emacs.d/init.el")
(stesla-select-define-key dotemacs-file "."
  (find-buffer-visiting stesla-dotemacs-file)
  (find-file stesla-dotemacs-file))

(stesla-select-define-key home-directory "~"
  (find-buffer-visiting "~")
  (dired "~"))
;; That ~ key is impossible to type...
(define-key stesla-select-prefix-map "`" 'stesla-select-home-directory)

(stesla-select-define-key info "i"
  (find-buffer-visiting "*info*")
  (info))

(stesla-select-define-key shell "!"
  (find-buffer-visiting "*eshell*")
  (eshell))

;; Default some files to built-in modes
(mapcar (lambda (mapping) (add-to-list 'auto-mode-alist mapping))
        '(("\\.dtd$" . xml-mode)
          ("\\.ebuild$" . sh-mode)
          ("\\.xml$" . xml-mode)
          ("\\.yml$" . conf-mode)
          ("bash_profile$" . sh-mode)
          ("bashrc$" . sh-mode)))

(if (not (fboundp 'when))
    (progn
      (defmacro when (test &rest body)
    "If TEST is non-nil, evaluate the forms in BODY.
If TEST is nil, return nil."
    `(if ,test
         (progn ,@body)
       nil))))

(defun require-no-error (package)
  "This is Ted O'Connor's non-erroring version of (require PACKAGE)."
  (condition-case nil (require package) (error nil)))

;;; Applications

;; This is necessary for ruby-mode to define it's font-locking stuff.  I'm
;; going to define it here, though, before all of my modes.
(require-no-error 'font-lock)

;;; compile
(when (require-no-error 'compile)
  (defun stesla-compile ()
    (interactive)
    (compile compile-command))
  (global-set-key (kbd "C-c r") 'stesla-compile))

;;; cc-mode
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

;;; dired-mode

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

;;; emacs-lisp-mode

(put 'shell-command-on-region 'lisp-indent-function 2)

;; tex-mode customizations

(when (require-no-error 'tex-mode)
  (defun stesla-latex-compile-command (filename)
    "Returns the rake command to build a PDF with the same basename as this file"
    (concat "rake clobber " (file-name-sans-extension filename) ".pdf"))

  (defun stesla-latex-compile-hook ()
    (make-variable-buffer-local 'compile-command)
    (setq compile-command (stesla-latex-compile-command (buffer-file-name))))

  (add-hook 'latex-mode-hook 'stesla-latex-compile-hook))

;;; Appearance

;; color-theme

(when (require-no-error 'color-theme)
  (add-hook 'after-init-hook 'color-theme-hober))

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

;; Customizations local to this computer
(load "~/.emacs.local" t)