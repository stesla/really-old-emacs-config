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
(show-paren-mode 1)
(setq-default show-trailing-whitespace t)

(setq make-backup-files t)
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))

(defconst stesla-dotemacs-file "~/.emacs.d/init.el")

(defun require-no-error (package)
  "This is Ted O'Connor's non-erroring version of (require PACKAGE)."
  (condition-case nil (require package) (error nil)))

(add-to-list 'load-path "~/.emacs.d/elisp")

(load "~/.emacs.d/appearance.el")
(load "~/.emacs.d/modes.el")
(load "~/.emacs.d/local.el" t)

(mapcar (lambda (mapping) (add-to-list 'auto-mode-alist mapping))
        '(("Rakefile" . ruby-mode)
          ("\\.ebuild$" . sh-mode)
          ("\\.rake$" . ruby-mode)
          ("\\.ru$" . ruby-mode)
          ("\\.xml$" . xml-mode)
          ("\\.yml$" . conf-mode)
          ("bash_profile$" . sh-mode)
          ("bashrc$" . sh-mode)
          ("\\.dtd$" . xml-mode)))

(global-set-key (kbd "C-c l") 'goto-line)

(defun stesla-hide-trailing-whitespace ()
  "Turn off trailing whitespace highlighting in this buffer."
  (interactive)
  (setq show-trailing-whitespace nil))

(mapc (lambda (mode-hook)
        (add-hook mode-hook 'stesla-hide-trailing-whitespace))
      '(Buffer-menu-mode-hook
        custom-mode-hook term-mode-hook Info-mode-hook
        comint-mode-hook buffer-menu-mode-hook apropos-mode-hook
        tooltip-show-hook gnus-article-mode-hook mail-mode-hook
        gnus-summary-mode-hook message-mode-hook gnus-group-mode-hook
        eshell-mode-hook w3-mode-hook w3m-mode-hook help-modeq))
