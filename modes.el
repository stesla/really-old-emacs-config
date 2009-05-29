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

;;; ido-mode
;; Gives us fancy tab completion and whatnot. iswitchb is dead, long live ido.

(when (require-no-error 'ido)
  (ido-mode t)
  (setq ido-enable-flex-matching t)

  (add-hook
   'ido-setup-hook
   (lambda ()
     (define-key ido-completion-map " " 'ido-next-match))))

;;; line numbers
(when (require-no-error 'linum)
  (global-linum-mode 1))

;;; stesla-select-mode
;; Bind [f8] to a keymap for summoning commonly used buffers.

(when (require-no-error 'stesla-select)
  (stesla-select-define-key dotemacs-file "."
			    (find-buffer-visiting stesla-dotemacs-file)
			    (find-file stesla-dotemacs-file))

  (stesla-select-define-key home-directory "~"
			    (find-buffer-visiting "~")
			    (dired "~"))
  (stesla-select-map-key "`" stesla-select-home-directory)

  (stesla-select-define-key info "i"
			    (find-buffer-visiting "*info*")
			    (info))

  (stesla-select-define-key shell "!"
			    (find-buffer-visiting "*eshell*")
			    (eshell)))

;;; tex-mode customizations

(when (require-no-error 'tex-mode)
  (defun stesla-latex-compile-command (filename)
    "Returns the rake command to build a PDF with the same basename as this file"
    (concat "rake clobber " (file-name-sans-extension filename) ".pdf"))

  (defun stesla-latex-compile-hook ()
    (make-variable-buffer-local 'compile-command)
    (setq compile-command (stesla-latex-compile-command (buffer-file-name))))

  (add-hook 'latex-mode-hook 'stesla-latex-compile-hook))

;;; uniquify
(when (require-no-error 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward))
