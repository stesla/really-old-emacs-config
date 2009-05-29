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
                        'eshell-mode-hook)))

  (mapc (lambda (mode-hook)
          (add-hook mode-hook hook))
        mode-hooks))

;; No tabs, only spaces
(setq-default indent-tabs-mode nil)

;; Show line and column numbers in modeline
(line-number-mode t)
(column-number-mode t)

;; 'y' for 'yes', 'n' for 'no'
(fset 'yes-or-no-p 'y-or-n-p)

; don't iconify on C-z when running in X
(when window-system (global-unset-key "\C-z"))

;; smooth scrolling
(setq scroll-step 3)
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 500)
(setq next-screen-context-lines 1)
(setq automatic-hscrolling 'nil)
