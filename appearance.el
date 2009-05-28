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
