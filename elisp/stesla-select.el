(defvar stesla-select-enable-minor-mode t
  "Whether or not to enable the minor mode in a new buffer by default.
Default is t.

To disable, (setq stesla-select-enable-minor-mode nil) before requiring 'stesla-select.")

(defvar *stesla-select-prefix-map* (make-sparse-keymap))
(define-key *stesla-select-prefix-map* "?" 'stesla-select-display-bindings)

(defun stesla-select-display-bindings ()
  (interactive)
  (describe-bindings stesla-select-prefix))

(defmacro stesla-select-map-key (key function)
  "Define KEY to run FUNCTION"
  `(define-key *stesla-select-prefix-map* ,key ',function))

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
       (define-key *stesla-select-prefix-map* ,key ',fname))))

(put 'stesla-select-define-key 'lisp-indent-function 2)


(defvar *stesla-select-keymap* (make-sparse-keymap))
(define-key *stesla-select-keymap* [f8] *stesla-select-prefix-map*)
(define-minor-mode stesla-select-mode
  "Stesla-Select minor mode. Keybindings to switch to common buffers."
  t
  " select"
  *stesla-select-keymap*)

(provide 'stesla-select)
