;; aspell がなければ起動しないことにする

(when (executable-find "aspell")
  (require 'ac-ispell)
  (custom-set-variables '(ac-ispell-requires 4))
  (eval-after-load "auto-complete"
    '(progn
       (ac-ispell-setup)))

  (let ((hooks '(git-commit-mode-hook emacs-lisp-mode-hook)))
    (cl-mapc #'(lambda (hook) (add-hook hook 'ac-ispell-ac-setup)) hooks)))

