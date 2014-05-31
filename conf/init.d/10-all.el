(require 'all)
(require 'all-ext)
(require 'view)
(defun all-mode-quit ()
  (interactive)
  (view-mode 1) (View-quit))

(define-key all-mode-map (kbd "C-c C-q") 'all-mode-quit)
