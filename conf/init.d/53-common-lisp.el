(eval-when-compile
  (require 'use-package))

;;; (@* "common lisp関係の設定")
(use-package company
  :commands (company-mode-on))

(use-package evil-cleverparens
  :commands (evil-cleverparens-mode))

(use-package aggressive-indent
  :commands (aggressive-indent-mode))

(use-package lisp-mode
  :ensure nil
  :config
  (defun my:lisp-hooks ()
    (setq-local company-idle-delay 0.2)
    (setq-local show-paren-style 'expression)

    (aggressive-indent-mode)
    (evil-cleverparens-mode)
    (company-mode-on)
    (set-newline-and-indent))

  (add-hook 'lisp-mode-hook 'my:lisp-hooks))

(let ((helper (expand-file-name "helper.el" my:roswell-path)))
  (when (file-exists-p helper)
    (defvar roswell-slime-contribs '(slime slime-fancy slime-company))
    (load helper)))
