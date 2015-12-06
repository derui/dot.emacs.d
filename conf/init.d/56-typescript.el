(require 'typescript)

(autoload 'typescript-mode "TypeScript" nil t)

(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

(require 'tide)
(eval-after-load 'tide
  '(progn
     (require 'flycheck-typescript-tslint)
     (flycheck-add-next-checker 'typescript-tide
                                'typescript-tslint 'append)))

(defun my:typescript-mode-hook ()
  (setq typescript-indent-level 2)

  (tide-setup)
  (flycheck-mode t)
  ;; (eldoc-mode t)
  (company-mode-on))

(add-hook 'typescript-mode-hook 'my:typescript-mode-hook)
