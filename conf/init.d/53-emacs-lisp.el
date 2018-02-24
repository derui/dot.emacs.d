(eval-when-compile
  (require 'use-package))

;;; (@* "elisp関係の設定")
;; 保存された場合に、自動的にバイトコンパイルを行うための設定
;; from rubikitch
(require 'elisp-mode)

(use-package auto-async-byte-compile
  :commands (enable-auto-async-byte-compile-mode))

(use-package company
  :commands (company-mode-on))

(use-package eldoc
  :ensure nil
  :commands (eldoc-mode)
  :config
  ;; idle時にdelayをかけない
  (setq eldoc-idle-delay 0)
  ;; echo areaに複数行表示を有効にする
  (setq eldoc-echo-area-use-multiline-p t)

  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode))

(use-package evil-cleverparens
  :commands (evil-cleverparens-mode))

(use-package aggressive-indent
  :commands (aggressive-indent-mode))

(defun my:emacs-lisp-hooks ()
  (setq-local company-idle-delay 0.2)
  (setq-local company-backends '(company-semantic company-files company-elisp))
  (setq-local show-paren-style 'expression)

  (aggressive-indent-mode)
  (evil-cleverparens-mode)
  (enable-auto-async-byte-compile-mode)
  (company-mode-on)
  (eldoc-mode)
  (set-newline-and-indent))

(add-hook 'emacs-lisp-mode-hook 'my:emacs-lisp-hooks)
