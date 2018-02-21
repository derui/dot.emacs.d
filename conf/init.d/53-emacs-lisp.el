(eval-when-compile
  (require 'use-package))

;;; (@* "elisp関係の設定")
;; 保存された場合に、自動的にバイトコンパイルを行うための設定
;; from rubikitch

(use-package auto-async-byte-compile
  :commands (enable-auto-async-byte-compile-mode))

(use-package company
  :commands (company-mode)
  :config
  (add-to-list 'company-backends 'company-elisp))

(use-package eldoc-extension
  :config
  (progn
    ;; idle時にdelayをかけない
    (setq eldoc-idle-delay 0)
    ;; echo areaに複数行表示を有効にする
    (setq eldoc-echo-area-use-multiline-p t)

    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
    ))

(defun my:emacs-lisp-hooks ()
  (make-local-variable 'company-idle-delay)
  (setq company-idle-delay 0.2)

  (enable-auto-async-byte-compile-mode)
  (company-mode 1)
  (eldoc-mode 1)
  (set-newline-and-indent))

(add-hook 'emacs-lisp-mode-hook 'my:emacs-lisp-hooks)
