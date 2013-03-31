(require 'eldoc-extension)
;; idle時にdelayをかけない
(setq eldoc-idle-delay 0)
;; echo areaに複数行表示を有効にする
(setq eldoc-echo-area-use-multiline-p t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
