(require 'ag)

(require 'wgrep-ag)
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)

(define-key ag-mode-map (kbd "r") 'wgrep-change-to-wgrep-mode)
