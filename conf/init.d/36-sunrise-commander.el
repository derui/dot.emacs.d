(require 'sunrise-commander)
(global-set-key (kbd "C-x C-d") 'sunrise)

(define-key sr-mode-map (kbd "<backspace>") 'sr-dired-prev-subdir)
