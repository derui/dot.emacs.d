(require 'swoop)

(defvar swoop-migemo-options
  "-q -e -d /usr/local/share/migemo/utf-8/migemo-dict")
(setq swoop-font-size-change: nil)
(define-key evil-motion-state-map (kbd "C-o") 'swoop-from-evil-search)
