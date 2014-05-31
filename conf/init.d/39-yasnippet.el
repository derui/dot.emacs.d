;; (@* "yasnippetを利用する設定")
(require 'yasnippet)
(yas/load-directory (locate-user-emacs-file "etc/snippets"))
(yas-global-mode 1)

(setq yas/trigger-key "SPC")
(setq yas/use-menu nil)

(defun yas/expand-link (key)
  "Hyperlink function for yasnippet expansion."
  (delete-region (point-at-bol) (1+ (point-at-eol)))
  (insert key)
  (yas/expand))

(defun yas/expand-link-choice (&rest keys)
  "Hyperlink to select yasnippet template."
  (yas/expand-link (completing-read "Select template: " keys nil t)))
