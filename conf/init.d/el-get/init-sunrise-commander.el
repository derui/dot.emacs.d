(require 'sunrise-commander)
(global-set-key (kbd "C-x C-d") 'sunrise)

;; (auto-install-from-emacswiki "sunrise-x-tabs.el")
;; (require 'sunrise-x-tabs)

;; sunrise-commanderのdirectoryFaceが目にやさしくないため、diredとほぼ同様に
;; なるように調整
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sr-directory-face ((t (:foreground "cornflower blue" :weight bold)))))
