(require 'ac-typescript)
(require 'typescript)
(autoload 'typescript-mode "TypeScript" nil t)

(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

(setq ac-typescript-server/isense-location
      (locate-user-emacs-file "conf/site-lisp/ac-typescript/bin/isense.js"))

(ac-typescript/start-server)
(add-hook 'typescript-mode-hook 'ac-typescript/ac-enable)
