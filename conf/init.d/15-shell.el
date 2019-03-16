(eval-when-compile
  (require 'use-package))
;; eshellも含めた、各種shellについての設定を記述する

(require 'shell)

(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "/bin/bash")
(setq shell-command-switch "-c")
(setenv "PATH" (concat (expand-file-name "~/bin:") (getenv "PATH")))
(setenv "EMACS" "t")

(use-package fish-mode
  :mode (("\\.fish$" . fish-mode)))
