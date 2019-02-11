(eval-when-compile
  (require 'use-package))
;; eshellも含めた、各種shellについての設定を記述する

;; 実行するShellはzsh
(setq explicit-shell-file-name "/bin/zsh")
(setq shell-file-name "/bin/zsh")
(setq shell-command-switch "-c")
(setenv "PATH" (concat (expand-file-name "~/bin:") (getenv "PATH")))
(setenv "EMACS" "t")

;; zsh からコマンドラインを編集する際に実行する。
(defun zsh-edit-command-line-hook ()
  (when (string-match "^zshecl" (buffer-name))
   ; (skk-mode 1)
    ))
(add-hook 'find-file-hook 'zsh-edit-command-line-hook)

(use-package fish-mode
  :ensure t
  :mode (("\\.fish$" . fish-mode)))
