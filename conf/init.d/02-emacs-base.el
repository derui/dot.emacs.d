(eval-when-compile
  (require 'use-package))
;; emacsの標準機能に関係する設定をまとめる


;; kill-regionにおいて、リージョンが選択されていない場合には
;; backward-kill-wardを実行するように。
(defun my:kill-word-or-kill-region (f &rest args)
  (if (and (called-interactively-p 'interactive) transient-mark-mode (not mark-active))
      (backward-kill-word 1)
    (apply f args)))

(advice-add 'kill-region :around 'my:kill-word-or-kill-region)

;; kill-lineの際に、次の行の行頭に連続している空白を削除する
(defun my:kill-line-and-fixup (f &rest args)
  (when (and (not (bolp)) (eolp))
    (forward-char)
    (fixup-whitespace)
    (backward-char)))

(advice-add 'kill-line :before #'my:kill-line-and-fixup)

(defun my:kill-word-at-point ()
  "delete word at under cursor. If spaces was under the cursor, delete horizontal spaces"
  (interactive)
  (let ((char (char-to-string (char-after (point)))))
    (cond
     ((string= " " char) (delete-horizontal-space))
     ((string-match "[\t\n -@\[-`{-~]" char) (kill-word 1))
     (t (forward-char) (backward-word) (kill-word 1)))))

;; (@* "root所有のファイルを開く際に、sudoで開き直すか聞く")
(defun my:file-root-p (filename)
  "Return t if file FILENAME created by root."
  (eq 0 (nth 2 (file-attributes filename))))

;; trampで開いたファイルについては、バッファ名を変更する
(defun my:th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

;; sudo でファイルを開く
(defun my:th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

;; find-file-hookの設定
(add-hook 'find-file-hook #'my:th-rename-tramp-buffer)

;; (@* "emacsclientを利用して、外部から実行できるようにしておく")
(use-package server
  :commands (server-running-p)
  :config
  (unless (server-running-p)
    (server-start)))

(use-package eldoc)
(use-package woman
  :config
  (add-to-list 'woman-manpath (expand-file-name "~/work/tool/opengl"))
  (add-to-list 'woman-path (expand-file-name "~/work/tool/opengl")))

(use-package beacon
  :commands (beacon-mode)
  :custom
  (beacon-color "yellow")
  :config
  (beacon-mode 1))

(use-package imenu-list
  :custom
  (imenu-list-size 0.25)
  (imenu-list-focus-after-activation t)
  :hook ((after-init . imenu-list-minor-mode)))
