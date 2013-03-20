;; emacsの標準機能に関係する設定をまとめる

;; kill-regionにおいて、リージョンが選択されていない場合には
;; backward-kill-wardを実行するように。
(defadvice kill-region (around kill-word-or-kill-region activate)
  (if (and (interactive-p) transient-mark-mode (not mark-active))
      (backward-kill-word 1)
    ad-do-it))

;; kill-lineの際に、次の行の行頭に連続している空白を削除する
(defadvice kill-line (before kill-line-and-fixup activate)
  (when (and (not (bolp)) (eolp))
    (forward-char)
    (fixup-whitespace)
    (backward-char)))

(defun kill-word-at-point ()
  "delete word at under cursor. If spaces was under the cursor, delete horizontal spaces"
  (interactive)
  (let ((char (char-to-string (char-after (point)))))
    (cond
     ((string= " " char) (delete-horizontal-space))
     ((string-match "[\t\n -@\[-`{-~]" char) (kill-word 1))
     (t (forward-char) (backward-word) (kill-word 1)))))

;; (@* "root所有のファイルを開く際に、sudoで開き直すか聞く")
(defun file-root-p (filename)
  "Return t if file FILENAME created by root."
  (eq 0 (nth 2 (file-attributes filename))))

;; trampで開いたファイルについては、バッファ名を変更する
(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

;; find-fileした際にも、sudo権限が必要なようなら聞くようにする
(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (file-root-p (ad-get-arg 0))
           (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

;; sudo でファイルを開く
(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

;; find-file-hookの設定
(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

;; (@* "emacsclientを利用して、外部から実行できるようにしておく")
(require 'server)
(unless (server-running-p)
  (server-start))

(require 'eldoc)

(require 'woman)
(add-to-list 'woman-manpath (expand-file-name "~/work/tool/opengl"))
(add-to-list 'woman-path (expand-file-name "~/work/tool/opengl"))
