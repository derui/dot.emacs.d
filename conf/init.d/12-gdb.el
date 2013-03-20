;; (@* "GDB関連の設定")
;; 有用な複数のウィンドウを開く。
(setq gdb-many-windows t)

;; IOバッファを表示する。
(setq gdb-use-separate-io-buffer t)

;; gdbを複数のwindowで開く場合、次のアドバイスを有効にする。
(when gdb-many-windows
  (defvar my:gud-window-configuration nil)

  (defun my:gdb-print-variable (&rest ARG)
    "gdbのバッファ中で入力された式をprintする。
printする際、数値.という表現があった場合、$数値.に変換する。"

    (interactive "*")
    (when (eql major-mode 'gud-mode)
      (let ((buf (buffer-substring-no-properties
                  (save-excursion
                    ;; 最初の空白まで移動する
                    (search-backward-regexp "[ \r\n\t]" nil t)
                    (+ 1 (point)))
                  (save-excursion
                    (search-forward-regexp "[ \r\n\t]" nil t)
                    (point)))))
        ;; buf内部で調査する。
        (delete-region (save-excursion
                         (comint-goto-process-mark)
                         (point))
                       (save-excursion
                         (end-of-line)
                         (point)))
        (comint-goto-process-mark)
        (message "%s"
                 (concat "p "
                         (replace-regexp-in-string "[^$]\\([0-9]+\\)\\.?"
                                                   (lambda (text)
                                                     (concat "$" text)) buf)))
        (insert (concat "p "
                        (replace-regexp-in-string "[^$]\\([0-9]+\\)\\.?"
                                                  (lambda (text)
                                                    (concat "$" text)) buf)))
        (end-of-line)
        (comint-send-input))
      ))

  (add-hook 'gud-mode-hook
            '(lambda ()
               (define-key gud-mode-map (kbd "C-c C-p") 'my:gdb-print-variable)
               (setq my:gud-window-configuration (current-window-configuration))))

  (add-hook 'kill-buffer-hook
            (lambda ()
              (when (string-match " \*gud-.+" (buffer-name (current-buffer)))
                ;; gud-関係の場合
                (ad-deactivate gdb-view-only-change-buffer)
                (when (window-configuration-p my:gud-window-configuration)
                  (set-window-configuration my:gud-window-configuration)
                  (setq my:gud-window-configuration nil)))))
  )
