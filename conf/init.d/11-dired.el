(require 'dired)
(require 'wdired)

;; (@> "dired関連のキーバインド設定")
;; m でマークして T で一括変換
(define-key dired-mode-map (kbd "T") 'my:dired-do-convert-coding-system)
;; 自由にリネームを行えるようにする
(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)
;; 指定したファイルから、音声ファイルを抽出する
(define-key dired-mode-map (kbd "e") 'my:ffmpeg-extract-audio)

;; (@> "dired-find-alternate-fileを有効にする")
(put 'dired-find-alternate-file 'disabled nil)

;; (@> "今日更新されたファイルについて、当日の日付で記述する")
(defface my-face-f-2 '((t (:foreground "GreenYellow"))) nil)
(defvar my-face-f-2 'my-face-f-2)
(defun my-dired-today-search (arg)
  "Fontlock search function for dired."
  (search-forward-regexp
   (concat (format-time-string "%Y-%m-%d" (current-time)) " [0-9]....") arg t))

(add-hook 'dired-mode-hook
          '(lambda ()
             (font-lock-add-keywords
              major-mode
              (list
               '(my-dired-today-search . my-face-f-2)
            ))))

;; (@> "diredで表示を行う際のソート順を設定する")
(defun ls-lisp-handle-switches (file-alist switches)
  ;; FILE-ALIST's elements are (FILE . FILE-ATTRIBUTES).
  ;; Return new alist sorted according to SWITCHES which is a list of
  ;; characters.  Default sorting is alphabetically.
  (let (index)
    (setq file-alist
          (sort file-alist
                (cond
                 ((memq ?S switches)    ; sorted on size
                  (function
                   (lambda (x y)
                     ;; 7th file attribute is file size
                     ;; Make largest file come first
                     (if (equal (nth 0 (cdr y))
                                (nth 0 (cdr x)))
                         (< (nth 7 (cdr y))
                            (nth 7 (cdr x)))
                       (nth 0 (cdr x))))))
                 ((memq ?t switches)    ; sorted on time
                  (setq index (ls-lisp-time-index switches))
                  (function
                   (lambda (x y)
                     (if (equal (nth 0 (cdr y))
                                (nth 0 (cdr x)))
                         (ls-lisp-time-lessp (nth index (cdr y))
                                             (nth index (cdr x)))
                       (nth 0 (cdr x))
                       ))))
                 ((memq ?X switches)    ; sorted on ext
                  (function
                   (lambda (x y)
                     (if (equal (nth 0 (cdr y))
                                (nth 0 (cdr x)))
                         (string-lessp (file-name-extension (upcase (car x)))
                                       (file-name-extension (upcase (car y))))
                       (nth 0 (cdr x))))))
                 (t                     ; sorted alphabetically
                  (if ls-lisp-dired-ignore-case
                      (function
                       (lambda (x y)
                         (if (equal (nth 0 (cdr y))
                                    (nth 0 (cdr x)))
                             (string-lessp (upcase (car x))
                                           (upcase (car y)))
                           (nth 0 (cdr x)))))
                    (function
                     (lambda (x y)
                       (if (equal (nth 0 (cdr y))
                                  (nth 0 (cdr x)))
                           (string-lessp (car x)
                                         (car y))
                         (nth 0 (cdr x)))))
                    )))))
    )

  (if (memq ?r switches)                ; reverse sort order
      (setq file-alist (nreverse file-alist)))
  file-alist)

;; (@> "dired を使って、一気にファイルの coding system (漢字) を変換する")
(defvar my:dired-default-file-coding-system nil
  "*Default coding system for converting file (s).")

(defvar my:dired-file-coding-system 'no-conversion)

;; 現在diredで選択されているファイルに対して、文字コードを変換する。
(defun my:dired-convert-coding-system ()
  (let ((file (dired-get-filename))
        (coding-system-for-write my:dired-file-coding-system)
        failure)
    (condition-case err
        (with-temp-buffer
          (insert-file file)
          (write-region (point-min) (point-max) file))
      (error (setq failure err)))
    (if (not failure)
        nil
      (dired-log "convert coding system error for %s:\n%s\n" file failure)
      (dired-make-relative file))))

;; マークされたファイルに対して、my:dired-convert-coding-systemを実行する
(defun my:dired-do-convert-coding-system (coding-system &optional arg)
  "Convert file (s) in specified coding system."
  (interactive
   (list (let ((default buffer-file-coding-system))
           (read-coding-system
            (format "Coding system for converting file (s) (default, %s): "
                    default)
            default))
         current-prefix-arg))
  (check-coding-system coding-system)
  (setq my:dired-file-coding-system coding-system)
  (dired-map-over-marks-check
   (function my:dired-convert-coding-system) arg 'convert-coding-system t))

;; (@> "常に再帰的にディレクトリの削除/コピーを行なうようにする")
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

;; dired上から指定した動画から音声のみを抽出する。
(defun my:ffmpeg-extract-audio ()
  (interactive)
  (let* ((buffer (get-buffer-create " *Output*"))
         (ext (file-name-extension (dired-get-filename t)))
         (extract-ext (if (string= "flv" ext) "mp3" "aac"))
         (basename (url-file-extension (dired-get-filename) t)))
    (call-process-shell-command
     (format "/usr/bin/ffmpeg -i \"%s\" -acodec copy \"%s\""
             (dired-get-filename t) (concat basename "." extract-ext)) nil nil t))
  (message (format "extract completed %s" (url-file-extension (dired-get-filename) t)))
  )
