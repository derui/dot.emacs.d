;; helmを有効にするための設定を行う。
(require 'helm-config)
(require 'helm-buffers)
(require 'helm-files)

(defun my:helm ()
  "Helm command for you."
  (interactive)
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (helm-other-buffer
     '(helm-source-buffers-list
       helm-source-recentf
       helm-source-files-in-current-dir
       helm-source-bookmarks
       helm-source-buffer-not-found)
     " *helm*"))

;; ディレイは0.2秒
(setq helm-input-idle-delay 0.02)

;; 候補のディレクトリが一つしかない場合に、自動的に展開しない
(setq helm-ff-auto-update-initial-value nil)

;; 検索文字列を入力しおわってから検索に入る。間隔は0.01秒にしておく。
(setq helm-idle-delay 0.01)
(defadvice helm-check-minibuffer-input (around sit-for activate)
  (if (sit-for helm-idle-delay t)
      ad-do-it))

;; helmの描画をwindow単位にする
(setq helm-save-configuration-functions
      '(set-window-configuration . current-window-configuration))

;; windowに表示されていない部分から更新する
(setq helm-quick-update t)

;; 候補がひとつしかない場合は、選択した時点で実行する
(setq helm-execute-action-at-once-if-one t)

;; emacsの終了時に、履歴を保存する
(remove-hook 'kill-emacs-hook 'helm-c-adaptive-save-history)

;; C-v/M-vで前後のsourceに移動する
(define-key helm-map (kbd "C-v") 'helm-next-source)
(define-key helm-map (kbd "M-v") 'helm-previous-source)
;; C-_でpersistent-actionを実行する
(define-key helm-map (kbd "C-_") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-h") 'backward-delete-char)

;; helm中から別のソースを読みだす。
(defvar helm-source-select-buffer "*helm source select*")
(defvar helm-c-source-call-source
  `((name . "Call helm source")
    (candidate-number-limit)
    (candidates
     . (lambda ()
         (loop for vname in (all-completions "helm-c-source-" obarray)
               for var = (intern vname)
               for name = (ignore-errors (assoc-default 'name (symbol-value var)))
               if name collect
               (cons (format "%s `%s'"
                             name (propertize vname 'face 'font-lock-variable-name-face))
                     var))))
    (action
     . (("Invoke helm with selected source"
         .
         (lambda (candidate)
           (setq helm-candidate-number-limit 9999)
           (helm candidate nil nil nil nil
                     helm-source-select-buffer)))
        ("Describe variable" . describe-variable)
        ("Find variable" . find-variable)))
    (persistent-action . describe-variable)
    (persistent-help . "Show description of this source")))

(defun helm-call-source-from-helm ()
  "Call helm source within `helm' session."
  (interactive)
  (setq helm-input-idle-delay 0)
  (helm-set-sources '(helm-c-source-call-source)))
(define-key helm-map (kbd "C-r") 'helm-call-source-from-helm)

;; マークが付けられた候補をいったりきたりする
(define-key helm-map (kbd "M-[") 'helm-prev-visible-mark)
(define-key helm-map (kbd "M-]") 'helm-next-visible-mark)

;; C-;でhelmを起動する
(global-set-key (kbd "C-;") 'my:helm)

;; persistent-actionを実行する際に、overlayを作成する
(defvar my:helm-c-persistent-highlight-overlay
  (make-overlay (point) (point)))

;; マッチした行にオーバーレイを置く。
(defun my:helm-persistent-highlight-point (start &optional end buf face rec)
  (goto-char start)
  (when (overlayp my:helm-c-persistent-highlight-overlay)
    (move-overlay my:helm-c-persistent-highlight-overlay
                  start
                  (or end (line-end-position))
                  buf))
  (overlay-put my:helm-c-persistent-highlight-overlay 'face (or face 'highlight))
  (when rec
    (recenter)))

;; helm-grepで特定の行に移動する際に、highlightを行う
(add-hook 'helm-grep-goto-hook
          (lambda ()
            (when helm-in-persistent-action
              (my:helm-persistent-highlight-point (point-at-bol) (point-at-eol)))))

;; helmの終了時に、persistent-actionで設置したoverlayを除去する
(add-hook 'helm-cleanup-hook
          (lambda ()
            (when (overlayp my:helm-c-persistent-highlight-overlay)
              (delete-overlay my:helm-c-persistent-highlight-overlay))))


;; helm-c-source-buffersで、現在開いているバッファを対象としない
(setq helm-allow-skipping-current-buffer t)

;; helm-c-source-files-in-dirを利用できるようにする。
(defun helm-c-transform-file-name-nondirectory (files)
  (mapcar (lambda (f) (cons (file-name-nondirectory f) f)) files))

(defun helm-c-source-files-in-dir (desc dir &optional match skip-opened-file)
  `((name . ,desc)
    (candidates . (lambda () (directory-files ,dir t ,match)))
    (candidate-transformer
     . (lambda (candidates)
         (helm-c-compose (list candidates)
                             '(,@(if skip-opened-file (list 'helm-c-skip-opened-files))
                               helm-c-transform-file-name-nondirectory))))
    (type . file)))

;; すべてのkill-ringを利用する
(setq helm-kill-ring-threshold 1)

(require 'helm-descbinds)
(helm-descbinds-install)

(require 'helm-ls-git)
