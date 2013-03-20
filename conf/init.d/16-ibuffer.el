;; (@* "進化したバッファリストであるibufferを利用するようにする")
(require 'ibuffer)

;; ibufferで、SPCで現在のカーソル下にあるファイルを別ウィンドウで閲覧することができる。
(defvar my:ibuffer-enable-visit-buffers nil
  "自動的に閲覧を開始するかどうかを決定する。")
(defun ibuffer-visit-buffer-other-window-scroll (&optional down)
  (interactive)
  (when my:ibuffer-enable-visit-buffers
    (let ((buf (ibuffer-current-buffer)))
      (when (and buf (buffer-live-p buf))
        (if (string=
             (buffer-name (window-buffer (next-window)))
             (buffer-name buf))
            (if down
                (scroll-other-window-down nil)
              (scroll-other-window))
          (ibuffer-visit-buffer-other-window-noselect))
        (if (eq major-mode 'ibuffer-mode)
            ()
          (setq buf (current-buffer))
          (switch-to-buffer (get-buffer "*Ibuffer*"))
          ))))
  )

(defun ibuffer-visit-buffer-other-window-scroll-down ()
  (interactive)
  (ibuffer-visit-buffer-other-window-scroll t))

(define-key ibuffer-mode-map (kbd "SPC") 'ibuffer-visit-buffer-other-window-scroll)
(define-key ibuffer-mode-map (kbd "b") 'ibuffer-visit-buffer-other-window-scroll-down)
(define-key ibuffer-mode-map (kbd "\C-c\C-e")
  (lambda () (interactive)
    (setq my:ibuffer-enable-visit-buffers
          (not my:ibuffer-enable-visit-buffers))))

;; n, p で次 (前) のバッファの内容を表示する
(defadvice ibuffer-forward-line
  (after ibuffer-scroll-page activate)
  (ibuffer-visit-buffer-other-window-scroll))
(defadvice ibuffer-backward-line
  (after ibuffer-scroll-page-down activate)
  (ibuffer-visit-buffer-other-window-scroll-down))
