(require 'view)

;; view-modeを活用するための設定
(setq view-read-only t)
(defvar pager-keybind nil)
(setq pager-keybind
      `( ;; vi-like
        ("o" . ,(lambda () (interactive)
                  (let ((anything-c-moccur-enable-initial-pattern nil))
                    (anything-c-moccur-occur-by-moccur))))
        (";" . anything)
        ("i" . View-exit-and-edit)
        ("h" . backward-char)
        ("l" . forward-char)
        ("j" . next-window-line)
        ("k" . previous-window-line)
        ("b" . scroll-down)
        (" " . scroll-up)
        ("G" . View-scroll-to-buffer-end)
        ("^" . seq-home)
        ("f" . my:search-forward-with-char)
        ("F" . my:search-backward-with-char)
        ("z" . view-recenter)
        ("{" . backward-paragraph)
        ("}" . forward-paragraph)
        ;; w3m-like
        ("w" . forward-word)
        ("e" . backward-word)
        ("(" . point-undo)
        (")" . point-redo)
        ("J" . ,(lambda () (interactive) (scroll-up 1)))
        ("K" . ,(lambda () (interactive) (scroll-down 1)))
        ;; bm-easy
        ("." . bm-toggle)
        ("[" . bm-previous)
        ("]" . bm-next)
        ("m" . bm-remove-all)
        ;; langhelp-like
        ("c" . scroll-other-window-down)
        ("v" . scroll-other-window)
        ;; expand-region
        ("@" . er/expand-region)
        ("<" . mark-previous-like-this)
        (">" . mark-next-like-this)
        ;; find file
        ("" . anything-find-file)
        ;; toggle truncate lines
        ("t" . ,(lambda () (interactive)
                  (if truncate-lines
                      (progn
                        (setq truncate-lines nil)
                        (message "truncate-lines to inactive"))
                    (setq truncate-lines t)
                    (mesasge "truncate-lines to active"))))
        ))

(defun define-many-keys (keymap key-table &optional includes)
  (let (key cmd)
    (dolist (key-cmd key-table)
      (setq key (car key-cmd)
            cmd (cdr key-cmd))
      (if (or (not includes) (member key includes))
        (define-key keymap key cmd))))
  keymap)

(defun view-mode-hook0 ()
  (define-many-keys view-mode-map pager-keybind)
  (view-mode-set-control-map view-mode-window-control-map "s")
  (if skk-mode (skk-mode -1))
  )
(add-hook 'view-mode-hook 'view-mode-hook0)

;; view-modeへの移行を行わないモードを設定する。
(defvar view-mode-ignore-mode-list '())
(add-to-list 'view-mode-ignore-mode-list "dired-mode")
(add-to-list 'view-mode-ignore-mode-list "mew-summary-mode")
(add-to-list 'view-mode-ignore-mode-list "mew-draft-mode")
(add-to-list 'view-mode-ignore-mode-list "mew-message-mode")

(defun view-mode-check-ignore-mode ()
  (not (find-if '(lambda (x) (string= x major-mode)) view-mode-ignore-mode-list))
  )

(defvar view-mode-always-with-find-file nil)
(setq view-mode-always-with-find-file nil)
;; 書き込み不能なファイルはview-modeで開くように
(defadvice find-file
  (around find-file-switch-to-view-file (file &optional wild) activate)
  (if (and (not (file-writable-p file))
           (not (file-directory-p file)))
      (view-file file)
    (progn
      ad-do-it
      (if (and (file-exists-p file)
               view-mode-always-with-find-file
               (view-mode-check-ignore-mode))
          (view-mode 1)))))

;; 書き込み不能な場合はview-modeを抜けないように
(defvar view-mode-force-exit nil)
(defmacro do-not-exit-view-mode-unless-writable-advice (f)
  `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
     (if (and (buffer-file-name)
              (not view-mode-force-exit)
              (not (file-writable-p (buffer-file-name))))
         (message "File is unwritable, so stay in view-mode.")
       (progn
         (hl-line-mode 0)
         ad-do-it))))

(do-not-exit-view-mode-unless-writable-advice view-mode-exit)
(do-not-exit-view-mode-unless-writable-advice view-mode-disable)

;; view-mode時に、手軽にウィンドウ移動、切替を行えるようにする。
(defvar view-mode-window-control-map nil)
(unless view-mode-window-control-map
  (setq view-mode-window-control-map (make-sparse-keymap))

  (define-key view-mode-window-control-map (kbd "l") 'windmove-right)
  (define-key view-mode-window-control-map (kbd "h") 'windmove-left)
  (define-key view-mode-window-control-map (kbd "k") 'windmove-up)
  (define-key view-mode-window-control-map (kbd "j") 'windmove-down)

  (define-key view-mode-window-control-map (kbd "d") 'delete-window)
  (define-key view-mode-window-control-map (kbd "v") 'split-window-horizontally)
  (define-key view-mode-window-control-map (kbd "c") 'split-window-vertically)
  (define-key view-mode-window-control-map (kbd "o") 'delete-other-windows)
  (define-key view-mode-window-control-map (kbd "s") 'my:other-window)
)

(defvar view-mode-original-keybind nil)
(defun view-mode-set-control-map (keymap prefix-key)
  (unless view-mode-original-keybind
    (dolist (l (cdr view-mode-map))
      (if (equal prefix-key (car l))
          (setq view-mode-original-keybind (list prefix-key (cdr l))))))
  (define-key view-mode-map prefix-key keymap))

(defun view-mode-unset-window-controls()
  (when view-mode-original-keybind
    (define-key view-mode-map (car view-mode-original-keybind)
      (cadr view-mode-original-keybind))
    (setq view-mode-original-keybind nil)))

(defvar view-mode-auto-change-alist '())
(defvar view-mode-auto-change-time 1.0)
(defvar view-mode-auto-change-timer nil)

(add-to-list 'view-mode-auto-change-alist "c++-mode")
(defun view-mode-set-auto-change-timer ()
  (unless view-mode-auto-change-timer
    (setq view-mode-auto-change-timer
          (run-with-idle-timer view-mode-auto-change-time t
                               '(lambda ()
                                  (if (find-if (lambda (x) (string= x major-mode)) view-mode-auto-change-alist)
                                      (view-mode)
                                    nil)))))
  )

(defun view-mode-cancel-auto-change-timer ()
  (when view-mode-auto-change-timer
    (cancel-timer view-mode-auto-change-timer)
    (setq view-mode-auto-change-timer nil))
  )

(defun view-mode-toggle-auto-change-timer ()
  (interactive)
  (if view-mode-auto-change-timer
      (view-mode-cancel-auto-change-timer)
    (view-mode-set-auto-change-timer))
  )

;; view-modeを活用する設定 -- ここまで
