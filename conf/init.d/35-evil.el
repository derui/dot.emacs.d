
(evil-mode 1)

(defadvice update-buffer-local-cursor-color
  (around evil-update-buffer-local-cursor-color-in-insert-state activate)
  ;; SKKによるカーソル色変更を, 挿入ステートかつ日本語モードの場合に限定
  "Allow ccc to update cursor color only when we are in insert
state and in `skk-j-mode'."
  (when (and (eq evil-state 'insert) (boundp 'skk-j-mode) skk-j-mode)
    ad-do-it))
(defadvice evil-refresh-cursor
  (around evil-refresh-cursor-unless-skk-mode activate)
  ;; Evilによるカーソルの変更を, 挿入ステートかつ日本語モードではない場合に限定
  "Allow ccc to update cursor color only when we are in insert
state and in `skk-j-mode'."
  (unless (and (eq evil-state 'insert) (boundp 'skk-j-mode) skk-j-mode)
    ad-do-it))

(defun evil-swap-key (map key1 key2)
  ;; MAP中のKEY1とKEY2を入れ替え
  "Swap KEY1 and KEY2 in MAP."
  (let ((def1 (lookup-key map key1))
        (def2 (lookup-key map key2)))
    (define-key map key1 def2)
    (define-key map key2 def1)))
;; 論理行と物理行の移動を入れ替え
(evil-swap-key evil-motion-state-map "j" "gj")
(evil-swap-key evil-motion-state-map "k" "gk")

(define-key evil-normal-state-map (kbd ";") 'my:helm)

;; evil-leaderの設定
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "d" #'sunrise-cd
  "e" #'find-file
  "b" #'switch-to-buffer)

;; ,z[a-z]で、別々のwindowへの切り替えを行う
(dolist (key-char 
         '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z))
  (evil-leader/set-key (concat "z" (char-to-string key-char)) #'win-switch-to-window)
  )
