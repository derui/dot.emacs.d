(require 'evil)
(require 'evil-leader)
(require 'evil-numbers)
(require 'avy)
(require 'mozc)

(setq evil-default-cursor t)

(defun my:evil-swap-key (map key1 key2)
  ;; MAP中のKEY1とKEY2を入れ替え
  "Swap KEY1 and KEY2 in MAP."
  (let ((def1 (lookup-key map key1))
        (def2 (lookup-key map key2)))
    (define-key map key1 def2)
    (define-key map key2 def1)))

(defun my:evil-define-state-to-all-map (key fun)
  "Define command to key on all state (not include insert state)"

  (define-key evil-normal-state-map key fun)
  (define-key evil-motion-state-map key fun)
  (define-key evil-visual-state-map key fun))

(defun evil-change-input-method (ime-state)
  (let ((when-emacs-state (string= evil-state "emacs")))
    (cond
     ((and ime-state (not current-input-method))
      (set-input-method my:input-method)
      (when (evil-normal-state-p)
        (evil-insert-state)))
     (t
      (set-input-method nil)))))

(defun evil-enable-ime ()
  (interactive)
  (evil-change-input-method t))

(defun evil-disable-ime ()
  (interactive)
  (evil-change-input-method nil))

(add-hook 'evil-normal-state-entry-hook #'evil-disable-ime)

(define-key evil-normal-state-map (kbd "s") nil)
(define-key evil-normal-state-map (kbd ";") 'my:helm)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "TAB") nil)
;; evil-jump-forwardを潰す。
(define-key evil-motion-state-map (kbd "TAB") nil)

;; Exit insert mode when typing `jj`. This is not need timeout
;; because it is as evil-command is used to sequencial event handling.
(evil-define-command my:evil-maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p))
        (entry-key ?j)
        (exit-key ?j))
    (insert entry-key)
    (let ((evt (read-event (format "Insert %c to exit insert state" exit-key) nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt exit-key))
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
       (t (push evt unread-command-events))))))

(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map (kbd "ESC") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "j") 'my:evil-maybe-exit)

(define-key evil-insert-state-map (kbd "<Hangul>") 'evil-enable-ime)
(define-key evil-insert-state-map (kbd "<henkan>") 'evil-enable-ime)
(define-key evil-insert-state-map (kbd "<Hangul_Hanja>") 'evil-disable-ime)
(define-key evil-insert-state-map (kbd "<muhenkan>") 'evil-disable-ime)

;; evil-leaderの設定
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "q" #'evil-delete-buffer
  "w" #'save-buffer
  "d" #'sunrise
  "D" #'sunrise-cd
  "e" #'find-file
  "b" #'switch-to-buffer
  "#" #'server-edit
  "sg" #'ag
  "sG" #'helm-do-ag
  "f" #'helm-ls-git-ls
  "y" #'helm-yas-complete
  "x" #'helm-M-x
  )

;;M-:
(evil-ex-define-cmd "eval" 'eval-expression)
(evil-ex-define-cmd "ev" "eval")

;;C-h k
(evil-ex-define-cmd "describe-key" 'describe-key)
(evil-ex-define-cmd "key" "describe-key")

;; 論理行と物理行の移動を入れ替え
(my:evil-swap-key evil-motion-state-map "j" "gj")
(my:evil-swap-key evil-motion-state-map "k" "gk")
(define-key evil-motion-state-map (kbd "s f") #'avy-goto-char)
(define-key evil-motion-state-map (kbd (concat "s" " j")) #'avy-goto-line-below)
(define-key evil-motion-state-map (kbd (concat "s" " k")) #'avy-goto-line-above)

(setq evil-normal-state-tag   (propertize "N" 'face '((:background "green" :foreground "black")))
      evil-emacs-state-tag    (propertize "E" 'face '((:background "orange" :foreground "black")))
      evil-insert-state-tag   (propertize "I" 'face '((:background "red")))
      evil-motion-state-tag   (propertize "M" 'face '((:background "blue")))
      evil-visual-state-tag   (propertize "V" 'face '((:background "grey80" :foreground "black")))
      evil-operator-state-tag (propertize "O" 'face '((:background "purple"))))

(evil-mode 1)
