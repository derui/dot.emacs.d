(eval-when-compile
  (require 'use-package))

(defvar my:input-method)

(defun my:evil-swap-key (map key1 key2)
  ;; MAP中のKEY1とKEY2を入れ替え
  "Swap KEY1 and KEY2 in MAP."
  (let ((def1 (lookup-key map key1))
        (def2 (lookup-key map key2)))
    (define-key map key1 def2)
    (define-key map key2 def1)))

(use-package evil-mc
  :after (evil hydra)
  :hook ((after-init . global-evil-mc-mode))
  :config
  (defhydra hydra-evil-mc (:hint nil)
    "
 Up^^             Down^^           Miscellaneous
---------------------------------------------------
 [_k_]   Next     [_j_]   Next     [_a_] Mark all
 [_K_]   Skip     [_J_]   Skip     [_c_] Clear all
 [_g_]  First     [_G_]   Last     [_q_] Quit
 "
    ("a" evil-mc-make-all-cursors :exit t)
    ("j" evil-mc-make-and-goto-next-match)
    ("J" evil-mc-skip-and-goto-next-match)
    ("k" evil-mc-make-and-goto-prev-match)
    ("K" evil-mc-skip-and-goto-prev-match)
    ("g" evil-mc-make-and-goto-first-cursor)
    ("G" evil-mc-make-and-goto-last-cursor)
    ("c" evil-mc-undo-all-cursors :exit t)
    ("q" nil)))

(use-package evil-leader
  :config
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "p" 'projectile-command-map
    "i" 'hydra-evil-mc/body
    "q" 'evil-delete-buffer
    "w" 'save-buffer
    "oc" 'org-capture
    "os" 'org-tree-slide-mode
    "d" 'dired-jump
    "e" 'find-file
    "b" 'ibuffer
    "#" 'server-edit
    "s" 'my:counsel-search-dwim
    "m" 'magit-status
    "f" 'counsel-git
    "tt" 'treemacs-select-window
    "tq" 'treemacs-quit
    ;; 'l' is head character of operations for 'lint'
    ;; Recommend to use evil's default keybinding (z =, s ] or s [) when correct warning issued from flyspell.
    "ll" 'langtool-check
    "lL" 'langtool-check-done
    ;; 'c' is head character of 'counsel'
    "ci" 'counsel-imenu
    "cg" 'counsel-git-grep
    "cs" 'counsel-ag
    "cf" 'counsel-git
    "x" 'counsel-M-x
    "z" 'winner-undo)

  ;; set up key binding for org-mode local with evil-leader
  (evil-leader/set-key-for-mode 'org-mode
    ",a" 'org-agenda
    ",n" 'org-narrow-to-subtree
    ",w" 'widen
    ",p" 'org-pomodoro)
  )

(use-package ace-window :defer t)
(use-package avy :defer t)
(use-package evil
  :commands (evil-ex-define-cmd evil-set-initial-state evil-normal-state-p evil-insert-state)
  :hook (;; Disable ime returned to normal state
         (evil-normal-state-entry . my:evil-disable-ime))

  :bind (:map
         evil-normal-state-map
         ("M-y" . counsel-yank-pop)
         ("s" . nil)
         (";" . ivy-switch-buffer)
         ("C-a" . evil-numbers/inc-at-pt)
         ("C-x" . evil-numbers/dec-at-pt)
         ("TAB" . nil)
         ;; evil-jump-forwardを潰す。
         :map evil-motion-state-map
         ("TAB" . nil)
         ("s f" . avy-goto-char)
         ("s j" . avy-goto-line-below)
         ("s k" . avy-goto-line-above)

         :map evil-insert-state-map
         ("M-y" . counsel-yank-pop)
         ("C-q" . evil-normal-state)
         ("<Hangul>" . my:evil-enable-ime)
         ("<henkan>" . my:evil-enable-ime)
         ("<Hangul_Hanja>" . my:evil-disable-ime)
         ("<muhenkan>" . my:evil-disable-ime)

         :map evil-window-map
         ("C-w" . ace-select-window))

  :preface
  (defun my:evil-change-input-method (ime-state)
    (let ((when-emacs-state (string= evil-state "emacs")))
      (cond
       ((and ime-state (or (not current-input-method) (string-equal current-input-method my:input-method)))
        (set-input-method my:input-method)
        (when (evil-normal-state-p)
          (evil-insert-state)))
       (t
        (set-input-method nil)))))

  (defun my:evil-enable-ime ()
    (interactive)
    (my:evil-change-input-method t))

  (defun my:evil-disable-ime ()
    (interactive)
    (my:evil-change-input-method nil))

  :config
  (use-package evil-numbers :commands (evil-numbers/dec-at-pt evil-numbers/inc-at-pt))

  (evil-set-initial-state 'ivy-occur-grep-mode 'normal)

  (setcdr evil-insert-state-map nil)
  ;;M-:
  (evil-ex-define-cmd "eval" 'eval-expression)
  (evil-ex-define-cmd "ev" "eval")

  ;;C-h k
  (evil-ex-define-cmd "describe-key" 'describe-key)
  (evil-ex-define-cmd "key" "describe-key")

  ;; 論理行と物理行の移動を入れ替え
  (my:evil-swap-key evil-motion-state-map "j" "gj")
  (my:evil-swap-key evil-motion-state-map "k" "gk")

  (setq evil-normal-state-tag   (propertize "N" 'face '((:foreground "black")))
        evil-emacs-state-tag    (propertize "E" 'face '((:foreground "black")))
        evil-insert-state-tag   (propertize "I" 'face '((:foreground "red")))
        evil-motion-state-tag   (propertize "M" 'face '((:foreground "blue")))
        evil-visual-state-tag   (propertize "V" 'face '((:foreground "black")))
        evil-operator-state-tag (propertize "O" 'face '((:foreground "purple"))))

  ;; To suppress error when exit from insert-state
  (setq abbrev-expand-function #'ignore)
  (evil-mode 1))
