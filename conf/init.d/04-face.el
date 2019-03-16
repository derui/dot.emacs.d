;; faceなどの見ための設定を記述する。
;; faceに関する設定ならば、adviceや関数なども設定していい。
;; ただし、各elisp個別の設定ならば、できればそっちに記述する。

;; (@> "全角空白、タブ、改行直前の空白に色をつける")
(defface my-face-b-1 '((t (:background "gray"))) "face for full-width space" :group 'my)
(defface my-face-b-2 '((t (:background "gray26"))) "face for tab" :group 'my)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) "" :group 'my)

(defun my:font-lock-mode ()
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-face-b-2 append)
     ("　" 0 my-face-b-1 append)
     ("[ \t]+$" 0 my-face-u-1 append))))
(advice-add 'font-lock-mode :before 'my:font-lock-mode)

(add-to-list 'custom-theme-load-path
             (expand-file-name (concat user-emacs-directory "themes")))
