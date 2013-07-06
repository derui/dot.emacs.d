;; faceなどの見ための設定を記述する。
;; faceに関する設定ならば、adviceや関数なども設定していい。
;; ただし、各elisp個別の設定ならば、できればそっちに記述する。

;; (@> "全角空白、タブ、改行直前の空白に色をつける")

(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "gray26"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)

(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-face-b-2 append)
     ("　" 0 my-face-b-1 append)
     ("[ \t]+$" 0 my-face-u-1 append)
     ; ;("[\r]*\n" 0 my-face-r-1 append)
     )))

(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

(add-to-list 'custom-theme-load-path
             (expand-file-name (concat user-emacs-directory "themes")))
