(eval-when-compile
  (require 'cl)
  (unless (fboundp 'cl-letf) (defalias 'cl-letf 'letf)))
(require 'sgml-mode)

(defgroup hatena-markup nil
  "Major mode for Hantea markup language."
  :prefix "hatena-markup-"
  :group 'hypermedia)

(defface hatena:markup:title
  '((((class color) (min-colors 8) (background dark))
     :foreground "#dfaf8f")
    (((class color) (min-colors 8) (background light))
     :foreground "#4f1f0f"))
  "Face for Hatena markup title."
  :group 'hatena-markup)

(defface hatena:markup:title-name
  '((t (:inherit hatena:markup:title :foreground "LemonChiffon4")))
  "Face for Hatena markup title."
  :group 'hatena-markup)

(defface hatena:markup:title-time
  '((t (:inherit hatena:markup:title-name)))
  "Face for Hatena markup title."
  :group 'hatena-markup)

(defface hatena:markup:title-category
  '((t (:inherit hatena:markup:title :foreground "LemonChiffon4")))
  "Face for Hatena markup title."
  :group 'hatena-markup)

(defface hatena:markup:headline1
  '((((class color) (min-colors 8) (background dark))
     :foreground "#8fb28f" :underline t)
    (((class color) (min-colors 8) (background light))
     :foreground "#0f320f" :underline t))
  "Face for Hatena markup headlines."
  :group 'hatena-markup)

(defface hatena:markup:headline2
  '((((class color) (min-colors 8) (background dark))
     :foreground "#7cb8bb")
    (((class color) (min-colors 8) (background light))
     :foreground "#0c484b"))
  "Face for Hatena markup small headlines."
  :group 'hatena-markup)

(defface hatena:markup:list-item
  '((t (:inherit font-lock-string-face)))
  "Face for Hatena markup list items."
  :group 'hatena-markup)

(defface hatena:markup:list-item-text
  '((t (:inherit default)))
  "Face for Hatena markup list item texts."
  :group 'hatena-markup)

(defface hatena:markup:definition-term
  '((t (:inherit font-lock-string-face)))
  "Face for Hatena markup definition terms."
  :group 'hatena-markup)

(defface hatena:markup:definition-description
  '((t (:inherit default)))
  "Face for Hatena markup definition descriptions."
  :group 'hatena-markup)

(defface hatena:markup:table-field
  '((((class color) (min-colors 8) (background dark))
     :foreground "#9fc59f")
    (((class color) (min-colors 8) (background light))
     :foreground "#1f451f"))
  "Face for Hatena markup table fields."
  :group 'hatena-markup)

(defface hatena:markup:table-head
  '((((class color) (min-colors 8) (background dark))
     :foreground "#94bff3")
    (((class color) (min-colors 8) (background light))
     :foreground "#142f73"))
  "Face for Hatena markup table heads."
  :group 'hatena-markup)

(defface hatena:markup:table-separator
  '((t (:inherit hatena:markup:table-field)))
  "Face for Hatena markup table separators."
  :group 'hatena-markup)

(defface hatena:markup:blockquote
  '((t (:foreground "DarkKhaki")))
  "Face for Hatena markup blockquotes."
  :group 'hatena-markup)

(defface hatena:markup:pre
  '((((class color) (min-colors 8) (background dark))
     :foreground "DarkKhaki")
    (((class color) (min-colors 8) (background light))
     :foreground "khaki4"))
  "Face for Hatena markup preformatted text indicators."
  :group 'hatena-markup)

(defface hatena:markup:pre-language
  '((t (:foreground "tomato")))
  "Face for Hatena markup preformatted text language specification."
  :group 'hatena-markup)

(defface hatena:markup:footnote
  '((((class color) (min-colors 8) (background dark))
     :foreground "#93e0e3")
    (((class color) (min-colors 8) (background light))
     :foreground "#136063"))
  "Face for Hatena markup footenotes."
  :group 'hatena-markup)

(defface hatena:markup:read-more
  '((t (:inherit font-lock-comment-face)))
  "Face for Hatena markup read more indicators."
  :group 'hatena-markup)

(defface hatena:markup:url
  '((((class color) (min-colors 8) (background dark))
     :foreground "#d0bf8f" :underline t)
    (((class color) (min-colors 8) (background light))
     :foreground "#503f0f" :underline t))
  "Face for Hatena markup urls."
  :group 'hatena-markup)

(defface hatena:markup:link
  '((((class color) (min-colors 8) (background dark))
     :foreground "IndianRed2")
    (((class color) (min-colors 8) (background light))
     :foreground "IndianRed4"))
  "Face for Hatena markup links."
  :group 'hatena-markup)

(defconst hatena-markup-auto-links
  '("mailto" "map" "twitter"
    "id" "question" "a:id" "b:id" "d:id" "f:id" "g" "idea" "i:id" "graph:id"
    "isbn" "ISBN" "asin" "ASIN" "jan" "JAN" "ean" "EAN" "ugomemo"))
(defconst hatena-markup-links
  `("niconico" "google" "amazon" "wikipedia" "search"
    "b:t" "b:keyword" "h:keyword" "h:id" "i:t" "graph:t" "keyword" "tex" "uke"
    "rakuten" ,@hatena-markup-auto-links))
(defconst hatena-markup-url "s?https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+")

(defconst hatena-markup-font-lock-keywords
  `(;; title
    ("^\\*[^*].*$" 0 'hatena:markup:title)
    ("^\\*\\([-_a-zA-Z0-9]+\\)\\*" 1 'hatena:markup:title-name t)
    ("^\\*\\([0-9]+\\)\\*" 1 'hatena:markup:title-time t)
    ("^\\*[^*]+\\*\\(\\(?:\\[.*?\\]\\)+\\)" 1 'hatena:markup:title-category t)
    ("^\\*\\(\\(?:\\[.*?\\]\\)+\\)" 1 'hatena:markup:title-category t)
    ;; headline1
    ("^\\*\\*[^*].*$" 0 'hatena:markup:headline1)
    ;; headline2
    ("^\\*\\*\\*.*$" 0 'hatena:markup:headline2)
    ;; list item
    ("^\\([-+]+\\)\\(.*\\)$"
     (1 'hatena:markup:list-item)
     (2 'hatena:markup:list-item-text))
    ;; definition list
    ("^\\(:[^:]*?:\\)\\(.*\\)$"
     (1 'hatena:markup:definition-term)
     (2 'hatena:markup:definition-description))
    ;; table
    ("^|"
     (0 'hatena:markup:table-separator)
     ("\\(*[^|]*\\)\\(|\\)"
      nil (progn (re-search-backward "^|") (forward-char)) ; reset
      (1 'hatena:markup:table-head)
      (2 'hatena:markup:table-separator))
     ("\\([^|]*\\)\\(|\\)" nil nil
      (1 'hatena:markup:table-field)
      (2 'hatena:markup:table-separator)))
    ;; blockquote
    (,(format "^\\(>>\\|>%s>\\|>%s%s>\\|<<\\)$"
              hatena-markup-url hatena-markup-url
              "\\(?::title\\|:title=.*\\|:bookmark\\)*")
     1 'hatena:markup:blockquote)
    ;; pre
    ("^\\(>|\\)\\([a-zA-Z0-9_?]+\\)\\(|\\)$"
     (1 'hatena:markup:pre t)
     (2 'hatena:markup:pre-language t)
     (3 'hatena:markup:pre t))
    ("^\\(>||\\|||<\\)$" (1 'hatena:markup:pre t))
    ("^\\(>|\\||<\\)$" 1 'hatena:markup:pre t)
    ;; footnote
    ("\\(((.*?))\\|((\\|))\\)" 0 'hatena:markup:footnote t)
    ;; read more
    ("^=====?$" 0 'hatena:markup:read-more)
    ;; url
    (,hatena-markup-url 0 'hatena:markup:url t)
    (,(concat "\\[" hatena-markup-url "\\]") 0 'hatena:markup:url t)
    ;; link without []
    (,(concat "\\[" (regexp-opt hatena-markup-links) ":.*?\\]")
     0 'hatena:markup:link t)
    ;; link with []
    ("\\[\\]" 0 'hatena:markup:link)
    (,(concat (regexp-opt hatena-markup-auto-links) ":[a-zA-Z0-9:_+#]+")
     0 'hatena:markup:link t)
    ("\\[\\[.*?\\]\\]" 0 'hatena:markup:link t)
    ;; comment
    ("\\(><!--\\|--><\\)" 0 'font-lock-comment-delimiter-face t)))

;;;###autoload
(define-derived-mode hatena:markup-mode html-mode "Hatena"
  "Major mode for Hatena markup language."
  (font-lock-add-keywords 'hatena:markup-mode hatena-markup-font-lock-keywords)
  (font-lock-mode 1)
  (set (make-local-variable 'indent-line-function) 'hatena-markup-indent-line))

(defalias 'sgml-parse-tag-backward-orig
  (symbol-function 'sgml-parse-tag-backward))
(defun hatena-markup-parse-tag-backward (&optional limit)
  (let ((savep (point)))
    (prog1 (sgml-parse-tag-backward-orig limit)
      (when (save-excursion (re-search-forward "^||?<$" (1+ savep) t))
        (goto-char savep)
        (error "No tag found")))))

(defun hatena-markup-indent-line ()
  (let ((lcon (sgml-lexical-context)))
    (cond
     ((save-excursion (beginning-of-line) ; pre and super-pre
                      (looking-at-p "^\\(>|[a-zA-Z0-9_?]*|?\\||?|<\\)$"))
      'noindent)
     ((and (integerp (cdr lcon))
           (or (eq (car lcon) 'tag)
               (and (eq (car lcon) 'text)
                    (save-excursion
                      (skip-chars-forward " \t\n")
                      (not (looking-at-p "</")))))
           (save-excursion
             (goto-char (cdr lcon))
             (or (looking-back "^>")
                 (looking-at-p "<$")
                 (let ((lcon2 (sgml-lexical-context)))
                   (when (integerp (cdr lcon2)) (goto-char (cdr lcon2)))
                   (and (integerp (cdr lcon2))
                        (eq (car lcon2) 'tag) (looking-at-p "<$"))))))
      (let* ((here (point))
             (savep (save-excursion (back-to-indentation) (< (point) here)))
             (context
              (cl-letf (((symbol-function 'sgml-parse-tag-backward)
                         (symbol-function 'hatena-markup-parse-tag-backward)))
                (goto-char (cdr lcon))
                (prog1 (nreverse (sgml-get-context nil))
                  (when (save-excursion (beginning-of-line)
                                        (looking-at-p "||?<$"))
                    (beginning-of-line)))))
             (col (+ (current-column) (* sgml-basic-offset (length context)))))
        (goto-char here)
        (if savep
            (save-excursion (indent-line-to col))
          (indent-line-to col))))
     (t (sgml-indent-line)))))

(defun hatena-markup-filter-context (context)
  (loop for tag in context
        when (> (length (sgml-tag-name tag)) 0)
        collect tag into result
        finally return result))

(defadvice sgml-get-context (around dont-count-empty-tag activate)
  (let ((context ad-do-it))
    (setq ad-return-value (hatena-markup-filter-context context))))

(provide 'hatena-markup-mode)
;;; hatena-markup-mode.el ends here
