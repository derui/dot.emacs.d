(require 'gtags)
(define-key gtags-mode-map (kbd "C-]") 'gtags-pop-stack)

;; 表示を見やすくする。
(defun gtags-goto-tag (tagname flag)
  (let (save prefix buffer lines)
    (setq save (current-buffer))
    (cond
     ((equal flag "P")
      (setq prefix "(P)"))
     ((equal flag "g")
      (setq prefix "(GREP)"))
     ((equal flag "I")
      (setq prefix "(IDUTILS)"))
     ((equal flag "s")
      (setq prefix "(S)"))
     ((equal flag "r")
      (setq prefix "(R)"))
     (t (setq prefix "(D)")))
    ;; load tag
    (setq buffer (generate-new-buffer (generate-new-buffer-name (concat "*GTAGS SELECT* " prefix tagname))))
    (set-buffer buffer)
    (message "Searching %s ..." tagname)
    (if (not (= 0 (call-process "global" nil t nil (concat "-x" flag) tagname))) ; remove '-a' option
        (progn (message (buffer-substring (point-min)(1- (point-max))))
               (gtags-pop-context))
      (goto-char (point-min))
      (setq lines (count-lines (point-min) (point-max)))
      (cond
       ((= 0 lines)
        (cond
         ((equal flag "P")
          (message "%s: path not found" tagname))
         ((equal flag "g")
          (message "%s: pattern not found" tagname))
         ((equal flag "I")
          (message "%s: token not found" tagname))
         ((equal flag "s")
          (message "%s: symbol not found" tagname))
         (t
          (message "%s: tag not found" tagname)))
        (gtags-pop-context)
        (kill-buffer buffer)
        (set-buffer save))
       ((= 1 lines)
        (message "Searching %s ... Done" tagname)
        (gtags-select-it t))
       (t
        ;; added by rubikitch
        (goto-char (point-min))
        (while (not (eobp))
          (put-text-property (point) (+ 16 (point)) 'invisible t)
          (forward-line 1))
        (goto-char (point-min))
        (switch-to-buffer buffer)
        (gtags-select-mode))))))

(defun gtags-select-it (delete)
  (let (line file)
    ;; get context from current tag line
    (beginning-of-line)
    (if (not (looking-at "[^ \t]+[ \t]+\\([0-9]+\\)[ \t]\\([^ \t]+\\)[ \t]"))
        (gtags-pop-context)
      (setq line (string-to-number (gtags-match-string 1)))
      (setq file (expand-file-name (gtags-match-string 2)))
      (if delete (kill-buffer (current-buffer)))
      ;; move to the context
      (if gtags-read-only (find-file-read-only file) (find-file file))
      (setq gtags-current-buffer (current-buffer))
      (goto-line line)
      (gtags-mode 1))))

;; 現在行が含まれる場合、そこを選択しておくようにすう。
(defun gtags-current-line ()
  (save-excursion
    (save-restriction
      (widen)
      (forward-char 1)
      (count-lines (point-min) (point)))))

(defadvice gtags-goto-tag (around show-current-position activate)
  "Point to the entry of current file/line."
  (let ((bfn buffer-file-name)
        (curline (gtags-current-line)))
    ad-do-it
    (when (and bfn (eq major-mode 'gtags-select-mode))
      (let ((path (if (looking-at "[^ \t]+[ \t]+[0-9]+[ \t]/[^ \t]+[ \t]")
                      bfn
                    (file-relative-name bfn))))
        (and (or (re-search-forward (format "%d[ \t]+%s" curline (regexp-quote path)) nil t)
                 (search-forward path nil t))
             (beginning-of-line))))))
