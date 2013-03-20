;; sdic.el ---- -*- Emacs-Lisp -*- Program to view dictionary.
;; $Id: sdic.el.in,v 2.27 2002/07/02 11:17:46 tsuchiya Exp $

;; Copyright (C) 1998,99 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: dictionary

;; SDIC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; SDIC is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SDIC; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


;;; Commentary:

;; 英和/和英辞書を閲覧する目的で作成した major mode です。
;; 利用及び再配布の際は、GNU 一般公用許諾書の適当なバージョンにしたがっ
;; て下さい。

;; 一次配布元
;;    http://namazu.org/~tsuchiya/sdic/


;;; Install:

;; (1) sdic.el, sdicf.el, sdicf-client.el, sdic-compat.el, sdic-gene.el
;;     と stem.el を適当な場所に保存して、必要ならバイトコンパイルして
;;     下さい。
;;
;;
;; (2) sdicf-client.el, sdic-compat.el, sdic-gene.el は辞書を検索する
;;     ためのライブラリです。これらのライブラリのどれかを使って辞書を
;;     検索できるようにして下さい。詳細については、README とそれぞれの
;;     ソースファイルを参照。
;;
;;
;; (3) 使えるようにした辞書のリストを、sdic-eiwa-dictionary-list およ
;;     び sdic-waei-dictionary-list に設定します。例えば、英和辞書
;;     /usr/dict/gene.sdic を sdicf-client.el を使って検索する場合は次
;;     のようになります。
;;
;;         (setq sdic-eiwa-dictionary-list '((sdicf-client "/usr/dict/gene.sdic")))
;;
;;     複数の辞書を同時に検索することも出来ます。
;;
;;         (setq sdic-waei-dictionary-list '((sdicf-client "~/data/jedict.sdic")
;;                                           (sdic-compat "/usr/dict/jgene.dic")))
;;
;;     辞書を利用しない場合は nil を代入して下さい。また、これらの設定
;;     は ~/.emacs などの適切な場所に書き込んで下さい。
;;
;;
;; (4) ~/.emacs に次のようなコードを挿入します。
;;
;;         (autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
;;         (global-set-key "\C-cw" 'sdic-describe-word)
;;
;;     好みに合わせて適当にキーバインドは変更して下さい。


;;; Note:

;; 検索結果の表示の仕方や動作を制御する変数があります。詳細については、
;; 下の source を参照して下さい。
;;
;; grep / array などの外部コマンドを利用して辞書検索する場合は、それら
;; の外部コマンドが対応している漢字コードを設定して、辞書もその漢字コー
;; ドに合わせる必要があります。その場合、日本語 EUC がおそらく一番安全
;; でしょう。
;;
;; Emacs20 または XEmacs で使用する場合は、
;;
;;     (set-language-environment "Japanese")
;;
;; と .emacs に設定されているか確認してください。


(provide 'sdic)
(require 'sdicf)
(require 'stem)


;;;----------------------------------------------------------------------
;;;		カスタマイズ用変数
;;;----------------------------------------------------------------------

(defvar sdic-left-margin 2 "*Left margin of contents.
説明文の左側の余白幅")

(defvar sdic-fill-column default-fill-column "*Right edge of contents.
説明文を整形する幅")

(defvar sdic-window-height 10 "*Height of window to show entrys and contents.
検索結果表示ウインドウの高さ")

(defvar sdic-warning-hidden-entry t "*If non-nil, warning of hidden entries is enable.
nil 以外が設定されている場合、検索結果表示ウインドウに表示しきれなかった情報があれば警告する")

(defvar sdic-disable-select-window nil "*Option to disable to select other window.
検索結果表示ウインドウにカーソルを移動しないようにする場合は nil 以外を設定する")

(defvar sdic-face-style 'bold  "*Style of entry.
見出し語を表示するために使う装飾形式")

(defvar sdic-face-color nil "*Color of entry.
見出し語を表示するために使う色")

(defvar sdic-disable-vi-key nil "*Option to disable some key.
辞書閲覧に vi ライクのキーを使わない場合は nil 以外を設定する")

(defvar sdic-eiwa-dictionary-list
  (delq nil (mapcar
	     (lambda (file)
	       (and (file-readable-p (expand-file-name file))
		    (list (if (string-match "\\.sdic$" file)
			      'sdicf-client
			    (if (and (require 'sdic-compat)
				     (sdic-compat-available-p))
				'sdic-compat
			      'sdic-gene))
			  file)))
	     '("/usr/local/share/dict/gene.sdic"))) "\
*Options of an English-Japanese dictionary.
英和辞典の検索メソッドのリストを指定する変数")

(defvar sdic-waei-dictionary-list
  (delq nil (mapcar
	     (lambda (file)
	       (and (file-readable-p (expand-file-name file))
		    (list (if (string-match "\\.sdic$" file)
			      'sdicf-client
			    (if (and (require 'sdic-compat)
				     (sdic-compat-available-p))
				'sdic-compat
			      'sdic-gene))
			  file)))
	     '("/usr/local/share/dict/jedict.sdic"))) "\
*Options of an English-Japanese dictionary.
和英辞典の検索メソッドのリストを指定する変数")

(defvar sdic-default-coding-system
  (if (>= emacs-major-version 20)
      (if (string-match "XEmacs" emacs-version)
	  (cond
	   ((member 'euc-japan-unix (coding-system-list)) 'euc-japan-unix)
	   ((member 'euc-jp-unix (coding-system-list)) 'euc-jp-unix)
	   (t 'euc-japan))
	'euc-japan-unix)
    *euc-japan*unix)
  "*Default coding-system for sdic and libraries.")




;;;----------------------------------------------------------------------
;;;		内部変数
;;;----------------------------------------------------------------------

(defvar sdic-english-prep-list '("at" "by" "for" "in" "on" "of" "with" "as" "before" "after") "\
List of English prepositions
英語の前置詞のリスト")

(defvar sdic-english-prep-regexp
  (format "\\(%s\\)\\b" (mapconcat 'regexp-quote sdic-english-prep-list "\\|")) "\
Regexp of Englist prepositions
英語の前置詞とマッチする正規表現")

(defvar sdic-eiwa-symbol-list nil "英和辞典のシンボル")
(defvar sdic-waei-symbol-list nil "和英辞典のシンボル")
(defvar sdic-buffer-start-point nil "検索結果表示バッファの表示開始ポイント")
(defvar sdic-mode-map nil "Keymap of sdic-mode")

(defvar sdic-kinsoku-bol-list
  (funcall (if (fboundp 'string-to-char-list)
	       'string-to-char-list
	     'string-to-list)
	   "!)-_~}]:;',.?、。，．・：；？！゛゜´｀¨＾￣＿ヽヾゝゞ〃仝々〆〇ー―‐／＼〜‖｜…‥’”）〕］｝〉》」』】°′″℃ぁぃぅぇぉっゃゅょゎァィゥェォッャュョヮヵヶ")
  "行頭禁則文字のリスト")

(defvar sdic-kinsoku-eol-list
  (funcall (if (fboundp 'string-to-char-list)
	       'string-to-char-list
	     'string-to-list)
	   "({[`‘“（〔［｛〈《「『【°′″§")
  "行末禁則文字のリスト")

(defvar sdic-kinsoku-spc-list
  (funcall (if (fboundp 'string-to-char-list)
	       'string-to-char-list
	     'string-to-list)
	   "\t 　")
  "空白文字のリスト")

(defconst sdic-version "2.1.3")
(defconst sdic-buffer-name "*sdic*" "検索結果表示バッファの名前")
(defconst sdic-mode-name "SDIC" "検索結果を表示するバッファの major mode")




;;;----------------------------------------------------------------------
;;;		検索メソッドを呼び出す関数
;;;----------------------------------------------------------------------

(defun sdic-init-dictionary (option-list) "\
Function to initialize dictionary.
指定された辞書と関連付けられている検索ライブラリを初期化する関数"
  (let (dic)
    (and option-list
	 (listp option-list)
	 (require (car option-list))
	 (setq dic (apply (get (car option-list) 'init-dictionary) (cdr option-list)))
	 (sdic-dictionary-symbol-p dic)
	 (put dic 'search-method (car option-list))
	 dic)))


(defun sdic-open-dictionary (dic) "\
Function to open dictionary.
指定された辞書を検索できるようにする関数"
  (and (sdic-dictionary-symbol-p dic)
       (funcall (get (get dic 'search-method) 'open-dictionary) dic)))


(defun sdic-close-dictionary (dic) "\
Function to close dictionary.
指定された辞書と関連付けられている検索ライブラリを終了する関数"
  (and (sdic-dictionary-symbol-p dic)
       (funcall (get (get dic 'search-method) 'close-dictionary) dic)))


(defun sdic-search-entry (dic word &optional search-type) "\
Function to search word in dictionary.
指定された辞書を検索する関数
見出し語、辞書シンボル、見出し語のIDからなる配列を要素とする配列を返す。"
  (mapcar (lambda (c)
	    (list (car c) dic (cdr c)))
	  (funcall (get (get dic 'search-method) 'search-entry) dic word search-type)))


(defsubst sdic-replace-string (string from to) "\
文字列 STRING に含まれている文字列 FROM を全て文字列 TO に置換した文字列を返す
FROM には正規表現を含む文字列を指定できるが、TO は固定文字列しか指定できない。"
  (let ((start 0) list)
    (while (string-match from string start)
      (setq list (cons to (cons (substring string start (match-beginning 0)) list))
	    start (match-end 0)))
    (eval (cons 'concat (nreverse (cons (substring string start) list))))))


(defun sdic-sort-dictionary-order (entry-list) "\
Function to sort entry list in dictionary order.
見出し語、辞書シンボル、見出し語のIDからなる配列を要素とする配列 
ENTRY-LIST を、見出し語の辞書順に並べ替える関数。"
  (mapcar 'cdr
	  (sort (mapcar (lambda (entry)
			  (if (string-match "\\Ca" (car entry))
			      (cons (concat (car entry) "\^@") entry)
			    (cons (concat (sdic-replace-string (downcase (car entry)) "[^A-z0-9]+" " ")
					  "\^@" (car entry) "\^@")
				  entry)))
			entry-list)
		(lambda (a b) (string< (car a) (car b))))))


(defun sdic-search-multi-dictionaries (dic-list word &optional search-type) "\
Function to search word in multi dictionaries.
指定されている複数の辞書を串刺検索する関数
見出し語、辞書シンボル、見出し語のIDからなる配列を要素とする配列を返す。"
  (sdic-sort-dictionary-order
   (apply 'append
	  (mapcar (lambda (dic)
		    (sdic-search-entry dic word search-type))
		  dic-list))))


(defun sdic-get-content (dic id) "\
Function to get content.
指定されている辞書から定義文を読み出す関数"
  (funcall (get (get dic 'search-method) 'get-content) dic id))


(defun sdic-make-dictionary-symbol ()
  (make-symbol "sdic-dictionary"))


(defun sdic-dictionary-symbol-p (symbol)
  (equal (symbol-name symbol) "sdic-dictionary"))




;;;----------------------------------------------------------------------
;;;		内部関数
;;;----------------------------------------------------------------------

(defun sdic-insert-content (word content)
  "見出し語と説明文を整形しながら挿入する"
  (sdic-overlay-put (sdic-make-overlay (point) (progn (insert word) (point))) 'face 'sdic-face)
  (let ((spc (make-string left-margin ?\ )) top buf (pos 0))
    (insert "\n" spc)
    (setq top (point))
    (while (string-match ",\\([^ ]\\)" content pos)
      (setq buf (cons ", " (cons (substring content pos (match-beginning 0)) buf))
	    pos (match-beginning 1)))
    (setq content (eval (cons 'concat (nreverse (if (< pos (length content)) (cons (substring content pos) buf) buf))))
	  buf nil
	  pos 0)
    (while (string-match "[^ 0-9]\\(/\\)[^ 0-9]" content pos)
      (setq buf (cons " / " (cons (substring content pos (match-beginning 1)) buf))
	    pos (match-end 1)))
    (eval (cons 'insert (nreverse (if (< pos (length content)) (cons (substring content pos) buf) buf))))
    (insert "\n")
    (forward-char -1)
    (while (if (>= (move-to-column fill-column) fill-column)
	       (not (progn
		      (if (memq (preceding-char) sdic-kinsoku-eol-list)
			  (progn
			    (forward-char -1)
			    (while (memq (preceding-char) sdic-kinsoku-eol-list)
			      (forward-char -1))
			    (insert "\n" spc))
			(while (memq (setq buf (following-char)) sdic-kinsoku-bol-list)
			  (forward-char))
			(if (memq buf sdic-kinsoku-spc-list)
			    (delete-region (point)
					   (progn
					     (forward-char)
					     (while (memq (following-char) sdic-kinsoku-spc-list)
					       (forward-char))
					     (point)))
			  (or (> (char-width buf) 1)
			      (re-search-backward "\\<" top t)
			      (end-of-line)))
			(or (eolp) (insert "\n" spc))))))
      (setq top (point)))
    (forward-char)))


(defun sdic-insert-entry-list (entry-list)
  "見出し語と説明文を整形しながら挿入する"
  (mapcar (lambda (entry)
	    (sdic-insert-content (car entry) (sdic-get-content (nth 1 entry) (nth 2 entry)))
	    (car entry))
	  entry-list))


;; 検索形式を判別するマクロ
(put 'sdic-decide-query-type 'lisp-indent-function 2)
(defmacro sdic-decide-query-type (dic-list query &rest sexp) "\
QUERY から検索形式を判定して複数の辞書 DIC-LIST を検索するマクロ
QUERY に検索形式を指定する構造が含まれていない場合は、default の動作と
して SEXP を評価する。通常の検索の場合は、検索された見出し語のリストを
返す。"
  (` (cond
      ;; 検索語が '' で囲まれている場合 -> 完全一致検索
      ((and (equal ?' (string-to-char (, query)))
	    (equal "'" (substring (, query) -1)))
       (sdic-insert-entry-list
	(sdic-search-multi-dictionaries (, dic-list) (substring (, query) 1 -1) 'lambda)))
      ;; 検索語の先頭に / がある場合 -> 全文検索
      ((equal ?/ (string-to-char (, query)))
       (sdic-insert-entry-list
	(sdic-search-multi-dictionaries (, dic-list) (substring (, query) 1) 0)))
      ;; 検索語の先頭に * がある場合 -> 後方一致検索
      ((equal ?* (string-to-char (, query)))
       (sdic-insert-entry-list
	(sdic-search-multi-dictionaries (, dic-list) (substring (, query) 1) t)))
      ;; 検索語の末尾に * がある場合 -> 前方一致検索
      ((equal "*" (substring (, query) -1))
       (sdic-insert-entry-list
	(sdic-search-multi-dictionaries (, dic-list) (substring (, query) 0 -1))))
      ;; 特に指定がない場合 -> 指定された S 式を評価
      (t
       (,@ sexp)))))


;; 英和辞典を検索する関数
(defun sdic-search-eiwa-dictionary (query)
  (sdic-decide-query-type sdic-eiwa-symbol-list query
    (let (word-list stem-list orig pat point str)
      (setq word-list (sdic-split-string (downcase query)))
      (prog1 (mapcar
	      (lambda (entry)
		(and (not point)
		     (string-match pat (car entry))
		     (setq point (point)
			   str (car entry)))
		(sdic-insert-content (car entry) (sdic-get-content (nth 1 entry) (nth 2 entry)))
		(car entry))
	      (prog1 (or
		      ;; (1) 不規則変化動詞を検索する
		      (and
		       (setq stem-list (copy-sequence (assoc (car word-list) stem:irregular-verb-alist))
			     pat nil)
		       (setq stem-list (delq t (mapcar (lambda (w) (or (equal w pat) (setq pat w))) stem-list)))
		       (sdic-sort-dictionary-order
			(apply 'append
			       (mapcar
				(lambda (word)
				  (setq pat (format "^\\(%s$\\|%s \\)"
						    (regexp-quote word) (regexp-quote word)))
				  (delq nil
					(mapcar
					 (lambda (entry)
					   (and (string-match pat (car entry))
						(or orig (setq orig word))
						entry))
					 (apply 'append
						(mapcar
						 (lambda (dic)
						   (sdic-search-entry dic word))
						 sdic-eiwa-symbol-list)))))
				stem-list))))
		      ;; (2) 不規則変化動詞を無視した stemming を行なって検索する
		      (progn
			(setq stem-list (let ((stem:irregular-verb-alist nil))
					  (stem:stripping-suffix (car word-list))))
			(if (> (length (car word-list)) 1)
			    (setq stem-list
				  (delq t (mapcar (lambda (w) (or (= (length w) 1) w)) stem-list))))
			;; 最長部分列を求める
			(setq pat (let* ((w1 (car stem-list))
					 (w2 (nth (1- (length stem-list)) stem-list))
					 (i (min (length w1) (length w2))))
				    (while (not (string= (substring w1 0 i)
							 (substring w2 0 i)))
				      (setq i (1- i)))
				    (substring w1 0 i)))
			(delq nil (mapcar
				   (lambda (entry)
				     (if (string-match pat (car entry)) entry))
				   (prog1 (mapcar
					   (lambda (entry)
					     ;; 検索結果から原形と推定される見出し語を検索
					     (setq str (downcase (car entry)))
					     (and (member str stem-list)
						  (not (member str orig))
						  (setq orig (cons str orig)))
					     entry)
					   (or (and
						(= (length stem-list) 1)
						(string= pat (car word-list))
						(< (length pat) 4)
						(append
						 (sdic-search-multi-dictionaries sdic-eiwa-symbol-list pat 'lambda)
						 (sdic-search-multi-dictionaries sdic-eiwa-symbol-list (concat pat " "))))
					       (sdic-search-multi-dictionaries sdic-eiwa-symbol-list pat)))
				     (if orig
					 (setq stem-list (copy-sequence orig)
					       orig (if (member (car word-list) orig)
							(car word-list)
						      (car (sort orig (lambda (a b) (> (length a) (length b))))))
					       pat (format "^\\(%s\\)"
							   (mapconcat (lambda (w)
									(format "%s$\\|%s "
										(regexp-quote w)
										(regexp-quote w)))
								      stem-list "\\|")))
				       (setq orig pat
					     pat (concat "^" (regexp-quote pat)))
				       (message "Can't find original form of \"%s\""
						(car word-list))
				       ))))))
		(setq pat (if (nth 1 word-list)
			      (concat "^\\("
				      (mapconcat (lambda (w)
						   (format "%s +%s$\\|%s +%s "
							   (regexp-quote w)
							   (regexp-quote (nth 1 word-list))
							   (regexp-quote w)
							   (regexp-quote (nth 1 word-list))))
						 stem-list "\\|")
				      (if (string= orig (car word-list))
					  "\\)"
					(format "\\|%s\\)" (regexp-quote orig))))
			    (format "^%s$" (regexp-quote orig))))))
	(setq sdic-buffer-start-point
	      (if point
		  (progn
		    (setq word-list (sdic-split-string query)
			  orig (car word-list))
		    (if (and (not (string= str orig))
			     (string= orig (downcase orig))
			     (let ((case-fold-search nil))
			       (goto-char point)
			       (setq pat (regexp-quote (car (sdic-split-string query))))
			       (search-forward-regexp (format "^\\(%s \\|%s$\\)" pat pat) nil t)))
			(match-beginning 0)
		      point))
		(point-min)))))))


;; 和英辞典を検索する関数
(defun sdic-search-waei-dictionary (query)
  (sdic-decide-query-type sdic-waei-symbol-list query
    ;; 特に指定がない場合 -> 前方一致検索
    (sdic-insert-entry-list
     (sdic-search-multi-dictionaries sdic-waei-symbol-list query))))




;;;----------------------------------------------------------------------
;;;		本体
;;;----------------------------------------------------------------------

(defun sdic-version () "\
SDIC のバージョンを返す関数"
  (interactive)
  (message "SDIC %s" sdic-version))


(defun sdic-word-at-point () "\
カーソル位置の単語を返す関数"
  (save-excursion
    (if (not (looking-at "\\<")) (forward-word -1))
    (if (looking-at sdic-english-prep-regexp)
	(let ((strs
	       (sdic-split-string
		(sdic-buffer-substring-no-properties
		 (progn (forward-word -1) (point)) (progn (forward-word 2) (point))))))
	  (if (string-match "\\cj" (car strs))
	      (car (cdr strs))
	    (concat (car strs) " " (car (cdr strs)))))
      (sdic-buffer-substring-no-properties (point) (progn (forward-word 1) (point))))))


(defvar sdic-read-minibuffer-history '() "\
sdic-read-from-minibuffer 関数のヒストリ")
(defun sdic-read-from-minibuffer (&optional init pre-prompt)
  "ミニバッファから単語を読みとる"
  (let ((w (or init (sdic-word-at-point) "")))
    (setq sdic-read-minibuffer-history (cons w sdic-read-minibuffer-history)
	  w (read-from-minibuffer (if pre-prompt
				      (format "%s Input word : " pre-prompt)
				    "Input word : ")
				  w nil nil '(sdic-read-minibuffer-history . 1)))
    (if (>= (length w) 2) w
      (read-from-minibuffer (format "\"%s\" is too short. Input word again : " w)
			    w nil nil '(sdic-read-minibuffer-history . 1)))))


(defun sdic-select-search-function ()
  "検索関数を選ぶ"
  (message "辞書を選んで下さい: E)英和 J)和英")
  (let ((sw (selected-window))
	(c (read-char)))
    (select-window sw)
    (cond
     ((or (= c ?e) (= c ?E)) 'sdic-search-eiwa-dictionary)
     ((or (= c ?j) (= c ?J)) 'sdic-search-waei-dictionary)
     (t (sdic-select-search-function)))))


;; 単語を辞書で調べる関数
(defun sdic-describe-word (word &optional search-function)
  "Display the meaning of word."
  (interactive
   (let ((f (if current-prefix-arg (sdic-select-search-function)))
	 (w (sdic-read-from-minibuffer)))
     (list w f)))
  (set-buffer (get-buffer-create sdic-buffer-name))
  (or (string= mode-name sdic-mode-name) (sdic-mode))
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((case-fold-search t)
	(sdic-buffer-start-point (point-min)))
    (if (prog1 (funcall (or search-function
			    (if (string-match "\\cj" word)
				'sdic-search-waei-dictionary
			      'sdic-search-eiwa-dictionary))
			word)
	  (setq buffer-read-only t)
	  (set-buffer-modified-p nil))
	(sdic-display-buffer sdic-buffer-start-point)
      (message "Can't find word, \"%s\"." word)
      nil)))


;; 主関数の宣言
(defalias 'sdic 'sdic-describe-word)


(defun sdic-describe-region (start end &optional search-function)
  "Display the meaning of pattern."
  (interactive
   (list (region-beginning)
	 (region-end)
	 (if current-prefix-arg (sdic-select-search-function))))
  (sdic-describe-word (buffer-substring start end) search-function))


(defun sdic-describe-word-at-point (&optional search-function)
  "Display the meaning of word at point in Japanese."
  (interactive (list (if current-prefix-arg (sdic-select-search-function))))
  (let ((orig-table (copy-syntax-table))
	word)
    (unwind-protect
	(progn
	  (modify-syntax-entry ?* "w")
	  (modify-syntax-entry ?' "w")
	  (modify-syntax-entry ?/ "w")
	  (setq word (or (sdic-word-at-point) (sdic-read-from-minibuffer))))
      (set-syntax-table orig-table))
    (or (sdic-describe-word word search-function)
	(sdic-describe-word (sdic-read-from-minibuffer word (format "Can't find word \"%s\"." word))
			    search-function))))


;;; 次の項目に移動する関数
(defun sdic-forward-item ()
  "Move point to the next item."
  (interactive)
  (let ((o))
    (goto-char (sdic-next-overlay-change
		(if (setq o (car (sdic-overlays-at (point))))
		    (sdic-overlay-end o)
		  (point))))))


;;; 前の項目に移動する関数
(defun sdic-backward-item ()
  "Move point to the previous item."
  (interactive)
  (let ((o))
    (goto-char (sdic-previous-overlay-change
		(sdic-previous-overlay-change
		 (if (setq o (car (sdic-overlays-at (point))))
		     (sdic-overlay-start o)
		   (sdic-previous-overlay-change (sdic-previous-overlay-change (point)))))))))


(defun sdic-goto-point-min () "\
バッファの先頭に移動する関数"
  (interactive)
  (goto-char (point-min)))


(defun sdic-goto-point-max () "\
バッファの末尾に移動する関数"
  (interactive)
  (goto-char (point-max)))


(defun sdic-display-buffer (&optional start-point) "\
検索結果表示バッファを表示する関数"
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let* ((buf (set-buffer sdic-buffer-name))
	       (w1 (selected-window))
	       (w2 (get-buffer-window buf))
	       (p (or start-point (point)))
	       (h sdic-window-height))
	  (if w2 (progn (select-window w2) (setq h (window-height w2)))
	    (setq w2 (select-window (if (one-window-p)
					(split-window w1 (- (window-height) h))
				      (next-window))))
	    (set-window-buffer w2 buf))
	  (set-window-start w2 p)
	  (and sdic-warning-hidden-entry
	       (> p (point-min))
	       (message "この前にもエントリがあります。"))
	  (goto-char p)
	  (if sdic-disable-select-window (select-window w1))
	  (buffer-size))
      (set-buffer old-buffer))))


(defun sdic-other-window () "\
検索表示バッファから元のバッファに戻る関数"
  (interactive)
  (let ((w (selected-window)))
    (if (and (string= (buffer-name (window-buffer w))
		      sdic-buffer-name)
	     (one-window-p))
	(progn
	  (split-window w (- (window-height) sdic-window-height))
	  (set-window-buffer w (other-buffer)))
      (other-window -1))))


(defun sdic-close-window () "\
検索表示バッファを表示しているウインドウを消去する関数"
  (interactive)
  (let ((w (get-buffer-window sdic-buffer-name))
	(b (get-buffer sdic-buffer-name)))
    (if w
	(progn
	  (bury-buffer b)
	  (if (= (window-height w) sdic-window-height)
	      (delete-window w)
	    (set-window-buffer w (other-buffer))
	    (select-window (next-window)))
	  ))))


(defun sdic-exit () "\
検索結果表示バッファを削除する関数"
  (interactive)
  (if (sdicf-buffer-live-p (get-buffer sdic-buffer-name))
      (progn
	(sdic-close-window)
	(kill-buffer sdic-buffer-name)))
  (mapcar 'sdic-close-dictionary sdic-eiwa-symbol-list)
  (mapcar 'sdic-close-dictionary sdic-waei-symbol-list)
  (setq sdic-eiwa-symbol-list nil
	sdic-waei-symbol-list nil))


;;; 辞書を閲覧する major-mode
(defun sdic-mode () "\
辞書を閲覧するメジャーモード

次のような形式の文字列を入力することによって検索方式を指定できます。

'word'          完全一致検索
word*           前方一致検索
*word           後方一致検索
/word           全文検索

これら以外の場合は、通常のキーワード検索を行います。


key             binding
---             -------

w               単語を検索する
'               完全一致検索をする
^               前方一致検索をする
$               後方一致検索をする
/               全文検索をする
W               辞書を指定して検索する
SPC             スクロールアップ
b               スクロールダウン ( BS / Delete キーも使えます )
n               次の項目
TAB             次の項目
p               前の項目
M-TAB           前の項目
o               辞書を閲覧しているウインドウから他のウインドウに移る
q               辞書を閲覧しているウインドウを消す
Q               SDIC を終了する
<               バッファの先頭に移動
>               バッファの終端に移動
?               ヘルプ表示
"
  (kill-all-local-variables)
  (make-local-variable 'fill-column)
  (setq major-mode 'sdic-mode
	mode-name   sdic-mode-name
	fill-column sdic-fill-column
	left-margin sdic-left-margin
	sdic-mode-map (make-keymap))
  ;; キーバインドの設定
  (define-key sdic-mode-map " " 'scroll-up)
  (define-key sdic-mode-map "b" 'scroll-down)
  (define-key sdic-mode-map [backspace] 'scroll-down)
  (define-key sdic-mode-map [delete] 'scroll-down)
  (define-key sdic-mode-map "\C-?" 'scroll-down)
  (define-key sdic-mode-map "n" 'sdic-forward-item)
  (define-key sdic-mode-map "\t" 'sdic-forward-item)
  (define-key sdic-mode-map "p" 'sdic-backward-item)
  (define-key sdic-mode-map "\M-\t" 'sdic-backward-item)
  (define-key sdic-mode-map "o" 'sdic-other-window)
  (define-key sdic-mode-map "q" 'sdic-close-window)
  (define-key sdic-mode-map "Q" 'sdic-exit)
  (define-key sdic-mode-map "w" 'sdic-describe-word)
  (define-key sdic-mode-map "W" (lambda ()
				  (interactive)
				  (let ((f (sdic-select-search-function)))
				    (sdic-describe-word (sdic-read-from-minibuffer) f))))
  (define-key sdic-mode-map "/" (lambda ()
				  (interactive)
				  (sdic-describe-word (sdic-read-from-minibuffer
						       (concat "/" (sdic-word-at-point))))))
  (define-key sdic-mode-map "^" (lambda ()
				  (interactive)
				  (sdic-describe-word (sdic-read-from-minibuffer
						       (concat (sdic-word-at-point) "*")))))
  (define-key sdic-mode-map "$" (lambda ()
				  (interactive)
				  (sdic-describe-word (sdic-read-from-minibuffer
						       (concat "*" (sdic-word-at-point))))))
  (define-key sdic-mode-map "'" (lambda ()
				  (interactive)
				  (sdic-describe-word (sdic-read-from-minibuffer
						       (concat "'" (sdic-word-at-point) "'")))))
  (define-key sdic-mode-map "<" 'sdic-goto-point-min)
  (define-key sdic-mode-map ">" 'sdic-goto-point-max)
  (define-key sdic-mode-map "?" 'describe-mode)
  (cond
   ((not sdic-disable-vi-key)
    (define-key sdic-mode-map "h" 'backward-char)
    (define-key sdic-mode-map "j" 'next-line)
    (define-key sdic-mode-map "k" 'previous-line)
    (define-key sdic-mode-map "l" 'forward-char)))
  (use-local-map sdic-mode-map)
  ;; 見出し語の face の設定
  (make-face 'sdic-face)
  (and (or (not (fboundp 'facep))
	   (facep sdic-face-style))
       (copy-face sdic-face-style 'sdic-face))
  (and window-system
       sdic-face-color
       (set-face-foreground 'sdic-face sdic-face-color))
  ;; それぞれの辞書を初期化する
  (or sdic-eiwa-symbol-list
      (setq sdic-eiwa-symbol-list
	    (delq nil (mapcar 'sdic-init-dictionary sdic-eiwa-dictionary-list))))
  (setq sdic-eiwa-symbol-list (delq nil (mapcar 'sdic-open-dictionary sdic-eiwa-symbol-list)))
  (or sdic-waei-symbol-list
      (setq sdic-waei-symbol-list
	    (delq nil (mapcar 'sdic-init-dictionary sdic-waei-dictionary-list))))
  (setq sdic-waei-symbol-list (delq nil (mapcar 'sdic-open-dictionary sdic-waei-symbol-list)))
  (run-hooks 'sdic-mode-hook))




;;;----------------------------------------------------------------------
;;;		各種 Emacsen の違いを吸収する関数
;;;----------------------------------------------------------------------

(if (fboundp 'next-overlay-change)
    (defalias 'sdic-next-overlay-change 'next-overlay-change)
  ;; XEmacs の場合
  (defun sdic-next-overlay-change (pos) "\
Return the next position after POS where an extent starts or ends.
If there are no more extent boundaries after POS, return (point-max)."
    (catch 'found-next-extent
      (mapcar (lambda (ext)
		(cond
		 ((> (extent-start-position ext) pos)
		  (throw 'found-next-extent (extent-start-position ext)))
		 ((> (extent-end-position ext) pos)
		  (throw 'found-next-extent (extent-end-position ext)))
		 ))
	      (extent-list))
      (point-max))))


(if (fboundp 'previous-overlay-change)
    (defalias 'sdic-previous-overlay-change 'previous-overlay-change)
  (if (fboundp 'extent-list)
      ;; XEmacs の場合
      (defun sdic-previous-overlay-change (pos) "\
Return the previous position before POS where an extent starts or ends.
If there are no more extent boundaries before POS, return (point-min)."
	(catch 'found-previous-extent
	  (mapcar (lambda (ext)
		    (cond
		     ((< (extent-end-position ext) pos)
		      (throw 'found-previous-extent (extent-end-position ext)))
		     ((< (extent-start-position ext) pos)
		      (throw 'found-previous-extent (extent-start-position ext)))
		     ))
		  (reverse (extent-list)))
	  (point-min)))
    ;; Emacs 19.34 以前の場合
    (defun sdic-previous-overlay-change (pos) "\
Return the previous position before POS where an overlay starts or ends.
If there are no more overlay boundaries before POS, return (point-min)."
      (if (> pos (point-max))
	  (error "Specified position is larger than point-max"))
      (save-excursion
	(let (next (prev (point-min)))
	  (while (> pos (setq next (sdic-next-overlay-change prev)))
	    (setq prev next))
	  prev)))))


(if (fboundp 'overlays-at)
    (defalias 'sdic-overlays-at 'overlays-at)
  (defun sdic-overlays-at (pos) "\
Return a list of the extents that contain position POS."
    (delq nil (mapcar (lambda (ext)
			(and (<= (extent-start-position ext) pos)
			     (> (extent-end-position ext) pos)
			     ext))
		      (extent-list)))))


(if (fboundp 'overlay-put)
    (defalias 'sdic-overlay-put 'overlay-put)
  (defalias 'sdic-overlay-put 'set-extent-property))


(if (fboundp 'make-overlay)
    (defalias 'sdic-make-overlay 'make-overlay)
  (defalias 'sdic-make-overlay 'make-extent))


(if (fboundp 'overlay-start)
    (defalias 'sdic-overlay-start 'overlay-start)
  (defalias 'sdic-overlay-start 'extent-start-position))


(if (fboundp 'overlay-end)
    (defalias 'sdic-overlay-end 'overlay-end)
  (defalias 'sdic-overlay-end 'extent-end-position))


(if (fboundp 'match-string)
    (defalias 'sdic-match-string 'match-string)
  ;; Introduced in Emacs 19.29.
  (defun sdic-match-string (num &optional string) "\
Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
    (if (match-beginning num)
	(if string
	    (substring string (match-beginning num) (match-end num))
	  (buffer-substring (match-beginning num) (match-end num))))))


(defun sdic-split-string (string &optional separators) "\
Splits STRING into substrings where there are matches for SEPARATORS.
Each match for SEPARATORS is a splitting point.
The substrings between the splitting points are made into a list
which is returned.
If SEPARATORS is absent, it defaults to \"[ \\f\\t\\n\\r\\v]+\".

If there is match for SEPARATORS at the beginning of STRING, we do not
include a null substring for that.  Likewise, if there is a match
at the end of STRING, we don't include a null substring for that."
  (or separators (setq separators "[ \f\t\n\r\v]+"))
  (let (list (start 0))
    (while (string-match separators string start)
      (or (= start (match-beginning 0))
	  (setq list (cons (substring string start (match-beginning 0)) list)))
      (setq start (match-end 0)))
    (nreverse (if (= start (length string)) list (cons (substring string start) list)))))


(if (fboundp 'buffer-substring-no-properties)
    (defalias 'sdic-buffer-substring-no-properties 'buffer-substring-no-properties)
  (defun sdic-buffer-substring-no-properties (start end) "\
Return the characters of part of the buffer, without the text properties.
The two arguments START and END are character positions;
they can be in either order. [Emacs 19.29 emulating function]"
    (let ((string (buffer-substring start end)))
      (set-text-properties 0 (length string) nil string)
      string)))
