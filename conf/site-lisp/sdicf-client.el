;; sdicf-client.el ---- -*- Emacs-Lisp -*- Library to search SDIC form dictionary.
;; $Id: sdicf-client.el,v 1.4 2002/07/02 11:17:46 tsuchiya Exp $

;; Copyright (C) 1998,99 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: dictionary

;; This file is part of SDIC.

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

;; SDIC 形式の辞書を sdicf.el を利用して検索するライブラリです。


;;; Install:

;; (1) 辞書を適切な形式に変換して、適当な場所( 例: /usr/dict/ )に保存
;;     して下さい。辞書変換用スクリプトとして以下の Perl スクリプトが
;;     利用できます。
;;
;;         gene.perl    - GENE95 辞書
;;         edict.perl   - EDICT 辞書
;;         eijirou.perl - 英辞郎
;;
;; (2) 使えるようにした辞書の定義情報を sdic-eiwa-dictionary-list また
;;     は sdic-waei-dictionary-list に追加して下さい。
;;
;;         (setq sdic-eiwa-dictionary-list
;;               (cons '(sdicf-client "/usr/dict/gene.dic") sdic-eiwa-dictionary-list))
;;
;;     辞書定義情報は次のような構成になっています。
;;
;;         (sdicf-client ファイル名 (オプションA 値A) (オプションB 値B) ...)
;;
;;     特別な指定が不要な場合には、オプションは省略できます。
;;
;;         (sdicf-client ファイル名)


;;; Options:

;; sdicf-client.el に対して指定できるオプションは次の通りです。
;;
;; coding-system
;;     辞書の漢字コードを指定します。省略した場合は、
;;     sdic-default-coding-system の値を使います。
;;
;; title
;;     辞書のタイトルを指定します。省略した場合は、辞書ファイルの 
;;     basename をタイトルとします。
;;
;; add-keys-to-headword
;;     全ての検索キーを含めて見出し語を構成する場合に t に設定して下さ
;;     い。和英辞書を検索する場合に、振り仮名も含めて出力する場合に利
;;     用します。
;;
;; strategy
;;     sdicf.el を通して辞書を検索する時の strategy を指定します。省略
;;     した場合は、sdicf.el の自動判定によって選ばれた strategy を使用
;;     します。


;;; Note;

;; sdicf.el は SDIC 形式の辞書を検索するためのライブラリです。それぞれ
;; の違いは次の通りです。3種類の strategy がサポートされています。
;;
;; `direct'
;;     辞書データを全てメモリに読み込んでから検索を行います。外部コマ
;;     ンドを必要としませんが、大量のメモリが必要になります。
;;
;; `grep'
;;     fgrep を利用して検索を行います。
;;
;; `array'
;;     array を利用して検索を行います。辞書の index file を事前に生成
;;     しておいてから検索を行いますので、高速に検索が可能です。しかし、
;;     index file は辞書の3倍程度の大きさになります。
;;
;; 比較的小規模の辞書を検索する場合は `grep' が最適でしょう。しかし、
;; 5MByte より大きい辞書の場合は `array' の利用を考慮すべきだと思いま
;; す。
;;
;; SDIC 形式の辞書の構造については、sdic.texi を参照してください。


;;; ライブラリ定義情報
(require 'sdic)
(require 'sdicf)
(provide 'sdicf-client)
(put 'sdicf-client 'version "2.0")
(put 'sdicf-client 'init-dictionary 'sdicf-client-init-dictionary)
(put 'sdicf-client 'open-dictionary 'sdicf-client-open-dictionary)
(put 'sdicf-client 'close-dictionary 'sdicf-client-close-dictionary)
(put 'sdicf-client 'search-entry 'sdicf-client-search-entry)
(put 'sdicf-client 'get-content 'sdicf-client-get-content)



;;;----------------------------------------------------------------------
;;;		本体
;;;----------------------------------------------------------------------

(defun sdicf-client-init-dictionary (file-name &rest option-list)
  "Function to initialize dictionary"
  (let ((dic (sdic-make-dictionary-symbol)))
    (if (file-readable-p (setq file-name (expand-file-name file-name)))
	(progn
	  (mapcar (lambda (c) (put dic (car c) (nth 1 c))) option-list)
	  (put dic 'file-name file-name)
	  (put dic 'identifier (concat "sdicf-client+" file-name))
	  (or (get dic 'title)
	      (put dic 'title (file-name-nondirectory file-name)))
	  (or (get dic 'coding-system)
	      (put dic 'coding-system sdic-default-coding-system))
	  dic)
      (error "Can't read dictionary: %s" (prin1-to-string file-name)))))


(defun sdicf-client-open-dictionary (dic)
  "Function to open dictionary"
  (if (put dic 'sdic-object
	   (sdicf-open (get dic 'file-name) (get dic 'coding-system) (get dic 'strategy)))
      dic))


(defun sdicf-client-close-dictionary (dic)
  "Function to close dictionary"
  (if (get dic 'sdic-object) (sdicf-close (get dic 'sdic-object))))


(defun sdicf-client-search-entry (dic string &optional search-type) "\
Function to search word with look or grep, and write results to current buffer.
search-type の値によって次のように動作を変更する。
    nil    : 前方一致検索
    t      : 後方一致検索
    lambda : 完全一致検索
    0      : 全文検索
検索結果として見つかった見出し語をキーとし、その定義文の先頭の point を値とする
連想配列を返す。
"
  (let ((case-fold-search t) list)
    (mapcar (if (get dic 'add-keys-to-headword)
		(lambda (entry)
		  (setq list (sdicf-entry-keywords entry))
		  (cons (if (= (length list) 1)
			    (car list)
			  (apply 'concat
				 (car list)
				 " "
				 (mapcar (lambda (s) (format "[%s]" s)) (cdr list))))
			entry))
	      (lambda (entry)
		(cons (sdicf-entry-headword entry) entry)))
	    (sdicf-search (get dic 'sdic-object)
			  (cond
			   ((not search-type) 'prefix)
			   ((eq search-type t) 'suffix)
			   ((eq search-type 'lambda) 'exact)
			   ((eq search-type 0) 'text)
			   (t (error "Illegal search method : %S" search-type)))
			  string))))


(defun sdicf-client-get-content (dic entry)
  (sdicf-entry-text entry))
