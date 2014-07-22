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

;; ����/�±Ѽ�������������Ū�Ǻ������� major mode �Ǥ���
;; ���ѵڤӺ����ۤκݤϡ�GNU ���̸��ѵ������Ŭ���ʥС������ˤ�������
;; �Ʋ�������

;; �켡���۸�
;;    http://namazu.org/~tsuchiya/sdic/


;;; Install:

;; (1) sdic.el, sdicf.el, sdicf-client.el, sdic-compat.el, sdic-gene.el
;;     �� stem.el ��Ŭ���ʾ�����¸���ơ�ɬ�פʤ�Х��ȥ���ѥ��뤷��
;;     ��������
;;
;;
;; (2) sdicf-client.el, sdic-compat.el, sdic-gene.el �ϼ���򸡺�����
;;     ����Υ饤�֥��Ǥ��������Υ饤�֥��Τɤ줫��ȤäƼ����
;;     �����Ǥ���褦�ˤ��Ʋ��������ܺ٤ˤĤ��Ƥϡ�README �Ȥ��줾���
;;     �������ե�����򻲾ȡ�
;;
;;
;; (3) �Ȥ���褦�ˤ�������Υꥹ�Ȥ�sdic-eiwa-dictionary-list ����
;;     �� sdic-waei-dictionary-list �����ꤷ�ޤ����㤨�С����¼���
;;     /usr/dict/gene.sdic �� sdicf-client.el ��ȤäƸ���������ϼ�
;;     �Τ褦�ˤʤ�ޤ���
;;
;;         (setq sdic-eiwa-dictionary-list '((sdicf-client "/usr/dict/gene.sdic")))
;;
;;     ʣ���μ����Ʊ���˸������뤳�Ȥ����ޤ���
;;
;;         (setq sdic-waei-dictionary-list '((sdicf-client "~/data/jedict.sdic")
;;                                           (sdic-compat "/usr/dict/jgene.dic")))
;;
;;     ��������Ѥ��ʤ����� nil ���������Ʋ��������ޤ�������������
;;     �� ~/.emacs �ʤɤ�Ŭ�ڤʾ��˽񤭹���ǲ�������
;;
;;
;; (4) ~/.emacs �˼��Τ褦�ʥ����ɤ��������ޤ���
;;
;;         (autoload 'sdic-describe-word "sdic" "��ñ��ΰ�̣��Ĵ�٤�" t nil)
;;         (global-set-key "\C-cw" 'sdic-describe-word)
;;
;;     ���ߤ˹�碌��Ŭ���˥����Х���ɤ��ѹ����Ʋ�������


;;; Note:

;; ������̤�ɽ���λ�����ư������椹���ѿ�������ޤ����ܺ٤ˤĤ��Ƥϡ�
;; ���� source �򻲾Ȥ��Ʋ�������
;;
;; grep / array �ʤɤγ������ޥ�ɤ����Ѥ��Ƽ��񸡺�������ϡ������
;; �γ������ޥ�ɤ��б����Ƥ�����������ɤ����ꤷ�ơ�����⤽�δ�������
;; �ɤ˹�碌��ɬ�פ�����ޤ������ξ�硢���ܸ� EUC �������餯���ְ���
;; �Ǥ��礦��
;;
;; Emacs20 �ޤ��� XEmacs �ǻ��Ѥ�����ϡ�
;;
;;     (set-language-environment "Japanese")
;;
;; �� .emacs �����ꤵ��Ƥ��뤫��ǧ���Ƥ���������


(provide 'sdic)
(require 'sdicf)
(require 'stem)


;;;----------------------------------------------------------------------
;;;		�������ޥ������ѿ�
;;;----------------------------------------------------------------------

(defvar sdic-left-margin 2 "*Left margin of contents.
����ʸ�κ�¦��;����")

(defvar sdic-fill-column default-fill-column "*Right edge of contents.
����ʸ������������")

(defvar sdic-window-height 10 "*Height of window to show entrys and contents.
�������ɽ��������ɥ��ι⤵")

(defvar sdic-warning-hidden-entry t "*If non-nil, warning of hidden entries is enable.
nil �ʳ������ꤵ��Ƥ����硢�������ɽ��������ɥ���ɽ��������ʤ��ä����󤬤���зٹ𤹤�")

(defvar sdic-disable-select-window nil "*Option to disable to select other window.
�������ɽ��������ɥ��˥���������ư���ʤ��褦�ˤ������ nil �ʳ������ꤹ��")

(defvar sdic-face-style 'bold  "*Style of entry.
���Ф����ɽ�����뤿��˻Ȥ���������")

(defvar sdic-face-color nil "*Color of entry.
���Ф����ɽ�����뤿��˻Ȥ���")

(defvar sdic-disable-vi-key nil "*Option to disable some key.
��������� vi �饤���Υ�����Ȥ�ʤ����� nil �ʳ������ꤹ��")

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
	     '(""))) "\
*Options of an English-Japanese dictionary.
���¼�ŵ�θ����᥽�åɤΥꥹ�Ȥ���ꤹ���ѿ�")

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
	     '(""))) "\
*Options of an English-Japanese dictionary.
�±Ѽ�ŵ�θ����᥽�åɤΥꥹ�Ȥ���ꤹ���ѿ�")

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
;;;		�����ѿ�
;;;----------------------------------------------------------------------

(defvar sdic-english-prep-list '("at" "by" "for" "in" "on" "of" "with" "as" "before" "after") "\
List of English prepositions
�Ѹ�����ֻ�Υꥹ��")

(defvar sdic-english-prep-regexp
  (format "\\(%s\\)\\b" (mapconcat 'regexp-quote sdic-english-prep-list "\\|")) "\
Regexp of Englist prepositions
�Ѹ�����ֻ�ȥޥå���������ɽ��")

(defvar sdic-eiwa-symbol-list nil "���¼�ŵ�Υ���ܥ�")
(defvar sdic-waei-symbol-list nil "�±Ѽ�ŵ�Υ���ܥ�")
(defvar sdic-buffer-start-point nil "�������ɽ���Хåե���ɽ�����ϥݥ����")
(defvar sdic-mode-map nil "Keymap of sdic-mode")

(defvar sdic-kinsoku-bol-list
  (funcall (if (fboundp 'string-to-char-list)
	       'string-to-char-list
	     'string-to-list)
	   "!)-_~}]:;',.?�����������������������������������������������������������������¡áġšǡɡˡ͡ϡѡӡաס١ۡ������������ä������������å�������")
  "��Ƭ��§ʸ���Υꥹ��")

(defvar sdic-kinsoku-eol-list
  (funcall (if (fboundp 'string-to-char-list)
	       'string-to-char-list
	     'string-to-list)
	   "({[`�ơȡʡ̡ΡСҡԡ֡ءڡ����")
  "������§ʸ���Υꥹ��")

(defvar sdic-kinsoku-spc-list
  (funcall (if (fboundp 'string-to-char-list)
	       'string-to-char-list
	     'string-to-list)
	   "\t ��")
  "����ʸ���Υꥹ��")

(defconst sdic-version "2.1.3")
(defconst sdic-buffer-name "*sdic*" "�������ɽ���Хåե���̾��")
(defconst sdic-mode-name "SDIC" "������̤�ɽ������Хåե��� major mode")




;;;----------------------------------------------------------------------
;;;		�����᥽�åɤ�ƤӽФ��ؿ�
;;;----------------------------------------------------------------------

(defun sdic-init-dictionary (option-list) "\
Function to initialize dictionary.
���ꤵ�줿����ȴ�Ϣ�դ����Ƥ��븡���饤�֥�����������ؿ�"
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
���ꤵ�줿����򸡺��Ǥ���褦�ˤ���ؿ�"
  (and (sdic-dictionary-symbol-p dic)
       (funcall (get (get dic 'search-method) 'open-dictionary) dic)))


(defun sdic-close-dictionary (dic) "\
Function to close dictionary.
���ꤵ�줿����ȴ�Ϣ�դ����Ƥ��븡���饤�֥���λ����ؿ�"
  (and (sdic-dictionary-symbol-p dic)
       (funcall (get (get dic 'search-method) 'close-dictionary) dic)))


(defun sdic-search-entry (dic word &optional search-type) "\
Function to search word in dictionary.
���ꤵ�줿����򸡺�����ؿ�
���Ф��졢���񥷥�ܥ롢���Ф����ID����ʤ���������ǤȤ���������֤���"
  (mapcar (lambda (c)
	    (list (car c) dic (cdr c)))
	  (funcall (get (get dic 'search-method) 'search-entry) dic word search-type)))


(defsubst sdic-replace-string (string from to) "\
ʸ���� STRING �˴ޤޤ�Ƥ���ʸ���� FROM ������ʸ���� TO ���ִ�����ʸ������֤�
FROM �ˤ�����ɽ����ޤ�ʸ��������Ǥ��뤬��TO �ϸ���ʸ���󤷤�����Ǥ��ʤ���"
  (let ((start 0) list)
    (while (string-match from string start)
      (setq list (cons to (cons (substring string start (match-beginning 0)) list))
	    start (match-end 0)))
    (eval (cons 'concat (nreverse (cons (substring string start) list))))))


(defun sdic-sort-dictionary-order (entry-list) "\
Function to sort entry list in dictionary order.
���Ф��졢���񥷥�ܥ롢���Ф����ID����ʤ���������ǤȤ������� 
ENTRY-LIST �򡢸��Ф���μ������¤��ؤ���ؿ���"
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
���ꤵ��Ƥ���ʣ���μ������ɸ�������ؿ�
���Ф��졢���񥷥�ܥ롢���Ф����ID����ʤ���������ǤȤ���������֤���"
  (sdic-sort-dictionary-order
   (apply 'append
	  (mapcar (lambda (dic)
		    (sdic-search-entry dic word search-type))
		  dic-list))))


(defun sdic-get-content (dic id) "\
Function to get content.
���ꤵ��Ƥ��뼭�񤫤����ʸ���ɤ߽Ф��ؿ�"
  (funcall (get (get dic 'search-method) 'get-content) dic id))


(defun sdic-make-dictionary-symbol ()
  (make-symbol "sdic-dictionary"))


(defun sdic-dictionary-symbol-p (symbol)
  (equal (symbol-name symbol) "sdic-dictionary"))




;;;----------------------------------------------------------------------
;;;		�����ؿ�
;;;----------------------------------------------------------------------

(defun sdic-insert-content (word content)
  "���Ф��������ʸ���������ʤ�����������"
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
  "���Ф��������ʸ���������ʤ�����������"
  (mapcar (lambda (entry)
	    (sdic-insert-content (car entry) (sdic-get-content (nth 1 entry) (nth 2 entry)))
	    (car entry))
	  entry-list))


;; ����������Ƚ�̤���ޥ���
(put 'sdic-decide-query-type 'lisp-indent-function 2)
(defmacro sdic-decide-query-type (dic-list query &rest sexp) "\
QUERY ���鸡��������Ƚ�ꤷ��ʣ���μ��� DIC-LIST �򸡺�����ޥ���
QUERY �˸�����������ꤹ�빽¤���ޤޤ�Ƥ��ʤ����ϡ�default ��ư���
���� SEXP ��ɾ�����롣�̾�θ����ξ��ϡ��������줿���Ф���Υꥹ�Ȥ�
�֤���"
  (` (cond
      ;; �����줬 '' �ǰϤޤ�Ƥ����� -> �������׸���
      ((and (equal ?' (string-to-char (, query)))
	    (equal "'" (substring (, query) -1)))
       (sdic-insert-entry-list
	(sdic-search-multi-dictionaries (, dic-list) (substring (, query) 1 -1) 'lambda)))
      ;; ���������Ƭ�� / �������� -> ��ʸ����
      ((equal ?/ (string-to-char (, query)))
       (sdic-insert-entry-list
	(sdic-search-multi-dictionaries (, dic-list) (substring (, query) 1) 0)))
      ;; ���������Ƭ�� * �������� -> �������׸���
      ((equal ?* (string-to-char (, query)))
       (sdic-insert-entry-list
	(sdic-search-multi-dictionaries (, dic-list) (substring (, query) 1) t)))
      ;; ������������� * �������� -> �������׸���
      ((equal "*" (substring (, query) -1))
       (sdic-insert-entry-list
	(sdic-search-multi-dictionaries (, dic-list) (substring (, query) 0 -1))))
      ;; �ä˻��꤬�ʤ���� -> ���ꤵ�줿 S ����ɾ��
      (t
       (,@ sexp)))))


;; ���¼�ŵ�򸡺�����ؿ�
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
		      ;; (1) �Ե�§�Ѳ�ư��򸡺�����
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
		      ;; (2) �Ե�§�Ѳ�ư���̵�뤷�� stemming ��ԤʤäƸ�������
		      (progn
			(setq stem-list (let ((stem:irregular-verb-alist nil))
					  (stem:stripping-suffix (car word-list))))
			(if (> (length (car word-list)) 1)
			    (setq stem-list
				  (delq t (mapcar (lambda (w) (or (= (length w) 1) w)) stem-list))))
			;; ��Ĺ��ʬ������
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
					     ;; ������̤��鸶���ȿ��ꤵ��븫�Ф���򸡺�
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


;; �±Ѽ�ŵ�򸡺�����ؿ�
(defun sdic-search-waei-dictionary (query)
  (sdic-decide-query-type sdic-waei-symbol-list query
    ;; �ä˻��꤬�ʤ���� -> �������׸���
    (sdic-insert-entry-list
     (sdic-search-multi-dictionaries sdic-waei-symbol-list query))))




;;;----------------------------------------------------------------------
;;;		����
;;;----------------------------------------------------------------------

(defun sdic-version () "\
SDIC �ΥС��������֤��ؿ�"
  (interactive)
  (message "SDIC %s" sdic-version))


(defun sdic-word-at-point () "\
����������֤�ñ����֤��ؿ�"
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
sdic-read-from-minibuffer �ؿ��Υҥ��ȥ�")
(defun sdic-read-from-minibuffer (&optional init pre-prompt)
  "�ߥ˥Хåե�����ñ����ɤߤȤ�"
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
  "�����ؿ�������"
  (message "���������ǲ�����: E)���� J)�±�")
  (let ((sw (selected-window))
	(c (read-char)))
    (select-window sw)
    (cond
     ((or (= c ?e) (= c ?E)) 'sdic-search-eiwa-dictionary)
     ((or (= c ?j) (= c ?J)) 'sdic-search-waei-dictionary)
     (t (sdic-select-search-function)))))


;; ñ��򼭽��Ĵ�٤�ؿ�
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


;; ��ؿ������
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


;;; ���ι��ܤ˰�ư����ؿ�
(defun sdic-forward-item ()
  "Move point to the next item."
  (interactive)
  (let ((o))
    (goto-char (sdic-next-overlay-change
		(if (setq o (car (sdic-overlays-at (point))))
		    (sdic-overlay-end o)
		  (point))))))


;;; ���ι��ܤ˰�ư����ؿ�
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
�Хåե�����Ƭ�˰�ư����ؿ�"
  (interactive)
  (goto-char (point-min)))


(defun sdic-goto-point-max () "\
�Хåե��������˰�ư����ؿ�"
  (interactive)
  (goto-char (point-max)))


(defun sdic-display-buffer (&optional start-point) "\
�������ɽ���Хåե���ɽ������ؿ�"
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
	       (message "�������ˤ⥨��ȥ꤬����ޤ���"))
	  (goto-char p)
	  (if sdic-disable-select-window (select-window w1))
	  (buffer-size))
      (set-buffer old-buffer))))


(defun sdic-other-window () "\
����ɽ���Хåե����鸵�ΥХåե������ؿ�"
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
����ɽ���Хåե���ɽ�����Ƥ��륦����ɥ���õ��ؿ�"
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
�������ɽ���Хåե���������ؿ�"
  (interactive)
  (if (sdicf-buffer-live-p (get-buffer sdic-buffer-name))
      (progn
	(sdic-close-window)
	(kill-buffer sdic-buffer-name)))
  (mapcar 'sdic-close-dictionary sdic-eiwa-symbol-list)
  (mapcar 'sdic-close-dictionary sdic-waei-symbol-list)
  (setq sdic-eiwa-symbol-list nil
	sdic-waei-symbol-list nil))


;;; ������������ major-mode
(defun sdic-mode () "\
������������᥸�㡼�⡼��

���Τ褦�ʷ�����ʸ��������Ϥ��뤳�Ȥˤ�äƸ������������Ǥ��ޤ���

'word'          �������׸���
word*           �������׸���
*word           �������׸���
/word           ��ʸ����

�����ʳ��ξ��ϡ��̾�Υ�����ɸ�����Ԥ��ޤ���


key             binding
---             -------

w               ñ��򸡺�����
'               �������׸����򤹤�
^               �������׸����򤹤�
$               �������׸����򤹤�
/               ��ʸ�����򤹤�
W               �������ꤷ�Ƹ�������
SPC             �������륢�å�
b               ������������� ( BS / Delete ������Ȥ��ޤ� )
n               ���ι���
TAB             ���ι���
p               ���ι���
M-TAB           ���ι���
o               �����������Ƥ��륦����ɥ�����¾�Υ�����ɥ��˰ܤ�
q               �����������Ƥ��륦����ɥ���ä�
Q               SDIC ��λ����
<               �Хåե�����Ƭ�˰�ư
>               �Хåե��ν�ü�˰�ư
?               �إ��ɽ��
"
  (kill-all-local-variables)
  (make-local-variable 'fill-column)
  (setq major-mode 'sdic-mode
	mode-name   sdic-mode-name
	fill-column sdic-fill-column
	left-margin sdic-left-margin
	sdic-mode-map (make-keymap))
  ;; �����Х���ɤ�����
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
  ;; ���Ф���� face ������
  (make-face 'sdic-face)
  (and (or (not (fboundp 'facep))
	   (facep sdic-face-style))
       (copy-face sdic-face-style 'sdic-face))
  (and window-system
       sdic-face-color
       (set-face-foreground 'sdic-face sdic-face-color))
  ;; ���줾��μ������������
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
;;;		�Ƽ� Emacsen �ΰ㤤��ۼ�����ؿ�
;;;----------------------------------------------------------------------

(if (fboundp 'next-overlay-change)
    (defalias 'sdic-next-overlay-change 'next-overlay-change)
  ;; XEmacs �ξ��
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
      ;; XEmacs �ξ��
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
    ;; Emacs 19.34 �����ξ��
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
