;; sdic-compat.el ---- -*- Emacs-Lisp -*- Library to search COMPAT format dictionary.
;; $Id: sdic-compat.el,v 2.5 2002/07/02 11:17:46 tsuchiya Exp $

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

;; COMPAT �����μ�������ץ����( look / grep )�����Ѥ��Ƹ�������
;; �饤�֥��Ǥ���COMPAT �����ξܺ٤ˤĤ��Ƥ� sdic.texi �򻲾Ȥ��Ʋ�
;; ������


;;; Install:

;; (1) look ����ʸ��/��ʸ���ΰ㤤��̵�뤷������������� grep ( fgrep 
;;     �ޤ��� GNU grep )��ɬ�פǤ����ޤ�������ɽ�����������Ѥ������ 
;;     egrep ��ɬ�פǤ����ѥ����̤äƤ��뤫��ǧ���Ʋ�������
;;
;; (2) �����Ŭ�ڤʷ������Ѵ����ơ�Ŭ���ʾ��( ��: /usr/dict/ )����¸
;;     ���Ʋ������������Ѵ��ѥ�����ץȤȤ��ưʲ��� Perl ������ץȤ�
;;     ���ѤǤ��ޤ���
;;
;;         gene.perl    - GENE95 ����
;;         jgene.perl   - GENE95 ���񤫤��±Ѽ������������
;;         eijirou.perl - �Ѽ�Ϻ
;;
;;     --compat ���ץ�������ꤹ��ɬ�פ�����ޤ���
;;
;; (3) �Ȥ���褦�ˤ���������������� sdic-eiwa-dictionary-list �ޤ�
;;     �� sdic-waei-dictionary-list ���ɲä��Ʋ�������
;;
;;         (setq sdic-eiwa-dictionary-list
;;               (cons '(sdic-compat "/usr/dict/gene.dic") sdic-eiwa-dictionary-list))
;;
;;     �����������ϼ��Τ褦�ʹ����ˤʤäƤ��ޤ���
;;
;;         (sdic-compat �ե�����̾ (���ץ����A ��A) (���ץ����B ��B) ...)
;;
;;     ���̤ʻ��꤬���פʾ��ˤϡ����ץ����Ͼ�ά�Ǥ��ޤ���
;;
;;         (sdic-compat �ե�����̾)


;;; Options:

;; sdic-compat.el ���Ф��ƻ���Ǥ��륪�ץ����ϼ����̤�Ǥ���
;;
;; coding-system
;;     ����δ��������ɤ���ꤷ�ޤ�����ά�������ϡ�
;;     sdic-default-coding-system ���ͤ�Ȥ��ޤ���
;;
;; title
;;     ����Υ����ȥ����ꤷ�ޤ�����ά�������ϡ�����ե������ 
;;     basename �򥿥��ȥ�Ȥ��ޤ���
;;
;; look
;;     �������׸���/�������׸����λ������Ѥ��볰�����ޥ�ɤ�̾�������
;;     ���ޤ�����ά�������� sdic-compat-look-command ���ͤ�Ȥ��ޤ���
;;
;; look-case-option
;;     look ���ץ����ˤ�äƻ��ꤵ�줿�������ޥ�ɤ��Ф��ơ�����ʸ��
;;     /��ʸ������̤��ʤ��Ǹ�������褦�˻ؼ����뤿��Υ��ޥ�ɥ饤��
;;     ��������ꤷ�ޤ�����ά�������� sdic-compat-look-case-option ��
;;     �ͤ�Ȥ��ޤ���
;;
;; grep
;;     �������׸���/��ʸ�����λ������Ѥ��볰�����ޥ�ɤ�̾������ꤷ��
;;     ������ά�������� sdic-compat-grep-command ���ͤ�Ȥ��ޤ���
;;
;; grep-case-option
;;     grep ���ץ����ˤ�äƻ��ꤵ�줿�������ޥ�ɤ��Ф��ơ�����ʸ��
;;     /��ʸ������̤��ʤ��Ǹ�������褦�˻ؼ����뤿��Υ��ޥ�ɥ饤��
;;     ��������ꤷ�ޤ�����ά�������� sdic-compat-grep-case-option ��
;;     �ͤ�Ȥ��ޤ���
;;
;; egrep
;;     ����ɽ�������λ������Ѥ��볰�����ޥ�ɤ�̾������ꤷ�ޤ�����ά
;;     �������� sdic-compat-egrep-command ���ͤ�Ȥ��ޤ���
;;
;; egrep-case-option
;;     egrep ���ץ����ˤ�äƻ��ꤵ�줿�������ޥ�ɤ��Ф��ơ�����ʸ
;;     ��/��ʸ������̤��ʤ��Ǹ�������褦�˻ؼ����뤿��Υ��ޥ�ɥ饤
;;     ���������ꤷ�ޤ�����ά�������� 
;;     sdic-compat-egrep-case-option ���ͤ�Ȥ��ޤ���


;;; Note:

;; sdic-compat-look-command / sdic-compat-grep-command /
;; sdic-compat-egrep-command ���ͤϼ�ưŪ�����ꤵ��ޤ����㤨�С�
;; sdic-compat-grep-command �ξ�硢fgrep / fgrep.exe / grep /
;; grep.exe ��4��Υ��ޥ�ɤ򸡺����ơ����Ĥ��ä����ޥ�ɤ�Ȥ��ޤ���
;;
;; sdic-compat.el �� sdic-gene.el ��Ʊ����ǽ���󶡤��Ƥ���饤�֥���
;; ����sdic-compat.el �ϳ������ޥ�ɤ�ƤӽФ��Ƥ���Τ��Ф��ơ�
;; sdic-gene.el �� Emacs �ε�ǽ�Τߤ����Ѥ��Ƥ��ޤ����������������Х�
;; �ե����ɤ߹���Ǥ��鸡����Ԥʤ��Τǡ����̤Υ��꤬ɬ�פˤʤ�ޤ���
;;
;; Default ������Ǥϡ�ɬ�פʳ������ޥ�ɤ����Ĥ��ä����� 
;; sdic-compat.el �򡢸��Ĥ���ʤ��ä����ˤ� sdic-gene.el ��Ȥ��褦
;; �ˤʤäƤ��ޤ���


;;; �饤�֥���������
(require 'sdic)
(require 'sdicf)
(provide 'sdic-compat)
(put 'sdic-compat 'version "2.0")
(put 'sdic-compat 'init-dictionary 'sdic-compat-init-dictionary)
(put 'sdic-compat 'open-dictionary 'sdic-compat-open-dictionary)
(put 'sdic-compat 'close-dictionary 'sdic-compat-close-dictionary)
(put 'sdic-compat 'search-entry 'sdic-compat-search-entry)
(put 'sdic-compat 'get-content 'sdic-compat-get-content)


;;;----------------------------------------------------------------------
;;;		���/�ѿ������
;;;----------------------------------------------------------------------

(defvar sdic-compat-look-command (sdicf-find-program "look" "look.exe")
  "*Executable file name of look")

(defvar sdic-compat-look-case-option "-f" "*Command line option for look to ignore case")

(defvar sdic-compat-grep-command (sdicf-find-program "fgrep" "fgrep.exe" "grep" "grep.exe")
  "*Executable file name of grep")

(defvar sdic-compat-grep-case-option "-i" "*Command line option for grep to ignore case")

(defvar sdic-compat-egrep-command (sdicf-find-program "egrep" "egrep.exe" "grep" "grep.exe")
  "*Executable file name of egrep")

(defvar sdic-compat-egrep-case-option "-i" "*Command line option for egrep to ignore case")

(defconst sdic-compat-search-buffer-name " *sdic-compat*")



;;;----------------------------------------------------------------------
;;;		����
;;;----------------------------------------------------------------------

(defun sdic-compat-available-p () "\
Function to check availability of library.
�饤�֥������Ѳ�ǽ���򸡺�����ؿ�"
  (and (file-executable-p sdic-compat-look-command)
       (file-executable-p sdic-compat-grep-command)))


(defun sdic-compat-init-dictionary (file-name &rest option-list)
  "Function to initialize dictionary"
  (let ((dic (sdic-make-dictionary-symbol)))
    (if (file-readable-p (setq file-name (expand-file-name file-name)))
	(progn
	  (mapcar '(lambda (c) (put dic (car c) (nth 1 c))) option-list)
	  (put dic 'file-name file-name)
	  (put dic 'identifier (concat "sdic-compat+" file-name))
	  (or (get dic 'title)
	      (put dic 'title (file-name-nondirectory file-name)))
	  (or (get dic 'look)
	      (put dic 'look sdic-compat-look-command))
	  (or (get dic 'look-case-option)
	      (put dic 'look-case-option sdic-compat-look-case-option))
	  (or (get dic 'grep)
	      (put dic 'grep sdic-compat-grep-command))
	  (or (get dic 'grep-case-option)
	      (put dic 'grep-case-option sdic-compat-grep-case-option))
	  (or (get dic 'egrep)
	      (put dic 'egrep sdic-compat-egrep-command))
	  (or (get dic 'egrep-case-option)
	      (put dic 'egrep-case-option sdic-compat-egrep-case-option))
	  (or (get dic 'coding-system)
	      (put dic 'coding-system sdic-default-coding-system))
	  (and (stringp (get dic 'look))
	       (stringp (get dic 'grep))
	       dic))
      (error "Can't read dictionary: %s" (prin1-to-string file-name)))))


(defun sdic-compat-open-dictionary (dic)
  "Function to open dictionary"
  (and (or (sdicf-buffer-live-p (get dic 'sdic-compat-search-buffer))
	   (put dic 'sdic-compat-search-buffer (generate-new-buffer sdic-compat-search-buffer-name)))
       dic))


(defun sdic-compat-close-dictionary (dic)
  "Function to close dictionary"
  (kill-buffer (get dic 'sdic-compat-search-buffer))
  (put dic 'sdic-compat-search-buffer nil))


(defun sdic-compat-search-entry (dic string &optional search-type) "\
Function to search word with look or grep, and write results to current buffer.
search-type ���ͤˤ�äƼ��Τ褦��ư����ѹ����롣
    nil    : �������׸���
    t      : �������׸���
    lambda : �������׸���
    0      : ��ʸ����
    regexp : ����ɽ������
������̤Ȥ��Ƹ��Ĥ��ä����Ф���򥭡��Ȥ����������ʸ����Ƭ�� point ���ͤȤ���
Ϣ��������֤���"
  (save-excursion
    (set-buffer (get dic 'sdic-compat-search-buffer))
    (save-restriction
      (if (get dic 'sdic-compat-erase-buffer)
	  (delete-region (point-min) (point-max))
	(goto-char (point-max))
	(narrow-to-region (point-max) (point-max)))
      (put dic 'sdic-compat-erase-buffer nil)
      (cond
       ;; �������׸����ξ�� -> look ��ȤäƸ���
       ((eq search-type nil)
	(if (string-match "\\Ca" string)
	    (sdicf-call-process (get dic 'look) (get dic 'coding-system) nil t nil
				string (get dic 'file-name))
	  (sdicf-call-process (get dic 'look) (get dic 'coding-system) nil t nil
			      (get dic 'look-case-option) string (get dic 'file-name))))
       ;; �������׸����ξ�� -> grep ��ȤäƸ���
       ((eq search-type t)
	(if (string-match "\\Ca" string)
	    (sdicf-call-process (get dic 'grep) (get dic 'coding-system) nil t nil
				(concat string "\t") (get dic 'file-name))
	  (sdicf-call-process (get dic 'grep) (get dic 'coding-system) nil t nil
			      (get dic 'grep-case-option)
			      (concat string "\t") (get dic 'file-name))))
       ;; �������׸����ξ�� -> look ��ȤäƸ��� / ;ʬ�ʥǡ�����õ�
       ((eq search-type 'lambda)
	(if (string-match "\\Ca" string)
	    (sdicf-call-process (get dic 'look) (get dic 'coding-system) nil t nil
				string (get dic 'file-name))
	  (sdicf-call-process (get dic 'look) (get dic 'coding-system) nil t nil
			      (get dic 'look-case-option)
			      string (get dic 'file-name)))
	(goto-char (point-min))
	(while (if (looking-at (format "%s\t" (regexp-quote string)))
		   (= 0 (forward-line 1))
		 (delete-region (point) (point-max)))))
       ;; ��ʸ�����ξ�� -> grep ��ȤäƸ���
       ((eq search-type 0)
	(if (string-match "\\Ca" string)
	    (sdicf-call-process (get dic 'grep) (get dic 'coding-system) nil t nil
				string (get dic 'file-name))
	  (sdicf-call-process (get dic 'grep) (get dic 'coding-system) nil t nil
			      (get dic 'grep-case-option)
			      string (get dic 'file-name))))
       ;; ����ɽ�������ξ�� -> egrep ��ȤäƸ���
       ((eq search-type 'regexp)
	(or (stringp (get dic 'egrep))
	    (error "%s" "Command to search regular expression pattern is not specified"))
	(if (string-match "\\Ca" string)
	    (sdicf-call-process (get dic 'egrep) (get dic 'coding-system) nil t nil
				string (get dic 'file-name))
	  (sdicf-call-process (get dic 'egrep) (get dic 'coding-system) nil t nil
			      (get dic 'egrep-case-option)
			      string (get dic 'file-name))))
       ;; ����ʳ��θ�����������ꤵ�줿���
       (t (error "Not supported search type is specified. \(%s\)"
		 (prin1-to-string search-type))))
      ;; �Ƹ�����̤� ID ����Ϳ����
      (goto-char (point-min))
      (let (ret)
	(while (if (looking-at "\\([^\t]+\\)\t")
		   (progn
		     (setq ret (cons (cons (sdic-match-string 1) (match-end 0)) ret))
		     (= 0 (forward-line 1)))))
	(nreverse ret)))))


(defun sdic-compat-get-content (dic point)
  (save-excursion
    (set-buffer (get dic 'sdic-compat-search-buffer))
    (put dic 'sdic-compat-erase-buffer t)
    (if (<= point (point-max))
	(buffer-substring (goto-char point) (progn (end-of-line) (point)))
      (error "Can't find content. (ID=%d)" point))))
