;; sdic-gene.el ---- -*- Emacs-Lisp -*- Library to search COMPAT format dictionary.
;; $Id: sdic-gene.el,v 2.6 2002/07/02 11:17:46 tsuchiya Exp $

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

;; This file is a part of sdic. Please see sdic.el for more detail.

;; COMPAT �����μ�������ץ�������餺�˸�������饤�֥��Ǥ���
;; COMPAT �����ξܺ٤ˤĤ��Ƥ� sdic.texi �򻲾Ȥ��Ʋ�������


;;; Install:

;; (1) �����Ŭ�ڤʷ������Ѵ����ơ�Ŭ���ʾ��( ��: /usr/dict/ )����¸
;;     ���Ʋ������������Ѵ��ѥ�����ץȤȤ��ưʲ��� Perl ������ץȤ�
;;     ���ѤǤ��ޤ���
;;
;;         gene.perl    - GENE95 ����
;;         jgene.perl   - GENE95 ���񤫤��±Ѽ������������
;;         eijirou.perl - �Ѽ�Ϻ
;;
;; (2) �Ȥ���褦�ˤ���������������� sdic-eiwa-dictionary-list �ޤ�
;;     �� sdic-waei-dictionary-list ���ɲä��Ʋ�������
;;
;;         (setq sdic-eiwa-dictionary-list
;;               (cons '(sdic-gene "/usr/dict/gene.dic") sdic-eiwa-dictionary-list))
;;
;;     �����������ϼ��Τ褦�ʹ����ˤʤäƤ��ޤ���
;;
;;         (sdic-gene �ե�����̾ (���ץ����A ��A) (���ץ����B ��B) ...)
;;
;;     ���̤ʻ��꤬���פʾ��ˤϡ����ץ����Ͼ�ά�Ǥ��ޤ���
;;
;;         (sdic-gene �ե�����̾)


;;; Options:

;; sdic-gene.el ���Ф��ƻ���Ǥ��륪�ץ����ϼ����̤�Ǥ���
;;
;; coding-system
;;     ����δ��������ɤ���ꤷ�ޤ�����ά�������ϡ�
;;     sdic-default-coding-system ���ͤ�Ȥ��ޤ���
;;
;; title
;;     ����Υ����ȥ����ꤷ�ޤ�����ά�������ϡ�����ե������ 
;;     basename �򥿥��ȥ�Ȥ��ޤ���
;;
;; extract
;;     ���̼����Ÿ�����뤿��γ������ޥ�ɤ���ꤷ�ޤ�����ά�������
;;     �ϡ����񤬰��̤���Ƥ��ʤ��ȸ��ʤ��ޤ���
;;
;; extract-option
;;     extract ���ץ����ˤ�äƻ��ꤵ�줿�������ޥ�ɤ��Ф��ơ�����
;;     ��Ÿ������ɸ����Ϥ˽��Ϥ����뤿��Υ��ޥ�ɥ饤���������ꤷ
;;     �ޤ�����ά�������� sdic-gene-extract-option ���ͤ�Ȥ��ޤ���


;;; Note:

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
(provide 'sdic-gene)
(put 'sdic-gene 'version "2.0")
(put 'sdic-gene 'init-dictionary 'sdic-gene-init-dictionary)
(put 'sdic-gene 'open-dictionary 'sdic-gene-open-dictionary)
(put 'sdic-gene 'close-dictionary 'sdic-gene-close-dictionary)
(put 'sdic-gene 'search-entry 'sdic-gene-search-entry)
(put 'sdic-gene 'get-content 'sdic-gene-get-content)


;;;----------------------------------------------------------------------
;;;		���/�ѿ������
;;;----------------------------------------------------------------------

(defvar sdic-gene-extract-option "-dc" "\
*Option for archiver.
���̼����Ÿ�����뤿��˻Ȥ����ץ����")

(defconst sdic-gene-search-buffer-name " *sdic-gene*")



;;;----------------------------------------------------------------------
;;;		����
;;;----------------------------------------------------------------------

(defun sdic-gene-init-dictionary (file-name &rest option-list)
  "Function to initialize dictionary"
  (let ((dic (sdic-make-dictionary-symbol)))
    (if (file-readable-p (setq file-name (expand-file-name file-name)))
	(progn
	  (mapcar '(lambda (c) (put dic (car c) (nth 1 c))) option-list)
	  (put dic 'file-name file-name)
	  (put dic 'identifier (concat "sdic-gene+" file-name))
	  (or (get dic 'title)
	      (put dic 'title (file-name-nondirectory file-name)))
	  (if (get dic 'extract)
	      (or (get dic 'extract-option)
		  (put dic 'extract-option sdic-gene-extract-option)))
	  (or (get dic 'coding-system)
	      (put dic 'coding-system sdic-default-coding-system))
	  dic)
      (error "Can't read dictionary: %s" (prin1-to-string file-name)))))


(defun sdic-gene-open-dictionary (dic)
  "Function to open dictionary"
  (if (or (sdicf-buffer-live-p (get dic 'sdic-gene-search-buffer))
	  (save-excursion
	    (set-buffer (put dic 'sdic-gene-search-buffer (generate-new-buffer sdic-gene-search-buffer-name)))
	    (buffer-disable-undo)
	    (insert "\n")
	    (prog1 (if (get dic 'extract)
		       (= 0 (sdicf-call-process (get dic 'extract) (get dic 'coding-system) nil t nil
						(get dic 'extract-option)
						(get dic 'file-name)))
		     (condition-case err
			 (sdicf-insert-file-contents (get dic 'file-name) (get dic 'coding-system))
		       (error nil)))
	      (setq buffer-read-only t)
	      (set-buffer-modified-p nil))))
      dic))


(defun sdic-gene-close-dictionary (dic)
  "Function to close dictionary"
  (kill-buffer (get dic 'sdic-gene-search-buffer))
  (put dic 'sdic-gene-search-buffer nil))


(defsubst sdic-gene-search-internal (string)
  "�̾�θ�����Ԥ������ؿ�"
  (let (ret (case-fold-search t))
    (while (search-forward string nil t)
      (save-excursion
	(setq ret (cons (cons (buffer-substring (progn (beginning-of-line) (point))
						(progn (skip-chars-forward "^\t") (point)))
			      (1+ (point)))
			ret))))
    (nreverse ret)))


(defsubst sdic-gene-re-search-internal (string)
  "����ɽ��������Ԥ������ؿ�"
  (let (ret (case-fold-search t))
    (while (re-search-forward string nil t)
      (save-excursion
	(setq ret (cons (cons (buffer-substring (progn (beginning-of-line) (point))
						(progn (skip-chars-forward "^\t") (point)))
			      (1+ (point)))
			ret))))
    (nreverse ret)))


(defun sdic-gene-search-entry (dic string &optional search-type) "\
Function to search word with look or grep, and write results to current buffer.
search-type ���ͤˤ�äƼ��Τ褦��ư����ѹ����롣
    nil    : �������׸���
    t      : �������׸���
    lambda : �������׸���
    0      : ��ʸ����
    regexp : ����ɽ������
������̤Ȥ��Ƹ��Ĥ��ä����Ф���򥭡��Ȥ����������ʸ����Ƭ�� point ���ͤȤ���
Ϣ��������֤���
"
  (save-excursion
    (set-buffer (get dic 'sdic-gene-search-buffer))
    (goto-char (point-min))
    (cond
     ;; �������׸���
     ((eq search-type nil)
      (sdic-gene-search-internal (concat "\n" string)))
     ;; �������׸���
     ((eq search-type t)
      (sdic-gene-search-internal (concat string "\t")))
     ;; �������׸���
     ((eq search-type 'lambda)
      (sdic-gene-search-internal (concat "\n" string "\t")))
     ;; ��ʸ����
     ((eq search-type 0)
      (sdic-gene-search-internal string))
     ;; ����ɽ������
     ((eq search-type 'regexp)
      (sdic-gene-re-search-internal string))
     ;; ����ʳ��θ�����������ꤵ�줿���
     (t (error "Not supported search type is specified. \(%s\)"
	       (prin1-to-string search-type))))))


(defun sdic-gene-get-content (dic point)
  (save-excursion
    (set-buffer (get dic 'sdic-gene-search-buffer))
    (if (<= point (point-max))
	(buffer-substring (goto-char point) (progn (end-of-line) (point)))
      (error "Can't find content. (ID=%d)" point))))
