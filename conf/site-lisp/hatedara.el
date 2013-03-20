;;; hatedara.el --- Emacs front end of Hatena diary writer

;; Copyright (C) 2005, 2007, 2008  emacsjjj

;; Author: <emacsjjj@ybb.ne.jp>
;; Keywords: blog
;; $Revision: 1.19 $ $Date: 2008-01-26 00:39:04 $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; hatedara.el �ϡ���뤵���ΤϤƤʥ������꡼����������ץ�
;; �ϤƤʥ������꡼�饤����(ά��:�Ϥƥ���) �� Emacs frontend �Ǥ���
;;   �Ϥƥ���Υڡ��� - http://www.hyuki.com/techinfo/hatena_diary_writer.html
;;
;; ���åȥ��å�
;;
;;   0. �Ϥƥ����Ȥ�����֤ˤ��Ƥ���
;;   1. hatedara.el �� load-path ���̤ä��Ȥ�����֤�
;;   2. .emacs �˰ʲ��������ä���
;;
;; (autoload 'hatedara "hatedara" nil t)
;; (setq hatedara-directory "~/diary")         ; ��������¸����ǥ��쥯�ȥ�
;; (setq hatedara-script-file "~/bin/hw.pl")   ; �Ϥƥ��饹����ץȤλ���
;;
;; �Ȥ���
;;
;;   1. M-x hatedara �Ǻ��������դ������ե�����򳫤��ޤ���
;;      C-u -2 M-x hatedara ��2�����������ե�����ˤʤ�ޤ���
;;   2. `C-cC-cC-t' (`hatedara-insert-tag') �ǰ��ѡ�pre �����������Ǥ�
;;      �ޤ���
;;   3. `C-cC-cC-b' (`hatedara-find-previous') ��ľ���Ρ�
;;      `C-cC-cC-f' (`hatedara-find-following') ��ľ��������������ޤ���
;;   4. `C-cC-cC-c' (`hatedara-submit') ���������������ޤ���
;;      `C-uC-cC-cC-c' �ǡ֤���äȤ��������פˤʤ�ޤ���
;;
;; ����
;;
;;   �ѹ��������ʤ�Τ�
;;     FIXME: ���꥿���ߥ󥰤ˤĤ��Ƴ�ǧ
;;     `hatedara-parent-mode' : hatedara-mode �θ��Ȥʤ�⡼��
;;     `hatedara-submit-key' : �����γ�����ƥ���
;;     `hatedara-trivial' : `C-cC-cC-c' ��֤���äȤ��������פˤ��뤫�ɤ���
;;     `hatedara-use-cookie' : ���å�����Ȥ����ɤ���
;;     `hatedara-submit-async' : ��Ʊ�����������뤫�ɤ���
;;   ������Ǥ��礦��������¾�ˤĤ��Ƥ�
;;     M-x customize-group RET hatedara
;;   ���ƤߤƲ�������
;;
;; �ռ�
;;
;;   hikigaeru ����� hatena-mode �򻲹ͤˤ����Ƥ��äƤޤ������ա�
;;     hatena-mode �Υڡ��� - http://d.hatena.ne.jp/hikigaeru/20040617

;;; Code:

;;; User variables:

(defgroup hatedara nil
  "Emacs front end of Hatena diary write"
  :group 'hypermedia)

(defgroup hatedara-script nil
  "�Ϥƥ���˴ؤ�������"
  :group 'hatedara)

(defgroup hatedara-face nil
  "hatedara-mode �� face"
  :group 'hatedara)

;; hatedara-mode �˴ؤ�������
(defcustom hatedara-directory (expand-file-name "~/.hatena")
  "*��������¸����ǥ��쥯�ȥꡥ"
  :type 'directory
  :group 'hatedara)

(defcustom hatedara-user-id nil
  "*�ϤƤʤΥ桼��̾��"
  :type '(choice (const nil) (string :tag "user-id"))
  :group 'hatedara)

(defcustom hatedara-passwd nil
  "*�ϤƤʤΥѥ���ɡ�"
  :type '(choice (const nil) (string :tag "password"))
  :group 'hatedara)

(defcustom hatedara-script-file (expand-file-name "hw.pl" hatedara-directory)
  "*�Ϥƥ��饹����ץȤλ��ꡥ"
  :type 'file
  :group 'hatedara)

(defcustom hatedara-parent-mode 'html-mode
  "*`hatedara-mode' �οƤȤʤ�⡼�ɡ�"
  :type 'function
  :group 'hatedara)

(defcustom hatedara-submit-key "\C-c\C-c\C-c"
  "*�������������륭����"
  :type 'string
  :group 'hatedara)

(defcustom hatedara-insert-tag-key "\C-c\C-c\C-t"
  "*���ѡ�pre �������������륭����"
  :type 'string
  :group 'hatedara)

(defcustom hatedara-find-previous-key "\C-c\C-c\C-b"
  "*ľ���������򳫤�������"
  :type 'string
  :group 'hatedara)

(defcustom hatedara-find-following-key "\C-c\C-c\C-f"
  "*ľ��������򳫤�������"
  :type 'string
  :group 'hatedara)

(defcustom hatedara-submit-async nil
  "*Non-nil �ʤ����������Ʊ���Ǽ¹Ԥ��롥
��Ʊ���ξ��� `hatedara-auto-reload' �λ����̵�� (���ɤ߹��ߤ��ʤ�)��"
  :type 'boolean
  :group 'hatedara)

(defcustom hatedara-auto-reload nil
  "*Non-nil �ʤ��������������˺��ɤ߹��ߤ��롥
`hatedara-submit-async' �� non-nil �ʤ�Ф��λ����̵�� (���ɤ߹��ߤ��ʤ�)��"
  :type 'boolean
  :group 'hatedara)

(defcustom hatedara-time-offset 6
  "*���������դ��ѹ�������֡�"
  :type '(choice (const nil) (integer :tag "hour"))
  :group 'hatedara)

;; �Ϥƥ���˴ؤ�������
(defcustom hatedara-trivial nil
  "*Non-nil �ʤ�С֤���äȤ��������ץ⡼�ɤ��������롥"
  :type 'boolean
  :group 'hatedara-script)

(defcustom hatedara-debug nil
  "*Non-nil �ʤ�ФϤƥ���ΥǥХå�ɽ����ͭ���ˤ��롥"
  :type 'boolean
  :group 'hatedara-script)

(defcustom hatedara-user-agent nil
  "*Non-nil �ʤ�����ꤵ��Ƥ���ʸ�����桼������������ȤȤ����������롥"
  :type '(choice (const nil) (string :tag "user-agent"))
  :group 'hatedara-script)

(defcustom hatedara-timeout nil
  "*Non-nil �ʤ�����ꤵ��Ƥ�����ͤ򥿥��ॢ���Ȼ��֤˻��ꤹ�롥"
  :type '(choice (const nil) (integer :tag "sec"))
  :group 'hatedara-script)

(defcustom hatedara-use-cookie nil
  "*Non-nil �ʤ�Х��å��������Ѥ��롥"
  :type 'boolean
  :group 'hatedara-script)

(defcustom hatedara-submit-editing-file-only nil
  "*Non-nil �ʤ���Խ���������Τߤ��������롥"
  :type 'boolean
  :group 'hatedara-script)

(defcustom hatedara-disable-replace-timestamp nil
  "*Non-nil �ʤ�и��Ф������ॹ����פ��ִ���ػߤ��롥"
  :type 'boolean
  :group 'hatedara-script)

(defcustom hatedara-config-file nil
  "*Non-nil �ʤ�����ꤵ��Ƥ���ե������Ϥƥ��������ե�����˻��ꤹ�롥"
  :type '(choice (const nil) (file :tag "config file name"))
  :group 'hatedara-script)

;;; Faces:

(defface hatedara-title-face
  '((((class color) (background light)) (:foreground "firebrick" :bold t))
    (((class color) (background dark)) (:foreground "moccasin" :bold t))
    (t (:bold t)))
  "�����ȥ�� face"
  :group 'hatedara-face)
(defvar hatedara-title-face 'hatedara-title-face)

(defface hatedara-caption-face
  '((((class color) (background light)) (:foreground "purple" :bold t))
    (((class color) (background dark)) (:foreground "yellow" :bold t))
    (t (:bold t)))
  "���Ф��� face"
  :group 'hatedara-face)
(defvar hatedara-caption-face 'hatedara-caption-face)

(defface hatedara-markup-face
  '((((class color) (background light))
     (:foreground "DarkGreen" :background "light yellow" :bold t))
    (((class color) (background dark))
     (:foreground "turquoise" :background "dark slate grey" :bold t))
    (t (:bold t)))
  "�ϤƤʤΥޡ������åפ� face"
  :group 'hatedara-face)
(defvar hatedara-markup-face 'hatedara-markup-face)

(defface hatedara-list-face
  '((((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark)) (:foreground "wheat"))
    (t (:bold t)))
  "�ꥹ�Ȥ� face"
  :group 'hatedara-face)
(defvar hatedara-list-face 'hatedara-list-face)

(defface hatedara-link-face
  '((((class color) (background light)) (:foreground "OrangeRed" :bold t))
    (((class color) (background dark)) (:foreground "magenta" :bold t))
    (t (:bold t)))
  "��󥯤� face"
  :group 'hatedara-face)
(defvar hatedara-link-face 'hatedara-link-face)

(defface hatedara-link-title-face
  '((((class color) (background light))
     (:foreground "OrangeRed" :background "light yellow" :bold t))
    (((class color) (background dark))
     (:foreground "magenta" :background "dark slate grey" :bold t))
    (t (:bold t)))
  "��󥯤� face"
  :group 'hatedara-face)
(defvar hatedara-link-title-face 'hatedara-link-title-face)

(defface hatedara-disable-link-face
  '((((class color) (background light)) (:foreground "DarkSlateGray" :bold t))
    (((class color) (background dark)) (:foreground "LightGray" :bold t))
    (t (:bold t)))
  "��ߥ�󥯤� face"
  :group 'hatedara-face)
(defvar hatedara-disable-link-face 'hatedara-disable-link-face)

;;; Internal variables:

(defvar hatedara-interpreter "perl")
(defvar hatedara-log-buffer " *hatedara log*")

(defconst hatedara-login-err-msg
  "^ERROR: Login: Unexpected response: 200 OK$")

(defvar hatedara-file-name-regexp
  "[0-9]\\{4\\}-[01][0-9]-[0-3][0-9]\\.txt\\'")

(defvar hatedara-comment-start-skip "\\(?:><!\\)?--[ \t]*")
(defvar hatedara-comment-end-skip "[ \t]*--\\([ \t\n]*><\\)?")
(defvar hatedara-comment-start "><!--")
(defvar hatedara-comment-end "--><")

(defvar hatedara-script-option-list
  '((hatedara-user-id "-u" hatedara-user-id)
    ;;(hatedara-passwd "-p" hatedara-passwd)
    (hatedara-trivial "-t")
    (hatedara-debug "-d")
    (hatedara-use-cookie "-c")
    (hatedara-disable-replace-timestamp "-M")
    (hatedara-user-agent "-a" hatedara-user-agent)
    (hatedara-timeout "-T" hatedara-timeout)
    (hatedara-submit-editing-file-only "-f" buffer-file-name)
    (hatedara-config-file "-n" hatedara-config-file)))

(defvar hatedara-markup-tag
  '(">>" "<<" ">|" "|<" ">||" "||<" "====" "====="))

(defvar hatedara-link-keyword
  '("http" "https" "ftp" "mailto" "isbn" "asin" "search" "google" "amazon"
    "rakuten" "id" "a" "b" "d" "f" "g" "i" "r" "idea" "map" "graph"
    "keyword" "jan" "ean" "question" "ISBN"))

(defvar hatedara-square-bracket-link-keyword
  (append hatedara-link-keyword '("tex" "uke")))

(defvar hatedara-syntax-filetype
  '("?" "aa" "mml"
    "a2ps" "a65" "aap" "abap" "abaqus" "abc" "abel" "acedb" "ada" "aflex"
    "ahdl" "alsaconf" "amiga" "aml" "ampl" "ant" "antlr" "apache" "apachestyle"
    "arch" "art" "asm" "asm68k" "asmh8300" "asn" "aspperl" "aspvbs" "asterisk"
    "asteriskvm" "atlas" "automake" "ave" "awk" "ayacc" "b" "baan" "basic" "bc"
    "bdf" "bib" "bindzone" "blank" "bst" "btm" "c" "calendar" "catalog" "cdl"
    "cf" "cfg" "ch" "change" "changelog" "chaskell" "cheetah" "chill"
    "chordpro" "cl" "clean" "clipper" "cmake" "cobol" "colortest" "conf"
    "config" "context" "cpp" "crm" "crontab" "cs" "csc" "csh" "csp" "css"
    "cterm" "ctrlh" "cupl" "cuplsim" "cvs" "cvsrc" "cweb" "cynlib" "cynpp" "d"
    "dcd" "dcl" "debchangelog" "debcontrol" "debsources" "def" "desc" "desktop"
    "dictconf" "dictdconf" "diff" "dircolors" "diva" "django" "dns" "docbk"
    "docbksgml" "docbkxml" "dosbatch" "dosini" "dot" "doxygen" "dracula" "dsl"
    "dtd" "dtml" "dylan" "dylanintr" "dylanlid" "ecd" "edif" "eiffel" "elf"
    "elinks" "elmfilt" "erlang" "eruby" "esmtprc" "esqlc" "esterel" "eterm"
    "eviews" "exim" "expect" "exports" "fasm" "fdcc" "fetchmail" "fgl"
    "flexwiki" "focexec" "form" "forth" "fortran" "foxpro" "fstab" "fvwm"
    "fvwm2m4" "gdb" "gdmo" "gedcom" "gkrellmrc" "gnuplot" "gp" "gpg" "grads"
    "gretl" "groff" "groovy" "group" "grub" "gsp" "gtkrc" "haskell" "hb" "help"
    "hercules" "hex" "hitest" "hog" "html" "htmlcheetah" "htmldjango" "htmlm4"
    "htmlos" "ia64" "icemenu" "icon" "idl" "idlang" "indent" "inform" "initex"
    "inittab" "ipfilter" "ishd" "iss" "ist" "jal" "jam" "jargon" "java"
    "javacc" "javascript" "jess" "jgraph" "jproperties" "jsp" "kconfig" "kix"
    "kscript" "kwt" "lace" "latte" "ld" "ldif" "lex" "lftp" "lhaskell" "libao"
    "lifelines" "lilo" "limits" "lisp" "lite" "loginaccess" "logindefs"
    "logtalk" "lotos" "lout" "lpc" "lprolog" "lscript" "lss" "lua" "lynx" "m4"
    "mail" "mailaliases" "mailcap" "make" "man" "manconf" "manual" "maple"
    "masm" "mason" "master" "matlab" "maxima" "mel" "mf" "mgl" "mgp" "mib"
    "mma" "mmix" "modconf" "model" "modsim3" "modula2" "modula3" "monk" "moo"
    "mp" "mplayerconf" "mrxvtrc" "msidl" "msql" "mupad" "mush" "muttrc" "mysql"
    "named" "nanorc" "nasm" "nastran" "natural" "ncf" "netrc" "netrw"
    "nosyntax" "nqc" "nroff" "nsis" "objc" "objcpp" "ocaml" "occam" "omnimark"
    "openroad" "opl" "ora" "pamconf" "papp" "pascal" "passwd" "pcap" "pccts"
    "perl" "pf" "pfmain" "php" "phtml" "pic" "pike" "pilrc" "pine" "pinfo"
    "plaintex" "plm" "plp" "plsql" "po" "pod" "postscr" "pov" "povini" "ppd"
    "ppwiz" "prescribe" "procmail" "progress" "prolog" "protocols" "psf"
    "ptcap" "purifylog" "pyrex" "python" "qf" "quake" "r" "racc" "radiance"
    "ratpoison" "rc" "rcs" "rcslog" "readline" "rebol" "registry" "remind"
    "resolv" "rexx" "rhelp" "rib" "rnc" "rnoweb" "robots" "rpcgen" "rpl" "rst"
    "rtf" "ruby" "samba" "sas" "sather" "scheme" "scilab" "screen" "sdl" "sed"
    "sendpr" "sensors" "services" "setserial" "sgml" "sgmldecl" "sgmllnx" "sh"
    "sicad" "sieve" "simula" "sinda" "sindacmp" "sindaout" "sisu" "skill" "sl"
    "slang" "slice" "slpconf" "slpreg" "slpspi" "slrnrc" "slrnsc" "sm" "smarty"
    "smcl" "smil" "smith" "sml" "snnsnet" "snnspat" "snnsres" "snobol4" "spec"
    "specman" "spice" "splint" "spup" "spyce" "sql" "sqlanywhere" "sqlforms"
    "sqlinformix" "sqlj" "sqloracle" "sqr" "squid" "sshconfig" "sshdconfig"
    "st" "stata" "stp" "strace" "sudoers" "svn" "syncolor" "synload" "syntax"
    "sysctl" "tads" "tags" "tak" "takcmp" "takout" "tar" "tasm" "tcl" "tcsh"
    "terminfo" "tex" "texinfo" "texmf" "tf" "tidy" "tilde" "tli" "tpp" "trasys"
    "trustees" "tsalt" "tsscl" "tssgm" "tssop" "uc" "udevconf" "udevperm"
    "udevrules" "uil" "updatedb" "valgrind" "vb" "vera" "verilog" "verilogams"
    "vgrindefs" "vhdl" "vim" "viminfo" "virata" "vmasm" "vrml" "vsejcl" "wdiff"
    "web" "webmacro" "wget" "whitespace" "winbatch" "wml" "wsh" "wsml" "wvdial"
    "xdefaults" "xf86conf" "xhtml" "xinetd" "xkb" "xmath" "xml" "xmodmap" "xpm"
    "xpm2" "xquery" "xs" "xsd" "xslt" "xxd" "yacc" "yaml" "z8a" "zsh"))

(defvar hatedara-font-lock-keywords
  `(;; ���Ф�
    ("^\\(\\*[^\n ]*\\)\\(.*\\)"
     (1 hatedara-markup-face t)
     (2 hatedara-caption-face t))
    ;; �ꥹ��
    ("^\\([-+]+\\)\\(.*\\)"
     (1 hatedara-markup-face t)
     (2 hatedara-list-face t))
    ;; ����ꥹ��
    ("^\\(:\\(.+?\\):\\)\\(.*\\)"
     (1 hatedara-markup-face t)
     (2 hatedara-list-face t)
     (3 font-lock-string-face t))
    ;; ɽ�Ȥ�
    ("^|.+|$"
     (0 hatedara-list-face t)
     ("|" (beginning-of-line) nil (0 hatedara-markup-face t))
     ("|\\(?:\\s-+\\)?\\(\\*[^|]+\\)" (beginning-of-line) nil
      (1 font-lock-keyword-face t)))
    ;; ���ѡ�pre��³�����ɤ�
    (,(concat "^\\(?:"
	      (concat (regexp-opt (nconc (mapcar (lambda (x)
						   (concat ">|" x "|"))
						 hatedara-syntax-filetype)
					 hatedara-markup-tag) t)
		      "\\|>s?https?://[^ \n]+>")
	      "\\)$")
     (0 hatedara-markup-face t))
    ;; ����
    ("\\(?:^\\|[^()]\\)\\((\\{2\\}\\)\\([^(].*?[^)]\\)\\()\\{2\\}\\)\\(?:$\\|[^()]\\)"
     (1 hatedara-markup-face t)
     (2 font-lock-comment-face t)
     (3 hatedara-markup-face t))
    ;; ���
    ;; FIXME: URI regexp see@ http://www.ietf.org/rfc/rfc3986.txt
    (,(concat "\\(?:"
	      (regexp-opt hatedara-link-keyword)
	      "\\):[^]> \n]+")
     (0 hatedara-link-face t))
    (,(concat "\\[\\(?:"
	      (regexp-opt hatedara-square-bracket-link-keyword)
	      "\\):[^]\n]+\\]")
     (0 hatedara-link-face t))
    (,(concat "\\(\\[\\(?:"
	      (regexp-opt hatedara-square-bracket-link-keyword)
	      "\\):[^]\n]+:\\(?:title\\|image\\)=\\)\\(.*?\\)\\(\\]\\)")
     (1 hatedara-link-face t)
     (2 hatedara-link-title-face t)
     (3 hatedara-link-face t))
    ;; ��ߥ��
    ("\\[\\][^\n]+\\[\\]"
     (0 hatedara-disable-link-face t))
    ;; �����ȥ�
    ("\\`.+"
     (0 hatedara-title-face t))
    ("\\`delete$"
     (0 font-lock-warning-face t))))

;;; Hooks:

(defvar hatedara-mode-hook nil
  "`hatedara-mode' ���ϻ��˸ƤФ�� hook��")
(defvar hatedara-submit-hook nil
  "����ľ���˸ƤФ�� hook��")
(defvar hatedara-submitted-hook nil
  "������λ��˸ƤФ�� hook��")

;;; Internal Functions:

(add-to-list 'auto-mode-alist
	     (cons (expand-file-name hatedara-file-name-regexp
				     hatedara-directory)
		   'hatedara-mode))

(defun hatedara-check-directory (dir)
  (if (file-exists-p dir)
      (unless (file-directory-p dir)
	(error "�ե����뤬¸�ߤ��ޤ�: %s" dir))
    (make-directory dir)
    (set-file-modes dir #o700)))

(defun hatedara-time2filename (time &optional date-offset)
  (format-time-string
   "%Y-%m-%d.txt"
   (apply 'encode-time
	  (let ((date (nthcdr (if date-offset 3 2) (decode-time time))))
	    (setcar date (+ (car date)
			    (or date-offset (- hatedara-time-offset))))
	    (while (> 9 (length date))
	      (setq date (cons 0 date)))
	    date))))

(defun hatedara-filename2time (name)
  (let ((time (nreverse
	       (mapcar 'string-to-number
		       (split-string (file-name-sans-extension
				      (file-name-nondirectory name))
				     "-")))))
    (while (> 6 (length time))
      (setq time (cons 0 time)))
    (apply 'encode-time time)))

(defun hatedara-open-diary (file)
  (find-file (expand-file-name file hatedara-directory))
  (hatedara-mode))

(defun hatedara-find-diary (name forward)
  (setq name (file-name-nondirectory name))
  (let ((ret (funcall (if forward 'car 'last)
		      (delq nil (mapcar (lambda (f)
					  (and
					   (apply 'string-lessp
						  (if forward
						      `(,name ,f)
						    `(,f ,name)))
					   f))
					(directory-files
					 hatedara-directory nil
					 hatedara-file-name-regexp))))))
    (if (listp ret) (car ret) ret)))

(defun hatedara-find-pf-subr (num &optional forward)
  (let ((file (if (numberp num)
		  (hatedara-time2filename
		   (hatedara-filename2time (buffer-file-name)) num)
		(or (hatedara-find-diary (buffer-file-name) forward)
		    (progn (message "%s�Υե�����Ϥ���ޤ���"
				    (if forward "ľ��" "ľ��"))
			   nil)))))
    (and file (hatedara-open-diary file))))

(defun hatedara-script-option (options &optional toggle)
  (let ((hatedara-trivial (or (and toggle (not hatedara-trivial))
			      (and (not toggle) hatedara-trivial)))
;; 	(options (if hatedara-submit-async
;; 		     (delq (assq 'hatedara-passwd options) options)
;; 		   options))
	ret)
    (dolist (opt options)
      (when (eval (car opt))
	(setq ret (nconc (mapcar (lambda (x)
				   (setq x (cond ((stringp x) x)
						 ((symbolp x) (eval x))
						 (t x)))
				   (if (numberp x) (number-to-string x) x))
				 (cdr opt))
			 ret))))
    ret))

(defun hatedara-submit-post-failure ()
  (with-current-buffer hatedara-log-buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward hatedara-login-err-msg (point-max) t)
	(hatedara-clear-userdata))))
  (beep)
  (display-warning 'hatedara "Submit incomplete"
		   :error hatedara-log-buffer))

(defun hatedara-submit-sync (args)
  (message "Submitting...")
  (let (ret)
    (with-temp-buffer
      (insert (format "%s\n" hatedara-passwd))
      (setq ret (apply 'call-process-region (point-min) (point-max)
		       hatedara-interpreter t hatedara-log-buffer t
		       (expand-file-name hatedara-script-file)
		       args)))
    (if (and (numberp ret) (zerop ret))
	(progn
	  (when (and hatedara-auto-reload
		     (eq major-mode 'hatedara-mode)
		     (not (buffer-modified-p)))
	    (let ((current (buffer-file-name)))
	      (erase-buffer)
	      (insert-file-contents current t)))
	  (message "Submitting...done")
	  (run-hooks 'hatedara-submitted-hook))
      (hatedara-submit-post-failure))))

(defun hatedara-submit-async (args)
  (message "Hatedara submitting...")
  (let ((proc (apply 'start-process "Hatedara submit" hatedara-log-buffer
		     hatedara-interpreter
		     (expand-file-name hatedara-script-file) args)))
    (set-process-filter proc 'hatedara-send-password)
    (set-process-sentinel proc 'hatedara-submit-sentinel)))

(defun hatedara-send-password (proc string)
  (when (get-buffer hatedara-log-buffer)
    (with-current-buffer hatedara-log-buffer
      (let ((moving (= (point) (process-mark proc))))
	(save-excursion
	  (goto-char (process-mark proc))
	  (insert string)
	  (set-marker (process-mark proc) (point))
	  (goto-char (point-min))
	  (when (re-search-forward "^Password: \\'" (point-max) t)
	    (process-send-string proc (format "%s\n" hatedara-passwd))))
	(and moving (goto-char (process-mark proc)))))))

(defun hatedara-submit-sentinel (proc event)
  (when (get-buffer hatedara-log-buffer)
    (with-current-buffer hatedara-log-buffer (insert event)))
  (if (string-match "finished\n" event)
      (progn
	(message "Hatedara submit complete")
	(run-hooks 'hatedara-submitted-hook))
    (hatedara-submit-post-failure)))

;;; User Functions:

(eval
 `(define-derived-mode hatedara-mode ,hatedara-parent-mode "Hatedara"
    "�Ϥƥ���⡼�ɡ�
\\{hatedara-mode-map}"
    (set (make-local-variable 'comment-start-skip) hatedara-comment-start-skip)
    (set (make-local-variable 'comment-end-skip) hatedara-comment-end-skip)
    (set (make-local-variable 'comment-start) hatedara-comment-start)
    (set (make-local-variable 'comment-end) hatedara-comment-end)
    (define-key hatedara-mode-map hatedara-submit-key 'hatedara-submit)
    (define-key hatedara-mode-map hatedara-insert-tag-key 'hatedara-insert-tag)
    (define-key hatedara-mode-map
      hatedara-find-previous-key 'hatedara-find-previous)
    (define-key hatedara-mode-map
      hatedara-find-following-key 'hatedara-find-following)
    (font-lock-add-keywords 'hatedara-mode hatedara-font-lock-keywords)
    (font-lock-mode 1)))

;; FIXME:
;; (defun hatedara-list-article ()
;;   (interactive)
;;   )

(defun hatedara-submit (&optional user pass)
  "`hatedara-script-file' ��ư�����������������롥
���ְ�����Ϳ����� `hatedara-trivial' �εդΥ⡼�ɤ��������롥"
  (interactive
   (list (or hatedara-user-id
	     (read-string "UserID: "))
	 (or hatedara-passwd
	     (read-passwd "Password: "))))
  (when (and (eq major-mode 'hatedara-mode)
	     (buffer-modified-p)
	     (y-or-n-p "��������¸���ޤ���? "))
    (save-buffer))
  (setq hatedara-user-id user
	hatedara-passwd pass)
  (run-hooks 'hatedara-submit-hook)
  (when (get-buffer hatedara-log-buffer)
    (with-current-buffer hatedara-log-buffer
      (erase-buffer)))
  (let ((default-directory (file-name-as-directory hatedara-directory)))
    (funcall (if hatedara-submit-async
		 'hatedara-submit-async
	       'hatedara-submit-sync)
	     (hatedara-script-option hatedara-script-option-list
				     current-prefix-arg))))

(defun hatedara-insert-tag ()
  "���ѡ�pre �������������롥
�꡼����󤬥����ƥ��֤ʤ�Ф����Ϥ�褦���������롥"
  (interactive)
  (let* ((taglist '((?h ">>" "<<") (?j ">|" "|<") (?k ">||" "||<")
		    (?l ">|FILETYPE|" "||<")))
	 (prompt (mapconcat (lambda (x) (format "%c:%s" (car x) (cadr x)))
			    taglist " "))
	 tag filetype)
    (while (progn (message prompt)
		  (null (setq tag (assq (read-char) taglist)))))
    (when (eq ?l (car tag))
      (setq filetype
	    (completing-read "filetype: " hatedara-syntax-filetype nil t)))
    (if (and mark-active transient-mark-mode)
	(hatedara-insert-tag-around-region tag filetype)
      (mapc (lambda (x) (insert x "\n")) (cdr tag))
      (when filetype
	(forward-line -2)
	(forward-char 2)
	(delete-char 8)
	(insert filetype)
	(forward-line 2))
      (forward-line -1)
      (insert "\n")
      (forward-line -1))))

(defun hatedara-insert-tag-around-region (tag &optional filetype)
  (let ((beg (min (mark) (point)))
	(end (max (mark) (point))))
    (goto-char end)
    (insert (nth 2 tag) "\n")
    (goto-char beg)
    (insert (nth 1 tag))
    (when filetype
      (backward-char 1)
      (delete-backward-char 8)
      (insert filetype)
      (forward-char))
    (insert "\n")))

(defun hatedara-find-previous (&optional num)
  "¸�ߤ���ľ���������򳫤���
NUM �����ͤǤ���Ф�������ʬ��������������ե������̵ͭ�˴ؤ�餺������"
  (interactive "P")
  (setq num (if (listp num) (car num) num))
  (hatedara-find-pf-subr (and (numberp num) (- num))))

(defun hatedara-find-following (&optional num)
  "¸�ߤ���ľ��������򳫤���
NUM �����ͤǤ���Ф�������ʬ�������������ե������̵ͭ�˴ؤ�餺������"
  (interactive "P")
  (setq num (if (listp num) (car num) num))
  (hatedara-find-pf-subr num t))

(defun hatedara-clear-userdata ()
  "`hatedara-user-id' �� `hatedara-passwd' �򥯥ꥢ���롥"
  (interactive)
  (setq hatedara-user-id nil
	hatedara-passwd nil))
(defalias 'hatedara-logout 'hatedara-clear-userdata)

(defun hatedara (&optional offset)
  "�����������򳫤���
OFFSET �����ͤǤ���Ф�������ʬ�������夷�������򳫤���"
  (interactive "P")
  (hatedara-check-directory hatedara-directory)
  (hatedara-open-diary
   (hatedara-time2filename (current-time) (when (numberp offset) offset))))

(provide 'hatedara)
;;; hatedara.el ends here
