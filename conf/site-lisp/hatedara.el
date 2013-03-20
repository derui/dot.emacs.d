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

;; hatedara.el は，結城さん作のはてなダイアリー更新スクリプト
;; はてなダイアリーライター(略称:はてダラ) の Emacs frontend です．
;;   はてダラのページ - http://www.hyuki.com/techinfo/hatena_diary_writer.html
;;
;; セットアップ
;;
;;   0. はてダラを使える状態にしておく
;;   1. hatedara.el を load-path の通ったところに置く
;;   2. .emacs に以下の設定を加える
;;
;; (autoload 'hatedara "hatedara" nil t)
;; (setq hatedara-directory "~/diary")         ; 日記を保存するディレクトリ
;; (setq hatedara-script-file "~/bin/hw.pl")   ; はてダラスクリプトの指定
;;
;; 使い方
;;
;;   1. M-x hatedara で今日の日付の日記ファイルを開きます．
;;      C-u -2 M-x hatedara で2日前の日記ファイルになります．
;;   2. `C-cC-cC-t' (`hatedara-insert-tag') で引用，pre タグが挿入でき
;;      ます．
;;   3. `C-cC-cC-b' (`hatedara-find-previous') で直前の，
;;      `C-cC-cC-f' (`hatedara-find-following') で直後の日記が開けます．
;;   4. `C-cC-cC-c' (`hatedara-submit') で日記を送信します．
;;      `C-uC-cC-cC-c' で「ちょっとした更新」になります．
;;
;; 設定
;;
;;   変更しそうなものは
;;     FIXME: 設定タイミングについて確認
;;     `hatedara-parent-mode' : hatedara-mode の元となるモード
;;     `hatedara-submit-key' : 送信の割り当てキー
;;     `hatedara-trivial' : `C-cC-cC-c' を「ちょっとした更新」にするかどうか
;;     `hatedara-use-cookie' : クッキーを使うかどうか
;;     `hatedara-submit-async' : 非同期で送信するかどうか
;;   あたりでしょうか．その他については
;;     M-x customize-group RET hatedara
;;   してみて下さい．
;;
;; 謝辞
;;
;;   hikigaeru さんの hatena-mode を参考にさせてもらってます．感謝．
;;     hatena-mode のページ - http://d.hatena.ne.jp/hikigaeru/20040617

;;; Code:

;;; User variables:

(defgroup hatedara nil
  "Emacs front end of Hatena diary write"
  :group 'hypermedia)

(defgroup hatedara-script nil
  "はてダラに関する設定"
  :group 'hatedara)

(defgroup hatedara-face nil
  "hatedara-mode の face"
  :group 'hatedara)

;; hatedara-mode に関する設定
(defcustom hatedara-directory (expand-file-name "~/.hatena")
  "*日記を保存するディレクトリ．"
  :type 'directory
  :group 'hatedara)

(defcustom hatedara-user-id nil
  "*はてなのユーザ名．"
  :type '(choice (const nil) (string :tag "user-id"))
  :group 'hatedara)

(defcustom hatedara-passwd nil
  "*はてなのパスワード．"
  :type '(choice (const nil) (string :tag "password"))
  :group 'hatedara)

(defcustom hatedara-script-file (expand-file-name "hw.pl" hatedara-directory)
  "*はてダラスクリプトの指定．"
  :type 'file
  :group 'hatedara)

(defcustom hatedara-parent-mode 'html-mode
  "*`hatedara-mode' の親となるモード．"
  :type 'function
  :group 'hatedara)

(defcustom hatedara-submit-key "\C-c\C-c\C-c"
  "*日記を送信するキー．"
  :type 'string
  :group 'hatedara)

(defcustom hatedara-insert-tag-key "\C-c\C-c\C-t"
  "*引用，pre タグを挿入するキー．"
  :type 'string
  :group 'hatedara)

(defcustom hatedara-find-previous-key "\C-c\C-c\C-b"
  "*直前の日記を開くキー．"
  :type 'string
  :group 'hatedara)

(defcustom hatedara-find-following-key "\C-c\C-c\C-f"
  "*直後の日記を開くキー．"
  :type 'string
  :group 'hatedara)

(defcustom hatedara-submit-async nil
  "*Non-nil ならば送信を非同期で実行する．
非同期の場合は `hatedara-auto-reload' の指定は無効 (再読み込みしない)．"
  :type 'boolean
  :group 'hatedara)

(defcustom hatedara-auto-reload nil
  "*Non-nil ならば日記を送信後に再読み込みする．
`hatedara-submit-async' が non-nil ならばこの指定は無効 (再読み込みしない)．"
  :type 'boolean
  :group 'hatedara)

(defcustom hatedara-time-offset 6
  "*日記の日付を変更する時間．"
  :type '(choice (const nil) (integer :tag "hour"))
  :group 'hatedara)

;; はてダラに関する設定
(defcustom hatedara-trivial nil
  "*Non-nil ならば「ちょっとした更新」モードで送信する．"
  :type 'boolean
  :group 'hatedara-script)

(defcustom hatedara-debug nil
  "*Non-nil ならばはてダラのデバッグ表示を有効にする．"
  :type 'boolean
  :group 'hatedara-script)

(defcustom hatedara-user-agent nil
  "*Non-nil ならば設定されている文字列をユーザエージェントとして送信する．"
  :type '(choice (const nil) (string :tag "user-agent"))
  :group 'hatedara-script)

(defcustom hatedara-timeout nil
  "*Non-nil ならば設定されている数値をタイムアウト時間に指定する．"
  :type '(choice (const nil) (integer :tag "sec"))
  :group 'hatedara-script)

(defcustom hatedara-use-cookie nil
  "*Non-nil ならばクッキーを利用する．"
  :type 'boolean
  :group 'hatedara-script)

(defcustom hatedara-submit-editing-file-only nil
  "*Non-nil ならば編集中の日記のみを送信する．"
  :type 'boolean
  :group 'hatedara-script)

(defcustom hatedara-disable-replace-timestamp nil
  "*Non-nil ならば見出しタイムスタンプの置換を禁止する．"
  :type 'boolean
  :group 'hatedara-script)

(defcustom hatedara-config-file nil
  "*Non-nil ならば設定されているファイルをはてダラの設定ファイルに指定する．"
  :type '(choice (const nil) (file :tag "config file name"))
  :group 'hatedara-script)

;;; Faces:

(defface hatedara-title-face
  '((((class color) (background light)) (:foreground "firebrick" :bold t))
    (((class color) (background dark)) (:foreground "moccasin" :bold t))
    (t (:bold t)))
  "タイトルの face"
  :group 'hatedara-face)
(defvar hatedara-title-face 'hatedara-title-face)

(defface hatedara-caption-face
  '((((class color) (background light)) (:foreground "purple" :bold t))
    (((class color) (background dark)) (:foreground "yellow" :bold t))
    (t (:bold t)))
  "見出しの face"
  :group 'hatedara-face)
(defvar hatedara-caption-face 'hatedara-caption-face)

(defface hatedara-markup-face
  '((((class color) (background light))
     (:foreground "DarkGreen" :background "light yellow" :bold t))
    (((class color) (background dark))
     (:foreground "turquoise" :background "dark slate grey" :bold t))
    (t (:bold t)))
  "はてなのマークアップの face"
  :group 'hatedara-face)
(defvar hatedara-markup-face 'hatedara-markup-face)

(defface hatedara-list-face
  '((((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark)) (:foreground "wheat"))
    (t (:bold t)))
  "リストの face"
  :group 'hatedara-face)
(defvar hatedara-list-face 'hatedara-list-face)

(defface hatedara-link-face
  '((((class color) (background light)) (:foreground "OrangeRed" :bold t))
    (((class color) (background dark)) (:foreground "magenta" :bold t))
    (t (:bold t)))
  "リンクの face"
  :group 'hatedara-face)
(defvar hatedara-link-face 'hatedara-link-face)

(defface hatedara-link-title-face
  '((((class color) (background light))
     (:foreground "OrangeRed" :background "light yellow" :bold t))
    (((class color) (background dark))
     (:foreground "magenta" :background "dark slate grey" :bold t))
    (t (:bold t)))
  "リンクの face"
  :group 'hatedara-face)
(defvar hatedara-link-title-face 'hatedara-link-title-face)

(defface hatedara-disable-link-face
  '((((class color) (background light)) (:foreground "DarkSlateGray" :bold t))
    (((class color) (background dark)) (:foreground "LightGray" :bold t))
    (t (:bold t)))
  "停止リンクの face"
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
  `(;; 見出し
    ("^\\(\\*[^\n ]*\\)\\(.*\\)"
     (1 hatedara-markup-face t)
     (2 hatedara-caption-face t))
    ;; リスト
    ("^\\([-+]+\\)\\(.*\\)"
     (1 hatedara-markup-face t)
     (2 hatedara-list-face t))
    ;; 定義リスト
    ("^\\(:\\(.+?\\):\\)\\(.*\\)"
     (1 hatedara-markup-face t)
     (2 hatedara-list-face t)
     (3 font-lock-string-face t))
    ;; 表組み
    ("^|.+|$"
     (0 hatedara-list-face t)
     ("|" (beginning-of-line) nil (0 hatedara-markup-face t))
     ("|\\(?:\\s-+\\)?\\(\\*[^|]+\\)" (beginning-of-line) nil
      (1 font-lock-keyword-face t)))
    ;; 引用，pre，続きを読む
    (,(concat "^\\(?:"
	      (concat (regexp-opt (nconc (mapcar (lambda (x)
						   (concat ">|" x "|"))
						 hatedara-syntax-filetype)
					 hatedara-markup-tag) t)
		      "\\|>s?https?://[^ \n]+>")
	      "\\)$")
     (0 hatedara-markup-face t))
    ;; 脚注
    ("\\(?:^\\|[^()]\\)\\((\\{2\\}\\)\\([^(].*?[^)]\\)\\()\\{2\\}\\)\\(?:$\\|[^()]\\)"
     (1 hatedara-markup-face t)
     (2 font-lock-comment-face t)
     (3 hatedara-markup-face t))
    ;; リンク
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
    ;; 停止リンク
    ("\\[\\][^\n]+\\[\\]"
     (0 hatedara-disable-link-face t))
    ;; タイトル
    ("\\`.+"
     (0 hatedara-title-face t))
    ("\\`delete$"
     (0 font-lock-warning-face t))))

;;; Hooks:

(defvar hatedara-mode-hook nil
  "`hatedara-mode' 開始時に呼ばれる hook．")
(defvar hatedara-submit-hook nil
  "送信直前に呼ばれる hook．")
(defvar hatedara-submitted-hook nil
  "送信完了後に呼ばれる hook．")

;;; Internal Functions:

(add-to-list 'auto-mode-alist
	     (cons (expand-file-name hatedara-file-name-regexp
				     hatedara-directory)
		   'hatedara-mode))

(defun hatedara-check-directory (dir)
  (if (file-exists-p dir)
      (unless (file-directory-p dir)
	(error "ファイルが存在します: %s" dir))
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
		    (progn (message "%sのファイルはありません"
				    (if forward "直後" "直前"))
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
    "はてダラモード．
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
  "`hatedara-script-file' を起動して日記を送信する．
前置引数を与えれば `hatedara-trivial' の逆のモードで送信する．"
  (interactive
   (list (or hatedara-user-id
	     (read-string "UserID: "))
	 (or hatedara-passwd
	     (read-passwd "Password: "))))
  (when (and (eq major-mode 'hatedara-mode)
	     (buffer-modified-p)
	     (y-or-n-p "日記を保存しますか? "))
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
  "引用，pre タグを挿入する．
リージョンがアクティブならばそれを囲むように挿入する．"
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
  "存在する直前の日記を開く．
NUM が数値であればその日付分だけ前の日記をファイルの有無に関わらず開く．"
  (interactive "P")
  (setq num (if (listp num) (car num) num))
  (hatedara-find-pf-subr (and (numberp num) (- num))))

(defun hatedara-find-following (&optional num)
  "存在する直後の日記を開く．
NUM が数値であればその日付分だけ先の日記をファイルの有無に関わらず開く．"
  (interactive "P")
  (setq num (if (listp num) (car num) num))
  (hatedara-find-pf-subr num t))

(defun hatedara-clear-userdata ()
  "`hatedara-user-id' と `hatedara-passwd' をクリアする．"
  (interactive)
  (setq hatedara-user-id nil
	hatedara-passwd nil))
(defalias 'hatedara-logout 'hatedara-clear-userdata)

(defun hatedara (&optional offset)
  "今日の日記を開く．
OFFSET が数値であればその日付分だけ前後した日記を開く．"
  (interactive "P")
  (hatedara-check-directory hatedara-directory)
  (hatedara-open-diary
   (hatedara-time2filename (current-time) (when (numberp offset) offset))))

(provide 'hatedara)
;;; hatedara.el ends here
