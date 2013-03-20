;;; sdicf.el --- Search library for SDIC format dictionary
;;; $Id: sdicf.el,v 1.20 2002/07/02 11:17:46 tsuchiya Exp $

;; Copyright (C) 1999 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;;	   NISHIDA Keisuke <knishida@ring.aist.go.jp>
;; Created: 1 Feb 1999
;; Version: 0.9
;; Keywords: dictionary

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; $B$3$l$O!"(BSDIC$B7A<0<-=q$r8!:w$9$k$?$a$N%i%$%V%i%j$G$9!#<!$N4X?t$+$i@.(B
;; $B$j$^$9!#(B

;;     sdicf-open           - SDIC $B<-=q$N%*!<%W%s(B
;;     sdicf-close          - SDIC $B<-=q$N%/%m!<%:(B
;;     sdicf-search         - SDIC $B<-=q$+$i8!:w(B
;;     sdicf-entry-headword - $B%(%s%H%j$N8+=P$78l$rF@$k(B
;;     sdicf-entry-keywords - $B%(%s%H%j$N8!:w%-!<$N%j%9%H$rF@$k(B
;;     sdicf-entry-text     - $B%(%s%H%j$NK\J8$rF@$k(B

;; $B$=$l$>$l$N4X?t$N>\:Y$O!"4X?t$N@bL@J8;zNs$K5-=R$5$l$F$$$^$9!#(B


;;; Note:
 
;; * GNU Emacs 19.30 $B0J9_$G$"$l$P!"(B`auto-compression-mode' $B$rM-8z$K$9$k(B
;;   $B$3$H$G!"(B`direct' $BJ}<0$G05=L$7$?<-=q$rMQ$$$k$3$H$,=PMh$k!#E83+$O<+(B
;;   $BF0$G9T$J$o$l$k$?$a!"FCJL$J@_Dj$OI,MW$"$j$^$;$s!#(B
;; 
;; * $BB.EY=E;k$N$?$a(B `save-match-data' $B$K$h$k0lCW%G!<%?$NB`Hr$H2sI|$O0l(B
;;   $B@Z$7$F$$$^$;$s!#(B



;;;------------------------------------------------------------
;;;		Customizable variables
;;;------------------------------------------------------------

(defun sdicf-find-program (&rest programs)
  (if programs
      (catch 'which
	(mapcar (lambda (file)
		  (mapcar (lambda (path)
			    (if (file-executable-p (expand-file-name file path))
				(throw 'which (expand-file-name file path))))
			  exec-path))
		programs))))

(defvar sdicf-default-directory (expand-file-name "~/")
  "*Default directory for executing command.")

(defvar sdicf-egrep-command (sdicf-find-program "egrep" "egrep.exe" "grep" "grep.exe")
  "*Executable file name of egrep")

(defvar sdicf-fgrep-command (sdicf-find-program "fgrep" "fgrep.exe" "grep" "grep.exe")
  "*Executable file name of fgrep")

(defvar sdicf-array-command (sdicf-find-program "array" "array.exe")
  "*Executable file name of array")

(defvar sdicf-default-coding-system
  (if (>= emacs-major-version 20)
      (if (featurep 'mule)
	  (if (string-match "XEmacs" emacs-version)
	      (cond
	       ((memq 'euc-japan-unix (coding-system-list)) 'euc-japan-unix)
	       ((memq 'euc-jp-unix (coding-system-list)) 'euc-jp-unix))
	    'euc-japan-unix))
    (and (boundp 'MULE) *euc-japan*unix))
  "*Default coding system for sdicf.el")

;; Error Symbols
(put 'sdicf-missing-file 'error-conditions '(error sdicf-errors sdicf-missing-file))
(put 'sdicf-missing-file 'error-message "Can't find file")
(put 'sdicf-missing-executable 'error-conditions '(error sdicf-errors sdicf-missing-executable))
(put 'sdicf-missing-executable 'error-message "Can't find executable")
(put 'sdicf-invalid-strategy 'error-conditions '(error sdicf-errors sdicf-invalid-strategy))
(put 'sdicf-invalid-strategy 'error-message "Invalid search strategy")
(put 'sdicf-decide-strategy 'error-conditions '(error sdicf-errors sdicf-decide-strategy))
(put 'sdicf-decide-strategy 'error-message "Can't decide strategy automatically")
(put 'sdicf-invalid-method 'error-conditions '(error sdicf-errors sdicf-invalid-method))
(put 'sdicf-invalid-method 'error-message "Invalid search method")



;;;------------------------------------------------------------
;;;		Internal variables
;;;------------------------------------------------------------

(defconst sdicf-version "0.9" "Version number of sdicf.el")

(defconst sdicf-strategy-alist
  '((array sdicf-array-available sdicf-array-init sdicf-array-quit sdicf-array-search)
    (grep sdicf-grep-available sdicf-grep-init sdicf-grep-quit sdicf-grep-search)
    (direct sdicf-direct-available sdicf-direct-init sdicf-direct-quit sdicf-direct-search))
  "$BMxMQ$G$-$k(B strategy $B$NO"A[G[Ns(B
$BG[Ns$N3FMWAG$O!"(B
    strategy $B$N%7%s%\%k(B
    strategy $B$NMxMQ2DG=@-$r8!::$9$k4X?t(B
    strategy $B$r=i4|2=$9$k4X?t(B
    strategy $B$r=*N;$9$k4X?t(B
    strategy $B$r;H$C$F8!:w$9$k4X?t(B
$B$N(B4$B$D$NMWAG$+$i$J$k%j%9%H$H$J$C$F$$$k!#(Bstrategy $B$N<+F0H=Dj$r9T$&$H$-$O!"(B
$B$3$NO"A[G[Ns$K@h$KEPO?$5$l$F$$$k(B strategy $B$,;H$o$l$k!#(B")



;;;------------------------------------------------------------
;;;		Internal functions
;;;------------------------------------------------------------

(if (fboundp 'buffer-live-p)
    (defalias 'sdicf-buffer-live-p 'buffer-live-p)
  (defun sdicf-buffer-live-p (object) "\
Return non-nil if OBJECT is a buffer which has not been killed.
Value is nil if OBJECT is not a buffer or if it has been killed."
    (and object (bufferp object) (buffer-name object))))

(defsubst sdicf-object-p (sdic)
  "$B<-=q%*%V%8%'%/%H$+$I$&$+8!::$9$k(B"
  (and (vectorp sdic) (eq 'SDIC (aref sdic 0))))

(defsubst sdicf-entry-p (entry)
  (and (stringp entry) (string-match "^<.>\\([^<]+\\)</.>" entry)))

(defsubst sdicf-get-filename (sdic)
  "$B<-=q%*%V%8%'%/%H$+$i%U%!%$%kL>$rF@$k(B"
  (aref sdic 1))

(defsubst sdicf-get-coding-system (sdic)
  "$B<-=q%*%V%8%'%/%H$+$i(B coding-system $B$rF@$k(B"
  (aref sdic 2))

(defsubst sdicf-get-strategy (sdic)
  "$B<-=q%*%V%8%'%/%H$+$i(B strategy $B$rF@$k(B"
  (aref sdic 3))

(defsubst sdicf-get-buffer (sdic)
  "$B<-=q%*%V%8%'%/%H$+$i8!:wMQ%P%C%U%!$rF@$k(B"
  (aref sdic 4))

(defun sdicf-common-init (sdic) "\
$B6&DL$N<-=q=i4|2=4X?t(B
$B:n6HMQ%P%C%U%!$,B8:_$9$k$3$H$r3NG'$7!"$J$1$l$P?7$7$/@8@.$9$k!#:n6HMQ%P%C(B
$B%U%!$rJV$9!#(B"
  (or (and (sdicf-buffer-live-p (sdicf-get-buffer sdic))
	   (sdicf-get-buffer sdic))
      (let ((buf (generate-new-buffer (format " *sdic %s*" (sdicf-get-filename sdic)))))
	(buffer-disable-undo buf)
	(aset sdic 4 buf))))

(defun sdicf-common-quit (sdic) "\
$B6&DL$N<-=q=*N;4X?t(B"
  (if (sdicf-buffer-live-p (sdicf-get-buffer sdic)) (kill-buffer (sdicf-get-buffer sdic))))

(defsubst sdicf-search-internal () "\
$B8=:_9T$r%A%'%C%/$7!"%(%s%H%j$J$i$P8=:_9T$NFbMF$r(B entries $B$K2C$($k!#(B
$B%]%$%s%H$r9T$N@hF,$K0\F0$7$F$*$+$J$1$l$P$J$i$J$$!#4X?t$N<B9T8e!"%]%$%s(B
$B%H$O<!$N9TF,$K0\F0$9$k!#(B"
  (if (eq (following-char) ?<)
      (progn
	(setq entries (cons (buffer-substring (point) (progn (end-of-line) (point))) entries))
	(forward-char))
    (forward-line)))

(defun sdicf-encode-string (string) "\
STRING $B$r%(%s%3!<%I$9$k(B
$B%(%s%3!<%I$7$?J8;zNs$rJV$9(B"
  (let ((start 0) ch list)
    (while (string-match "[&<>\n]" string start)
      (setq ch (aref string (match-beginning 0))
	    list (cons (if (eq ch ?&) "&amp;"
			 (if (eq ch ?<) "&lt;"
			   (if (eq ch ?>) "&gt;" "&lf;")))
		       (cons (substring string start (match-beginning 0)) list))
	    start (match-end 0)))
    (eval (cons 'concat (nreverse (cons (substring string start) list))))))

(defun sdicf-decode-string (string) "\
STRING $B$r%G%3!<%I$9$k(B
$B%G%3!<%I$7$?J8;zNs$rJV$9(B"
  (let ((start 0) list)
    (while (string-match "&\\(\\(lt\\)\\|\\(gt\\)\\|\\(lf\\)\\|\\(amp\\)\\);" string start)
      (setq list (cons (if (match-beginning 2) "<"
			 (if (match-beginning 3) ">"
			   (if (match-beginning 4) "\n" "&")))
		       (cons (substring string start (match-beginning 0)) list))
	    start (match-end 0)))
    (eval (cons 'concat (nreverse (cons (substring string start) list))))))

(defun sdicf-insert-file-contents (filename coding-system &optional visit beg end replace) "\
CODING-SYSTEM $B$rL@<(E*$K;XDj$7$F(B insert-file-contents $B$r8F$S=P$9(B
CODING-SYSTEM $B0J30$N0z?t$N0UL#$O(B insert-file-contents $B$HF1$8(B"
  (let ((coding-system-for-read coding-system)
	(file-coding-system-for-read coding-system))
    (insert-file-contents filename visit beg end replace)))

(defun sdicf-call-process (program coding-system &optional infile buffer display &rest args) "\
CODING-SYSTEM $B$rL@<(E*$K;XDj$7$F(B call-process $B$r8F$S=P$9(B
CODING-SYSTEM $B0J30$N0z?t$N0UL#$O(B call-process $B$HF1$8(B"
  (let ((default-directory sdicf-default-directory)
	(coding-system-for-read coding-system)
	(coding-system-for-write coding-system)
	(process-input-coding-system coding-system)
	(process-output-coding-system coding-system)
	(file-name-coding-system coding-system)
	(default-process-coding-system (cons coding-system coding-system)))
    (apply 'call-process program infile buffer display args)))

(defun sdicf-start-process (name buffer program coding-system &rest args) "\
start-process $B$r<B9T$7$?8e!"@8@.$5$l$?%W%m%;%9$K(B CODING-SYSTEM $B$r@_Dj$9$k(B
CODING-SYSTEM $B0J30$N0z?t$N0UL#$O(B start-process $B$HF1$8(B"  
  (let* ((default-directory sdicf-default-directory)
	 (proc (apply 'start-process name buffer program args)))
    (if (fboundp 'set-process-coding-system)
	(set-process-coding-system proc coding-system coding-system)
      (set-process-input-coding-system proc coding-system)
      (set-process-output-coding-system proc coding-system))
    proc))



;;; Strategy `direct'

(defun sdicf-direct-available (sdic)
  (or (file-readable-p (sdicf-get-filename sdic))
      (signal 'sdicf-missing-file (list (sdicf-get-filename sdic)))))

(defun sdicf-direct-init (sdic)
  (or (sdicf-buffer-live-p (sdicf-get-buffer sdic))
      (save-excursion
	(sdicf-common-init sdic)
	(set-buffer (sdicf-get-buffer sdic))
	(delete-region (point-min) (point-max))
	(sdicf-insert-file-contents (sdicf-get-filename sdic) (sdicf-get-coding-system sdic))
	(while (re-search-forward "^#" nil t)
	  (delete-region (1- (point)) (progn (end-of-line) (min (1+ (point)) (point-max)))))
	(setq buffer-read-only t)
	(set-buffer-modified-p nil)
	t)))

(defalias 'sdicf-direct-quit 'sdicf-common-quit)

(defun sdicf-direct-search (sdic pattern &optional case regexp) "\
$B8!:wBP>]$N%U%!%$%k$r%P%C%U%!$KFI$_9~$s$G8!:w$r9T$&(B

$B8+$D$+$C$?%(%s%H%j$N%j%9%H$rJV$9!#(BCASE $B$,(B nil $B$J$i$P!"BgJ8;z>.J8;z$N0c(B
$B$$$r6hJL$7$F8!:w$9$k!#(BREGEXP $B$,(B Non-nil $B$J$i$P!"(BPATTERN $B$r@55,I=8=$H8+(B
$B$J$7$F8!:w$9$k!#(B"
  (sdicf-direct-init sdic)
  (save-excursion
    (set-buffer (sdicf-get-buffer sdic))
    (let ((case-fold-search case) entries)
      (goto-char (point-min))
      (if regexp
	  (while (re-search-forward pattern nil t)
	    (forward-line 0)
	    (sdicf-search-internal))
	(while (search-forward pattern nil t)
	  (forward-line 0)
	  (sdicf-search-internal)))
      (nreverse entries))))



;;; Strategy `grep'

(defun sdicf-grep-available (sdic)
  (and (or (file-readable-p (sdicf-get-filename sdic))
	   (signal 'sdicf-missing-file (list (sdicf-get-filename sdic))))
       (or (and (stringp sdicf-fgrep-command)
		(file-executable-p sdicf-fgrep-command))
	   (signal 'sdicf-missing-executable '(fgrep grep)))
       (or (and (stringp sdicf-egrep-command)
		(file-executable-p sdicf-egrep-command))
	   (signal 'sdicf-missing-executable '(egrep grep)))))

(defalias 'sdicf-grep-init 'sdicf-common-init)

(defalias 'sdicf-grep-quit 'sdicf-common-quit)

(defun sdicf-grep-search (sdic pattern &optional case regexp) "\
fgrep / egrep $B$^$?$O(B grep $B$r;H$C$F8!:w$r9T$&(B

$B8+$D$+$C$?%(%s%H%j$N%j%9%H$rJV$9!#(BCASE $B$,(B nil $B$J$i$P!"BgJ8;z>.J8;z$N0c(B
$B$$$r6hJL$7$F8!:w$9$k!#(BREGEXP $B$,(B nil $B$J$i$P(B sdicf-fgrep-command $B$G;XDj(B
$B$5$l$?%3%^%s%I$r;H$C$F8!:w$9$k!#(BREGEXP $B$,(B Non-nil $B$J$i$P(B 
sdicf-egrep-command $B$G;XDj$5$l$?%3%^%s%I$r;H$&!#(B"
  (sdicf-grep-init sdic)
  (save-excursion
    (set-buffer (sdicf-get-buffer sdic))
    (delete-region (point-min) (point-max))
    (apply 'sdicf-call-process
	   (if regexp sdicf-egrep-command sdicf-fgrep-command)
	   (sdicf-get-coding-system sdic)
	   nil t nil
	   (if regexp (if case (list "-i" "-e" pattern (sdicf-get-filename sdic))
			(list "-e" pattern (sdicf-get-filename sdic)))
	     (if case (list "-i" pattern (sdicf-get-filename sdic))
	       (list "-e" pattern (sdicf-get-filename sdic)))))
    (goto-char (point-min))
    (let (entries)
      (while (not (eobp)) (sdicf-search-internal))
      (nreverse entries))))



;;; Strategy `array'

(defun sdicf-array-available (sdic)
  (and (or (file-readable-p (sdicf-get-filename sdic))
	   (signal 'sdicf-missing-file (list (sdicf-get-filename sdic))))
       (or (file-readable-p (concat (sdicf-get-filename sdic) ".ary"))
	   (signal 'sdicf-missing-file (list (concat (sdicf-get-filename sdic) ".ary"))))
       (or (and (stringp sdicf-array-command)
		(file-executable-p sdicf-array-command))
	   (signal 'sdicf-missing-executable '(array)))))

(defun sdicf-array-init (sdic)
  (sdicf-common-init sdic)
  (let ((proc (get-buffer-process (sdicf-get-buffer sdic))))
    (or (and proc (eq (process-status proc) 'run))
	(progn
	  (setq proc (sdicf-start-process "array"
					  (sdicf-get-buffer sdic)
					  sdicf-array-command
					  (sdicf-get-coding-system sdic)
					  (sdicf-get-filename sdic)))
	  (accept-process-output proc)
	  (process-send-string proc "style line\n")
	  (accept-process-output proc)
	  (process-send-string proc "order index\n")
	  (accept-process-output proc)
	  (process-kill-without-query proc)
	  (set-process-filter proc 'sdicf-array-wait-prompt)
	  t))))

(defun sdicf-array-quit (sdic)
  (if (sdicf-buffer-live-p (sdicf-get-buffer sdic))
      (let ((proc (get-buffer-process (sdicf-get-buffer sdic))))
	(and proc
	     (eq (process-status proc) 'run)
	     (set-process-filter proc nil)
	     (process-send-string proc "quit\n"))
	(kill-buffer (sdicf-get-buffer sdic)))))

(defun sdicf-array-send-string (proc string) "\
$B;XDj$5$l$?J8;zNs(B STRING $B$r%3%^%s%I$H$7$F(B PROC $B$KEO$7$F%W%m%s%W%H$,8=$l$k$^$GBT$D4X?t(B"
  (save-excursion
    (let ((sdicf-array-wait-prompt-flag t))
      (set-buffer (process-buffer proc))
      (set-marker (process-mark proc) (point-max))
      (process-send-string proc (concat string "\n"))
      (while sdicf-array-wait-prompt-flag (accept-process-output proc)))))

(defun sdicf-array-wait-prompt (proc string) "\
$B%W%m%s%W%H(B ok $B$,8=$l$?$3$H$r8!CN$7$F!"(Bsdicf-array-wait-prompt-flag $B$r(B nil $B$K$9$k%U%#%k%?4X?t(B"
  (save-excursion
    (save-match-data ; Emacs-19.34 $B0J9_$O<+F0E*$K8!:w7k2L$NBTHr(B/$B2sI|$,9T$o$l$k$N$GITMW(B
      (set-buffer (process-buffer proc))
      (goto-char (process-mark proc))
      (insert string)
      (set-marker (process-mark proc) (point))
      (skip-chars-backward " \t\n")
      (forward-line 0)
      (if (looking-at "ok\n")
	  (setq sdicf-array-wait-prompt-flag nil))
      )))

(defun sdicf-array-search (sdic pattern &optional case regexp) "\
array $B$r;H$C$F8!:w$r9T$&(B

$B8+$D$+$C$?%(%s%H%j$N%j%9%H$rJV$9!#(Barray $B$O@55,I=8=8!:w$*$h$SBgJ8;z>.J8(B
$B;z$N0c$$$r6hJL$7$J$$8!:w$O=PMh$J$$!#=>$C$F!"(BCASE $B$,(B Non-nil $B$N>l9g$O!"(B
$BBgJ8;z>.J8;z$r6hJL$7$F8!:w$7$?>l9g$N7k2L$rJV$9!#(BREGEXP $B$,(B Non-nil $B$N>l(B
$B9g$O6u$j%9%H$rJV$9!#(B"
  (sdicf-array-init sdic)
  (if regexp
      (signal 'sdicf-invalid-method '(regexp))
    (save-excursion
      (let ((proc (get-buffer-process (set-buffer (sdicf-get-buffer sdic))))
	    (case-fold-search nil))
	(sdicf-array-send-string proc "init")
	(delete-region (point-min) (point-max))
	(sdicf-array-send-string proc (concat "search " pattern))
	(if (looking-at "FOUND:")
	    (progn
	      (delete-region (point-min) (point-max))
	      (sdicf-array-send-string proc "show")
	      (let (entries cons)
		(while (not (eobp)) (sdicf-search-internal))
		(setq entries (sort entries 'string<)
		      cons entries)
		(while (cdr cons)
		  (if (equal (car cons) (car (cdr cons)))
		      (setcdr cons (cdr (cdr cons)))
		    (setq cons (cdr cons))))
		entries)))))))


;;;------------------------------------------------------------
;;;		Interface functions
;;;------------------------------------------------------------

(defun sdicf-open (filename &optional coding-system strategy) "\
SDIC$B7A<0$N<-=q$r%*!<%W%s$9$k(B

FILENAME $B$O<-=q$N%U%!%$%kL>!#(BSTRATEGY $B$O8!:w$r9T$J$&J}<0$r;XDj$9$k0z?t(B
$B$G!"<!$N$$$:$l$+$NCM$r<h$k!#(B

    `direct' - $B<-=q$r%P%C%U%!$KFI$s$GD>@\8!:w!#(B
    `grep'   - grep $B%3%^%s%I$rMQ$$$F8!:w!#(B
    `array'  - SUFARY $B$rMQ$$$?9bB.8!:w!#(B

STRATEGY $B$,>JN,$5$l$?>l9g$O(B sdicf-strategy-alist $B$NCM$r;H$C$F<+F0E*$K(B
$BH=Dj$9$k!#(BCODING-SYSTEM $B$,>JN,$5$l$?>l9g$O!"(Bsdicf-default-coding-system
$B$NCM$r;H$&!#(B

SDIC $B<-=q%*%V%8%'%/%H$O(B CAR $B$,(B `SDIC' $B$N%Y%/%?$G$"$k!#0J2<$N(B4$B$D$NMWAG(B
$B$r;}$D!#(B
    $B!&%U%!%$%kL>(B
    $B!&<-=q$N(B coding-system
    $B!&(Bstrategy
    $B!&:n6HMQ%P%C%U%!(B
"
  (let ((sdic (vector 'SDIC filename (or coding-system sdicf-default-coding-system) nil nil)))
    (aset sdic 3 (if strategy
		     (if (assq strategy sdicf-strategy-alist)
			 (if (funcall (nth 1 (assq strategy sdicf-strategy-alist)) sdic)
			     strategy)
		       (signal 'sdicf-invalid-strategy (list strategy)))
		   (catch 'found-strategy
		     (mapcar (lambda (e)
			       (if (condition-case nil
				       (funcall (nth 1 e) sdic)
				     (sdicf-errors nil))
				   (throw 'found-strategy (car e))))
			     sdicf-strategy-alist)
		     (signal 'sdicf-decide-strategy nil))))
    sdic))

(defun sdicf-close (sdic)
  "SDIC$B7A<0$N<-=q$r%/%m!<%:$9$k(B"
  (or (sdicf-object-p sdic)
      (signal 'wrong-type-argument (list 'sdicf-object-p sdic)))
  (funcall (nth 3 (assq (sdicf-get-strategy sdic) sdicf-strategy-alist)) sdic))

(defun sdicf-search (sdic method word) "\
SDIC$B7A<0$N<-=q$+$i(B WORD $B$r%-!<$H$7$F8!:w$r9T$&(B

$B8+IU$+$C$?%(%s%H%j$N%j%9%H$rJV$9!#(BMETHOD $B$O8!:wK!$G!"<!$N$$$:$l$+$NCM(B
$B$r<h$k!#(B

    `prefix' - $BA0J}0lCW8!:w(B
    `suffix' - $B8eJ}0lCW8!:w(B
    `exact'  - $B40A40lCW8!:w(B
    `text'   - $BA4J88!:w(B
    `regexp' - $B@55,I=8=8!:w(B

$BA0J}0lCW8!:w!"8eJ}0lCW8!:w!"40A40lCW8!:w$N>l9g$OBgJ8;z(B/$B>.J8;z$r6hJL$7(B
$B$F8!:w$r9T$&!#A4J88!:w$*$h$S@55,I=8=8!:w$N>l9g$O!"(Bcase-fold-search $B$N(B
$BCM$K$h$C$FJQ2=$9$k!#$?$@$7!"(Bstrategy $B$K$h$C$F$O!";XDj$5$l$?8!:wJ}<0$K(B
$BBP1~$7$F$$$J$$>l9g$,$"$k$N$G!"Cm0U$9$k$3$H!#BP1~$7$F$$$J$$>l9g$NJV$jCM(B
$B$O!"(Bstrategy $B$K$h$k!#(B"
  (or (sdicf-object-p sdic)
      (signal 'wrong-type-argument (list 'sdicf-object-p sdic)))
  (or (stringp word)
      (signal 'wrong-type-argument (list 'stringp word)))
  (let ((case-fold-search (if (eq method 'text) case-fold-search)))
    (funcall (nth 4 (assq (sdicf-get-strategy sdic) sdicf-strategy-alist))
	     sdic
	     (cond
	      ((eq method 'prefix) (concat "<K>" (sdicf-encode-string (downcase word))))
	      ((eq method 'suffix) (concat (sdicf-encode-string (downcase word)) "</K>"))
	      ((eq method 'exact) (concat "<K>" (sdicf-encode-string (downcase word)) "</K>"))
	      ((eq method 'text) word)
	      ((eq method 'regexp) word)
	      (t (signal 'sdicf-invalid-method (list method))))
	     (and (or (eq method 'text) (eq method 'regexp)) case-fold-search)
	     (eq method 'regexp))))

(defun sdicf-entry-headword (entry)
  "$B%(%s%H%j(B ENTRY $B$N8+=P$78l$rJV$9!#(B"
  (or (sdicf-entry-p entry)
      (signal 'wrong-type-argument (list 'sdicf-entry-p entry)))
  (sdicf-decode-string (substring entry (match-beginning 1) (match-end 1))))

(defun sdicf-entry-keywords (entry &optional add-headword) "\
$B%(%s%H%j(B ENTRY $B$N8!:w%-!<$N%j%9%H$rJV$9(B
ADD-HEADWORD $B$,(B Non-nil $B$N>l9g$O8!:w%-!<$K8+=P$78l$r2C$($?%j%9%H$rJV$9(B"
  (or (sdicf-entry-p entry)
      (signal 'wrong-type-argument (list 'sdicf-entry-p entry)))
  (let ((start (match-end 0))
	(keywords (if (or add-headword (string= "<K>" (substring entry 0 3)))
		      (list (sdicf-decode-string (substring entry (match-beginning 1) (match-end 1)))))))
    (while (eq start (string-match "<.>\\([^<]+\\)</.>" entry start))
      (setq start (match-end 0)
	    keywords (cons (sdicf-decode-string (substring entry (match-beginning 1) (match-end 1))) keywords)))
    (nreverse keywords)))

(defun sdicf-entry-text (entry)
  "$B%(%s%H%j(B ENTRY $B$NK\J8$rJV$9!#(B"
  (or (stringp entry)
      (signal 'wrong-type-argument (list 'stringp entry)))
  (sdicf-decode-string (substring entry (string-match "[^>]*$" entry))))


(provide 'sdicf)

;;; sdicf.el ends here
