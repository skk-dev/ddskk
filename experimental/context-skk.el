;;; context-skk.el --- turning off skk when the point leaves "string" or ; comments
;;
;; Copyright (C) 2003 Masatake YAMATO
;;
;; Author: Masatake YAMATO <jet@gyve.org>
;; Created: Tue May 13 19:12:23 2003
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;
;;; Commentary:
;; このプログラムは編集の文脈に応じて自動的にskkのモードをlatinに切り替え
;; ます。
;;
;; あるプログラミング言語でプログラムを書いているとき、日本語入力の必要が
;; あるのは一般に、そのプログラミング言語の文字列中かコメント中に限られま
;; す。文字列、コメントの「外」を編集するときは、多くの場合日本語入力は必
;; 要ありません。
;; たとえば 「え ^H l」 emacs lispでは、
;; "〜" や ;; 〜
;; といった個所でだけ日本語入力が必要となります。
;; 
;; このプログラムでは、現在の文字列とコメントの「外」を編集開始と同時に
;; (skkがオンであれば)skkの入力モードをlatinに切り替えます。「外」の編集
;; を開始するにあたって、日本語入力が 「お ^H l」 onになっていたために発生
;; した入力誤りとその修正操作を回避することができます。
;;
;; 自動切り替えはcontext-skkというマイナーモードとして実装してあります。
;; M-x context-skk
;; で 自動切り替えのon/offをできます。モードラインに ";" が表示されている場合、
;; マイナーモードがonになっていることを意味します。
;;
;; - インストール
;; (add-hook 'skk-load-hook
;;           '(require 'context-skk))
;;
;; - todo 
;; prefix arg
;; context-context-skk.el

;;; Code: 
;;(require 'skk)

;;
;; Options
;;
(defvar context-skk-context-check-hook ()
  "*日本語入力を自動的にoffにしたい「コンテキスト」にいればtを返す関数を登録する。")
(add-hook ' context-skk-context-check-hook
	    'context-skk-out-of-string-or-comment-in-programming-mode-p)
(add-hook ' context-skk-context-check-hook
	    'context-skk-in-mew-draft-attachments-region-p)

(defvar context-skk-programming-mode
  '(ada-mode antlr-mode asm-mode autoconf-mode awk-mode
    c-mode objc-mode java-mode idl-mode pike-mode cperl-mode
    ;;?? dcl-mode
    delphi-mode f90-mode fortran-mode
    icon-mode idlwave-mode inferior-lisp-mode m4-mode makefile-mode
    metafont-mode modula-2-mode octave-mode pascal-mode perl-mode
    prolog-mode ps-mode postscript-mode scheme-mode sh-mode simula-mode
    ;; sql-mode
    tcl-mode vhdl-mode emacs-lisp-mode lisp-interaction-mode)
  "*context-skkにて「プログラミングモード」とみなすモードのリスト")


(define-minor-mode context-skk
  "文脈に応じて自動的にskkの入力モードをlatinに切り換えるマイナーモード。"
  t " \";\"")

;;
;; Advices
;;
(defadvice skk-insert (around skk-insert-ctx-switch activate)
  "文脈に応じて自動的にskkの入力モードをlatinにする。"
  (if (and context-skk (context-skk-context-check))
      (context-skk-insert) 
    ad-do-it))

(defadvice skk-jisx0208-latin-insert (around skk-jisx0208-latin-insert-ctx-switch activate)
  "文脈に応じて自動的にskkの入力モードをlatinにする。"
  (if (and context-skk (context-skk-context-check))
      (context-skk-insert) 
    ad-do-it))

;;
;; Helper
;;
(defun context-skk-context-check ()
  "日本語入力を自動的にoffにしたい「コンテキスト」にいればtを返す"
  (run-hook-with-args-until-success 'context-skk-context-check-hook))

(defun context-skk-insert ()
  "skk-latin-modeをonにした上`this-command-keys'に対する関数を呼び出し直す。"
  (skk-latin-mode t)
  (call-interactively (key-binding (this-command-keys))))

(defun context-skk-out-of-string-or-comment-in-programming-mode-p ()
  "プログラミングモードにあって文字列あるいはコメントの外にいればnon-nilを返す。
プログラミングモードにいない場合はnilを返す。
プログラミングモードにあって文字列あるいはコメントの中にいる場合nilを返す。"
  (and (context-skk-in-programming-mode-p) 
       (not (or (context-skk-in-string-p)
		(context-skk-in-comment-p)))))

(defun context-skk-in-mew-draft-attachments-region-p ()
  (and (eq major-mode 'mew-draft-mode)
       (fboundp 'mew-attach-begin)
       (<= (or (mew-attach-begin) (1+ (point-max))) (1- (point)))))
;;
;; Sub predicators
;;
(defun context-skk-in-string-p ()
  (nth 3 (parse-partial-sexp (point) (point-min))))
(defun context-skk-in-comment-p ()
  (nth 4 (parse-partial-sexp (point) (point-min))))

(defun context-skk-in-programming-mode-p ()
  (memq major-mode
	context-skk-programming-mode))

(provide 'context-skk)
;; context-skk.el ends here
