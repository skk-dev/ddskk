;; skk-ja-names.el --- localization の可能性に関する小さな試み  -*- coding: iso-2022-jp -*-

;; Copyright (C) 2010 SKK Development Team <skk@ring.gr.jp>

;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Keywords: japanese, mule, input method

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 21 世紀になり、アプリケーションの localization は 一般的なものになり
;; ましたが、GNU Emacs は基本的に英語知識が前提とされています (ドキュメ
;; ントの一部は数学英語の知識を前提にすらしています)。

;; このファイルは SKK のコマンド名・オプション名を localize することで
;; ユーザーに利点があるかどうかを確かめるために設置されました。よい命名
;; 法に至るまでは少なくとも experimental とします。

;; 今のところ思いつく基本方針

;; 1. 初心者、若年者、英語が苦手な気持ちを考える

;; 2. 便利なもの、分かりやすいものを厳選する

;; 3. Emacs において英語以外のシンボル名の入力は当然手間である。そうすると
;;    補完前提で、補完しやすい名前の付け方がよいと想像される。

;;; Code:

(defalias 'SKKモード 'skk-mode)
(defalias 'SKKのバージョン 'skk-version)
(defalias 'SKKのバグを報告する 'skk-submit-bug-report)
(defalias 'SKKのチュートリアル 'skk-tutorial)

(defvaralias 'SKK基本設定-個人ファイルを置くディレクトリ名 'skk-user-directory)

(defvaralias 'SKKキー設定-リターンキーを確定に使う? 'skk-egg-like-newline)
(defvaralias 'SKKキー設定-前候補表示するキー群 'skk-egg-like-newline)
(defvaralias 'SKKキー設定-確定に使うキー 'skk-kakutei-key)

(defvaralias 'SKK辞書設定-L辞書のファイル名 'skk-large-jisyo)
(defvaralias 'SKK辞書設定-CDB辞書のファイル名 'skk-cdb-large-jisyo)
(defvaralias 'SKK辞書設定-個人辞書を共有する? 'skk-share-private-jisyo)

(defvaralias 'SKK変換設定-送り仮名が正しい候補を優先する?
  'skk-henkan-strict-okuri-precedence)
(defvaralias 'SKK変換設定-余計な送り仮名の処理方法を選ぶ!
  'skk-check-okurigana-on-touroku)

(defvaralias 'SKK表示設定-インライン表示する? 'skk-show-inline)
(defvaralias 'SKK表示設定-ツールティップ表示する? 'skk-show-inline)
(defvaralias 'SKK表示設定-日本語メニュー表示する? 'skk-show-japanese-menu)
(defvaralias 'SKK表示設定-日本語メッセージを表示する?
  'skk-japanese-message-and-error)

(defvaralias 'SKK拡張設定-先読みしておく? 'skk-preload)
(defvaralias 'SKK拡張設定-SKKのアイコンを表示する? 'skk-show-japanese-menu)

(provide 'skk-ja-names)

;; skk-ja-names.el ends here

