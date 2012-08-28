;; SKK English tutorial text -*- coding: iso-2022-jp -*-
;; Copyright (C) 1992, 1996, 1998
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Copyright (C) 1999, 2000, 2001, 2002
;; SKK Development Team <skk@ring.gr.jp>
;;
;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: SKK.tut.E,v 1.14 2012/08/28 12:56:48 skk-cvs Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2012/08/28 12:56:48 $
;;
;; This is the tutorial text for Daredevil SKK version 11.1 and and later
;; versions.
;;
;; Permission is granted to make and distribute verbatim copies of this
;; tutorial text provided the copyright notice and this permission notice
;; are preserved on all copies.
;;
;; Permission is granted to copy and distribute modified versions of this
;; tutorial text under the conditions for verbatim copying, provided that
;; the entire resulting derived work is distributed under the terms of a
;; permission notice identical to this one.
;;
;; Permission is granted to copy and distribute translations of this
;; tutorial text into another language, under the above conditions for
;; modified versions, except that this permission notice may be stated in
;; a translation approved by Masahiko Sato.
----
== How to use the SKK tutorial ==

SKK is a software system that provides a very fast and efficient Japanese
input environment for Emacs.  As it is implemented based on a very simple
principle, you can master the usage of SKK quickly.  This tutorial explains
SKK assuming that you know enough about Emacs.

You will learn the usage of SKK by solving problems.  The last line of each
Problem buffer will begin with 【>>】.  You have to make line in the Solution
buffer as same as the last line of each Problem buffer.  When you have
done, type 『C-x n』(Next).  (『C-x』 means <control-x>, that is, typing
『x』 key holding the control key.)  If your solution is correct, the
buffers will be updated and you will proceed to the next problem.

Type 『C-x q』(Quit) if you wish to quit the tutorial.  Type 『C-x s』
(Skip) if you wish to skip the problem.

>> (concat "My name is " (skktut-user-name) ".")
----
== Invoking SKK, かな mode ==

In the following, if you encounter lines beginning with 【!!】, follow the
instructions there.

You can invoke SKK by typing『C-x C-j』.  Let's try that.

!! Type 『C-x C-j』.

Note that a part of the mode line which shows minor modes of the Solution
buffer is now 【かな】.  This mode is called かな mode.  In this mode,
lowercase Latin letters are interpreted as Romazi (ローマ字) and will be
converted to the corresponding hirakanas.  For instance, if you type
『aiueo』, you will get 【あいうえお】.  (In かな mode, don't use
uppercase Latin characters until we explain about them.)

>> かきくけこ
----
== From かな mode to ASCII mode ==

You can move from かな mode to ASCII mode by typing 『l』(for Latin).

!! Type 『l』 now.

Note also how the mode line changed.

In ASCII mode, keys will be interpreted as ordinary Emacs commands.  It is
almost identical with the state you were in just before you invoked SKK,
and we will explain the tiny difference soon.  In ASCII mode, you will see
【SKK】 in the mode line, and this is to indicate that SKK has been
invoked in this buffer.

>> Now we are in ASCII mode.
----
== From ASCII mode to かな mode ==

You can move from ASCII mode to かな mode by typing 『C-j』.  In summary:

	 l  : かな --> ASCII
	C-j : ASCII --> かな

>> ASCII から かな
----
== カナ mode ==

Input of katakana (カタカナ) is done in カナ mode.  You can move
between かな mode and カナ mode by typing 『q』(for qana = かな/カナ
(^_^)).

	q : かな --> カナ
	q : カナ --> かな

>> ひらかな から カタカナ そしてまた ひらかな
----
== How to input 【ん】 ==

【ん】 can be inputted by 『nn』 or 『n'』.  If it will be followed by a
consonant except 『n』 and 『y』, you may simply type 『n』.

>> かなかんじへんかん
----
== How to imput small ひらかな characters==

You can enter each small hirakana character by prefixing 『x』 to its
Romazi representation, e.g.:

	『xa』==>【ぁ】、『xya』==>【ゃ】

However, in most cases, you can enter them like this:

	『chotto』==>【ちょっと】、『tyotto』==>【ちょっと】

>> あっ そうだったのか
----
== Input of long vowel ==

A long vowel may be entered by using:

	『-』==>【ー】

>> えーと
----
== 全英 (zenei) mode ==

Zenkaku (全角, square shaped) character strings like 【ＳＫＫ】 may be
entered in 全英 (zenei) mode.  In 全英 mode, Latin characters 『a』-『z』
and 『A』-『Z』 are input as square shaped alphabets (normally occupying
double horizontal space compared to ASCII characters).  From either かな
mode or カナ mode, you can move to 全英 mode by typing 『L』(for Large
sized Latin).  From 全英 mode, you can return to かな/カナ mode by 『C-j』.

	 L  : かな/カナ -->   全英
	C-j :   全英    --> かな/カナ

>> ＳＫＫがつかえるのはＥｍａｃｓだけです
----
== Summary up to this point ==

You can invoke SKK by 『C-x C-j』.  SKK has four modes:

	ASCII mode, かな mode, カナ mode and 全英 mode

You can change modes as follows:

	ASCII  == C-j ==>  かな/カナ  == l ==> ASCII

	かな <== q ==> カナ

	全英   == C-j ==>  かな/カナ  == L ==> 全英

>> ＳＫＫには ASCIIモード と かなモード と カナモード と ぜんえいモード がある
----
== かな mode ==

In かな mode, the following keys have special meanings.  Other keys such
as 『C-a』, 『%』 are interpreted as ordinary Emacs keys.  This is also
the case for カナ mode.

	l, q, x and L		Already explained.
	Other lowercases	Interpreted as Romazi, and entered as かな.
	Other uppercases	Entering kana-to-kanji conversion mode.
	/			Entering ASCII conversion mode.
				(To be explained later.)
	,			Inserting 【、】.
	.			Inserting 【。】.
	[ and ]			Inserting 【「】 and 【」】 respectively.

!! Try typing 『,』『.』『[』 and 『]』.

>> 「あ、そうだったのか。」
----
== Kana-to-kanji conversion ==

Kana-to-kanji conversion is done in かな mode.

!! If you are not in かな mode now, change mode to かな mode, and type
!! 『Kanji』.  (Note that the first character is an uppercase character.)

The buffer should become 【▽かんじ】.  You are in ▽ mode now.

!! Now hit <space> key.

You have: 【▼漢字】.  You are in ▼ mode now.

!! Type 『C-j』.

You have solved the following problem!

>> 漢字
----
Let's try another one.

!! If you are not in かな mode now, change mode to かな mode, type
!! 『Kanji』, and then hit <space> key twice.

The buffer should change as follows:

	【▽かんじ】 ==> 【▼漢字】 ==> 【▼幹事】

!! Type 『C-j』.

You've got the solution!

>> 幹事
----
This one is slightly more complicated, so type carefully.  『□』 will
denote a <space> key.  If you see 『□□』, for instance, type <space> key
twice.

!! Change mode to かな mode if necessary and type
!! 『Kanji□□noNyuuryoku□haYoui□desu.』.

>> 漢字の入力は容易です。
----
!! In かな mode, type 『kanji』.

You get 【かんじ】.  Suppose, however, that you should have typed 『Kanji』.
You can delete three characters backwards and type 『Kanji』 in such a case.
But, there is also the following alternative way to achieve the same effect.

!! Type 『C-b』(backward-char) three times.

The point is now on 【か】.

!! Type 『Q』.

【▽】 is inserted before 【か】, and you are in ▽ mode.

!! Type 『C-e』(end-of-line), 『□』(<space>) and 『C-j』.

>> 漢字
----
== Sub modes of かな mode ==

かな mode is divided into two submodes: hirakana no-conversion mode and
conversion to Kanji mode.  Hirakana no-conversion mode will be referred to
as ■ mode.  Conversion to kanji mode is further divided into two modes:
▽ mode and ▼ mode.  Namely, かな mode is divided into three submodes:

	■ mode, ▽ mode, ▼ mode

In ■ mode, lowercase Romazi characters you type are converted to
hirakanas and are inserted into the buffer.  These hirakanas are not
converted to Kanji anymore.  ■ mode is the basic mode of SKK and you
should remember this point.

>> ひらかなかくていにゅうりょくは、なんども、れんしゅうしてきました。
----
== ▽ mode and ▼ mode ==

Kana-to-kanji conversion in SKK is done as follows.
	1 Determine the string which is the source of conversion. (▽ mode)
	2 Show candidates of conversion for the source. (▼ mode)
	3 Select the candidate you want and finish the conversion. (▼ mode)
As we have already seen, in the case of entering 【漢字】, the source
string of the conversion is 【かんじ】.  If you type 『Kanji』, 【かんじ】
will be entered in ▽ mode.  If you hit <space> in ▽ mode, the string
between 【▽】 and the point becomes the source of the conversion.  At
the same time, the mode becomes ▼ mode and the first candidate will be
shown.  In ▼ mode, <space> displays the next candidate.  If you type
『C-j』 in ▼ mode, the current candidate becomes the final result of the
conversion, and the mode becomes ■ mode.

Hint: 洗濯(せんたく), 選択(せんたく)

>> 「洗濯」を選択する。
----
Modes related to SKK usually change as follows.

	■ == Uppercase char. ==> ▽ == <space> ==> ▼ == 『C-j』 ==> ■

	▽ == 『C-j』 ==> ■

is used to go back to ■ mode when you enter ▽ mode by mistake.

	▼ == <return> and all the pritable keys  ==> ■

means that these keys automatically select the current candidate as the
final result of conversion.  This action is called 確定 (Kakutei).  Let
us try this.

!! Type 『Kyouto□,Oosaka□,Koube□.』 in ■ mode.  Recall that
!! 『□』 denotes a <space> key.

>> 京都、大阪、神戸。
----
We have learned that, in ▼ mode, <return> and all the printable keys
cause 確定 as side effects.

Moreover, if you type any uppercase character except 『L』, the
current candidate becomes the final result and enter ▽ mode.

	▼ == uppercase except 『L』 ==> ▽

!! Type 『Kana□Kanji□Henkan□.』 in ■ mode.

>> 仮名漢字変換。
----
!! Type 『Kanakanjihenkan□』 in ■ mode.

Your buffer becomes 【▼かなかんじへんかん】 and you should see the prompt:
	かなかんじへんかん
in the minibuffer.  In ▼ mode, the source string to be converted is
already fixed.  In our case, SKK tried to convert the string 【かなかんじ
へんかん】, but could not find it in 辞書. (i.e., dictionary)
In such a case, SKK automatically enters register-a-word-to-dictionary
mode (this mode will be referred to as 辞書登録 mode).
  In 辞書登録 mode, the string for which the conversion failed becomes the
key word (見出し語, midashigo) for the new entry to be registered to 辞書.

!! Verify that the cursor is in the minibuffer, and type
!! 『Kana□Kanji□Henkan□』 followed by <return> key.

>> 仮名漢字変換
----
We have just added the line

	かなかんじへんかん /仮名漢字変換/

to the 辞書 buffer.  We can verify this as follows.

!! Type 『Kanakanjihenkan□』 in ■ mode, and
!! 確定 (Kakutei, finalize) it by 『C-j』.

>> 仮名漢字変換
----
== empty 登録 (registration) ==

If you enter 辞書登録 mode by mistake, you can abort it by hitting 『C-g』
or by registering an empty string.

!! Type 『Kanakanjihenken□』 in ■ mode.

辞書 does not contain 【かなかんじへんけん】, so you get 【かなかんじへん
けん】 in the minibuffer.

!! Verify that the cursor is in the minibuffer, and hit <return>.

The 【*答*】 buffer now becomes 【▽かなかんじへんけん】.

!! Verify that 【*答*】 is the current buffer.  Hit <delete> key
!! twice, and then type 『kan□.』.

>> 仮名漢字変換。
----
We assume that 辞書 contains the line:

	ご /五/互/伍/午/呉/吾/娯/後/御/悟/梧/檎/瑚/碁/語/誤/護/醐/

You can enter 【碁】 as follows.

!! Type 『Go□□□□□□』 in ■ mode. (You have to hit <space> six times.)

You now see the line:

	A:檎  S:瑚  D:碁  F:語  J:誤  K:護  L:醐  [残り 0]

in the echo area.  As we have just seen, after the fifth candidate, seven of
them are displayed together in the echo area.  To select 【碁】, type
lowercase 『d』.

>> 碁
----
!! Type 【Go□】 in ■ mode.

This time 【碁】 is displayed as the first candidate.  This is because we
have done 確定 by that character.  But we want 【午】 this time.

!! Type 『□□□□□□』. (six <space>'s.)

We entered 辞書登録 mode as we ran out of candidates.  Actually 【午】 was
among the candidated but we had overrun.

!! Hit <return> key and type 『x』.

<return> key registers an empty string (which actually makes no real
registration) and we return to the state just before 辞書登録.  In ▼ mode,
we can return to previous candiate(s) by the 『x』 key.  You should now be
able to select 【午】 from the candidates in the echo area.

>> 午
----
We try 【午後】 this time.

!! Type 『Gogo□』in ■ mode.

As there is no 【ごご】 in 辞書, we enter 辞書登録 mode.

!! Verify that the minibuffer prompt is 【ごご】, and type 『Go□』.

The minibuffer becomes 【ごご ▼午】.

!! Then, type 『Go□□□□□j』.

We have 【ごご 午後】 in the minibuffer.

!! Hit <return> key.

>> 午後
----
We now have the entry 【ごご /午後/】 in the 辞書 buffer.  So the
following problem should be easy.

>> 午後の碁。
----
== Recursive 辞書登録 ==

!! Type 『Saikiteki□』 in ■ mode.
!! Verify the prompt 【さいきてき】 in the minibuffer, and type 『Saiki□』.

We don't have 【さいき】 in 辞書 either, so we recursively
(i.e., 再帰的に) enter 辞書登録 mode.

!! Construct 【再帰】 in the minibuffer.  (Hint: 再(さい), 帰(き))
!! Hit <return> key after you have 【さいき 再帰】 in the minibuffer.

Now we can continue the registration of 【さいきてき】.

!! You should have 【さいきてき 再帰】 in the minibuffer.  Construct 【的】.

Hint: 辞書(じしょ), 登録(とうろく), 学習(がくしゅう)

>> 再帰的辞書登録を学習しました。
----
== How to enter words with okurigana ==

Words like 「動く」 have okurigana parts, and SKK 辞書 have entries like:

	うごk /動/

for these words.  We make use of these entries to enter words with
okuriganas.

!! in ■ mode, type 『UgoKounimoUgoKenakatta.』.

Some nouns become verbs by affixing them with 「する」, but for such verbs
it is recommened to enter the original noun and the okurigana part
separately.

>> 動こうにも動けなかった。
----
;; (save-excursion (save-match-data (set-buffer " *skk-tut-jisyo*") (goto-char skk-okuri-ari-min) (when (search-forward "\nうごk /動/" skk-okuri-nasi-min t) (delete-region (progn (beginning-of-line) (point)) (progn (forward-line 1) (point))))))
== How to register words with okurigana ==

Let's see what happens if 辞書 does not have 「うごk」 as 見出し語.

!! In ■ mode, type『UgoKu』.

We enter 辞書登録 mode in the minibuffer, and we have the prompt 【うご*く
】.  As 辞書 has the entry 「どう /動/」, we can make 【動】 in the
minibuffer.

Remark: You have only to construct the main part without the okurigana,
in the minibuffer.

!! Hit <return> key after you have 【動】 in the minibuffer.

>> 動く
----
== How to enter words containing double consonants ==

We do this like the following:

	『TukaTta□』 → 【使った】
	『HosSuru□』 → 【欲する】

where our 辞書 is like:

	つかt /使/
	ほっs /欲/

Hint: 人口(じんこう), 一億(いちおく), 達した(たっした), 知った(しった)

>> 人口が一億に達したことを知った。
----
== ASCII conversion ==

It is possible to enter 見出し語 in ASCII characters, and transform them.
This is called 「英字変換」(ASCII conversion).

!! In ■ mode, type 『/file□』.

Typing 『/』 in ■ mode lets you enter ▽ mode.  Unlike ordinary ▽ mode,
however, the ASCII characters you type will be entered without any
transformations.  In this case, our 辞書 does not have the 見出し語
「file」, we enter 辞書登録 mode.

!! Verify that the prompt in the minibuffer is 【file】, type
!! 『qfairu』, and then hit <return> key.

>> 「ファイル」が登録されました。
----
== Symbols ==

The following symbols are registered in 辞書 under the 見出し語
「きごう」.

      　 、 。 ， ． ・ ： ； ？ ！ ゛ ゜ ´ ｀ ¨ ＾ ￣ ＿ ヽ ヾ ゝ ゞ 〃
   仝 々 〆 〇 ー ― ‐ ／ ＼ 〜 ‖ ｜ … ‥ ‘ ’ “ ” （ ） 〔 〕 ［ ］
   ｛ ｝ 〈 〉 《 》 「 」 『 』 【 】 ＋ − ± × ÷ ＝ ≠ ＜ ＞ ≦ ≧ ∞
   ∴ ♂ ♀ ° ′ ″ ℃ ￥ ＄ ¢ £ ％ ＃ ＆ ＊ ＠ § ☆ ★ ○ ● ◎ ◇
      ◆ □ ■ △ ▲ ▽ ▼ ※ 〒 → ← ↑ ↓ 〓

>> 〒980
----
== Greek and Cyrillic characters ==

They are registered in the Jisyoes as follows
(in classical Jisyoes, Russia and russia
were used instead of Cyrillic and cyrillic).

  Greek /Α/Β/Γ/Δ/Ε/Ζ/Η/Θ/Ι/Κ/ ...

  greek /α/β/γ/δ/ε/ζ/η/θ/ι/κ/ ...

  Cyrillic /А/Б/В/Г/Д/Е/Ё/Ж/З/И/ ...

  cyrillic /а/б/в/г/д/е/ё/ж/з/и/ ...

We can use 英字変換 (ASCII conversion) to enter these characters.

>> εδ
----
== Input by code ==

You can enter any Kanji characters by their codes (either JIS or EUC).

For example, the JIS code for 「學」 is 「555C」.  So you can enter it as
follows.

!! In ■ mode, type 『\』.

You get the prompt
【7/8 bits or KUTEN code for japanese-jisx0208 (00nn or CR for Jump Menu):】.

!! Type 『555c』 and hit <return> key.

Hint: 東北(とうほく), 大(だい)

>> 東北大學
----
== Completion of 見出し語 ==

Hitting <tab> key in ▽ mode invokes completion of 見出し語.  Let σ be
the string you entered in ▽ mode just before you type <tab>.  Then the
first occurrence (if any) of such a τ in the 辞書 buffer that has σ as
its initial segement and distinct from σ will be displayed.

!! In ■ mode, type 『konoHikouki□yorianoHi』.

The buffer becomes 【この飛行機よりあの▽ひ】.

!! Now, hit <tab> key.

Completion is performed and the buffer becomes
【この飛行機よりあの▽ひこうき】.

>> この飛行機よりあの飛行機にしよう。
----
The 見出し語 search for the completion is performed starting from the
beginning of the 辞書 buffer.  Therefore, if there are several 見出し語
in the 辞書 buffer whose initial segment matches σ, then the first such
string will be displayed.  If that is not what you expected, you can type
『.』(period) to get the second candidate, and you can continue to type 『.』
to get more candates.  If you have overrun, you can back up by typing
『,』(comma) several times.

Let us try to enter the sentence 「佐藤と斎藤では佐藤のほうがありふれた名
字だ。」.

!! In ■ mode, type 『Satou□toSaitou□dehaSa』 and hit <tab> key.

The buffer becomes 【佐藤と斎藤では▽さいとう】.

!! Type 『..,』.

>> 佐藤と斎藤では佐藤のほうがありふれた名字だ。
----
When SKK is invoked for the first time in the current Emacs session, it
reads in your personal 辞書 file into the 辞書 buffer.  The 辞書 buffer
will be updated each time you make a 確定.
When you exit from Emacs, SKK saves the content of the 辞書 buffer into
your personal 辞書 file.  So, normally, you don't have to be aware of
saving the 辞書 buffer.  When SKK updates your personal 辞書 file
(.skk-jisyo) it backups the content of the current .skk-jisyo to
.skk-jisyo.BAK.  This backup file may be useful if your .skk-jisyo is
damaged accidentally.

SKK has a lot more features we couldn't explain in this short tutorial.
For further information about SKK refer to the SKK manual.

Here is your last problem.  Hint: 終了(しゅうりょう)

>> これで SKK チュートリアルを終了します。
----
;; (skktut-end-tutorial)

;; Local Variables:
;; mode:skk-auto-fill
;; fill-column:74
;; End:
