;; SKK English tutorial text
;; Copyright (C) 1992, 1996, 1998
;; Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;;
;; Author: Masahiko Sato <masahiko@kuis.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: SKK.tut.E,v 1.4 2001/09/12 11:16:22 czkmt Exp $
;; Keywords: japanese
;; Last Modified: $Date: 2001/09/12 11:16:22 $
;;
;; This is the tutorial text for Daredevil SKK version 11.1 and and later versions.
;;
;; Permission is granted to make and distribute verbatim copies of
;; this tutorial text provided the copyright notice and this permission notice
;; are preserved on all copies.
;;
;; Permission is granted to copy and distribute modified versions of this
;; tutorial text under the conditions for verbatim copying, provided that 
;; the entire resulting derived work is distributed under the terms of a 
;; permission notice identical to this one.
;;
;; Permission is granted to copy and distribute translations of this tutorial
;; text into another language, under the above conditions for modified
;; versions, except that this permission notice may be stated in a
;; translation approved by Masahiko Sato.
----
== How to use the SKK tutorial ==

SKK is a software system that provides a very fast and efficient Japanese
input environment for Mule.  As it is implemented based on a very simple
principle, you can master the usage of SKK quickly. This tutorial explains
SKK assuming that you know enough about Mule.

You will learn the usage of SKK by solving problems.  The bottom line of
each Problem Window will begin with $B!Z(B>>$B![(B.  You have to make a line which
has the same content as that line in the Solution Window. When you are
done, type $B!X(BC-x n$B!Y(B(Next).  ($B!X(BC-x$B!Y(B means <control-x>, that is, typing
$B!X(Bx$B!Y(B key holding the control key.)  If your solution is correct, the
window will be updated and you will proceed to the next problem.

Type $B!X(BC-x q$B!Y(B(Quit) if you wish to quit the tutorial.  Type $B!X(BC-x s$B!Y(B
(Skip) if you wish to skip the problem.

>> (concat "My name is " (user-full-name) ".")
----
== Invoking SKK, $B$+$J(B mode ==

In the following, if you encounter lines beginning with $B!Z(B!!$B![(B, follow the
instructions there.

You can invoke SKK by typing$B!X(BC-x C-j$B!Y(B.  Let's try that.

!! Type $B!X(BC-x C-j$B!Y(B.

Note that a part of the mode line which shows minor modes of the Solution
Window is now $B!Z$+$J![(B.  This mode is called $B$+$J(B mode.  In this mode,
lower case Latin letters are interpreted as Romaji ($B%m!<%^;z(B) and will be
converted to the corresponding hirakanas.  For instance, if you type
$B!X(Baiueo$B!Y(B, you will get $B!Z$"$$$&$($*![(B.  (In $B$+$J(B mode, don't use upper
case latin characters until we explain about them.)

>> $B$+$-$/$1$3(B
----
== From $B$+$J(B mode to ASCII mode ==

You can move from $B$+$J(B mode to ASCII mode by typing $B!X(Bl$B!Y(B(for latin).

!! Type $B!X(Bl$B!Y(B now.

Note also how the mode line changed.

In ASCII mode, keys will be interpreted as ordinary Emacs commands. It is
almost identical with the state you were in just before you invoked SKK,
and we will explain the tiny difference soon.  In ASCII mode, you will see
$B!Z(BSKK$B![(B in the mode line, and this is to indicate that SKK has been
invoked in this buffer.

>> Now we are in ASCII mode.
----
== From ASCII mode to $B$+$J(B mode. ==

You can move from ASCII mode to $B$+$J(B mode by typing $B!X(BC-j$B!Y(B.  In summary:

	 l  : $B$+$J(B --> ASCII
	C-j : ASCII --> $B$+$J(B

>> ASCII $B$+$i(B $B$+$J(B
----
== $B%+%J(B mode ==

Input of katakana ($B%+%?%+%J(B) is done in $B%+%J(B mode.  You can move
between $B$+$J(B mode and $B%+%J(B mode by typing $B!X(Bq$B!Y(B(for qana = $B$+$J(B/$B%+%J(B
(^_^)).

	q : $B$+$J(B --> $B%+%J(B
	q : $B%+%J(B --> $B$+$J(B

>> $B$R$i$+$J(B $B$+$i(B $B%+%?%+%J(B $B$=$7$F$^$?(B $B$R$i$+$J(B
----
== How to input $B!Z$s![(B ==

$B!Z$s![(B can be input by $B!X(Bnn$B!Y(B or $B!X(Bn'$B!Y(B.  If it will be followed by a
consonant other than $B!X(Bn$B!Y(B or $B!X(By$B!Y(B, you may simply type $B!X(Bn$B!Y(B.

>> $B$+$J$+$s$8$X$s$+$s(B
----
== How to imput small $B$R$i$+$J(B characters==

You can enter each small hirakana character by prefixing $B!X(Bx$B!Y(B to its
romaji representation, e.g.:

	$B!X(Bxa$B!Y(B==>$B!Z$!![!"!X(Bxya$B!Y(B==>$B!Z$c![(B

However, in most cases, you can enter them like this:

	$B!X(Bchotto$B!Y(B==>$B!Z$A$g$C$H![!"!X(Btyotto$B!Y(B==>$B!Z$A$g$C$H![(B

>> $B$"$C(B $B$=$&$@$C$?$N$+(B
----
== Input of long vowel ==

A long vowel may be entered by using:

	$B!X(B-$B!Y(B==>$B!Z!<![(B

>> $B$(!<$H(B
----
== $BA41Q(B (zenei) mode ==

Zenkaku ($BA43Q(B, square shaped) character strings like $B!Z#S#K#K![(B may be
entered in $BA41Q(B (zenei) mode.  In $BA41Q(B mode, latin characters $B!X(Ba$B!Y(B-$B!X(Bz$B!Y(B 
and $B!X(BA$B!Y(B-$B!X(BZ$B!Y(B are input as square shaped alphabets (normally occupying
double horizontal space compared to ASCII characters).  From either $B$+$J(B 
mode or $B%+%J(B mode, you can move to $BA41Q(B mode by typing $B!X(BL$B!Y(B(for Large
sized Latin).  From $BA41Q(B mode, you can return to $B$+$J(B/$B%+%J(B mode by $B!X(BC-j$B!Y(B.

	 L  : $B$+$J(B/$B%+%J(B -->   $BA41Q(B
	C-j :   $BA41Q(B    --> $B$+$J(B/$B%+%J(B

>> $B#S#K#K$,$D$+$($k$N$O#M#u#l#e$@$1$G$9(B
----
== Summary up to this point ==

You can invoke SKK by $B!X(BC-x C-j$B!Y(B.  SKK has four modes:

	ASCII mode, $B$+$J(B mode, $B%+%J(B mode, $BA41Q(B mode

You can change modes as follows:

	ASCII  == C-j ==>  $B$+$J(B/$B%+%J(B  == l ==> ASCII

	$B$+$J(B <== q ==> $B%+%J(B

	$BA41Q(B   == C-j ==>  $B$+$J(B/$B%+%J(B  == L ==> $BA41Q(B

>> $B#S#K#K$K$O(B ASCII$B%b!<%I(B $B$H(B $B$+$J%b!<%I(B $B$H(B $B%+%J%b!<%I(B $B$H(B $B$<$s$($$%b!<%I(B $B$,$"$k(B
----
== $B$+$J(B mode ==

In $B$+$J(B mode, the following keys have special meanings.  Other keys such
as $B!X(BC-a$B!Y(B, $B!X(B%$B!Y(B are interpreted as ordinary Emacs keys.  This is also
the case for $B%+%J(B mode.

	l$B!"(Bq$B!"(Bx$B!"(BL		Already explained.
	Other small alphabets.	Interpreted as romaji, and entered as $B$+$J(B.
	Big alphabets.		Enter kana-to-kanji conversion mode.
	/			Enter ASCII conversion mode. (To be explained.)
	,			Input as $B!Z!"![(B.
	.			Input as $B!Z!#![(B.
	[ and ]			Input as$B!Z!V![(B and $B!Z!W![(B.

!! Try typing $B!X(B,$B!Y!X(B.$B!Y!X(B[$B!Y(B and $B!X(B]$B!Y(B.

>> $B!V$"!"$=$&$@$C$?$N$+!#!W(B
----
== Kana-to-kanji conversion ==

Kana-to-kanji conversion is done in $B$+$J(B mode.

!! If you are not in $B$+$J(B mode now, change mode to $B$+$J(B mode, and type
!! $B!X(BKanji$B!Y(B.  (Note that the first character is an uppercase character.)

The window should become $B!Z"&$+$s$8![(B.  You are in $B"&(B mode now.

!! Now hit <space> key.

You have: $B!Z"'4A;z![(B.  You are in $B"'(B mode now.

!! Type $B!X(BC-j$B!Y(B.

You have solved the following problem!

>> $B4A;z(B
----
Let's try another one.

!! If you are not in $B$+$J(B mode now, change mode to $B$+$J(B mode, type
!! $B!X(BKanji$B!Y(B, and then hit <space> key twice.

The window should change as follows:

	$B!Z"&$+$s$8![(B ==> $B!Z"'4A;z![(B ==> $B!Z"'44;v![(B

!! Type $B!X(BC-j$B!Y(B.

You got the solution!

>> $B44;v(B
----
This one is slightly more complicated, so type carefully.  $B!X""!Y(B will
denote a <space> key.  If you see $B!X""""!Y(B, for instance, type <space> key
twice.

!! Change mode to $B$+$J(B mode if necessary and type
!! $B!X(BKanji$B""""(BnoNyuuryoku$B""(BhaYoui$B""(Bdesu.$B!Y(B.

>> $B4A;z$NF~NO$OMF0W$G$9!#(B
----
!! In $B$+$J(B mode, type $B!X(Bkanji$B!Y(B.

You get $B!Z$+$s$8![(B.  Suppose, however, that you should have typed $B!X(BKanji$B!Y(B.
You can delete 3 characters backwards and type $B!X(BKanji$B!Y(B in such a case.
But, there is also the following alternative way to achieve the same effect.

!! Type $B!X(BC-b$B!Y(B(backward-char) 3 times.

Cursor is now on $B!Z$+![(B.

!! Type $B!X(BQ$B!Y(B.

$B!Z"&![(B is inserted before $B!Z$+![(B, and you are in $B"&(B mode.

!! Type $B!X(BC-e$B!Y(B(end-of-line), $B!X""!Y(B(<space>), and $B!X(BC-j$B!Y(B.

>> $B4A;z(B
----
== Sub modes of $B$+$J(B mode ==

$B$+$J(B mode is divided into two submodes: hirakana no-conversion mode and
conversion to Kanji mode.  Hirakana no-conversion mode will be referred to
as $B"#(B mode.  Conversion to kanji mode is further divided into two modes: 
$B"&(B mode and $B"'(B mode.  Namely, $B$+$J(B mode is divided into three submodes:

	$B"#(B mode, $B"&(B mode, $B"'(B mode

In $B"#(B mode, lower case romaji characters you type are converted to
hirakanas and are inserted into the buffer.  These hirakanas are not
converted to Kanji anymore.  $B"#(B mode is the basic mode of SKK and you
should remember this point.

>> $B$R$i$+$J$+$/$F$$$K$e$&$j$g$/$O!"$J$s$I$b!"$l$s$7$e$&$7$F$-$^$7$?!#(B
----
== $B"&(B mode and $B"'(B mode ==

Kana-to-kanji conversion in SKK is done as follows.
	1 Determine the string which is the source of conversion. ($B"&(B mode)
	2 Show candidates of conversion for the source. ($B"'(B mode)
	3 Select the candidate you want and finish the conversion. ($B"'(B mode)
As we have already seen, in the case of entering $B!Z4A;z![(B, the source
string of the conversion is $B!Z$+$s$8![(B.  If you type $B!X(BKanji$B!Y(B, $B!Z$+$s$8![(B 
will be entered in $B"&(B mode.  If you hit <space> in $B"&(B mode, the string
between $B!Z"&![(B and the cursor becomes the source of the conversion.  At
the same time, the mode becomes $B"'(B mode and the first candidate will be
shown.  In $B"'(B mode, <space> displays the next candidate.  If you type 
$B!X(BC-j$B!Y(B in $B"'(B mode, the current candidate becomes the final result of the
conversion, and the mode becomes $B"#(B mode.

Hint: $B@vBu(B($B$;$s$?$/(B), $BA*Br(B($B$;$s$?$/(B)

>> $B!V@vBu!W$rA*Br$9$k!#(B
----
Modes related to SKK usually change as follows.

	$B"#(B == Upper case char. ==> $B"&(B == <space> ==> $B"'(B == $B!X(BC-j$B!Y(B ==> $B"#(B

	$B"&(B == $B!X(BC-j$B!Y(B ==> $B"#(B

is used to go back to $B"#(B mode when you enter $B"&(B mode by mistake.

	$B"'(B == <return> and all the pritable keys  ==> $B"#(B

means that these keys automatically select the current candidate as the
final result of conversion.  This action is called kakutei ($B3NDj(B). Let
us try this.

!! Type $B!X(BKyouto$B""(B,Oosaka$B""(B,Koube$B""(B.$B!Y(B in $B"#(B mode.  Recall that
!! $B!X""!Y(B denotes a <space> key.

>> $B5~ET!"Bg:e!"?@8M!#(B
----
We have learned that, in $B"'(B mode, <return> and all the printable keys
cause kakutei as side effects.

Moreover, if you type any upper case character other than $B!X(BL$B!Y(B, the
current candidate becomes the final result and enter $B"&(B mode.

	$B"'(B == upper case char. other than $B!X(BL$B!Y(B ==> $B"&(B

!! Type $B!X(BKana$B""(BKanji$B""(BHenkan$B""(B.$B!Y(B in $B"#(B mode.

>> $B2>L>4A;zJQ49!#(B
----
!! Type $B!X(BKanakanjihenkan$B""!Y(B in $B"#(B mode.

Your window becomes $B!Z"'$+$J$+$s$8$X$s$+$s![(B and you should see the prompt:
	$B$+$J$+$s$8$X$s$+$s(B
in the echo area.  In $B"'(B mode, the source string to be converted is
already fixed.  In our case, SKK tried to convert the string $B!Z$+$J$+$s$8(B
$B$X$s$+$s![(B, but could not found it in the Jisho ($B<-=q(B, dictionary) buffer.
In such a case, SKK automatically enters register-a-word-to-dictionary
mode (this mode will be referred to as $B<-=qEPO?(B mode).
  In $B<-=qEPO?(B mode, the string for which the conversion failed becomes the
key word ($B8+=P$78l(B, midashigo) for the new entry to be registered to $B<-=q(B
buffer.

!! Verify that the cursor is in the echo area, and type
!! $B!X(BKana$B""(BKanji$B""(BHenkan$B""!Y(B followed by <return> key.

>> $B2>L>4A;zJQ49(B
----
We have just added the line

	$B$+$J$+$s$8$X$s$+$s(B /$B2>L>4A;zJQ49(B/

to the $B<-=q(B buffer.  We can verify this as follows.

!! Type $B!X(BKanakanjihenkan$B""!Y(B in $B"#(B mode, and
!! $B3NDj(B (kakutei, finalize) it by $B!X(BC-j$B!Y(B.

>> $B2>L>4A;zJQ49(B
----
== empty $BEPO?(B (registration) ==

If you enter $B<-=qEPO?(B mode by mistake, you can abort it by hitting $B!X(BC-g$B!Y(B 
or by registering an empty string.

!! Type $B!X(BKanakanjihenken$B""!Y(B in $B"#(B mode.

$B<-=q(B does not contain $B!Z$+$J$+$s$8$X$s$1$s![(B, so you get $B!Z$+$J$+$s$8$X$s(B
$B$1$s![(B in the echo area.

!! Veriry that the cursor is in the echo area, and hit <return>.

The $B!Z(B*$BEz(B*$B![(B window now becomes $B!Z"&$+$J$+$s$8$X$s$1$s![(B.

!! Verify that the cursor is in the $B!Z(B*$BEz(B*$B![(B window.  Hit <delete> key
!! twice, and then type $B!X(Bkan$B""(B.$B!Y(B.

>> $B2>L>4A;zJQ49!#(B
----
We assume that $B<-=q(B contains the line:

	$B$4(B /$B8^(B/$B8_(B/$B8`(B/$B8a(B/$B8b(B/$B8c(B/$B8d(B/$B8e(B/$B8f(B/$B8g(B/$B8h(B/$B8i(B/$B8j(B/$B8k(B/$B8l(B/$B8m(B/$B8n(B/$B8o(B/

You can enter $B!Z8k![(B as follows.

!! Type $B!X(BGo$B""""""""""""!Y(B in $B"#(B mode. (You have to hit <space> 6 times.)

You now see the line:

	A:$B8i(B  S:$B8j(B  D:$B8k(B  F:$B8l(B  J:$B8m(B  K:$B8n(B  L:$B8o(B  [$B;D$j(B 0]

in the echo area.  As we have just seen, after the fifth candidate, 7 of
them are displayed together in the echo area.  To select $B!Z8k![(B, type
small $B!X(Bd$B!Y(B.

>> $B8k(B
----
!! Type $B!Z(BGo$B""![(B in $B"#(B mode.

This time $B!Z8k![(B is displayed as the first candidate.  This is because we
have kekutei-ed by that character.  But we want $B!Z8a![(B this time.

!! Type $B!X""""""""""""!Y(B. (6 <space>'s.)

We entered $B<-=qEPO?(B mode as we ran out of kouhos.  Actually $B!Z8a![(B was
among the candidated but we had overrun.

!! Hit <return> key and type $B!X(Bx$B!Y(B.

<return> key registers an empty string (which actually does no real
registration) and we return to the state just before $B<-=qEPO?(B. In $B"'(B mode,
we can return to previous candiate(s) by the $B!X(Bx$B!Y(B key.  You should now be
able to select $B!Z8a![(B from the candidates in the minibuffer.

>> $B8a(B
----
We try $B!Z8a8e![(B this time.

!! Type $B!X(BGogo$B""!Y(Bin $B"#(B mode.

As there is no $B!Z$4$4![(B in the $B<-=q(B buffer, we enter $B<-=qEPO?(B mode.

!! Verify that the minibuffer prompt is $B!Z$4$4![(B, and type $B!X(BGo$B""!Y(B.

Minibuffer becomes $B!Z$4$4(B $B"'8a![(B.

!! Then, type $B!X(BGo$B""""""""""(Bj$B!Y(B.

We have $B!Z$4$4(B $B8a8e![(B in the minibuffer.

!! Hit <return> key.

>> $B8a8e(B
----
We now have the entry $B!Z$4$4(B /$B8a8e(B/$B![(B in the  $B<-=q(B buffer.  So the
following problem should be easy.

>> $B8a8e$N8k!#(B
----
== Recursive $B<-=qEPO?(B ==

!! Type $B!X(BSaikiteki$B""!Y(B in $B"#(B mode.
!! Verify the prompt $B!Z$5$$$-$F$-![(B in the echo area, and type $B!X(BSaiki$B""!Y(B.

We don't have $B!Z$5$$$-![(B in the minibuffer either, so we recursively
(i.e., $B:F5"E*$K(B) enter $B<-=qEPO?(B mode.

!! Construct $B!Z:F5"![(B in the minibuffer.  (Hint: $B:F(B($B$5$$(B), $B5"(B($B$-(B))
!! Hit <return> key after you have $B!Z$5$$$-(B $B:F5"![(B in the minibuffer.

Now we can continue the registration of $B!Z$5$$$-$F$-![(B.

!! You should have $B!Z$5$$$-$F$-(B $B:F5"![(B in the minibuffer. Construct $B!ZE*![(B.

Hint: $B<-=q(B($B$8$7$g(B), $BEPO?(B($B$H$&$m$/(B), $B3X=,(B($B$,$/$7$e$&(B)

>> $B:F5"E*<-=qEPO?$r3X=,$7$^$7$?!#(B
----
== How to enter words with okurigana ==

Words like $B!VF0$/!W(B have okurigana parts, and SKK $B<-=q(B have entries like:

	$B$&$4(Bk /$BF0(B/

for these words.  We make use of these entries to enter words with
okuriganas.

!! in $B"#(B mode, type $B!X(BUgoKounimoUgoKenakatta.$B!Y(B.

Some nouns become verbs by affixing them with $B!V$9$k!W(B, but for such verbs
it is recommened to enter the original noun and the okurigana part
separately.

>> $BF0$3$&$K$bF0$1$J$+$C$?!#(B
----
;; (save-excursion (save-match-data (set-buffer " *skk-tut-jisyo*") (goto-char skk-okuri-ari-min) (search-forward "\n$B$&$4(Bk /$BF0(B/" skk-okuri-nasi-min) (delete-region (progn (beginning-of-line) (point)) (progn (forward-line 1) (point)))))
== How to register words with okurigana ==

Let's see what happens if the $B<-=q(B does not have $B!V$&$4(Bk$B!W(B as $B8+=P$78l(B.

!! In $B"#(B mode, type$B!X(BUgoKu$B!Y(B.

We enter $B<-=qEPO?(B mode in the minibuffer, and we have the prompt $B!Z$&$4(B*$B$/(B 
$B![(B.  As the $B<-=q(B has the entry $B!V$I$&(B /$BF0(B/$B!W(B, we can make $B!ZF0![(B in the
minibuffer.

Remark: You have only to construct the main part without the okurigana,
in the minibuffer.

!! Hit <return> key after you have $B!ZF0![(B in the minibuffer.

>> $BF0$/(B
----
== How to enter words containing double consonants. ==

We do this like the following:

	$B!X(BTukaTta$B""!Y(B $B"*(B $B!Z;H$C$?![(B
	$B!X(BHosSuru$B""!Y(B $B"*(B $B!ZM_$9$k![(B

where our $B<-=q(B is like:

	$B$D$+(Bt /$B;H(B/
	$B$[$C(Bs /$BM_(B/

Hint: $B?M8}(B($B$8$s$3$&(B), $B0l2/(B($B$$$A$*$/(B), $BC#$7$?(B($B$?$C$7$?(B), $BCN$C$?(B($B$7$C$?(B)

>> $B?M8}$,0l2/$KC#$7$?$3$H$rCN$C$?!#(B
----
== ASCII conversion ==

It is possible to enter $B8+=P$78l(B in ascii characters, and transform them.
This is called $B!V1Q;zJQ49!W(B(ASCII conversion).

!! In $B"#(B mode, type $B!X(B/file$B""!Y(B.

Typing $B!X(B/$B!Y(B in $B"#(B mode lets you enter $B"&(B mode.  Unlike ordinary $B"&(B mode,
however, the ascii characters you type will be entered without any
transoformations.  In this case, our $B<-=q(B does not have the $B8+=P$78l(B 
$B!V(Bfile$B!W(B, we enter $B<-=qEPO?(B mode.

!! Verify that the prompt in the minibuffer is $B!Z(Bfile$B![(B, and type
!! $B!X(Bqfairu$B!Y(B, and then hit <return> key.

>> $B!V%U%!%$%k!W$,EPO?$5$l$^$7$?!#(B
----
== Symbols ==

The following symbols are registered in the $B<-=q(B under the $B8+=P$78l(B 
$B!V$-$4$&!W(B.

      $B!!(B $B!"(B $B!#(B $B!$(B $B!%(B $B!&(B $B!'(B $B!((B $B!)(B $B!*(B $B!+(B $B!,(B $B!-(B $B!.(B $B!/(B $B!0(B $B!1(B $B!2(B $B!3(B $B!4(B $B!5(B $B!6(B $B!7(B
   $B!8(B $B!9(B $B!:(B $B!;(B $B!<(B $B!=(B $B!>(B $B!?(B $B!@(B $B!A(B $B!B(B $B!C(B $B!D(B $B!E(B $B!F(B $B!G(B $B!H(B $B!I(B $B!J(B $B!K(B $B!L(B $B!M(B $B!N(B $B!O(B
   $B!P(B $B!Q(B $B!R(B $B!S(B $B!T(B $B!U(B $B!V(B $B![(B $B!X(B $B!Y(B $B!Z(B $B![(B $B!\(B $B!](B $B!^(B $B!_(B $B!`(B $B!a(B $B!b(B $B!c(B $B!d(B $B!e(B $B!f(B $B!g(B
   $B!h(B $B!i(B $B!j(B $B!k(B $B!l(B $B!m(B $B!n(B $B!o(B $B!p(B $B!q(B $B!r(B $B!s(B $B!t(B $B!u(B $B!v(B $B!w(B $B!x(B $B!y(B $B!z(B $B!{(B $B!|(B $B!}(B $B!~(B
      $B"!(B $B""(B $B"#(B $B"$(B $B"%(B $B"&(B $B"'(B $B"((B $B")(B $B"*(B $B"+(B $B",(B $B"-(B $B".(B $B"/(B $B"0(B $B"1(B $B"2(B $B"3(B $B"4(B $B"5(B $B"6(B $B"7(B

>> $B")(B980
----
== Greek and Russian characters ==

They are registered in the $B<-=q(B as follows.

Greek /$B&!(B/$B&"(B/$B&#(B/$B&$(B/$B&%(B/$B&&(B/$B&'(B/$B&((B/$B&)(B/$B&*(B/$B&+(B/$B&,(B/$B&-(B/$B&.(B/$B&/(B/$B&0(B/$B&1(B/$B&2(B/$B&3(B/$B&4(B/$B&5(B/$B&6(B/$B&7(B/$B&8(B/
greek /$B&A(B/$B&B(B/$B&C(B/$B&D(B/$B&E(B/$B&F(B/$B&G(B/$B&H(B/$B&I(B/$B&J(B/$B&K(B/$B&L(B/$B&M(B/$B&N(B/$B&O(B/$B&P(B/$B&Q(B/$B&R(B/$B&S(B/$B&T(B/$B&U(B/$B&V(B/$B&W(B/$B&X(B/
Russia /$B'!(B/$B'"(B/$B'#(B/$B'$(B/$B'%(B/$B'&(B/$B''(B/$B'((B/$B')(B/$B'*(B/$B'+(B/$B',(B/$B'-(B/$B'.(B/$B'/(B/$B'0(B/$B'1(B/$B'2(B/$B'3(B/$B'4(B/$B'5(B/$B'6(B/$B'7(B/$B'8(B/$B'9(B/$B':(B/$B';(B/$B'<(B/$B'=(B/$B'>(B/$B'?(B/$B'@(B/$B'A(B/
russia /$B'Q(B/$B'R(B/$B'S(B/$B'T(B/$B'U(B/$B'V(B/$B'W(B/$B'X(B/$B'Y(B/$B'Z(B/$B'[(B/$B'\(B/$B'](B/$B'^(B/$B'_(B/$B'`(B/$B'a(B/$B'b(B/$B'c(B/$B'd(B/$B'e(B/$B'f(B/$B'g(B/$B'h(B/$B'i(B/$B'j(B/$B'k(B/$B'l(B/$B'm(B/$B'n(B/$B'o(B/$B'p(B/$B'q(B/

We can use $B1Q;zJQ49(B (ASCII conversion) to enter these characters.

>> $B&E&D(B
----
== Input by code ==

You can enter any Kanji characters by their codes (either JIS or EUC).

For example, the JIS code for $B!VU\!W(B is $B!V(B555C$B!W(B.  So you can enter it as
follows.

!! In $B"#(B mode, type $B!X(B\$B!Y(B.

You get the prompt 
$B!Z(B7/8 bits or KUTEN code for japanese-jisx0208 (00nn or CR for Jump Menu):$B![(B.

!! Type $B!X(B555c$B!Y(B and hit <return> key.

Hint: $BElKL(B($B$H$&$[$/(B), $BBg(B($B$@$$(B)

>> $BElKLBgU\(B
----
== Completion of $B8+=P$78l(B ==

Hitting <tab> key in $B"&(B mode invokes completion of $B8+=P$78l(B.  Let $B&R(B be
the string you entered in $B"&(B mode just before you type <tab>.  Then the
first occurrence (if any) of such a $B&S(B in the $B<-=q(B buffer that has $B&R(B as
its initial segement and distinct from $B&R(B will be displayed.

!! In $B"#(B mode, type $B!X(BkonoHikouki$B""(ByorianoHi$B!Y(B.

The display becomes $B!Z$3$NHt9T5!$h$j$"$N"&$R![(B.

!! Now, hit <tab> key.

Completion is performed and the display becomes 
$B!Z$3$NHt9T5!$h$j$"$N"&$R$3$&$-![(B.

>> $B$3$NHt9T5!$h$j$"$NHt9T5!$K$7$h$&!#(B
----
The $B8+=P$78l(B search for the completion is performed starting from the
beginning of the $B<-=q(B buffer.  Therefore, if there are several $B8+=P$78l(B 
in the $B<-=q(B buffer whose initial segment matches $B&R(B, then the first such
string will be displayed.  If that is not what you expected, you can type 
$B!X(B.$B!Y(B(period) to get the second candiate, and you can continue to type $B!X(B.$B!Y(B
to get more candates.  If you have overrun, you can back up by typing 
$B!X(B,$B!Y(B(comma) several times. 

Let us try to enter the sentence $B!V:4F#$H:XF#$G$O:4F#$N$[$&$,$"$j$U$l$?L>(B
$B;z$@!#!W(B.

!! In $B"#(B mode, type $B!X(BSatou$B""(BtoSaitou$B""(BdehaSa$B!Y(B and hit <tab> key.

The display becomes$B!Z:4F#$H:XF#$G$O"&$5$$$H$&![(B.

!! Type $B!X(B..,$B!Y(B.

>> $B:4F#$H:XF#$G$O:4F#$N$[$&$,$"$j$U$l$?L>;z$@!#(B
----
When SKK is invoked for the first time in the current Mule session, it
reads in your personal $B<-=q(B file into the $B<-=q(B buffers.  (We use two
$B<-=q(B buffers, one for words with okurigana, and for those without
okurigana.)  A $B<-=q(B buffer will be updated each time you make a kakutei.
When you exit from Mule, SKK saves the content of the $B<-=q(B buffer into
your personal $B<-=q(B file.  So, normally, you don't have to worry about
savings of the $B<-=q(B buffers.  When SKK updates your personal $B<-=q(B file
(.skk-jisyo) it copies the content of the current .skk-jisyo to
.skk-jisyo.BAK.  This back up file may be useful if your .skk-jisyo is
damaged accidentally.

SKK has a lot more features we couldn't explain in this short tutorial.
For further information about SKK refer to the SKK manual.

Here is your last problem.  Hint: $B=*N;(B($B$7$e$&$j$g$&(B)

>> $B$3$l$G(B SKK $B%A%e!<%H%j%"%k$r=*N;$7$^$9!#(B
----
;; (skktut-end-tutorial)

;; Local Variables:
;; mode:skk-auto-fill
;; fill-column:74
;; End:
