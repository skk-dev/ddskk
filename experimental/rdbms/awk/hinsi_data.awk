BEGIN{
  OFS = "\t";
}
{
if ($3 != "") {
  printf("%s\t%s\t", $1, $2);
  if ($3 == "カ行(行く)") print "0";
  else if ($3 == "カ行五段") print "1";
  else if ($3 == "ガ行五段") print "2";
  else if ($3 == "サ行(する)") print "3";
  else if ($3 == "サ行(する)&名詞") print "3", "44";
  else if ($3 == "サ行(する)&名詞化接尾語") print "3", "36";
  # "3" が 3 つになったから "4","5" は欠番。直すの面倒だもん。
  else if ($3 == "サ行五段") print "6";
  else if ($3 == "ザ行(ずる)") print "7";
  else if ($3 == "タ行五段") print "8";
  else if ($3 == "ナ行五段") print "9";
  else if ($3 == "バ行五段") print "10";
  else if ($3 == "マ行五段") print "11";
  else if ($3 == "ラ行(下さい)") print "12";
  else if ($3 == "ラ行五段") print "13";
  else if ($3 == "ワ行五段") print "14";
  else if ($3 == "一段") print "15";
  else if ($3 == "一段&名詞") print "15", "44";
  # "16" は欠番。
  else if ($3 == "一動幹") print "17";
  else if ($3 == "記号") print "18";
  else if ($3 == "形容詞") print "19";
  else if (match($3, /形容詞化接尾/) != 0) print "20";
  else if ($3 == "形容動詞") print "21";
  else if (($3 == "形容動詞&名詞") || ($3 == "形容動詞,名詞")) print "21", "44";
  # sort して uniq すると何故かこいつが出てこない。
  else if ($3 == "形容詞化接尾動詞") print "22";
  else if ($3 == "形容動詞(たる)") print "23";
  else if ($3 == "形容動詞化接尾語") print "24";
  else if ($3 == "固有名詞") print "25";
  else if ($3 == "助数詞") print "26";
  else if ($3 == "人名") print "27";
  else if ($3 == "人名&地名") print "27", "42";
  # "28" は欠番。
  else if ($3 == "数詞") print "29";
  else if ($3 == "接続詞,感動詞") print "30";
  # "接続詞" と "接続語" は違うのか？
  else if ($3 == "接頭語") print "31";
  else if ($3 == "接頭語(各)") print "32";
  # とりあえず  
  else if (match($3, /接頭語\(お/) != 0) print "31";
  else if ($3 == "接頭助数詞") print "33";
  else if ($3 == "接頭数詞") print "34";
  else if ($3 == "接頭地名") print "35";
  else if ($3 == "接尾語") print "36";
  else if ($3 == "接尾助数詞") print "37";
  else if ($3 == "接尾人名") print "38";
  else if ($3 == "接尾地名") print "39";
  else if ($3 == "接尾動詞") print "40";
  else if ($3 == "単漢字") print "41";
  else if ($3 == "地名") print "42";
  else if ($3 == "副詞") print "43";
  else if ($3 == "名詞") print "44";
  else if ($3 == "来(き)") print "45";
  else if ($3 == "来(く)") print "46";
  else if ($3 == "来(こ)") print "47";
  else if ($3 == "連体詞") print "48";
  # sort して uniq すると何故かこいつら 3 つが出てこない。
  else if ($3 == "為(し)") print "49";
  else if ($3 == "為(す)") print "50";
  else if ($3 == "為(せ)") print "51";
}
}
