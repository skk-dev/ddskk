#
# Makefile for rdbms
# 
SHELL = /bin/sh
#NKF = nkf -e
GAWK = jgawk
SQLSHELL = psql
CREATEDB = createdb
PJISYO = ~/.skk-jisyo
#KAKUTEI_JISYO = ~/.skk-jisyo.kakutei
#INITIAL_JISYO = ~/.skk-jisyo.initial
LJISYO = /usr/local/share/skk/SKK-JISYO.L
2JISYO = /usr/local/share/skk/SKK-JISYO.JIS2
KANJIDIC = kanjidic/kanjidic
PUBDIC = pubdic+/pubdic.p

# do NOT edit the following.

all: maintable busyu kcode stroke hinsi
.PHONY: all

maintable:
	$(GAWK) -f ./awk/dicconv.awk $(PJISYO) | sort | uniq - > ./tmp/private.txt
	$(GAWK) -f ./awk/dicconv.awk $(LJISYO) | sort | uniq - > ./tmp/large.txt
# $(NKF) $(2JISYO) | $(GAWK) -f ./awk/dicconv.awk - | sort | uniq - > ./tmp/jis2.txt
#-$(NKF) $(KAKUTEI_JISYO) |  $(GAWK) -f ./awk/dicconv.awk - | sort | uniq - > ./tmp/kakutei.txt
#-$(NKF) INITIAL_JISYO |  $(GAWK) -f ./awk/dicconv.awk - | sort | uniq - > ./tmp/initial.txt
	-$(CREATEDB) skk
	$(SQLSHELL) skk -e < ./sql/createtbl.sql

busyu:
	$(GAWK) -f ./awk/busyu_base.awk $(KANJIDIC) | sort -n > ./tmp/busyu_base.txt
	$(GAWK) -f ./awk/busyu_data.awk $(KANJIDIC) | sort -n > ./tmp/busyu_data.txt
	-$(CREATEDB) skk
	$(SQLSHELL) skk -e < ./sql/busyu.sql

kcode:
	$(GAWK) -f ./awk/kcode.awk $(KANJIDIC) > ./tmp/kcode.txt
	-$(CREATEDB) skk
	$(SQLSHELL) skk -e < ./sql/kcode.sql

stroke:
	$(GAWK) -f ./awk/stroke.awk $(KANJIDIC) | sort -n > ./tmp/stroke.txt
	-$(CREATEDB) skk
	$(SQLSHELL) skk -e < ./sql/stroke.sql

hinsi:
	$(GAWK) 'BEGIN {OFS = "\t";}{split($$3, entry, "/"); print $$1, $$2, entry[1]}' $(PUBDIC) > ./tmp/temp.txt
#$(GAWK) '{print $$3}' ./tmp/temp.txt | sort | uniq -d | \
#	$(GAWK) '$$1 !~/^$$/{printf("%d\t%s\n", NR - 2, $$0)}' - > ./tmp/hinsi_base.txt
	$(GAWK) -f ./awk/hinsi_data.awk ./tmp/temp.txt > ./tmp/hinsi_data.txt
	-$(CREATEDB) skk
	$(SQLSHELL) skk -e < ./sql/hinsi.sql

.PHONY: clean
clean:
	-rm -f *~ ./tmp/private.txt ./tmp/large.txt ./tmp/jis2.txt ./tmp/busyu_base.txt \
        ./tmp/busyu_data.txt ./tmp/kcode.txt ./tmp/stroke.txt ./tmp/kakutei.txt \
        ./tmp/initial.txt ./tmp/temp*.txt /tmp/hinsi_data.txt
