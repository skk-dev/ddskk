# Makefile: makefile for SKK.
#
# Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
# Version: $Id: Makefile,v 1.26 2000/09/11 20:09:26 minakaji Exp $
# Last Modified: $Date: 2000/09/11 20:09:26 $

VERSION = 10.61

BZIP2     = bzip2 -9
CP	  = /bin/cp -p
DATE	  = date
EMACS	  = emacs
FLAGS     = -batch -q -no-site-file -l SKK-MK
GZIP      = gzip -9
MD5	  = md5
RM	  = /bin/rm -f
SNAPBASE  = ddskk-`$(DATE) '+%Y%m%d'`
TAR	  = tar
XEMACS	  = xemacs

elc:
	$(EMACS) $(FLAGS) -f SKK-MK-compile

info:
	$(EMACS) $(FLAGS) -f SKK-MK-compile-info

install:
	$(EMACS) $(FLAGS) -f SKK-MK-install 

install-package:
	$(XEMACS) $(FLAGS) -f SKK-MK-install-package 


what-where:
	$(EMACS) $(FLAGS) -f SKK-MK-what-where

what-where-package:
	$(EMACS) $(FLAGS) -f SKK-MK-what-where-package

clean:
	-$(RM) skk-autoloads.el *.elc ./doc/skk.info* experimental/*.elc \
	 `find . -name '*~'` `find . -name '.*~'` `find . -name '.#*'`

tar: clean
	cd .. ;\
	$(RM) skk-10.{60,61,62,63} skk-$(VERSION) skk-snapshot skk-$(VERSION).tar.gz skk-$(VERSION).tar.bz2 ;\
	$(RM) skk-$(VERSION) ;\
	ln -sf main skk-$(VERSION) ;\
	$(TAR) cvpf skk-$(VERSION).tar --exclude-from=skk-$(VERSION)/skk.ex --dereference skk-$(VERSION) ;\
	$(BZIP2) -cf skk-$(VERSION).tar > skk-$(VERSION).tar.bz2 ;\
	$(GZIP) -cf skk-$(VERSION).tar > skk-$(VERSION).tar.gz ;\
	$(RM) skk-$(VERSION).tar ;\
	$(RM) skk-$(VERSION) ;\
	$(MD5) skk-$(VERSION).tar.bz2 >skk-$(VERSION).tar.bz2.md5 ;\
	$(MD5) skk-$(VERSION).tar.gz >skk-$(VERSION).tar.gz.md5

snapshot: clean
	cd .. ;\
	$(RM) skk-11.{60,61,62,63} skk-$(VERSION) skk-snapshot $(SNAPBASE).tar.gz $(SNAPBASE).tar.bz2 ;\
	$(RM) $(SNAPBASE) ;\
	ln -sf main $(SNAPBASE) ;\
	$(TAR) cvpf $(SNAPBASE).tar --exclude-from=$(SNAPBASE)/skk.ex --dereference $(SNAPBASE);\
	$(BZIP2) -cf $(SNAPBASE).tar > $(SNAPBASE).tar.bz2 ;\
	$(GZIP) -cf $(SNAPBASE).tar > $(SNAPBASE).tar.gz ;\
	$(RM) $(SNAPBASE).tar ;\
	$(RM) $(SNAPBASE) ;\
	$(MD5) $(SNAPBASE).tar.bz2 >$(SNAPBASE).tar.bz2.md5 ;\
	$(MD5) $(SNAPBASE).tar.gz >$(SNAPBASE).tar.gz.md5
# end of Makefile.
