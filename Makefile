# Makefile: makefile for SKK.
#
# Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
# Version: $Id: Makefile,v 1.25 2000/09/10 10:04:20 minakaji Exp $
# Last Modified: $Date: 2000/09/10 10:04:20 $

VERSION = 10.61

TAR	= /usr/local/bin/tar
RM	= /bin/rm -f
CP	= /bin/cp -p
EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l SKK-MK
DATE	= date

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
         `find . -name '*~'` `find . -name '.*~'`

tar: clean
	cd .. ; -$(RM) skk-$(VERSION) skk-snapshot ;\
	ln -sf main skk-$(VERSION) ;\
	$(TAR) cvzpf skk-$(VERSION).tar.gz --exclude-from=skk-$(VERSION)/skk.ex --dereference skk-$(VERSION) ;\
	$(RM) skk-$(VERSION)

snapshot: clean
	cd .. ; $(RM) skk-$(VERSION) skk-snapshot skk-`$(DATE) '+%Y%m%d'`.tar.gz ;\
	ln -sf main skk-`$(DATE) '+%Y%m%d'` ;\
	$(TAR) cvzpf skk-`$(DATE) '+%Y%m%d'`.tar.gz --exclude-from=skk-`$(DATE) '+%Y%m%d'`/skk.ex --dereference skk-`$(DATE) '+%Y%m%d'` ;\
	$(RM) skk-`$(DATE) '+%Y%m%d'`
# end of Makefile.
