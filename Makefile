# Makefile: makefile for SKK.
#
# Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
# Version: $Id: Makefile,v 1.21 2000/07/07 22:25:14 minakaji Exp $
# Last Modified: $Date: 2000/07/07 22:25:14 $

VERSION = 10.61

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p
EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l SKK-MK
DATE	= date

elc:
	$(EMACS) $(FLAGS) -f SKK-MK-compile

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
	-$(RM) ../skk*
	cd .. ; ln -sf main skk-$(VERSION) ; \
	$(TAR) cvzpf skk$(VERSION).tar.gz --exclude-from=./skk-$(VERSION)/skk.ex \
               --dereference  skk-$(VERSION)

snapshot: clean
	-$(RM) ../skk*
	cd .. ; ln -sf main skk-snapshot
	$(TAR) cvzpf ../skk-`$(DATE) '+%Y%m%d'`.tar.gz --exclude-from=skk.ex --dereference ../skk-snapshot

# end of Makefile.
