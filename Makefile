# Makefile: makefile for SKK.
#
# Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
# Version: $Id: Makefile,v 1.1 1999/09/15 07:20:12 minakaji Exp $
# Last Modified: $Date: 1999/09/15 07:20:12 $

VERSION = 10.52

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l SKK-MK

PREFIX = NONE
LISPDIR = NONE
PACKAGEDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE


elc:
	$(EMACS) $(FLAGS) -f compile-skk

install:
	$(EMACS) $(FLAGS) -f install-skk $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)


install-package:
	$(XEMACS) $(FLAGS) -f install-skk-package $(PACKAGEDIR)


what-where:
	$(EMACS) $(FLAGS) -f what-where-skk $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)


clean:
	-$(RM) *.elc *~


tar:
	-$(RM) *.elc `find . -name '*~'`
	cd .. ; $(TAR) cvzpf skk$(VERSION).tar.gz \
	--exclude-from=./skk-$(VERSION)/skk.ex skk-$(VERSION)

# end of Makefile.
