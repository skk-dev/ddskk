# Makefile: makefile for SKK.
#
# Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
# Version: $Id: Makefile,v 1.3 1999/09/15 10:46:07 minakaji Exp $
# Last Modified: $Date: 1999/09/15 10:46:07 $

VERSION = 10.52

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l SKK-MK

# When some value other than `NONE' is defined, overwrite variables
# defined in SKK-MK and SKK-CFG.  You can specify in SKK-CFG, too.
PREFIX = NONE
LISPDIR = NONE
PACKAGEDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE

elc:
	$(EMACS) $(FLAGS) -f SKK-MK-compile

install:
	$(EMACS) $(FLAGS) -f SKK-MK-install $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)


install-package:
	$(XEMACS) $(FLAGS) -f SKK-MK-install-package $(PACKAGEDIR)


what-where:
	$(EMACS) $(FLAGS) -f SKK-MK-what-where $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)

what-where-package:
	$(EMACS) $(FLAGS) -f SKK-MK-what-where-package \
                $(PACKAGEDIR)

clean:
	-$(RM) *.elc ./doc/skk.info* `find . -name '*~'` 

tar:
	-$(RM) *.elc `find . -name '*~'` `find . -name '*~'`
	cd .. ; $(TAR) cvzpf skk$(VERSION).tar.gz \
	--exclude-from=./skk-$(VERSION)/skk.ex skk-$(VERSION)

# end of Makefile.
