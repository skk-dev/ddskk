# Makefile: makefile for SKK.
#
# Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
# Version: $Id: Makefile,v 1.7 1999/09/16 21:50:24 minakaji Exp $
# Last Modified: $Date: 1999/09/16 21:50:24 $

VERSION = 10.52a

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p
CHMOD   = /bin/chmod

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
	-$(RM) *.elc ./doc/skk.info* ../skk-$(VERSION).tar.gz \
               `find . -name '*~'` `find . -name '*~'`
	$(CHMOD) a+w Makefile make.bat SKK-CFG doc/skk.texi
	cd .. ; $(TAR) cvzpf skk$(VERSION).tar.gz --exclude-from=./skk-$(VERSION)/skk.ex \
                             skk-$(VERSION)

checkin:
	$(CHMOD) a-w Makefile make.bat SKK-CFG doc/skk.texi

# end of Makefile.
