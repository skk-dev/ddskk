# Makefile: makefile for SKK.
#
# Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
# Version: $Id: Makefile,v 1.12 1999/10/03 15:19:41 minakaji Exp $
# Last Modified: $Date: 1999/10/03 15:19:41 $

VERSION = 10.55

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p
CHMOD   = /bin/chmod

EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l SKK-MK

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
