# Makefile: makefile for SKK.
#
# Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
# Version: $Id: Makefile,v 1.16 2000/03/11 02:14:53 minakaji Exp $
# Last Modified: $Date: 2000/03/11 02:14:53 $

VERSION = 10.59

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p
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
	-$(RM) *.elc ./doc/skk.info* `find . -name '*~'` `find . -name '.*~'` \
         ../skk*

tar: clean
	cd .. ; ln -sf main skk-$(VERSION) ; \
	$(TAR) cvzpf skk$(VERSION).tar.gz --exclude-from=./skk-$(VERSION)/skk.ex \
               --dereference  skk-$(VERSION)

# end of Makefile.
