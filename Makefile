# Makefile: makefile for SKK.
#
# Maintainer: SKK Development Team <skk@ring.gr.jp>
# Version: $Id: Makefile,v 1.36 2000/11/12 10:47:20 czkmt Exp $
# Last Modified: $Date: 2000/11/12 10:47:20 $


VERSION = 11.3

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
set_jisyo =

elc:
	$(EMACS) $(FLAGS) -f SKK-MK-compile

package:
	$(XEMACS) $(FLAGS) -f SKK-MK-compile-package

info:
	$(EMACS) $(FLAGS) -f SKK-MK-compile-info

install:
	$(EMACS) $(FLAGS) -f SKK-MK-install 

install-elc:
	$(EMACS) $(FLAGS) -f SKK-MK-install-elc

install-info:
	$(EMACS) $(FLAGS) -f SKK-MK-install-info

install-package:
	$(XEMACS) $(FLAGS) -f SKK-MK-install-package 


what-where:
	$(EMACS) $(FLAGS) -f SKK-MK-what-where

what-where-package:
	$(XEMACS) $(FLAGS) -f SKK-MK-what-where-package

clean:
	-$(RM) skk-autoloads.el skk-setup.el leim-list.el *.elc experimental/*.elc \
	auto-autoloads.el custom-load.el \
	experimental/skk-isearch.el ./doc/skk.info* `find . -name '*~'` `find . -name '.*~'` `find . -name '.#*'`

tar: clean
	cd .. ;\
	$(RM) ddskk-11.{1,2,3} ddskk-$(VERSION) ddskk-snapshot ddskk-$(VERSION).tar.gz ddskk-$(VERSION).tar.bz2 ;\
	$(RM) ddskk-$(VERSION) ;\
	ln -sf main ddskk-$(VERSION) ;\
	$(TAR) cvpf ddskk-$(VERSION).tar --exclude-from=ddskk-$(VERSION)/skk.ex --dereference ddskk-$(VERSION) ;\
	$(BZIP2) -cf ddskk-$(VERSION).tar > ddskk-$(VERSION).tar.bz2 ;\
	$(GZIP) -cf ddskk-$(VERSION).tar > ddskk-$(VERSION).tar.gz ;\
	$(RM) ddskk-$(VERSION).tar ;\
	$(RM) ddskk-$(VERSION) ;\
	$(MD5) ddskk-$(VERSION).tar.bz2 >ddskk-$(VERSION).tar.bz2.md5 ;\
	$(MD5) ddskk-$(VERSION).tar.gz >ddskk-$(VERSION).tar.gz.md5

snapshot: clean
	cd .. ;\
	$(RM) ddskk-11.{1,2,3} ddskk-$(VERSION) ddskk-snapshot $(SNAPBASE).tar.gz $(SNAPBASE).tar.bz2 ;\
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
