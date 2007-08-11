# Makefile: makefile for SKK.
#
# Maintainer: SKK Development Team <skk@ring.gr.jp>
# Version: $Id: Makefile,v 1.62 2007/08/11 11:05:30 skk-cvs Exp $
# Last Modified: $Date: 2007/08/11 11:05:30 $


VERSION = 13.0.90

BZIP2     = bzip2 -9
DATE	  = date
EMACS	  = emacs
ETAGS	  = etags
FLAGS     = -batch -q -no-site-file -l SKK-MK
GZIP      = gzip -9
MD5	  = md5
RM	  = /bin/rm -f
SNAPBASE  = ddskk-`$(DATE) '+%Y%m%d'`
TAR	  = tar
XEMACS	  = xemacs
RUBY      = ruby
SKK_DEFAULT_JISYO =
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

skk-dic:
	$(RM) skk-dic.el
	$(EMACS) $(FLAGS) -f SKK-MK-skk-dic

rb-skk-dic:
	$(RM) skk-dic.el
	$(RUBY) etc/skk-dic.rb ../dic/SKK-JISYO.S

TAGS:
	$(ETAGS) `find . -name '*.el'`
clean:
	-$(RM) leim-list.el skk-autoloads.el skk-setup.el *.elc experimental/*.elc \
	auto-autoloads.el custom-load.el \
	./doc/skk.info* `find . -name '*~'` `find . -name '.*~'` `find . -name '.#*'`

tar: clean
	cp -f skk-dic.el.in skk-dic.el
	cd .. ;\
	$(RM) ddskk-11.{1,2,3} ddskk-$(VERSION) ddskk-snapshot ddskk$(VERSION).tar.gz ddskk$(VERSION).tar.bz2 ddskk-$(VERSION).tar.gz ddskk-$(VERSION).tar.bz2 ;\
	$(RM) ddskk-$(VERSION) ;\
	ln -sf main ddskk-$(VERSION) ;\
	$(TAR) -cvpf ddskk-$(VERSION).tar --exclude-from=ddskk-$(VERSION)/skk.ex --dereference ddskk-$(VERSION) ;\
	$(BZIP2) -cf ddskk-$(VERSION).tar > ddskk-$(VERSION).tar.bz2 ;\
	$(GZIP) -cf ddskk-$(VERSION).tar > ddskk-$(VERSION).tar.gz ;\
	$(RM) ddskk-$(VERSION).tar ;\
	$(RM) ddskk-$(VERSION) ;\
	$(MD5) ddskk-$(VERSION).tar.bz2 >ddskk-$(VERSION).tar.bz2.md5 ;\
	$(MD5) ddskk-$(VERSION).tar.gz >ddskk-$(VERSION).tar.gz.md5
	$(RM) skk-dic.el

snapshot: clean
	cd .. ;\
	$(RM) ddskk-11.{1,2,3} ddskk-$(VERSION) ddskk-snapshot $(SNAPBASE).tar.gz $(SNAPBASE).tar.bz2 ;\
	$(RM) $(SNAPBASE) ;\
	ln -sf main $(SNAPBASE) ;\
	$(TAR) -cvpf $(SNAPBASE).tar --exclude-from=$(SNAPBASE)/skk.ex --dereference $(SNAPBASE);\
	$(BZIP2) -cf $(SNAPBASE).tar > $(SNAPBASE).tar.bz2 ;\
	$(GZIP) -cf $(SNAPBASE).tar > $(SNAPBASE).tar.gz ;\
	$(RM) $(SNAPBASE).tar ;\
	$(RM) $(SNAPBASE) ;\
	$(MD5) $(SNAPBASE).tar.bz2 >$(SNAPBASE).tar.bz2.md5 ;\
	$(MD5) $(SNAPBASE).tar.gz >$(SNAPBASE).tar.gz.md5
	$(RM) skk-dic.el

# end of Makefile.
