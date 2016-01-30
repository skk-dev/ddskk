# Makefile: makefile for SKK.
#
# Maintainer: SKK Development Team <skk@ring.gr.jp>


VERSION = 16.1.50

BZIP2     = bzip2 -9
DATE	  = date
EMACS	  = emacs
ETAGS	  = etags
FLAGS     = -batch -q -no-site-file -l SKK-MK
GZIP      = gzip -9
MD5	  = md5
RM	  = /bin/rm -f
SNAPBASE  = ddskk-`$(DATE) '+%Y%m%d'`
TAR	  = gtar
XEMACS	  = xemacs
RUBY      = ruby
PWD       = pwd
CURL      = curl
SKK_DEFAULT_JISYO =
set_jisyo =

TEST_DEP_1=ert
TEST_DEP_1_STABLE_URL=http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el?h=emacs-24.3

elc:
	$(EMACS) $(FLAGS) -f SKK-MK-compile

.PHONY: test downloads
test:
	$(EMACS) -batch -Q -L . -L test -l test/all-tests.el -f ert-run-tests-batch-and-exit

downloads :
	$(CURL) '$(TEST_DEP_1_STABLE_URL)' > $(TEST_DEP_1).el

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

uninstall:
	$(EMACS) $(FLAGS) -f SKK-MK-uninstall

get:
	$(EMACS) -batch -q -no-site-file -l tar-util.el -l skk-develop.el --eval='(skk-get "./dic")'

TAGS:
	$(ETAGS) `find . -name '*.el'`

clean:
	-$(RM) leim-list.el skk-autoloads.el skk-setup.el *.elc experimental/*.elc \
	auto-autoloads.el custom-load.el ert.el \
	./doc/skk.info* `find . -name '*~'` `find . -name '.*~'` `find . -name '.#*'`

tar: clean
	$(RM) ../ddskk-11.{1,2,3} ../ddskk-$(VERSION) ../ddskk-snapshot ../ddskk$(VERSION).tar.gz ../ddskk$(VERSION).tar.bz2 ../ddskk-$(VERSION).tar.gz ../ddskk-$(VERSION).tar.bz2 ;\
	ln -sf `$(PWD)`  ../ddskk-$(VERSION) ;\
	cd .. ;\
	$(TAR) -cvpf ddskk-$(VERSION).tar --exclude-from=ddskk-$(VERSION)/skk.ex --dereference ddskk-$(VERSION) ;\
	$(BZIP2) -cf ddskk-$(VERSION).tar > ddskk-$(VERSION).tar.bz2 ;\
	$(GZIP) -cf ddskk-$(VERSION).tar > ddskk-$(VERSION).tar.gz ;\
	$(RM) ddskk-$(VERSION).tar ;\
	$(RM) ddskk-$(VERSION) ;\
	$(MD5) ddskk-$(VERSION).tar.bz2 >ddskk-$(VERSION).tar.bz2.md5 ;\
	$(MD5) ddskk-$(VERSION).tar.gz >ddskk-$(VERSION).tar.gz.md5

snapshot: clean
	$(RM) ../ddskk-11.{1,2,3} ../ddskk-$(VERSION) ../ddskk-snapshot $(SNAPBASE).tar.gz ../$(SNAPBASE).tar.bz2 ;\
	$(RM) ../$(SNAPBASE) ;\
	ln -sf `$(PWD)` ../$(SNAPBASE) ;\
	cd .. ;\
	$(TAR) -cvpf $(SNAPBASE).tar --exclude-from=$(SNAPBASE)/skk.ex --dereference $(SNAPBASE);\
	$(GZIP) -cf $(SNAPBASE).tar > $(SNAPBASE).tar.gz ;\
	$(RM) $(SNAPBASE).tar ;\
	$(RM) $(SNAPBASE) ;\
	$(MD5) $(SNAPBASE).tar.gz >$(SNAPBASE).tar.gz.md5

# end of Makefile.
