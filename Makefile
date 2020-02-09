# Makefile: makefile for SKK.
#
# Maintainer: SKK Development Team <skk@ring.gr.jp>


VERSION = 16.3.50

BZIP2     = bzip2 -9
DATE	  = date
EMACS	  = emacs
ETAGS	  = etags
FLAGS     = -batch -q -no-site-file -l SKK-MK
GIT       = git
GZIP      = gzip -9
MD5	  = md5
RM	  = /bin/rm -f
TEXI2PDF  = /usr/bin/texi2pdf
MAKEINFO  = /usr/bin/makeinfo
DBTOEPUB  = /usr/bin/dbtoepub
KINDLEGEN = $(HOME)/bin/kindlegen
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
	./doc/skk.info* ./doc/skk.html* ./doc/skk.pdf \
	./doc/skk.xml ./doc/skk.epub ./doc/skk.mobi \
	./doc/*.aux ./doc/*.cp* ./doc/*.fn* ./doc/*.ky* ./doc/*.log ./doc/*.toc ./doc/*.vr* \
	`find . -name '*~'` `find . -name '.*~'` `find . -name '.#*'`

release: clean
	$(RM) ../ddskk-$(VERSION).tar.gz ../ddskk-$(VERSION).tar.bz2 ;\
	$(GIT) archive --format=tar.gz --prefix=ddskk-$(VERSION)/ HEAD > ../ddskk-$(VERSION).tar.gz ;\
	$(GIT) archive --format=tar --prefix=ddskk-$(VERSION)/ HEAD | $(BZIP2) -c > ../ddskk-$(VERSION).tar.bz2 ;\
	$(MD5) ../ddskk-$(VERSION).tar.bz2 > ../ddskk-$(VERSION).tar.bz2.md5 ;\
	$(MD5) ../ddskk-$(VERSION).tar.gz > ../ddskk-$(VERSION).tar.gz.md5

tar: clean
	$(RM) ../ddskk-$(VERSION) ../ddskk-snapshot $(SNAPBASE).tar.gz ../$(SNAPBASE).tar.bz2 ;\
	$(RM) ../$(SNAPBASE) ;\
	ln -sf `$(PWD)` ../$(SNAPBASE) ;\
	cd .. ;\
	$(TAR) -cvpf $(SNAPBASE).tar --exclude-from=$(SNAPBASE)/skk.ex --dereference $(SNAPBASE);\
	$(GZIP) -cf $(SNAPBASE).tar > $(SNAPBASE).tar.gz ;\
	$(RM) $(SNAPBASE).tar ;\
	$(RM) $(SNAPBASE) ;\
	$(MD5) $(SNAPBASE).tar.gz >$(SNAPBASE).tar.gz.md5

# end of Makefile.
