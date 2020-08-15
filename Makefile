# Makefile: makefile for SKK.
#
# Maintainer: SKK Development Team <skk@ring.gr.jp>


VERSION = 17.1

BZIP2     = bzip2 -9
CD	  = cd
CP        = cp --force
DATE	  = date
EMACS	  = emacs
ETAGS	  = etags
FIND	  = find . -maxdepth 1 -name
FLAGS     = --batch --no-init-file --quick --load SKK-MK
GIT       = git
GZIP      = gzip -9
MD5	  = md5
RM	  = /bin/rm -f
SNAPBASE  = ddskk-`$(DATE) '+%Y%m%d'`
TAR	  = gtar
TEE	  = tee
PWD       = pwd
CURL      = curl
XARGS	  = xargs --max-args=1 --verbose
SKK_DEFAULT_JISYO =
set_jisyo =

TEST_DEP_1=ert
TEST_DEP_1_STABLE_URL=http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el?h=emacs-24.3

elc:
	$(EMACS) $(FLAGS) --funcall SKK-MK-generate-autoloads-el
	$(FIND) '*.el' | $(XARGS) $(EMACS) --batch --no-init-file --quick --directory ./ --funcall batch-byte-compile

.PHONY: test downloads
test:
	$(EMACS) --batch --quick --directory ./ --directory test --load test/all-tests.el --funcall ert-run-tests-batch-and-exit
	$(RM) skk-bayesian.el* skk-def.el* skk-mkmgk.el* skk-tutcdef.el* skk-tutcode.el* skk-autoloads.el*
	$(CP) bayesian/skk-bayesian.el .
	$(CP) tut-code/*.el .
	$(EMACS) $(FLAGS) --funcall SKK-MK-generate-autoloads-el
	$(EMACS) --batch --quick --directory ./ --funcall batch-byte-compile skk-bayesian.el
	$(EMACS) --batch --quick --directory ./ --funcall batch-byte-compile skk-def.el
	$(EMACS) --batch --quick --directory ./ --funcall batch-byte-compile skk-mkmgk.el
	$(EMACS) --batch --quick --directory ./ --funcall batch-byte-compile skk-tutcdef.el
	$(EMACS) --batch --quick --directory ./ --funcall batch-byte-compile skk-tutcode.el
	$(RM) skk-bayesian.el* skk-def.el* skk-mkmgk.el* skk-tutcdef.el* skk-tutcode.el*
	$(CD) nicola;make;make clean

checkdoc:
	$(RM) checkdoc.log
	$(FIND) '*.el' | $(XARGS) $(EMACS) --batch --quick --load maint/checkdoc-batch.el --funcall checkdoc-batch-commandline 2>&1 | $(TEE) checkdoc.log

downloads :
	$(CURL) '$(TEST_DEP_1_STABLE_URL)' > $(TEST_DEP_1).el

info:
	$(EMACS) $(FLAGS) --funcall SKK-MK-compile-info

install:
	$(EMACS) $(FLAGS) --funcall SKK-MK-install

install-elc:
	$(EMACS) $(FLAGS) --funcall SKK-MK-install-elc

install-info:
	$(EMACS) $(FLAGS) --funcall SKK-MK-install-info

what-where:
	$(EMACS) $(FLAGS) --funcall SKK-MK-what-where

uninstall:
	$(EMACS) $(FLAGS) --funcall SKK-MK-uninstall

get:
	$(EMACS) $(FLAGS) --funcall SKK-MK-generate-autoloads-el
	$(EMACS) --batch --no-init-file --quick --directory ./ --load tar-util.el --load skk-develop.el --eval='(skk-get "./dic")'

TAGS:
	$(ETAGS) `find . -name '*.el'`

clean:
	-$(RM) leim-list.el skk-autoloads.el skk-setup.el *.elc experimental/*.elc \
	auto-autoloads.el custom-load.el ert.el checkdoc.log\
	./doc/skk.info* ./doc/skk.html* ./doc/skk.pdf \
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
