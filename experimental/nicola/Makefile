#
# Makefile for NICOLA-DDSKK.
#

PACKAGE = nicola-ddskk

TAR	= /bin/tar
RM	= /bin/rm -f
CP	= /bin/cp -p
GZIP    = /bin/gzip -9

EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l NICOLA-DDSKK-MK

PREFIX = NONE
LISPDIR = NONE
PACKAGEDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE

GOMI	= *.elc auto-autoloads.el custom-load.el nicola-ddskk-autoloads.el

DDSKK_VERSION=11.3
VERSION	= $(DDSKK_VERSION)-`date -u '+%Y%m%d'`

all: elc

elc: clean
	$(EMACS) $(FLAGS) -f compile-nicola-ddskk $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)

install: elc
	$(EMACS) $(FLAGS) -f install-nicola-ddskk $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)


package: clean
	$(XEMACS) $(FLAGS) -f compile-nicola-ddskk-package $(PACKAGEDIR)

install-package: package
	$(XEMACS) $(FLAGS) -f install-nicola-ddskk-package $(PACKAGEDIR)

clean:
	-$(RM) $(GOMI)

tar: clean
	cd .. ; $(RM) $(PACKAGE)-$(VERSION) $(PACKAGE)-$(VERSION).tar.gz ;\
	ln -sf nicola $(PACKAGE)-$(VERSION) ;\
	$(TAR) cvpf $(PACKAGE)-$(VERSION).tar --exclude=CVS -h $(PACKAGE)-$(VERSION) ;\
	$(GZIP) $(PACKAGE)-$(VERSION).tar
