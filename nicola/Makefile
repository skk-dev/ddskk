#
# Makefile for NICOLA-DDSKK.
#

PACKAGE = nicola-ddskk

TAR	= /bin/tar
RM	= /bin/rm -f

EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l NICOLA-DDSKK-MK

PREFIX = NONE
LISPDIR = NONE
PACKAGEDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE

GOMI	= *.elc auto-autoloads.el custom-load.el nicola-ddskk-autoloads.el

all: elc

elc: clean
	$(EMACS) $(FLAGS) -f compile-nicola-ddskk $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)

install-elc: elc
	$(EMACS) $(FLAGS) -f install-nicola-ddskk $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)

install: install-elc

package: clean
	$(XEMACS) $(FLAGS) -f compile-nicola-ddskk-package $(PACKAGEDIR)

install-package: package
	$(XEMACS) $(FLAGS) -f install-nicola-ddskk-package $(PACKAGEDIR)

clean:
	-$(RM) $(GOMI)
