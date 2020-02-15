#
# Makefile for NICOLA-DDSKK.
#

RM	= /bin/rm -f

EMACS	= emacs
FLAGS   = -batch -q -no-site-file -l NICOLA-DDSKK-MK

PREFIX = NONE
LISPDIR = NONE
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

clean:
	-$(RM) $(GOMI)
