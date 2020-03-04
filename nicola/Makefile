#
# Makefile for NICOLA-DDSKK.
#

RM	= /bin/rm -f

EMACS	= emacs
FLAGS   = --batch --quick -no-site-file --directory ../ --load NICOLA-DDSKK-MK

PREFIX = NONE
LISPDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE

GOMI	= *.elc auto-autoloads.el custom-load.el nicola-ddskk-autoloads.el

all: elc

elc: clean
	$(EMACS) $(FLAGS) --funcall compile-nicola-ddskk $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)

install-elc: elc
	$(EMACS) $(FLAGS) --funcall install-nicola-ddskk $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)

install: install-elc

clean:
	-$(RM) $(GOMI)
