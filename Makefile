# Makefile - for the IDLWAVE distribution.
#
# Maintainer: J.D. Smith <jdsmith@astro.cornell.edu>
# Version: VERSIONTAG 
#
# To install IDLWAVE, (optionally) edit the Makefile, type `make',
# then `make install', or `make install-all' (to include full info
# help files).

##----------------------------------------------------------------------
##  YOU MUST EDIT THE FOLLOWING LINES 
##----------------------------------------------------------------------

# Prefix of installation path for Info and Lisp files
prefix=/usr/local

# Where info files go.
infodir = $(prefix)/share/info

# Where local lisp files go.
lispdir = $(prefix)/share/emacs/site-lisp

# Where would you like to install the HTML help files?
# Don't forget to configure the Emacs variable `idlwave-html-help-location'
# It must also point to the directory where help files are installed.
helpdir = $(prefix)/etc

# Name of your emacs binary
EMACS=emacs

##----------------------------------------------------------------------
## YOU MAY NEED TO EDIT THESE (PROBABLY NOT...)
##----------------------------------------------------------------------

# Using emacs in batch mode.
BATCH=$(EMACS) -batch -no-init-file -l lpath.el

# Specify the byte-compiler for compiling .el files
ELC= $(BATCH) -f batch-byte-compile

# How to make a dvi file from a texinfo file
TEXI2DVI = texi2dvi

# How to create directories
MKDIR = mkdir -p

# How to make a postscript file from a dvi file
DVIPS = dvips

# How to create the info files from the texinfo file
MAKEINFO = makeinfo

# How to create the HTML file
TEXI2HTML = texi2html -monolithic -number

# How to create the PDF fil

TEXI2PDF = texi2pdf

# How to move the byte compiled files to their destination.  
MV = mv

# How to copy the lisp files to their destination.
CP = cp -p

##----------------------------------------------------------------------
##  BELOW THIS LINE ON YOUR OWN RISK!
##----------------------------------------------------------------------

# The following variables need to be defined by the maintainer
LISPFILES  = idlwave.el idlw-variables.el  idlw-help.el idlw-shell.el idlw-scan.el\
	     idlw-routine.el idlw-complete.el idlw-bindings.el idlw-menus.el\
	     idlw-toolbar.el idlw-complete-structtag.el idlw-roprompt.el 
ELCFILES   = $(LISPFILES:.el=.elc)
TEXIFILES  = idlwave.texi
INFOFILES  = idlwave
RINFOFILES = idlw-rinfo.el
HTMLHELPDIR  = idl_html_help
DLDIR     = /Volumes/www/idlwave/download
HTMLDIR    = /Volumes/www/idlwave/
XEMACSDIR  = packages/xemacs-packages/idlwave

.SUFFIXES: .el .elc .texi
SHELL = /bin/sh

DISTFILES= README INSTALL CHANGES COPYING Makefile\
	$(LISPFILES) $(TEXIFILES) $(INFOFILES) lpath.el\
	idlwave_catalog tutorial.pro

WEBDISTFILES= idlwave.ps idlwave.pdf idlwave.html CHANGES
HELPDISTFILES= $(HTMLHELPDIR)
HELPDISTFILE=idlwave-idlv$(IDL)-help.tar.bz2
XEMACSDISTFILES= README INSTALL CHANGES COPYING\
	$(LISPFILES) $(TEXIFILES) $(INFOFILES)\
	idlwave_catalog tutorial.pro

EMACSDISTFILES= $(LISPFILES) $(TEXIFILES) ChangeLog

DOWNGRADEFILES= README.downgrade idlw-rinfo.el idlw-help.el idlw-help.txt

.PHONY: all
all:	lisp

.PHONY: lisp
lisp:	$(LISPFILES)
	$(ELC) $(LISPFILES)

.PHONY: install
install: install-lisp

.PHONY: install-all
install-all: install-lisp install-info

.PHONY: install-lisp
install-lisp: 
	if [ ! -d $(lispdir) ]; then $(MKDIR) $(lispdir); else true; fi ;
	$(CP) $(LISPFILES) $(lispdir)
	$(CP) $(ELCFILES)  $(lispdir)

.PHONY: install-info
install-info: info
	if [ ! -d $(infodir) ]; then $(MKDIR) $(infodir); else true; fi ;
	$(CP) $(INFOFILES) $(infodir)


# HELP 
HELPFILEMAYBE := $(shell ls -1 idlwave-*help.tar.* 2>/dev/null | head -1 )
ifdef HELPFILEMAYBE
HELPFILECODE  := $(shell echo $(HELPFILEMAYBE) | grep -q "\.gz$$" && echo "z" || echo "j")
endif

# XEMACS VERSION
XEMACS-TAG := $(shell [ -f  $(XEMACSDIR)/Makefile ] &&  perl -ne 'if(/^VERSION\s+=\s+([0-9]\.[0-9]+)/) {print $$1; exit}' $(XEMACSDIR)/Makefile)


.PHONY: helpdist
helpdist: helpdistfile
ifdef IDL
	if [ ! -f $(DLDIR)/$(HELPDISTFILE) ]; then $(CP) $(HELPDISTFILE) $(DLDIR); (cd $(DLDIR); ln -sf $(HELPDISTFILE) idlwave-help.tar.bz2) ; fi
	if [ ! -f $(DLDIR)/$(HELPDISTFILE:.bz2=.gz) ]; then $(CP) $(HELPDISTFILE:.bz2=.gz) $(DLDIR); (cd $(DLDIR); ln -sf $(HELPDISTFILE) idlwave-help.tar.gz) ; fi
endif

.PHONY: helpdistfile
helpdistfile: $(HELPDISTFILE) $(HELPDISTFILE:.bz2=.gz)

$(HELPDISTFILE): $(HELPDISTFILES)
	tar cjf $(HELPDISTFILE) $(HELPDISTFILES)

$(HELPDISTFILE:.bz2=.gz): $(HELPDISTFILES)
	tar czf $(HELPDISTFILE:.bz2=.gz) $(HELPDISTFILES)

.PHONY: install-help
install-help: 
	@[ -d  idl_html_help -o -f "$(HELPFILEMAYBE)" ] || { echo -e "****  Help package missing.  ****\n      Download here from idlwave.org and try again, if desired."; exit 1; }
	if [ ! -d $(helpdir) ]; then $(MKDIR) $(helpdir); else true; fi ;
	if [ -f $(HELPFILEMAYBE) ]; then tar x$(HELPFILECODE)f $(HELPFILEMAYBE) -C $(helpdir) ; else  $(CP) $(HTMLHELPDIR) $(helpdir) ; fi ;

# EMACS code 

.PHONY: distfile
distfile: $(DISTFILES)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	rm -rf idlwave-$(TAG)
	mkdir idlwave-$(TAG)
	cp -p $(DISTFILES) idlwave-$(TAG)/
	chmod ug+rw idlwave-$(TAG)/*
	perl -pi -e 's/\bVERSIONTAG\b/$(TAG)/' idlwave-$(TAG)/* 
	tar czvf idlwave-$(TAG).tar.gz idlwave-$(TAG)
	rm -rf idlwave-$(TAG)	


.PHONY: dist
dist: $(WEBDISTFILES)
	make distfile TAG=$(TAG)
	cp -p idlwave-$(TAG).tar.gz $(DLDIR)
	(cd $(DLDIR); ln -sf idlwave-$(TAG).tar.gz idlwave.tar.gz)
	(cd $(DLDIR); ln -sf idlwave-$(TAG).tar.gz idlwave-alpha.tar.gz)
	cp -f $(WEBDISTFILES) $(HTMLDIR)
	perl -pi -e 's/\bVERSIONTAG\b/$(TAG)/' $(HTMLDIR)/CHANGES

.PHONY: alphadist
alphadist:
	make distfile TAG=$(TAG)
	cp idlwave-$(TAG).tar.gz $(DLDIR)
	(cd $(DLDIR); ln -sf idlwave-$(TAG).tar.gz idlwave-alpha.tar.gz)

.el.elc:
	$(ELC) $<


# XEMACS Code

.PHONY: xemacsdistfile
xemacsdistfile: $(XEMACSDISTFILES)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	cp -pf $(XEMACSDISTFILES) $(XEMACSDIR)/
	(cd $(XEMACSDIR); perl -pi -e 's/\bVERSIONTAG\b/$(TAG)/' $(XEMACSDISTFILES))
	perl -pi -e 's/^(AUTHOR_VERSION\s*=\s*)([0-9]\.[0-9.a-z]+)/$${1}$(TAG)/' $(XEMACSDIR)/Makefile
	(cd $(XEMACSDIR); make bindist)

.PHONY: xemacsdist
xemacsdist: 
	make xemacsdistfile TAG=$(TAG)
	cp -p xemacs-packages/idlwave-$(XEMACS-TAG)-pkg.tar.gz $(DLDIR)
	(cd $(DLDIR); ln -sf idlwave-$(XEMACS-TAG)-pkg.tar.gz idlwave-xemacs.tar.gz)

.PHONY: xemacsalphadist
xemacsalphadist:
	make xemacsdistfile TAG=$(TAG)
	cp -p xemacs-packages/idlwave-$(TAG)-pkg.tar.gz $(DLDIR)
	(cd $(DLDIR); ln -sf idlwave-$(TAG)-pkg.tar.gz idlwave-xemacs-alpha.tar.gz)

# Specific File Targets

$(INFOFILES): $(TEXIFILES)
	$(MAKEINFO) idlwave.texi

idlwave.dvi: idlwave.texi
	$(TEXI2DVI) idlwave.texi

idlwave.ps: idlwave.dvi
	$(DVIPS) -o idlwave.ps idlwave.dvi

idlwave.html: idlwave.texi
	$(TEXI2HTML) idlwave.texi

idlwave.pdf: idlwave.texi
	$(TEXI2PDF) idlwave.texi

NUTSHELL: idlwave.texi
	makeinfo --no-headers idlwave.texi|perl NUTSHELL.pl>NUTSHELL


# Helper Targets

.PHONY: info
info:	$(INFOFILES)

.PHONY: dvi
dvi:    idlwave.dvi

.PHONY: view
view:   
	xdvi idlwave.dvi&

.PHONY: ps
ps:	idlwave.ps

.PHONY: ps2
ps2:	idlwave.ps
	psnup -2 idlwave.ps>idlwave.ps2

.PHONY: html
html:	idlwave.html

.PHONY: dvi
pdf:    idlwave.pdf

rinfo:  rinfo56_html

rinfo53:
	./get_rinfo53 -txt -path pdf53 -idl idl_5.3

rinfo54:
	./get_rinfo54 -txt -path pdf54 -idl idl_5.4

rinfo55: 
	./get_rinfo -fixup help55fixup.txt -txt -path pdf55 -idl idl_5.5

rinfo56: 
	./get_rinfo -txt -path pdf56 -idl idl_5.6

rinfo56_html:
	./get_html_rinfo -path idl_html_help/ -idl idl_5.6

dgkit:
	@if [ "X$(IDL)" = "X" ]; then echo "*** No IDL tag ***"; exit 1; fi
	make rinfo$(IDL)
	tar czvf idlwave-downgrade-for-idl$(IDL).tar.gz $(DOWNGRADEFILES)


# Cleanup

clean:
	rm -f $(ELCFILES)
	rm -f *~ 
	rm -f *.aux *.cp *.cps *.dvi *.fn *.fns *.ky *.kys *.pg *.pgs
	rm -f *.toc *.tp *.tps *.vr *.vrs *.log *.html *.ps

veryclean:
	rm -f $(ELCFILES)
	rm -f *~ idlwave idlwave-[1-9]
	rm -f *.aux *.cp *.cps *.dvi *.fn *.fns *.ky *.kys *.pg *.pgs
	rm -f *.toc *.tp *.tps *.vr *.vrs *.log *.html *.ps

