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
infodir = $(prefix)/info

# Where local lisp files go.
lispdir = $(prefix)/share/emacs/site-lisp

# Where would you like to install the help files?
# Don't forget to configure the Emacs variable `idlwave-help-directory'
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
LISPFILES  = idlwave.el idlw-shell.el idlw-rinfo.el idlw-toolbar.el \
	     idlw-complete-structtag.el idlw-roprompt.el
LISPFILES1 = $(LISPFILES) idlw-help.el
ELCFILES   = $(LISPFILES:.el=.elc)
TEXIFILES  = idlwave.texi
INFOFILES  = idlwave idlwave-1 idlwave-2 idlwave-3
RINFOFILES = idlw-help.el idlw-help.txt idlw-rinfo.el
HELPFILES  = idlw-help.el idlw-help.txt
DLDIR     = /var/www/html/idlwave/download
HTMLDIR    = /var/www/html/idlwave/
XEMACSDIR  = packages/xemacs-packages/idlwave

# An alternative installation point
#MY_INFODIR = /home/strw/dominik/lib/emacs/info
#MY_LISPDIR = /home/strw/dominik/lib/emacs/lisp

.SUFFIXES: .el .elc .texi
SHELL = /bin/sh

DISTFILES= README INSTALL CHANGES ChangeLog COPYING Makefile\
	$(LISPFILES) $(TEXIFILES) $(INFOFILES) lpath.el\
	idltags get_rinfo tutorial.pro

WEBDISTFILES= idlwave.ps idlwave.pdf idlwave.html CHANGES
HELPDISTFILES= README.hlp idlw-help-topics.el help55fixup.txt $(HELPFILES)

XEMACSDISTFILES= README INSTALL CHANGES ChangeLog COPYING\
	$(LISPFILES) $(TEXIFILES) $(INFOFILES)\
	idltags get_rinfo help55fixup.txt tutorial.pro

EMACSDISTFILES= $(LISPFILES) $(TEXIFILES) ChangeLog

DOWNGRADEFILES= README.downgrade idlw-rinfo.el idlw-help.el idlw-help.txt

.PHONY: all
all:	lisp

.PHONY: install
install: install-lisp

.PHONY: install-all
install-all: install-lisp install-info install-help

.PHONY: lisp
lisp:	$(LISPFILES)
	$(ELC) $(LISPFILES)

.PHONY: compile
compile: $(LISPFILES)
	$(ELC) $(LISPFILES)

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

.PHONY: install-lisp
install-lisp: 
	if [ ! -d $(lispdir) ]; then $(MKDIR) $(lispdir); else true; fi ;
	$(CP) $(LISPFILES) $(lispdir)
	$(CP) $(ELCFILES)  $(lispdir)

.PHONY: install-info
install-info:
	if [ ! -d $(infodir) ]; then $(MKDIR) $(infodir); else true; fi ;
	$(CP) $(INFOFILES) $(infodir)

.PHONY: install-help
install-help: 
	@[ -f  idlw-help.el -a -f idlw-help.txt ] || { echo "Help package missing. download from idlwave.org and install here."; exit 1; }
	$(ELC) idlw-help.el
	if [ ! -d $(helpdir) ]; then $(MKDIR) $(helpdir); else true; fi ;
	$(CP) $(HELPFILES) idlw-help.elc $(helpdir)

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

rinfo:  rinfo55

rinfo53:
	./get_rinfo53 -txt -path pdf53 -idl idl_5.3

rinfo54:
	./get_rinfo54 -txt -path pdf54 -idl idl_5.4

rinfo55: 
	./get_rinfo -fixup help55fixup.txt -txt -path pdf55 -idl idl_5.5

dgkit:
	@if [ "X$(IDL)" = "X" ]; then echo "*** No IDL tag ***"; exit 1; fi
	make rinfo$(IDL)
	gtar zcvf idlwave-downgrade-for-idl$(IDL).tar.gz $(DOWNGRADEFILES)

NUTSHELL: idlwave.texi
	makeinfo --no-headers idlwave.texi|perl NUTSHELL.pl>NUTSHELL

wcompile:
	xemacs -batch -q -l lpath-warn.el -f batch-byte-compile $(LISPFILES1)

xcompile:
	xemacs -batch -q -l lpath-warn.el -f batch-byte-compile $(LISPFILES1)

ecompile:
	emacs -batch -q -l lpath-warn.el -f batch-byte-compile $(LISPFILES1)

ccompile:
	xemacs -batch -q -l lpath-compatible.el -f batch-byte-compile $(LISPFILES1)

#myinstall: $(LISPFILES) $(ELCFILES) $(INFOFILES)
#	if [ ! -d $(MY_LISPDIR) ]; then mkdir $(MY_LISPDIR); else true; fi ;
#	$(CP) $(LISPFILES) $(MY_LISPDIR)
#	$(CP) $(ELCFILES)  $(MY_LISPDIR)
#	if [ ! -d $(MY_INFODIR) ]; then mkdir $(MY_INFODIR); else true; fi ;
#	$(CP) $(INFOFILES) $(MY_INFODIR)

.PHONY: distfile
distfile: $(DISTFILES)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
#	make rinfo
	rm -rf idlwave-$(TAG)
	mkdir idlwave-$(TAG)
	cp -p $(DISTFILES) idlwave-$(TAG)/
	chmod ug+rw idlwave-$(TAG)/*
	perl -pi -e 's/\sVERSIONTAG\b/ $(TAG)/' idlwave-$(TAG)/* 
	tar czvf idlwave-$(TAG).tar.gz idlwave-$(TAG)
	rm -rf idlwave-$(TAG)	
	rm -rf idlwave-help-$(TAG)
	mkdir idlwave-help-$(TAG)
	cp -p $(HELPDISTFILES) idlwave-help-$(TAG)/
	perl -pi -e 's/\sVERSIONTAG\b/ $(TAG)/' idlwave-help-$(TAG)/*.el
	perl -pi -e '{local $$/=$$/; open(TOPICS,"idlw-help-topics.el"); chomp($$created=<TOPICS>.<TOPICS>); undef $$/; $$topics=<TOPICS>;} s/^;;; INSERT-CREATED-BY-HERE/$$created/; s/^;;; INSERT-HELP-TOPICS-HERE/$$topics/;' idlwave-help-$(TAG)/*.el
	tar czvf idlwave-$(TAG)-help.tar.gz -C idlwave-help-$(TAG) $(HELPDISTFILES)
	rm -rf idlwave-help-$(TAG)

.PHONY: dist
dist: $(WEBDISTFILES)
	make distfile TAG=$(TAG)
	cp -p idlwave-$(TAG).tar.gz $(DLDIR)
	cp -p idlwave-$(TAG)-help.tar.gz $(DLDIR)
	(cd $(DLDIR); ln -sf idlwave-$(TAG).tar.gz idlwave.tar.gz)
	(cd $(DLDIR); ln -sf idlwave-$(TAG)-help.tar.gz idlwave-help.tar.gz)
	(cd $(DLDIR); ln -sf idlwave-$(TAG).tar.gz idlwave-alpha.tar.gz)
	cp -f $(WEBDISTFILES) $(HTMLDIR)
	perl -pi -e 's/\sVERSIONTAG\b/ $(TAG)/' $(HTMLDIR)/CHANGES

.PHONY: xemacsdistfile
xemacsdistfile: $(XEMACSDISTFILES)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	cp -pf $(XEMACSDISTFILES) $(XEMACSDIR)/
	perl -pi -e 's/^((?:AUTHOR_)?VERSION\s*=\s*)([0-9]\.[0-9.a-z]+)/$${1}$(TAG)/' $(XEMACSDIR)/Makefile
	perl -pi -e 's/\sVERSIONTAG\b/ $(TAG)/' $(XEMACSDIR)/*
	(cd $(XEMACSDIR); make bindist)

.PHONY: xemacsdist
xemacsdist: 
	make xemacsdistfile TAG=$(TAG)
	cp -p xemacs-packages/idlwave-$(TAG)-pkg.tar.gz $(DLDIR)
	(cd $(DLDIR); ln -sf idlwave-$(TAG)-pkg.tar.gz idlwave-xemacs.tar.gz)

alphadist: $(WEBDISTFILES)
	make distfile TAG=$(TAG)
	cp idlwave-$(TAG).tar.gz $(DLDIR)
	cp idlwave-$(TAG)-help.tar.gz $(DLDIR)
	cp CHANGES $(HTMLDIR)
	perl -pi -e 's/\sVERSIONTAG\b/ $(TAG)/' $(HTMLDIR)/CHANGES
	(cd $(DLDIR); ln -sf idlwave-$(TAG).tar.gz idlwave-alpha.tar.gz)
	(cd $(DLDIR); ln -sf idlwave-$(TAG)-help.tar.gz idlwave-help-alpha.tar.gz)

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


linkelc:
	rm -f ../lisp/idlw*.elc
	(cd ../lisp;ln -s ../idlwave/idlw*.elc .)

unlinkelc:
	rm -f ../lisp/idlw*.elc
	rm -f *.elc

.el.elc:
	$(ELC) $<