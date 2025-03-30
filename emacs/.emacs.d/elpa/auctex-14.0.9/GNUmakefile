# Rules to generate the files that need to go into the ELPA package.

# This file is part of AUCTeX.

# AUCTeX is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at
# your option) any later version.

# AUCTeX is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Files we need to auto-generate for elpa:
#   README
#   ChangeLog
#   doc: preview-dtxdoc.texi
#   doc: version.texi
#   doc: tex-ref.pdf
#   latex: prauctex.cfg
#   latex: prauctex.def
#   latex: prcounters.def
#   latex: preview.sty
#   latex: prfootnotes.def
#   latex: prlyx.def
#   latex: prshowbox.def
#   latex: prshowlabels.def
#   latex: prtightpage.def
#   latex: prtracingall.def

EMACSBIN=emacs
EMACS=$(EMACSBIN) --batch -q -no-site-file -no-init-file -l lpath.el
MAKEINFO=makeinfo
INSTALL_INFO=install-info
PERL=perl
PDFLATEX=pdfla$(TEX)
PDFTEX=pdf$(TEX)

MANUALS=auctex preview-latex
INFO_FILES:=$(addprefix doc/, $(MANUALS:=.info))

TEXMFGEN:=$(shell sed -n 's/^%<installer>.*file[{]\([^}.]*\.[sdc][tef][yfg]\)[}].*/\1/p' latex/preview.dtx)
LATEX_FILES:=$(patsubst %, latex/%, $(shell echo $$(echo "$(TEXMFGEN)")))

MAIN_GENERATED_FILES=README 		\
		doc/version.texi	\
		doc/preview-dtxdoc.texi	\
		doc/tex-ref.pdf         \
		$(LATEX_FILES)

ALL_GENERATED_FILES=$(MAIN_GENERATED_FILES)	\
		doc/dir				\
		$(INFO_FILES)

# Generate & compile everything including the manuals below doc/.
all: $(ALL_GENERATED_FILES) compile auctex-autoloads.el

compile: $(patsubst %.el,%.elc,$(wildcard *.el style/*.el))

# If we were depending on emacs 29.1, we could simply use
# loaddefs-generate...
AUTOLOAD=--eval '\
(let* ((autoload-file (expand-file-name "$@")) \
       (autoload-file-dir (file-name-directory autoload-file)) \
       (addition-code "(add-to-list (quote load-path)\n\
               (directory-file-name\n\
                 (file-name-directory load-file-name)))")) \
  (if (fboundp (quote loaddefs-generate)) \
      (loaddefs-generate autoload-file-dir autoload-file \
                         (list "tex-wizard.el" "auctex.el" "lpath.el") \
                         addition-code) \
    (setq generated-autoload-file autoload-file) \
    (update-directory-autoloads autoload-file-dir) \
    (with-temp-file autoload-file \
      (insert-file-contents autoload-file) \
      (cond ((progn (goto-char (point-min)) \
                    (re-search-forward "^;;; Code:" nil t)) \
             (newline 2)) \
            ((progn (goto-char (point-min)) \
                    (re-search-forward "^$$")) \
             (open-line 2)) \
            (t (goto-char (point-max)))) \
      (insert addition-code))) \
  (save-buffers-kill-emacs t))'

auctex-autoloads.el:
	rm -f $@
	$(EMACS) $(AUTOLOAD)

%.elc: %.el
	$(EMACS) -f batch-byte-compile $<

# Generate everything but don't compile or build the docs.  The docs
# will be built on elpa due to :doc ("doc/auctex.texi"
# "doc/preview-latex.texi") in the auctex recipe in elpa-packges and
# compiling is done locally.
elpa: $(MAIN_GENERATED_FILES) ChangeLog

clean:
	rm -f $(ALL_GENERATED_FILES) \
		$(wildcard *.elc style/*.elc) \
		$(wildcard latex/*.aux latex/*.drv latex/*.hd latex/*.log) \
		$(wildcard latex/*.out latex/*.pdf latex/*.tar.gz) \
		$(wildcard doc/*.cp doc/*.cps doc/*.fn doc/*.fns) \
		$(wildcard doc/*.ky doc/*.kys doc/*.vr doc/*.vrs) \
		$(wildcard doc/*.aux doc/*.log doc/*.toc) \
		$(wildcard doc/*.pdf doc/*.synctex.gz) \
		latex/preview-mk.ins latex/preview.ins \
		auctex-autoloads.el \
		$(DYNVARSFILES)

# Copied&adapted from doc/Makefile.in.  The 'echo ""' part fixes the
# spacing above the insertion from preview-readme.texi.
MAKEINFO_PLAIN=$(MAKEINFO) -D rawfile --no-headers
README: doc/intro.texi doc/preview-readme.texi doc/macros.texi
	(cd doc; $(MAKEINFO_PLAIN) intro.texi --output -) >$@
	(cd doc; echo "") >>$@
	(cd doc; $(MAKEINFO_PLAIN) preview-readme.texi --output -) >>$@

# Committer date of HEAD.
AUCTEXDATE:=$(shell (git log -n1 --pretty=tformat:"%ci" 2>/dev/null \
                     || date +"%Y-%m-%d %T") 		       	    \
                    | sed -re 's/ /_/' -e 's/ .*//')
# Extract the version number from the diff line "+;; Version: 14.0.4" of
# the commit HEAD which is only filled when we did a release in the last
# commit.
THISVERSION:=$(shell git show HEAD -- auctex.el 2>/dev/null \
	| sed -nre 's/[+];; Version: ([0-9]+.[0-9]+.[0-9]+)/\1/p')
# Extract the last released version number from `auctex.el`.
LASTVERSION:=$(shell grep "^;; Version:" auctex.el \
                     | sed -nre 's/;; Version: ([0-9]+.[0-9]+.[0-9]+)/\1/p;q')
AUCTEXVERSION:=$(if $(THISVERSION),$(THISVERSION),$(LASTVERSION).$(AUCTEXDATE))

doc/version.texi:
	echo @set VERSION $(AUCTEXVERSION) >$@
	echo @set UPDATED $(AUCTEXDATE) >>$@

ChangeLog:
	rm -f $@
	./build-aux/gitlog-to-auctexlog && cat ChangeLog.1 >> $@

# Copied&adapted from doc/Makefile.in.
doc/preview-dtxdoc.texi: latex/preview.dtx doc/preview-dtxdoc.pl
	$(PERL) doc/preview-dtxdoc.pl latex/preview.dtx $@

# Copied&adapted from doc/Makefile.in.
doc/tex-ref.pdf: doc/tex-ref.tex
	cd doc && $(PDFTEX) tex-ref.tex && rm tex-ref.log

# Copied&adapted from doc/Makefile.in.
TEXI_SOURCES:=$(wildcard doc/*.texi) doc/version.texi doc/preview-dtxdoc.texi
$(INFO_FILES): doc/%.info: $(TEXI_SOURCES)
	cd doc; $(MAKEINFO) --no-split $*.texi

doc/dir: $(INFO_FILES)
	for f in $(INFO_FILES); do $(INSTALL_INFO) --info-dir=doc $$f; done

$(LATEX_FILES): latex/preview.dtx latex/bootstrap.ins
	cd latex; $(TEX) '\nonstopmode \input bootstrap.ins'
	cd latex; $(TEX) '\nonstopmode \input preview-mk.ins'

# The next rules are copied&adapted from Makefile.in and
# latex/Makefile.in.  `preview-ctan' is the rule needed for creating a
# tarball for CTAN upload.
preview.ins: latex/preview.dtx
	cd latex && rm -f $@ && \
	$(TEX) '\nonstopmode\def\jobname{.ins}\input docstrip ' \
	'\generate{\file{preview.ins}{\from{preview.dtx}{installer}}}' \
	'\endbatchfile'

preview.pdf: latex/preview.dtx latex/preview.sty
	cd latex && \
	$(PDFLATEX) '\nonstopmode \input{preview.drv}' && \
	$(PDFLATEX) '\nonstopmode \input{preview.drv}' && \
	$(PDFLATEX) '\nonstopmode \input{preview.drv}'

preview-ctan: $(LATEX_FILES) preview.ins preview.pdf
	cd latex && \
	mkdir -p preview && \
	cp README preview.dtx preview.ins preview.pdf preview/ && \
	tar -cz --owner=root --group=root -f preview.tar.gz preview/ && \
	rm -rf preview/

# Cross-file variable checking with lexical binding
# https://www.gnu.org/software/emacs/manual/html_node/elisp/Converting-to-Lexical-Binding.html
DYNVARSFILES = *.dynvars style/*.dynvars auctex-dynvars
dynvars-check:
	rm -f $(wildcard *.elc) $(wildcard style/*.elc) $(DYNVARSFILES)
	EMACS_GENERATE_DYNVARS=1 $(EMACS) -f batch-byte-compile \
		$(wildcard *.el) $(wildcard style/*.el) \
		> /dev/null 2>&1
	cat *.dynvars style/*.dynvars > auctex-dynvars
	rm -f $(wildcard *.elc) $(wildcard style/*.elc)
	EMACS_DYNVARS_FILE=auctex-dynvars $(EMACS) \
		-f batch-byte-compile \
		$(wildcard *.el) $(wildcard style/*.el)
