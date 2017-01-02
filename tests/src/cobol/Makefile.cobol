# Makefile for the GnuCOBOL FAQ
# Brian Tiffin, Modified: 2015-11-14/06:58-0500
# Dedicated to the public domain, all rights waived
.RECIPEPREFIX = >

# default options, note that -g will leave intermediate files
COBCOPTS = -I ../../../src/client -L../../../src/client/.libs -llixac

COB_LDFLAGS = -Wl,--no-as-needed

# filenames to cleanup
COBCCLEAN = $*.c $*.s $*.i $*.c.h $*.c.l* $*.so $*.html $*

# Simple GnuCOBOL rules.  Customize to taste,
# create an executable
%: %.cob
	cobc $(COBCOPTS) -x $^ -o $@

# create an executable, and run it
%.run: %.cob
	cobc $(COBCOPTS) -xj $^ -o $@

# create an executable, and mark date-compiled
#%.mark: %.cob
#> sed -i 's#date-compiled\..*$$#date-compiled\. '\
#"$$(date +%Y-%m-%d/%H:%M%z)"'\.#' $^
#> cobc $(COBCOPTS) -x $^ -o $@

# create a dynamic module
%.so: %.cob
	cobc $(COBCOPTS) -m $^ -o $@

# create a linkable object
%.o: %.cob
	cobc $(COBCOPTS) -c $^ -o $@

# generate C code
%.c: %.cob
	cobc $(COBCOPTS) -C $^

# generate assembly
%.s: %.cob
	cobc $(COBCOPTS) -S $^

# generate intermediates in tmps
%.i: %.cob
	[ -d tmps ] || mkdir tmps
	cobc $(COBCOPTS) --save-temps=tmps -c $^

# create an executable; if errors, call vim in quickfix
%.q: %.cob
	cobc $(COBCOPTS) -x $^ 2>errors.err || vi -q

# make binary; capture warnings, call vim quickfix
%.qw: %.cob
	cobc $(COBCOPTS) -x $^ 2>errors.err ; vi -q

# run ocdoc to get documentation
%.ocdoc: %.cob
	./ocdoc $^ $*.rst $*.html $*.css

# run rst2html
%.html: %.cob
	sed ':loop;/!rst.marker!/{d};N;b loop' $^ | sed '$$d' \
    | sed 's/:SAMPLE:/$*/' | rst2html >$*.html

# run cobxref
%.lst: %.cob
	cobc $(COBCOPTS) -Xref $^

# run cobolmac, .cbl to .cob
%.mac: %.cbl
	cobolmac <$^ >$*.cob

# clean up -g files, with interactive prompting, just in case
%.clean: %.cob
	@echo "Remove: " $(COBCCLEAN)
	@(read -p "Are you sure? " -r; \
    if [[ $$REPLY =~ ^[Yy]$$ ]]; then rm $(COBCCLEAN) ; fi)

# tectonics for occurlrefresh
occurlrefresh: occurl.c occurlsym.cpy occurlrefresh.cbl
	cobc -x $(COBCOPTS) occurlrefresh.cbl occurl.c -lcurl

