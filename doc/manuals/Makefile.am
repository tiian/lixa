STYLESHEETS_DIR = @STYLESHEETS_DIR@
EXTRA_DIST = LIXA.xml.in $(doc_DATA) $(manual_sources) index_no_xsltproc.html

manual_sources = LIXA.xml \
	LIXA_Bibliography.xml \
	LIXA_Configuration.xml \
	LIXA_Development.xml \
	LIXA_Development_COBOL.xml \
	LIXA_Execution.xml \
	LIXA_In_Depth.xml \
	LIXA_Installation.xml \
	LIXA_Introduction.xml \
	LIXA_Preface.xml \
	LIXA_Recovery.xml \
	LIXA_Tuning.xml

docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)/html
doc_DATA = *.html

all: index_all_in_one.html index.html

if COMPILE_DOCBOOK_STUFF
index_all_in_one.html: $(manual_sources)
	@echo "Creating HTML manual (single page)..."
	xsltproc --stringparam generate.toc "book toc,title,figure,example,equation" -o index_all_in_one.html $(STYLESHEETS_DIR)/docbook.xsl LIXA.xml
index.html: $(manual_sources)
	@echo "Creating HTML manual (multi page)..."
	xsltproc --stringparam generate.toc "book toc,title,figure,example,equation" -o index.html $(STYLESHEETS_DIR)/chunk.xsl LIXA.xml
else
index_all_in_one.html: $(manual_sources)
	@echo "Sorry, you must have xsltproc and Docbook stylesheets to create HTML manual from XML files"
	cp index_no_xsltproc.html index_all_in_one.html
index.html: $(manual_sources)
	@echo "Sorry, you must have xsltproc and Docbook stylesheets to create HTML manual from XML files"
	cp index_no_xsltproc.html index.html
endif
