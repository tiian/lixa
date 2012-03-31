STYLESHEETS_DIR = @STYLESHEETS_DIR@
EXTRA_DIST = LIXA_EXT.xml.in $(doc_DATA) $(manual_sources) $(dia_images) $(png_images) index_no_xsltproc.html

manual_sources = LIXA_EXT.xml \
	LIXA_EXT_Introduction.xml \
	LIXA_EXT_PHP.xml \
	LIXA_EXT_Bibliography.xml

dia_images =

png_images =

docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)/html
doc_DATA = *.html $(png_images)


all: index_ext_all_in_one.html index_ext.html

if COMPILE_DOCBOOK_STUFF
index_ext_all_in_one.html: $(manual_sources) $(png_images)
	@echo "Creating HTML manual (single page)..."
	xsltproc --stringparam generate.toc "book toc,title,figure,example,equation" -o index_ext_all_in_one.html $(STYLESHEETS_DIR)/docbook.xsl LIXA_EXT.xml
index_ext.html: $(manual_sources) $(png_images)
	@echo "Creating HTML manual (multi page)..."
	xsltproc --stringparam generate.toc "book toc,title,figure,example,equation" -o index_ext.html $(STYLESHEETS_DIR)/chunk.xsl LIXA_EXT.xml
else
index_ext_all_in_one.html: $(manual_sources) $(png_images)
	@echo "Sorry, you must have xsltproc and Docbook stylesheets to create HTML manual from XML files"
	cp index_no_xsltproc.html index_ext_all_in_one.html
index_ext.html: $(manual_sources) $(png_images)
	@echo "Sorry, you must have xsltproc and Docbook stylesheets to create HTML manual from XML files"
	cp index_no_xsltproc.html index_ext.html
endif
