STYLESHEETS_DIR = /usr/share/xml/docbook/stylesheet/nwalsh

manual_sources = LIXA.xml \
	LIXA_Bibliography.xml \
	LIXA_Configuration.xml \
	LIXA_Development.xml \
	LIXA_Execution.xml \
	LIXA_In_Depth.xml \
	LIXA_Installation.xml \
	LIXA_Introduction.xml \
	LIXA_Preface.xml \
	LIXA_Recovery.xml

dia_images = LIXA_Configuration_1.dia  LIXA_Configuration_4.dia \
	LIXA_Configuration_7.dia LIXA_Configuration_2.dia \
	LIXA_Configuration_5.dia LIXA_Configuration_8.dia \
	LIXA_Configuration_3.dia LIXA_Configuration_6.dia \
	LIXA_Configuration_9.dia \
	LIXA_Development_1.dia LIXA_Development_2.dia LIXA_Development_3.dia \
	LIXA_Development_4.dia LIXA_Development_5.dia LIXA_Development_6.dia \
	LIXA_Development_7.dia LIXA_Development_8.dia LIXA_Development_9.dia \
	LIXA_Development_10.dia LIXA_Development_11.dia \
	LIXA_Recovery_01.dia LIXA_Recovery_02.dia

png_images = LIXA_Configuration_1.png  LIXA_Configuration_4.png \
	LIXA_Configuration_7.png LIXA_Configuration_2.png \
	LIXA_Configuration_5.png LIXA_Configuration_8.png \
	LIXA_Configuration_3.png LIXA_Configuration_6.png \
	LIXA_Configuration_9.png \
	LIXA_Development_1.png LIXA_Development_2.png LIXA_Development_3.png \
	LIXA_Development_4.png LIXA_Development_5.png LIXA_Development_6.png \
	LIXA_Development_7.png LIXA_Development_8.png LIXA_Development_9.png \
	LIXA_Development_10.png LIXA_Development_11.png \
	LIXA_Recovery_01.png LIXA_Recovery_02.png

docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)/html
doc_DATA = *.html $(png_images)

EXTRA_DIST = $(doc_DATA) $(manual_sources) $(dia_images) $(png_images)

all: INDEX.html index.html

if HAVE_XSLTPROC
INDEX.html: $(manual_sources) $(png_images)
	@echo "Creating HTML manual (single page)..."
	xsltproc --stringparam generate.toc "book toc,title,figure,example,equation" -o INDEX.html $(STYLESHEETS_DIR)/xhtml/docbook.xsl LIXA.xml
index.html: $(manual_sources) $(png_images)
	@echo "Creating HTML manual (multi page)..."
	xsltproc --stringparam generate.toc "book toc,title,figure,example,equation" -o index.html $(STYLESHEETS_DIR)/xhtml/chunk.xsl LIXA.xml
else
INDEX.html: $(manual_sources) $(png_images)
	@echo "Sorry, you must have xsltproc installed to create HTML manual from XML files"
index.html: $(manual_sources) $(png_images)
	@echo "Sorry, you must have xsltproc installed to create HTML manual from XML files"
endif
