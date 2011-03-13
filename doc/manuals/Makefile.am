STYLESHEETS_DIR = /usr/share/xml/docbook/stylesheet/nwalsh

manual_sources = LIXA.xml \
	LIXA_Bibliography.xml \
	LIXA_Configuration.xml \
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
	LIXA_Configuration_9.dia

png_images = LIXA_Configuration_1.png  LIXA_Configuration_4.png \
	LIXA_Configuration_7.png LIXA_Configuration_2.png \
	LIXA_Configuration_5.png LIXA_Configuration_8.png \
	LIXA_Configuration_3.png LIXA_Configuration_6.png \
	LIXA_Configuration_9.png

docdir = $(datadir)/doc/$(PACKAGE)-$(VERSION)/html
doc_DATA = index.html $(png_images)

EXTRA_DIST = $(doc_DATA) $(manual_sources) $(dia_images) $(png_images)

all: index.html

if HAVE_XSLTPROC
index.html: $(manual_sources) $(png_images)
	@echo "Creating HTML manual..."
	xsltproc -o index.html $(STYLESHEETS_DIR)/xhtml/docbook.xsl LIXA.xml
else
index.html: $(manual_sources) $(png_images)
	@echo "Sorry, you must have xsltproc installed to create HTML manual from XML files"
endif
