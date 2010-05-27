STYLESHEETS_DIR = /usr/share/xml/docbook/stylesheet/nwalsh

all: html pdf

html:	LIXA.html

pdf: 	LIXA.pdf

LIXA.html:	LIXA.xml \
		LIXA_Preface.xml \
		LIXA_Introduction.xml \
		LIXA_Bibliography.xml

LIXA.fo:	LIXA.xml \
		LIXA_Preface.xml \
		LIXA_Introduction.xml \
		LIXA_Bibliography.xml

%.html:	%.xml
	xsltproc -o $@ $(STYLESHEETS_DIR)/xhtml/docbook.xsl $<
	cp $@ /var/www/lixa

%.fo:	%.xml
	xsltproc -o $@ $(STYLESHEETS_DIR)/fo/docbook.xsl $<

%.pdf:	%.fo
	xmlroff -o $@ $<
	cp $@ /var/www/lixa

clean:
	rm -rf *.html *.fo *.pdf
