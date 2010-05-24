STYLESHEETS_DIR = /usr/share/xml/docbook/stylesheet/nwalsh

all: html pdf

html:	LIXA_Getting_Started.html

pdf: 	LIXA_Getting_Started.pdf

LIXA_Getting_Started.html:	LIXA_Getting_Started.xml \
				LIXA_Getting_Started_Preface.xml

LIXA_Getting_Started.fo:	LIXA_Getting_Started.xml \
				LIXA_Getting_Started_Preface.xml

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
