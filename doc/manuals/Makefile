STYLESHEETS_DIR = /usr/share/xml/docbook/stylesheet/nwalsh

all: html pdf

html:
	xsltproc -o LIXA_Getting_Started.html $(STYLESHEETS_DIR)/xhtml/docbook.xsl LIXA_Getting_Started.xml

fo:
	xsltproc -o LIXA_Getting_Started.fo $(STYLESHEETS_DIR)/fo/docbook.xsl LIXA_Getting_Started.xml

pdf: fo
	xmlroff -o LIXA_Getting_Started.pdf LIXA_Getting_Started.fo

clean:
	rm -rf LIXA_Getting_Started.html LIXA_Getting_Started.fo LIXA_Getting_Started.pdf
