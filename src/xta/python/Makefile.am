dist_noinst_DATA = xta.i setup.py.i

# Produce XTA wrapper using SWIG (C++)

xta_wrap.cpp: xta.i ../cpp/Xta.hpp
	$(SWIG) -python -I../cpp -o $@ xta.i

setup.py:	setup.py.i ../../../config.status
	sed \
	-e 's|@libdir[@]|$(libdir)|g' \
        -e 's|@abs_top_builddir[@]|$(abs_top_builddir)|g' \
	-e 's|@_XTA_PREFIX[@]|$(_XTA_PREFIX)|g' \
	-e 's|@PACKAGE_BUGREPORT[@]|$(PACKAGE_BUGREPORT)|g' \
	-e 's|@PACKAGE_NAME[@]|$(PACKAGE_NAME)|g' \
	-e 's|@PACKAGE_STRING[@]|$(PACKAGE_STRING)|g' \
	-e 's|@PACKAGE_VERSION[@]|$(PACKAGE_VERSION)|g' \
	-e "s|@LIXA_CONFIG_GLIB2_PYTHON_INCLUDEDIR[@]|$(LIXA_CONFIG_GLIB2_PYTHON_INCLUDEDIR)|g" \
	-e "s|@LIXA_CONFIG_LIBXML2_PYTHON_INCLUDEDIR[@]|$(LIXA_CONFIG_LIBXML2_PYTHON_INCLUDEDIR)|g" \
	-e "s|@MYSQL_INCLUDE_DIR[@]|$(MYSQL_INCLUDE_DIR)|g" \
	-e "s|@POSTGRESQL_INCLUDE_DIR[@]|$(POSTGRESQL_INCLUDE_DIR)|g" \
	$< >$@

# Build Python module extension
all-local:	xta_wrap.cpp setup.py
	python setup.py build_ext --inplace

clean-local:
	rm -f xta_wrap.cpp _xta.so xta.py setup.py

# Install Python module extension
install-exec-local:
	python setup.py install --record installed_files.txt

# Uninstall Python module extension
uninstall-local:
	cat installed_files.txt | xargs rm -rfv

