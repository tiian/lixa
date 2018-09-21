#!/usr/bin/env @PYTHON@

"""
setup.py file for SWIG generated module
"""

from distutils.core import setup, Extension

xta_module = Extension('_@_XTA_PREFIX@',
        include_dirs=['..', '../cpp',
                 '@abs_top_builddir@/src/common',
                 '@abs_top_builddir@/src/client',
                 '@LIXA_CONFIG_GLIB2_PYTHON_INCLUDEDIR@',
                 '@LIXA_CONFIG_LIBXML2_PYTHON_INCLUDEDIR@',
                 '@MYSQL_INCLUDE_DIR@',
                 '@POSTGRESQL_INCLUDE_DIR@'],
        sources=['xta_wrap.cpp'],
        library_dirs=['@abs_top_builddir@/src/xta/cpp/.libs'],
        runtime_library_dirs=['@libdir@'],
        libraries=['lixtapp']
        )

setup (name = '@_XTA_PREFIX@',
        version = '@PACKAGE_VERSION@',
        author = 'Christian Ferrari',
        author_email = '@PACKAGE_BUGREPORT@',
        description = """@PACKAGE_NAME@ python module""",
        ext_modules = [@_XTA_PREFIX@_module],
        py_modules = ['@_XTA_PREFIX@'],
        )

