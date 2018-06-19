# C++ API for XTA

This directory contains the code necessary to create the C++ API for XTA: it's
basically a wrapper around the C API for XTA.
It's not an header only wrapper because some tools require real C++ functions
(and function pointers) to create further wrappers around C++ API.
