dist_noinst_DATA = xta.i

# Produce XTA wrapper using SWIG (C++)

xta_wrap.cxx: xta.i ../cpp/Xta.hpp
	$(SWIG) -c++ -python -I../cpp
