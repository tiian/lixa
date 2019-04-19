
EXTRA_DIST = case3100.py.in \
	case3104.py.in case3105.py.in \
	case3106.py.in case3107.py.in case3110.py.in

case3100.py:	case3100.py.in
	sed -e 's|@PYTHON[@]|$(PYTHON)|g' $< >$@
	chmod a+x $@
case3104.py:	case3104.py.in
	sed -e 's|@PYTHON[@]|$(PYTHON)|g' $< >$@
	chmod a+x $@
case3105.py:	case3105.py.in
	sed -e 's|@PYTHON[@]|$(PYTHON)|g' $< >$@
	chmod a+x $@
case3106.py:	case3106.py.in
	sed -e 's|@PYTHON[@]|$(PYTHON)|g' $< >$@
	chmod a+x $@
case3107.py:	case3107.py.in
	sed -e 's|@PYTHON[@]|$(PYTHON)|g' $< >$@
	chmod a+x $@
case3110.py:	case3110.py.in
	sed -e 's|@PYTHON[@]|$(PYTHON)|g' $< >$@
	chmod a+x $@

check-local: case3100.py case3104.py case3105.py case3106.py case3107.py \
	case3110.py
