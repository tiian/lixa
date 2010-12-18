#!/bin/sh

echo "Checking testsuite.log file produced with last 'make check' command"

LIST=/tmp/parse_check_output.$$.txt
AWKPGM=/tmp/parse_check_output.awk

find . -name testsuite.log -o -name lixad.trace | sort > $LIST

cat > $AWKPGM <<EOL
/==[0-9]+==.*definitely lost: / { if ($ 4 > 0) print }
/==[0-9]+==.*indirectly lost: / { if ($ 4 > 0) print }
/==[0-9]+==.*possibly lost: / { if ($ 4 > 0) print }
/==[0-9]+==.*still reachable: / { if ($ 4 > 0) print }
/==[0-9]+==.*Possible data race/ { print }
EOL

cat $LIST | while read f
do
	echo "Analyzing file $f:"
	awk -f $AWKPGM $f
done

