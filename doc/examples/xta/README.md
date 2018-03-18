# XTA example Application Programs #

This directory contains examples related to the usage of XTA: XA Transactional
API.

All the examples require the availability of LIXA state server: a lixad daemon
must be up and running.

See [LIXA Reference Guide Manual](http://www.tiian.org/lixa/manuals/html/index.html) for detailed instructions.

## "Single Application" Pattern ##

Program *example_xta_sa01.c* requires MySQL (or MariaDB) and PostgreSQL to work.
After proper installation of LIXA, you can compile the program with the
following commands:

. /opt/lixa/bin/lixa_env.sh

gcc example_xta_sa01.c $(lixa-config -x -c -f -l -d) -lpq $(mysql_config --libs_r) -o example_xta_sa01

