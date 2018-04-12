# XTA example Application Programs #

This directory contains examples related to the usage of XTA: XA Transactional
API.

All the examples require the availability of LIXA state server: a lixad daemon
must be up and running.

See [LIXA Reference Guide Manual](http://www.tiian.org/lixa/manuals/html/index.html), chapter "Developing Application Programs using XTA (XA Transaction API) interface", for detailed instructions.

## "Single Application" Pattern ##

Program *example_xta_sa01.c* requires MySQL (or MariaDB) and PostgreSQL to work.
After proper installation of LIXA, you can compile the program with the
following commands:

`. /opt/lixa/bin/lixa_env.sh`

`gcc example_xta_sa01.c $(lixa-config -x -c -f -l -d) -lpq $(mysql_config --libs_r) -o example_xta_sa01`

## "Multiple Applications, Consecutive Calls" Pattern ##

Program *example_xta_macc01.c* requires Oracle Client to work.
After proper installation of LIXA, you can compile the program with the
following commands:

`. /opt/lixa/bin/lixa_env.sh`

`gcc example_xta_macc01.c $(lixa-config -x -c -f -l -d) -I/opt/oracle/instantclient_12_1/sdk/include -L/opt/oracle/instantclient_12_1 -Wl,-rpath -Wl,/opt/oracle/instantclient_12_1 -l clntsh -l nnz12 -o example_xta_macc01`

## "Multiple Applications, Concurrent Branches/Pseudo Synchronous" Pattern ##

Program *example_xta_macbps01.c* requires MySQL (or MariaDB) to work.
Program *example_xta_macbps02.c* requires PostgreSQL to work.
After proper installation of LIXA, you can compile the programs with the
following commands:

`. /opt/lixa/bin/lixa_env.sh`

`gcc example_xta_macbps01.c $(lixa-config -x -c -f -l -d) $(mysql_config --libs_r) -o example_xta_macbps01`

`gcc example_xta_macbps02.c $(lixa-config -x -c -f -l -d) -lpq -o example_xta_macbps02`

## "Multiple Applications, Concurrent Branches/Pseudo Asynchronous" Pattern ##

Program *example_xta_macbpa01.c* requires MySQL (or MariaDB) to work.
Program *example_xta_macbpa02.c* requires PostgreSQL to work.
After proper installation of LIXA, you can compile the programs with the
following commands:

`. /opt/lixa/bin/lixa_env.sh`

`gcc example_xta_macbpa01.c $(lixa-config -x -c -f -l -d) $(mysql_config --libs_r) -o example_xta_macbpa01`

`gcc example_xta_macbpa02.c $(lixa-config -x -c -f -l -d) -lpq -o example_xta_macbpa02`


