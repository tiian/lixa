# XTA example Application Programs for Python language #

This directory contains examples related to the usage of XTA: XA Transactional
API.

All the examples require the availability of LIXA state server: a lixad daemon
must be up and running.

See [LIXA Reference Guide Manual](http://www.tiian.org/lixa/manuals/html/index.html), chapter "Developing Application Programs using XTA (XA Transaction API) interface", for detailed instructions.

For PostgreSQL, use driver psycopg2 at git commit
81addddaee2c690e925bb8f381e7bcb02ca97687
https://github.com/fogzot/psycopg2/commit/81addddaee2c690e925bb8f381e7bcb02ca97687
and checkout branch "feature-expose-pgconn".
The feature should be merged in Psycopg2 2.8 official version

For MySQL/MariaDB, use driver mysqlclient-python at git commit
54c69436f45f2c78b3ace754780a1a265e60d430
https://github.com/PyMySQL/mysqlclient-python/commit/54c69436f45f2c78b3ace754780a1a265e60d430
or later

## "Single Application" Pattern ##

Program *example_xta_sa11.py* requires MySQL (or MariaDB) and PostgreSQL to
work.

PostgreSQL requires Psycopg 2.8 or above

## "Multiple Applications, Concurrent Branches/Pseudo Synchronous" Pattern ##

Program *example_xta_macbps21.py* requires MySQL (or MariaDB) to work.

Program *example_xta_macbps22.py* requires PostgreSQL to work.

## "Multiple Applications, Concurrent Branches/Pseudo Asynchronous" Pattern ##

Program *example_xta_macbpa21.py* requires MySQL (or MariaDB) to work.

Program *example_xta_macbpa22.py* requires PostgreSQL to work.

