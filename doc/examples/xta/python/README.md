# XTA example Application Programs for Python language #

This directory contains examples related to the usage of XTA: XA Transactional
API.

All the examples require the availability of LIXA state server: a lixad daemon
must be up and running.

See [LIXA Reference Guide Manual](http://www.tiian.org/lixa/manuals/html/index.html), chapter "Developing Application Programs using XTA (XA Transaction API) interface", for detailed instructions.

For PostgreSQL, use driver psycopg2 version 2.8 or higher.
For MySQL, use driver mysqlclient-python with pull request
"get_native_connection":

* https://github.com/PyMySQL/mysqlclient-python/pull/269
* https://github.com/tiian/mysqlclient-python/tree/get_native_connection


## "Single Application" Pattern ##

Program *example_xta_sa11.py* requires MySQL (or MariaDB) and PostgreSQL to
work.

PostgreSQL requires Psycopg 2.8 or above

## "Multiple Applications, Concurrent Branches/Pseudo Synchronous" Pattern ##

Program *example_xta_macbps21.py* requires MySQL (or MariaDB) to work.
Program *example_xta_macbps22.py* requires PostgreSQL to work.

### Work in progress, copied from C++, to be documented for Python ###



## "Multiple Applications, Concurrent Branches/Pseudo Asynchronous" Pattern ##

Program *example_xta_macbpa11.cpp* requires MySQL (or MariaDB) to work.
Program *example_xta_macbpa12.cpp* requires PostgreSQL to work.
After proper installation of LIXA, you can compile the programs with the
following commands:

`. /opt/lixa/bin/lixa_env.sh`

`g++ example_xta_macbpa11.cpp $(lixa-config -c -f -l -d --xta --language-cpp) $(mysql_config --libs_r) -o example_xta_macbpa11`

`g++ example_xta_macbpa12.cpp $(lixa-config -c -f -l -d --xta --language-cpp) -lpq -o example_xta_macbpa12`

