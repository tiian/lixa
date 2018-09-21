# Python3 note #

If your system defaults to Python2.x and you want to compile XTA module for
Python3, prepend "PYTHON_VERSION=3" to configure command at build time;
example:

    PYTHON_VERSION=3 ./configure --enable-crash --with-docbook --with-mysql --with-postgresql --with-oracle-include=/opt/oracle/instantclient_12_1/sdk/include/ --with-oracle-lib=/opt/oracle/instantclient_12_1/


