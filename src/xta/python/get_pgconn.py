# This hack has been documented here
# https://gist.github.com/dvarrazzo/b7c8f050bbd39dd2c104
def get_pgconn(conn):
        """
        Return the address of the libpq connection string from a
        psycopg connection
        """
        from ctypes import string_at
        from sys import getsizeof
        from socket import ntohl, htonl
        from binascii import hexlify

        hver = "%08x" % ntohl(conn.server_version)
        mem = hexlify(string_at(id(conn), getsizeof(conn)))
        ver_off = mem.find(hver)
        assert ver_off > 0
        assert mem.find(hver, ver_off + 8) == -1, "there should be only one"
        pgconn = htonl(int(mem[ver_off + 8:ver_off + 16], 16))
#       print "pgconn= ", pgconn
        return pgconn

