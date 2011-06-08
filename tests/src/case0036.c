#include <stdio.h>
#include <stdlib.h>


#include <lixa_xid.h>



int main(int argc, char *argv[])
{
    XID xid1, xid2;
    lixa_ser_xid_t ser_xid1, ser_xid2;
    int i, j;
    
    xid1.formatID = random();
    for (i=0; i<XIDDATASIZE; ++i) {
        xid1.data[i] = random();
    }
    for (i=1; i<=64; ++i) {
        for (j=1; j<=64; ++j) {
            xid1.gtrid_length = i;
            xid1.bqual_length = j;
            if (!lixa_xid_serialize(&xid1, ser_xid1))
                return 1;
            printf("i=%d,j=%d\txid='%s'\n", i, j, ser_xid1);
            if (!lixa_xid_deserialize(&xid2, ser_xid1))
                return 1;
            if (!lixa_xid_serialize(&xid2, ser_xid2))
                return 1;
            if (lixa_xid_compare(&xid1, &xid2)) {
                printf("xid1='%s' != xid2='%s'\n", ser_xid1, ser_xid2);
                return 1;
            }
        }
    }
    return 0;
}
