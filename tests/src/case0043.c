#include <stdio.h>
#include <stdlib.h>


#include <lixa_xid.h>



int main(int argc, char *argv[])
{
    XID xid1;

    LIXA_TRACE_INIT;
    if (lixa_xid_deserialize(
            &xid1,
            "11111111-2222-3333-4444-555555555555.66666666-7777-8888-9999-000000000000"))
        return 1;
    if (lixa_xid_deserialize(&xid1, ""))
        return 1;
    if (lixa_xid_deserialize(&xid1, "."))
        return 1;
    if (lixa_xid_deserialize(&xid1, ".."))
        return 1;
    if (lixa_xid_deserialize(&xid1, "4.b.0"))
        return 1;
    if (lixa_xid_deserialize(&xid1, "34.b.0"))
        return 1;
    if (lixa_xid_deserialize(&xid1, "34.b1.0"))
        return 1;
    if (!lixa_xid_deserialize(&xid1, "34.b1.03"))
        return 1;
    if (!lixa_xid_deserialize(&xid1, "345.b1.03"))
        return 1;
    if (lixa_xid_deserialize(&xid1, "45.b17.03"))
        return 1;
    if (lixa_xid_deserialize(&xid1, "45.b1.03d"))
        return 1;
    if (lixa_xid_deserialize(&xid1, "45.17.0g"))
        return 1;
    /* special case: NULL XID */
    if (!lixa_xid_deserialize(&xid1, "-1.."))
        return 1;
    /* LIXA standard XID */
    if (!lixa_xid_deserialize(
            &xid1,
            "1279875137.b15c6c7b1d714876959d5f552d35706a.b6034fd41d4265ac935a4c525e59548b"))
        return 1;
    
    return 0;
}
