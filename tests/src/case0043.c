#include <stdio.h>
#include <stdlib.h>


#include <lixa_xid.h>



int main(int argc, char *argv[])
{
    XID xid1;
    
    if (!lixa_xid_deserialize(
            &xid1,
            "11111111-2222-3333-4444-555555555555.66666666-7777-8888-9999-000000000000"))
        return 1;
    
    return 0;
}
