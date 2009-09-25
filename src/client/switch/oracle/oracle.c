#include "xa.h"


extern struct xa_switch_t xaosw;


struct xa_switch_t *get_switch()
{
    return &xaosw;
}

