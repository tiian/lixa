#include "xa.h"



/*
 * This is Oracle specific: the xa_switch_t struct supplied is named xaosw
 */
extern struct xa_switch_t xaosw;



/*
 * The function is exported and dynamically retrieved afted the module was
 * fetched
 */
struct xa_switch_t *lixa_get_xa_switch()
{
    return &xaosw;
}
