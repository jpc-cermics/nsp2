#ifndef NSP_INC_XDR_H
#define NSP_INC_XDR_H

#ifdef WIN32 
#include "../../libxdr/rpc/xdr_inc.h"
#ifdef interface 
#undef interface 
#endif 
#else 
#include <rpc/types.h>
#include <rpc/xdr.h>
#endif

#endif
