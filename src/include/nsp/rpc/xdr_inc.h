#ifndef _RPC_XDR_INC_H
#define _RPC_XDR_INC_H

#ifdef __MINGW32__
#ifndef __MSC__
#define __MSC__
#endif
#endif 

#ifndef __MSC__
#include <netinet/in.h> /** jpc : je met netinet/ avant rpc pour eviter un warning */
#include <rpc/types.h> 
#include <rpc/xdr.h>
#else
#include "types.h" 
#include "xdr.h"
#endif

#endif
