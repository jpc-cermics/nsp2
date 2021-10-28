#ifndef _RPC_XDR_INC_H
#define _RPC_XDR_INC_H

#if !defined(__MSC__) && !defined(__MINGW32__)
#include <netinet/in.h> /** jpc : je met netinet/ avant rpc pour eviter un warning */
#endif

#include <nsp/rpc/types.h> 
#include <nsp/rpc/xdr.h>

#endif
