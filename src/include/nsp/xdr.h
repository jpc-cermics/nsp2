#ifndef NSP_INC_XDR_H
#define NSP_INC_XDR_H

#ifdef WIN32
#include "rpc/xdr_inc.h"
#ifdef interface 
#undef interface 
#endif 
#else  /* WIN32 */
#include <rpc/types.h>
#include <rpc/xdr.h>
#ifdef HAVE_TIRPC
#define xdr_int8_t   xdr_char
#define xdr_uint8_t  xdr_u_char
#define xdr_uint16_t xdr_u_int16_t
#define xdr_uint32_t xdr_u_int32_t
#define xdr_uint64_t xdr_u_int64_t
#endif 
#endif
#endif /* WIN32 */
