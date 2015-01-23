#ifndef NSP_INC_XDR_H
#define NSP_INC_XDR_H

#ifdef WIN32
#include <stdio.h>
#include "rpc/xdr_inc.h"
#ifdef interface 
#undef interface 
#endif 
#else  /* NOT WIN32 */
#include <rpc/types.h>
#include <rpc/xdr.h>
#if defined(HAVE_TIRPC) || defined(__OpenBSD__)
#define xdr_int8_t   xdr_char
#define xdr_uint8_t  xdr_u_char
#define xdr_uint16_t xdr_u_int16_t
#define xdr_uint32_t xdr_u_int32_t
#define xdr_uint64_t xdr_u_int64_t
#endif /* HAVE_TIRPC */
#endif /* WIN32 */
#endif /* NSP_INC_XDR_H */

