#ifndef NSP_INC_XDR_H
#define NSP_INC_XDR_H

#include <stdio.h>

#if !defined(__MSC__) && !defined(__MINGW32__)
#include <netinet/in.h> 
#endif

#include <nsp/rpc/types.h>
#include <nsp/rpc/xdr.h>

#ifdef WIN32
#ifdef interface 
#undef interface 
#endif 
#else  /* NOT WIN32 */
#if defined(HAVE_TIRPC) || defined(__OpenBSD__)
#define xdr_int8_t   xdr_char
#define xdr_uint8_t  xdr_u_char
#define xdr_uint16_t xdr_u_int16_t
#define xdr_uint32_t xdr_u_int32_t
#define xdr_uint64_t xdr_u_int64_t
#endif /* HAVE_TIRPC */
#endif /* WIN32 */
#endif /* NSP_INC_XDR_H */

