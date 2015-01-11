/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 *
 */

/*
 * Rpc additions to <sys/types.h>
 */
#ifndef _RPC_TYPES_H
#define _RPC_TYPES_H

#define	bool_t	int
#define	enum_t	int
#define __dontcare__	-1

#ifndef FALSE
#	define FALSE	(0)
#endif
#ifndef TRUE
#	define TRUE	(1)
#endif
#ifndef NULL
#	define NULL	0
#endif

#define mem_alloc(bsize)	malloc(bsize)
#define mem_free(ptr, bsize)	free(ptr)

#ifndef makedev /* ie, we haven't already included it */
#include <sys/types.h>
#ifdef __MSC__ 
/* #undef FALSE */
/* #undef TRUE */
#include <winsock.h>
typedef char * caddr_t;
#define bzero(x,n) memset(x,0,n)
#define bcopy(x,y,n) memcpy(x,y,n)
#define IEEEFP
#endif /* __MSC__ */

#endif /* makedev */

#ifndef __MSC__
#include <sys/time.h>
#endif 

#ifndef INADDR_LOOPBACK
#define	INADDR_LOOPBACK		(u_long)0x7F000001
#endif


/* for MINGWIN32   
 * replaced by winsock.h
 #ifndef PASCAL 
 #define PASCAL      __stdcall
 #endif
 u_long PASCAL htonl (u_long hostlong);
 u_long PASCAL ntohl (u_long netlong);
 */

#ifdef __MINGW32__
#include <winsock.h>
#endif 

#endif /* !_RPC_TYPES_H */
