/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
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
 */


/*
 * xdr_mem.h, XDR implementation using memory buffers.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 *
 * If you have some data to be interpreted as external data representation
 * or to be converted to external data representation in a memory buffer,
 * then this is the package for you.
 *
 */

#include <string.h>

#ifdef __MINGW32__
#define __MSC__
#endif 

#ifndef __MSC__
#include <netinet/in.h> /** jpc : je met netinet/ avant rpc pour eviter un warning */
#include <rpc/types.h> 
#include <rpc/xdr.h>
#else
#include "rpc/types.h" 
#include "rpc/xdr.h"
#endif


static bool_t xdrmem_getlong (XDR *, long *);
static bool_t xdrmem_putlong (XDR *, const long *);
static bool_t xdrmem_getbytes (XDR *, caddr_t, u_int);
static bool_t xdrmem_putbytes (XDR *, const char *, u_int);
static u_int xdrmem_getpos (const XDR *);
static bool_t xdrmem_setpos (XDR *, u_int);
static int32_t *xdrmem_inline (XDR *, u_int);
static void xdrmem_destroy (XDR *);
static bool_t xdrmem_getint32 (XDR *, int32_t *);
static bool_t xdrmem_putint32 (XDR *, const int32_t *);

static const struct xdr_ops xdrmem_ops =
{
  xdrmem_getlong,
  xdrmem_putlong,
  xdrmem_getbytes,
  xdrmem_putbytes,
  xdrmem_getpos,
  xdrmem_setpos,
  xdrmem_inline,
  xdrmem_destroy,
  xdrmem_getint32,
  xdrmem_putint32
};

/*
 * The procedure xdrmem_create initializes a stream descriptor for a
 * memory buffer.
 */
void
xdrmem_create(register XDR *xdrs, caddr_t addr, u_int size, enum xdr_op op)
{

  xdrs->x_op = op;
  xdrs->x_ops = (struct xdr_ops *) &xdrmem_ops;
  xdrs->x_private = xdrs->x_base = addr;
  xdrs->x_handy = size;
}

static void
xdrmem_destroy(XDR *xdrs)
{
}

static bool_t
xdrmem_getlong(register XDR *xdrs, long int *lp)
{

  if ((xdrs->x_handy -= sizeof(long)) < 0)
    return (FALSE);
  *lp = (long)ntohl((u_long)(*((long *)(xdrs->x_private))));
  xdrs->x_private += sizeof(long);
  return (TRUE);
}

static bool_t
xdrmem_putlong(register XDR *xdrs,const long int *lp)
{

  if ((xdrs->x_handy -= sizeof(long)) < 0)
    return (FALSE);
  *(long *)xdrs->x_private = (long)htonl((u_long)(*lp));
  xdrs->x_private += sizeof(long);
  return (TRUE);
}

static bool_t
xdrmem_getbytes(register XDR *xdrs, caddr_t addr, register u_int len)
{

  if ((xdrs->x_handy -= len) < 0)
    return (FALSE);
  bcopy(xdrs->x_private, addr, len);
  xdrs->x_private += len;
  return (TRUE);
}


static bool_t
xdrmem_putbytes (XDR *xdrs, const char *addr, u_int len)
{
  if (xdrs->x_handy < len)
    return FALSE;
  xdrs->x_handy -= len;
  memcpy (xdrs->x_private, addr, len);
  xdrs->x_private += len;
  return TRUE;
}


static u_int
xdrmem_getpos(const XDR *xdrs)
{

  return ((u_int)xdrs->x_private - (u_int)xdrs->x_base);
}

static bool_t
xdrmem_setpos(register XDR *xdrs, u_int pos)
{
  register caddr_t newaddr = xdrs->x_base + pos;
  register caddr_t lastaddr = xdrs->x_private + xdrs->x_handy;

  if ((long)newaddr > (long)lastaddr)
    return (FALSE);
  xdrs->x_private = newaddr;
  xdrs->x_handy = (int)lastaddr - (int)newaddr;
  return (TRUE);
}


static int32_t *
xdrmem_inline (XDR *xdrs, u_int len)
{
  int32_t *buf = 0;

  if (xdrs->x_handy >= len)
    {
      xdrs->x_handy -= len;
      buf = (int32_t *) xdrs->x_private;
      xdrs->x_private += len;
    }
  return buf;
}
/*
 * Gets the next word from the memory referenced by xdrs and places it
 * in the int pointed to by ip.  It then increments the private word to
 * point at the next element.  Neither object pointed to is const
 */
static bool_t
xdrmem_getint32 (XDR *xdrs, int32_t *ip)
{
  if (xdrs->x_handy < 4)
    return FALSE;
  xdrs->x_handy -= 4;
  *ip = ntohl ((*((int32_t *) (xdrs->x_private))));
  xdrs->x_private += 4;
  return TRUE;
}

/*
 * Puts the long pointed to by lp in the memory referenced by xdrs.  It
 * then increments the private word to point at the next element.  The
 * long pointed at is const
 */
static bool_t
xdrmem_putint32 (XDR *xdrs, const int32_t *ip)
{
  if (xdrs->x_handy < 4)
    return FALSE;
  xdrs->x_handy -= 4;
  *(int32_t *) xdrs->x_private = htonl (*ip);
  xdrs->x_private += 4;
  return TRUE;
}
