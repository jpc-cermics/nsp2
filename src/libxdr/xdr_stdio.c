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
 * xdr_stdio.c, XDR implementation on standard i/o file.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 *
 * This set of routines implements a XDR on a stdio stream.
 * XDR_ENCODE serializes onto the stream, XDR_DECODE de-serializes
 * from the stream.
 */
#include <stdio.h>
#include <nsp/rpc/types.h>
#include <nsp/rpc/xdr.h>
#include "nspxdr.h"

static bool_t xdrstdio_getlong (XDR *, long *);
static bool_t xdrstdio_putlong (XDR *, const long *);
static bool_t xdrstdio_getbytes (XDR *, caddr_t, u_int);
static bool_t xdrstdio_putbytes (XDR *, const char *, u_int);
static u_int xdrstdio_getpos (const XDR *);
static bool_t xdrstdio_setpos (XDR *, u_int);
static int32_t *xdrstdio_inline (XDR *, u_int);
static void xdrstdio_destroy (XDR *);
static bool_t xdrstdio_getint32 (XDR *, int32_t *);
static bool_t xdrstdio_putint32 (XDR *, const int32_t *);

/*
 * Ops vector for stdio type XDR
 */
static struct xdr_ops	xdrstdio_ops = {
  xdrstdio_getlong,		/* deserialize a long int */
  xdrstdio_putlong,		/* serialize a long int */
  xdrstdio_getbytes,		/* deserialize counted bytes */
  xdrstdio_putbytes,		/* serialize counted bytes */
  xdrstdio_getpos,		/* get offset in the stream */
  xdrstdio_setpos,		/* set offset in the stream */
  xdrstdio_inline,		/* prime stream for inline macros */
  xdrstdio_destroy,		/* destroy stream */
  xdrstdio_getint32,		/* deserialize a int */
  xdrstdio_putint32		/* serialize a int */
};

/*
 * Initialize a stdio xdr stream.
 * Sets the xdr stream handle xdrs for use on the stream file.
 * Operation flag is set to op.
 */
void
xdrstdio_create(register XDR *xdrs, FILE *file, enum xdr_op op)
{

  xdrs->x_op = op;
  xdrs->x_ops = &xdrstdio_ops;
  xdrs->x_private = (caddr_t)file;
  xdrs->x_handy = 0;
  xdrs->x_base = 0;
}

/*
 * Destroy a stdio xdr stream.
 * Cleans up the xdr stream handle xdrs previously set up by xdrstdio_create.
 */
static void
xdrstdio_destroy(register XDR *xdrs)
{
  (void)fflush((FILE *)xdrs->x_private);
  /* xx should we close the file ?? */
};

static bool_t
xdrstdio_getlong(XDR *xdrs, register long int *lp)
{

  if (fread((caddr_t)lp, sizeof(long), 1, (FILE *)xdrs->x_private) != 1)
    return (FALSE);
#ifndef mc68000
  *lp = ntohl(*lp);
#endif
  return (TRUE);
}

static bool_t
xdrstdio_putlong(XDR *xdrs, const long int *lp)
{

#ifndef mc68000
  long mycopy = htonl(*lp);
  lp = &mycopy;
#endif
  if (fwrite((caddr_t)lp, sizeof(long), 1, (FILE *)xdrs->x_private) != 1)
    return (FALSE);
  return (TRUE);
}

static bool_t
xdrstdio_getbytes(XDR *xdrs, caddr_t addr, u_int len)
{

  if ((len != 0) && (fread(addr, (int)len, 1, (FILE *)xdrs->x_private) != 1))
    return (FALSE);
  return (TRUE);
}

static bool_t
xdrstdio_putbytes(XDR *xdrs, const char * addr, u_int len)
{

  if ((len != 0) && (fwrite(addr, (int)len, 1, (FILE *)xdrs->x_private) != 1))
    return (FALSE);
  return (TRUE);
}

static u_int
xdrstdio_getpos(const XDR *xdrs)
{

  return ((u_int) ftell((FILE *)xdrs->x_private));
}

static bool_t
xdrstdio_setpos(XDR *xdrs, u_int pos)
{

  return ((fseek((FILE *)xdrs->x_private, (long)pos, 0) < 0) ?
	  FALSE : TRUE);
}

static int32_t *
xdrstdio_inline(XDR *xdrs, u_int len)
{

  /*
   * Must do some work to implement this: must insure
   * enough data in the underlying stdio buffer,
   * that the buffer is aligned so that we can indirect through a
   * long *, and stuff this pointer in xdrs->x_buf.  Doing
   * a fread or fwrite to a scratch buffer would defeat
   * most of the gains to be had here and require storage
   * management on this buffer, so we don't do this.
   */
  return (NULL);
}


static bool_t
xdrstdio_getint32 (XDR *xdrs, int32_t *ip)
{
  int32_t mycopy;

  if (fread ((caddr_t) &mycopy, 4, 1, (FILE *) xdrs->x_private) != 1)
    return FALSE;
  *ip = ntohl (mycopy);
  return TRUE;
}

static bool_t
xdrstdio_putint32 (XDR *xdrs, const int32_t *ip)
{
  int32_t mycopy = htonl (*ip);

  ip = &mycopy;
  if (fwrite ((caddr_t) ip, 4, 1, (FILE *) xdrs->x_private) != 1)
    return FALSE;
  return TRUE;
}
