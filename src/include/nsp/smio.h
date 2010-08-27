#ifndef NSP_INC_SMIO 
#define NSP_INC_SMIO

/*
 * Copyright (C) 2009-2010 Jean-Philippe Chancelier Enpc/Cermics
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdio.h>   
#include "nsp/sciio.h" 
#include <nsp/objectf.h>
#include "nsp/xdr.h"

#ifdef HAVE_ZLIB
#include <zlib.h>
#endif 

/**
 * nsp_smio:
 * @file : file structure FILE
 * @xdrs:   xdr struture 
 * @flag:      flag for special open (xdr) 
 * @openf:  flags used in fopen 
 * @fname:   file name 
 *
 * used to store informations 
 * for files.
 */

typedef struct _nsp_smio nsp_smio;

struct _nsp_smio {
  XDR  xdrs[1];  /* xdr struture */
  int  flag;     /* flag for special open (xdr) */
  char openf[4]; /* flags used in fopen */
  unsigned int pos,len;   /* position in stream and len of string D */
  nsp_string D; /* string for storing data */
  int ref_count;
};

/**
 * NspSMio:
 * @obj : a #nsp_smio pointer.
 *
 * inherits from #NspObject used to store informations 
 * for files.
 */

/* typedef struct _NspSMio NspSMio; */

typedef struct _NspTypeSMio { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeSMio;

struct _NspSMio {
  /*< private >*/
  NspObject father; 
  NspTypeSMio *type; 
  /*< public >*/
  nsp_smio *obj;
};

extern int nsp_type_smio_id;
extern NspTypeSMio *nsp_type_smio;

NspTypeSMio *new_type_smio(type_mode mode);

NspSMio *nsp_new_smio();

/*
 * Object methods redefined for file 
 */
extern NspSMio *nsp_smio_wcreate(char *name, char *str,int flag,unsigned int len);
extern NspSMio *nsp_smio_rcreate(char *name, char *str,int flag,const char *data,unsigned int len);
extern NspSMio *nsp_smio_copy(NspSMio *H);
extern NspSMio *nsp_smio_full_copy(NspSMio  *self);
extern void nsp_smio_destroy(NspSMio *H);
extern int nsp_smio_info(NspSMio *H, int indent,char *name, int rec_level);
extern int nsp_smio_print(NspSMio *H, int indent,char *name, int rec_level);

/* setting file flags  **/

#define OPEN_MASK 0x000f 
#define XDR_MASK 0x00f0 
#define SWAP_MASK 0x0f00 

#define SWAP_ON(flag) (flag |= (1 << 8) )
#define SWAP_OFF(flag) (flag &= ~SWAP_MASK )
#define XDR_ON(flag)  (flag |= (1 << 4 ) )
#define XDR_OFF(flag)  (flag &= ~XDR_MASK )
#define OPEN_ON(flag)  (flag |= (1  ) )
#define OPEN_OFF(flag)  (flag &= ~OPEN_MASK )

#define IS_OPENED(flag) ( flag & OPEN_MASK )
#define IS_XDR(flag) ( flag & XDR_MASK)
#define USE_SWAP(flag) ( flag & SWAP_MASK) 

#define NULLSMIO (NspSMio *) 0

extern NspSMio *GetSMio (Stack stack, int i);
extern NspSMio *GetSMioCopy (Stack stack, int i);
extern NspSMio *nsp_smio_object(NspObject *O);
extern NspSMio *nsp_smio_ropen(char *mode,int xdr_on,int swap_on, const char *str,unsigned int len );
extern NspSMio *nsp_smio_wopen(char *mode,int xdr_on,int swap_on,unsigned int len);
extern int IsSMioObj (Stack stack, int i);
extern int is_little_endian(void);
extern int nsp_smio_close(NspSMio *F);
extern int nsp_smio_eof(NspSMio *f);
extern int nsp_smio_error(NspSMio *f);
extern int nsp_smio_get(NspSMio *F,void *x,int n,const char *type,int *items_read);
extern int nsp_smio_getc(NspSMio *F);
extern int nsp_smio_ungetc(NspSMio *F,int  c);
extern int nsp_smio_getstr (NspSMio *F, char *start, int n,int *n_read);
extern int nsp_smio_printf_matrix(NspSMio *F,char *format,char *sep,NspMatrix *M,NspSMatrix *S);
extern int nsp_smio_printf_smatrix(NspSMio *F,NspSMatrix *S);
extern int nsp_smio_put(NspSMio *F,void *x,int n, char *type);
extern int nsp_smio_putstr(NspSMio *F,const char *str);
extern int nsp_smio_read_lines(NspSMio *F,NspSMatrix **S,int nlines);
extern int nsp_smio_scanf_smatrix(NspSMio *F,NspSMatrix **S);
extern int nsp_smio_scanf_matrix(NspSMio *F,char *format,NspMatrix **M,int flag,NspSMatrix **S);
extern int nsp_smio_seek(NspSMio *F,long int offset,const char *flag);
extern int nsp_smio_tell(NspSMio *F,long int *offset);
extern void nsp_smio_clearerr(NspSMio *f);
extern int nsp_smio_xdr_save(XDR *xdrs, NspSMio *S);
extern NspSMio *nsp_smio_xdr_load(XDR *xdrs);

#endif 

#ifdef SMio_Private 
static int nsp_init_smio(NspSMio *ob,NspTypeSMio *type);
static int nsp_smio_size(NspSMio *Mat, int flag);
static char *nsp_smio_type_as_string(void);
static char *nsp_smio_type_short_string(NspObject *v);
static int nsp_smio_eq(NspObject *A, NspObject *B);
static int nsp_smio_neq(NspObject *A, NspObject *B);
static NspMethods *nsp_smio_get_methods(void);

#endif 


