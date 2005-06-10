#ifndef NSP_INC_SCIFILE 
#define NSP_INC_SCIFILE

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/object.h"

/*
 * NspFile inherits from NspObject 
 */

/*** xdr files **/
#ifdef macintosh
#	include "types.h"
#else /* not macintosh */
#       ifndef VMS
#   	include <sys/types.h>	/* for <netinet/in.h> on some systems */
#   	if !defined(__MSC__) && ! defined(__MINGW32__)
#          include <netinet/in.h>	/* for htonl() */
#   	endif
#	endif
#endif /* not macintosh */

#ifdef WIN32 
#include "../xdr/rpc/types.h"
#include "../xdr/rpc/xdr.h"
#ifdef interface 
#undef interface 
#endif 
#else 
#include <rpc/types.h>
#include <rpc/xdr.h>
#endif

typedef struct _NspFile NspFile;

typedef struct _NspTypeSciFile { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeSciFile;

struct _NspFile {
  /*< private >*/
  NspObject father; 
  NspTypeSciFile *type; 
  /*< public >*/
  FILE *file ;   /* the file */
  XDR  xdrs[1];  /* xdr struture */
  int  flag;     /* flag for special open (xdr) */
  char openf[4]; /* flags used in fopen */
  char *fname;  /* file name */
};

extern int nsp_type_file_id;
extern NspTypeSciFile *nsp_type_file;

NspTypeSciFile *new_type_file(type_mode mode);

NspFile *nsp_new_file();

/*
 * Object methods redefined for file 
 */

#ifdef SciFile_Private 
static int nsp_init_file(NspFile *ob,NspTypeSciFile *type);
static int nsp_file_size(NspFile *Mat, int flag);
static char *nsp_file_type_as_string(void);
static char *nsp_file_type_short_string(void);
static int nsp_file_eq(NspObject *A, NspObject *B);
static int nsp_file_neq(NspObject *A, NspObject *B);
static NspMethods *nsp_file_get_methods(void);
#endif 

NspFile *nsp_file_create(char *name, char *fname, char *str, int flag, FILE *f);
NspFile *nsp_file_copy(NspFile *H);
void nsp_file_destroy(NspFile *H);
void nsp_file_info(NspFile *H, int indent);
void nsp_file_print(NspFile *H, int indent);

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

#define NULLSCIFILE (NspFile *) 0

/* FileObj.c */

extern NspFile *nsp_file_object(NspObject *O);
extern int IsSciFileObj (Stack stack, int i);
extern NspFile *GetSciFileCopy (Stack stack, int i);
extern NspFile *GetSciFile (Stack stack, int i);

/* File.c */
NspFile *nsp_file_open(char *fname, char *mode,int xdr_on,int swap_on);
extern int nsp_file_close(NspFile *F);
extern NspFile *nsp_file_open_xdr_r(char *fname);
extern int nsp_file_close_xdr_r(NspFile *F);
extern NspFile *nsp_file_open_xdr_w(char *fname);
extern int nsp_file_close_xdr_w(NspFile *F);
extern int nsp_xdr_save_d(XDR *xdrs, double x);
extern int nsp_xdr_load_d(XDR *xdrs, double *x);
extern int nsp_xdr_save_i(XDR *xdrs, int ix);
extern int nsp_xdr_load_i(XDR *xdrs, int *ix);
extern int nsp_xdr_save_c(XDR *xdrs, char c);
extern int nsp_xdr_load_c(XDR *xdrs, char *c);
extern int nsp_xdr_save_array_i(XDR *xdrs, int *nx, int l);
extern int nsp_xdr_load_array_i(XDR *xdrs, int *nx, int l);
extern int nsp_xdr_save_array_d(XDR *xdrs, double *nx, int l);
extern int nsp_xdr_load_array_d(XDR *xdrs, double *nx, int mn);
extern int nsp_xdr_save_string(XDR *xdrs, char *str);
extern int nsp_xdr_load_string(XDR *xdrs, char *buf, int buf_len);

extern int is_little_endian(void);
extern int nsp_feof(NspFile *f);
extern int nsp_ferror(NspFile *f);
extern void nsp_clearerr(NspFile *f);
extern int nsp_fseek(NspFile *F,long int offset,const char *flag);
extern int nsp_ftell(NspFile *F,long int *offset);
extern int nsp_mput(NspFile *F,void *x,int n, char *type);
extern int nsp_mget(NspFile *F,void *x,int n,const char *type, int *items_read);
extern int nsp_mgetstr(NspFile *F, char **start, int n);
extern int nsp_mgetstr1 (NspFile *F, char *start, int n, int *n_read);
extern int nsp_putstr(NspFile *F, char *str);

extern int do_printf (char *fname,FILE * fp, char *format,Stack stack,int n_args,
	       int arg_cnt,int line, char **strv);

extern int do_scanf (char *command,FILE *fp,
		     char *format,Stack stack,int iline, int *nargs, 
		     char *strv,int *retval);




#endif 
