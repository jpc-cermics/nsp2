#ifndef INC_NSP_SCIFILE 
#define INC_NSP_SCIFILE

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

#include <stdio.h>   /** for file declaration **/
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
#   	ifndef __MSC__ 
#          include <netinet/in.h>	/* for htonl() */
#   	endif
#	endif
#endif /* not macintosh */

#ifdef WIN32 
#include "../xdr/rpc/types.h"
#include "../xdr/rpc/xdr.h"
#else 
#include <rpc/types.h>
#include <rpc/xdr.h>
#endif

typedef struct _nsp_scifile NspFile;

typedef int (*scifile_save) (NspFile  *F, NspFile *M);

typedef struct _nsp_type_SciFile { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
  scifile_save *save;
} NspTypeSciFile;

struct _nsp_scifile {
  NspObject father; 
  NspTypeSciFile *type; 
  FILE *file ;   /* the file */
  XDR  xdrs[1];  /* xdr struture */
  int  flag;     /* flag for special open (xdr) */
  char openf[4]; /* flags used in fopen */
  char *fname;  /* file name */
};

extern int nsp_type_scifile_id;
extern NspTypeSciFile *nsp_type_scifile;

NspTypeSciFile *new_type_scifile(type_mode mode);

NspFile *new_scifile();

/*
 * Object methods redefined for scifile 
 */

#ifdef SciFile_Private 
static int init_file(NspFile *ob,NspTypeSciFile *type);
static int file_size(NspFile *Mat, int flag);
static char *file_type_as_string(void);
static char *file_type_short_string(void);
static int file_eq(NspObject *A, NspObject *B);
static int file_neq(NspObject *A, NspObject *B);
static NspMethods *file_get_methods(void);
#endif 

NspFile *file_create(char *name, char *fname, char *str, integer flag, FILE *f);
NspFile *file_copy(NspFile *H);
void file_destroy(NspFile *H);
void file_info(NspFile *H, int indent);
void file_print(NspFile *H, int indent);

/** setting file flags  **/


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

/* Obj.c */

extern int nsp_object_xdr_save(NspFile  *F, NspObject *O);

/* FileObj.c */

extern NspFile *file_object (NspObject *O);
extern int IsSciFileObj (Stack stack, int i);
extern NspFile *GetSciFileCopy (Stack stack, int i);
extern NspFile *GetSciFile (Stack stack, int i);

/* File.c */
NspFile *SciFileOpen(char *fname, char *mode,int xdr_on,int swap_on);
extern int SciFileClose (NspFile *F);
extern NspFile *SciFileOpenXdrR (char *fname);
extern int SciFileCloseXdrR (NspFile *F);
extern NspFile *SciFileOpenXdrW (char *fname);
extern int SciFileCloseXdrW (NspFile *F);
extern int XdrSaveD (NspFile *F, double x);
extern int XdrLoadD (NspFile *F, double *x);
extern int XdrSaveI (NspFile *F, integer ix);
extern int XdrLoadI (NspFile *F, integer *ix);
extern int XdrSaveC (NspFile *F, char c);
extern int XdrLoadC (NspFile *F, char *c);
extern int XdrSaveArrayI (NspFile *F, int *nx, int l);
extern int XdrLoadArrayI (NspFile *F, int *nx, int l);
extern int XdrSaveArrayD (NspFile *F, double *nx, integer l);
extern int XdrLoadArrayD (NspFile *F, double *nx, integer mn);
extern int XdrSaveString (NspFile *F, char *str);
extern int XdrLoadString (NspFile *F, char *buf, int buf_len);

extern int nsp_object_xdr_save(NspFile *F, NspObject *O);
extern NspObject *nsp_object_xdr_load(NspFile *F); 

extern int is_little_endian(void);
extern int nsp_feof(NspFile *f);
extern int nsp_ferror(NspFile *f);
extern void nsp_clearerr(NspFile *f);
extern int nsp_fseek(NspFile *F,long int offset,const char *flag);
extern int nsp_ftell(NspFile *F,long int *offset);
extern int nsp_mput(NspFile *F,void *x,int n, char *type);
extern int nsp_mget(NspFile *F,void *x,int n,const char *type, int *items_read);
extern int nsp_mgetstr(NspFile *F, char **start, integer n);
extern int nsp_mgetstr1 (NspFile *F, char *start, int n, int *n_read);
extern int nsp_putstr(NspFile *F, char *str);

extern int do_printf (char *fname,FILE * fp, char *format,Stack stack,int n_args,
	       int arg_cnt,int line, char **strv);
extern int do_scanf (char *fname,FILE *fp, char *format,Stack stack,int iline, int *nargs, 
			     char *strv,int *retval);




#endif 
