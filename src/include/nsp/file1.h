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
static int init_scifile(NspFile *ob,NspTypeSciFile *type);
static int SciFileSize(NspFile *Mat, int flag);
char *SciFileType(void);
char *SciFileShType(NspFile *M);
NspObject *SciFileLoopExtract(char *str, NspObject *O, NspObject *O1, int i, int *rep);
int SciFileObjEq(NspObject *A,NspObject *B);
int SciFileObjNeq(NspObject *A,NspObject *B);
#endif 


/** flag can be ... **/


/*********************************************
 * Opens file given by file and return it's id 
 * in fd. 
 * status can be "r","w","a" or "rb","wb","ab"
 *********************************************/

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

 extern NspFile *SciFileObj (NspObject *O);
 extern int IsSciFileObj (Stack stack, int i);
 extern NspFile *GetSciFileCopy (Stack stack, int i);
 extern NspFile *GetSciFile (Stack stack, int i);

/* File.c */

 extern NspFile *SciFileCreate (char *name, char *fname, char *str, integer flag, FILE *f);
 extern NspFile *SciFileCopy (NspFile *A);
 extern void SciFileDestroy (NspFile *F);
 extern void SciFileInfo (NspFile *F, int indent);
 extern void SciFilePrint (NspFile *F, int indent);
 extern NspFile *SciFileOpen (char *fname, char *openf);
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


#endif 
