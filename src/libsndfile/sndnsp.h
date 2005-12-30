#ifndef NSP_INC_SNDFILE 
#define NSP_INC_SNDFILE

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/object.h"
#include "nsp/xdr.h"

/*
 *
 */

typedef struct _nspsndfile nspsndfile;

struct _nspsndfile {
  SF_INFO sfinfo; 
  SNDFILE *sf;
  int mode;
  nsp_string fname;
  int refcount;
};

/*
 * NspSndFile inherits from NspObject 
 */

typedef struct _NspSndFile NspSndFile;

typedef struct _NspTypeSndFile { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypeSndFile;

struct _NspSndFile {
  /*< private >*/
  NspObject father; 
  NspTypeSndFile *type; 
  /*< public >*/
  nspsndfile *snd;
};

extern int nsp_type_sndfile_id;
extern NspTypeSndFile *nsp_type_sndfile;

NspTypeSndFile *new_type_sndfile(type_mode mode);

NspSndFile *nsp_new_sndfile();

/*
 * Object methods redefined for file 
 */

#ifdef SndFile_Private 
static int nsp_init_sndfile(NspSndFile *ob,NspTypeSndFile *type);
static int nsp_sndfile_size(NspSndFile *Mat, int flag);
static char *nsp_sndfile_type_as_string(void);
static char *nsp_sndfile_type_short_string(void);
static int nsp_sndfile_eq(NspObject *A, NspObject *B);
static int nsp_sndfile_neq(NspObject *A, NspObject *B);
static NspMethods *nsp_sndfile_get_methods(void);
static AttrTab nsp_sndfile_attrs[];
#endif 

NspSndFile *nsp_sndfile_create(char *name, char *fname);
NspSndFile *nsp_sndfile_copy(NspSndFile *H);
void nsp_sndfile_destroy(NspSndFile *H);
void nsp_sndfile_info(NspSndFile *H, int indent,char *name, int rec_level);
void nsp_sndfile_print(NspSndFile *H, int indent,char *name, int rec_level);

#define NULLSNDFILE (NspSndFile *) 0

/* SndFileObj.c */

extern NspSndFile *nsp_sndfile_object(NspObject *O);
extern int IsSndFileObj (Stack stack, int i);
extern NspSndFile *GetSndFileCopy (Stack stack, int i);
extern NspSndFile *GetSndFile (Stack stack, int i);

#endif 
