/* Nsp
 * Copyright (C) 2005 Jean-Philippe Chancelier Enpc/Cermics
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
 *
 * Interface with the sndfile library. 
 *
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SndFile_Private 
#include "nsp/object.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include <sndfile.h>
#include "sndnsp.h" 

/*
 * NspSndFile inherits from NspObject 
 */

int nsp_type_sndfile_id=0;
NspTypeSndFile *nsp_type_sndfile=NULL;

NspTypeSndFile *new_type_sndfile(type_mode mode)
{
  NspTypeSndFile *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_sndfile != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_sndfile;
    }
  if (( type =  malloc(sizeof(NspTypeSndFile)))== NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = nsp_sndfile_attrs ;
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods =nsp_sndfile_get_methods; 
  type->new = (new_func *)nsp_new_sndfile;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for sndfile */ 

  top->pr = (print_func *)nsp_sndfile_print;	              /* printing */   
  top->dealloc = (dealloc_func *)nsp_sndfile_destroy;           /* dealloc */  
  top->copy  =  (copy_func *)nsp_sndfile_copy;                  /* copy object */  
  top->size  = (size_func *)nsp_sndfile_size;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *)nsp_sndfile_type_as_string;               /* type as a String */  
  top->sh_type = (sh_type_func *)nsp_sndfile_type_short_string;           /* type as a short string */  
  top->info = (info_func *)nsp_sndfile_info;                    /* info */  
  /* top->is_true = (is_true_func  *) sndfile_IsTrue;  */         /* check if object can be considered as true */  
  /*top->loop =(loop_func *) sndfile_LoopExtract;     */	/* for loops */  
  top->path_extract =  NULL;					/* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)nsp_sndfile_object;    	/* get object stored in SciObj */  
  top->eq  = (eq_func *)nsp_sndfile_eq;			/* equality check */  
  top->neq  = (eq_func *)nsp_sndfile_neq;		        /* non-equality check */

  /* specific methods for sndfile */
  type->init = (init_func *)nsp_init_sndfile;

  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_sndfile_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_sndfile
       */
      type->id =  nsp_type_sndfile_id = nsp_new_type_id();
      nsp_type_sndfile = type;
      if ( nsp_register_type(nsp_type_sndfile) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_sndfile(mode);
    }
  else 
    {
      type->id = nsp_type_sndfile_id;
      return type;
    }
}
/*
 * initialize Scifile instances 
 * locally and by calling initializer on parent class 
 */

static int nsp_init_sndfile(NspSndFile *o,NspTypeSndFile *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of SndFile 
 */

NspSndFile *nsp_new_sndfile() 
{
  NspSndFile *loc; 
  /* type must exists */
  nsp_type_sndfile = new_type_sndfile(T_BASE);
  if ( (loc = malloc(sizeof(NspSndFile)))== NULLSNDFILE) return loc;
  /* initialize object */
  if (nsp_init_sndfile(loc,nsp_type_sndfile) == FAIL) return NULLSNDFILE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for sndfile 
 *-----------------------------------------------*/

/*
 * size 
 */

static int nsp_sndfile_size(NspSndFile  *H, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char nsp_sndfile_type_name[]="SndFile";
static char nsp_sndfile_short_type_name[]="sf";

static char *nsp_sndfile_type_as_string(void)
{
  return(nsp_sndfile_type_name);
}

static char *nsp_sndfile_type_short_string(void)
{
  return(nsp_sndfile_short_type_name);
}


static int nsp_sndfile_full_comp(NspSndFile * A,NspSndFile * B,char *op,int *err)
{
  Scierror("SndFileFullComp: to be implemented \n");
  return FALSE;
}

static int nsp_sndfile_eq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_ivect_id) == FALSE) return FALSE ;
  rep =nsp_sndfile_full_comp((NspSndFile *) A,(NspSndFile *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

static int nsp_sndfile_neq(NspObject *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_ivect_id) == FALSE) return TRUE;
  rep =nsp_sndfile_full_comp((NspSndFile *) A,(NspSndFile *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * delete 
 */

void nsp_sndfile_destroy(NspSndFile  *F)
{
  if ( F->snd->refcount == 0 ) 
    {
      /* we must close file ? before destroying everything */
      if ( F->snd->sf != NULL) 
	{
	  if ( F->snd->mode == SFM_WRITE) sf_command (F->snd->sf, SFC_UPDATE_HEADER_NOW, NULL, 0) ;
	  sf_close(F->snd->sf);
	}
      FREE(F->snd->fname);
      FREE(F->snd);
    }
  else 
    {
      F->snd->refcount--;
    }
  FREE(F) ;
}

/*
 * info 
 */

void nsp_sndfile_info(NspSndFile  *F, int indent)
{
  int i;
  if (F == NULLSNDFILE) 
    {
      Sciprintf("Null Pointer SndFile \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if ( strcmp(NSP_OBJECT(F)->name,NVOID) == 0) 
    Sciprintf("SndFile fname=%s\n",F->snd->fname);
  else
    Sciprintf("%s =\tSndFile fname=%s\n",NSP_OBJECT(F)->name,F->snd->fname);
}

/*
 * print 
 */

void nsp_sndfile_print(NspSndFile  *F, int indent)
{
  nsp_sndfile_info(F,indent);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Hash objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspSndFile  *nsp_sndfile_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_sndfile_id) == TRUE) return ((NspSndFile *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_sndfile));
  return(NULL);
}


int IsSndFileObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_sndfile_id);
}

int IsSndFile(NspObject *obj)
{
  return nsp_object_type(obj, nsp_type_sndfile_id);
}

NspSndFile *GetSndFileCopy(Stack stack, int i)
{
  if (  GetSndFile(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspSndFile *GetSndFile(Stack stack, int i)
{
  NspSndFile *M;
  if (( M =nsp_sndfile_object(NthObj(i))) == NULLSNDFILE  )
    ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 *-----------------------------------------------------*/

NspSndFile *nsp_sndfile_create(char *name, char *fname)
{
  NspSndFile *F =nsp_new_sndfile() ;
  if ( F == NULLSNDFILE) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return NULLSNDFILE;
    }
  if ((F->snd = malloc(sizeof(nspsndfile))) == NULL) 
    {
      FREE(F);
      Scierror("Error:\tRunning out of memory\n");
      return NULLSNDFILE;
    }
    
  if ((NSP_OBJECT(F)->name =new_nsp_string(name)) == NULLSTRING) return NULLSNDFILE;
  NSP_OBJECT(F)->ret_pos = -1 ; 
  if ((F->snd->fname = new_nsp_string(fname)) == NULLSTRING) return NULLSNDFILE;
  F->snd->refcount = 1;
  F->snd->sf = NULL;
  return(F);
}

/*
 * copy: must share the snd pointer 
 */

NspSndFile *nsp_sndfile_copy(NspSndFile  *A)
{
  NspSndFile *F =nsp_new_sndfile() ;
  if ( F == NULLSNDFILE) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return NULLSNDFILE;
    }
  F->snd = A->snd;
  A->snd->refcount++;
  return F;
}

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

static NspObject * int_sndfile_get_frames(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspSndFile *) Hv)->snd->sfinfo.frames);
}

static NspObject * int_sndfile_get_samplerate(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspSndFile *) Hv)->snd->sfinfo.samplerate);
}

static NspObject * int_sndfile_get_channels(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspSndFile *) Hv)->snd->sfinfo.channels);
}

static NspSMatrix *nsp_sndfile_format_to_string(int format);

static NspObject * int_sndfile_get_format(void *Hv,char *attr)
{
  return (NspObject *) nsp_sndfile_format_to_string(((NspSndFile *) Hv)->snd->sfinfo.format);
}

static NspObject * int_sndfile_get_sections(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspSndFile *) Hv)->snd->sfinfo.sections);
}

static NspObject * int_sndfile_get_seekable(void *Hv,char *attr)
{
  return nsp_create_boolean_object(NVOID,((NspSndFile *) Hv)->snd->sfinfo.seekable);
}


static int int_sndfile_set_void(void *Hv, char *attr, NspObject *O)
{
  Scierror("Error: attribute of sndfile objects cannot be changed after creation\n");
  return FAIL;
}

static AttrTab nsp_sndfile_attrs[] = {
  {"frames",int_sndfile_get_frames, int_sndfile_set_void,NULL},
  {"samplerate",int_sndfile_get_samplerate, int_sndfile_set_void,NULL},
  {"channels",int_sndfile_get_channels, int_sndfile_set_void,NULL},
  {"format",int_sndfile_get_format, int_sndfile_set_void,NULL},
  {"sections",int_sndfile_get_sections, int_sndfile_set_void,NULL},
  {"seekable",int_sndfile_get_seekable, int_sndfile_set_void,NULL},
  { (char *) 0, NULL}
};


/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/


/*
 * f.close[]
 */

static int int_sndfile_close(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspSndFile *F = self;
  CheckRhs(0,0);
  /* if the file was opened for writing then 
   * we update the header before closing 
   */
  if ( F->snd->sf != NULL) 
    {
      int rep;
      if ( F->snd->mode == SFM_WRITE) sf_command (F->snd->sf, SFC_UPDATE_HEADER_NOW, NULL, 0) ;
      rep = sf_close(F->snd->sf);
      if (rep != 0) 
	{
	  Scierror("Error: %s while closing %s\n",sf_error_number(rep),F->snd->fname);
	  return RET_BUG;
	}
      /* F->snd->sf = NULL; */
    }
  return 0;
}

/*
 * f.error[];
 */

static int int_sndfile_error(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int rep=0;
  NspSndFile *F = self;
  CheckRhs(0,0);
  if ( F->snd->sf  != NULL) 
    {
      rep=sf_error(F->snd->sf);
    }
  if ( nsp_move_double(stack,1,(double) rep) == FAIL) return RET_BUG;
  return 1;
}

/*
 * f.strerror[];
 */

static int int_sndfile_strerror(void *self,Stack stack, int rhs, int opt, int lhs)
{
  /* const char* sf_strerror (SNDFILE *sndfile) ;
   */
  NspSndFile *F = self;
  CheckRhs(0,0);
  if ( F->snd->sf  != NULL) 
    {
      const char *str= sf_strerror(F->snd->sf);
      if ( nsp_move_string(stack,1,str,-1) == FAIL) return RET_BUG;
    }
  else
    {
      if ( nsp_move_string(stack,1,"sndfile is closed !\n",-1) == FAIL) return RET_BUG;
    }
  return 1;
}



/*
 * f.error_number[];
 */

static int int_sndfile_error_number(void *self,Stack stack, int rhs, int opt, int lhs)
{
  const char *str;
  int err;
  CheckRhs(1,1);
  if (GetScalarInt (stack, 1, &err) == FAIL) return RET_BUG;
  str= sf_error_number(err);
  if ( nsp_move_string(stack,1,str,-1) == FAIL) return RET_BUG;
  return 1;
}

/*
 * f.write[A]
 */

static int int_sf_write_double(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int ret;
  NspSndFile *F = self;
  NspMatrix *Val;
  CheckRhs(1,1);
  if (( Val = GetRealMat(stack,1)) == NULLMAT) return RET_BUG;
  if ( F->snd->mode == SFM_READ )
    {
      Scierror("Error: trying to write in a file opened for reading\n");
      return RET_BUG;
    }
  if ( F->snd->sfinfo.channels != Val->m )
    {
      Scierror("Error: number of rows (%d) and number of channels (%d) should be the same\n",
	       Val->m, F->snd->sfinfo.channels);
      return RET_BUG;
    }

  ret= sf_write_double(F->snd->sf,Val->R,Val->mn);
  if ( nsp_move_double(stack,1,(double) ret) == FAIL) return RET_BUG;
  return 1;
}

/*
 * f.read[n]
 */

static int int_sf_read_double(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int ret,m,n;
  NspSndFile *F = self;
  NspMatrix *Val;
  CheckRhs(0,1);

  if ( F->snd->sf == NULL) 
    {
      Scierror("Error:\tsoundfile is not opened\n");
      return RET_BUG;
    }
  m = F->snd->sfinfo.channels;
  if ( rhs == 1 ) 
    {
      if (GetScalarInt (stack, 1, &n) == FAIL) return RET_BUG;
    }
  else 
    {
      n = F->snd->sfinfo.frames;
    }
  if (( Val =nsp_matrix_create(NVOID,'r',m,n)) == NULLMAT) return RET_BUG;
  /* 
   * 
   */ 
  ret=sf_read_double(F->snd->sf,Val->R,m*n);
  if ( nsp_matrix_resize(Val,m,ret/m) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Val);
  return 1;
}

/* 
 * f.seek[]
 */

static int int_sf_seek(void *self,Stack stack, int rhs, int opt, int lhs)
{
  sf_count_t frames;
  char flag_def[]="set", *flag=flag_def;
  double df;
  int rep,iflag;
  NspSndFile *F = self;
  CheckRhs(1,2);
  if ( F->snd->sf == NULL) 
    {
      Scierror("Error:\tsoundfile is not opened\n");
      return RET_BUG;
    }
  if (GetScalarDouble (stack, 1, &df) == FAIL) return RET_BUG;
  frames = (sf_count_t) df;
  if ( rhs == 2 ) 
    {
      if ((flag = GetString(stack,2)) == (char*)0) return RET_BUG;
    }
  if ( strncmp(flag,"set",3)==0 ) 
    iflag = SEEK_SET; 
  else if ( strncmp(flag,"cur",3)==0 )  
    iflag = SEEK_CUR; 
  else if ( strncmp(flag,"end",3)==0 )  
    iflag = SEEK_END; 
  else 
    {
      Scierror("fseek : flag = %s not recognized\n",flag);
      return RET_BUG;
    }
  rep = sf_seek( F->snd->sf,frames,iflag);
  if ( rep == -1 )
    {
      Scierror("Error: seek failed for sound file %s\n", F->snd->fname);
      return RET_BUG;
    }
  return 0;
}

static NspMethods nsp_sndfile_methods[] = {
  {"close", int_sndfile_close},
  {"error", int_sndfile_error },
  {"strerror", int_sndfile_strerror },
  {"error_number", int_sndfile_error_number},
  {"read", int_sf_read_double},
  {"seek", int_sf_seek},
  {"write", int_sf_write_double },
  { (char *) 0, NULL}
};

static NspMethods *nsp_sndfile_get_methods(void) { return nsp_sndfile_methods;};

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

/*
 * f=fopen(fname [,mode])
 */

static int nsp_sndfile_format(NspSMatrix *S);

static int int_sndfile_fopen(Stack stack, int rhs, int opt, int lhs)
{
  NspSndFile *F;
  NspSMatrix *Format=NULL;
  int sfmode;
  double frames = 0.0;
  SF_INFO sfi ={0,44100,1, SF_FORMAT_WAV |SF_FORMAT_PCM_16  ,1,1};
  int_types T[] = {string,new_opts, t_end} ;
  nsp_option opts[] ={{"mode",string,NULLOBJ,-1},
		      {"frames",s_double,NULLOBJ,-1},
		      {"samplerate",s_int,NULLOBJ,-1},
		      {"channels",s_int,NULLOBJ,-1},
		      {"format",smat,NULLOBJ,-1},
		      {"sections",s_int,NULLOBJ,-1},
		      {"seekable",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  char *Fname, *mode = NULL, *def_mode = "r";
  if ( GetArgs(stack,rhs,opt,T,&Fname,&opts,&mode,&frames,&sfi.samplerate,&sfi.channels,&Format,&sfi.sections,
	       &sfi.seekable) == FAIL) return RET_BUG;
  sfi.frames =  (sf_count_t) frames;
  if ( mode == NULL) mode = def_mode;
  if ( Format != NULL ) 
    {
      if ((sfi.format = nsp_sndfile_format(Format))==0) return RET_BUG;
    }
  if ( strcmp(mode,"r")==0 ) 
    {
      sfi.format = 0;
      sfmode = SFM_READ;
    }
  else if (strcmp(mode,"w")==0) 
    {
      if ( sf_format_check(&sfi) == FALSE ) 
	{
	  Scierror("Error: the selected format is not valid for writing sndfile\n");
	  return RET_BUG;
	}
      sfmode = SFM_WRITE;
    }
  else if (strcmp(mode,"rw")==0) 
    {
      sfmode = SFM_RDWR ;
    }
  else 
    {
      Scierror("Error:\tsndfile unrecognized mode %s\n",mode);
      return RET_BUG;
    }
  /* initialize the type for sndfile */
  new_type_sndfile(T_BASE);
  if (( F = nsp_sndfile_create(NVOID, Fname)) == NULLSNDFILE) return RET_BUG;
  F->snd->sfinfo = sfi;
  F->snd->sf = sf_open (F->snd->fname, sfmode, &F->snd->sfinfo);
  F->snd->mode = sfmode;
  if ( F->snd->sf == NULL) 
    {
      Scierror("Error: while opening file %s mode=%s\n",F->snd->fname,mode);
      return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) F);
  return 1;
}


static int int_nsp_play(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Val=NULL;
  int job=1;
  int_types T[] = {realmat,s_int, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&Val,&job) == FAIL) return RET_BUG;
#ifdef WITH_PLAY 
  if ( nsp_play (Val,job) == FAIL) return RET_BUG;
  return 0;
#else 
  Scierror("nsp_play not activated \n");
  return RET_BUG;
#endif
}

static OpTab SndFile_func[]={
  {"sndfile", int_sndfile_fopen},
  {"play", int_nsp_play},
  {(char *) 0, NULL}
};

int SndFile_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(SndFile_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 *  (for adding or removing functions) 
 */

void SndFile_Interf_Info(int i, char **fname, function (**f))
{
  *fname = SndFile_func[i].name;
  *f = SndFile_func[i].fonc;
}

/*
 * utilities to define a  formats at nsp level 
 * by strings. 
 */

typedef struct _snd_formats snd_formats; 

struct _snd_formats {
  char *name;
  int  val;
};

snd_formats sndfile_major_formats[] =
  {
    {"WAV"	,SF_FORMAT_WAV },
    {"AIFF"	,SF_FORMAT_AIFF },
    {"AU"	,SF_FORMAT_AU },
    {"RAW"	,SF_FORMAT_RAW },
    {"PAF"	,SF_FORMAT_PAF },
    {"SVX"	,SF_FORMAT_SVX },
    {"NIST"	,SF_FORMAT_NIST },
    {"VOC"	,SF_FORMAT_VOC },
    {"IRCAM"	,SF_FORMAT_IRCAM },
    {"W64"	,SF_FORMAT_W64 },
    {"MAT4"	,SF_FORMAT_MAT4 },
    {"MAT5"	,SF_FORMAT_MAT5 },
    {"PVF"	,SF_FORMAT_PVF },
    {"XI"	,SF_FORMAT_XI },
    {"HTK"	,SF_FORMAT_HTK },
    {"SDS"	,SF_FORMAT_SDS },
    {"AVR"	,SF_FORMAT_AVR },
    {"WAVEX"	,SF_FORMAT_WAVEX },
    {"SD2"	,SF_FORMAT_SD2 },
    {NULL,0}
  };

/* Subtypes from here on. */
snd_formats sndfile_subtypes_formats[] =
  {
    {"PCM_S8",	     SF_FORMAT_PCM_S8 },
    {"PCM_16",	     SF_FORMAT_PCM_16 },
    {"PCM_24",	     SF_FORMAT_PCM_24 },
    {"PCM_32",	     SF_FORMAT_PCM_32 },
    {"PCM_U8",	     SF_FORMAT_PCM_U8 },
    {"FLOAT",        SF_FORMAT_FLOAT },
    {"DOUBLE",	     SF_FORMAT_DOUBLE },
    {"ULAW"	,    SF_FORMAT_ULAW },
    {"ALAW"	,    SF_FORMAT_ALAW },
    {"IMA_ADPCM",    SF_FORMAT_IMA_ADPCM },
    {"MS_ADPCM",     SF_FORMAT_MS_ADPCM },
    {"GSM610",       SF_FORMAT_GSM610 },
    {"VOX_ADPCM",    SF_FORMAT_VOX_ADPCM },
    {"G721_32",      SF_FORMAT_G721_32 },
    {"G723_24",      SF_FORMAT_G723_24 },
    {"G723_40",	     SF_FORMAT_G723_40 },
    {"DWVW_12",	     SF_FORMAT_DWVW_12 },
    {"DWVW_16",      SF_FORMAT_DWVW_16 },
    {"DWVW_24",      SF_FORMAT_DWVW_24 },
    {"DWVW_N",       SF_FORMAT_DWVW_N },
    {"DPCM_8",       SF_FORMAT_DPCM_8 },
    {"DPCM_16",      SF_FORMAT_DPCM_16 },
    {NULL,0}
  };
/* Endian-ness options. */

snd_formats sndfile_endian_formats[] = 
  {
    {"FILE"	,    SF_ENDIAN_FILE },
    {"LITTLE"    ,   SF_ENDIAN_LITTLE },
    {"BIG"	,    SF_ENDIAN_BIG },
    {"CPU"	,    SF_ENDIAN_CPU },
    {NULL,0}
  };

static int nsp_sndfile_format(NspSMatrix *S)
{
  int fm1=0,fm2=0,fm3=0,format=0;
  if ( S->mn > 3 || S->mn < 1) 
    {
      Scierror("Error: String matrix for format has wrong size\n");
      return 0;
    }
  fm1 = is_string_in_struct(S->S[0],(void **)sndfile_major_formats,sizeof(snd_formats),0);
  if ( fm1 <0 ) 
    {
      Scierror("Error: format %s is wrong\n",S->S[0]);
      return 0;
    }
  format = sndfile_major_formats[fm1].val ;
  if ( S->mn >= 2) 
    {
      fm2 = is_string_in_struct(S->S[1],(void **)sndfile_subtypes_formats,sizeof(snd_formats),0);
      if ( fm2 < 0 ) 
	{
	  Scierror("Error: subtype format %s is wrong\n",S->S[1]);
	  return 0;
	}
      format |= sndfile_subtypes_formats[fm2].val;
    }
  if ( S->mn >= 3 ) 
    {
      fm3 = is_string_in_struct(S->S[2],(void **)sndfile_endian_formats,sizeof(snd_formats),0);
      if ( fm3 < 0 ) 
	{
	  Scierror("Error: endian format %s is wrong\n",S->S[2]);
	  return 0;
	}
      format |= sndfile_endian_formats[fm3].val;
    }
  return format;
}


static NspSMatrix *nsp_sndfile_format_to_string(int format)
{
  char def[]="";
  const char *T[]={NULL,NULL,NULL};
  snd_formats *v; 
  int x,size=3;
  x = SF_FORMAT_TYPEMASK & format ;
  v = sndfile_major_formats;
  while ( v->name != NULL) { if ( v->val == x ) { T[0] = v->name;break;} v++;}
  if ( T[0] == NULL ) return nsp_smatrix_create_from_array(NVOID,0,T);
  x = SF_FORMAT_SUBMASK & format; 
  v = sndfile_subtypes_formats;
  while ( v->name != NULL) { if ( v->val == x ) { T[1] = v->name;break;} v++;}
  x = SF_FORMAT_ENDMASK  & format ;
  v = sndfile_endian_formats;
  while ( v->name != NULL) { if ( v->val == x ) { T[2] = v->name;break;} v++;}
  if ( T[2] == NULL || strcmp(T[2],"FILE")==0 ) size=2; 
  if ( size == 2 && T[1] == NULL) size=1;
  if ( size == 3 && T[1] == NULL) T[1]=def;
  return nsp_smatrix_create_from_array(NVOID,size,T);
}









