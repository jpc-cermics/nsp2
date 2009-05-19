/* Nsp
 * Copyright (C) 2005-2009 Jean-Philippe Chancelier Enpc/Cermics
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
 * Interface with the portaudio library. 
 *
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define Pa_Private 
#include "nsp/object.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include <sndfile.h>
#include <portaudio.h>
#include "pansp.h" 

#define FRAMES_PER_BUFFER   (1024)

/*
 * NspPa inherits from NspObject 
 */

int nsp_type_pa_id=0;
NspTypePa *nsp_type_pa=NULL;

NspTypePa *new_type_pa(type_mode mode)
{
  NspTypePa *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_pa != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pa;
    }
  if (( type =  malloc(sizeof(NspTypePa)))== NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = nsp_pa_attrs ;
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods =nsp_pa_get_methods; 
  type->new = (new_func *)nsp_new_pa;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for pa */ 

  top->pr = (print_func *)nsp_pa_print;	              /* printing */   
  top->dealloc = (dealloc_func *)nsp_pa_destroy;           /* dealloc */  
  top->copy  =  (copy_func *)nsp_pa_copy;                  /* copy object */  
  top->size  = (size_func *)nsp_pa_size;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *)nsp_pa_type_as_string;               /* type as a String */  
  top->sh_type = (sh_type_func *)nsp_pa_type_short_string;           /* type as a short string */  
  top->info = (info_func *)nsp_pa_info;                    /* info */  
  /* top->is_true = (is_true_func  *) pa_IsTrue;  */         /* check if object can be considered as true */  
  /*top->loop =(loop_func *) pa_LoopExtract;     */	/* for loops */  
  top->path_extract =  NULL;					/* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)nsp_pa_object;    	/* get object stored in SciObj */  
  top->eq  = (eq_func *)nsp_pa_eq;			/* equality check */  
  top->neq  = (eq_func *)nsp_pa_neq;		        /* non-equality check */

  /* specific methods for pa */
  type->init = (init_func *)nsp_init_pa;

  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_pa_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_pa
       */
      type->id =  nsp_type_pa_id = nsp_new_type_id();
      nsp_type_pa = type;
      if ( nsp_register_type(nsp_type_pa) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_pa(mode);
    }
  else 
    {
      type->id = nsp_type_pa_id;
      return type;
    }
}
/*
 * initialize Scifile instances 
 * locally and by calling initializer on parent class 
 */

static int nsp_init_pa(NspPa *o,NspTypePa *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Pa 
 */

NspPa *nsp_new_pa() 
{
  NspPa *loc; 
  /* type must exists */
  nsp_type_pa = new_type_pa(T_BASE);
  if ( (loc = malloc(sizeof(NspPa)))== NULLPA) return loc;
  /* initialize object */
  if (nsp_init_pa(loc,nsp_type_pa) == FAIL) return NULLPA;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for pa 
 *-----------------------------------------------*/

/*
 * size 
 */

static int nsp_pa_size(NspPa  *H, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char nsp_pa_type_name[]="Paudio";
static char nsp_pa_short_type_name[]="paudio";

static char *nsp_pa_type_as_string(void)
{
  return(nsp_pa_type_name);
}

static char *nsp_pa_type_short_string(NspObject *v)
{
  return(nsp_pa_short_type_name);
}


static int nsp_pa_full_comp(NspPa * A,NspPa * B,char *op,int *err)
{
  Scierror("PaFullComp: to be implemented \n");
  return FALSE;
}

static int nsp_pa_eq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_ivect_id) == FALSE) return FALSE ;
  rep =nsp_pa_full_comp((NspPa *) A,(NspPa *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

static int nsp_pa_neq(NspObject *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_ivect_id) == FALSE) return TRUE;
  rep =nsp_pa_full_comp((NspPa *) A,(NspPa *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * delete 
 */

void nsp_pa_destroy(NspPa  *F)
{
  if ( F->pa->refcount == 0 ) 
    {
      FREE(F->pa);
    }
  else 
    {
      F->pa->refcount--;
    }
  nsp_object_destroy_name(NSP_OBJECT(F));
  FREE(F) ;
}

/*
 * info 
 */

void nsp_pa_info(NspPa  *F, int indent,char *name,int rec_level)
{
  int i;
  if (F == NULLPA) 
    {
      Sciprintf("Null Pointer Pa \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  if ( strcmp(NSP_OBJECT(F)->name,NVOID) == 0) 
    Sciprintf("Pa fname=%s\n","void");
  else
    Sciprintf("%s =\tPa fname=%s\n",NSP_OBJECT(F)->name,"void");
}

/*
 * print 
 */

int nsp_pa_print(NspPa  *F, int indent,char *name, int rec_level)
{
  nsp_pa_info(F,indent,NULL,0);
  return TRUE;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Hash objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPa  *nsp_pa_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_pa_id) == TRUE) return ((NspPa *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_pa));
  return(NULL);
}


int IsPaObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pa_id);
}

int IsPa(NspObject *obj)
{
  return nsp_object_type(obj, nsp_type_pa_id);
}

NspPa *GetPaCopy(Stack stack, int i)
{
  if (  GetPa(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPa *GetPa(Stack stack, int i)
{
  NspPa *M;
  if (( M =nsp_pa_object(NthObj(i))) == NULLPA  )
    ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 *-----------------------------------------------------*/

NspPa *nsp_pa_create(char *name) 
{
  NspPa *F =nsp_new_pa() ;
  if ( F == NULLPA) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return NULLPA;
    }
  if ((F->pa = malloc(sizeof(nsppa))) == NULL) 
    {
      FREE(F);
      Scierror("Error:\tRunning out of memory\n");
      return NULLPA;
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(F),name) == NULL)
    return NULLPA;
  NSP_OBJECT(F)->ret_pos = -1 ; 
  F->pa->channels= 2;
  F->pa->o_device= -1;
  F->pa->sample_rate= 44100;
  F->pa->ostream = NULL;
  F->pa->refcount = 1;
  return(F);
}

/*
 * copy: must share the snd pointer 
 */

NspPa *nsp_pa_copy(NspPa  *A)
{
  NspPa *F =nsp_new_pa() ;
  if ( F == NULLPA) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return NULLPA;
    }
  F->pa = A->pa;
  A->pa->refcount++;
  return F;
}

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

static NspObject * int_pa_get_frames(void *Hv,const char *attr)
{
  /* return nsp_create_object_from_double(NVOID,((NspPa *) Hv)->pa->sfinfo.frames); */
  return NULL;
}

static int int_pa_set_void(void *Hv,const   char *attr, NspObject *O)
{
  Scierror("Error: attribute of pa objects cannot be changed after creation\n");
  return FAIL;
}

static AttrTab nsp_pa_attrs[] = {
  {"frames",int_pa_get_frames, int_pa_set_void,NULL,NULL },
  { (char *) 0, NULL, NULL, NULL , NULL  }
};


/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/


/*
 * play given data in the audio stream. 
 */


static int int_pa_play_stream(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspPa *P = self;
  PaError err;   
  float buffer[FRAMES_PER_BUFFER*2]; /* stereo output buffer */
  int offset = 0;
  NspMatrix *M;
  int sync=FALSE;
  int_types T[] = {realmat,new_opts, t_end} ;
  nsp_option opts[] ={{"sync",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&M,&opts,&sync) == FAIL) return RET_BUG;

  if ( P->pa->ostream == NULL) 
    {
      Scierror("Error: audio stream is not opened\n");
      return RET_BUG;
     }

  if ( M->m != P->pa->channels )
    {
      Scierror("Error: expecting a matrice with %d rows\n", P->pa->channels);
      return RET_BUG;
    }
  while (1) 
    {
      int i,j, n;
      n = Min( FRAMES_PER_BUFFER, (M->n - offset));
      for( i=0; i < n ; i++ )
	{
	  for ( j = 0 ; j < M->m ; j++) 
	    buffer[j+M->m*(i)] = M->R[j+ M->m*(i+offset)];
	}
      for (  ; i < FRAMES_PER_BUFFER; i++)
	{
	  for ( j = 0 ; j < M->m ; j++) 
	    buffer[j+M->m*(i)] = 0;
	}
      offset += n;
      err = Pa_WriteStream( P->pa->ostream, buffer, FRAMES_PER_BUFFER );
      if( err != paNoError ) 
	{
	  Scierror("Error: %s\n", Pa_GetErrorText(err));
	  return RET_BUG;
	}
      if ( offset >= M->n ) break;
    }
  return 0;
}


static int int_pa_stop_stream(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspPa *P = self;
  PaError err;   
  CheckStdRhs(0,0);
  CheckLhs(0,1);
  /* -- Now we stop the stream -- */
  if (( err = Pa_StopStream( P->pa->ostream )) != paNoError )
    goto end;
  
  if ((err = Pa_CloseStream(P->pa->ostream)) != paNoError)
    {
      Scierror("Error: in portaudio, %s\n", Pa_GetErrorText(err));
      P->pa->err=FAIL;goto end;
    }
  
 end :
  P->pa->ostream = NULL;
  Pa_Terminate();
  nsp_pa_thread_set_status(NSP_PA_INACTIVE);
  return 0;
}

static NspMethods nsp_pa_methods[] = {
  {"write", int_pa_play_stream },
  {"stop", int_pa_stop_stream },
  { (char *) 0, NULL}
};

static NspMethods *nsp_pa_get_methods(void) { return nsp_pa_methods;};

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

/*
 * f=player_create(...)
 */


static void nsp_paobj_open(NspPa *P);

static int int_player_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPa *F;
  int samplerate=44100,device=-1,channels=2;
  int_types T[] = {new_opts, t_end} ;
  nsp_option opts[] ={{"samplerate",s_int,NULLOBJ,-1},
		      {"channels",s_int,NULLOBJ,-1},
		      {"device", s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&opts,&samplerate,&channels,&device) == FAIL)
    return RET_BUG;
  /* initialize the type for pa */
  new_type_pa(T_BASE);
  if (( F = nsp_pa_create(NVOID)) == NULLPA) return RET_BUG;
  F->pa->channels= channels;
  F->pa->o_device= device;
  F->pa->sample_rate= samplerate;
  F->pa->ostream = NULL;
  nsp_paobj_open(F);
  if ( F->pa->err == FAIL) 
    {
      nsp_pa_destroy(F);
      return RET_BUG;
    }
  
  MoveObj(stack,1,(NspObject *) F);
  return 1;
}


static void nsp_paobj_open(NspPa *P)
{
  int max;
  PaStreamParameters ostream_p;
  PaError err;   
  
  P->pa->err=OK;

  if ((err = Pa_Initialize()) != paNoError)
    {
      Scierror("Error: in portaudio, %s\n",Pa_GetErrorText(err));
      P->pa->err=FAIL; goto end;
    }
  ostream_p.device = P->pa->o_device;
  if ( ostream_p.device == -1 ) 
    ostream_p.device = Pa_GetDefaultOutputDevice();

  if ( ostream_p.device == paNoDevice ) 
    {
      Scierror("Error: in portaudio, output device %d not found \n",
	       P->pa->o_device);
      P->pa->err=FAIL;goto end;
  }
  
  max = Pa_GetDeviceInfo(ostream_p.device)->maxOutputChannels;

  if ( P->pa->channels > 2 )
    {
      Scierror("Error: in portaudio here only stereo or mono sounds (%d>2) \n", 
	       P->pa->channels);
      P->pa->err=FAIL;goto end;
    }

  ostream_p.channelCount = P->pa->channels;
  ostream_p.sampleFormat = paFloat32;
  ostream_p.suggestedLatency = Pa_GetDeviceInfo(ostream_p.device )->defaultLowOutputLatency;
  ostream_p.hostApiSpecificStreamInfo = 0;

  if ((err = Pa_IsFormatSupported(NULL, &ostream_p, 44100)) != paNoError)
    {
      Scierror("Error: in portaudio, %s\n", Pa_GetErrorText(err));
      P->pa->err=FAIL;goto end;
    }
  
  err = Pa_OpenStream(&P->pa->ostream,
		      NULL,
		      &ostream_p,
		      44100,
		      FRAMES_PER_BUFFER, /* frames per buffer */
		      paNoFlag,
		      NULL,
		      NULL);
  
  if (err)
    {
      Scierror("Error: in portaudio, %s\n", Pa_GetErrorText(err));
      goto end;
    }

  if ((err = Pa_StartStream(P->pa->ostream))!= paNoError)
    {
      Scierror("Error: in portaudio, %s\n", Pa_GetErrorText(err));
      P->pa->err=FAIL;goto end;
    }
  return;
		
 end :
  Pa_Terminate();
  nsp_pa_thread_set_status(NSP_PA_INACTIVE);
}




/*
 * interface for playfile 
 */


static int int_nsp_play_file(Stack stack,int rhs,int opt,int lhs)
{
  int err,sync=FALSE,device=-1; 
  int_types T[] = {string,new_opts, t_end} ;
  nsp_option opts[] ={{"sync",s_bool,NULLOBJ,-1},
		      {"device",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  char *str;
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&str,&opts,&sync,&device) == FAIL) return RET_BUG;
  err= nsp_play_file( str,sync,device);
  if ( lhs == 1) 
    {
      if ( nsp_move_double(stack,1,(double) err)==FAIL) return RET_BUG;
      return 1;
    }
  return (err == FAIL) ? RET_BUG: 0;
}

static int  int_nsp_play_data(Stack stack,int rhs,int opt,int lhs)
{
  NspMatrix *M;
  int err,sync=FALSE,device=-1, sample_rate=44100, nocb=FALSE;
  int_types T[] = {realmat,new_opts, t_end} ;
  nsp_option opts[] ={{"sync",s_bool,NULLOBJ,-1},
		      {"device",s_int,NULLOBJ,-1},
		      {"samplerate",s_int,NULLOBJ,-1},
		      {"nocb",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&M,&opts,&sync,&device,&sample_rate,&nocb) == FAIL) return RET_BUG;
  if ( nocb == TRUE ) 
    err= nsp_play_data_no_cb(M,sample_rate,sync,device);
  else
    err= nsp_play_data(M,sample_rate,sync,device);
  
  if ( lhs == 1) 
    {
      if ( nsp_move_double(stack,1,(double) err)==FAIL) return RET_BUG;
      return 1;
    }
  return (err == FAIL) ? RET_BUG: 0;
}

static int  int_nsp_record_data(Stack stack,int rhs,int opt,int lhs)
{
  double seconds = 1;
  int sample_rate = 44100, channels = 2, device = -1, err, o_device=-2;
  NspMatrix *M;
  int_types T[] = {new_opts, t_end} ;
  nsp_option opts[] ={{"device",s_int,NULLOBJ,-1},
		      {"channels",s_int,NULLOBJ,-1},
		      {"seconds",s_double,NULLOBJ,-1},
		      {"samplerate",s_int,NULLOBJ,-1},
		      {"o_device",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&opts,&device,&channels,&seconds,&sample_rate,&o_device) == FAIL)
    return RET_BUG;
  err=  nsp_record_data(&M, seconds, sample_rate, channels, device, o_device);
  if ( err == FAIL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}


static OpTab Paudio_func[]={
  {"playsndfile", int_nsp_play_file},
  {"playsnd",     int_nsp_play_data},
  {"recordsnd",   int_nsp_record_data},
  {"player_create", int_player_create},
  {(char *) 0, NULL}
};

int Paudio_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Paudio_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 *  (for adding or removing functions) 
 */

void Paudio_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Paudio_func[i].name;
  *f = Paudio_func[i].fonc;
}


#define FRAMES_PER_BUFFER   (1024)

/* Initialize a portaudio stream 
 *
 */

/* play without call back.
 *
 */






