/*
 * nsp version
 *  Jean-Philippe Chancelier 
 *  Copyright Enpc 
 *
 * for blocks coded in nsp 
 */

#include "nsp/machine.h"
#include "nsp/object.h"

#include "scicos/scicos.h"

/* XXXXX */
extern int nsp_gtk_eval_function(NspPList *func,NspObject *args[],int n_args,NspObject  *ret[],int *nret);
extern int nsp_gtk_eval_function_by_name(char *name,NspObject *args[],int n_args,NspObject  *ret[],int *nret);

static int scicos_scifunc(  NspObject **Args,int mrhs,NspObject **Ret, int *mlhs ) 
{
  switch (Scicos->params.scsptr_flag ) 
    {
    case  fun_macros: 
      /* 
       *	 Sciprintf("Evaluate a given macro\n");
       *	 nsp_object_print( Scicos->params.scsptr,0,0,0);
       */
      return nsp_gtk_eval_function((NspPList *) Scicos->params.scsptr,Args, mrhs, Ret, mlhs);
      break;
    case fun_macro_name:
      /* 
       *	 Sciprintf("Evaluate a macro given by its name: %s\n",
       *	 Scicos->params.scsptr);
       */
      return nsp_gtk_eval_function_by_name(Scicos->params.scsptr,Args, mrhs, Ret, mlhs);
    case fun_pointer: 
      Scierror("Internal error: Expecting a macro or macro name\n");
      return FAIL;
    }
  return FAIL;
}

/**
 * scicos_itosci:
 * @name: 
 * @x: 
 * @mx: 
 * @nx: 
 * 
 * 
 * 
 * Return value: 
 **/

static NspObject *scicos_itosci(const char *name, const int *x,int mx,int nx) 
{
  int i;
  NspMatrix *M;
  if ((M = nsp_matrix_create(name,'r',mx,nx)) == NULLMAT) return NULLOBJ;
  for ( i = 0 ; i < M->mn; i++) M->R[i]=(double) x[i];
  return NSP_OBJECT(M);
}

/**
 * scicos_dtosci:
 * @name: 
 * @x: 
 * @mx: 
 * @nx: 
 * 
 * 
 * 
 * Return value: 
 **/

static NspObject *scicos_dtosci(const char *name,const double *x,int mx,int nx) 
{
  NspMatrix *M;
  if ((M = nsp_matrix_create(name,'r',mx,nx)) == NULLMAT) return NULLOBJ;
  memcpy(M->R,x,M->mn*sizeof(double));
  return NSP_OBJECT(M);
}

/**
 * scicos_str2sci:
 * @name: 
 * @x: 
 * 
 * 
 * 
 * Return value: 
 **/

static NspObject *scicos_str2sci(const char *name,nsp_const_string x)
{
  return (NspObject *) nsp_smatrix_create(name,1,1,x,1);
}

/**
 * scicos_obj_to_mserial:
 * @x: array pointer 
 * @nx: size of @x 
 * @Obj: #NspObject to be stored in @x
 * 
 * fills array @x of size @nx with the Matrix serialized version 
 * of nsp object @Obj. If @nx is not equal to the serialized size 
 * an error is raised
 * 
 * Return value: %OK or %FAIL
 **/

static int scicos_obj_to_mserial(double *x,int nx,const NspObject *Obj )
{
  int i; 
  NspObject *S;
  NspMatrix *A;
  if ( nx == 0) return OK; 
  if ((S = nsp_object_serialize(Obj))== NULLOBJ) return FAIL;
  /* serialize in a matrix */
  A = nsp_serial_to_matrix((NspSerial *) S);
  nsp_object_destroy(&S);
  if ( A== NULLMAT)  return FAIL;
  if ( A->mn != nx ) 
    {
      Sciprintf("Error: cannot store a serialized nsp object (size %d) in double array (soze %d)\n",
		A->mn,nx);
    }
  for ( i= 0 ; i < A->mn; i++) x[i]= A->R[i];
  nsp_matrix_destroy(A);
  return OK;
}

/**
 * scicos_mserial_to_obj:
 * @x: array pointer 
 * @nx: size of @x 
 * 
 * unserialize the nsp object stored in a double array @x.
 * 
 * Return value: %NULLOBJ or a new #NspObject
 **/

static NspObject *scicos_mserial_to_obj(const char *name,const double *x,int nx)
{
  NspMatrix *Z=NULL;
  NspSerial *S=NULL;
  NspObject *Obj=NULL;
  /* new matrix from x */
  if ((Z=  nsp_matrix_create_from_array(NVOID,1,nx,x,NULL))== NULL) goto err;
  if ( nx == 0 )
    {
      if (nsp_object_set_name(NSP_OBJECT(Z),name) == FAIL) goto err;
      return NSP_OBJECT(Z);
    }
  /* Z is supposed to contain serialized data  */
  if ((S= nsp_matrix_to_serial(Z ))== NULL) goto err;
  /* unserialize S */
  if ((Obj=nsp_object_unserialize(S))== NULLOBJ) goto err;
  if (nsp_object_set_name(Obj,name) == FAIL) goto err;
 err:
  if ( S != NULL) nsp_serial_destroy(S);
  if ( Z != NULL) nsp_matrix_destroy(Z);
  return Obj;
}

/**
 * scicos_scitod:
 * @: 
 * @mx: 
 * @nx: 
 * @Ob: 
 * 
 * 
 * Return value: 
 **/

static int scicos_scitod(double *x,int mx,int nx,const NspObject *Ob)
{
  NspMatrix *M= ((NspMatrix *) Ob);
  int i;
  if ( mx*nx == 0 || M->mn == 0) return OK;
  if ( M->m != mx || M->n != nx || M->rc_type != 'r' ) 
    {
      Sciprintf("Expecting a (%d,%d) matrix and (%d,%d) returned\n",mx,nx,
		M->m,M->n);
    }
  for ( i = 0 ; i < Min(M->mn,mx*nx); i++) x[i]= M->R[i];
  return OK;
}

/**
 * scicos_scitoi:
 * @: 
 * @mx: 
 * @nx: 
 * @Ob: 
 * 
 * 
 * 
 * Return value: 
 **/

static int scicos_scitoi(int x[],int mx,int nx,const NspObject *Ob)
{
  NspMatrix *M= ((NspMatrix *) Ob);
  int i;
  if ( mx*nx == 0 || M->mn == 0) return OK;
  if ( M->m != mx || M->n != nx || M->rc_type != 'r' ) 
    {
      Sciprintf("Expecting a (%d,%d) matrix and (%d,%d) returned\n",mx,nx,
	       M->m,M->n);
    }
  for ( i = 0 ; i < Min(M->mn,mx*nx) ; i++) x[i]= M->R[i];
  return OK;
}

/**
 * scicos_list_to_vars:
 * @: 
 * @nout: 
 * @: 
 * @Ob: 
 * 
 * 
 * 
 * Return value: 
 **/

static int scicos_list_to_vars(double *outptr[],int nout,int outsz[],NspObject *Ob)
{
  int k; 
  NspList *L= (NspList *) Ob;
  for ( k = nout-1; k >=0 ; k--) 
    {
      NspObject *elt = nsp_list_get_element(L,k+1);
      if ( elt == NULL) return FAIL;
      if ( scicos_scitod(outptr[k],outsz[k],1,elt)== FAIL) return FAIL;
    }
  return OK;
}

/**
 * scicos_vars_to_list:
 * @: 
 * @nin: 
 * @: 
 * 
 * 
 * 
 * Return value: 
 **/

static NspObject *scicos_vars_to_list(const char *name,double **inptr,int nin,int *insz)
{
  int k;
  NspList *Ob;
  if ((Ob = nsp_list_create(name) ) == NULL) return NULL;
  for( k=0 ; k < nin ; k++) 
    {
      NspObject *elt;
      if ((elt = scicos_dtosci("el",inptr[k],insz[k],1))== NULL)
	{
	  nsp_list_destroy(Ob);
	  return NULL;
	}
      if ( nsp_list_insert(Ob,elt,k+1) == FAIL)
	{
	  nsp_list_destroy(Ob);
	  return NULL;
	}
    }
  return NSP_OBJECT(Ob);
}
 
/* XXX: note that the array z transmited here is suposed 
 * to be a nsp object serialized in a matrix. 
 * Thus we have to serialize/unserialize here.
 *
 *
 */

void  scicos_sciblk2(int *flag, int *nevprt, double *t, double *xd, double *x, int *nx, double *z,
		     int *nz, double *tvec, int *ntvec, double *rpar, int *nrpar, int *ipar, 
		     int *nipar, double **inptr, int *insz, int *nin, double **outptr, 
		     int *outsz, int *nout)
{
  int mlhs=5,mrhs=8;
  int i;
  NspObject *Args[8]={NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL};
  NspObject *Ret[5]={NULL,NULL,NULL,NULL,NULL};
  
  /* we give no names to Args, they will be freed by scicos_scifunc */
  if ((Args[0]= scicos_itosci(NVOID,flag,1,1)) == NULL) goto err;
  if ((Args[1]= scicos_itosci(NVOID,nevprt,1,1)) == NULL) goto err;
  if ((Args[2]= scicos_dtosci(NVOID,t,1,1)) == NULL) goto err;
  if ((Args[3]= scicos_dtosci(NVOID,x,*nx,1)) == NULL) goto err;
  if ((Args[4]= scicos_mserial_to_obj(NVOID,z,*nz))== NULL) goto err;
  if ((Args[5]= scicos_mserial_to_obj(NVOID,rpar,*nrpar)) == NULL) goto err; 
  if ((Args[6]= scicos_itosci(NVOID,ipar,*nipar,1)) == NULL) goto err;
  if ((Args[7]= scicos_vars_to_list(NVOID,inptr,*nin,insz))==NULLOBJ) goto err;

  /* function to be evaluated or name of function to be evaluated */

  if ( scicos_scifunc(Args,mrhs,Ret,&mlhs) == FAIL) goto err;

  switch (*flag) 
    {
    case 1 :
      if ( scicos_obj_to_mserial(z,*nz,Ret[2])== FAIL) goto err;
      scicos_scitod(x,*nx,1,Ret[1]);
      if (*nout != 0 ) 
	{
	  if ( scicos_list_to_vars(outptr,*nout,outsz,Ret[0])==FAIL) goto err;
	}
      break;
    case 0 :
      /*     [y,x,z,tvec,xd]=func(flag,nevprt,t,x,z,rpar,ipar,u) */
      /*  x'  computation */
      scicos_scitod(xd,*nx,1,Ret[4]);
      break;
    case 2 :
      if ( scicos_obj_to_mserial(z,*nz,Ret[2])== FAIL) goto err;
      scicos_scitod(x,*nx,1,Ret[1]);
      break;
    case 3 :
      scicos_scitod(tvec,*ntvec,1,Ret[3]);
      break;
    case 4 :
    case 5 :
      if ( scicos_obj_to_mserial(z,*nz,Ret[2])== FAIL) goto err;
      scicos_scitod(x,*nx,1,Ret[1]);
      break;
    case 6 :
      if ( scicos_obj_to_mserial(z,*nz,Ret[2])== FAIL) goto err;
      scicos_scitod(x,*nx,1,Ret[1]);
      if ( *nout !=0 ) 
	{
	  if ( scicos_list_to_vars(outptr,*nout,outsz,Ret[0])==FAIL) goto err;
	}
      break;
    }
  for (i=0 ; i < mlhs ; i++)
    {
      if ( Ret[i] != NULLOBJ)  nsp_object_destroy(&Ret[i]);
    }
  return;
 err: 
  *flag=-1;
}

/* time added in block 
 *
 */

void scicos_sciblk4(scicos_block *Blocks, int *flag)
{
  int mlhs=1,mrhs=2;
  NspObject *Ob;
  NspHash *H=NULL,*Hi=NULL;
  NspObject *Hel[32], *Args[2],*Ret[1];
  int p = 0,i;
  double time=get_scicos_time();
  if ((Hel[p++]=   scicos_dtosci("time",&time,1,1))== NULL) goto err;
  if ((Hel[p++]=   scicos_itosci("nevprt",&Blocks->nevprt,1,1))== NULL) goto err;
  if ((Hel[p++]=   scicos_itosci("nevprt",&Blocks->nevprt,1,1))== NULL) goto err;
  if ((Hel[p++]=   scicos_itosci("type",&Blocks->type,1,1))== NULL) goto err;
  /* if ((Hel[p++]=   scicos_itosci(&Blocks->scsptr,0,1))== NULL) goto err; */
  /* if ((Hel[p++]=   scicos_itosci("nz",&Blocks->nz,1,1))== NULL) goto err; */
  if ((Hel[p++]=  scicos_mserial_to_obj("z",Blocks->z,Blocks->nz))== NULL) goto err;
  /* if ((Hel[p++]=   scicos_itosci("nx",&Blocks->nx,1,1))== NULL) goto err; */
  if ((Hel[p++]=   scicos_dtosci("x",Blocks->x,Blocks->nx,1))== NULL) goto err;
  if ((Hel[p++]=   scicos_dtosci("xd",Blocks->xd,Blocks->nx,1))== NULL) goto err;
  if ((Hel[p++]=   scicos_dtosci("res",Blocks->res,Blocks->nx,1))== NULL) goto err;
  if ((Hel[p++]=   scicos_itosci("nin",&Blocks->nin,1,1))== NULL) goto err; 
  if ((Hel[p++]=   scicos_itosci("insz",Blocks->insz,Blocks->nin,1))== NULL) goto err;
  if ((Hel[p++]= scicos_vars_to_list("inptr",Blocks->inptr,Blocks->nin,Blocks->insz))==NULLOBJ) goto err;
  if ((Hel[p++]=   scicos_itosci("outsz",Blocks->outsz,Blocks->nout,1))== NULL) goto err; 
  if ((Hel[p++]=   scicos_itosci("nout",&Blocks->nout,1,1))== NULL) goto err;
  if ((Hel[p++]= scicos_vars_to_list("outptr",Blocks->outptr,Blocks->nout,Blocks->outsz))==NULLOBJ) goto err;
  if ((Hel[p++]=   scicos_itosci("nevout",&Blocks->nevout,1,1))== NULL) goto err;
  if ((Hel[p++]=   scicos_dtosci("evout",Blocks->evout,Blocks->nevout,1))== NULL) goto err;
  /* if ((Hel[p++]=   scicos_itosci("nrpar",&Blocks->nrpar,1,1))== NULL) goto err; */
  if ((Hel[p++]=  scicos_mserial_to_obj("rpar",Blocks->rpar,Blocks->nrpar)) == NULL) goto err; 
  /* if ((Hel[p++]=   scicos_itosci("nipar",&Blocks->nipar,1,1))== NULL) goto err; */
  if ((Hel[p++]=   scicos_itosci("ipar",Blocks->ipar,Blocks->nipar,1))== NULL) goto err;
  /* if ((Hel[p++]=   scicos_itosci("ng",&Blocks->ng,1,1))== NULL) goto err; */
  if ((Hel[p++]=   scicos_dtosci("g",Blocks->g,Blocks->ng,1))== NULL) goto err;
  if ((Hel[p++]=   scicos_itosci("ztyp",&Blocks->ztyp,1,1))== NULL) goto err;
  if ((Hel[p++]=   scicos_itosci("jroot",Blocks->jroot,Blocks->ng,1))== NULL) goto err;
  if ((Hel[p++]=   scicos_str2sci("label",Blocks->label))== NULL) goto err;
  /* if ((Hel[p++]=  scicos_mserial_to_obj(Blocks->work,0))== NULL) goto err; */
  /* if ((Hel[p++]=   scicos_itosci("nmode",&Blocks->nmode,1,1))== NULL) goto err; */
  if ((Hel[p++]=   scicos_itosci("mode",Blocks->mode,Blocks->nmode,1))== NULL) goto err; 

  if(( Hi = nsp_hash_create(NVOID,p)) == NULLHASH) goto err; 
  for ( i = 0 ; i < p ; i++) 
    {
      if (nsp_hash_enter(Hi,Hel[i]) == FAIL) goto err; 
    }
  Args[0]=NSP_OBJECT(Hi);
  if ((Args[1]= scicos_itosci(NVOID,flag,1,1)) == NULL) goto err;
  
  if ( scicos_scifunc(Args,mrhs,Ret,&mlhs) == FAIL) goto err;
  H = (NspHash *) Ret[0];
  switch (*flag) {
  case 1 :
    /* z,x et outptr */
    if ( Blocks->nx != 0) 
      {
	if ( nsp_hash_find(H,"x",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->x,Blocks->nx,1,Ob);
      }
    if (Blocks->nz != 0) 
      {
	if ( nsp_hash_find(H,"z",&Ob) == FAIL) goto err;
	if ( scicos_obj_to_mserial(Blocks->z,Blocks->nz,Ob)== FAIL) goto err;
      }
    if (Blocks->nout!=0) 
      {
	if ( nsp_hash_find(H,"outptr",&Ob) == FAIL) goto err;
	if ( scicos_list_to_vars(Blocks->outptr,Blocks->nout,Blocks->outsz,Ob)==FAIL) goto err;
      }
    break;
  case 0 :
    /*  x'  computation */
    if (Blocks->nx != 0)
      {
	if ( nsp_hash_find(H,"xd",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->xd,Blocks->nx,1,Ob);
	/* res XXX */
	if ( nsp_hash_find(H,"res",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->res,Blocks->nx,1,Ob);
      }
      break;
  case 2 :
    /* z */
    if (Blocks->nz != 0) 
      {
	if ( nsp_hash_find(H,"z",&Ob) == FAIL) goto err;
	if ( scicos_obj_to_mserial(Blocks->z,Blocks->nz,Ob)== FAIL) goto err;
      }
    /* x */
    if (Blocks->nx != 0)
      {
	if ( nsp_hash_find(H,"x",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->x,Blocks->nx,1,Ob);
	if ( nsp_hash_find(H,"xd",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->xd,Blocks->nx,1,Ob);
      }
    if ( nsp_hash_find(H,"mode",&Ob) == FAIL) goto err;
    scicos_scitoi(Blocks->mode,Blocks->nmode,1,Ob);
    break;
  case 3 :
    if ( nsp_hash_find(H,"evout",&Ob) == FAIL) goto err;
    scicos_scitod(Blocks->evout,Blocks->nevout,1,Ob);
    break;
  case 4 :
  case 5 :
    if (Blocks->nz != 0) 
      {
	if ( nsp_hash_find(H,"z",&Ob) == FAIL) goto err;
	if ( scicos_obj_to_mserial(Blocks->z,Blocks->nz,Ob)== FAIL) goto err;
      }
    if (Blocks->nx != 0) 
      {
	/* 8ieme element de la tlist x */
	if ( nsp_hash_find(H,"x",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->x,Blocks->nx,1,Ob);
	/* 9 ieme element de la tlist xd */
	if ( nsp_hash_find(H,"xd",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->xd,Blocks->nx,1,Ob);
    }
    break;
  case 6 :
    if (Blocks->nz != 0) 
      {
	if ( nsp_hash_find(H,"z",&Ob) == FAIL) goto err;
	if ( scicos_obj_to_mserial(Blocks->z,Blocks->nz,Ob)== FAIL) goto err;
      }
    if (Blocks->nx != 0) 
      {
	if ( nsp_hash_find(H,"x",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->x,Blocks->nx,1,Ob);
	if ( nsp_hash_find(H,"xd",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->xd,Blocks->nx,1,Ob);
      }
    if (Blocks->nout!=0) 
      {
	if ( nsp_hash_find(H,"outptr",&Ob) == FAIL) goto err;
	if ( scicos_list_to_vars(Blocks->outptr,Blocks->nout,Blocks->outsz,Ob)==FAIL) goto err;
      }
    break;
  case 7 :
    if (Blocks->nx != 0)
      {
	/* 9 ieme element de la tlist xd */
	if ( nsp_hash_find(H,"xd",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->xd,Blocks->nx,1,Ob);
      }
    /* 30 ieme element de la tlist mode */
    if ( nsp_hash_find(H,"mode",&Ob) == FAIL) goto err;
    scicos_scitoi(Blocks->mode,Blocks->nmode,1,Ob);
    break;
  case 9 :
    /* 24 ieme element de la tlist g */
    if ( nsp_hash_find(H,"g",&Ob) == FAIL) goto err;
    scicos_scitod(Blocks->g,Blocks->ng,1,Ob);
    /* 30 ieme element de la tlist mode */
    if ( nsp_hash_find(H,"mode",&Ob) == FAIL) goto err;
    scicos_scitoi(Blocks->mode,Blocks->nmode,1,Ob);
    break;
  }
  nsp_hash_destroy(H);
  return;
 err: 
  if ( H != NULL) nsp_hash_destroy(H);
  *flag=-1;
}



/*     routine used to evaluate a block defined by a scilab function */
/*     scilab function syntax must be */
/*     [y,x,z,tvec,xd]=func(flag,nevprt,t,x,z,rpar,ipar,u) */
/*     with */
/*        t      scalar current time */
/*        x      column vector continuous state */
/*        z      column vector discrete state */
/*        u      column vector block input */
/*        nevprt int */
/*        flag   int */
/*        y      column vector block output */
/*        xd     column vector block state derivative */

void scicos_sciblk(int *flag, int *nevprt, double *t, double *xd, double *x, int *nx,
		   double *z, int *nz, double *tvec, int *ntvec, double *rpar, int *nrpar,
		   int *ipar, int *nipar, double *u, int *nu, double *y, int *ny)
{
  int mlhs= 5 , mrhs= 8;
  NspObject * Args[9]; 
  NspObject * Ret[6];
  /* FIXME: give names to all */
  if ((Args[0]= scicos_itosci(NVOID,flag,1,1)) == NULL) goto err;
  if ((Args[1]= scicos_itosci(NVOID,nevprt,1,1)) == NULL) goto err;
  if ((Args[2]= scicos_dtosci(NVOID,t,1,1)) == NULL) goto err;
  if ((Args[3]= scicos_dtosci(NVOID,x,*nx,1)) == NULL) goto err;
  if ((Args[4]= scicos_mserial_to_obj(NVOID,z,*nz))== NULL) goto err;
  if ((Args[5]= scicos_mserial_to_obj(NVOID,rpar,*nrpar)) == NULL) goto err; 
  if ((Args[6]= scicos_itosci(NVOID,ipar,*nipar,1)) == NULL) goto err;
  if ((Args[8]= scicos_dtosci(NVOID,u,*nu,1)) == NULL) goto err;
  /*     macro execution */

  if ( scicos_scifunc(Args,mrhs,Ret,&mlhs) == FAIL) goto err;
  /*     transfer output variables to fortran */
  switch (*flag) 
    {
      /*     [y,x,z,tvec,xd]=func(flag,nevprt,t,x,z,rpar,ipar,u) */
    case 1: 
      /* y or z computation */
      scicos_scitod(z, *nz, 1,Ret[2]);
      scicos_scitod(x, *nx, 1,Ret[1]);
      scicos_scitod(y, *ny, 1,Ret[0]);
      break;
    case 0:
      scicos_scitod(xd, *nx, 1,Ret[4]);
      break;
    case 2: 
      /*  x'  computation */
      scicos_scitod(z, *nz, 1,Ret[2]);
      scicos_scitod(x, *nx, 1,Ret[1]);
      break;
    case 3:
      scicos_scitod(tvec, *ntvec, 1,Ret[3]);
      break;
    case 4: 
    case 5:
      scicos_scitod(z, *nz, 1,Ret[2]);
      scicos_scitod(x, *nx, 1,Ret[1]);
      break;
    case 6: 
      scicos_scitod(z, *nz, 1,Ret[2]);
      scicos_scitod(x, *nx, 1,Ret[1]);
      scicos_scitod(y, *ny, 1,Ret[0]);
      break;
    }
  return ;
 err: 
    *flag=-1;
    return;
} 


