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


static int scicos_scifunc(  NspObject **Args,int mrhs,NspObject **Ret, int *mlhs ) 
{
  switch (Scicos->params.scsptr_flag ) 
    {
    case  fun_macros: 
      /* Sciprintf("Evaluate a given macro\n");
       * nsp_object_print( Scicos->params.scsptr,0,0,0);
       */
       break;
    case fun_macro_name:
      Scierror("To be done scicos_scifunc_n evaluate a macro given by name\n");
      nsp_object_print( Scicos->params.scsptr,0,0,0);break;
      return FAIL;
    case fun_pointer: 
      Scierror("Internal error: Expecting a macro or macro name\n");
      return FAIL;
    }
  return nsp_gtk_eval_function((NspPList *) Scicos->params.scsptr,Args, mrhs, Ret, mlhs);
}

static NspMatrix *scicos_itosci(const int x[],int mx,int nx) 
{
  int i;
  NspMatrix *M;
  if ((M = nsp_matrix_create(NVOID,'r',mx,nx)) == NULLMAT) return NULLMAT; 
  for ( i = 0 ; i < M->mn; i++) M->R[i]=(double) x[i];
  return M;
}

static NspMatrix *scicos_dtosci(const double x[],int mx,int nx) 
{
  int i;
  NspMatrix *M;
  if ((M = nsp_matrix_create(NVOID,'r',mx,nx)) == NULLMAT) return NULLMAT; 
  for ( i = 0 ; i < M->mn; i++) M->R[i]= x[i];
  return M;
}

static NspSMatrix *scicos_str2sci(nsp_const_string x)
{
  return nsp_smatrix_create(NVOID,1,1,x,1);
}

static void scicos_scitovv(double x[],int nx, NspObject *Ob )
{
  NspMatrix *M= ((NspMatrix *) Ob);
  int i; 
  for ( i= 0 ; i < Min(nx,M->mn) ; i++) x[i]=M->R[i];
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

static int scicos_obj_to_mserial(double *x,int nx, NspObject *Obj )
{
  int i; 
  NspObject *S;
  NspMatrix *A;;
  if ((S = nsp_object_serialize(Obj))== NULLOBJ) return FAIL;
  /* serialize in a matrix */
  if ((A = nsp_serial_to_matrix((NspSerial *) S))== NULLMAT) 
    {
      nsp_object_destroy(&S);
      return FAIL;
    }
  if ( A->mn != nx ) 
    {
      Sciprintf("Error: cannot store a serialized nsp object (size %d) in double array (soze %d)\n",
		A->mn,nx);
    }
  for ( i= 0 ; i < A->mn; i++) x[i]= A->R[i];
  nsp_matrix_destroy(A);
  return OK;
}

/* XXXX: les vv fonctions sont des fonctions avec serialization */

/**
 * scicos_vvtosci:
 * @: 
 * @nx: 
 * 
 * XXX, to be removed an directly inserted in next function 
 * scicos_mserial_to_obj.
 * 
 * Return value: 
 **/

static NspMatrix *scicos_vvtosci(const double x[],int nx) 
{
  return nsp_matrix_create_from_array(NVOID,1,nx,x,NULL);
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

static NspObject *scicos_mserial_to_obj(const double *x,int nx)
{
  NspMatrix *Z=NULL;
  NspSerial *S=NULL;
  NspObject *Obj=NULL;
  if ((Z = scicos_vvtosci(x,nx)) == NULL) goto err;
  /* unserialize z in two steps */
  if ((S= nsp_matrix_to_serial(Z ))== NULL) goto err;
  if ((Obj=nsp_object_unserialize(S))== NULLOBJ) goto err;
 err:
  if ( S != NULL) nsp_serial_destroy(S);
  if ( Z != NULL) nsp_matrix_destroy(Z);
  return Obj;
}



static int scicos_scitod(double x[],int mx,int nx, NspObject *Ob)
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

static int scicos_scitoi(int x[],int mx,int nx, NspObject *Ob)
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

static NspObject *scicos_vars_to_list(double *inptr[],int nin,int insz[])
{
  int k;
  NspObject *Ob;
  if ((Ob = (NspObject *) nsp_list_create("L") ) == NULL) return NULL;
  for( k=0 ; k < nin ; k++) 
    {
      NspObject *elt;
      if ((elt = (NspObject *) scicos_dtosci(inptr[k],insz[k],1))== NULL)
	{
	  nsp_list_destroy((NspList *) Ob);
	  return NULL;
	}
      if ( nsp_list_insert((NspList *) Ob,elt,k+1) == FAIL)
	{
	  nsp_list_destroy((NspList *) Ob);
	  return NULL;
	}
    }
  return Ob;
}
 
/* XXX: note that the array z transmited here is suposed 
 * to be a nsp object serialized in a matrix. 
 * Thus we have to serialize/unserialize here.
 *
 *
 */

void  sciblk2(int *flag, int *nevprt, double *t, double *xd, double *x, int *nx, double *z,
	      int *nz, double *tvec, int *ntvec, double *rpar, int *nrpar, int *ipar, 
	      int *nipar, double **inptr, int *insz, int *nin, double **outptr, 
	      int *outsz, int *nout)
{
  int mlhs=5,mrhs=8;
  NspObject * Args[8];
  NspObject * Ret[5];
  
  /* FIXME: give names to all */
  if ((Args[0]= (NspObject *) scicos_itosci(flag,1,1)) == NULL) goto err;
  if ((Args[1]= (NspObject *) scicos_itosci(nevprt,1,1)) == NULL) goto err;
  if ((Args[2]= (NspObject *) scicos_dtosci(t,1,1)) == NULL) goto err;
  if ((Args[3]= (NspObject *) scicos_dtosci(x,*nx,1)) == NULL) goto err;
  if ((Args[4]= (NspObject *) scicos_mserial_to_obj(z,*nz))== NULL) goto err;
  if ((Args[5]= (NspObject *) scicos_vvtosci(rpar,*nrpar)) == NULL) goto err; 
  if ((Args[6]= (NspObject *) scicos_itosci(ipar,*nipar,1)) == NULL) goto err;
  if ((Args[7]= scicos_vars_to_list(inptr,*nin,insz))==NULLOBJ) goto err;

  /* function to be evaluated or name of function to be evaluated */

  if ( scicos_scifunc(Args,mrhs,Ret,&mlhs) == FAIL) goto err;

  switch (*flag) 
    {
    case 1 :
      if ( scicos_obj_to_mserial(z,*nz,Ret[2])== FAIL) goto err;
      /* scicos_scitovv(z,*nz,Ret[2]); */
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
      /* scicos_scitovv(z,*nz,Ret[2]); */
      scicos_scitod(x,*nx,1,Ret[1]);
      break;
    case 3 :
      scicos_scitod(tvec,*ntvec,1,Ret[3]);
      break;
    case 4 :
    case 5 :
      if ( scicos_obj_to_mserial(z,*nz,Ret[2])== FAIL) goto err;
      /* scicos_scitovv(z,*nz,Ret[2]); */
      scicos_scitod(x,*nx,1,Ret[1]);
      break;
    case 6 :
      if ( scicos_obj_to_mserial(z,*nz,Ret[2])== FAIL) goto err;
      /* scicos_scitovv(z,*nz,Ret[2]);*/
      scicos_scitod(x,*nx,1,Ret[1]);
      if ( *nout !=0 ) 
	{
	  if ( scicos_list_to_vars(outptr,*nout,outsz,Ret[0])==FAIL) goto err;
	}
      break;
    }
  /* XXX : we must clear Ret variables */
  return;
 err: 
    *flag=-1;
}


void sciblk2i(int *flag, int *nevprt, double *t, double *residual, double *xd, double *x, 
	      int *nx, double *z, int *nz, double *tvec, int *ntvec, double *rpar, int *nrpar, 
	      int *ipar, int *nipar, double **inptr, int *insz, int *nin, double **outptr, 
	      int *outsz, int *nout)
{
  int mlhs=6,mrhs=9;
  /*
  [y,  x,  z,  tvec,xd]=func(flag,nevprt,t,xd,x,z,rpar,ipar,u)
  [y,  x,  z,  tvec,res]=func(flag,nevprt,t,xd,x,z,rpar,ipar,u)
  */
  NspObject * Args[10]; 
  NspObject * Ret[7];

  /* FIXME: give names to all */
  if ((Args[0]= (NspObject *) scicos_itosci(flag,1,1)) == NULL) goto err;
  if ((Args[1]= (NspObject *) scicos_itosci(nevprt,1,1)) == NULL) goto err;
  if ((Args[2]= (NspObject *) scicos_dtosci(t,1,1)) == NULL) goto err;
  if ((Args[3]= (NspObject *) scicos_dtosci(xd,*nx,1)) == NULL) goto err;
  if ((Args[4]= (NspObject *) scicos_dtosci(x,*nx,1)) == NULL) goto err;
  if ((Args[5]= (NspObject *) scicos_vvtosci(z,*nz)) == NULL) goto err;
  if ((Args[6]= (NspObject *) scicos_vvtosci(rpar,*nrpar)) == NULL) goto err; 
  if ((Args[7]= (NspObject *) scicos_itosci(ipar,*nipar,1)) == NULL) goto err;
  if ((Args[8]= (NspObject *) nsp_list_create("L") ) == NULL) goto err;
  if ((Args[9]= scicos_vars_to_list(inptr,*nin,insz))==NULLOBJ) goto err;

  if ( scicos_scifunc(Args,mrhs,Ret,&mlhs) == FAIL) goto err;

  switch (*flag) 
    {
    case 1 :
      /* y  computation */
      /* int Ret[0] */
      if ( *nout != 0 ) 
	{
	  if ( scicos_list_to_vars(outptr,*nout,outsz,Ret[0])==FAIL) goto err;
	}
      break;
    case 0 :
      /*  residual  computation */
      scicos_scitod(residual,*nx,1,Ret[4]);
      break;
    case 2 : 
      /* continuous and discrete state jump */
      scicos_scitod(xd,*nx,1,Ret[4]);
      scicos_scitovv(z,*nz,Ret[2]);
      scicos_scitod(x,*nx,1,Ret[1]);
      break;
    case 3 :
      /* output event */
      scicos_scitod(tvec,*ntvec,1,Ret[3]);
      break;
    case 4 :
      scicos_scitod(xd,*nx,1,Ret[4]);
      scicos_scitovv(z,*nz,Ret[2]);
      scicos_scitod(x,*nx,1,Ret[1]);
      break;
    case 5 :
      scicos_scitod(xd,*nx,1,Ret[4]);
      scicos_scitovv(z,*nz,Ret[2]);
      scicos_scitod(x,*nx,1,Ret[1]);
      break;
    case 6 :
      scicos_scitod(xd,*nx,1,Ret[4]);
      scicos_scitovv(z,*nz,Ret[2]);
      scicos_scitod(x,*nx,1,Ret[1]);
      if (*nout !=0) 
	{
	  if ( scicos_list_to_vars(outptr,*nout,outsz,Ret[0])==FAIL) goto err;
	}
      break;
    }
  return;
 err: 
  *flag=-1;
}



void sciblk4(scicos_block *Blocks, int flag)
{
  int mlhs=1,mrhs=2;
  NspObject *Ob;
  NspHash *H;
  NspObject * Args[31]; 
  NspObject * Ret[5];
  int p = 0;
  /* this are the tlist names */
  /* 
     char *str[]={ "scicos_block","nevprt","funpt","type",
     "scsptr","nz","z","nx","x","xd","res","nin",
     "insz","inptr","nout","outsz","outptr","nevout",
     "evout","nrpar","rpar","nipar","ipar","ng","g",
     "ztyp","jroot","label","work","nmode","mode"};
  */
  if ((Args[p++]= (NspObject *)  scicos_itosci(&Blocks->nevprt,1,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_itosci(Blocks->funpt,0,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_itosci(&Blocks->type,1,1))== NULL) goto err;
  /* if ((Args[p++]= (NspObject *)  scicos_itosci(&Blocks->scsptr,0,1))== NULL) goto err; */
  if ((Args[p++]= (NspObject *)  scicos_itosci(&Blocks->nz,1,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_vvtosci(Blocks->z,Blocks->nz))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_itosci(&Blocks->nx,1,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_dtosci(Blocks->x,Blocks->nx,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_dtosci(Blocks->xd,Blocks->nx,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_dtosci(Blocks->res,Blocks->nx,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_itosci(&Blocks->nin,1,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_itosci(Blocks->insz,Blocks->nin,1))== NULL) goto err;
  if ((Args[p++]= scicos_vars_to_list(Blocks->inptr,Blocks->nin,Blocks->insz))==NULLOBJ) goto err;
  if ((Args[p++]= (NspObject *)  scicos_itosci(Blocks->outsz,Blocks->nout,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_itosci(&Blocks->nout,1,1))== NULL) goto err;
  if ((Args[p++]= scicos_vars_to_list(Blocks->outptr,Blocks->nout,Blocks->outsz))==NULLOBJ) goto err;
  if ((Args[p++]= (NspObject *)  scicos_itosci(&Blocks->nevout,1,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_dtosci(Blocks->evout,Blocks->nevout,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_itosci(&Blocks->nrpar,1,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_vvtosci(Blocks->rpar,Blocks->nrpar))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_itosci(&Blocks->nipar,1,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_itosci(Blocks->ipar,Blocks->nipar,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_itosci(&Blocks->ng,1,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_dtosci(Blocks->g,Blocks->ng,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_itosci(&Blocks->ztyp,1,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_itosci(Blocks->jroot,Blocks->ng,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_str2sci(Blocks->label))== NULL) goto err;
  /* if ((Args[p++]= (NspObject *)  scicos_vvtosci(Blocks->work,0))== NULL) goto err; */
  if ((Args[p++]= (NspObject *)  scicos_itosci(&Blocks->nmode,1,1))== NULL) goto err;
  if ((Args[p++]= (NspObject *)  scicos_itosci(Blocks->mode,Blocks->nmode,1))== NULL) goto err; 
  if ((Args[p++]= (NspObject *)  scicos_itosci(Blocks->mode,Blocks->nmode,1))== NULL) goto err; 

  if ( scicos_scifunc(Args,mrhs,Ret,&mlhs) == FAIL) goto err;

  H=(NspHash *) Ret[0];
  switch (flag) {
  case 1 :
    /* y computation */
    if (Blocks->nout!=0) 
      {
	/* 16ieme element de la tlist y */
	if ( nsp_hash_find(H,"y",&Ob) == FAIL) goto err;
	if ( scicos_list_to_vars(Blocks->outptr,Blocks->nout,Blocks->outsz,Ob)==FAIL) goto err;
      }
    break;
  case 0 :
    /*  x'  computation */
    /* 9 ieme element de la tlist xd */
    if (Blocks->nx != 0)
      {
	if ( nsp_hash_find(H,"xd",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->xd,Blocks->nx,1,Ob);
	/* 10 ieme element de la tlist res */
	if ( nsp_hash_find(H,"res",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->res,Blocks->nx,1,Ob);
      }
      break;
  case 2 :
    /* 6ieme element de la tlist z */
    if (Blocks->nz != 0) 
      {
	if ( nsp_hash_find(H,"z",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->z,Blocks->nz,1,Ob);
      }
    /* 8 ieme element de la tlist x */
    if (Blocks->nx != 0)
      {
	if ( nsp_hash_find(H,"x",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->x,Blocks->nx,1,Ob);
	/* 9 ieme element de la tlist xd */
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
    if (Blocks->nz != 0) 
      {
	if ( nsp_hash_find(H,"z",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->z,Blocks->nz,1,Ob);
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
  case 5 :
    if (Blocks->nz != 0) 
      {
	if ( nsp_hash_find(H,"z",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->z,Blocks->nz,1,Ob);
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
	/* 6ieme element de la tlist z */
	if ( nsp_hash_find(H,"z",&Ob) == FAIL) goto err;
	scicos_scitod(Blocks->z,Blocks->nz,1,Ob);
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
    /* 16ieme element de la tlist y */
    if (Blocks->nout!=0) 
      {
	/* 16ieme element de la tlist y */
	if ( nsp_hash_find(H,"y",&Ob) == FAIL) goto err;
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
  return;
 err: 
  flag=-1;
}





void scicos_sciblk(int *flag, int *nevprt, double *t, double *xd, double *x, int *nx,
		   double *z, int *nz, double *tvec, int *ntvec, double *rpar, int *nrpar,
		   int *ipar, int *nipar, double *u, int *nu, double *y, int *ny)
{
  int mlhs= 5 , mrhs= 8;
  /*     Copyright INRIA */
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
  NspObject * Args[9]; 
  NspObject * Ret[6];
  /* FIXME: give names to all */
  if ((Args[0]= (NspObject *) scicos_itosci(flag,1,1)) == NULL) goto err;
  if ((Args[1]= (NspObject *) scicos_itosci(nevprt,1,1)) == NULL) goto err;
  if ((Args[2]= (NspObject *) scicos_dtosci(t,1,1)) == NULL) goto err;
  if ((Args[3]= (NspObject *) scicos_dtosci(x,*nx,1)) == NULL) goto err;
  if ((Args[4]= (NspObject *) scicos_vvtosci(z,*nz)) == NULL) goto err;
  if ((Args[5]= (NspObject *) scicos_vvtosci(rpar,*nrpar)) == NULL) goto err; 
  if ((Args[6]= (NspObject *) scicos_itosci(ipar,*nipar,1)) == NULL) goto err;
  if ((Args[8]= (NspObject *) scicos_dtosci(u,*nu,1)) == NULL) goto err;
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


