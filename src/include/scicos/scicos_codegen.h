#ifndef SCICOS_CODEGEN_H 
#define SCICOS_CODEGEN_H 

#include <nsp/nsp.h>
#include <nsp/object.h> 
#include <nsp/hash.h> 

/* structure definition of in/out sensors/actuators */
typedef struct {
  int typ;      /* data type */
  int ndims;    /* number of dims */
  int ndata;    /* number of data */
  int *dims;    /* size of data (length ndims) */
  double *time; /* date of data (length ndata) */
  void *data;   /* data (length ndata*prod(dims)) */
} scicos_inout;

#endif /* SCICOS_CODEGEN_H */

#ifdef SCICOS_CODEGEN 

/* forces the include of some static functions 
 */

/* faire le cas cplx == TRUE dans ce cas les donnes sont 
 * avec des complexes type scilab ? 
 */

static NspObject *scicos_inout_to_obj(scicos_inout *out,int cplx)
{
  NspMatrix *Values=NULL,*Times=NULL;
  NspHash *H=NULL;
  int i,j;
  /* 
   * create a hash table with two fields: values and time 
   *
   */
  int m=out->ndata;
  int n=out->dims[0]*out->dims[1];
  if (( Values = nsp_matrix_create("values",'r',m,n))==NULL) return NULL;
  if (out->dims[0]==1 && out->dims[1]==1) 
    {
      /* scalar case */
      for(i=0 ; i < m ; i++) Values->R[i] = *(((double *) out->data) +i);
    }
  else 
    {
      double *od = (double *) out->data;
      /* matrix and vector case */
      for(i=0 ; i < m ; i++) 
	for( j=0 ; j < n;j++)
	  Values->R[j*m + i] = od[i*n + j];
    }
  if (( Times = nsp_matrix_create("time",'r',m,1))==NULL) goto end;
  for(i=0 ; i < m ; i++) Times->R[i] = out->time[i];
  if (( H = nsp_hash_create(NVOID,2)) == NULLHASH) goto end;
  if (nsp_hash_enter(H,NSP_OBJECT(Times))== FAIL) goto end;
  if (nsp_hash_enter(H,NSP_OBJECT(Values))== FAIL) goto end;
  return NSP_OBJECT(H);
 end:
  if ( H != NULL) 
    nsp_hash_destroy(H);
  else 
    {
      if ( Times != NULL) nsp_matrix_destroy(Times);
      if ( Values != NULL) nsp_matrix_destroy(Values);
    }
  return NULL;
}


static int scicos_in_fill(scicos_inout *in,int type,int *in_dims,NspMatrix *M, int cmplx)
{
  double *d=(double *) in->data;
  int i,j;
  in->typ       = type;
  in->ndims     = 2;
  in->ndata     = M->m;
  in->dims      = in_dims;
  in->dims[0] = 1;
  in->dims[1] = M->n;
  if (( in->data = malloc(M->m*M->n*sizeof(double)))==NULL)
    {
      return FAIL;
    }
  for( i=0 ; i < M->m ;i++) 
    for(j=0; j < M->n ;j++) 
      d[ i*M->n + j] = M->R[i+ j*M->m];
  return OK;
}



static int scicos_out_fill(scicos_inout *out,int type,int *out_dims,int m,
			   int n, int cmplx)
{
  out->typ       = type;
  out->ndims     = 2;
  out->ndata     = 1000;
  out->dims      = out_dims;
  out->dims[0] = m;
  out->dims[1] = n;
  out->data = NULL;
  out->time = NULL;
  if (( out->data = malloc(m*n*(out->ndata)*sizeof(double)))==NULL)
    {
      return FAIL;
    }
  if (( out->time = malloc((out->ndata)*sizeof(double)))==NULL)
    {
      return FAIL;
    }
  return OK;
}

#endif 
