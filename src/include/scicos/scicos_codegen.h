#ifndef SCICOS_CODEGEN_H 
#define SCICOS_CODEGEN_H 
#endif 

#ifdef SCICOS_CODEGEN 

/* faire le cas cplx == TRUE dans ce cas les donnes sont 
 * avec des complexes type scilab ? 
 */

static NspObject *scicos_inout_to_obj(scicos_inout *out,int cplx)
{
  NspMatrix *Values=NULL,*Times=NULL;
  NspHash *H=NULL;
  int i;
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
      for(i=0 ; i < m ; i++) Values->R[i] = out->data[i];
    }
  else 
    {
      /* matrix and vector case */
      for(i=0 ; i < m ; i++) 
	for( j=0 ; j < n;j++)
	  Values->R[j*m + i] = out->data[i*n + j];
    }
  if (( Times = nsp_matrix_create("time",'r',m,1))==NULL) goto end;
  for(i=0 ; i < m ; i++) Times->R[i] = out->time[i];
  if (( H = nsp_hash_create(NVOID,2)) == NULLHASH) goto end;
  if (nsp_hash_enter(H,NSP_OBJECT(Time))== FAIL) goto end;
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
    {
      for(j=0; j < M->n ;j++) 
	{
	  in->data[ i*M->n + j] = M->R[i+ j*M->m];
	}
    }
  return OK;
}



static int scicos_out_fill(scicos_inout *out,int type,int *out_dims,m,n, int cmplx)
{
  double *data; 
  out->typ       = type;
  out->ndims     = 2;
  out->ndata     = 1000;
  out->dims      = out_dims;
  out->dims[0] = m;
  out->dims[1] = n;
  if (( out->data = malloc(m*n*(out->ndata)*sizeof(double)))==NULL)
    {
      return FAIL;
    }
  if (( out->time = malloc((out->ndata)*sizeof(double)))==NULL) ==NULL)
    {
      return FAIL;
    }
  return OK;
}

#endif 
