#ifndef SCI_CONVERT 
#define SCI_CONVERT 

/*
 * This Software is ( Copyright ENPC 1998 ) 
 * Jean-Philippe Chancelier Enpc/Cergrene   
 */

extern int nsp_int2double(int *,int *,int *,double *,int *);
extern int nsp_double2int(int *,double *,int *);
extern int nsp_float2int(int *,float *,int *);
extern int nsp_float2double(int *,float *,int *,double *,int *);
extern int nsp_double2float(int *,double *,float *);
extern int nsp_int2float(int *,int *,int *,float *,int *);

/* online conversion of double arrays */

int nsp_convert_type_to_double(double *x,int n,const char *type);
int nsp_convert_double_to_type(double *x,int n,const char *type);

#endif

