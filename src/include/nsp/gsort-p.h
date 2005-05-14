#ifndef SCI_SORT 
#define SCI_SORT 

#include "nsp/string.h" 

/*
 * This Software is (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

int C2F(gsort) (int *xI,double *xD,int *ind,int *iflag,int *m,int *n,nsp_const_string type,nsp_const_string  iord);
void C2F(gsorts) (char **data,int *ind,int *iflag,int *m,int *n,nsp_const_string type,nsp_const_string iord);

#endif


