#ifndef SCI_SORT 
#define SCI_SORT 

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

int C2F(gsort) (int *xI,double *xD,int *ind,int *iflag,int *m,int *n,char *type,char *iord);
void C2F(gsorts) (char **data,int *ind,int *iflag,int *m,int *n,char *type,char *iord);

#endif


