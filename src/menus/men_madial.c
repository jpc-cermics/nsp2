/*-------------------------------------------------------------------
 * This Software is (Copyright ENPC 1998-2003) 
 * Jean-Philippe Chancelier Enpc/Cermics
 *-------------------------------------------------------------------*/

#include "nsp/menus.h"

/*************************************************     
 * interface with scilab 
 **********************************************************/

int  nsp_matrix_dialog(NspSMatrix *Title,NspSMatrix *Labels_v,NspSMatrix *Labels_h,
		       NspSMatrix *Init_matrix,int *cancel)
{
  int rep,ierr=0;
  char *labels =nsp_smatrix_elts_concat(Title,"\n",1,"\n",1);
  if ( labels == NULL) return FAIL;
  rep =  nsp_matrix_dialog_(labels,Labels_v->S,Labels_h->S, Init_matrix->S, Labels_v->mn, Labels_h->mn,&ierr);
  StringDestroy(&labels);
  if ( ierr == 0) 
    {
      *cancel = ( rep == FALSE) ? 1 : 0;
      return OK;
    }
  return FAIL;
}
