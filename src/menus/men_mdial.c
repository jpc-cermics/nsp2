/*-------------------------------------------------------------------
 * This Software is (Copyright ENPC 1998-2003) 
 * Jean-Philippe Chancelier Enpc/Cermics
 *-------------------------------------------------------------------*/

#include "nsp/menus.h"

/*************************************************     
 * interface with scilab 
 **********************************************************/

int  nsp_multi_dialog(NspSMatrix *Title,NspSMatrix *Labels,NspSMatrix *Init_values, int *cancel)
{
  int rep,ierr=0;
  char *labels  =nsp_smatrix_elts_concat(Title,"\n",1,"\n",1); 
  int nv =  Init_values->mn;
  if ( nv >  NPAGESMAX*NITEMMAXPAGE )
    {
      Scierror("Sorry : x_mdialog limited to %d items on Windows\n", NPAGESMAX*NITEMMAXPAGE );
      return FAIL;
    }
  rep =  nsp_multi_dialog_(labels,Labels->S,Init_values->S, nv, &ierr);
  StringDestroy(&labels);
  if ( ierr == 0 )
    {
      *cancel = ( rep == FALSE ) ? 1 : 0;
      return OK;
    }
  return FAIL;
}
