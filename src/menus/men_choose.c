/*-------------------------------------------------------------------
 * This Software is (Copyright ENPC 1998-2003) 
 * Jean-Philippe Chancelier Enpc/Cermics
 *-------------------------------------------------------------------*/

#include "nsp/menus.h"

/*****************************************
 * Interface with a Scilab ``structure'' 
 *****************************************/

static char *button_def[]={"Cancel",NULL};

int nsp_choose(NspSMatrix *Items,NspSMatrix *Title,NspSMatrix *button,int *nrep)
{
  int Rep,choice=0 ;
  char **but_names; 
  String *descr =nsp_smatrix_elts_concat(Title,"\n",1,"\n",1);
  but_names = (button == NULL) ?  button_def : button->S  ; 
  Rep = nsp_choose_(descr,Items->S,Items->mn,but_names,1,&choice);
  *nrep= ( Rep == TRUE ) ? (1+ choice) : 0;
  StringDestroy(&descr);
  return OK;
}




