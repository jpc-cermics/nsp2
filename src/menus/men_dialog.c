/*-------------------------------------------------------------------
 * This Software is (Copyright ENPC 1998-2003) 
 * Jean-Philippe Chancelier Enpc/Cermics
 *-------------------------------------------------------------------*/

#include "nsp/menus.h"

/*************************************************     
 * dialog 
 * returns FAIL if the dialog was canceled 
 * returns OK   if the dialog was done 
 *     The the answer is in Rep 
 *     except if lack of memory happens, then Rep == NULLOBJ;
 **********************************************************/

int nsp_dialog(NspSMatrix *Title,NspSMatrix *Init,NspObject **Rep)
{
  NspSMatrix *S;
  char *answer = NULL;
  static char *buttons[]={"OK","Cancel",NULL};
  int rep,ierr=0 ;
  String *title =nsp_smatrix_elts_concat(Title,"\n",1,"\n",1);
  String *init =nsp_smatrix_elts_concat(Init,"\n",1,"\n",1);
  rep = nsp_dialog_(title,init, buttons,&ierr,&answer );
  StringDestroy(&init);
  StringDestroy(&title);
  if ( rep == FALSE)   return FAIL;
  /* \n must be converted */ 
  S =nsp_smatrix_split(answer,"\n");
  if ( S != NULLSMAT ) { S->m = S->n ; S->n=1;} /* column vector */
  *Rep = (NspObject *) S;
  FREE(answer);/** allocated in DialogOK **/
  return OK;
}

/*********************************************************
 * to open a dialog in a procedure 
 *********************************************************/

void nsp_dialog1(char *title, char *init, char **buttons,char *value,int *ok)
{
  char *answer = NULL;
  int rep,ierr=0;
  rep = nsp_dialog_(title,init,buttons,&ierr,&answer );
  if (rep == FALSE )
    *ok=0;
  else {
    *ok=1;
    strcpy(value,answer);
    FREE(answer);/** allocated in DialogOK **/
  }
}

