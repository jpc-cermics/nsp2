/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998 )                          
 * Jean-Philippe Chancelier Enpc/Cergrene                            
 *-------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <malloc.h>
#include <string.h>

#define  GRint_Private 
#include "nsp/object.h"

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "../zcalelm/convert.h"
#include "grint.h" 

/* graphic object interface 
 * NspGRint inherits from NspObject 
 */

int nsp_type_grint_id=0;
NspTypeGRint *nsp_type_grint=NULL;

NspTypeGRint *new_type_grint(type_mode mode)
{
  NspTypeGRint *type = NULL;
  if ( nsp_type_grint != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_grint;
    }

  if ((type =  malloc(sizeof(NspTypeGRint)))==NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;

  /* specific methods for grint */
      
  type->init = NULL;
      
  /* 
   * GRint interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_grint_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGRint called nsp_type_grint
       */
      type->id =  nsp_type_grint_id = nsp_new_type_id();
      nsp_type_grint = type;
      if ( nsp_register_type(nsp_type_grint) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_grint(mode);
    }
  else 
    {
      type->id = nsp_type_grint_id;
      return type;
    }
}



