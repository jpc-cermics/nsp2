/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <math.h>
#include <stdio.h>
#include <string.h>

#define  GRint_Private 
#include "nsp/object.h"

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/grint.h" 

/* graphic object interface 
 * NspGRint inherits from NspObject 
 */

int nsp_type_grint_id=0;
NspTypeGRint *nsp_type_grint=NULL;

NspTypeGRint *new_type_grint(type_mode mode)
{
  NspTypeObject *top;
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

  top = NSP_TYPE_OBJECT (type->surtype);
  while (top->surtype != NULL)
    top = NSP_TYPE_OBJECT (top->surtype);

  /* object methods redefined for matint */

  top->s_type = (s_type_func *) grint_type_as_string;

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


/* these method is necessary when registering types */

static char grint_type_name[] = "Grint";

static char *grint_type_as_string (void)
{
  return (grint_type_name);
}


