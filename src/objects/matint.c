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

#define  Matint_Private 
#include "nsp/object.h"
#include "nsp/matint.h" 
#include "nsp/interf.h"

/* 
 * Interface NspMatint 
 * FIXME: should contains methods which are to be implemented 
 *        by all Objects which behaves like matrices. 
 * the only method actually implemented is redim 
 * Note that the interface is to be used at nsp level and 
 * internally.
 */

int nsp_type_matint_id=0;
NspTypeMatint *nsp_type_matint=NULL;

NspTypeMatint *new_type_matint(type_mode mode)
{
  NspTypeMatint *type = NULL;
  if ( nsp_type_matint != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_matint;
    }

  if ((type =  malloc(sizeof(NspTypeMatint)))==NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;

  /* specific methods for matint */
      
  type->init = NULL;
      
  /* 
   * Matint interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_matint_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatint called nsp_type_matint
       */
      type->id =  nsp_type_matint_id = nsp_new_type_id();
      nsp_type_matint = type;
      if ( nsp_register_type(nsp_type_matint) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_matint(mode);
    }
  else 
    {
      type->id = nsp_type_matint_id;
      return type;
    }
}

/* 
 * method redim 
 * A.redim[m,n] at nsp level 
 *   i.e an interface for redim
 *   this interface is in the methods table of the interface matint.
 *   we call the associated redim function which is supposed to be
 *   implemented in the matint interface. 
 */

static int int_matint_redim(NspMatrix *self,Stack stack,int rhs,int opt,int lhs)
{
  NspTypeBase *type;
  int m1, n1;
  CheckRhs (2,2);
  CheckLhs (0,0);
  if (GetScalarInt (stack, 1, &m1) == FAIL)    return RET_BUG;
  if (GetScalarInt (stack, 2, &n1) == FAIL)    return RET_BUG;
  /* interface is supposed to be initialized 
   *  nsp_type_matint= new_type_matint(T_BASE); 
   */
  /* search nsp_type_matint interface which is implemented 
   * by self 
   * FIXME: note that we have already searched for matint 
   *        before calling this function: maybe we should 
   *        add here the good type in the arg list ? ... 
   */
  if (( type = check_implements(self,nsp_type_matint_id)) == NULL )
    {
      Scierror("Object do not implements matint interface\n");
      return RET_BUG;
    }
  if ( MAT_INT(type)->redim(self,m1,n1) != OK) return RET_BUG;
  return 0;
}

static NspMethods matint_methods[] = {
  {"redim",(nsp_method *) int_matint_redim},
  { NULL, NULL}
};


NspMethods *matint_get_methods(void) { return matint_methods;};

