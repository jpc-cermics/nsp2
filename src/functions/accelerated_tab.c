/* Nsp
 * Copyright (C) 2006 Bruno Pinçon
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

#include "FunTab.h"  /* FindFunction */
#include "nsp/accelerated_tab.h"
#include "nsp/plisttoken.h" /*for name_maxl **/
#include "callfunc.h"

static int matint_types[5];

AcceleratedTab concatr_tab =     { "concatr",     2, 5, matint_types, 0, NULL};
AcceleratedTab concatd_tab =     { "concatd",     2, 5, matint_types, 0, NULL};
AcceleratedTab extract_tab =     { "extract",     1, 5, matint_types, 0, NULL};
AcceleratedTab extractelts_tab = { "extractelts", 1, 5, matint_types, 0, NULL};
AcceleratedTab extractcols_tab = { "extractcols", 1, 5, matint_types, 0, NULL};
AcceleratedTab extractrows_tab = { "extractrows", 1, 5, matint_types, 0, NULL};
AcceleratedTab resize2vect_tab = { "resize2vect", 1, 5, matint_types, 0, NULL};
AcceleratedTab deleteelts_tab =  { "deleteelts",  1, 5, matint_types, 0, NULL};
AcceleratedTab deletecols_tab =  { "deletecols",  1, 5, matint_types, 0, NULL};
AcceleratedTab deleterows_tab =  { "deleterows",  1, 5, matint_types, 0, NULL};
AcceleratedTab tozero_tab =      { "tozero",      1, 5, matint_types, 0, NULL};
AcceleratedTab setrowscols_tab = { "setrowscols", 1, 5, matint_types, 0, NULL};

static int init_func_tab(AcceleratedTab *tab)
{
  int i, type_id_max = 0, Int, Num;
  char op_typed[NAME_MAXL];
  char *str, *op, *name, *suffix, *nothing;

  /* find the max id of all the supported types */
  for ( i = 0 ; i < tab->nb_accelerated_types ; i++ )
    if ( tab->accelerated_types[i] > type_id_max )
      type_id_max = tab->accelerated_types[i];

  /* allocate func array */
  tab->length = type_id_max;
  if ( (tab->func=malloc(tab->length*sizeof(function *))) == NULL )
    {
      Sciprintf("Warning: enable to allocate the %s accelerated func tab\n",tab->opname);
      return FAIL;
    }
  for ( i = 0 ; i < tab->length ; i++ )
    tab->func[i] = (function *) 0;

  str = tab->opname;
  op = op_typed;
  while ( *str != '\0' ) *op++ = *str++ ;
  *op++ = '_';

  for ( i = 0 ; i < tab->nb_accelerated_types ; i++ )
    {
      name = op;
      suffix = nsp_get_short_string_from_id(tab->accelerated_types[i]);
      str = suffix;
      /* arity is supposed to be >= 1 and <= 2 */
      while ( *str != '\0' ) *name++ = *str++;
      if ( tab->arity == 2 )
	{
	  str = suffix;
	  *name++ = '_';
	  while ( *str != '\0' ) *name++ = *str++;
	}
      *name = '\0';
      if ( FindFunction(op_typed, &Int, &Num) == OK ) 
	(*(Interfaces[Int].info))(Num, &nothing, &(tab->func[tab->accelerated_types[i]-1]));
      else
	Sciprintf("Warning: %s not found (building the accelerated table)\n", op_typed);
    }
  return OK;
}

int nsp_init_accelerated_tabs(void)
{
  matint_types[0] = nsp_type_matrix_id;
  matint_types[1] = nsp_type_smatrix_id;
  matint_types[2] = nsp_type_bmatrix_id;
  matint_types[3] = nsp_type_pmatrix_id;
  matint_types[4] = nsp_type_cells_id;

  if ( init_func_tab(&concatr_tab) == FAIL ) return FAIL;
  if ( init_func_tab(&concatd_tab) == FAIL ) return FAIL;
  if ( init_func_tab(&extract_tab) == FAIL ) return FAIL;
  if ( init_func_tab(&extractelts_tab) == FAIL ) return FAIL;
  if ( init_func_tab(&extractcols_tab) == FAIL ) return FAIL;
  if ( init_func_tab(&extractrows_tab) == FAIL ) return FAIL;
  if ( init_func_tab(&resize2vect_tab) == FAIL ) return FAIL;
  if ( init_func_tab(&deleteelts_tab) == FAIL ) return FAIL;
  if ( init_func_tab(&deletecols_tab) == FAIL ) return FAIL;
  if ( init_func_tab(&deleterows_tab) == FAIL ) return FAIL;
  if ( init_func_tab(&tozero_tab) == FAIL ) return FAIL;
  if ( init_func_tab(&setrowscols_tab) == FAIL ) return FAIL;
     
  return OK;
}

function *nsp_get_fast_function(AcceleratedTab *tab, int type_id)
{
  return ( type_id > tab->length ) ? NULL : tab->func[type_id-1];
}
