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
#include "nsp/plistc.h"

static const char *xmatint_types[]={ "Mat","SMat","BMat","PMat","Cells",NULL};
static const char *xnum_and_str_types[]={ "Mat","SMat",NULL};
static const char *xnum_types[]={ "Mat",NULL};
static const char *xbool_types[]={ "BMat",NULL};

AcceleratedTab accelerated_tabs[] =
  {
    /* accelerated functions */
    { concatr_tab,     "concatr",     2,  xmatint_types, 0, NULL},
    { concatd_tab,     "concatd",     2,  xmatint_types, 0, NULL},
    { extract_tab,     "extract",     1,  xmatint_types, 0, NULL},
    { extractelts_tab, "extractelts", 1,  xmatint_types, 0, NULL},
    { extractcols_tab, "extractcols", 1,  xmatint_types, 0, NULL},
    { extractrows_tab, "extractrows", 1,  xmatint_types, 0, NULL},
    { resize2vect_tab, "resize2vect", 1,  xmatint_types, 0, NULL},
    { deleteelts_tab,  "deleteelts",  1,  xmatint_types, 0, NULL},
    { deletecols_tab,  "deletecols",  1,  xmatint_types, 0, NULL},
    { deleterows_tab,  "deleterows",  1,  xmatint_types, 0, NULL},
    { tozero_tab,      "tozero",      1,  xmatint_types, 0, NULL},
    { setrowscols_tab, "setrowscols", 1,  xmatint_types, 0, NULL},
    /* accelerated operators */
    { QUOTE_OP,          NULL,         1,  NULL, 0, NULL},
    { STAR_OP ,          NULL,         2,  xnum_types, 0, NULL},
    { PLUS_OP ,          NULL,         2,  xnum_and_str_types, 0, NULL},
    { HAT_OP,            NULL,         2,  xnum_types, 0, NULL},
    { COLON_OP,          NULL,         2,  xnum_types, 0, NULL},			      
    { OR_OP,             NULL,         2,  xbool_types, 0, NULL},			      
    { AND_OP,            NULL,         2,  xbool_types, 0, NULL},			      
    { TILDE_OP,          NULL,         1,  NULL, 0, NULL},			      
    { RETURN_OP,         NULL,         1,  NULL, 0, NULL},			      
    { COMMA_OP,          NULL,         1,  NULL, 0, NULL},			      
    { SEMICOLON_OP,      NULL,         1,  NULL, 0, NULL},		      
    { MINUS_OP,          NULL,         2,  xnum_types, 0, NULL},			      
    { SLASH_OP,          NULL,         2,  xnum_types, 0, NULL},			      
    { BACKSLASH_OP,      NULL,         2,  xnum_types, 0, NULL},		      
    { DOTSTAR ,          NULL,         2,  xnum_types, 0, NULL},			      
    { DOTSLASH,          NULL,         2,  xnum_types, 0, NULL},			      
    { DOTBSLASH,         NULL,         1,  NULL, 0, NULL},			      
    { DOTPLUS ,          NULL,         1,  NULL, 0, NULL},			      
    { STARDOT ,          NULL,         1,  NULL, 0, NULL},			      
    { SLASHDOT ,         NULL,         1,  NULL, 0, NULL},			      
    { BSLASHDOT ,        NULL,         1,  NULL, 0, NULL},			      
    { DOTSTARDOT,        NULL,         1,  NULL, 0, NULL},			      
    { DOTSLASHDOT,       NULL,         1,  NULL, 0, NULL},		      
    { DOTBSLASHDOT,      NULL,         1,  NULL, 0, NULL},		      
    { DOTHAT,            NULL,         2,  xnum_types, 0, NULL},			      
    { EQ  ,              NULL,         2,  xmatint_types, 0, NULL},			      
    { LEQ  ,             NULL,         2,  xnum_and_str_types, 0, NULL},			      
    { GEQ  ,             NULL,         2,  xnum_and_str_types, 0, NULL},			      
    { NEQ  ,             NULL,         2,  xmatint_types, 0, NULL},			      
    { DOTEQ ,            NULL,         1,  NULL, 0, NULL},			      
    { DOTLEQ ,           NULL,         1,  NULL, 0, NULL},			      
    { DOTLT  ,           NULL,         1,  NULL, 0, NULL},			      
    { DOTGEQ ,           NULL,         1,  NULL, 0, NULL},			      
    { DOTGT ,            NULL,         1,  NULL, 0, NULL},			      
    { DOTNEQ ,           NULL,         1,  NULL, 0, NULL},			      
    { DOTPRIM ,          NULL,         1,  NULL, 0, NULL},			      
    { MOINS  ,           NULL,         1,  NULL, 0, NULL},   
    { SEQAND ,           NULL,         1,  NULL, 0, NULL},   
    { SEQOR,             NULL,         1,  NULL, 0, NULL},   
    { LT_OP,             NULL,         2,  xnum_and_str_types, 0, NULL},
    { GT_OP,             NULL,         2,  xnum_and_str_types, 0, NULL},
    { undef_tab},
  };


static int get_type_max_id(const char *table[])
{
  int type_id=nsp_no_type_id ;
  const char **str = table ;
  while ( *str != NULL) 
    {
      NspTypeBase *type = nsp_get_type_from_name(*str);
      if ( type_id < type->id) type_id = type->id;
      str++;
    }
  return type_id;
}


static int init_func_tab1(AcceleratedTab *tab)
{
  int i, Int, Num;
  char op_typed[NAME_MAXL], *op, *name,  *nothing;
  const char *suffix;
  const char *str;
  
  if ( tab->accelerated_types == NULL) 
    {
      tab->length = 0;
      return OK;
    }

  /* find the max id of all the supported types */
  tab->length  = get_type_max_id(tab->accelerated_types);
  /* allocate func array and set to zero */
  if ( (tab->func=calloc(tab->length, sizeof(function *))) == NULL )
    {
      Sciprintf("Warning: enable to allocate the %s accelerated func tab\n",tab->opname);
      return FAIL;
    }

  if ( tab->opname == NULL) 
    tab->opname = nsp_astcode_to_nickname(tab->ops_id);

  str = tab->opname;
  op = op_typed;
  while ( *str != '\0' ) *op++ = *str++ ;
  *op++ = '_';

  i=0;
  while (tab->accelerated_types[i] != NULL) 
    {
      NspTypeBase *type = nsp_get_type_from_name(tab->accelerated_types[i]);
      name = op;
      suffix = nsp_get_short_string_from_id(type->id);
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
      if ( nsp_find_function(op_typed, &Int, &Num) == OK )
	(*(Interfaces[Int].info))(Num, &nothing, &(tab->func[type->id-1]));
      else
	Sciprintf("Warning: %s not found (building the accelerated table)\n", op_typed);
      /* Sciprintf("Found accelerated operation for %s\n",op_typed); */
      i++;
    }
  return OK;
}


/**
 * nsp_init_accelerated_tabs:
 * @void: 
 * 
 * Initialize static tables which contains functions 
 * pointers associated to specific data types. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_init_accelerated_tabs(void)
{
  int i=0; 
  AcceleratedTab *tab; 

  tab= accelerated_tabs;
  while ( tab->ops_id != undef_tab ) 
    {
      if ( i != tab->ops_id && i != tab->ops_id - NOTCODE_OP + setrowscols_tab) 
	{
	  Scierror("Warning: accelerated table is badly filled at position %d\n",i);
	  exit(1);
	}
      if ( init_func_tab1(tab) == FAIL ) return FAIL;
      tab++;
      i++;
    }
  return OK;
}



/**
 * nsp_get_fast_function:
 * @tab: an #AcceleratedTab pointer 
 * @type_id: an integer 
 * 
 * Searches if @tab contains a function to be used for arguments of 
 * type @type_id.
 * 
 * Returns: %NULL or a #function.
 **/

function *nsp_get_fast_function(AcceleratedTab *tab, int type_id)
{
  return ( type_id > tab->length ) ? NULL : tab->func[type_id-1];
}
