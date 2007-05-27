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
 *
 * utility function for checking parsed expressions 
 * 
 *--------------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h>

#include "nsp/object.h"
#include "nsp/plistc.h"
#include "Functions.h" 

static int IsColConc (PList plist, PList *plist1, int *kount);
static int Isname (PList plist, PList *plist1, int *kount);
static int IsFeval (PList plist, PList *plist1, int *kount);


#define IsName(x) ((x)!= NULLPLIST && (x)->type ==NAME && (x)->next == NULLPLIST)
#define IsPList(x) ((x)!= NULLPLIST && (x)->type ==PLIST && (x)->next == NULLPLIST)

/**
 * nsp_check_is_mlhs:
 * @plist: 
 * @plist1: 
 * @kount: 
 * 
 * Returns: 
 */
/*
 * checks for  [<x>,<x>,.....,<x>] 
 * where x can be a <name> or <feval>:= f(...) 
 * ( scilab multiple left hand side )
 * inside plist a mlhs is coded as a set of colconcat 
 * as (((<name><name><COLCONCAT>) <name> COLCONCAT) MATRIX)
 * If a mlhs is recognised IsMlhs returns true 
 * and plist1 is set to 
 * ( <name> <name> .... MLHS) 
 * 
 * XXXXX Attention IsMlhs peut renvoyer False et plist1 peut-etre 
 * non vide : il faut alors le netoyer .
 * 
 * 
 **/
int nsp_check_is_mlhs(PList plist, PList *plist1, int *kount)
{
  PList loc;
  if (Isname(plist,plist1,kount)==OK) return(OK);
  if (IsFeval(plist,plist1,kount)==OK) return(OK);
  if (plist!= NULLPLIST && plist->type == PLIST 
      && plist->next == NULLPLIST)
    {
      loc = (PList) plist->O;
      /* check for (MATRIX ... ) */
      if ( loc->type == P_MATRIX ) 
	{
	  if (Isname(loc->next,plist1,kount) == OK) return(OK);
	  if (IsFeval(loc->next,plist1,kount)== OK) return(OK);
	  if (loc->next->type == PLIST) 
	    {
	      return( IsColConc((PList) loc->next->O,plist1,kount));
	    }
	}
    }
  return(FAIL);
}

/**
 * IsColConc:
 * @plist: 
 * @plist1: 
 * @kount: 
 *
 *  Returns: 
 */
/* 
 * if plist = ( <x> <y> COLCONCAT )
 * <y> := <name> or <feval> 
 * <x> := <name> or <feval> or (<x> <y> COLCONCAT) 
 * returns true and all the 
 *     <name> and <feval> are added at end of list plist1
 * 
 * Returns: 
 **/

static int IsColConc(PList plist, PList *plist1, int *kount)
{
  if ( plist->type != COLCONCAT ) return FAIL;
  plist = plist->next;
  if ( Isname(plist,plist1,kount) == OK 
       || IsFeval(plist,plist1,kount) == OK 
       || ( plist->type == PLIST 
	    && IsColConc((PList) plist->O,plist1,kount) == OK )
       )
    {
      if ( Isname(plist->next,plist1,kount)==OK
	   || IsFeval(plist->next,plist1,kount) == OK)
	return(OK) ;
      else 
	return FAIL;
    }
  return(FAIL);
}

/**
 * Isname:
 * @plist: 
 * @plist1: 
 * @kount: 
 *
 * Returns: 
 */
/*
 * if plist = ( <name> ..... )
 * returns true and <name> is added at end of list plist1
 * 
 **/
static int Isname(PList plist, PList *plist1, int *kount)
{
  if ( plist != NULLPLIST && plist->type == NAME )
    {
      if (nsp_parse_add_name(plist1,(char *) plist->O)  == FAIL) return(FAIL);
      (*kount)++;
      return(OK);
    }
  else 
    return(FAIL);
}


/**
 * IsFeval:
 * @plist: 
 * @plist1: 
 * @kount: 
 * 
 * 
 * if plist = ((.... FEVAL) ...... )
 * returns true and a copy of 
 * (.... FEVAL) is added at end of list plist1
 * 
 * also valid for LISTEVAL 
 * 
 * Returns: 
 **/
static int IsFeval(PList plist, PList *plist1, int *kount)
{
  if ( plist != NULLPLIST && plist->type == PLIST)
    {
      PList loc1 = (PList) plist->O,loc2;
      if ( loc1->type == FEVAL 
	   || loc1->type == LISTEVAL 
	   || loc1->type == CALLEVAL)  
	{
	  /* (((,,,,)) MATRIX) **/
	  /* we must perform a copy since plist will be destroyed */
	  if ((loc2 =nsp_plist_copy(loc1)) == NULLPLIST) return(FAIL);
	  if (nsp_parse_add_list(plist1,&loc2) == FAIL) return(FAIL);
	  (*kount)++;
	  return(OK);
	}
    }
  return FAIL;
}

/**
 * nsp_check_simple_listeval:
 * @plist: a #PList 
 * 
 * checks if @plist is a simple LISTEVAL 
 * i.e  (LISTEVAL name (ARGS ... ))
 * if true LISTEVAL is replaced by CALLEVAL 
 * (a function call or elt extraction)
 * 
 * Returns: %TRUE or %FALSE
 **/

int nsp_check_simple_listeval(PList plist)
{
  if ( plist->type == LISTEVAL
       && plist->arity == 2 
       && plist->next != NULL
       && plist->next->type == NAME 
       && plist->next->next != NULL 
       && plist->next->next->type == PLIST  
       && ((PList) plist->next->next->O)->type == ARGS )
    {
      plist->type = CALLEVAL; 
      return OK;
    }
  return FAIL;
}

/**
 * nsp_check_simple_mlhs:
 * @L: a #PList 
 * 
 * checks that @L is a MLHS just composed of names. 
 * 
 * Returns: %TRUE or %FALSE
 **/

int nsp_check_simple_mlhs(PList L)
{
  if ( L->type != MLHS ) return FAIL;
  L=L->next;
  while ( L  != NULLPLIST ) 
    {
      if (  L->type != NAME  ) return FAIL;
      L = L->next ;
    }
  return OK;
}



