/* Nsp
 * Copyright (C) 1998-2010 Jean-Philippe Chancelier Enpc/Cermics
 * Copyright (C) 2005-2010 Bruno Pinçon Esial/Iecn
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
#include <stdlib.h>
#include <string.h>

#include <nsp/object.h> 
#include <nsp/matrix.h> 
#include <nsp/bmatrix.h> 
#include <nsp/smatrix.h> 
#include <nsp/imatrix.h> 
#include <nsp/sprowmatrix.h> 
#include <nsp/spcolmatrix.h> 
#include <nsp/matint.h> 
#include <nsp/file.h> 
#include <nsp/type.h> 
#include <nsp/hobj.h> 
#include <nsp/list.h> 

/* FIXME */
extern NspObject *nsp_eval_macro_code(NspPList *PL, NspObject **O, NspList *args,int *first) ; 

/*
 *  Doubly linked lists (NspList of Objects )
 *  basic Objects are defined in Obj.c 
 *
 *  a quick overview of NspList : (to be continued ...)
 *     
 */

/**
 *nsp_list_create:
 * @name: name of the list or %NVOID.
 * 
 * Creates a new empty list with name @name 
 * 
 * Return value: a new #NspList or %NULLLIST
 **/

NspList*nsp_list_create(const char *name)
{
  NspList *Loc = new_list();

  if ( Loc == NULLLIST)
    {
      Scierror("Error:\t running out of memory\n");
      return NULLLIST;
    }
  if (name != NULLSTRING) 
    { 
      if ( nsp_object_set_initial_name(NSP_OBJECT(Loc),name) == NULL)
	return NULLLIST;
    }
  else 
    {
      NSP_OBJECT(Loc)->name = NULLSTRING;
    }
  NSP_OBJECT(Loc)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  /*
    Loc->otype = LIST;
    Loc->ftype = List_Type;
  */
  return Loc;
}


/**
 *nsp_cell_create:
 * @O:  #NspObject to be inserted 
 * 
 * creates a new #Cell in which object @O is stored.
 * Note that @O can be a %NULLOBJ
 * 
 * Return value: a new #Cell or %NULLOBJ
 **/

Cell *nsp_cell_create(NspObject *O)
{
  Cell *Loc;
  Loc = ( Cell *) MALLOC(sizeof( Cell));
  if (Loc == NULLCELL)
    {
      Scierror("Error:\tNo more space\n");
      return NULLCELL;
    }
  /* XXXXX : this is important to check !!!! */
  if (O != NULLOBJ &&  strcmp(nsp_object_get_name(O),NVOID) == 0) 
    {
      Sciprintf("Error:\ta cell is unnamed, something wrong in List.c\n");
    }
  Loc->O = O;
  Loc->prev = Loc->next = NULLCELL;
  return Loc;
} 


/**
 *nsp_cell_destroy:
 * @c: a pointer to a #Cell pointer 
 * 
 * deletes the #Cell @c and the object stored in it
 * 
 **/

void nsp_cell_destroy(Cell **c)
{
  if ((*c) != NULLCELL)
    {
      nsp_object_destroy(&(*c)->O);
      FREE((*c));
    }
} 


/**
 *nsp_list_destroy:
 * @l: a #NspList 
 * 
 * deletes the #NspList and all its elements.
 * 
 **/

void nsp_list_destroy(NspList *l)
{
  if (l != NULLLIST)
    {
      Cell *loc,*loc1;
      nsp_object_destroy_name(NSP_OBJECT(l));
      loc = l->first;
      while ( loc != NULLCELL) 
	{
	  loc1= loc->next ;
	  nsp_cell_destroy(&loc);
	  loc = loc1;
	}
      FREE(l);
    }
} 

/**
 *nsp_list_destroy_bis:
 * @l: a #NspList 
 * 
 * deletes the #NspList but not the elements.
 * 
 **/

void nsp_list_destroy_bis(NspList *l)
{
  if (l != NULLLIST)
    {
      Cell *loc,*loc1;
      nsp_object_destroy_name(NSP_OBJECT(l));
      loc = l->first;
      while ( loc != NULLCELL) 
	{
	  loc1= loc->next ;
	  FREE(loc);
	  loc = loc1;
	}
      FREE(l);
    }
} 


/**
 *nsp_list_copy:
 * @L: a #NspList 
 * 
 * returns a full copy of the #NspList @L.
 * Elements inside the list are copied too
 * 
 * 
 * Return value: a new #NspList or %NULLLIST 
 **/

NspList*nsp_list_copy(NspList *L)
{
  NspList *Loc;
  Cell *cloc,*cloc1=NULLCELL,*cloc2=NULLCELL;
  if ( (Loc=nsp_list_create(NVOID)) == NULLLIST ) return NULLLIST;
  cloc = L->first ;
  while ( cloc != NULLCELL ) 
    {
      NspObject *Oloc;
      if ( cloc->O == NULLOBJ ) 
	Oloc = NULLOBJ;
      else 
	{
	  if ((Oloc = nsp_object_copy_with_name(cloc->O))== NULLOBJ) return NULLLIST;
	}
      if ((cloc1 =nsp_cell_create(Oloc))== NULLCELL) return NULLLIST;
      if ( cloc->prev == NULLCELL) 
	{
	  Loc->first = cloc1;
	}
      else 
	{
	  cloc1->prev = cloc2;
	  cloc2->next = cloc1;
	}
      cloc2= cloc1;
      cloc = cloc->next;
    }
  Loc->nel = L->nel;
  Loc->last = cloc1;
  /* eventuellement copier current et icurrent */
  return Loc;
} 

/**
 *nsp_list_full_copy:
 * @L: a #NspList 
 * 
 * returns a full copy of the #NspList @L.
 * Elements inside the list are copied too
 * 
 * 
 * Return value: a new #NspList or %NULLLIST 
 **/

NspList*nsp_list_full_copy(NspList *L)
{
  NspList *Loc;
  Cell *cloc,*cloc1=NULLCELL,*cloc2=NULLCELL;
  if ( (Loc=nsp_list_create(NVOID)) == NULLLIST ) return NULLLIST;
  cloc = L->first ;
  while ( cloc != NULLCELL ) 
    {
      NspObject *Oloc;
      if ( cloc->O == NULLOBJ ) 
	Oloc = NULLOBJ;
      else 
	{
	  if ((Oloc =nsp_object_full_copy(cloc->O))== NULLOBJ) return NULLLIST;
	  if (nsp_object_set_name(Oloc,nsp_object_get_name(cloc->O)) == FAIL)
	    return NULLLIST;
	}
      if ((cloc1 =nsp_cell_create(Oloc))== NULLCELL) return NULLLIST;
      if ( cloc->prev == NULLCELL) 
	{
	  Loc->first = cloc1;
	}
      else 
	{
	  cloc1->prev = cloc2;
	  cloc2->next = cloc1;
	}
      cloc2= cloc1;
      cloc = cloc->next;
    }
  Loc->nel = L->nel;
  Loc->last = cloc1;
  /* eventuellement copier current et icurrent */
  return Loc;
} 


/**
 *nsp_list_extract:
 * @L: a #NspList 
 * @Elts: indices of elements to extract given in a #NspMatrix.
 * 
 * extracts the Elements of a list @L. The elements 
 * to be extracted are givent by their indices in the list 
 * through the #NspMatrix @Elts. The extracted elements are 
 * copied and returned in a new List.
 * 
 * Return value:  a new #NspList or %NULLLIST 
 **/
NspList *nsp_list_extract(NspList *L, NspMatrix *Elts)
{
  int i;
  NspList *Loc;
  NspObject *O;

  if ( (Loc =nsp_list_create(NVOID)) == NULLLIST ) return NULLLIST;
  for ( i = 0 ; i < Elts->mn ; i++ )
    {
      /* nsp_list_get_element take care of out range indices and also
         of NULLOBJ (and send error messages) */
      if ( (O = nsp_list_get_element(L,(int) Elts->R[i])) == NULLOBJ )
	goto err;
      if ( (O =nsp_object_copy(O)) == NULLOBJ ) goto err;
      if ( nsp_object_set_name(O,"lel") == FAIL ) goto err;
      if ( nsp_list_end_insert(Loc,O) == FAIL ) goto err;
    }
  return Loc;

 err:
  nsp_list_destroy(Loc);
  return NULLLIST;
} 

/**
 *nsp_list_get_cell_pointer:
 * @L: a #NspList
 * @n: an integer 
 * 
 * returns a pointer to the @n-th Cell of a List. 
 * CAUTION : no test is done on @n (1 <= @n =< @L->nel)
 * 
 * Return value:  a #Cell or %NULLCELL 
 **/

Cell *nsp_list_get_cell_pointer(NspList *L, int n)
{
  int count=1;
  Cell *cell;

  if ( L->icurrent > 0 )
    {
      if ( n + n <= L->icurrent )                /* start from first and move forward */
	{
	  cell = L->first;
	  while ( count < n ) 
	    { cell = cell->next; count++; }
	}
      else if ( n < L->icurrent )                /* start from current  and move backward */
	{
	  cell = L->current;
	  do
	    { cell = cell->prev; count++; }
	  while ( count <=  L->icurrent - n );
	}
      else if ( n + n <= L->nel + L->icurrent )  /* start from current and move forward */
	{
	  cell = L->current;
	  while ( count < n - L->icurrent + 1 )
	    { cell = cell->next; count++;}
	}
      else                                       /* start from last  and move backward */
	{
	  cell = L->last;
	  while ( count <= L->nel - n )
	    { cell = cell->prev; count++; }
	}
    }
  else
    {
      if ( n + n <= L->nel )
	{
	  cell = L->first;
	  while ( count < n ) 
	    { cell = cell->next; count++; }
	}
      else
	{
	  cell = L->last;
	  while ( count <= L->nel - n )
	    { cell = cell->prev; count++; }
	}
    }
  return cell;
} 

/**
 *nsp_get_cell_in_sorted_list:
 * @L: a #NspList 
 * @str: a string 
 * 
 * search for an object with name @str in a sorted #NspList @L.
 * 
 * Return value: %NULLCELL or the adress of the #Cell which contains the object named @str.
 **/

static Cell *nsp_get_cell_in_sorted_list(NspList *L, nsp_const_string str, Cell **prev)
{
  Cell *C;
  int cmp;

  if ( (C = L->current) != NULLCELL  &&  C->O != NULLOBJ ) /* "accelerated" search */
    {
      cmp = strcmp(str, NSP_OBJECT(C->O)->name);
      if ( cmp == 0 )       /* already found */
	{
	  *prev = C->prev; 
	  return C;
	} 
      else if ( cmp < 0 )   /* search backward */
	{
	  C = C->prev;
	  while ( C != NULLCELL ) 
	    {
	      if ( C->O != NULLOBJ )
		{ 
		  cmp = strcmp(str,NSP_OBJECT(C->O)->name);
		  if ( cmp == 0 ) { *prev = C->prev; return C;}
		  if ( cmp >  0 ) { *prev = C; return NULLCELL;}
		}
	      C = C->prev;
	    }
	  *prev = NULLCELL; 
	  return NULLCELL;
	}
      else   /* cmp > 0 */  /* search forward */
	{
	  C = C->next;
	  while ( C != NULLCELL ) 
	    {
	      if ( C->O != NULLOBJ )
		{ 
		  cmp = strcmp(str,NSP_OBJECT(C->O)->name);
		  if ( cmp == 0 ) { *prev = C->prev; return C;}
		  if ( cmp <  0 ) { *prev = C->prev; return NULLCELL;}
		}
	      C = C->next;
	    }
	  *prev = L->last;
	  return NULLCELL;
	} 
    }
  else         /* "usual" search */
    {
      C = L->first;
      while ( C != NULLCELL ) 
	{
	  if ( C->O != NULLOBJ )
	    { 
	      cmp = strcmp(str,NSP_OBJECT(C->O)->name);
	      if ( cmp == 0 ) { *prev = C->prev; return C;}
	      if ( cmp <  0 ) { *prev = C->prev; return NULLCELL;}
	    }
	  C = C->next;
	}
      *prev = L->last;
      return NULLCELL;
    } 
}


/**
 *nsp_list_insert:
 * @L: a #NspList L
 * @O: a #NspObject to be inserted 
 * @n: insertion position. 
 * 
 * inserts object @O at position @n in the list @L.
 * If position @n is 0 insertion is performed at the begining 
 * If position @n exists in @L the corresponding object 
 * is replaced 
 * If position @n > Lenght(@L) the list length is increased 
 * 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_list_insert(NspList *L, NspObject *O, int n)
{
  int i;
  Cell *Loc=NULLCELL,*Loc1;
  NspObject *Ob;

  if ( n < 0 ) 
    {
      Scierror("Error:\tInvalid negative index for list insertion %d\n",n);
      return FAIL;
    }

  if ( n == 0 )
    {
      return nsp_list_begin_insert(L,O);
    }
  else if ( n <= L->nel )
    {
      Loc = nsp_list_get_cell_pointer(L, n);
      nsp_object_destroy(&Loc->O);
      Loc->O = O;
    }
  else  /* we must add n-L->nel-1 cells with NULLOBJECT then a cell with the object O */
    {
      Loc1 = L->last;
      for ( i = L->nel+1 ; i <= n ; i++ )
	{
	  Ob = i < n ? NULLOBJ : O;
	  if ( (Loc=nsp_cell_create(Ob)) == NULLCELL ) return FAIL;
	  Loc->prev = Loc1;
	  if ( Loc1 == NULLCELL ) 
	    L->first = Loc;
	  else
	    Loc1->next = Loc;
	  Loc1 = Loc;
	}
      L->nel = n;
      L->last = Loc;
    }
  L->icurrent = n;
  L->current = Loc;
  
  return OK;
} 


/**
 *nsp_list_get_element:
 * @L: a #NspList 
 * @n: an integer giving the position of the object to be returned
 * 
 * returns a pointer to the @n-th #NspObject of list @L.
 * 
 * Return value: a #NspObject or %NULLOBJECT 
 **/

NspObject *nsp_list_get_element(NspList *L, int n)
{
  Cell *cell;

  if ( n <= 0 ||  n > L->nel ) 
    {
      Scierror("Error:\tindex %d is out of range\n",n);
      return NULLOBJ;
    }

  cell = nsp_list_get_cell_pointer(L, n);

  if ( cell->O == NULLOBJ )
    Scierror("Error:\tlist element at index %d is Undefined\n",n);
    
  L->icurrent = n;
  L->current = cell;
  return cell->O;
} 


/**
 *nsp_list_end_insert:
 * @L: a #NspList 
 * @A: a #NspObject
 * 
 * inserts object @A at the end of the list @L. 
 * The object @A is not copied and the list must exists 
 * i.e @L should not be %NULL. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_list_end_insert(NspList *L, NspObject *A)
{
  Cell *Loc;

  if ( (Loc=nsp_cell_create(A)) == NULLCELL ) return FAIL;

  if ( L->last == NULLCELL )
    { 
      L->first = Loc;
      L->last = Loc;
    }
  else 
    {
      L->last->next = Loc;
      Loc->prev = L->last;
      L->last = Loc;
    }

  L->nel++;
  L->icurrent = L->nel;
  L->current = Loc;
  return OK;
}


/**
 *nsp_list_begin_insert:
 * @L: a #NspList 
 * @A: a #NspObject
 * 
 * inserts object @A at the beginning of list @L. 
 * The object @A is not copied and the list must exists 
 * i.e @L should not be %NULL. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_list_begin_insert(NspList *L, NspObject *A)
{
  Cell *Loc;

  if ( (Loc=nsp_cell_create(A)) == NULLCELL ) return FAIL;

  if ( L->first == NULLCELL )
    { 
      L->first = Loc;
      L->last = Loc;
    }
  else 
    {
      L->first->prev = Loc;
      Loc->next = L->first;
      L->first = Loc;
    }

  L->nel++;
  L->icurrent = 1;
  L->current = Loc;
  return OK;
}

/**
 *nsp_list_store:
 * @L: a #NspList 
 * @A: a #NspObject
 * @n: an integer 
 * 
 * inserts object @A in the list @L at position @n. 
 * The object which was at position @n is moved to @n+1
 * The object @A is not copied.
 *
 * Return value: %OK or %FAIL
 **/

int nsp_list_store(NspList *L, NspObject *A, int n)
{ 
  Cell *Loc, *Loc1;

  if ( n <= 0 || n > L->nel+1 ) 
    {
      Scierror("Error:\tInvalid index for list store %d\n",n);
      return FAIL;
    }

  if ( (Loc1 = nsp_cell_create(A)) == NULLCELL ) return FAIL;

  if ( n == 1 )
    {
      Loc1->next = L->first;
      if ( L->last == NULLCELL ) 
	L->last = Loc1;
      else
	L->first->prev = Loc1;
      L->first = Loc1;
    }
  else  /* here we know that n > 1 and that the list has at least n-1 elements */
    {
      Loc = nsp_list_get_cell_pointer(L,n-1);
      if ( L->last == Loc ) 
	L->last = Loc1;
      else
	Loc->next->prev = Loc1;
      Loc1->prev = Loc;
      Loc1->next = Loc->next;
      Loc->next = Loc1;
    }

  L->icurrent = n;
  L->current = Loc1;
  L->nel++;
  return OK;
}


/**
 * nsp_remove_cell_from_list
 * @L: a #NspList 
 * @Loc: a #Cell 
 * 
 * supresses the #Cell @Loc (without destroying it) from the list @L.
 * Note that @Loc must be a valid #Cell of list @L
 * @L->first, @L->last and @L->nel are updated but not @L->icurrent
 * and @L->current 
 * 
 **/

void nsp_remove_cell_from_list(NspList *L, Cell *Loc)
{
  if ( L->nel == 1 )   /* list with one cell => become an empty list */
    {
      L->first = NULLCELL; L->last = NULLCELL;
    }
  else  /* list with at least 2 cells (=> first != last and it will stay at least one cell) */
    {
      if ( L->first == Loc )
	{
	  L->first = Loc->next; 
	  Loc->next->prev = NULLCELL;
	}
      else if ( L->last == Loc )
	{
	  L->last = Loc->prev; 
	  Loc->prev->next = NULLCELL;
	}
      else
	{
	  Loc->prev->next = Loc->next; 
	  Loc->next->prev = Loc->prev;
	}
    }
  L->nel--;
}

/**
 *nsp_list_delete_elt_by_name:
 * @L: a #NspList 
 * @str: s atring 
 * 
 * supresses if found the first object named @str from the list @L.
 * 
 **/

void nsp_list_delete_elt_by_name(NspList *L, char *str)
{
  Cell *Loc = L->first;
  while ( Loc != NULLCELL ) 
    {
      if ( Loc->O != NULLOBJ  &&  Ocheckname(Loc->O,str) ) 
	{ 
	  nsp_remove_cell_from_list(L, Loc);
	  L->icurrent = 0;
	  L->current = NULLCELL;
	  nsp_cell_destroy(&Loc);
	  return;
	}
      Loc = Loc->next;
    }
}


typedef void (*destr)( Cell **c);

static int DeleteNth_g(NspList *L, int n, destr F)
{
  Cell *Loc;

  if ( n <= 0  ||  n > L->nel ) 
    {
      Scierror("Error:\tindex %d out of range for list deletion\n",n);
      return FAIL;
    }
  Loc = nsp_list_get_cell_pointer(L, n);
  nsp_remove_cell_from_list(L, Loc);
  L->icurrent = n-1;
  L->current = Loc->prev;
  F(&Loc);
  return OK;
}


/**
 *nsp_list_delete_elt:
 * @L: a #NspList 
 * @nel: an integer 
 * 
 *
 * supresses the @nel-th element from the list @L.
 * The @nel-th element is destroyed.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_list_delete_elt(NspList *L, int nel)
{
  return DeleteNth_g(L, nel,nsp_cell_destroy);
}


/**
 *nsp_list_delete_cell:
 * @L: a #NspList 
 * @nel: an integer 
 * 
 * supresses the @nel-th cell from the list @L.
 * Note that the object stored in the @nel-th cell is not 
 * destroyed 
 * 
 * Return value:  %OK or %FAIL
 **/

int nsp_list_delete_cell(NspList *L, int nel)
{
  return DeleteNth_g(L, nel,nsp_cell_only_destroy);
}


/**
 * nsp_list_remove_first:
 * @L: a #NspList 
 * 
 * removes first element from list @L.
 * 
 **/

void  nsp_list_remove_first(NspList *L)
{
  Cell *Loc;
  Loc = L->first;
  if ( Loc == NULLCELL) return ; 
  if ( L->nel == 1 )   /* list with one cell => become an empty list */
    {
      L->first = L->current= L->last = NULLCELL;
      L->icurrent=0;
    }
  else  /* list with at least 2 cells */
    {
      L->first = Loc->next; 
      Loc->next->prev = NULLCELL;
      L->current= L->first;
      L->icurrent=1;
    }
  nsp_cell_destroy(&Loc);
  L->nel--;
}

/**
 * nsp_list_remove_last:
 * @L: a #NspList 
 * 
 * removes last element from list @L.
 **/

void  nsp_list_remove_last(NspList *L)
{
  Cell *Loc;
  Loc = L->last;
  if ( L->first == NULLCELL) return ; 
  if ( L->nel == 1 )   /* list with one cell => become an empty list */
    {
      L->first = L->current= L->last = NULLCELL;
      L->icurrent=0;
    }
  else  /* list with at least 2 cells */
    {
      L->last = Loc->prev; 
      Loc->prev->next = NULLCELL;
      if ( L->current== Loc) 
	{
	  L->current= L->first;
	  L->icurrent=1;
	}
    }
  nsp_cell_destroy(&Loc);
  L->nel--;
}

/**
 *nsp_list_length:
 * @L: a #NspList 
 * 
 * returns the length of list @L.
 *
 * Return value: the length of list @L as an int
 **/

int nsp_list_length(NspList *L)
{
  return L->nel;
}


/**
 *nsp_list_concat:
 * @L1:  a #NspList 
 * @L2:  a #NspList 
 * 
 * Concatenation of two lists @L1 = Concat(@L1,Copy of @L2); 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_list_concat(NspList *L1, NspList *L2)
{
  Cell *Loc;
  NspList *L2copy;

  if ( L2->first == NULLCELL ) return OK;
  if ( (L2copy = nsp_list_copy(L2)) == NULLLIST ) return FAIL;

  Loc = L1->last;
  if ( Loc == NULLCELL ) 
    {
      L1->first = L2copy->first;
    }
  else 
    {
      Loc->next = L2copy->first;
      L2copy->first->prev = Loc;
    }
  L1->last = L2copy->last;  /* L2 is not an empty list */
  L1->nel += L2copy->nel;
  L2copy->first= NULLCELL; L2copy->nel = 0; L2copy->last = NULLCELL;
  nsp_list_destroy(L2copy);
  return OK;
}


/**
 *nsp_list_info:
 * @L: a #NspList 
 * @indent: an integer. 
 * @name: a string 
 * @rec_level: an integer
 * 
 * Display of infos an Object of type #NspList 
 *
 * Returns: %TRUE or %FALSE
 **/

static void nsp_list_info_tree(NspList *L, int indent,char *name,int rec_level);

int nsp_list_info(NspList *L, int indent,char *name,int rec_level)
{
  int colors[]={ 34,32,31,35,36};
  const int name_len=128;
  char epname[name_len];
  const char *pname = (name != NULL) ? name : NSP_OBJECT(L)->name;
  Cell *C;
  int i=1;

  if ( user_pref.list_as_tree == TRUE ) 
    {
      nsp_list_info_tree(L,indent,name,rec_level);
      return TRUE;
    }

  if ( rec_level <= user_pref.pr_depth ) 
    {
      /* recursively call print on elements */
      Sciprintf1(indent,"%s\t=\t\tl (%d)\n",(strcmp(pname,NVOID) != 0) ? pname : " ",L->nel);
      Sciprintf1(indent+1,"(\n");
      C= L->first;
      while ( C != NULLCELL) 
	{
	  if ( rec_level >= 0 && rec_level <= 4) 
	    {
	      int col=colors[rec_level];
	      sprintf(epname,"\033[%dm(%d)\033[0m",col,i);
	    }
	  else 
	    {
	      sprintf(epname,"(%d)",i);
	    }
	  if ( C->O != NULLOBJ )
	    {
	      nsp_object_info(C->O,indent+2,epname,rec_level+1);      
	    }
	  else
	    {
	      Sciprintf1(indent+2,"%s Undefined\n",epname);
	    }
	  C = C->next ;i++;
	  /* if ( C != NULLCELL ) Sciprintf("\n"); */
	}
      Sciprintf1(indent+1, ")\n");
    }
  else
    {
      Sciprintf1(indent,"%s\t= ...\t\tl (%d)\n",(strcmp(pname,NVOID) != 0) ? pname : " ",L->nel);
    }
  return TRUE;
} 

static void nsp_list_info_tree(NspList *L, int indent,char *name,int rec_level)
{
  const int name_len=128;
  char epname[name_len];
  const char *pname = (name != NULL) ? name : NSP_OBJECT(L)->name;
  Cell *C;
  int i=1;

  if ( rec_level <= user_pref.pr_depth ) 
    {
      /* recursively call print on elements */
      Sciprintf1(indent,"%s.\n",(strcmp(pname,NVOID) != 0) ? pname : "");
      C= L->first;
      while ( C != NULLCELL) 
	{
	  int j;
	  sprintf(epname,"%s",pname);
	  for ( j = 0 ; j < strlen(epname);j++) 
	    {
	      if (epname[j] !='-' && epname[j] != '`' && epname[j] != ' ' && epname[j] != '|') epname[j]=' ';
	    }
	  for ( j = 0 ; j < strlen(epname);j++) if (epname[j]=='-' || epname[j] == '`' ) epname[j]=' ';
	  if ( C->next == NULLCELL) 
	    strcat(epname,"`-");
	  else 
	    strcat(epname,"|-");
	  if ( C->O != NULLOBJ )
	    {
	      nsp_object_info(C->O,indent,epname,rec_level+1);      
	    }
	  else
	    {
	      Sciprintf1(indent+2,"%s Undefined\n",epname);
	    }
	  C = C->next ;i++;
	  /* if ( C != NULLCELL ) Sciprintf("\n"); */
	}
    }
  else
    {
      Sciprintf1(indent,"%s\t= ...\t\tl (%d)\n",(strcmp(pname,NVOID) != 0) ? pname : " ",L->nel);
    }
} 

/**
 * nsp_list_print:
 * @L: a #NspList 
 * @indent: integer 
 * @name: a string 
 * @rec_level: an integer
 * 
 * display of a list.
 *
 * Return value: %TRUE or %FALSE.
 **/

int nsp_list_print(NspList *L, int indent,char *name, int rec_level)
{
  const int name_len=128;
  char epname[name_len],epname1[name_len];
  const char *pname = (name != NULL) ? name : NSP_OBJECT(L)->name;
  int j;
  Cell *C;
  int i=1;
  for ( j=0 ; j < indent ; j++) Sciprintf(" ");

  if (user_pref.pr_as_read_syntax)
    {
      int count=1;
      sprintf(epname,"L__%d",rec_level);
      Sciprintf("%s=list();\n",epname);
      C= L->first;
      while ( C != NULLCELL) 
	{
	  if ( C->O != NULLOBJ )
	    {
	      sprintf(epname1,"%s(%d)",epname,count);
	      nsp_object_print(C->O,indent+1,epname1,rec_level+1);
	    }
	  C = C->next;
	  count++;
	  if ( C != NULLCELL )
	    {
	      Sciprintf1(indent+1,"\n");
	    }
	}
      if ( strcmp(pname,NVOID) != 0) 
	{
	  Sciprintf1(indent+1,"%s=%s;\n",pname,epname);
	}
    }
  else
    {
      int colors[]={ 34,32,31,35,36};

      if ( rec_level <= user_pref.pr_depth ) 
	{
	  /* recursively call print on elements */
	  Sciprintf("%s\t=\t\tl (%d)\n",(strcmp(pname,NVOID) != 0) ? pname : " ",L->nel);
	  Sciprintf1(indent+1,"(\n");
	  C= L->first;
	  while ( C != NULLCELL) 
	    {
	      if ( rec_level >= 0 && rec_level <= 4) 
		{
		  int col=colors[rec_level];
		  sprintf(epname,"\033[%dm(%d)\033[0m",col,i);
		}
	      else 
		{
		  sprintf(epname,"(%d)",i);
		}
	      if ( C->O != NULLOBJ )
		{
		  if ( nsp_object_print(C->O,indent+2,epname,rec_level+1) == FALSE) 
		    return FALSE;
		}
	      else
		{
		  Sciprintf1(indent+2,"%s Undefined\n",epname);
		}
	      C = C->next ;i++;
	      /* if ( C != NULLCELL ) Sciprintf("\n"); */
	    }
	  Sciprintf1(indent+1, ")\n");
	}
      else
	{
	  Sciprintf("%s\t= ...\t\tl (%d)\n",(strcmp(pname,NVOID) != 0) ? pname : " ",L->nel);
	}
    }
  return TRUE;
}


/**
 * nsp_list_latex_print:
 * @L: a #NspList 
 * 
 * print the #NspList @L using the default Sciprintf() function and LaTeX 
 * syntax. 
 */

void nsp_list_latex_print(NspList *L)
{
  Cell *C;
  int i=1;
  const char *pname = NSP_OBJECT(L)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if (strcmp(pname,NVOID) != 0) Sciprintf("%s\n", pname);
  Sciprintf1(1,"\\begin{itemize}\n");
  C= L->first;
  while ( C != NULLCELL) 
    {
      Sciprintf("\\item[(%d)]",i);
      if ( C->O != NULLOBJ )
	{
	  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
	  nsp_object_latex(C->O,2,NULL,1);    
	  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
	}
      else
	{
	  Sciprintf("Undefined\n");
	}
      C = C->next ;i++;
      if ( C != NULLCELL ) Sciprintf("\n");
    }
  Sciprintf1(1, "\\end{itemize}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
}

/**
 *nsp_cell_only_destroy:
 * @c: a #Cell
 * 
 * Delete the Cell @c but not the stored #NspObject 
 * 
 **/

void nsp_cell_only_destroy(Cell **c)
{
  if ((*c) != NULLCELL)
    {
      FREE((*c));
    }
} 

/**
 *nsp_sorted_list_search:
 * @L: 
 * @str: 
 * 
 * Searches for a list element named @str in a Sorted list @L.
 * 
 * Return value: a new #NspObject or %NULLOBJ
 **/

NspObject *nsp_sorted_list_search(NspList *L, nsp_const_string str)
{
  Cell *C, *prev=NULLCELL;   
  if ( (C = nsp_get_cell_in_sorted_list(L, str, &prev)) != NULLCELL )
    {
      L->current = C;
      return C->O;
    }
  else
    return NULLOBJ;
}


/**
 *nsp_sorted_list_search_and_remove:
 * @L: 
 * @str: 
 * 
 * Searches for a list element named @str in #NspList @L.
 * Returns this element and remove the 
 * element from the list (without destroying the returned object).
 * 
 * Return value: a #NspObject or %NULLOBJ
 **/

NspObject *nsp_sorted_list_search_and_remove(NspList *L, nsp_const_string str)
{
  Cell *Loc, *prev;
  NspObject *Ret;

  if ( (Loc = nsp_get_cell_in_sorted_list(L, str, &prev)) != NULLCELL )
    {
      Ret = Loc->O;
      nsp_remove_cell_from_list(L, Loc);
      L->current = Loc->prev;
      nsp_cell_only_destroy(&Loc);
      return Ret;
    }
  else
    return NULLOBJ;
}


/**
 *nsp_sorted_list_insert:
 * @L: 
 * @O: 
 * 
 * 
 * Inserts object @O using lexicographic order. 
 * If an object with same name exists in the list @O will replace it.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_sorted_list_insert(NspList *L, NspObject *O)
{
  Cell *Loc,*Loc1,*Loc2;

  if ( L->first == NULLCELL )
    {
      if ( (Loc2 = nsp_cell_create(O)) == NULLCELL ) return FAIL;
      L->first = Loc2;
      L->last = Loc2;
      L->current = Loc2;
      L->nel = 1;
      return OK;
    }

  /* here we know that L has at least one element */
  Loc = nsp_get_cell_in_sorted_list(L, NSP_OBJECT(O)->name, &Loc1);

  if ( Loc != NULLCELL )   /* we replace Object by the new one **/ 
    {
      nsp_object_destroy(&Loc->O);
      Loc->O = O;
      L->current = Loc;
      return OK;	
    }
  else   /* the Object O must be inserted after cell Loc1  */
    {
      if ( (Loc2 = nsp_cell_create(NULLOBJ)) == NULLCELL ) return FAIL;
      Loc2->O = O; 
      if ( Loc1 == NULLCELL )      /* insert in head  */
	{
	  Loc2->next = L->first;
	  L->first->prev = Loc2;
	  L->first = Loc2;
	}
      else if ( Loc1 == L->last )  /* insert in queue  */
	{
	  Loc2->prev = L->last;
	  L->last->next = Loc2;
	  L->last = Loc2;
	}
      else
	{
	  Loc2->prev = Loc1;
	  Loc2->next = Loc1->next;
	  Loc1->next->prev = Loc2;
	  Loc1->next = Loc2;
	}
      L->current = Loc2;
      L->nel++;
      return OK;
    }
} 


/**
 *nsp_list_map:
 * @L: a #NspList 
 * @PL: a #NspPList
 * @args: a #NspList 
 * 
 * maps function @PL to each element of list @L passing extra 
 * arguments to the function through @args.
 * 
 * Return value: the new #NspList obtained after mapping or %NULLLIST
 **/

NspList *nsp_list_map(NspList *L, NspPList *PL, NspList *args)  
{
  NspObject *O[2];
  int first = -1;
  NspList *L_map;
  Cell *L_cell,*cell1=NULLCELL,*cell2=NULLCELL;
  if ( (L_map =nsp_list_create(NVOID)) == NULLLIST ) return NULLLIST;
  L_cell = L->first ;
  O[1]=NULLOBJ;
  while ( L_cell != NULLCELL) 
    {
      O[0] = L_cell->O;
      if ( O[0] != NULLOBJ ) 
	{
	  /* the object is copied without name,it will be freed by 
	   * nsp_eval_macro_code
	   */
	  if ((O[0] =nsp_object_copy(O[0]))== NULLOBJ) return NULLLIST;
	  /* stack position to use is computed on the first call and set 
	   * for next calls in first 
	   */
	  if ((O[0] =nsp_eval_macro_code(PL,O,args,&first))== NULLOBJ) return NULLLIST;
	  if ( nsp_object_set_name(O[0],"lel") == FAIL ) return NULLLIST;
	  if ((cell1 =nsp_cell_create(O[0]))== NULLCELL) return NULLLIST;
	}
      else 
	{
	  if ((cell1 =nsp_cell_create(O[0]))== NULLCELL) return NULLLIST;
	}
      if ( L_cell->prev == NULLCELL) 
	{
	  L_map->first = cell1;
	}
      else 
	{
	  cell1->prev = cell2;
	  cell2->next = cell1;
	}
      cell2= cell1;
      L_cell = L_cell->next;
      L_map->nel++;
    }
  L_map->last = cell1;

  return L_map;
} 



static NspObject *cell_fold_right(Cell *C,NspObject *x, NspPList *PL, NspList *args,int *first) ;


/**
 *nsp_list_fold_right:
 * @L: a #NspList 
 * @x: a #NspObject
 * @PL: a #NspPList
 * @args: a #NspList 
 * 
 * folds function @PL to list @L passing extra 
 * arguments to the function through @args.
 * returns f(L(1),f(L(2),....f(L(n),@x,@args),args),args);
 * 
 * Return value: a new #NspList or %NULLLIST
 *
 **/

NspObject *nsp_list_fold_right(NspList *L,NspObject *x, NspPList *PL, NspList *args)
{
  int first = -1;
  return cell_fold_right(L->first,x, PL,args,&first);
}

static NspObject *cell_fold_right(Cell *C,NspObject *x, NspPList *PL, NspList *args,int *first)  
{
  NspObject *O[]={NULL,NULL,NULL};
  /* limit case */
  if ( C == NULLCELL ) 
    {
      Scierror("foldr: NspList is too short\n");
      return NULLOBJ ; 
    }
  if ( (O[0]= C->O) == NULLOBJ  ) 
    {
      Scierror("foldr: NspList with unknown elements are not allowed \n");
      return NULLOBJ ; 
    }
  if ( C->next == NULLCELL) 
    {
      if (( O[1] = nsp_object_copy(x))== NULLOBJ) return NULLOBJ;
      return nsp_eval_macro_code(PL,O,args,first);
    }
  else 
    {
      O[1]= cell_fold_right(C->next,x, PL,args,first);
      if ( O[1] == NULLOBJ ) return NULLOBJ; 
      return nsp_eval_macro_code(PL,O,args,first);
    }
  return NULLOBJ;
} 



/**
 *nsp_list_fold_left:
 * @L: a #NspList 
 * @x: a #NspObject
 * @PL: a #NspPList
 * @largs: a #NspList 
 * 
 * folds function @PL to list @L passing extra 
 * arguments to the function through @largs.
 * for example on a list of length 3 it returns f(f(f(x,L(1),largs),L(2),largs),L(3),largs);
 * 
 * Return value: a new #NspList or %NULLLIST
 **/

extern NspObject *nsp_list_fold_left(NspList *L, NspObject *x,NspPList *PL, NspList *largs)
{
  int first = -1;
  Cell *C= L->first;
  NspObject *args[]={NULL,NULL,NULL}, *Ret;
  /* limit case */
  if (( Ret = nsp_object_copy(x))== NULLOBJ) 
    return Ret;
  while ( C != NULLCELL) 
    {
      args[0]=Ret;
      if ( (args[1]= C->O) == NULLOBJ ) 
	{
	  Scierror("foldl: NspList with unknown elements are not allowed \n");
	  return NULLOBJ ; 
	}
      if ((Ret = nsp_eval_macro_code(PL,args,largs,&first))==NULLOBJ)
	return NULLOBJ;
      C=C->next;
    }
  return Ret;
} 


/**
 *nsp_list_equal:
 * @L1: a #NspList
 * @L2: a #NspList
 * 
 * if the two lists do not have the same length returns the %f  #NspBMatrix 
 * else returns a #NspBMatrix @B such that @B(i)= @L1(i)== @L2(i) 
 * 
 * Return value: a new #NspBMatrix or NULLBMAT.
 **/

NspBMatrix  *nsp_list_equal(NspList *L1, NspList *L2)
{
  int count=0;
  NspBMatrix *B;
  Cell *L1_cell,*L2_cell;
  int l1=nsp_list_length(L1); 
  int l2=nsp_list_length(L2); 
  if ( l1 != l2 ) 
    {
      if ((B=nsp_bmatrix_create(NVOID,1,1))== NULLBMAT) return NULLBMAT;
      B->B[0]=FALSE; 
      return B;
    }
  if ((B=nsp_bmatrix_create(NVOID,1,l1))== NULLBMAT) return NULLBMAT;
  L1_cell = L1->first ;
  L2_cell = L2->first ;
  while ( L1_cell != NULLCELL) 
    {
      if ( L1_cell->O->type->eq != NULL) 
	B->B[count]=  L1_cell->O->type->eq(L1_cell->O,L2_cell->O);
      else 	
	B->B[count]= FALSE;
      L1_cell = L1_cell->next;
      L2_cell = L2_cell->next;
      count++;
    }
  return B;
} 

/**
 *nsp_list_not_equal:
 * @L1: a #NspList
 * @L2: a #NspList
 * 
 * if the two lists do not have the same length returns the %t  #NspBMatrix 
 * else returns a #NspBMatrix @B such that @B(i)= and(@L1(i) <> @L2(i))
 * 
 * Return value: a new #NspBMatrix or NULLBMAT.
 **/

NspBMatrix  *nsp_list_not_equal(NspList *L1, NspList *L2)
{
  int count=0;
  NspBMatrix *B;
  Cell *L1_cell,*L2_cell;
  int l1=nsp_list_length(L1); 
  int l2=nsp_list_length(L2); 
  if ( l1 != l2 ) 
    {
      if ((B=nsp_bmatrix_create(NVOID,1,1))== NULLBMAT) return NULLBMAT;
      B->B[0]= TRUE;
      return B;
    }
  if ((B=nsp_bmatrix_create(NVOID,1,l1))== NULLBMAT) return NULLBMAT;
  L1_cell = L1->first ;
  L2_cell = L2->first ;
  while ( L1_cell != NULLCELL) 
    {
      if ( L1_cell->O->type->neq != NULL) 
	B->B[count]=  L1_cell->O->type->neq(L1_cell->O,L2_cell->O);
      else 	
	B->B[count]= TRUE;
      L1_cell = L1_cell->next;
      L2_cell = L2_cell->next;
      count++;
    }
  return B;
} 


/**
 *nsp_list_full_equal:
 * @L1: a #NspList
 * @L2: a #NspList
 * 
 * if the two list do not have the same length returns %FALSE
 * else returns %TRUE if all the elments @L1(i) equals  @L2(i) and 
 * %FALSE elsewhere.
 * 
 * 
 * Return value: %TRUE or %FALSE
 **/

int nsp_list_full_equal(NspList *L1, NspList *L2)
{
  int rep = TRUE;
  Cell *L1_cell,*L2_cell;
  int l1=nsp_list_length(L1); 
  int l2=nsp_list_length(L2); 
  if ( l1 != l2 ) 
    {
      return FALSE; 
    }
  L1_cell = L1->first ;
  L2_cell = L2->first ;
  while ( L1_cell != NULLCELL) 
    {
      if ( L1_cell->O->type->eq != NULL) 
	rep &= L1_cell->O->type->eq(L1_cell->O,L2_cell->O);
      else 	
	rep = FALSE;
      if ( rep == FALSE) break ; 
      L1_cell = L1_cell->next;
      L2_cell = L2_cell->next;
    }
  return rep;
} 

/**
 *nsp_list_full_not_equal:
 * @L1: a #NspList
 * @L2: a #NspList
 * 
 * if the two list do not have the same length returns %TRUE 
 * else returns %FALSE if all the elements @L1(i) equals  @L2(i) and 
 * %TRUE elsewhere.
 * 
 * Return value: %TRUE or %FALSE
 **/

int nsp_list_full_not_equal(NspList *L1, NspList *L2)
{
  int rep = FALSE;
  Cell *L1_cell,*L2_cell;
  int l1=nsp_list_length(L1); 
  int l2=nsp_list_length(L2); 
  if ( l1 != l2 ) 
    {
      return TRUE;
    }
  L1_cell = L1->first ;
  L2_cell = L2->first ;
  while ( L1_cell != NULLCELL) 
    {
      /* attention XXX L1_cell->O == NULL est possible */
      if ( L1_cell->O->type->neq != NULL) 
	rep = L1_cell->O->type->neq(L1_cell->O,L2_cell->O);
      else 	
	rep = TRUE;
      if ( rep == TRUE) break ; 
      L1_cell = L1_cell->next;
      L2_cell = L2_cell->next;
    }
  return rep;
} 

/**
 *nsp_list_unique:
 * @L: a #NspList
 * @Ind: a #NspMatrix 
 * @Occ:  a #NspMatrix 
 * 
 *  build a new list LL with unique elements of L
 *  if Ind != NULL , Ind->R[k] give an index i such that LL(k) = L(i)
 *  if Ind != NUL then Occ may be != NULL (else Occ = NULL) and
 *  if Occ != NULL Occ->R[k] is the number of occurences in L of LL(k).
 *
 * Return value: a NspList
 **/

NspList *nsp_list_unique(NspList *L, NspObject **Ind, NspMatrix **Occ, char ind_type)
{
  NspList *LL;
  int i, j, k, *index;
  Boolean found;
  Cell *cell_L, *cell_LL;
  NspMatrix *occ=NULLMAT;
  NspObject *O=NULLOBJ;

  if ( (LL = nsp_list_create(NVOID)) == NULLLIST ) return NULLLIST;

  if (Ind != NULL )
    {
      if ( (*Ind = nsp_alloc_mat_or_imat(1,L->nel, ind_type, &index)) == NULLOBJ ) goto err;

      if (Occ != NULL )
	if ( (occ = nsp_matrix_create(NVOID,'r',1,L->nel)) == NULLMAT ) goto err;
    }

  cell_L = L->first; i = 0; k = 0;
  while ( cell_L != NULLCELL )
    {
      i++;
      if ( cell_L->O != NULLOBJ )
	{
	  found = FALSE;
	  cell_LL = LL->first; j = 0;
	  while ( cell_LL != NULLCELL  &&  !found )
	    {
	      if ( cell_L->O->type->eq(cell_L->O, cell_LL->O) )
		{
		  found = TRUE;
		  if ( Occ != NULL ) occ->R[j]++;
		}
	      j++;
	      cell_LL = cell_LL->next;
	    }
	  if ( !found )
	    {
	      if ( (O = nsp_object_copy_with_name(cell_L->O)) == NULLOBJ ) goto err;
	      if ( nsp_list_end_insert(LL,O) == FAIL ) { nsp_object_destroy(&O); goto err;}
	      if ( Ind != NULL )
		{
		  index[k] = i;
		  if ( Occ != NULL ) occ->R[k] = 1;
		}
	      k++;
	    }
	}
      cell_L = cell_L->next;
    }

  if ( Ind != NULL )
    {
      if ( ind_type == 'd' )
	nsp_matrix_resize((NspMatrix *) *Ind, 1, k);
      else
	nsp_imatrix_resize((NspIMatrix *) *Ind, 1, k);
      if ( ind_type == 'd' )
	*Ind = (NspObject *) Mat2double((NspMatrix *) *Ind);

      if ( Occ != NULL ) 
	{
	  nsp_matrix_resize(occ,1,k);
	  *Occ = occ;
	}
    }
  return LL;

 err:
  nsp_list_destroy(LL);
  if ( Ind != NULL )
    nsp_object_destroy(Ind);
  nsp_matrix_destroy(occ);
  return NULLLIST;
}


/**
 *nsp_list_compact:
 * @L1: a#NspList 
 * @flag: a character.
 * 
 * compacts list by column or row appending element of compatible size and 
 * type. 
 * 
 * 
 * Return value: %Ok or %FAIL.
 **/

int nsp_list_compact(NspList *L1, char flag )
{
  Cell *cell =  L1->first ;
  while ( cell != NULLCELL) 
    {
      if ( cell->O != NULLOBJ ) 
	{
	  NspTypeBase *type =(NspTypeBase *) cell->O->basetype;
	  Cell *next=  cell->next; 
	  if ( IsList(cell->O) )
	    {
	      if (nsp_list_compact((NspList *) cell->O,flag)== FAIL) return FAIL;
	    }
	  else 
	    {
	      while ( next != NULL &&  next->O != NULLOBJ && check_cast(next->O,type->id))
		{
		  int dflag = FALSE;
		  if ( type->id == nsp_type_matrix_id ) 
		    {
		      /* 2 consecutive arguments are scalar matrices */
		      if ( flag == 'c' ) 
			{
			  if (nsp_object_get_size(cell->O,1) ==nsp_object_get_size(next->O,1))
			    {
			      if ( nsp_matrix_concat_right((NspMatrix *) cell->O,(NspMatrix *)next->O) == FAIL) return FAIL;
			      dflag = TRUE;
			    }
			}
		      else 
			{
			  if (nsp_object_get_size(cell->O,2) ==nsp_object_get_size(next->O,2))
			    {
			      NspMatrix *A;
			      if ((A=nsp_matrix_concat_down((NspMatrix *)cell->O,(NspMatrix *)next->O)) == NULL) return FAIL;
			      nsp_object_destroy(&cell->O);
			      cell->O = (NspObject *) A;
			      if (nsp_object_set_name(cell->O,"lel") == FAIL) return FAIL;
			      dflag = TRUE;
			    }
			}
		    }
		  else if ( type->id == nsp_type_smatrix_id ) 
		    {
		      /* 2 consecutive arguments are string matrices */
		      if ( flag == 'c' ) 
			{
			  if (nsp_object_get_size(cell->O,1) ==nsp_object_get_size(next->O,1))
			    {
			      if ( nsp_smatrix_concat_right((NspSMatrix *) cell->O,(NspSMatrix *)next->O) == FAIL) return FAIL;
			      dflag = TRUE;
			    }
			}
		      else 
			{
			  if (nsp_object_get_size(cell->O,2) ==nsp_object_get_size(next->O,2))
			    {
			      NspSMatrix *A;
			      if ((A=nsp_smatrix_concat_down((NspSMatrix *)cell->O,(NspSMatrix *)next->O)) == NULL) return FAIL;
			      nsp_object_destroy(&cell->O);
			      cell->O = (NspObject *) A;
			      if (nsp_object_set_name(cell->O,"lel") == FAIL) return FAIL;
			      dflag = TRUE;
			    }
			}
		    }
		  else if ( type->id == nsp_type_bmatrix_id ) 
		    {
		      /* 2 consecutive arguments are boolean matrices */
		      if ( flag == 'c' ) 
			{
			  if (nsp_object_get_size(cell->O,1) ==nsp_object_get_size(next->O,1))
			    {
			      if (nsp_bmatrix_concat_right((NspBMatrix *) cell->O,(NspBMatrix *)next->O) == FAIL) return FAIL;
			      dflag = TRUE;
			    }
			}
		      else 
			{
			  if (nsp_object_get_size(cell->O,2) ==nsp_object_get_size(next->O,2))
			    {
			      NspBMatrix *A;
			      if ((A=nsp_bmatrix_concat_down((NspBMatrix *)cell->O,(NspBMatrix *)next->O)) == NULL) return FAIL;
			      nsp_object_destroy(&cell->O);
			      cell->O = (NspObject *) A;
			      if (nsp_object_set_name(cell->O,"lel") == FAIL) return FAIL;
			      dflag = TRUE;
			    }
			}
		    }
	
		  /* remove cell */
		  if ( dflag == TRUE ) 
		    {
		      cell->next = next->next; 
		      if ( cell->next != NULL) cell->next->prev = cell;
		      nsp_cell_destroy(&next); 
		      next = cell->next ;
		    }
		  else 
		    {
		      break; /* leave the first while */
		    }
		}
	    }
	}
      cell = cell->next ;
    }

  /* update length, last, icurrent and current */
  L1->icurrent = 0; L1->current = NULLCELL;
  cell = L1->first; L1->nel = 0;
  while ( cell != NULLCELL) 
    { 
      L1->nel++; L1->last = cell; 
      cell = cell->next; 
    }

  return OK;
}

/**
 *nsp_list_has:
 * @L   : a #NspList 
 * @Obj : a #NspObject
 * @ind : an integer 
 *
 *  tests if the list L contains the object Obj
 *
 *  FIXME : voir si on maj icurrent et current 
 *
 * Return value: %TRUE or %FALSE
 **/

Boolean nsp_list_has(NspList *L, NspObject *Obj, int *ind)
{
  int i = 0;
  Boolean found = FALSE;
  Cell *cell_L = L->first;

  while ( !found  &&  cell_L != NULLCELL )
    {
      i++;
      if ( cell_L->O != NULLOBJ  &&  cell_L->O->type->eq(cell_L->O, Obj) )
	{
	  found = TRUE;
	  L->icurrent = i; L->current = cell_L;
	}
      cell_L = cell_L->next;      
    }

  *ind = found ? i : 0;

  return found;
}
