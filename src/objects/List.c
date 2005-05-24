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
#include <stdlib.h>
#include <string.h>

#include "nsp/object.h"
#include "nsp/interf.h" 

/* FIXME */
extern NspObject *EvalMacro(NspPList *PL, NspObject **O, NspList *args,int *first) ; 

/*
 *  Doubly linked lists (NspList of Objects )
 *  basic Objects are defined in Obj.c 
 */

/**
 *nsp_list_create:
 * @name: 
 * @tname: 
 * 
 * Creates a new empty list with name name 
 * and type tname if we want to define a tlist 
 * 
 * 
 * Return value: 
 **/

NspList*nsp_list_create(char *name, char *tname)
{
  NspList *Loc = new_list();

  if ( Loc == NULLLIST)
    {
      Scierror("Error:\t running out of memeory\n");
      return(NULLLIST);
    }
  if (name != NULLSTRING) 
    { 
      if (( NSP_OBJECT(Loc)->name =new_nsp_string(name)) == NULLSTRING)
	return(NULLLIST);
    }
  else 
    {
      NSP_OBJECT(Loc)->name = NULLSTRING;
    }
  if (tname != NULLSTRING) 
    { 
      if (( Loc->tname =new_nsp_string(tname)) == NULLSTRING)
	return(NULLLIST);
    }
  else 
    {
      Loc->tname =  NULLSTRING;
    }
  NSP_OBJECT(Loc)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  /*
    Loc->otype = LIST;
    Loc->ftype = List_Type;
  */
  return(Loc);
}


/**
 *nsp_cell_create:
 * @name: 
 * @O: 
 * 
 * creates a new cell in which O is stored 
 * O can be a NULLOBJ
 * 
 * Return value: 
 **/

Cell *nsp_cell_create(char *name, NspObject *O)
{
  Cell *Loc;
  Loc = ( Cell *) MALLOC(sizeof( Cell));
  if (Loc == NULLCELL)
    {
      Scierror("Error:\tNo more space\n");
      return(NULLCELL);
    }
  if (name !=  NULLSTRING) 
    { 
      if (( Loc->name =new_nsp_string(name)) == NULLSTRING)
	return(NULLCELL);
    }
  else 
    {
      Loc->name =  NULLSTRING;
    }
  Loc->O = O;
  Loc->prev = Loc->next = NULLCELL;
  return(Loc);
} 


/**
 *nsp_cell_destroy:
 * @c: 
 * 
 * Delete the Cell and the object stored in it
 * 
 **/

void nsp_cell_destroy(Cell **c)
{
  if ((*c) != NULLCELL)
    {
      FREE((*c)->name);
      nsp_object_destroy(&(*c)->O);
      FREE((*c));
    }
} 


/**
 *nsp_list_destroy:
 * @l: 
 * 
 * Delete the NspList and all its elements 
 * 
 **/

void nsp_list_destroy(NspList *l)
{
  if (l != NULLLIST)
    {
      Cell *loc,*loc1;
      FREE(NSP_OBJECT(l)->name);
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
 *nsp_list_copy:
 * @L: 
 * 
 * returns a copy of the NspList L 
 * elements inside the list are copied too
 * 
 * 
 * Return value: 
 **/

NspList*nsp_list_copy(NspList *L)
{
  NspList *Loc;
  Cell *cloc,*cloc1=NULLCELL,*cloc2=NULLCELL;
  if ( ( Loc =nsp_list_create(NVOID,L->tname) ) == NULLLIST) return(NULLLIST) ;
  cloc = L->first ;
  while ( cloc != NULLCELL) 
    {
      NspObject *Oloc;
      if ( cloc->O == NULLOBJ ) 
	Oloc = NULLOBJ;
      else 
	{
	  if ((Oloc =nsp_object_copy_with_name(cloc->O))== NULLOBJ) return NULLLIST;
	}
      if ((cloc1 =nsp_cell_create(cloc->name,Oloc))== NULLCELL) return(NULLLIST);
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
  return(Loc);
} 

/**
 *nsp_list_extract:
 * @L: 
 * @Elts: 
 * 
 * Extract the Elements of a list  specified by Elts 
 * and store a copy of them in a new list 
 * 
 * 
 * Return value: 
 **/

NspList*nsp_list_extract(NspList *L, NspMatrix *Elts)
{
  NspList *Loc;
  int rmin,rmax,i,l;
  Bounds(Elts,&rmin,&rmax);
  l =nsp_list_length(L);
  if ( rmin < 1  || rmax > l) 
    {
      Scierror("Error:\tIndices out of bounds\n");
      return(NULLLIST);
    }
  if ( ( Loc =nsp_list_create(NVOID,L->tname) ) == NULLLIST) return(NULLLIST) ;
  for ( i = 0 ; i < Elts->mn ; i++ )
    {
      /* Pourrait etre ameliore en construisant la liste petit a petit XXXXXX **/
      NspObject *O;
      O =nsp_list_get_element(L,((int) Elts->R[i]));
      if ( O == NULLOBJ ) 
	{
	  Scierror("Error:\t%s does not exists\n",
		   ArgPosition(((int) Elts->R[i])));
	  return NULLLIST ;
	}
      if ( (O=nsp_object_copy(O)) == NULLOBJ ) return NULLLIST;
      if (nsp_object_set_name(O,"lel") == FAIL) return NULLLIST;
      if (nsp_list_end_insert(Loc,O) == FAIL) return NULLLIST;
    }
  return Loc;
} 


/**
 *nsp_list_insert:
 * @L: 
 * @O: 
 * @n: 
 * 
 * Insert Object O at position n 
 * If position n is 0 insert at the begining 
 * If position n exists in L the corresponding object 
 * is replaced 
 * If position n > Lenght(L) the list length is increased 
 * 
 * 
 * Return value: 
 **/

int nsp_list_insert(NspList *L, NspObject *O, int n)
{
  int count = 1;
  Cell *Loc,*Loc1;
  if ( n < 0 ) 
    {
      Scierror("Error:\tInvalid negative indice for list insertion %d\n",n);
      return(FAIL);
    }
  if ( n == 0 
       || ( L->first == NULLCELL && n==1 ))
    return(nsp_list_store(L,O,1));
  Loc  = L->first;
  if ( Loc != NULLCELL ) 
    while ( count < n && Loc->next != NULLCELL) 
      { Loc = Loc->next;count ++;}
  if ( count == n ) 
    {
      nsp_object_destroy(&Loc->O);
      Loc->O = O;
      return(OK) ;
    }
  while ( count != n ) 
    {
      if (( Loc1 =nsp_cell_create( NULLSTRING,NULLOBJ))== NULLCELL) return(FAIL);
      if ( Loc == NULLCELL) 
	{
	  /* L was an empty list we only get here once **/
	  count--;
	  L->first = Loc1;
	  Loc1->prev = NULLCELL;
	}
      else
	{
	  /* L was not an empty list **/
	  Loc->next = Loc1 ;
	  Loc1->prev = Loc;
	}
      Loc = Loc1;
      count++;
    }
  Loc->O= O;
  return(OK);
} 


/**
 *nsp_list_get_element:
 * @L: 
 * @nel: 
 * 
 * returns a pointer to the Nth (=nel) NspObject  of a List
 * or NULLOBJ 
 * 
 * 
 * Return value: 
 **/

NspObject *nsp_list_get_element(NspList *L, int nel)
{
  int count = 1;
  Cell *cell = L->first;
  if ( nel <= 0) 
    {
      Scierror("Error:\tNul or negative indice %d in list extraction\n",nel);
      return NULLOBJ;
    }
  while ( count < nel && cell != NULLCELL ) 
    { cell = cell->next;count ++;}
  if ( count != nel || cell == NULLCELL) 
    {
      Scierror( "Error:\tList too short %s not found\n",ArgPosition(nel));
      return( NULLOBJ );
    }
  else 
    {
      if ( cell->O == NULLOBJ )
	{
	  Scierror("Error:\t%s is Undefined\n",ArgPosition(nel));
	  return(cell->O);
	}
      return(cell->O);
    }
} 


/**
 *nsp_list_end_insert:
 * @L: 
 * @A: 
 * 
 * insert Object A at end of NspList   NspList must exists  
 * Object A is not copied 
 * 
 * 
 * Return value: 
 **/

int nsp_list_end_insert(NspList *L, NspObject *A)
{
  Cell *Loc,*Loc1;
  if (( Loc1 =nsp_cell_create( NULLSTRING,A))== NULLCELL) return(FAIL);
  Loc = L->first ;
  if ( Loc == NULLCELL) 
    {
      L->first = Loc1;
    }
  else 
    {
      while ( Loc->next != NULLCELL) Loc = Loc->next ;
      Loc->next = Loc1;
      Loc1->prev = Loc;
    }
  return(OK);
}


/**
 *nsp_list_store:
 * @L: 
 * @A: 
 * @n: 
 * 
 * insert Object A in a NspList at position n 
 * the previous object which was at position n is moved to n+1
 * The Object A is not copied
 *
 * Return value: 
 **/

int nsp_list_store(NspList *L, NspObject *A, int n)
{ 
  int count = 1;
  Cell *Loc = L->first, *Loc1;
  /* searching element n-1 */
  if ( Loc == NULLCELL) 
    {
      if ( n != 1) 
	{
	  int i = n;
	  Scierror( "List too short  element %d not found\n",i);
	  return(FAIL);
	}
      else 
	{
	  if (( Loc1 =nsp_cell_create(NULLSTRING,A))== NULLCELL) return(FAIL);
	  L->first = Loc1;
	}
    }
  else 
    {
      while ( count < n  && Loc->next != NULLCELL)
	{ Loc = Loc->next;count ++;}
      if ( count != n ) 
	{
	  int i = n;
	  Scierror( "List too short  element %d not found\n",i);
	  return( FAIL);
	}
      /* we want to insert Loc1 before Loc **/
      if (( Loc1 =nsp_cell_create( NULLSTRING,A))== NULLCELL) return(FAIL);
      Loc1->next= Loc ;
      Loc1->prev= Loc->prev;
      if ( Loc->prev == NULLCELL) 
	{
	  L->first = Loc1;
	}
      else 
	{
	  Loc->prev->next = Loc1;
	}
      Loc->prev = Loc1;
    }
  return(OK);
}


/**
 *nsp_list_delete_elt_by_name:
 * @L: 
 * @str: 
 * 
 * supresses the element of a NspList with name str
 * 
 **/

void nsp_list_delete_elt_by_name(NspList *L, char *str)
{
  Cell *Loc = L->first;
  while ( Loc != NULLCELL) 
    {
      if ( Loc->O != NULLOBJ && Ocheckname(Loc->O,str)) 
	{
	  if ( Loc->prev  == NULLCELL )
	    {
	      L->first = Loc->next ;
	      if ( L->first != NULLCELL) L->first->prev = NULLCELL;
	      nsp_cell_destroy(&Loc);
	      return;
	    }
	  else 
	    {
	      Loc->prev->next = Loc->next ;
	      if ( Loc->next != NULLCELL) Loc->next->prev = Loc->prev;
	      nsp_cell_destroy(&Loc);
	      return;
	    }
	}
      Loc = Loc->next;
    }
}


typedef void (*destr)( Cell **c);

static int DeleteNth_g(NspList *L, int nel, destr F)
{
  int count = 1;
  Cell *Loc = L->first;
  if ( Loc == NULLCELL ) return(OK);
  while ( count < nel && Loc->next != NULLCELL) 
    { Loc = Loc->next;count ++;}
  if ( count != nel ) 
    {
      int i = nel;
      Scierror("List too short  element %d not found\n",i) ;
      return(FAIL);
    }
  /* want to delete Loc **/
  if ( Loc->prev  == NULLCELL )
    {
      L->first = Loc->next ;
      if ( Loc->next != NULLCELL) L->first->prev = NULLCELL;
      F(&Loc);
    }
  else 
    {
      (Loc->prev)->next = Loc->next ;
      if ( Loc->next != NULLCELL) (Loc->next)->prev = Loc->prev;
      F(&Loc);
    }
  return(OK) ;
}


/**
 *nsp_list_delete_elt:
 * @L: 
 * @nel: 
 * 
 *
 * supresses the nth element of a NspList 
 * the nth element is destroyed 
 * 
 * Return value: 
 **/

int nsp_list_delete_elt(NspList *L, int nel)
{
  return DeleteNth_g(L, nel,nsp_cell_destroy);
}

/**
 *nsp_list_delete_cell:
 * @L: 
 * @nel: 
 * 
 * 
 * 
 * Return value: 
 **/

int nsp_list_delete_cell(NspList *L, int nel)
{
  return DeleteNth_g(L, nel,nsp_cell_only_destroy);
}



/**
 *nsp_list_length:
 * @L: 
 * 
 *  length of list l
 *
 * Return value: 
 **/
int nsp_list_length(NspList *L)
{
  int count = 0;
  Cell *Loc = L->first;
  while ( Loc != NULLCELL) { Loc = Loc->next;count ++;}
  return(count);
}


/**
 *nsp_list_concat:
 * @L1: 
 * @L2: 
 * 
 *  Concatenation of two NspList  L1 = Concat(L1,Copy of L2); 
 * 
 * Return value: 
 **/

int nsp_list_concat(NspList *L1, NspList *L2)
{
  Cell *Loc;
  NspList *L2copy;
  if ( L2->first == NULLCELL) return(OK);
  if ((L2copy=nsp_list_copy(L2)) == NULLLIST) return(FAIL);
  Loc = L1->first;
  if ( Loc == NULLCELL) 
    {
      L1->first = L2copy->first;
    }
  else 
    {
      while ( Loc->next != NULLCELL) { Loc = Loc->next; }
      Loc->next = L2copy->first;
      L2copy->first->prev=Loc;
    }
  L2copy->first= NULLCELL;
  nsp_list_destroy(L2copy);
  return(OK);
}

/*
 *Scilab 
 */

/**
 *nsp_list_info:
 * @L: 
 * @indent: 
 * 
 * Display of an Object of type NspList 
 *
 **/

void nsp_list_info(NspList *L, int indent)
{
  Cell *C;
  int i=1,j,len;
  for ( j=0 ; j < indent ; j++) Sciprintf(" ");
  if ( NSP_OBJECT(L)->name !=  NULLSTRING)
    {
      Sciprintf("List %s = (",NSP_OBJECT(L)->name);
      len=indent+strlen(NSP_OBJECT(L)->name)+9; 
    }
  else 
    {
      Sciprintf("List = (");
      len = indent+2+8;
    }
  if ( L->tname != NULLSTRING) Sciprintf(L->tname); 
  C= L->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  /* if ( C->name !=  NULLSTRING) Sciprintf("<%s>",C->name); **/
	  nsp_object_info(C->O,(i==1)? 0: len);      
	}
      else
	{
	  for ( j=0 ; j < (i==1)? 0: len ; j++) Sciprintf(" ");
	  Sciprintf("%d Undefined",i);
	}
      C = C->next ;
      i++;
    }
  for ( j=0 ; j < len-1 ; j++) Sciprintf(" ");
  Sciprintf(")\n");
} 


/**
 *nsp_list_print:
 * @L: 
 * @indent: 
 * 
 *  Scilab Display of an Object of type NspList 
 * 
 **/

void nsp_list_print(NspList *L, int indent)
{
  int j;
  Cell *C;
  int i=1;
  for ( j=0 ; j < indent ; j++) Sciprintf(" ");
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(NSP_OBJECT(L)->name,NVOID) != 0) Sciprintf("%s=",NSP_OBJECT(L)->name);
      Sciprintf("list(\n");
      C= L->first;
      while ( C != NULLCELL) 
	{
	  if ( C->O != NULLOBJ )
	    {
	      nsp_object_print(C->O,indent+1);
	    }
	  else
	    {
	      Sciprintf(" []");/* FIXME XXXX */
	    }
	  C = C->next;
	  if ( C != NULLCELL )
	    {
	      for ( j=0 ; j < indent+1; j++) Sciprintf(" ");
	      Sciprintf(",\n");
	    }
	}
      for ( j=0 ; j < indent ; j++) Sciprintf(" ");
      Sciprintf( ")\n");
    }
  else
    {
      Sciprintf("%s\t=\t\tl\n",(strcmp(NSP_OBJECT(L)->name,NVOID) != 0) ? NSP_OBJECT(L)->name : " ");
      for ( j=0 ; j < indent ; j++) Sciprintf(" ");
      Sciprintf("(\n");
      C= L->first;
      while ( C != NULLCELL) 
	{
	  if ( C->O != NULLOBJ )
	    {
	      nsp_object_print(C->O,indent+1);      
	    }
	  else
	    {
	      Sciprintf(" %d Undefined\n",i);
	    }
	  C = C->next ;i++;
	  if ( C != NULLCELL ) Sciprintf("\n");
	}
      for ( j=0 ; j < indent ; j++) Sciprintf(" ");
      Sciprintf( ")\n");
    } 
}


/*
 */

NspObject *ListSearch_Old(NspList *L, nsp_const_string str)
{
  Cell *C;
  C= L->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ &&  Ocheckname(C->O,str) ) 
	return(C->O);
      C = C->next ;
    }
  return NULLOBJ;
} 

/**
 *nsp_list_search:
 * @L: 
 * @str: 
 * 
 * Search for a NspList element named str in NspList L
 * XXX we are only supposed to search elts in tlist *
 * 
 * 
 * Return value: 
 **/


NspObject *nsp_list_search(NspList *L, nsp_const_string str)
{
  int i;
  Cell *C =  L->first;
  if ( L->tname == NULL ) return NULLOBJ;
  if ( C == NULLCELL ) return NULLOBJ;
  i = is_string_in_array(str,((NspSMatrix*) C->O)->S,1);
  if ( i <0  )  return NULLOBJ;
  return nsp_list_get_element(L,i+1);
} 


/**
 *nsp_list_search_and_remove:
 * @L: 
 * @str: 
 * 
 * Search for a NspList element named str in NspList L
 * returns this element and remove the 
 * element from the list (without destroying 
 * the returned object )
 * 
 * Return value: 
 **/

NspObject *nsp_list_search_and_remove(NspList *L, char *str)
{
  Cell *Loc = L->first;
  NspObject *Ret;
  while ( Loc != NULLCELL) 
    {
      if (Loc->O != NULLOBJ && Ocheckname(Loc->O,str)) 
	{
	  Ret = Loc->O;
	  if ( Loc->prev  == NULLCELL )
	    {
	      L->first = Loc->next ;
	      if ( L->first != NULLCELL) L->first->prev = NULLCELL;
	      nsp_cell_only_destroy(&Loc);
	      return Ret;
	    }
	  else 
	    {
	      Loc->prev->next = Loc->next ;
	      if ( Loc->next != NULLCELL) Loc->next->prev = Loc->prev;
	      nsp_cell_only_destroy(&Loc);
	      return Ret;
	    }
	}
      Loc = Loc->next;
    }
  return NULLOBJ;
}



/**
 *nsp_list_search_and_replace:
 * @L: 
 * @O: 
 * 
 * Search for a NspList element with the same name 
 * as O and replace this element with new Object O
 * returning true or false 
 * 
 * 
 * Return value: 
 **/

int nsp_list_search_and_replace(NspList *L, NspObject *O)
{
  int i;
  Cell *C = L->first;
  if ( L->tname == NULL ) return FAIL;
  if ( C == NULLCELL ) return FAIL;
  i = is_string_in_array(NSP_OBJECT(O)->name ,((NspSMatrix*) C->O)->S,1);
  if ( i <0  )  return FAIL;
  return nsp_list_insert(L,O,i+1);
}
	

/**
 *nsp_cell_only_destroy:
 * @c: 
 * 
 * Delete the Cell but not the stored NspObject 
 * 
 **/

void nsp_cell_only_destroy(Cell **c)
{
  if ((*c) != NULLCELL)
    {
      FREE((*c)->name);
      FREE((*c));
    }
} 

/*
 */

/**
 *nsp_sorted_list_search:
 * @L: 
 * @str: 
 * 
 * Search for a NspList element named str in a Sorted NspList L
 * 
 * 
 * Return value: 
 **/

NspObject *nsp_sorted_list_search(NspList *L, nsp_const_string str)
{
  Cell *C;
  C= L->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{ 
	  int cmp = strcmp(str,NSP_OBJECT(C->O)->name);
	  if ( cmp == 0) return(C->O);
	  if ( cmp <  0) return NULLOBJ ;
	}
      C = C->next ;
    }
  return NULLOBJ;
} 


/**
 *nsp_sorted_list_search_and_remove:
 * @L: 
 * @str: 
 * 
 * Search for a NspList element named @str in #NspList @L
 * returns this element and remove the 
 * element from the list (without destroying 
 * the returned object )
 * 
 * Return value: a #NspObject or %NULLOBJ
 **/

NspObject *nsp_sorted_list_search_and_remove(NspList *L, nsp_const_string str)
{
  Cell *Loc = L->first;
  NspObject *Ret;
  while ( Loc != NULLCELL) 
    {
      if (Loc->O != NULLOBJ) 
	{
	  int cmp = strcmp(str, NSP_OBJECT(Loc->O)->name);
	  if ( cmp ==  0) 
	    {
	      Ret = Loc->O;
	      if ( Loc->prev  == NULLCELL )
		{
		  L->first = Loc->next ;
		  if ( L->first != NULLCELL) L->first->prev = NULLCELL;
		  nsp_cell_only_destroy(&Loc);
		  return Ret;
		}
	      else 
		{
		  Loc->prev->next = Loc->next ;
		  if ( Loc->next != NULLCELL) Loc->next->prev = Loc->prev;
		  nsp_cell_only_destroy(&Loc);
		  return Ret;
		}
	    }
	  if ( cmp < 0 ) return NULLOBJ;
	}
      Loc = Loc->next;
    }
  return NULLOBJ;
}


/**
 *nsp_sorted_list_insert:
 * @L: 
 * @O: 
 * 
 *nsp_sorted_list_insert(L,O)
 * Insert Object O using lexicographic order 
 * If an Object with same name exists in the list O will replace it 
 * 
 * Return value: 
 **/

int nsp_sorted_list_insert(NspList *L, NspObject *O)
{
  Cell *Loc,*Loc1,*Loc2;
  Loc= Loc1 = L->first;
  if ( Loc == NULLCELL ) return nsp_list_store(L,O,1);
  while ( Loc != NULLCELL) 
    { 
      int cmp =  strcmp(NSP_OBJECT(O)->name,NSP_OBJECT(Loc->O)->name) ;
      if ( cmp == 0 )   
	{
	  /* we replace Object by the new one **/ 
	  nsp_object_destroy(&Loc->O);
	  Loc->O = O;
	  return(OK) ;	
	}
      if ( cmp < 0 ) 
	{
	  /* NspObject must be inserted before Loc **/
	  if (( Loc2 =nsp_cell_create( NULLSTRING,NULLOBJ))== NULLCELL) return(FAIL);
	  Loc2->O = O ; 
	  Loc2->next = Loc;
	  Loc2->prev = Loc->prev ; 
	  if ( Loc->prev == NULLCELL)
	    L->first = Loc2  ; 
	  else
	    Loc->prev->next = Loc2;
	  Loc->prev = Loc2;
	  return OK;
	}
      Loc1 = Loc;
      Loc = Loc->next;
    }
  /* Here we must insert NspObject after Loc1 and Loc1 is not the first cell **/
  if (( Loc2 =nsp_cell_create( NULLSTRING,NULLOBJ))== NULLCELL) return(FAIL);
  Loc1->next = Loc2 ;
  Loc2->prev = Loc1;
  Loc2->O = O;
  return(OK);
} 


/**
 *nsp_list_map:
 * @L: 
 * @PL: 
 * @args: 
 * 
 * Map 
 * Map(L,function,args) 
 * function can be a primitive a macro a string 
 * ==> XXXX
 * 
 * 
 * Return value: 
 **/

NspList*nsp_list_map(NspList *L, NspPList *PL, NspList *args)  
{
  NspObject *O[2];
  int first = -1;
  NspList *L_map;
  Cell *L_cell,*cell1=NULLCELL,*cell2=NULLCELL;
  if ( ( L_map =nsp_list_create(NVOID,L->tname) ) == NULLLIST) return(NULLLIST) ;
  L_cell = L->first ;
  O[1]=NULLOBJ;
  while ( L_cell != NULLCELL) 
    {
      O[0] = L_cell->O;
      if ( O[0] != NULLOBJ ) 
	{
	  if ((O[0] =nsp_object_copy_with_name(O[0]))== NULLOBJ) return NULLLIST;
	  /* stack position to use is computed on the first call and set 
	   * for next calls in first 
	   */
	  if ((O[0] = EvalMacro(PL,O,args,&first))== NULLOBJ) return NULLLIST;
	  if ((cell1 =nsp_cell_create(L_cell->name,O[0]))== NULLCELL) return(NULLLIST);
	}
      else 
	{
	  if ((cell1 =nsp_cell_create(L_cell->name,O[0]))== NULLCELL) return(NULLLIST);
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
    }
  return L_map;
} 



static NspObject *cell_fold_right(Cell *C, NspPList *PL, NspList *args,int *first) ;


/**
 *nsp_list_fold_right:
 * @L: 
 * @PL: 
 * @args: 
 * 
 * FoldRight  
 * FoldRight(L,f,args) 
 * returns f(L(1),f(L(2),....),args(1),..,args(n))
 * function can be a primitive a macro a string 
 * ==> XXXX
 * 
 * Return value: 
 **/

NspObject *nsp_list_fold_right(NspList *L, NspPList *PL, NspList *args)
{
  int first = -1;
  return cell_fold_right(L->first, PL,args,&first);
}

static NspObject *cell_fold_right(Cell *C, NspPList *PL, NspList *args,int *first)  
{
  NspObject *O[3];
  /* limit case */
  if ( C == NULLCELL || C->next == NULLCELL ) 
    {
      Scierror("foldr: NspList is too short\n");
      return NULLOBJ ; 
    }
  if ( (O[0]= C->O) == NULLOBJ  ||  (O[1]= C->next->O)  == NULLOBJ )
    {
      Scierror("foldr: NspList with unknown elements are not allowed \n");
      return NULLOBJ ; 
    }
  if ( C->next->next == NULLCELL) 
    {
      O[2]= NULLOBJ;
      return EvalMacro(PL,O,args,first);
    }
  else 
    {
      O[1]= cell_fold_right(C->next, PL,args,first);
      if ( O[1] == NULLOBJ ) return NULLOBJ; 
      return EvalMacro(PL,O,args,first);
    }
  return NULLOBJ;
} 


/**
 *nsp_list_equal:
 * @L1: 
 * @L2: 
 * 
 *nsp_list_equal(L1,L2)
 * if the two list do not have the same length returns %f 
 * else returns a Boolean vector B such that B(i)= L1(i)== L2(i) 
 * XXXX quand l'op de comparaison n'existe pas il faudrait metre un mes d'erreur 
 * 
 * 
 * Return value: 
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
 * @L1: 
 * @L2: 
 * 
 * 
 * 
 * Return value: 
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
 * @L1: 
 * @L2: 
 * 
 *nsp_list_equal(L1,L2)
 * if the two list do not have the same length returns FALSE 
 * else returns and(L1(i)== L2(i)) 
 * 
 * 
 * Return value: 
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
 * @L1: 
 * @L2: 
 * 
 * 
 * 
 * Return value: 
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
 *nsp_list_compact:
 * @L1: 
 * @flag: 
 * 
 *  Compact list by column or row appendind elemnt of compatible size and 
 *  type 
 * 
 * 
 * Return value: 
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
		      /* 2 consecutive arguments are scalar matrices */
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
  return OK;
}







