/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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
 */

/*
 * PList means Parsed lists : parsed lists are stored in PList structures 
 * and they can be used within nsp through NspPList objects (in fact this is 
 * only used for nsp coded functions).
 * This file contains basic functions to build and manage PList 
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nsp/machine.h"
#include "nsp/object.h"
#include "nsp/astnode.h"
#include "nsp/interf.h"
#include "nsp/plistc.h"
#include "nsp/pr-output.h"
#include "Functions.h" 

/**
 * nsp_parse_add:
 * @plist: 
 * @op: 
 * @arity: 
 * @line: 
 * 
 * adds a new #PList at start of @plist. @plist can be empty 
 * and in that case the new created #PList is returned.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_parse_add(PList *plist, int op, int arity, int line)
{
  PList loc = *plist,loc1;
  if ( (loc1 =nsp_eplist_create()) == NULLPLIST )
    return(FAIL);
  loc1->type = op;
  loc1->arity = arity ;
  loc1->O = (void *) NSP_INT_TO_POINTER(line);
  *plist = loc1;
  loc1->next = loc;
  loc1->prev = NULL;
  if ( loc != NULLPLIST) loc->prev = loc1;
  return OK;
}

/**
 * nsp_parse_add_last:
 * @plist: 
 * @op: 
 * @arity: 
 * @line: 
 * 
 * adds a new #PList at end of @plist. @plist can be empty 
 * and in that case the new created #PList is returned.

 * 
 * Return value: %OK or %FAIL
 **/

int nsp_parse_add_last(PList *plist, int op, int arity, int line)
{
  PList loc = *plist,loc1;
  if ( (loc1 =nsp_eplist_create()) == NULLPLIST )
    return(FAIL);
  loc1->type = op;
  loc1->arity = arity ;
  loc1->O = (void *) NSP_INT_TO_POINTER(line);
  if ( loc == NULLPLIST) 
    {
      *plist = loc1;
    }
  else 
    {
      while ( loc->next != NULLPLIST ) loc=loc->next;
      loc->next = loc1;
      loc1->prev = loc;
    }
  return(OK);
}

/*
 * Add a <<name>> at end of current list 
 * if list is empty a list is created 
 */

/**
 * ParseAdd_name:
 * @plist: 
 * @str: 
 * @tag: 
 * @arity: 
 * 
 * adds a new #PList containing a tag at end of current list 
 * if list is empty a list is created. 
 * 
 * Return value: %OK or %FAIL
 **/

static int ParseAdd_name(PList *plist, char *str,int tag,int arity)
{
  PList loc = *plist,loc1;
  if ( (loc1 =nsp_eplist_create()) == NULLPLIST ) return(FAIL);
  if (( loc1->O =new_nsp_string(str)) == NULLSTRING) return(FAIL);
  loc1->type = tag;
  loc1->arity = arity; /* for names arity can be used as a work integer */
  if ( loc == NULLPLIST) 
    {
      *plist = loc1;
    }
  else 
    {
      while ( loc->next != NULLPLIST ) loc=loc->next;
      loc->next = loc1;
      loc1->prev = loc;
    }
  return(OK);
}

/**
 * nsp_parse_add_name:
 * @plist: 
 * @str: 
 * 
 * adds a new #PList containing a name at end of current list 
 * if list is empty a list is created. 
 * 
 * 
 * Return value: 
 **/

int nsp_parse_add_name(PList *plist, char *str)
{
  return  ParseAdd_name(plist,str,NAME,-1);
}

int nsp_parse_add_name1(PList *plist, char *str,int arity)
{
  return  ParseAdd_name(plist,str,NAME,arity);
}



/**
 * nsp_parse_add_opname:
 * @plist: 
 * @str: 
 * 
 * adds a new #PList containing an opname at end of current list 
 * if list is empty a list is created. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_parse_add_opname(PList *plist, char *str)
{
  return  ParseAdd_name(plist,str,OPNAME,-1);
}

/* 
 */

/**
 * nsp_parse_add_object:
 * @plist: 
 * @obj: 
 * 
 * adds a new "PList at end of @plist containing a nsp object (without copy).
 * This is mainly used for functions to store a symbol table.
 * 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_parse_add_object(PList *plist, NspObject *obj )
{
  PList loc = *plist,loc1;
  if ( (loc1 =nsp_eplist_create()) == NULLPLIST ) return(FAIL);
  loc1->O = obj;
  loc1->type = OBJECT ;
  if ( loc == NULLPLIST) 
    {
      *plist = loc1;
    }
  else 
    {
      while ( loc->next != NULLPLIST ) loc=loc->next;
      loc->next = loc1;
      loc1->prev = loc;
    }
  return(OK);
}


/**
 * nsp_parse_add_string:
 * @plist: 
 * @str: 
 * 
 * adds a new #PList containing a string at end of current list 
 * if list is empty a list is created. 
 * 
 * 
 * Return value: %OK or %FAIL
 **/
int nsp_parse_add_string(PList *plist, char *str)
{
  PList loc = *plist,loc1;
  if ( (loc1 =nsp_eplist_create()) == NULLPLIST ) return(FAIL);
  if (( loc1->O =new_nsp_string(str)) == NULLSTRING) return(FAIL);
  loc1->type = STRING;
  if ( loc == NULLPLIST) 
    {
      *plist = loc1;
    }
  else 
    {
      while ( loc->next != NULLPLIST ) loc=loc->next;
      loc->next = loc1;
      loc1->prev = loc;
    }
  return(OK);
}

/**
 * nsp_parse_add_comment:
 * @plist: 
 * @str: 
 * 
 * 
 * adds a new #PList containing a comment at end of current list 
 * if list is empty a list is created. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_parse_add_comment(PList *plist, char *str)
{
  PList loc = *plist,loc1;
  if ( (loc1 =nsp_eplist_create()) == NULLPLIST ) return(FAIL);
  if (( loc1->O =new_nsp_string(str)) == NULLSTRING) return(FAIL);
  loc1->type = COMMENT;
  if ( loc == NULLPLIST) 
    {
      *plist = loc1;
    }
  else 
    {
      while ( loc->next != NULLPLIST ) loc=loc->next;
      loc->next = loc1;
      loc1->prev = loc;
    }
  return(OK);
}

/**
 * nsp_parse_add_list:
 * @plist: 
 * @l: 
 * 
 * adds the #PList @l as last element of #PList @plist
 * plist=(a b c) l=(1 2 3) --> (a b c (1 2 3))
 * 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_parse_add_list(PList *plist, PList *l)
{
  PList loc = *plist,loc1;
  if ( *l == NULLPLIST ) return(OK);
  if ( (*l)->next == NULLPLIST  ) 
    {
      loc1= *l;
    }
  else 
    {
      if ( (loc1 =nsp_eplist_create()) == NULLPLIST )
	return(FAIL);
      loc1->O = *l;
      loc1->type = PLIST;
    }
  if ( loc == NULLPLIST) 
    {
      *plist = loc1;
    }
  else 
    {
      while ( loc->next != NULLPLIST ) loc=loc->next;
      loc->next = loc1;
      loc1->prev = loc;
    }
  return(OK);
}


/**
 * nsp_parse_append:
 * @plist: 
 * @l: 
 * 
 * appends @plist and @l. 
 * plist=(a b c) l=(1 2 3) --> (a b c 1 2 3)
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_parse_append(PList *plist, PList *l)
{
  PList loc = *plist;
  if ( *l == NULLPLIST ) return(OK);
  if ( loc == NULLPLIST) 
    {
      *plist = *l ;
    }
  else 
    {
      while ( loc->next != NULLPLIST ) loc=loc->next;
      loc->next = *l;
      (*l)->prev = loc;
    }
  return(OK);
}


/**
 * nsp_parse_add_list1:
 * @plist: 
 * @l: 
 * 
 * fills @plist with @l inserted in a list i.e plist= (l).
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_parse_add_list1(PList *plist, PList *l)
{
  PList loc1;
  if ((loc1 =nsp_eplist_create()) == NULLPLIST )  return(FAIL);
  loc1->type = PLIST ;
  loc1->O = *l;
  *plist = loc1;
  return(OK);
}

/**
 * new_nsp_parsed_double:
 * @str: a string containing a double representation 
 * 
 * 
 * Returns: a new #parse_double 
 **/

static parse_double *new_nsp_parsed_double(nsp_string str)
{
  parse_double *p;
  if (( p = malloc(sizeof(parse_double)))== NULL) return NULL;
  if (( p->str =new_nsp_string(str)) == NULLSTRING) return NULL;
  p->val = atof(str);
  return p;
}

/**
 * nsp_parse_add_doublei:
 * @plist: 
 * @str: 
 * 
 * adds a new #PList containing a string coding the double value (@str) and a double value at end of 
 * current #PList @plist. if list is empty a list is created. 
 * 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_parse_add_doublei(PList *plist, char *str)
{
  PList loc = *plist,loc1;
  if ( (loc1 =nsp_eplist_create()) == NULLPLIST ) return(FAIL);
  if (( loc1->O = new_nsp_parsed_double(str)) == NULLSTRING) return(FAIL);
  loc1->type = NUMBER;
  if ( loc == NULLPLIST) 
    {
      *plist = loc1;
    }
  else 
    {
      while ( loc->next != NULLPLIST ) loc=loc->next;
      loc->next = loc1;
      loc1->prev = loc;
    }
  return(OK);
}
/**
 * new_nsp_parsed_int:
 * @str: a string containing an int representation 
 * 
 * 
 * Returns: a new #parse_int 
 **/

#define STR2NUM(val)					\
  for ( i = 0 ; i < len  ; i++ )			\
    {							\
      int code = str[i];				\
      if ( code >= '0' && code <= '9')			\
	code -= '0';					\
      else						\
	return p;					\
      val = 10*(val) + code;				\
    }							\
  if ( str[0] == '-' ) val -= val;

static parse_int *new_nsp_parsed_int(nsp_string str, int type)
{
  parse_int *p;
  int len = strlen(str);
  int i;
  if (( p = malloc(sizeof(parse_int)))== NULL) return NULL;
  if (( p->str =new_nsp_string(str)) == NULLSTRING) return NULL;
  switch ( type )
    {
    case INUMBER32: p->Gint32 =0; STR2NUM(p->Gint32);break;
    case INUMBER64: p->Guint32 =0;STR2NUM(p->Guint32);break;
    case UNUMBER32: p->Gint64 =0; STR2NUM(p->Gint64);break;
    case UNUMBER64: p->Guint64 =0; STR2NUM(p->Guint64);break;
    }
  return p;
}

/**
 * nsp_parse_add_inti:
 * @plist: 
 * @str: 
 * 
 * adds a new #PList containing a string coding the int value (@str) and a int value at end of 
 * current #PList @plist. if list is empty a list is created. 
 * 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_parse_add_inti(PList *plist, char *str, int type)
{
  PList loc = *plist,loc1;
  if ( (loc1 =nsp_eplist_create()) == NULLPLIST ) return(FAIL);
  if (( loc1->O = new_nsp_parsed_int(str,type)) == NULLSTRING) return(FAIL);
  loc1->type = type;
  if ( loc == NULLPLIST) 
    {
      *plist = loc1;
    }
  else 
    {
      while ( loc->next != NULLPLIST ) loc=loc->next;
      loc->next = loc1;
      loc1->prev = loc;
    }
  return(OK);
}


/**
 * nsp_eplist_create:
 * @void: 
 * 
 * returns a new #PList object which is used as cell element of an abstract 
 * syntaxic tree.
 * 
 * Return value: a new #PList or %NULLPLIST
 **/

PList nsp_eplist_create(void)
{
  PList Loc;
  Loc = (PList) MALLOC(sizeof(PCell));
  if (Loc == NULLPLIST)
    {
      Scierror("Parse: Memory exhausted\n");
      return(NULLPLIST);
    }
  Loc->next = NULLPLIST;
  Loc->prev = NULLPLIST;
  return(Loc);
}

/**
 * nsp_plist_destroy:
 * @List: 
 * 
 * destroys @List and all its cells.
 * 
 **/

void nsp_plist_destroy(PList *List)
{
  PList loc = *List,loc1 ;
  NspObject *Obj = NULL;
  while ( loc != NULLPLIST ) 
    {
      switch (loc->type) 
	{
	case NAME :
	case OPNAME :
	case STRING :
	case COMMENT :
	  FREE(loc->O);
	  break;
	case OBJECT : 
	  Obj = loc->O;
	  nsp_object_destroy(&Obj);
	  break;
	case NUMBER :
	  FREE(((parse_double *)loc->O)->str);
 	  FREE(loc->O);
	  break;
	case INUMBER32 :
	case INUMBER64 :
	case UNUMBER32 :
	case UNUMBER64 :
	  FREE(((parse_int *)loc->O)->str);
 	  FREE(loc->O);
	  break;
	case PLIST: 
	  loc1= (PList) loc->O;
	  nsp_plist_destroy(&loc1);
	  break;
	}
      loc1= loc->next ;
      FREE( loc );
      loc = loc1;
    }
  *List = NULLPLIST;
} 

/* copy of a plist
 *
 */

static PList _nsp_plist_copy(PList L,int tag)
{
  NspObject *obj;
  PList Res = NULLPLIST,loc=NULLPLIST,loc1=NULLPLIST, Res_last = NULLPLIST;
  /* we keep track of last element to accelerate the copy */
  while ( L  != NULLPLIST ) 
    {
      switch ( L->type) 
	{
	case NAME :
	  if ( ParseAdd_name(&Res_last,(char*) L->O,NAME,(tag==TRUE) ? L->arity: -1)==FAIL) return(NULLPLIST);
	  break;
	case OPNAME :
	  if (nsp_parse_add_opname(&Res_last,(char*) L->O)==FAIL) return(NULLPLIST);
	  break;
	case OBJECT :
	  if ((obj=nsp_object_copy_with_name(L->O)) == NULLOBJ) return(NULLPLIST); 
	  if (nsp_parse_add_object(&Res_last,obj)==FAIL) return(NULLPLIST);
	  break;
	case STRING :
	  if (nsp_parse_add_string(&Res_last,(char*) L->O)==FAIL) return(NULLPLIST);
	  break;
	case COMMENT :
	  if (nsp_parse_add_comment(&Res_last,(char*) L->O)==FAIL) return(NULLPLIST);
	  break;
 	case NUMBER :
	  if (nsp_parse_add_doublei(&Res_last,((parse_double *) L->O)->str)==FAIL) return(NULLPLIST);
	  break;
	case INUMBER32 :
	case INUMBER64 :
	case UNUMBER32 :
	case UNUMBER64 :
	  if (nsp_parse_add_inti(&Res_last,((parse_double *) L->O)->str,L->type)==FAIL) return(NULLPLIST);
	  break;
	case PLIST: 
	  if ((loc=_nsp_plist_copy((PList) L->O,tag)) == NULLPLIST) return(NULLPLIST);
	  if (nsp_parse_add_list1(&loc1,&loc) == FAIL) return (NULLPLIST);
	  if (nsp_parse_add_list(&Res_last,&loc1)== FAIL) return(NULLPLIST);
	  break;
	default: 
	  if (nsp_parse_add_last(&Res_last,L->type,L->arity,NSP_POINTER_TO_INT( L->O)) == FAIL) 
	    return(NULLPLIST);
	}
      L= L->next;
      if ( Res == NULL) Res = Res_last; 
      while ( Res_last->next != NULL) Res_last = Res_last->next;
    }
  return(Res);
}

/**
 * nsp_plist_copy:
 * @L: 
 * 
 * returns a copy of @L. 
 * 
 * Return value: a new #PList or %NULLPLIST
 **/

PList nsp_plist_copy(PList L)
{
  return _nsp_plist_copy(L,TRUE);
}
 
/**
 * nsp_plist_copy_no_local_vars:
 * @L: 
 * 
 * returns a copy of @L in xhich local variables are not tagged.
 * 
 * Return value: a new #PList or %NULLPLIST
 **/

PList nsp_plist_copy_no_local_vars(PList L)
{
  return _nsp_plist_copy(L,FALSE);
}

/* converts a PList L to a list of astnode that 
 * can be used at nsp level.
 */

/**
 * _nsp_list_add:
 * @list: 
 * @op: 
 * @arity: 
 * @data: 
 * 
 * creates a new astnode and insert it in a #NspList 
 * 
 * Return value: %OK or %FAIL.
 **/

static int _nsp_list_add(NspList **list, int op, int arity,void *data)
{
  int_types T[]={obj,t_end} ;
  NspAstNode *astn;
  NspObject *Obj;
  if((astn=astnode_create("L", op, arity,data,NULL)) == NULLASTNODE ) 
    return FAIL;
  if ( *list == NULL )
    {
      if (( Obj = (NspObject *) BuildListFromArgs("lel",T,astn)) == NULLOBJ ) 
	return FAIL;
      *list = (NspList *) Obj;
      return OK;
    }
  return nsp_list_end_insert(*list,NSP_OBJECT(astn));
}

/**
 * _nsp_list_add_list:
 * @list: 
 * @L: 
 * 
 * Add a list l as last element of list plist 
 * plist=(a b c) l=(1 2 3) --> (a b c (1 2 3))
 * 
 * 
 * Return value: 
 **/

static int _nsp_list_add_list(NspList **list,NspList *L)
{
  if ( *list == NULL )
    {
      *list = L;
      return OK;
    }
  return nsp_list_end_insert(*list,NSP_OBJECT(L));
}

/**
 * nsp_plist_to_list:
 * @L: 
 * 
 * converts a #PList to a nsp #NspList with #NspAstNode as elements. 
 * The #NspList can be used at nsp level.
 * 
 * Return value: a new #NspList or %NULLIST.
 **/

NspList *nsp_plist_to_list(PList L)
{
  NspObject *obj;
  NspList *Res = NULL,*loc=NULL;
  while ( L  != NULLPLIST ) 
    {
      switch ( L->type) 
	{
	case OBJECT :
	  if ((obj=nsp_object_copy(L->O)) == NULLOBJ) return(NULLLIST); 
	  if ( _nsp_list_add(&Res,L->type,L->arity,obj)==FAIL) return(NULLLIST);
	  break;
	case NUMBER :
	  if ( _nsp_list_add(&Res,L->type,L->arity,((parse_double *) L->O)->str)==FAIL) 
	    return(NULLLIST);
	  break;
	case INUMBER32 :
	case INUMBER64 :
	case UNUMBER32 :
	case UNUMBER64 :
	  if ( _nsp_list_add(&Res,L->type,L->arity,((parse_int *) L->O)->str)==FAIL) 
	    return(NULLLIST);
	  break;
	case PLIST: 
	  if ((loc= nsp_plist_to_list((PList) L->O)) == NULLLIST) return(NULLLIST);
	  /* if ( _nsp_list_add_list1(&loc1,loc) == FAIL) return (NULLLIST); */
	  if ( _nsp_list_add_list(&Res,loc)== FAIL) return(NULLLIST);
	  break;
	default: 
	  if ( _nsp_list_add(&Res,L->type,L->arity,L->O)==FAIL) return(NULLLIST);
	}
      L= L->next;
    }
  return(Res);
} 

/**
 * nsp_last:
 * @plist: 
 * 
 * returns the last cell of a #PList. 
 * 
 * Return value: 
 **/

PList nsp_last(PList plist)
{
  if ( plist == NULLPLIST) return NULLPLIST;
  while (1) 
    {
      if ( plist->next == NULLPLIST ) return(plist);
      plist=plist->next ;
    }
}

/**
 * nsp_plist_print_internal:
 * @L: 
 * 
 * prints a #PList in a format which highlight the internal coding. 
 **/

static void _nsp_plist_print_internal(PList L, int indent);

void nsp_plist_print_internal(PList L)
{
  _nsp_plist_print_internal(L,4);
}

static void _nsp_plist_print_internal(PList L, int indent)
{
  const char *s;
  int i=0;
  Sciprintf("(");
  while ( L != NULLPLIST ) 
    {
      switch ( L->type ) 
	{
	case STRING:
	  Sciprintf("\"%s\"",(char *) L->O);
	  break;
	case COMMENT:
	  Sciprintf1(indent,"//%s",(char *) L->O);
	  break;
	case NUMBER:
	  Sciprintf("%s",((parse_double *) L->O)->str);
	  break;
	case INUMBER32 :
	case INUMBER64 :
	case UNUMBER32 :
	case UNUMBER64 :
	  Sciprintf("%s",((parse_int *) L->O)->str);
	  break;
	case NAME :
	  Sciprintf("%s",(char *) L->O);
	  break;
	case OPNAME :
	  Sciprintf("'%s'",(char *) L->O);
	  break;
	case OBJECT : 
	  Sciprintf("obj",(char *) L->O);
	  break;
	case PLIST:
	  _nsp_plist_print_internal((PList) L->O,indent+1);
	  break;
	case EMPTYMAT:
	  Sciprintf("\"[]\"");break;
	case EMPTYCELL:
	  Sciprintf("\"{}\"");break;
	default:
	  if ( L->arity != i ) 
	    {
	      Sciprintf("[a:%d, args:%d]",L->arity,i);
	    }
	  s=nsp_astcode_to_name(L->type);
	  if ( s != (char *) 0 )
	    Sciprintf("\"%d#%s[line %d]",L->arity,s,NSP_POINTER_TO_INT(L->O));
	  else 
	    Sciprintf("\"UNKNOWN->%d",L->type);
	  Sciprintf("\"");
	}
      L = L->next ;
      if ( L != NULLPLIST ) Sciprintf(" ");
      i++;
    }
  Sciprintf(")");Sciprintf1(indent-1,"\n");
} 


/*
 */

static int _nsp_plist_pretty_print_args (PList L,int Larity,int indent,int pos,int posret, char *sep);
static int _nsp_plist_pretty_print(PList List, int indent, int pos, int posret);
static int _nsp_plist_pretty_print_opname(int type, int indent, int pos);
static int _nsp_plist_pretty_print_args(PList List, int Larity, int indent, int pos, int posret, char *sep);
static int _nsp_plist_pretty_print_arg(PList L, int i, int pos, int posret);
static int _nsp_plist_pretty_print_arg_ret(PList L, int i, int pos, int posret, int *ret);

/**
 * nsp_plist_pretty_print:
 * @L: 
 * @indent: the curent indentation to use.
 * 
 * pretty printing of a #PList. 
 * 
 **/

void nsp_plist_pretty_print(PList L, int indent)
{
  _nsp_plist_pretty_print(L,indent,0,indent);
}

typedef enum { p_black =30, 
               p_red =31,
               p_green=32,
               p_yellow=33,
               p_blue=34,
               p_purple=35,
               p_cyan=36,
               p_white=37} nsp_term_colors;

#define NSP_PRINTF_COLOR(col,str)					\
  ((user_pref.color == TRUE ) ? Sciprintf("\033[%dm%s\033[0m",col,str)	\
   :Sciprintf("%s",str));

#define NSP_PRINTF1_COLOR(posret,col,str) \
  ((user_pref.color == TRUE ) ? Sciprintf1(posret,"\033[%dm%s\033[0m",col,str) \
   : Sciprintf1(posret,"%s",str))

#define CMAX 50

#define PRINTTAG(tag)							\
  if (pos != posret ) {							\
    Sciprintf("\n");newpos = NSP_PRINTF1_COLOR(posret,p_blue,tag) ;}	\
  else { newpos = pos+ NSP_PRINTF_COLOR(p_blue,tag) ; }

static int _nsp_plist_pretty_print(PList List, int indent, int pos, int posret)
{
  PList L=List;
  const char *s;
  int j,newpos=0,ret=FALSE;
  /* just in case L is not the first */
  while ( L->prev != NULL) L= L->prev;
  List = L->next;
  if ( L->type > 0 ) 
    {
      /* operators **/
      switch ( L->arity ) 
	{
	case 0:
	  /* : can be a 0 arity operator **/
	  return _nsp_plist_pretty_print_opname(L->type,indent,pos);
	  break;
	case 1:
	  switch ( L->type ) 
	    {
	    case  COMMA_OP : 
	    case  SEMICOLON_OP  :
	      newpos =_nsp_plist_pretty_print_arg(List,indent,pos,posret);
	      newpos =_nsp_plist_pretty_print_opname(L->type,0,newpos);
	      Sciprintf("\n");
	      return 0;
	      break;
	    case QUOTE_OP : 
	    case DOTPRIM:
	      newpos =_nsp_plist_pretty_print_arg(List,indent,pos,posret);
	      newpos =_nsp_plist_pretty_print_opname(L->type,0,newpos);
	      return  newpos;
	      break;
	    case RETURN_OP : 
	      _nsp_plist_pretty_print_arg(List,indent,pos,posret);
	      Sciprintf("\n");
	      return 0;
	      break;
	    case TILDE_OP : 
	    default:
	      newpos =_nsp_plist_pretty_print_opname(L->type,indent,pos);
	      newpos =_nsp_plist_pretty_print_arg(List,0,newpos,posret);
	      return newpos;
	    }
	  break;
	case 2:
	  newpos =_nsp_plist_pretty_print_arg(List,indent,pos,posret);
	  newpos =_nsp_plist_pretty_print_opname(L->type,0,newpos);
	  if ( newpos > CMAX) 
	    {
	      Sciprintf("\n");
	      newpos =_nsp_plist_pretty_print_arg(List->next,posret,0,posret);
	    }
	  else
	    {
	      newpos =_nsp_plist_pretty_print_arg(List->next,0,newpos,posret);
	    }
	  return newpos;
	  break;
	default :
	  newpos = pos;
	  for ( j = 0 ; j <  L->arity ; j++ )
	    {
	      newpos =_nsp_plist_pretty_print_arg(List,(j == 0) ? indent : 1,
					   newpos,posret);
	      if ( j != L->arity -1 ) 
		newpos =_nsp_plist_pretty_print_opname(L->type,1,newpos);
	      List = List->next;
	    }
	  break;
	}
    }
  else 
    {
      switch ( L->type ) 
	{
	case OPT:
	  newpos =_nsp_plist_pretty_print_arg(List,indent,pos,posret);
	  newpos += Sciprintf(" = ");
	  newpos =_nsp_plist_pretty_print_arg(List->next,0,newpos,posret);
	  return newpos;
	  break;
	case EQUAL_OP:
	  newpos =_nsp_plist_pretty_print_arg(List,indent,pos,posret);
	  newpos += Sciprintf("=");
	  newpos =_nsp_plist_pretty_print_arg(List->next,0,newpos,newpos);
	  return newpos;
	  break;
	case MLHS  :
	  /* <lmhs> = ... **/
	  /* we do not display the left and right bracket 
	   *  if arity is one 
	   */
	  if ( L->arity > 1) 
	    newpos = pos +  Sciprintf1(indent,"[");
	  else
	    newpos = pos +  Sciprintf1(indent,"");
	  newpos = _nsp_plist_pretty_print_args(List,L->arity,0,newpos,newpos,",");
	  if ( L->arity > 1) 
	    newpos += Sciprintf("]");
	  return newpos;
	  break;
	case ARGS :
	  newpos = pos +  Sciprintf1(indent,"(");
	  newpos = _nsp_plist_pretty_print_args(List,L->arity,0,newpos,newpos,",");
	  newpos += Sciprintf(")");
	  return newpos;
	  break;
	case CELLARGS :
	  newpos = pos +  Sciprintf1(indent,"{");
	  newpos = _nsp_plist_pretty_print_args(List,L->arity,0,newpos,newpos,",");
	  newpos += Sciprintf("}");
	  return newpos;
	  break;
	case METARGS :
	  newpos = pos +  Sciprintf1(indent,"[");
	  newpos = _nsp_plist_pretty_print_args(List,L->arity,0,newpos,newpos,",");
	  newpos += Sciprintf("]");
	  return newpos;
	  break;
	case DOTARGS :
	  return pos+Sciprintf1(0,".%s",(char *) List->O);
	case CALLEVAL:
	case LISTEVAL :
	  if (0) Sciprintf1(indent,"LIST%d,%d,%d\n",indent,pos,posret) ;
	  newpos = pos +  Sciprintf1(indent,"");
	  newpos = _nsp_plist_pretty_print_args(List,L->arity,0,newpos,newpos,"");
	  return newpos;
	  break;
	case FEVAL :
	  if (0) Sciprintf1(indent,"FEVAL%d,%d,%d\n",indent,pos,posret) ;
	  newpos =_nsp_plist_pretty_print_arg(List,indent,pos,posret);
	  newpos += Sciprintf("(");
	  newpos = _nsp_plist_pretty_print_args(List->next,L->arity-1,0,newpos,newpos,",");
	  newpos += Sciprintf(")");
	  return newpos;
	  break;
	case PLIST :
	  if (L->next == NULLPLIST )
	    newpos=_nsp_plist_pretty_print_arg(L,indent,pos,posret);/* XXXXXXX */
	  else
	    newpos = 0;
	  return newpos;
	  break;
	case COMMENT:
	case NAME :
	case OPNAME :
	case NUMBER:
	case STRING:
	case INUMBER32 :
	case INUMBER64 :
	case UNUMBER32 :
	case UNUMBER64 :
	  return _nsp_plist_pretty_print_arg(L,indent,pos,posret);
	  break;
	case OBJECT: 
	  return Sciprintf1(indent,"{object}")+ pos;
	  /* ignore */
	  break;
	case EMPTYMAT:
	  return Sciprintf1(indent,"[]")+ pos;
	case EMPTYCELL:
	  return Sciprintf1(indent,"{}")+ pos;
	  break;
	case P_MATRIX :
	  newpos = pos + Sciprintf1(indent,"[");
	  newpos =_nsp_plist_pretty_print_arg(List,0,newpos,posret+1);
	  newpos += Sciprintf("]");
	  return newpos;
	  break;
	case P_CELL :
	  newpos = pos + Sciprintf1(indent,"{");
	  newpos =_nsp_plist_pretty_print_arg(List,0,newpos,posret+1);
	  newpos += Sciprintf("}");
	  return newpos;
	  break;
	case ROWCONCAT:
	case COLCONCAT:
	case DIAGCONCAT:
	  newpos =_nsp_plist_pretty_print_arg_ret(List,indent,pos,posret,&ret);
	  if ( newpos == 0) 
	    {
	      newpos =_nsp_plist_pretty_print_opname(L->type,posret,newpos);
	    }
	  else 
	    {
	      newpos =_nsp_plist_pretty_print_opname(L->type,0,newpos);
	    }
	  if ( newpos > CMAX )
	    {
	      if ( L->type == COLCONCAT ) Sciprintf("...");
	      Sciprintf("\n");
	      newpos =_nsp_plist_pretty_print_arg_ret(List->next,posret,0,posret,&ret);
	    }
	  else 
	    {
	      newpos =_nsp_plist_pretty_print_arg_ret(List->next,0,newpos,posret,&ret);
	    }
	  return newpos;
	  break;
	case CELLROWCONCAT:
	case CELLCOLCONCAT:
	case CELLDIAGCONCAT:
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      if ( j > 0 && newpos > CMAX  ) 
		{
		  Sciprintf("\n");
		  newpos =_nsp_plist_pretty_print_arg(List,posret,0,posret);
		}
	      else
		{
		  newpos =_nsp_plist_pretty_print_arg(List,0,newpos,posret);
		}
	      if ( j < L->arity-1)
		newpos =_nsp_plist_pretty_print_opname(L->type,0,newpos);
	      List = List->next;
	    }
	  return newpos;
	  break;
	case WHILE:
	  PRINTTAG("while");
	  newpos =_nsp_plist_pretty_print_arg(List,1,newpos,posret);
	  newpos += NSP_PRINTF1_COLOR(1,p_blue,"do\n");
	  newpos =_nsp_plist_pretty_print_arg(List->next,posret+2,0,posret+2);
	  newpos += NSP_PRINTF1_COLOR(posret,p_blue,"end");
	  return newpos;
	  break;
	case FUNCTION:
	  /* Sciprintf("function arity %d\n",L->arity); */
	  PRINTTAG("function");
	  _nsp_plist_pretty_print_arg(List,1,pos,newpos+1);
	  Sciprintf("\n");
	  newpos =_nsp_plist_pretty_print_arg(List->next,posret+2,pos,posret+2);
	  if ( newpos != 0)  Sciprintf("\n");
	  if ( L->arity == 3 ) 
	    {
	      newpos =_nsp_plist_pretty_print_arg(List->next->next,posret+2,pos,posret+2);
	    }
	  return NSP_PRINTF1_COLOR(posret,p_blue,"endfunction");
	  break;
	case FOR:
	  PRINTTAG("for");
	  newpos =_nsp_plist_pretty_print_arg(List,1,newpos,posret);
	  newpos += Sciprintf("=") ;
	  newpos =_nsp_plist_pretty_print_arg(List->next,0,newpos,newpos+1);
	  newpos += Sciprintf(" do\n");
	  newpos =_nsp_plist_pretty_print_arg(List->next->next,posret+2,0,posret+2);
	  if ( newpos != 0)  Sciprintf("\n");
	  return NSP_PRINTF1_COLOR(posret,p_blue,"end");
	  break;
	case IF :
	  /* a sequence of if elseif etc.... */
	  if (0) Sciprintf1(posret,"IF%d,%d,%d",indent,pos,posret) ;
	  PRINTTAG("if");
	  for ( j = 0 ; j < L->arity  ; j += 2 )
	    {
	      if ( j == L->arity-1 ) 
		{
		  /* we have reached the last else **/
		  if ( newpos != 0) Sciprintf("\n");
		  NSP_PRINTF1_COLOR(posret,p_blue,"else\n");
		  newpos =_nsp_plist_pretty_print_arg(List,posret+2,0,posret+2);
		}
	      else 
		{ 
		  if ( j != 0) 
		    {
		      if ( newpos != 0) Sciprintf("\n");
		      newpos = NSP_PRINTF1_COLOR(posret,p_blue,"elseif");
		    }
		  newpos =_nsp_plist_pretty_print_arg(List,1,newpos+1,newpos+1);
		  NSP_PRINTF1_COLOR(1,p_blue,"then\n");
		  List = List->next ;
		  newpos =_nsp_plist_pretty_print_arg(List,posret+2,0,posret+2);
		  List = List->next ;
		}
	    }
	  if ( newpos != 0) Sciprintf("\n");
	  newpos = NSP_PRINTF1_COLOR(posret,p_blue,"end");
	  return newpos;
	  break;
	case TRYCATCH :
	  /* XXXXX */
	  /* try catch sequence */
	  PRINTTAG("try");
	  newpos += Sciprintf1(1,"\n");
	  newpos =_nsp_plist_pretty_print_arg(List,posret+2,0,posret+2);
	  newpos += NSP_PRINTF1_COLOR(posret,p_blue,"catch");
	  List = List->next;
	  newpos += Sciprintf1(1,"\n");
	  newpos =_nsp_plist_pretty_print_arg(List,posret+2,0,posret+2);
	  if ( L->arity == 2 ) 
	    {
	      newpos += NSP_PRINTF1_COLOR(posret,p_blue,"end");
	    }
	  else 
	    {
	      List = List->next;
	      newpos += NSP_PRINTF1_COLOR(posret,p_blue,"finally");
	      newpos =_nsp_plist_pretty_print_arg(List,posret+2,0,posret+2);
	      newpos += NSP_PRINTF1_COLOR(posret,p_blue,"end");
	    }
	  return newpos;
	  break;
	case SELECT :
	  /* arity N. List argument is the test other arguments are 
	     the cases **/
	  if (0) Sciprintf1(indent,"slect{%d,%d,%d\n",indent,pos,posret) ;
	  PRINTTAG("select");
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      if ( j==0) 
		{
		  _nsp_plist_pretty_print_arg(List,1,newpos,posret);
		  Sciprintf("\n");
		  newpos = 0;
		}
	      else
		{
		  newpos=_nsp_plist_pretty_print_arg(List,posret+2,newpos,posret+2);
		}
	      List = List->next;
	    }
	  if ( newpos != 0) Sciprintf("\n");
	  newpos = NSP_PRINTF1_COLOR(posret,p_blue,"end");
	  return newpos;
	  break;
	case STATEMENTS :
	  if (0) Sciprintf1(indent,"st{%d,%d,%d\n",indent,pos,posret) ;
	  newpos = pos +  Sciprintf1(indent,"");
	  newpos= _nsp_plist_pretty_print_args(List,L->arity,0,newpos,posret,"");
	  if (0) Sciprintf1(indent,"}\n");
	  return newpos;
	  break;
	case STATEMENTS1 :
	  if (0) Sciprintf1(indent,"st1{%d,%d,%d\n",indent,pos,posret) ;
	  newpos = pos +  Sciprintf1(indent,"");
	  newpos= _nsp_plist_pretty_print_args(List,L->arity,0,newpos,posret,"");
	  if (0) Sciprintf1(indent,"}\n");
	  return newpos;
	  break;
	case PARENTH :
	  if (0) Sciprintf1(indent,"paren{%d,%d,%d\n",indent,pos,posret) ;
	  newpos = pos + Sciprintf1(indent,"(") ;
	  newpos = _nsp_plist_pretty_print_args(List,L->arity,0,newpos,newpos,",");
	  newpos += Sciprintf(")");
	  return newpos;
	  break;
	case CASE :
	  if (0) Sciprintf1(indent,"case{%d,%d,%d\n",indent,pos,posret) ;
	  if ( pos != 0) Sciprintf("\n");
	  newpos = NSP_PRINTF1_COLOR(posret,p_blue,"case") ;
	  newpos =_nsp_plist_pretty_print_arg(List,1,newpos,newpos+1);
	  newpos += NSP_PRINTF1_COLOR(1,p_blue,"then\n") ;
	  newpos =_nsp_plist_pretty_print_arg(List->next,posret+2,0,posret+2);
	  return newpos;
	  break;
	case LASTCASE :
	  if ( pos != 0) Sciprintf("\n");
	  NSP_PRINTF1_COLOR(posret,p_blue,"else\n") ;
	  newpos =_nsp_plist_pretty_print_arg(List,posret+2,0,posret+2);
	  return newpos;
	  break;
	case GLOBAL:
	  /* n-ary global */
	  PRINTTAG("global");Sciprintf(" ");
	  /* newpos = NSP_PRINTF1_COLOR(posret,p_blue,"global ") ; */
	  newpos = _nsp_plist_pretty_print_args(List,L->arity,0,newpos,newpos,",");
	  return newpos;
	  break;
	case CLEAR:
	  /* n-ary global */
	  PRINTTAG("clear");Sciprintf(" ");
	  newpos = _nsp_plist_pretty_print_args(List,L->arity,0,newpos,newpos,",");
	  return newpos;
	  break;
	case CLEARGLOBAL:
	  /* n-ary global */
	  PRINTTAG("clearglobal");Sciprintf(" ");
	  newpos = _nsp_plist_pretty_print_args(List,L->arity,0,newpos,newpos,",");
	  return newpos;
	  break;
	default:
	  Sciprintf("Warning in PlistPrettyPrint :");
	  s=nsp_astcode_to_name(L->type);
	  if ( s != (char *) 0) Sciprintf(" %s ",s);
	}
    }
  return 0;
}
 
static int _nsp_plist_pretty_print_opname(int type, int indent, int pos)
{
  Sciprintf1(indent,"");
  return pos+ nsp_print_opname(type) + indent;
}

/* a set of Args separated by sep */

/* #define WITH_SYMB_TABLE_DEBUG */

static int _nsp_plist_pretty_print_args(PList List, int Larity, int indent, int pos, int posret, char *sep)
{
  int j,  newpos=pos,indent1=indent;
  for ( j = 0 ; j < Larity ; j++)
    {
      newpos =_nsp_plist_pretty_print_arg(List,indent1,newpos,posret);
      if ( j != Larity -1 ) newpos += Sciprintf(sep);
      /* reset indent for next argument of necessary **/
      if ( indent1 != indent ) indent1 = indent; 
      /* newpos==0 if the previous ArgPrettyPrint has inserted a \n **/
      if ( newpos == 0) indent1 = posret;
      /* if we have remaining arguments and  line is too long we insert \n */
      if ( newpos > CMAX && j != Larity -1 ) 
	{
	  newpos=0; indent1=indent;Sciprintf("\n");
	}
      List = List->next;
    }
  return newpos;
}

/* One Arg Pretty print **/

static int _nsp_plist_pretty_print_arg(PList L, int i, int pos, int posret)
{
  int n;
  if ( L == NULLPLIST ) 
    {
      Scierror("Something Strange: nullplist ....\n");
      return 0 ; /*exit(1);*/
    }
  switch (L->type) 
    {
    case NAME :
#ifdef WITH_SYMB_TABLE_DEBUG
      return pos+Sciprintf1(i,"%s<%d>",(char *) L->O,L->arity);
#else 
      return pos+Sciprintf1(i,"%s",(char *) L->O);
#endif 
      break;
    case NUMBER:
      return pos+  Sciprintf1(i,"%s",((parse_double *) L->O)->str);
      break;
    case INUMBER32 :
    case INUMBER64 :
    case UNUMBER32 :
    case UNUMBER64 :
      return pos+  Sciprintf1(i,"%s",((parse_int *) L->O)->str);
      break;
    case OPNAME :
      return pos+Sciprintf1(i,"'%s'",(char *) L->O);
      break;
    case OBJECT :
#ifdef WITH_SYMB_TABLE_DEBUG      
      n = pos+Sciprintf1(i,"{object:Ox%x}\n",(unsigned int) L->O);
      nsp_object_print(L->O,0,0,0);
      return n;
#else 
      return pos;
#endif
      break;
    case STRING:
      /* return pos+Sciprintf1(i,"\"%s\"",(char *) L->O);*/
      n= pos + Sciprintf1(i,"");
      n+= strlen(((char *) L->O));
      nsp_print_string_as_read((char *) L->O);
      return n;
      break;
    case COMMENT:
      return pos+Sciprintf1(i,"//%s",(char *) L->O);
      break;
    case EMPTYMAT:
      return pos+Sciprintf1(i,"[]");break;
    case EMPTYCELL:
      return pos+Sciprintf1(i,"{}");break;
    case BREAK:
      return pos+NSP_PRINTF1_COLOR(i,p_blue,"break");break;
    case PLIST :
      return _nsp_plist_pretty_print((PList) L->O,i,pos,posret);
      break;
    case PRETURN: 
      return pos+NSP_PRINTF1_COLOR(i,p_blue,"return");
      break;
    case QUIT :
      return pos+NSP_PRINTF1_COLOR(i,p_blue,"quit");
      break;
    case NSP_EXIT :
      return pos+NSP_PRINTF1_COLOR(i,p_blue,"exit");
      break;
    case PAUSE :
      return pos+NSP_PRINTF1_COLOR(i,p_blue,"pause");
      break;
    case ABORT :
      return pos+NSP_PRINTF1_COLOR(i,p_blue,"abort");
      break;
    case CONTINUE :
      return pos+NSP_PRINTF1_COLOR(i,p_blue,"continue");
      break;
    case WHO :
      return pos+NSP_PRINTF1_COLOR(i,p_blue,"who");
      break;
    case WHAT :
      return pos+NSP_PRINTF1_COLOR(i,p_blue,"what");
      break;
    case CLEAR :
      return pos+NSP_PRINTF1_COLOR(i,p_blue,"clear");
      break;
    case CLEARGLOBAL :
      return pos+NSP_PRINTF1_COLOR(i,p_blue,"clearglobal");
      break;
    default:
      Scierror("Something Strange L->type ....\n");
      return 0;
    }
  return 0;
}

/* similar to _nsp_plist_pretty_print_arg_ 
 * but add a newline if ar is a comment 
 */

static int _nsp_plist_pretty_print_arg_ret(PList L, int i, int pos, int posret, int *ret)
{
  int newpos= _nsp_plist_pretty_print_arg(L,i,pos,posret);
  if ( L != NULLPLIST  &&  L->type == COMMENT )
    {
      Sciprintf("\n");
      newpos = 0;
      *ret = TRUE;
    }
  else 
    {
      *ret = FALSE;
    }
  return newpos; 
}

/**
 * nsp_plist_print:
 * @L: 
 * @indent: 
 * 
 * prints a #PList with fully parenthesized expressions.
 * Note that the given pointer can start not at the begining 
 * of the list @L in that case we walk back at the begining of 
 * the list for printing and the element given by @L can be 
 * highlighted if wanted. 
 * 
 **/

static void _nsp_plist_print(PList List, int indent);
static void _nsp_plist_print_arg(PList L, int i);

void nsp_plist_print(PList L, int indent)
{
  _nsp_plist_print(L,indent);
}


static void _nsp_plist_print(PList List, int indent)
{
  PList L=List;/* operator */
  PList ListInit = List ; 
  const char *s;
  int j;
  /* just in case L is not at the begining */
  while ( L->prev != NULL) L= L->prev;
  List = L->next;
  if ( L->type > 0 )
    {
      /* operators **/
      switch ( L->arity ) 
	{
	case 0:
	  /* : can be a 0 arity operator **/
	  nsp_print_opname(L->type);
	  break;
	case 1:
	  switch ( L->type ) 
	    {
	    case  COMMA_OP : 
	    case  SEMICOLON_OP  :
	      _nsp_plist_print_arg(List,indent);
	      nsp_print_opname(L->type);
	      break;
	    case QUOTE_OP : 
	      _nsp_plist_print_arg(List,indent);
	      nsp_print_opname(L->type);
	      break;
	    case TILDE_OP : 
	      nsp_print_opname(L->type);
	      Sciprintf("("); 
	      _nsp_plist_print_arg(List,indent);
	      Sciprintf(")");
	      break;
	    case RETURN_OP : 
	      _nsp_plist_print_arg(List,indent);
	      Sciprintf("\n");
	      break;
	    default:
	      Sciprintf("("); 
	      nsp_print_opname(L->type);
	      _nsp_plist_print_arg(List,indent);
	      Sciprintf(")");
	    }
	  break;
	case 2:
	  Sciprintf("(");
	  _nsp_plist_print_arg(List,indent);
	  nsp_print_opname(L->type);
	  _nsp_plist_print_arg(List->next,indent);
	  Sciprintf(")");
	  break;
	default :
	  Sciprintf("(");
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _nsp_plist_print_arg(List,indent);
	      if ( j < L->arity -1 ) 
		nsp_print_opname(L->type);
	      else 
		Sciprintf( ")");
	      List = List->next;
	    }
	  break;
	}
    }
  else 
    {
      switch ( L->type ) 
	{
	case EQUAL_OP:
	case OPT:
	  _nsp_plist_print_arg(List,indent);
	  Sciprintf("=");
	  _nsp_plist_print_arg(List->next,indent);
	  break;
	case MLHS  :
	  Sciprintf("[");
	  if ( L->arity == 0) Sciprintf("]");
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _nsp_plist_print_arg(List,indent);
	      Sciprintf((j == L->arity -1 ) ? "]" : ",");
	      List = List->next;
	    }
	  break;
	case ARGS :
	  Sciprintf("(");
	  if ( L->arity == 0) Sciprintf(")");
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _nsp_plist_print_arg(List,indent);
	      Sciprintf((j == L->arity -1 ) ? ")" : ",");
	      List = List->next;
	    }
	  break;
	case CELLARGS :
	  Sciprintf("{");
	  if ( L->arity == 0) Sciprintf("}");
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _nsp_plist_print_arg(List,indent);
	      Sciprintf((j == L->arity -1 ) ? "}" : ",");
	      List = List->next;
	    }
	  break;
	case METARGS :
	  Sciprintf("[");
	  if ( L->arity == 0) Sciprintf("]");
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _nsp_plist_print_arg(List,indent);
	      Sciprintf((j == L->arity -1 ) ? "]" : ",");
	      List = List->next;
	    }
	  break;
	case DOTARGS :
	  Sciprintf(".%s",(char *) List->O);
	  break;
	case CALLEVAL:
	case LISTEVAL :
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      if ( List == ListInit ) Sciprintf("-?->");
	      _nsp_plist_print_arg(List,indent);
	      List = List->next;
	    }
	  break;
	case FEVAL :
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _nsp_plist_print_arg(List,indent);
	      if ( 1== L->arity) Sciprintf("(");
	      Sciprintf(( j < L->arity ) ? ((j==0) ? "(" : ",") : ")");
	    }
	  break;
	case PLIST :
	  if (L->next == NULLPLIST )
	    _nsp_plist_print_arg(L,indent);/* XXXXXXX */
	  break;
	case COMMENT :
	case NAME :
	case OPNAME :
	case NUMBER:
	case STRING:
	case INUMBER32 :
	case INUMBER64 :
	case UNUMBER32 :
	case UNUMBER64 :
	  _nsp_plist_print_arg(L,indent);
	  break;
	case OBJECT :
	  break;
	case EMPTYMAT:
	  Sciprintf("[]");break;
	case EMPTYCELL:
	  Sciprintf("{}");break;
	case P_MATRIX :
	  Sciprintf("[");
	  _nsp_plist_print_arg(List,indent);
	  Sciprintf("]");
	  break;
	case P_CELL :
	  Sciprintf("{");
	  _nsp_plist_print_arg(List,indent);
	  Sciprintf("}");
	  break;
	case ROWCONCAT:
	case COLCONCAT:
	case DIAGCONCAT:
	  Sciprintf("[");
	  _nsp_plist_print_arg(List,indent);
	  nsp_print_opname(L->type);
	  _nsp_plist_print_arg(List->next,indent);
	  Sciprintf("]");
	  break;
	case CELLROWCONCAT:
	case CELLCOLCONCAT:
	case CELLDIAGCONCAT:
	  if ( L->arity > 1) Sciprintf("{");
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _nsp_plist_print_arg(List,indent);
	      if ( j < L->arity-1)
		nsp_print_opname(L->type);
	      List = List->next;
	    }
	  if ( L->arity > 1) Sciprintf("}");
	  break;
	case WHILE:
	  Sciprintf("while ") ;
	  _nsp_plist_print_arg(List,indent);
	  Sciprintf("do");
	  Sciprintf1(indent+2,"\n");
	  _nsp_plist_print_arg(List->next,indent);
	  Sciprintf1(indent,"\n");
	  Sciprintf("end");
	  break;
	case FUNCTION:
	  Sciprintf("function ") ;
	  _nsp_plist_print_arg(List,indent);
	  Sciprintf1(indent+2,"\n");
	  _nsp_plist_print_arg(List->next,indent);
	  Sciprintf1(indent,"\n");
	  Sciprintf("endfunction");
	  break;
	case FOR:
	  Sciprintf("for ") ;
	  _nsp_plist_print_arg(List,indent);
	  Sciprintf("= ") ;
	  _nsp_plist_print_arg(List->next,indent);
	  Sciprintf("do");
	  Sciprintf1(indent+2,"\n");
	  _nsp_plist_print_arg(List->next->next,indent);
	  Sciprintf("end");
	  break;
	case IF :
	  /* a sequence of if elseif etc.... */
	  Sciprintf("if ") ;
	  for ( j = 0 ; j < L->arity  ; j += 2 )
	    {
	      if ( j == L->arity-1 ) 
		{
		  /* we have reached the last else */
		  Sciprintf("else");Sciprintf1(indent+2,"\n");
		  _nsp_plist_print_arg(List,indent);
		}
	      else 
		{ 
		  if ( j != 0) 
		    {
		      Sciprintf("elseif");Sciprintf1(indent+2,"\n");
		    }
		  _nsp_plist_print_arg(List,indent);
		  Sciprintf("then");Sciprintf1(indent+2,"\n");
		  List = List->next ;
		  _nsp_plist_print_arg(List,indent);
		  List = List->next ;
		}
	    }
	  Sciprintf("end");
	  break;
	case TRYCATCH :
	  Sciprintf("try");Sciprintf1(indent+2,"\n");
	  _nsp_plist_print_arg(List,indent);
	  Sciprintf("catch");Sciprintf1(indent+2,"\n");
	  _nsp_plist_print_arg(List->next,indent);
	  if ( L->arity == 3 ) 
	    {
	      Sciprintf("finally");Sciprintf1(indent+2,"\n");
	      _nsp_plist_print_arg(List->next->next,indent);
	    }
	  Sciprintf("end");
	  break;
	case SELECT :
	  /* arity N. List argument is the test other arguments are 
	   * the cases 
	   */
	  Sciprintf("select ") ;
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _nsp_plist_print_arg(List,indent+2);
	      List = List->next;
	    }
	  Sciprintf("end");
	  break;
	case STATEMENTS :
	  Sciprintf("{") ;
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _nsp_plist_print_arg(List,indent);
	      List = List->next;
	    }
	  Sciprintf("}");
	  Sciprintf1(indent,"\n");
	  break;
	case STATEMENTS1 :
	  Sciprintf("$") ;
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _nsp_plist_print_arg(List,indent);
	      List = List->next;
	    }
	  Sciprintf("$");Sciprintf1(indent,"\n");
	  break;
	case PARENTH :
	  Sciprintf("(") ;
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _nsp_plist_print_arg(List,indent);
	      List = List->next;
	    }
	  Sciprintf(")");
	  break;
	case CASE :
	  Sciprintf("case ") ;
	  _nsp_plist_print_arg(List,indent);
	  Sciprintf("then ") ;
	  _nsp_plist_print_arg(List->next,indent);
	  break;
	case LASTCASE :
	  Sciprintf("else ") ;
	  _nsp_plist_print_arg(List,indent);
	  break;
	case GLOBAL:
	  /* n-ary global */
	  Sciprintf("global ");
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _nsp_plist_print_arg(List,indent);
	      Sciprintf((j == L->arity -1 ) ? "" : ",");
	      List = List->next;
	    }
	  break;
	case CLEAR:
	  /* n-ary global */
	  Sciprintf("clear ");
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _nsp_plist_print_arg(List,indent);
	      Sciprintf((j == L->arity -1 ) ? "" : ",");
	      List = List->next;
	    }
	  break;
	case CLEARGLOBAL:
	  /* n-ary global */
	  Sciprintf("clearglobal ");
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _nsp_plist_print_arg(List,indent);
	      Sciprintf((j == L->arity -1 ) ? "" : ",");
	      List = List->next;
	    }
	  break;
	default:
	  Sciprintf("Warning in PlistPrint :");
	  s=nsp_astcode_to_name(L->type);
	  if ( s != (char *) 0) Sciprintf(" %s ",s);
	}
    }
}

static void _nsp_plist_print_arg(PList L, int i)
{
  if ( L == NULLPLIST ) 
    {
      Scierror("Something Strange: nullplist ....\n");
      return ; /* exit(1); */
    }
  switch (L->type) 
    {
    case NAME :
      Sciprintf("%s",(char *) L->O);
#ifdef WITH_SYMB_TABLE_DEBUG
      if ( L->arity != -1) Sciprintf("<%d>",L->arity);
#endif 
      break;
    case NUMBER:
      Sciprintf("%s", ((parse_double *) L->O)->str);break;
    case INUMBER32 :
    case INUMBER64 :
    case UNUMBER32 :
    case UNUMBER64 :
      Sciprintf("%s", ((parse_int *) L->O)->str);break;
    case OPNAME :
      Sciprintf("'%s'",(char *) L->O);break;
    case STRING:
      Sciprintf("\"%s\"",(char *) L->O);
      break;
    case OBJECT: 
      break;
    case COMMENT:
      Sciprintf1(i,"//%s",(char *) L->O);
      break;
    case EMPTYMAT:
      Sciprintf("[]");break;
    case EMPTYCELL:
      Sciprintf("{}");break;
    case BREAK:
      Sciprintf("break");break;
    case PLIST :
      _nsp_plist_print((PList) L->O,i);
      break;
    case PRETURN: 
      Sciprintf("return");
      break;
    case QUIT :
      Sciprintf("quit");
      break;
    case NSP_EXIT :
      Sciprintf("exit");
      break;
    case PAUSE :
      Sciprintf("pause");
      break;
    case ABORT :
      Sciprintf("abort");
      break;
    case CONTINUE :
      Sciprintf("continue");
      break;
    default:
      Scierror("Something Strange L->type ....\n");
    }
}

/**
 * nsp_plist_info:
 * @L: 
 * @indent: 
 * 
 * prints information on a #PList. If @L is the #PList code of 
 * a function then only the calling sequence is shown. 
 * else the whole #PList is printed.
 * 
 **/

void nsp_plist_info(PList L, int indent)
{
  int j;
  for ( j=0 ; j < indent ; j++) Sciprintf(" ");
  if ( L->type == FUNCTION)
    {
      Sciprintf("function ") ;
      _nsp_plist_print_arg(L,indent);
      Sciprintf1(indent,"\n");
    }
  else 
    {
      _nsp_plist_print(L,indent);
    }
}

/**
 * nsp_parser_get_line:
 * @L: 
 * 
 * 
 * returns the line number contained in the first cell of @L.
 * note that when no line information is available we can walk 
 * at the beginning of @L to search.
 *
 * Return value: a line number or -1.
 **/

int nsp_parser_get_line(PList L)
{
  if ( L == NULLPLIST ) return -1;
  switch ( L->type ) 
    {
    case PLIST: return nsp_parser_get_line((PList) L->O);
    case COMMENT :
    case STRING:
    case NUMBER:
    case INUMBER32 :
    case INUMBER64 :
    case UNUMBER32 :
    case UNUMBER64 :
    case NAME :
    case OPNAME :
    case EMPTYMAT:
    case EMPTYCELL:
      while ( L->prev != NULL) 
	{
	  int rep;
	  L= L->prev;
	  if ((rep=nsp_parser_get_line(L)) != -1) return rep;
	}
      return nsp_parser_get_line(L);
    default:
      return NSP_POINTER_TO_INT(L->O);
    }
} 



/**
 * nsp_plist2smatrix:
 * @L: 
 * @indent: 
 * 
 * converts a #Plist to a string matrix using the #Plist 
 * print function.
 * 
 * Return value: a new #NspSMatrix or NULL
 **/

NspSMatrix *nsp_plist2smatrix(PList L, int indent) 
{
  NspSMatrix *res; 
  IOVFun def = SetScilabIO(Sciprint2string);
  nsp_plist_pretty_print(L,0);
  res = (NspSMatrix *) Sciprint2string_reset(); 
  SetScilabIO(def);
  return res;
}

/* get informations on in out number of arguments 
 * 
 */

static void plist_arg_get_nargs(PList L,int *lhs , int *rhsp1);

void plist_get_nargs(PList List,int *lhs , int *rhsp1)
{
  if ( List->type > 0 )
    {
      /* ignore */
    }
  else 
    {
      switch ( List->type ) 
	{
	case EQUAL_OP:
	case OPT:
	  plist_arg_get_nargs(List->next,lhs,rhsp1);
	  plist_arg_get_nargs(List->next->next,lhs,rhsp1);
	  break;
	case MLHS  :
	  *lhs = List->arity;
	  break;
	case FEVAL :
	  *rhsp1 = List->arity ;
	  break;
	case PLIST : 
	  plist_arg_get_nargs(List,lhs,rhsp1);
	  break;
	case FUNCTION:
	  plist_arg_get_nargs(List->next,lhs,rhsp1);
	  break;
	default:
	  /* ignore */
	  break;
	}
    }
}

static void plist_arg_get_nargs(PList L,int *lhs , int *rhsp1)
{
  if ( L == NULLPLIST ) return ;
  switch (L->type) 
    {
    case PLIST :
      plist_get_nargs((PList) L->O,lhs,rhsp1);
      break;
    default: 
      break;
    }
}


static void Arg_name_to_local_name(int rec,PList L,NspBHash *H);

/*
 * add informations on local objects.
 */

/**
 * plist_name_to_local_id:
 * @List: 
 * @H: 
 * @rec: 
 * 
 * walk through a #PList and tags symbols with their ids found in 
 * the hash table @H. This function is used to tag local variables
 * of a function. Note that when should not walk inside the 
 * functions defined in the function. 
 **/

void plist_name_to_local_id(PList List,NspBHash *H,int rec)
{
  PList L=List,L1;
  const char *s;
  int j;
  List = List->next;
  if ( L->type > 0 )
    {
      /* operators */
      switch ( L->arity ) 
	{
	case 0:
	  break;
	case 1:
	  switch ( L->type ) 
	    {
	    case  COMMA_OP : 
	    case  SEMICOLON_OP  :
	      Arg_name_to_local_name(rec,List,H);
	      break;
	    case QUOTE_OP : 
	      Arg_name_to_local_name(rec,List,H);
	      break;
	    case TILDE_OP : 
	      Arg_name_to_local_name(rec,List,H);
	      break;
	    case RETURN_OP : 
	      Arg_name_to_local_name(rec,List,H);
	      break;
	    default:
	      Arg_name_to_local_name(rec,List,H);
	    }
	  break;
	case 2:
	  Arg_name_to_local_name(rec,List,H);
	  Arg_name_to_local_name(rec,List->next,H);
	  break;
	default :
	  for ( j =  0 ; j < L->arity ; j++)
	    {
	      Arg_name_to_local_name(rec,List,H);
	      List = List->next;
	    }
	  break;
	}
    }
  else 
    {
      switch ( L->type ) 
	{
	case EQUAL_OP:
	case OPT:
	  Arg_name_to_local_name(rec,List,H);
	  Arg_name_to_local_name(rec,List->next,H);
	  break;
	case MLHS  :
	case ARGS :
	case CELLARGS :
	case METARGS :
	  for ( j =  0 ; j < L->arity ; j++)
	    {
	      Arg_name_to_local_name(rec,List,H);
	      List = List->next;
	    }
	  break;
	case DOTARGS :
	  Arg_name_to_local_name(rec,List,H);
	  break;
	case CALLEVAL:
	case LISTEVAL :
	case FEVAL :
	  for ( j =  0 ; j < L->arity ; j++)
	    {
	      Arg_name_to_local_name(rec,List,H);
	      List = List->next;
	    }
	  break;
	case PLIST :
	  if (L->next == NULLPLIST )
	    Arg_name_to_local_name(rec,L,H);/* XXXXXXX */
	  break;
	case COMMENT :
	case NAME :
	case OPNAME :
	case NUMBER:
	case STRING:
	case INUMBER32 :
	case INUMBER64 :
	case UNUMBER32 :
	case UNUMBER64 :
	  Arg_name_to_local_name(rec,L,H);
	  break;
	case OBJECT :
	  break;
	case EMPTYMAT:
	  break;
	case EMPTYCELL:
	  break;
	case P_MATRIX :
	case P_CELL :
	  Arg_name_to_local_name(rec,List,H);
	  break;
	case ROWCONCAT:
	case COLCONCAT:
	case DIAGCONCAT:
	  Arg_name_to_local_name(rec,List,H);
	  Arg_name_to_local_name(rec,List->next,H);
	  break;
	case CELLROWCONCAT:
	case CELLCOLCONCAT:
	case CELLDIAGCONCAT:
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      Arg_name_to_local_name(rec,List,H);
	      List = List->next;
	    }
	  break;
	case WHILE:
	  Arg_name_to_local_name(rec,List,H);
	  Arg_name_to_local_name(rec,List->next,H);
	  break;
	case FUNCTION:
	  /* the function prototype (= (ret-args) (feval (args))) 
	   * here we want to gather (ret-args) but also args 
	   */
	  /* this will gather ret-args */
	  if ( rec == 0 ) 
	    {
	      Arg_name_to_local_name(rec+1,List,H);
	      /* function call prototype */
	      L1 = ((PList) List->O)->next->next;
	      /* the function body i.e statements */
	      Arg_name_to_local_name(rec+1,List->next,H);
	    }
	  break;
	case FOR:
	  Arg_name_to_local_name(rec,List,H);
	  Arg_name_to_local_name(rec,List->next,H);
	  Arg_name_to_local_name(rec,List->next->next,H);
	  break;
	case IF :
	  for ( j = 0 ; j < L->arity  ; j += 2 )
	    {
	      if ( j == L->arity-1 ) 
		{
		  Arg_name_to_local_name(rec,List,H);
		}
	      else 
		{ 
		  Arg_name_to_local_name(rec,List,H);
		  List = List->next ;
		  Arg_name_to_local_name(rec,List,H);
		  List = List->next ;
		}
	    }
	  break;
	case TRYCATCH :
	  Arg_name_to_local_name(rec,List,H);
	  Arg_name_to_local_name(rec,List->next,H);
	  if ( L->arity == 3 ) 
	    {
	      Arg_name_to_local_name(rec,List->next->next,H);
	    }
	  break;
	case SELECT :
	case STATEMENTS :
	case STATEMENTS1 :
	case PARENTH :
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      Arg_name_to_local_name(rec,List,H);
	      List = List->next;
	    }
	  break;
	case CASE :
	  Arg_name_to_local_name(rec,List,H);
	  Arg_name_to_local_name(rec,List->next,H);
	  break;
	case LASTCASE :
	  Arg_name_to_local_name(rec,List,H);
	  break;
	case CLEAR:
	case GLOBAL:
	case CLEARGLOBAL:
	  for ( j =  0 ; j < L->arity ; j++)
	    {
	      Arg_name_to_local_name(rec,List,H);
	      List = List->next;
	    }
	  break;
	default:
	  s=nsp_astcode_to_name(L->type);
	}
    }
}


static void Arg_name_to_local_name(int rec,PList L,NspBHash *H)
{
  /*   NspObject *obj; */
  int val;
  if ( L == NULLPLIST ) 
    {
      Scierror("Something Strange: nullplist ....\n");
      return ; /* exit(1); */
    }
  switch (L->type) 
    {
    case NAME :
      /* Sciprintf("Je cherche %s\n",(char *) L->O); */
      if ( nsp_bhash_find(H,(char *) L->O,&val) == OK) 
	{
	  /* Sciprintf("OK pour %s\n",(char *) L->O); */
	  L->arity = val ; /*  (int) ((NspMatrix *) obj)->R[0]; */
	}
      break;
    case PLIST :
      plist_name_to_local_id((PList) L->O,H,rec);
      break;
    }
}


/* Search a symbol in a plist 
 *
 */

static void _plist_name(PList List,const char *name,int *res) ;


void plist_name_search(PList List,const char *name,int *res) 
{
  PList L=List,L1;
  const char *s;
  int j;
  List = List->next;
  if ( L->type > 0 )
    {
      /* operators **/
      switch ( L->arity ) 
	{
	case 0:
	  break;
	case 1:
	  switch ( L->type ) 
	    {
	    case  COMMA_OP : 
	    case  SEMICOLON_OP  :
	      _plist_name(List,name,res);
	      break;
	    case QUOTE_OP : 
	      _plist_name(List,name,res);
	      break;
	    case TILDE_OP : 
	      _plist_name(List,name,res);
	      break;
	    case RETURN_OP : 
	      _plist_name(List,name,res);
	      break;
	    default:
	      _plist_name(List,name,res);
	    }
	  break;
	case 2:
	  _plist_name(List,name,res);
	  _plist_name(List->next,name,res);
	  break;
	default :
	  for ( j =  0 ; j < L->arity ; j++)
	    {
	      _plist_name(List,name,res);
	      List = List->next;
	    }
	  break;
	}
    }
  else 
    {
      switch ( L->type ) 
	{
	case EQUAL_OP:
	case OPT:
	  _plist_name(List,name,res);
	  _plist_name(List->next,name,res);
	  break;
	case MLHS  :
	case ARGS :
	case CELLARGS :
	case METARGS :
	  for ( j =  0 ; j < L->arity ; j++)
	    {
	      _plist_name(List,name,res);
	      List = List->next;
	    }
	  break;
	case DOTARGS :
	  _plist_name(List,name,res);
	  break;
	case CALLEVAL:
	case LISTEVAL :
	case FEVAL :
	  for ( j =  0 ; j < L->arity ; j++)
	    {
	      _plist_name(List,name,res);
	      List = List->next;
	    }
	  break;
	case PLIST :
	  if (L->next == NULLPLIST )
	    _plist_name(L,name,res);/* XXXXXXX */
	  break;
	case COMMENT :
	case NAME :
	case OPNAME :
	case NUMBER:
	case STRING:
	case INUMBER32 :
	case INUMBER64 :
	case UNUMBER32 :
	case UNUMBER64 :
	  _plist_name(L,name,res);
	  break;
	case OBJECT :
	  break;
	case EMPTYMAT:
	  break;
	case EMPTYCELL:
	  break;
	case P_MATRIX :
	case P_CELL :
	  _plist_name(List,name,res);
	  break;
	case ROWCONCAT:
	case COLCONCAT:
	case DIAGCONCAT:
	  _plist_name(List,name,res);
	  _plist_name(List->next,name,res);
	  break;
	case CELLROWCONCAT:
	case CELLCOLCONCAT:
	case CELLDIAGCONCAT:
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _plist_name(List,name,res);
	      List = List->next;
	    }
	  break;
	case WHILE:
	  _plist_name(List,name,res);
	  _plist_name(List->next,name,res);
	  break;
	case FUNCTION:
	  /* the function prototype (= (ret-args) (feval (args))) 
	   * here we want to gather (ret-args) but also args 
	   */
	  /* this will gather ret-args */
	  _plist_name(List,name,res);
	  /* function call prototype */
	  L1 = ((PList) List->O)->next->next;
	  /* the function body i.e statements */
	  _plist_name(List->next,name,res);
	  break;
	case FOR:
	  _plist_name(List,name,res);
	  _plist_name(List->next,name,res);
	  _plist_name(List->next->next,name,res);
	  break;
	case IF :
	  for ( j = 0 ; j < L->arity  ; j += 2 )
	    {
	      if ( j == L->arity-1 ) 
		{
		  _plist_name(List,name,res);
		}
	      else 
		{ 
		  _plist_name(List,name,res);
		  List = List->next ;
		  _plist_name(List,name,res);
		  List = List->next ;
		}
	    }
	  break;
	case TRYCATCH :
	  _plist_name(List,name,res);
	  _plist_name(List->next,name,res);
	  if ( L->arity == 3 ) 
	    {
	      _plist_name(List->next->next,name,res);
	    }
	  break;
	case SELECT :
	case STATEMENTS :
	case STATEMENTS1 :
	case PARENTH :
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _plist_name(List,name,res);
	      List = List->next;
	    }
	  break;
	case CASE :
	  _plist_name(List,name,res);
	  _plist_name(List->next,name,res);
	  break;
	case LASTCASE :
	  _plist_name(List,name,res);
	  break;
	default:
	  s=nsp_astcode_to_name(L->type);
	}
    }
}


static void _plist_name(PList List,const char *name,int *res) 
{
  if ( List == NULLPLIST ) 
    {
      Scierror("Something Strange: nullplist ....\n");
      return ; /* exit(1); */
    }
  switch (List->type) 
    {
    case NAME :
      if ( strcmp(name,(char *) List->O) == 0 ) *res = TRUE;
      break;
    case PLIST :
      plist_name_search((PList) List->O,name,res);
      break;
    }
}

