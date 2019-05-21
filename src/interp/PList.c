/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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

#include <nsp/nsp.h> 
#include <nsp/plist.h> 
#include <nsp/plistc.h> 
#include <nsp/bhash.h> 
#include <nsp/smatrix.h> 
#include <nsp/list.h> 
#include <nsp/file.h> 
#include <nsp/hobj.h> 
#include <nsp/function.h> 
#include <nsp/hash.h> 
#include <nsp/astnode.h> 
#include <nsp/ast.h> 
#include <nsp/interf.h>
#include <nsp/pr-output.h>
#include <nsp/frame.h>
#include <nsp/parse.h>

extern int nsp_parser_get_line(PList L);

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
  loc1->arity = 0; /* for names arity can be used as a work integer */
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
int nsp_parse_add_string(PList *plist, char *str, int tag )
{
  PList loc = *plist,loc1;
  if ( (loc1 =nsp_eplist_create()) == NULLPLIST ) return(FAIL);
  if (( loc1->O =new_nsp_string(str)) == NULLSTRING) return(FAIL);
  loc1->type = STRING;
  loc1->arity = tag; /* used to transmit the string delimiter */
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
  loc1->arity = 0; /* for names arity can be used as a work integer */
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
      loc1->arity= 0;
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
  loc1->arity= 0;
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
  loc1->arity= 0;
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
    case INUMBER64: p->Gint64 =0; STR2NUM(p->Gint64);break;
    case UNUMBER32: p->Guint32 =0; STR2NUM(p->Guint32);break;
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
  loc1->arity= 0;
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
	  if (nsp_parse_add_string(&Res_last,(char*) L->O, L->arity)==FAIL) return(NULLPLIST);
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
 * @name:
 * @list: 
 * @op: 
 * @arity: 
 * @data: 
 * 
 * creates a new astnode and insert it in a #NspList at 
 * the end if @list is non NULL or creates a new list if 
 * @list is %NULL.
 * 
 * Return value: %OK or %FAIL.
 **/

static int _nsp_list_add(const char *name,NspList **list, int op, int arity,void *data)
{
  int_types T[]={obj,t_end} ;
  NspAstNode *astn;
  NspObject *Obj;
  if((astn=astnode_create("L", op, arity,data,NULL)) == NULLASTNODE ) 
    return FAIL;
  if ( *list == NULL )
    {
      if (( Obj = (NspObject *) BuildListFromArgs(name,T,astn)) == NULLOBJ ) 
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
 * @name: a string pointer
 * @L: a #PList
 * 
 * converts a #PList to a nsp #NspList with #NspAstNode as elements. 
 * The #NspList can be used at nsp level.
 * 
 * Return value: a new #NspList or %NULLIST.
 **/

NspList *nsp_plist_to_list(const char *name,PList L)
{
  char *str;
  NspObject *obj;
  NspList *Res = NULL,*loc=NULL;
  while ( L  != NULLPLIST ) 
    {
      switch ( L->type) 
	{
	case OBJECT :
	  if ((obj=nsp_object_copy_with_name(L->O)) == NULLOBJ) return(NULLLIST); 
	  if ( _nsp_list_add(name,&Res,L->type,L->arity,obj)==FAIL) return(NULLLIST);
	  break;
	case NUMBER :
	  if ((str = nsp_string_copy(((parse_double *) L->O)->str)) ==NULL)
	    return NULLLIST;
	  if ( _nsp_list_add(name,&Res,L->type,L->arity,str) ==FAIL) 
	    return NULLLIST;
	  break;
	case INUMBER32 :
	case INUMBER64 :
	case UNUMBER32 :
	case UNUMBER64 :
	  if ((str = nsp_string_copy(((parse_int *) L->O)->str)) ==NULL)
	    return NULLLIST;
	  if ( _nsp_list_add(name,&Res,L->type,L->arity,str) ==FAIL) 
	    return NULLLIST;
	  break;
	case STRING:
	case COMMENT:
	case NAME :
	case OPNAME :
	  if ((str = nsp_string_copy((char *) L->O)) ==NULL)
	    return NULLLIST;
	  if ( _nsp_list_add(name,&Res,L->type,L->arity,str) ==FAIL) 
	    return NULLLIST;
	  break;
	case PLIST: 
	  if ((loc= nsp_plist_to_list("lel",(PList) L->O)) == NULLLIST) return(NULLLIST);
	  /* if ( _nsp_list_add_list1(&loc1,loc) == FAIL) return (NULLLIST); */
	  if ( _nsp_list_add_list(&Res,loc)== FAIL) return(NULLLIST);
	  break;
	default: 
	  if ( _nsp_list_add(name,&Res,L->type,L->arity,L->O)==FAIL) return(NULLLIST);
	}
      L= L->next;
    }
  return(Res);
} 


/**
 * nsp_plist_to_ast:
 * @L: 
 * 
 * converts a #PList to a nsp #NspAst which represents ast which can be 
 * manipulated at nsp level. 
 * 
 * Return value: a new #NspAst or %NULLAST.
 **/

static NspAst *nsp_plist_node_to_ast(const char *name,PList L);

NspAst *nsp_plist_to_ast(const char *name,PList L)
{
  PList L1=L;
  NspAst *ast;
  NspList *args;
  if ( L == NULL ) 
    {
      Scierror("Ast: internal error, nsp_plist_ast_args called with wrong arguments\n");
      return NULL;
    }
  /* first get args */
  L1 = L1->next;
  if ((args= nsp_list_create("args"))== NULLLIST) return NULLAST;
  while ( L1  != NULLPLIST ) 
    {
      /* arguments can be nodes or new asts */
      /* if ( L1->type != OBJECT )  */
	{
	  if ((ast = nsp_plist_node_to_ast("ast",L1)) == NULLAST)  return NULLAST;
	  if ( nsp_list_end_insert(args,NSP_OBJECT(ast))== FAIL) goto err;
	}
      L1 = L1->next;
    }
  /* then create an ast with tolevel object */
  if ((ast = nsp_plist_node_to_ast(name,L)) == NULLAST)  return NULLAST;
  if ( ast->args != NULL) 
    {
      nsp_list_destroy(ast->args);
    }
  ast->args = args;
  return ast;
 err:
  nsp_list_destroy(args);
  return NULLAST;
}

/**
 * nsp_plist_node_to_ast:
 * @name: a string 
 * @L: a #PList 
 * 
 * utility function for function nsp_plist_to_ast().
 * Note that the field L->O is only relevant for the case 
 * which are listed here. On the other cases it only codes a 
 * line number thus we do not put it in the nsp_ast_create call.
 * Note also that a FUNCTION node contains an OBJECT in the 
 * PList code and the object is inserted in the #NspAst. 
 * 
 * Returns: a new #NspAst 
 **/

static NspAst *nsp_plist_node_to_ast(const char *name,PList L)
{
  char *str;
  NspObject *obj;
  int line_number = nsp_parser_get_line(L);
  switch ( L->type) 
    {
    case OBJECT :
      if ((obj=nsp_object_copy_and_name("table",L->O)) == NULLOBJ) return NULLAST;
      return nsp_ast_create(name,L->type,L->arity,NULL,obj,NULL,NULL,line_number,NULL);
      break;
    case NUMBER :
      if ((str = nsp_string_copy(((parse_double *) L->O)->str)) ==NULL)
	return NULLAST;
      return nsp_ast_create(name,L->type,L->arity,str,NULL,NULL,NULL,line_number,NULL);
      break;
    case INUMBER32 :
    case INUMBER64 :
    case UNUMBER32 :
    case UNUMBER64 :
      if ((str = nsp_string_copy(((parse_int *) L->O)->str)) ==NULL)
	return NULLAST;
      return nsp_ast_create(name,L->type,L->arity,str,NULL,NULL,NULL,line_number,NULL);
      break;
    case STRING:
      /* L->arity for string is 0 or 1 to code for ' or " */
      if ((str = nsp_string_copy((char *) L->O)) ==NULL)
	return NULLAST;
      return nsp_ast_create(name,L->type,L->arity,str,NULL,NULL,NULL,line_number,NULL);
    case COMMENT:
    case NAME : /* Note that the L->arity field is an integer which stands for 
		 * the position of name in the table of a function */
    case OPNAME :
      /* arity of NAME can be different to zero we put 0 here */
      if ((str = nsp_string_copy((char *) L->O)) ==NULL)
	return NULLAST;
      return nsp_ast_create(name,L->type,0,str,NULL,NULL,NULL,line_number,NULL);
      break;
    case PLIST: 
      return nsp_plist_to_ast(name,(PList) L->O);
      break;
    case FUNCTION:
      return nsp_ast_create(name,L->type,L->arity,NULL,NULL,NULL,NULL,line_number,NULL);
    default: 
      /* we do not transmit L->O to nsp_ast_name since it only code 
       * a line number here
       */
      return nsp_ast_create(name,L->type,L->arity,NULL,NULL,NULL,NULL,line_number,NULL);
    }
  return NULL;
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
	  Sciprintf("[%s:%d:%d]",(char *) L->O,VAR_ID(L->arity),VAR_TAG(L->arity));
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

/**
 * nsp_plist_pretty_print:
 * @L: 
 * @indent: the curent indentation to use.
 * 
 * pretty printing of a #PList. 
 * we use function nsp_plist_generic_pretty_printer
 * which is common to #NspAst and #PList 
 * 
 **/

void nsp_plist_pretty_print(PList L, int indent, int color, int target,int space,int columns)
{
  /* int color=TRUE, target = 4 / * terminal * /, space=FALSE, columns=90; */
  /* just in case L is not at the begining */
  while ( L->prev != NULL) L= L->prev;
  nsp_plist_generic_pretty_printer(L, indent, color, target , space, columns);
}

/**
 * nsp_plist_print:
 * @L: 
 * @indent: 
 * 
 * This function is mainly used for error messages 
 * and could be replaced by nsp_plist_pretty_print
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
	    case  COMMA_RET_OP : 
	    case  SEMICOLON_RET_OP  :
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
#ifdef NSP_PARSE_MATRIX_AS_CELLS 
	  if ( L->arity > 1) Sciprintf("[");
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      _nsp_plist_print_arg(List,indent);
	      if ( j < L->arity-1)
		nsp_print_opname(L->type);
	      List = List->next;
	    }
	  if ( L->arity > 1) Sciprintf("]");
#else 
	  Sciprintf("[");
	  _nsp_plist_print_arg(List,indent);
	  nsp_print_opname(L->type);
	  _nsp_plist_print_arg(List->next,indent);
	  Sciprintf("]");
#endif
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
	      Sciprintf((j == L->arity -1 ) ? "" : ",");
	      List = List->next;
	    }
	  Sciprintf(")");
	  break;
	case CASE :
	  if ( List->type == COMMENT )
	    {
	      /* case can be a comment only */
	      _nsp_plist_print_arg(List,indent);
	    }
	  else
	    {
	      Sciprintf("case ") ;
	      _nsp_plist_print_arg(List,indent);
	      Sciprintf("then ") ;
	      _nsp_plist_print_arg(List->next,indent);
	    }
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
 * pretty printer function.
 * 
 * Return value: a new #NspSMatrix or NULL
 **/

NspSMatrix *nsp_plist2smatrix(PList L, int indent) 
{
  int color=FALSE, target = 4 /* terminal */, space=FALSE, columns=90;
  NspSMatrix *res; 
  IOVFun def = SetScilabIO(Sciprint2string);
  nsp_plist_pretty_print(L, indent, color, target, space, columns);
  res = (NspSMatrix *) Sciprint2string_reset(); 
  SetScilabIO(def);
  return res;
}

/* get informations on in out number of arguments 
 * 
 */

int nsp_plist_get_nargs(PList List,int *lhs , int *rhsp1, NspSMatrix *in, NspSMatrix *out)
{
  if ( List->type > 0 )
    {
      /* ignore */
    }
  else 
    {
      switch ( List->type ) 
	{
	case OPT:
	  /* nsp_plist_print_internal(List); */
	  List = List->next ;
	  if ( in != NULL) 
	    if ( nsp_row_smatrix_append_string(in,(char *) List->O)  == FAIL) return FAIL;
	  break;
	case EQUAL_OP:
	  List = List->next ;
	  if ( List != NULLPLIST && List->type == PLIST ) 
	    nsp_plist_get_nargs((PList) List->O,lhs,rhsp1,in,out);
	  List = List->next ;
	  if ( List != NULLPLIST && List->type == PLIST ) 
	    nsp_plist_get_nargs((PList) List->O,lhs,rhsp1,in,out);
	  break;
	case MLHS  :
	  *lhs = List->arity;
	  if ( out != NULL) 
	    while ( List->next != NULL ) 
	      {
		List = List->next ;
		if ( nsp_row_smatrix_append_string(out,(char *) List->O) == FAIL)
		  return FAIL;
		/* printf("name: %s\n",(char *) List->O); */
	      }
	  break;
	case FEVAL :
	  *rhsp1 = List->arity ;
	  List = List->next ; /* ignore the function name */
	  if ( in != NULL) 
	    while ( List->next != NULL ) 
	      {
		List = List->next ;
		if ( List->type == NAME )
		  {
		    if ( nsp_row_smatrix_append_string(in,(char *) List->O)  == FAIL) 
		      return FAIL;
		    /* printf("name: %s\n",(char *) List->O); */
		  }
		else
		  {
		    /* collect the optional arguments */
		    nsp_plist_get_nargs((PList) List->O,lhs,rhsp1,in,out);
		  }
	      }
	  break;
	case PLIST : 
	  nsp_plist_get_nargs((PList) List->O,lhs,rhsp1,in,out);
	  break;
	case FUNCTION:
	  List = List->next ;
	  if ( List != NULLPLIST && List->type == PLIST ) 
	    nsp_plist_get_nargs((PList) List->O,lhs,rhsp1,in,out);
	  break;
	default:
	  /* ignore */
	  break;
	}
    }
  return OK;
}

static void Arg_name_to_local_name(int rec,PList L,NspBHash *H);

/*
 * add informations on local objects.
 */

/**
 * nsp_plist_name_to_local_id:
 * @List: 
 * @H: 
 * @rec: 
 * 
 * walk through a #PList and tags symbols with their ids found in 
 * the hash table @H. This function is used to tag local variables
 * of a function and have a faster access to their values. 
 * Note that we should not walk inside the functions defined in the function. 
 **/

void nsp_plist_name_to_local_id(PList List,NspBHash *H,int rec)
{
  PList L=List; /* ,L1; */
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
	    case  COMMA_RET_OP : 
	    case  SEMICOLON_RET_OP  :
	      Arg_name_to_local_name(rec,List,H);
	      break;
	    case QUOTE_OP :
	    case DOTPRIM: 
	      Arg_name_to_local_name(rec,List,H);
	      break;
	    case RETURN_OP : 
	      Arg_name_to_local_name(rec,List,H);
	      break;
	    case TILDE_OP : 
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
	case OPT:
	case EQUAL_OP:
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
#ifdef NSP_PARSE_MATRIX_AS_CELLS 
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      Arg_name_to_local_name(rec,List,H);
	      List = List->next;
	    }
#else 
	  Arg_name_to_local_name(rec,List,H);
	  Arg_name_to_local_name(rec,List->next,H);
#endif 
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
	      /* L1 = ((PList) List->O)->next->next; */
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
	case GLOBAL:
	case CLEAR:
	case CLEARGLOBAL:
	  for ( j =  0 ; j < L->arity ; j++)
	    {
	      Arg_name_to_local_name(rec,List,H);
	      List = List->next;
	    }
	  break;
	case PAUSE:
	case HELP:
	case WHO:
	case EXEC:
	case APROPOS:
	case CD_COMMAND:
	case LS_COMMAND:
	case PWD_COMMAND:
	  break;
	default:
	  {
	    /* const char *s = nsp_astcode_to_name(L->type);*/
	  }
	}
    }
}

/* we store in the arity field of PList of type NAME 
 * information about the id of associated variable.
 * (If the variable is not local it's arity is -1).
 */


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
	  /* Sciprintf("found %s and set its arity value to %d\n",(char *) L->O,val); */
	  L->arity = val ; /*  (int) ((NspMatrix *) obj)->R[0]; */
	}
      break;

    case NUMBER:
    case INUMBER32 :
    case INUMBER64 :
    case UNUMBER32 :
    case UNUMBER64 :
    case OPNAME :
    case OBJECT :
    case STRING:
    case COMMENT:
    case EMPTYMAT:
    case EMPTYCELL:
    case BREAK:
      /* nothing to do */
      break;
    case PLIST :
      nsp_plist_name_to_local_id((PList) L->O,H,rec);
      break;
    case PRETURN: 
    case QUIT :
    case NSP_EXIT :
    case PAUSE :
    case ABORT :
    case CONTINUE :
    case WHO :
    case HELP:
    case CD_COMMAND :
    case LS_COMMAND :
    case PWD_COMMAND :
    case WHAT :
    case CLEAR :
    case CLEARGLOBAL :
      /* nothing to do */
      break;
    }
}


/**
 * nsp_plist_equal:
 * @L1: a #PList
 * @L2: a #PList
 * 
 * checks if the two #PList are identical.
 * 
 * Return value: %TRUE or %FALSE
 **/

int nsp_plist_equal(PList L1,PList L2)
{
  /* Sciprintf("->test equal\n");
  if ( L1 != NULLPLIST )     nsp_plist_print_internal(L1);
  if ( L2 != NULLPLIST )     nsp_plist_print_internal(L2);
  */ 

  while ( L1  != NULLPLIST ) 
    {
      /* Sciprintf("while step \n"); */
      if ( L2 == NULLPLIST ) 
	{
	  /* Sciprintf("L2 is null \n"); */
	  /* Sciprintf("<- test equal\n"); */
	  return FALSE;
	}
      if ( L1->type != L2->type ) 
	{
	   /* Sciprintf("type differ %s %s\n",nsp_astcode_to_name(L1->type),
		    nsp_astcode_to_name(L2->type)); */
	   /* Sciprintf("<- test equal\n"); */
	  return FALSE;
	}
      switch ( L1->type) 
	{
	case OBJECT :
	  /* Sciprintf("<- test equal\n"); */
	  return TRUE;
	  break;
	case NUMBER :
	  if (strcmp(((parse_double *) L1->O)->str,((parse_double *) L2->O)->str) != 0)
	    {
	      /* Sciprintf("number differ\n"); */
	      /* Sciprintf("<- test equal\n"); */
	      return FALSE;
	    }
	  break;
	case INUMBER32 :
	case INUMBER64 :
	case UNUMBER32 :
	case UNUMBER64 :
	  if ( strcmp(((parse_int *) L1->O)->str,((parse_int *) L2->O)->str) != 0)
	    {
	      /*  Sciprintf("number differ\n"); */
	      /*  Sciprintf("<- test equal\n"); */
	      return FALSE;
	    }
	  break;
	case STRING:
	case COMMENT:
	case NAME :
	case OPNAME :
	  if ( strcmp(((char *) L1->O),((char *) L2->O)) != 0)
	    {
	      /* Sciprintf("name/comments differ %s %s\n",((char *) L1->O),
			((char *) L2->O)); */
	      /*  Sciprintf("<- test equal\n"); */
	      return FALSE;
	    }
	  break;
	case PLIST: 
	  if (nsp_plist_equal((PList) L1->O, (PList) L2->O) == FALSE)
	    {
	      /*  Sciprintf("<- test equal\n"); */
	      return FALSE;
	    }
	  break;
	default: 
	  if ( L1->type != L2->type )
	    {
	      /*  Sciprintf("<- test equal\n"); */
	      return FALSE;
	    }
	}
      L1= L1->next;
      L2= L2->next;
    }
  /* if L2 is longer */
  /* Sciprintf("<- test equal\n"); */
  if ( L2 != NULLPLIST ) return FALSE;
  return TRUE;
} 

/* detect which local variables are persistent in 
 * a function définition.
 */

static void detect_arg_name(int rec,PList L,NspBHash *H, int action);

static int check_persistent_calleval(PList List,NspBHash *H, int action);


void nsp_plist_name_detect_persistent(PList List,NspBHash *H,int rec, int action)
{
  PList L=List; /* ,L1; */
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
	    case  COMMA_RET_OP : 
	    case  SEMICOLON_RET_OP  :
	      detect_arg_name(rec,List,H,action);
	      break;
	    case QUOTE_OP :
	    case DOTPRIM: 
	      detect_arg_name(rec,List,H,action);
	      break;
	    case RETURN_OP : 
	      detect_arg_name(rec,List,H,action);
	      break;
	    case TILDE_OP : 
	      detect_arg_name(rec,List,H,action);
	      break;
	    default:
	      detect_arg_name(rec,List,H,action);
	    }
	  break;
	case 2:
	  detect_arg_name(rec,List,H,action);
	  detect_arg_name(rec,List->next,H,action);
	  break;
	default :
	  for ( j =  0 ; j < L->arity ; j++)
	    {
	      detect_arg_name(rec,List,H,action);
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
	case EQUAL_OP:
	case MLHS  :
	case ARGS :
	case CELLARGS :
	case METARGS :
	case DOTARGS : break;
	case CALLEVAL:
	  if ( check_persistent_calleval(List,H, action) == OK ) 
	    {
	    }
	  else
	    {
	      for ( j =  0 ; j < L->arity ; j++)
		{
		  detect_arg_name(rec,List,H,action);
		  List = List->next;
		}
	    }
	  break;
	case LISTEVAL :
	case FEVAL :
	  for ( j =  0 ; j < L->arity ; j++)
	    {
	      detect_arg_name(rec,List,H,action);
	      List = List->next;
	    }
	  break;
	case PLIST :
	  if (L->next == NULLPLIST )
	    detect_arg_name(rec,L,H,action);/* XXXXXXX */
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
	case OBJECT :
	case EMPTYMAT:
	case EMPTYCELL:
	case P_MATRIX :
	case P_CELL :
	case ROWCONCAT:
	case COLCONCAT:
	case DIAGCONCAT:
	case CELLROWCONCAT:
	case CELLCOLCONCAT:
	case CELLDIAGCONCAT:
	  break;
	case WHILE:
	  detect_arg_name(rec,List,H,action);
	  detect_arg_name(rec,List->next,H,action);
	  break;
	case FUNCTION:
	  /* the function prototype (= (ret-args) (feval (args))) 
	   * here we want to gather (ret-args) but also args 
	   */
	  /* this will gather ret-args */
	  if ( rec == 0 ) 
	    {
	      detect_arg_name(rec+1,List,H,action);
	      /* function call prototype */
	      /* L1 = ((PList) List->O)->next->next; */
	      /* the function body i.e statements */
	      detect_arg_name(rec+1,List->next,H,action);
	    }
	  break;
	case FOR:
	  detect_arg_name(rec,List,H,action);
	  detect_arg_name(rec,List->next,H,action);
	  detect_arg_name(rec,List->next->next,H,action);
	  break;
	case IF :
	  for ( j = 0 ; j < L->arity  ; j += 2 )
	    {
	      if ( j == L->arity-1 ) 
		{
		  detect_arg_name(rec,List,H,action);
		}
	      else 
		{ 
		  detect_arg_name(rec,List,H,action);
		  List = List->next ;
		  detect_arg_name(rec,List,H,action);
		  List = List->next ;
		}
	    }
	  break;
	case TRYCATCH :
	  detect_arg_name(rec,List,H,action);
	  detect_arg_name(rec,List->next,H,action);
	  if ( L->arity == 3 ) 
	    {
	      detect_arg_name(rec,List->next->next,H,action);
	    }
	  break;
	case SELECT :
	case STATEMENTS :
	case STATEMENTS1 :
	case PARENTH :
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      detect_arg_name(rec,List,H,action);
	      List = List->next;
	    }
	  break;
	case CASE :
	  detect_arg_name(rec,List,H,action);
	  detect_arg_name(rec,List->next,H,action);
	  break;
	case LASTCASE :
	  detect_arg_name(rec,List,H,action);
	  break;
	case GLOBAL:
	case CLEAR:
	case CLEARGLOBAL:
	case PAUSE:
	case HELP:
	case WHO:
	case EXEC:
	case APROPOS:
	case CD_COMMAND:
	case LS_COMMAND:
	case PWD_COMMAND:
	  break;
	default:
	  {
	    /* const char *s = nsp_astcode_to_name(L->type);*/
	  }
	}
    }
}

static void detect_arg_name(int rec,PList L,NspBHash *H, int action )
{
  if ( L == NULLPLIST || L->type != PLIST )  return;
  nsp_plist_name_detect_persistent((PList) L->O,H,rec, action);
}

static int check_persistent_calleval(PList List,NspBHash *H,int action)
{
  char *name;
  PList L;
  int arity, j;
  if ( List == NULL || List->type != NAME || strcmp((char *) List->O,"persistent") != 0 )
    return FAIL;
  /* we are in persisten(....)*/
  List = List->next;
  if ( List == NULL || List->type != PLIST || ((PList) List->O)->type != ARGS )
    return FAIL;
  List = ((PList) List->O);
  arity = List->arity;
  for ( j =  0 ; j < arity ; j++)
    {
      int val;
      List = List->next;
      if ( List == NULL || List->type != PLIST || ((PList) List->O)->type != OPT )
	return FAIL;
      L= ((PList) List->O)->next;
      if ( L== NULL || L->type != NAME ) return FAIL;
      name = (char *) L->O;

      if ( action == TRUE )
	{
	  /* enter name in the hash table */
	  if ( nsp_bhash_find(H,name,&val) == FAIL) 
	    {
	      nsp_bhash_enter(H,name,0);
	    }
	}
      else
	{
	  if ( nsp_bhash_find(H,name,&val) == OK) 
	    {
	      /* tag the value of name as persistent variable */
	      int new_val = VAR_SET_PERSISTENT(val);
	      nsp_bhash_enter(H,name,new_val);
	    }
	}
    }
  return OK;
}

