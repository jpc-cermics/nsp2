/* Nsp
 * Copyright (C) 1998-2003 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
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
#include "astnode.h"
#include "nsp/interf.h"
#include "nsp/plistc.h"


static int ArgsPrettyPrint (PList L,int Larity,int indent,int pos,int posret, char *sep);

/*
 * Add a Scilab operator at start of list 
 * if list is empty a list is created 
 * XXXXX Add at the begining 
 */

int ParseAdd(PList *plist, int op, int arity, int line)
{
  PList loc = *plist,loc1;
  if ( (loc1 = EPListCreate()) == NULLPLIST )
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

/* add at end of list : used in copy */

int ParseAddLast(PList *plist, int op, int arity, int line)
{
  PList loc = *plist,loc1;
  if ( (loc1 = EPListCreate()) == NULLPLIST )
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

static int ParseAdd_name(PList *plist, char *str,int tag,int arity);

int ParseAddName(PList *plist, char *str)
{
  return  ParseAdd_name(plist,str,NAME,-1);
}

int ParseAddOPName(PList *plist, char *str)
{
  return  ParseAdd_name(plist,str,OPNAME,-1);
}

static int ParseAdd_name(PList *plist, char *str,int tag,int arity)
{
  PList loc = *plist,loc1;
  if ( (loc1 = EPListCreate()) == NULLPLIST ) return(FAIL);
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

/* 
 * Add an nsp object 
 * This is mainly used for functions 
 * to store the symbol table associated to the function. 
 */

int ParseAddObject(PList *plist, NspObject *obj )
{
  PList loc = *plist,loc1;
  if ( (loc1 = EPListCreate()) == NULLPLIST ) return(FAIL);
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


/*
 * Add a string at end of current list
 * if list is empty a list is created  
 */

int ParseAddString(PList *plist, char *str)
{
  PList loc = *plist,loc1;
  if ( (loc1 = EPListCreate()) == NULLPLIST ) return(FAIL);
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

/*
 * Add a comment at end of current list
 * if list is empty a list is created  
 */

int ParseAddComment(PList *plist, char *str)
{
  PList loc = *plist,loc1;
  if ( (loc1 = EPListCreate()) == NULLPLIST ) return(FAIL);
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


/*
 * Add a list l as last element of list plist 
 * plist=(a b c) l=(1 2 3) --> (a b c (1 2 3))
 */

int ParseAddList(PList *plist, PList *l)
{
  PList loc = *plist,loc1;
  if ( *l == NULLPLIST ) return(OK);
  if ( (*l)->next == NULLPLIST  ) 
    {
      loc1= *l;
    }
  else 
    {
      if ( (loc1 = EPListCreate()) == NULLPLIST )
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



/*
 * Append two lists 						     
 * plist=(a b c) l=(1 2 3) --> (a b c 1 2 3)
 */

int ParseAppend(PList *plist, PList *l)
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


/*
 * return plist = (l)    
 */

int ParseAddList1(PList *plist, PList *l)
{
  PList loc1;
  if ((loc1 = EPListCreate()) == NULLPLIST )  return(FAIL);
  loc1->type = PLIST ;
  loc1->O = *l;
  *plist = loc1;
  return(OK);
}


/*
 * Add a double at end of list 
 */

static parse_double *new_nsp_parsed_double(nsp_string str)
{
  parse_double *p;
  if (( p = malloc(sizeof(parse_double)))== NULL) return NULL;
  if (( p->str =new_nsp_string(str)) == NULLSTRING) return NULL;
  p->val = atof(str);
  return p;
}

int ParseAddDoubleI(PList *plist, char *str)
{
  PList loc = *plist,loc1;
  if ( (loc1 = EPListCreate()) == NULLPLIST ) return(FAIL);
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


/*
 * Res=EListCreate 
 * Creates a new one cell list 
 */

PList EPListCreate(void)
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

/*
 * Delete the NspList and all its elements 
 */

void PListDestroy(PList *List)
{
  PList loc = *List,loc1 ;
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
	  nsp_object_destroy((NspObject **) &loc->O);
	  break;
	case NUMBER :
	  FREE(((parse_double *)loc->O)->str);
 	  FREE(loc->O);
	  break;
	case PLIST: 
	  loc1= (PList) loc->O;
	  PListDestroy (&loc1);
	  break;
	}
      loc1= loc->next ;
      FREE( loc );
      loc = loc1;
    }
  *List = NULLPLIST;
} 

/*
 * Res=PListCopy(L)
 * returns in Res a copy of the PList L 
 * elements inside the list are copied too
 * XXXXXX Attention en cas de FAIL une 
 *    partie de L peut ne pas etre nettoyee 
 */

PList PListCopy(PList L)
{
  NspObject *obj;
  PList Res = NULLPLIST,loc=NULLPLIST,loc1=NULLPLIST;
  while ( L  != NULLPLIST ) 
    {
      switch ( L->type) 
	{
	case NAME :
	  if ( ParseAdd_name(&Res,(char*) L->O,NAME,L->arity)==FAIL) return(NULLPLIST);
	  break;
	case OPNAME :
	  if (ParseAddOPName(&Res,(char*) L->O)==FAIL) return(NULLPLIST);
	  break;
	case OBJECT :
	  if ((obj=nsp_object_copy(L->O)) == NULLOBJ) return(NULLPLIST); 
	  if (ParseAddObject(&Res,obj)==FAIL) return(NULLPLIST);
	  break;
	case STRING :
	  if (ParseAddString(&Res,(char*) L->O)==FAIL) return(NULLPLIST);
	  break;
	case COMMENT :
	  if (ParseAddComment(&Res,(char*) L->O)==FAIL) return(NULLPLIST);
	  break;
	case NUMBER :
	  if (ParseAddDoubleI(&Res,((parse_double *) L->O)->str)==FAIL) return(NULLPLIST);
	  break;
	case PLIST: 
	  if ((loc= PListCopy((PList) L->O)) == NULLPLIST) return(NULLPLIST);
	  if (ParseAddList1(&loc1,&loc) == FAIL) return (NULLPLIST);
	  if (ParseAddList(&Res,&loc1)== FAIL) return(NULLPLIST);
	  break;
	default: 
	  if ( ParseAddLast(&Res,L->type,L->arity,NSP_POINTER_TO_INT( L->O)) == FAIL) 
	    return(NULLPLIST);
	}
      L= L->next;
    }
  return(Res);
} 


/* converts a PList L to a list 
 * 
 */

static char * nsp_get_type_as_string(int type);

/* insert a language keywork 
 * add list(op,arity,line) at end of list 
 * or returns list(op,arity,line) if list is NULL.
 * should be renamed nsp_list_add_keyword 
 */

static int nsp_list_add(NspList **list, int op, int arity, int line)
{
#if 0 
  int_types T[]={string,s_int,s_int, t_end} ;
  NspObject *obj;
  char *sop= nsp_get_type_as_string(op);
  if ( sop == NULL) return FAIL;
  if (( obj = (NspObject *) BuildListFromArgs(T,sop,arity,line)) == NULLOBJ ) 
    return FAIL;
  if ( *list == NULL )
    {
      *list = (NspList *) obj;
      return OK;
    }
  return nsp_list_end_insert(*list,obj);
#else 
  int_types T[]={list_begin,obj,list_end, t_end} ;
  NspAstNode *astn;
  NspObject *Obj;
  if((astn=astnode_create("L", op, arity,line,NULL)) == NULLASTNODE ) 
    return FAIL;
  if ( *list == NULL )
    {
      if (( Obj = (NspObject *) BuildListFromArgs(T,astn)) == NULLOBJ ) 
	return FAIL;
      *list = (NspList *) Obj;
      return OK;
    }
  return nsp_list_end_insert(*list,NSP_OBJECT(astn));
#endif 
}

static int nsp_list_add_name_(NspList **list, char *str,char * tag,int arity)
{
  int_types T[]={list_begin,string,s_int,string,list_end, t_end} ;
  NspObject *obj;
  if (( obj = (NspObject *) BuildListFromArgs(T,tag,arity,str)) == NULLOBJ ) 
    return FAIL;
  if ( *list == NULL )
    {
      *list = (NspList *) obj;
      return OK;
    }
  return nsp_list_end_insert(*list,obj);
}

static int nsp_list_add_opname(NspList **list, char *str)
{
  return nsp_list_add_name_(list,str,"OPNAME",-1);
}



/*
 * Add a double at end of list 
 */

static int nsp_list_add_double(NspList **list, char *str)
{
  int_types T[]={list_begin,string,string,s_double,list_end, t_end} ;
  NspObject *obj;
  if (( obj = (NspObject *) BuildListFromArgs(T,"NUMBER",str,atof(str))) == NULLOBJ ) 
    return FAIL;
  if ( *list == NULL )
    {
      *list = (NspList *) obj;
      return OK;
    }
  return nsp_list_end_insert(*list,obj);
}

/* 
 * Add an nsp object 
 * This is mainly used for functions 
 * to store the symbol table associated to the function. 
 */

static int nsp_list_add_object(NspList **list, NspObject *object )
{
  int_types T[]={string,obj, t_end} ;
  NspObject *L;
  if ((L = (NspObject *) BuildListFromArgs(T,"OBJECT",object)) == NULLOBJ ) 
    return FAIL;
  if ( *list == NULL )
    {
      *list = (NspList *) L;
      return OK;
    }
  return nsp_list_end_insert(*list,L);
}

static int nsp_list_add_string(NspList **list, char *str)
{
  return  nsp_list_add_name_(list, str,"STRING",-1);
}

static int nsp_list_add_comments(NspList **list, char *str)
{
  return  nsp_list_add_name_(list, str,"COMMENT",-1);
}

/*
 * Add a list l as last element of list plist 
 * plist=(a b c) l=(1 2 3) --> (a b c (1 2 3))
 */

static int nsp_list_add_list(NspList **list,NspList *L)
{
  if ( *list == NULL )
    {
      *list = L;
      return OK;
    }
  return nsp_list_end_insert(*list,NSP_OBJECT(L));
}

/*
 * return plist = (l)    
 */

static int nsp_list_add_list1(NspList **list,NspList *L)
{
  if ((*list = nsp_list_create(NVOID))==NULL) return FAIL;
  return nsp_list_end_insert(*list,NSP_OBJECT(L));
}

static char * nsp_get_type_as_string(int type)
{
  if ( type > 0 && type  != '=' ) 
    {
      return OpCode2Str(type);
    }
  else
    {
      switch ( type ) 
	{
	case OPT : return "OPT";
	case '=': return "=";
	case MLHS  : return "MLHS";
	case FEVAL : return "FEVAL";
	case ARGS : return "ARGS";
	case METARGS : return "METARGS";
	case DOTARGS : return "DOTARGS";
	case CELLARGS : return "CELLARGS";
	case CALLEVAL : return "CALLEVAL";
	case LISTEVAL : return "LISTEVAL";
	case PLIST : return "PLIST";
	case COMMENT : return "COMMENT";
	case NAME : return "NAME";
	case OPNAME : return "OPNAME";
	case NUMBER: return "NUMBER";
	case STRING: return "STRING";
	case EMPTYMAT: return "EMPTYMAT";
	case EMPTYCELL: return "EMPTYCELL";
	case P_MATRIX : return "P_MATRIX";
	case P_CELL : return "P_CELL";
	case CELLDIAGCONCAT: return "CELLDIAGCONCAT";
	case CELLROWCONCAT: return "CELLROWCONCAT";
	case CELLCOLCONCAT: return "CELLCOLCONCAT";
	case ROWCONCAT: return "ROWCONCAT";
	case COLCONCAT: return "COLCONCAT";
	case DIAGCONCAT: return "DIAGCONCAT";
	case WHILE: return "WHILE";
	case FUNCTION: return "FUNCTION";
	case FOR: return "FOR";
	case IF : return "IF";
	case TRYCATCH : return "TRYCATCH";
	case SELECT : return "SELECT";
	case STATEMENTS : return "STATEMENTS";
	case STATEMENTS1 : return "STATEMENTS1";
	case PARENTH : return "PARENTH";
	case CASE : return "CASE";
	case LASTCASE : return "LASTCASE";
	case PAUSE:  return "PAUSE";
	case CLEAR:  return "CLEAR";
	case CLEARGLOBAL:  return "CLEARGLOBAL";
	case HELP  : return "HELP";
	case GLOBAL: return "GLOBAL";
	case EXEC: return "EXEC";
	case APROPOS: return "APROPOS";
	default :
	  return NULL;
	}
    }
}

NspList *nsp_plist_to_list(PList L)
{
  NspObject *obj;
  NspList *Res = NULL,*loc=NULL,*loc1=NULL;
  while ( L  != NULLPLIST ) 
    {
      switch ( L->type) 
	{
	case NAME :
	  if ( nsp_list_add_name_(&Res,(char*) L->O,"NAME",L->arity)==FAIL) return(NULLLIST);
	  break;
	case OPNAME :
	  if ( nsp_list_add_opname(&Res,(char*) L->O)==FAIL) return(NULLLIST);
	  break;
	case OBJECT :
	  if ((obj=nsp_object_copy(L->O)) == NULLOBJ) return(NULLLIST); 
	  if ( nsp_list_add_object(&Res,obj)==FAIL) return(NULLLIST);
	  break;
	case STRING :
	  if ( nsp_list_add_string(&Res,(char*) L->O)==FAIL) return(NULLLIST);
	  break;
	case COMMENT :
	  if ( nsp_list_add_comments(&Res,(char*) L->O)==FAIL) return(NULLLIST);
	  break;
	case NUMBER :
	  if ( nsp_list_add_double(&Res,((parse_double *) L->O)->str)==FAIL) return(NULLLIST);
	  break;
	case PLIST: 
	  if ((loc= nsp_plist_to_list((PList) L->O)) == NULLLIST) return(NULLLIST);
	  if ( nsp_list_add_list1(&loc1,loc) == FAIL) return (NULLLIST);
	  if ( nsp_list_add_list(&Res,loc1)== FAIL) return(NULLLIST);
	  break;
	default: 
	  if ( nsp_list_add(&Res,L->type,L->arity,NSP_POINTER_TO_INT( L->O)) == FAIL) 
	    return(NULLLIST);
	}
      L= L->next;
    }
  return(Res);
} 




/*
 * Returns the last element of a PList 
 */

PList Last(PList plist)
{
  if ( plist == NULLPLIST) return NULLPLIST;
  while (1) 
    {
      if ( plist->next == NULLPLIST ) return(plist);
      plist=plist->next ;
    }
}

/*
 *  Scilab Display of an Object of type PList 
 *  in internal mode i.e like in Lisp 
 *  PrInt <=> PrintInternal
 */

void PListPrInt_I(PList L, int indent)
{
  char *s;
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
	  PListPrInt_I((PList) L->O,indent+1);
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
	  if (  IsCodeKeyword(L->type)== OK) 
	    s= Keycode2str(L->type);
	  else s= OpCode2Str(L->type);
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


void PListPrInt(PList L)
{
  PListPrInt_I(L,4);
}


/*
 * Pretty Print of a PList 
 * (not fully parenthesized expressions )
 *  indent is the curent indentation to use 
 *  pos is the current position 
 *  posret is the new indentation to use if a \n is inserted 
 *  return value of the function : the new current position 
 */

#define CMAX 50

PList FirstEl(PList L)
{
  int k;
  PList First = L;
  for ( k=0; k < L->arity ; k++) 
    {
      if ((First = First->prev) == NULL) break;
    }
  if ( k != L->arity ) 
    {
      Scierror("Error:\tSomething Wrong in PListPrettyPrint\n");
      return 0;
    }
  return First;
}

#define PRINTTAG(tag)  if (pos != posret ) {			 \
    Sciprintf("\n");newpos = Sciprintf1(posret,tag) ;}		 \
  else { newpos = pos+ Sciprintf(tag) ; }			 \

static int PListPrettyPrint_I(PList List, int indent, int pos, int posret)
{
  PList L=List;
  char *s;
  int j,newpos=0;
  /* just in case L is not the first */
  while ( L->prev != NULL) L= L->prev;
  List = L->next;
  if ( L->type > 0 && L->type  != '=' ) 
    {
      /* operators **/
      switch ( L->arity ) 
	{
	case 0:
	  /* : can be a 0 arity operator **/
	  return PrettyPrintOPname(L->type,indent,pos);
	  break;
	case 1:
	  switch ( L->type ) 
	    {
	    case  ',' : 
	    case  ';'  :
	      newpos = ArgPrettyPrint(List,indent,pos,posret);
	      newpos = PrettyPrintOPname(L->type,0,newpos);
	      Sciprintf("\n");
	      return 0;
	      break;
	    case '\'' : 
	    case DOTPRIM:
	      newpos = ArgPrettyPrint(List,indent,pos,posret);
	      newpos = PrettyPrintOPname(L->type,0,newpos);
	      return  newpos;
	      break;
	    case '\n' : 
	      ArgPrettyPrint(List,indent,pos,posret);
	      Sciprintf("\n");
	      return 0;
	      break;
	    case '~' : 
	    default:
	      newpos = PrettyPrintOPname(L->type,indent,pos);
	      newpos = ArgPrettyPrint(List,0,newpos,posret);
	      return newpos;
	    }
	  break;
	case 2:
	  newpos = ArgPrettyPrint(List,indent,pos,posret);
	  newpos = PrettyPrintOPname(L->type,0,newpos);
	  if ( newpos > CMAX) 
	    {
	      Sciprintf("\n");
	      newpos = ArgPrettyPrint(List->next,posret,0,posret);
	    }
	  else
	    {
	      newpos = ArgPrettyPrint(List->next,0,newpos,posret);
	    }
	  return newpos;
	  break;
	default :
	  newpos = pos;
	  for ( j = 0 ; j <  L->arity ; j++ )
	    {
	      newpos = ArgPrettyPrint(List,(j == 0) ? indent : 1,
				      newpos,posret);
	      if ( j != L->arity -1 ) 
		newpos = PrettyPrintOPname(L->type,1,newpos);
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
	  newpos = ArgPrettyPrint(List,indent,pos,posret);
	  newpos += Sciprintf(" = ");
	  newpos = ArgPrettyPrint(List->next,0,newpos,posret);
	  return newpos;
	  break;
	case '=':
	  newpos = ArgPrettyPrint(List,indent,pos,posret);
	  newpos += Sciprintf("=");
	  newpos = ArgPrettyPrint(List->next,0,newpos,newpos);
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
	  newpos = ArgsPrettyPrint(List,L->arity,0,newpos,newpos,",");
	  if ( L->arity > 1) 
	    newpos += Sciprintf("]");
	  return newpos;
	  break;
	case ARGS :
	  newpos = pos +  Sciprintf1(indent,"(");
	  newpos = ArgsPrettyPrint(List,L->arity,0,newpos,newpos,",");
	  newpos += Sciprintf(")");
	  return newpos;
	  break;
	case CELLARGS :
	  newpos = pos +  Sciprintf1(indent,"{");
	  newpos = ArgsPrettyPrint(List,L->arity,0,newpos,newpos,",");
	  newpos += Sciprintf("}");
	  return newpos;
	  break;
	case METARGS :
	  newpos = pos +  Sciprintf1(indent,"[");
	  newpos = ArgsPrettyPrint(List,L->arity,0,newpos,newpos,",");
	  newpos += Sciprintf("]");
	  return newpos;
	  break;
	case DOTARGS :
	  return pos+Sciprintf1(0,".%s",(char *) List->O);
	case CALLEVAL:
	case LISTEVAL :
	  if (0) Sciprintf1(indent,"LIST%d,%d,%d\n",indent,pos,posret) ;
	  newpos = pos +  Sciprintf1(indent,"");
	  newpos = ArgsPrettyPrint(List,L->arity,0,newpos,newpos,"");
	  return newpos;
	  break;
	case FEVAL :
	  if (0) Sciprintf1(indent,"FEVAL%d,%d,%d\n",indent,pos,posret) ;
	  newpos = ArgPrettyPrint(List,indent,pos,posret);
	  newpos += Sciprintf("(");
	  newpos = ArgsPrettyPrint(List->next,L->arity-1,0,newpos,newpos,",");
	  newpos += Sciprintf(")");
	  return newpos;
	  break;
	case PLIST :
	  if (L->next == NULLPLIST )
	    newpos= ArgPrettyPrint(L,indent,pos,posret);/* XXXXXXX */
	  else
	    newpos = 0;
	  return newpos;
	  break;
	case COMMENT:
	case NAME :
	case OPNAME :
	case NUMBER:
	case STRING:
	  return ArgPrettyPrint(L,indent,pos,posret);
	  break;
	case OBJECT: 
	  /* ignore */
	  break;
	case EMPTYMAT:
	  return Sciprintf1(indent,"[]")+ pos;
	case EMPTYCELL:
	  return Sciprintf1(indent,"{}")+ pos;
	  break;
	case P_MATRIX :
	  newpos = pos + Sciprintf1(indent,"[");
	  newpos = ArgPrettyPrint(List,0,newpos,posret+1);
	  newpos += Sciprintf("]");
	  return newpos;
	  break;
	case P_CELL :
	  newpos = pos + Sciprintf1(indent,"{");
	  newpos = ArgPrettyPrint(List,0,newpos,posret+1);
	  newpos += Sciprintf("}");
	  return newpos;
	  break;
	case ROWCONCAT:
	case COLCONCAT:
	case DIAGCONCAT:
	  newpos = ArgPrettyPrint(List,indent,pos,posret);
	  newpos = PrettyPrintOPname(L->type,0,newpos);
	  if ( newpos > CMAX )
	    {
	      Sciprintf("\n");
	      newpos = ArgPrettyPrint(List->next,posret,0,posret);
	    }
	  else 
	    {
	      newpos = ArgPrettyPrint(List->next,0,newpos,posret);
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
		  newpos = ArgPrettyPrint(List,posret,0,posret);
		}
	      else
		{
		  newpos = ArgPrettyPrint(List,0,newpos,posret);
		}
	      if ( j < L->arity-1)
		newpos = PrettyPrintOPname(L->type,0,newpos);
	      List = List->next;
	    }
	  return newpos;
	  break;
	case WHILE:
	  PRINTTAG("while");
	  newpos = ArgPrettyPrint(List,1,newpos,posret);
	  newpos += Sciprintf1(1,"do\n");
	  newpos = ArgPrettyPrint(List->next,posret+2,0,posret+2);
	  newpos += Sciprintf1(posret,"end");
	  return newpos;
	  break;
	case FUNCTION:
	  PRINTTAG("function");
	  ArgPrettyPrint(List,1,pos,newpos+1);
	  Sciprintf("\n");
	  newpos = ArgPrettyPrint(List->next,posret+2,pos,posret+2);
	  if ( newpos != 0)  Sciprintf("\n");
	  return Sciprintf1(posret,"endfunction");
	  break;
	case FOR:
	  PRINTTAG("for");
	  newpos = ArgPrettyPrint(List,1,newpos,posret);
	  newpos += Sciprintf("=") ;
	  newpos = ArgPrettyPrint(List->next,0,newpos,newpos+1);
	  newpos += Sciprintf(" do\n");
	  newpos = ArgPrettyPrint(List->next->next,posret+2,0,posret+2);
	  if ( newpos != 0)  Sciprintf("\n");
	  return Sciprintf1(posret,"end");
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
		  Sciprintf1(posret,"else\n");
		  newpos = ArgPrettyPrint(List,posret+2,0,posret+2);
		}
	      else 
		{ 
		  if ( j != 0) 
		    {
		      if ( newpos != 0) Sciprintf("\n");
		      newpos = Sciprintf1(posret,"elseif");
		    }
		  newpos = ArgPrettyPrint(List,1,newpos+1,newpos+1);
		  Sciprintf1(1,"then\n");
		  List = List->next ;
		  newpos = ArgPrettyPrint(List,posret+2,0,posret+2);
		  List = List->next ;
		}
	    }
	  if ( newpos != 0) Sciprintf("\n");
	  newpos = Sciprintf1(posret,"end");
	  return newpos;
	  break;
	case TRYCATCH :
	  /* XXXXX */
	  /* try catch sequence */
	  PRINTTAG("try");
	  newpos += Sciprintf1(1,"\n");
	  newpos =ArgPrettyPrint(List,posret+2,0,posret+2);
	  newpos += Sciprintf1(posret,"catch");
	  List = List->next;
	  newpos += Sciprintf1(1,"\n");
	  newpos =ArgPrettyPrint(List,posret+2,0,posret+2);
	  if ( L->arity == 2 ) 
	    {
	      newpos += Sciprintf1(posret,"end");
	    }
	  else 
	    {
	      List = List->next;
	      newpos += Sciprintf1(posret,"finally");
	      newpos = ArgPrettyPrint(List,posret+2,0,posret+2);
	      newpos += Sciprintf1(posret,"end");
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
		  ArgPrettyPrint(List,1,newpos,posret);
		  Sciprintf("\n");
		  newpos = 0;
		}
	      else
		{
		  newpos= ArgPrettyPrint(List,posret+2,newpos,posret+2);
		}
	      List = List->next;
	    }
	  if ( newpos != 0) Sciprintf("\n");
	  newpos = Sciprintf1(posret,"end");
	  return newpos;
	  break;
	case STATEMENTS :
	  if (0) Sciprintf1(indent,"st{%d,%d,%d\n",indent,pos,posret) ;
	  newpos = pos +  Sciprintf1(indent,"");
	  newpos= ArgsPrettyPrint(List,L->arity,0,newpos,posret,"");
	  if (0) Sciprintf1(indent,"}\n");
	  return newpos;
	  break;
	case STATEMENTS1 :
	  if (0) Sciprintf1(indent,"st1{%d,%d,%d\n",indent,pos,posret) ;
	  newpos = pos +  Sciprintf1(indent,"");
	  newpos= ArgsPrettyPrint(List,L->arity,0,newpos,posret,"");
	  if (0) Sciprintf1(indent,"}\n");
	  return newpos;
	  break;
	case PARENTH :
	  if (0) Sciprintf1(indent,"paren{%d,%d,%d\n",indent,pos,posret) ;
	  newpos = pos + Sciprintf1(indent,"(") ;
	  newpos = ArgsPrettyPrint(List,L->arity,0,newpos,newpos,",");
	  newpos += Sciprintf(")");
	  return newpos;
	  break;
	case CASE :
	  if (0) Sciprintf1(indent,"case{%d,%d,%d\n",indent,pos,posret) ;
	  if ( pos != 0) Sciprintf("\n");
	  newpos = Sciprintf1(posret,"case") ;
	  newpos = ArgPrettyPrint(List,1,newpos,newpos+1);
	  newpos += Sciprintf1(1,"then\n") ;
	  newpos = ArgPrettyPrint(List->next,posret+2,0,posret+2);
	  return newpos;
	  break;
	case LASTCASE :
	  if ( pos != 0) Sciprintf("\n");
	  Sciprintf1(posret,"else\n") ;
	  newpos = ArgPrettyPrint(List,posret+2,0,posret+2);
	  return newpos;
	  break;
	default:
	  Sciprintf("Warning in PlistPrettyPrint :");
	  s=Keycode2str(L->type);
	  if ( s != (char *) 0) Sciprintf(" %s ",s);
	}
    }
  return 0;
}
 
int PrettyPrintOPname(int type, int indent, int pos)
{
  Sciprintf1(indent,"");
  return pos+ PrintOPname(type) + indent;
}

void PListPrettyPrint(PList L, int indent)
{
  PListPrettyPrint_I(L,indent,0,indent);
}


/* a set of Args separated by sep */

static int ArgsPrettyPrint(PList List, int Larity, int indent, int pos, int posret, char *sep)
{
  int j,  newpos=pos,indent1=indent;
  for ( j = 0 ; j < Larity ; j++)
    {
      newpos = ArgPrettyPrint(List,indent1,newpos,posret);
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

int ArgPrettyPrint(PList L, int i, int pos, int posret)
{
  if ( L == NULLPLIST ) 
    {
      Scierror("Something Strange: nullplist ....\n");
      return 0 ; /*exit(1);*/
    }
  switch (L->type) 
    {
    case NAME :
#ifdef WITH_SYMB_TABLE 
      return pos+Sciprintf1(i,"%s<%d>",(char *) L->O,L->arity);
#else 
      return pos+Sciprintf1(i,"%s",(char *) L->O);
#endif 
      break;
    case NUMBER:
      return pos+  Sciprintf1(i,"%s",((parse_double *) L->O)->str);
      break;
    case OPNAME :
      return pos+Sciprintf1(i,"'%s'",(char *) L->O);
      break;
    case OBJECT :
      break;
    case STRING:
      return pos+Sciprintf1(i,"\"%s\"",(char *) L->O);
      break;
    case COMMENT:
      return pos+Sciprintf1(i,"//%s",(char *) L->O);
      break;
    case EMPTYMAT:
      return pos+Sciprintf1(i,"[]");break;
    case EMPTYCELL:
      return pos+Sciprintf1(i,"{}");break;
    case BREAK:
      return pos+Sciprintf1(i,"break");break;
    case PLIST :
      return PListPrettyPrint_I((PList) L->O,i,pos,posret);
      break;
    case PRETURN: 
      return pos+Sciprintf1(i,"return");
      break;
    case QUIT :
      return pos+Sciprintf1(i,"quit");
      break;
    case NSP_EXIT :
      return pos+Sciprintf1(i,"exit");
      break;
    case PAUSE :
      return pos+Sciprintf1(i,"pause");
      break;
    case ABORT :
      return pos+Sciprintf1(i,"abort");
      break;
    case CONTINUE :
      return pos+Sciprintf1(i,"continue");
      break;
    case WHO :
      return pos+Sciprintf1(i,"who");
      break;
    case WHAT :
      return pos+Sciprintf1(i,"what");
      break;
    case CLEAR :
      return pos+Sciprintf1(i,"clear");
      break;
    case CLEARGLOBAL:
      return pos+Sciprintf1(i,"clearglobal");
      break;
    default:
      Scierror("Something Strange L->type ....\n");
      return 0;
    }
  return 0;
}



/*
 * Pretty Print of a PList 
 *        Really shows how expression where parsed 
 *        ( fully parenthesized expressions )
 */

static void PListPrint_I(PList List, int indent)
{
  PList L=List;/* operator */
  PList ListInit = List ; 
  char *s;
  int j;
  /* just in case L is not at the begining */
  while ( L->prev != NULL) L= L->prev;
  List = L->next;
  if ( L->type > 0 && L->type  != '=' ) 
    {
      /* operators **/
      switch ( L->arity ) 
	{
	case 0:
	  /* : can be a 0 arity operator **/
	  PrintOPname(L->type);
	  break;
	case 1:
	  switch ( L->type ) 
	    {
	    case  ',' : 
	    case  ';'  :
	      ArgPrint(List,indent);
	      PrintOPname(L->type);
	      break;
	    case '\'' : 
	      ArgPrint(List,indent);
	      PrintOPname(L->type);
	      break;
	    case '~' : 
	      PrintOPname(L->type);
	      Sciprintf("("); 
	      ArgPrint(List,indent);
	      Sciprintf(")");
	      break;
	    case '\n' : 
	      ArgPrint(List,indent);
	      Sciprintf("\n");
	      break;
	    default:
	      Sciprintf("("); 
	      PrintOPname(L->type);
	      ArgPrint(List,indent);
	      Sciprintf(")");
	    }
	  break;
	case 2:
	  Sciprintf("(");
	  ArgPrint(List,indent);
	  PrintOPname(L->type);
	  ArgPrint(List->next,indent);
	  Sciprintf(")");
	  break;
	default :
	  Sciprintf("(");
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      ArgPrint(List,indent);
	      if ( j < L->arity -1 ) 
		PrintOPname(L->type);
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
	case '=':
	case OPT:
	  /* PListPrInt(L); **/
	  ArgPrint(List,indent);
	  Sciprintf("=");
	  ArgPrint(List->next,indent);
	  break;
	case MLHS  :
	  Sciprintf("[");
	  if ( L->arity == 0) Sciprintf("]");
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      ArgPrint(List,indent);
	      Sciprintf((j == L->arity -1 ) ? "]" : ",");
	      List = List->next;
	    }
	  break;
	case ARGS :
	  Sciprintf("(");
	  if ( L->arity == 0) Sciprintf(")");
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      ArgPrint(List,indent);
	      Sciprintf((j == L->arity -1 ) ? ")" : ",");
	      List = List->next;
	    }
	  break;
	case CELLARGS :
	  Sciprintf("{");
	  if ( L->arity == 0) Sciprintf("}");
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      ArgPrint(List,indent);
	      Sciprintf((j == L->arity -1 ) ? "}" : ",");
	      List = List->next;
	    }
	  break;
	case METARGS :
	  Sciprintf("[");
	  if ( L->arity == 0) Sciprintf("]");
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      ArgPrint(List,indent);
	      Sciprintf((j == L->arity -1 ) ? "]" : ",");
	      List = List->next;
	    }
	  break;
	case DOTARGS :
	  Sciprintf(".");
	  ArgPrint(L->prev,indent);
	  break;
	case CALLEVAL:
	case LISTEVAL :
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      if ( List == ListInit ) Sciprintf("-?->");
	      ArgPrint(List,indent);
	      List = List->next;
	    }
	  break;
	case FEVAL :
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      ArgPrint(List,indent);
	      if ( 1== L->arity) Sciprintf("(");
	      Sciprintf(( j < L->arity ) ? ((j==0) ? "(" : ",") : ")");
	    }
	  break;
	case PLIST :
	  if (L->next == NULLPLIST )
	    ArgPrint(L,indent);/* XXXXXXX */
	  break;
	case COMMENT :
	case NAME :
	case OPNAME :
	case NUMBER:
	case STRING:
	  ArgPrint(L,indent);
	  break;
	case OBJECT :
	  break;
	case EMPTYMAT:
	  Sciprintf("[]");break;
	case EMPTYCELL:
	  Sciprintf("{}");break;
	case P_MATRIX :
	case P_CELL :
	  Sciprintf("[");
	  ArgPrint(List,indent);
	  Sciprintf("]");
	  break;
	case ROWCONCAT:
	case COLCONCAT:
	case DIAGCONCAT:
	case CELLROWCONCAT:
	case CELLCOLCONCAT:
	case CELLDIAGCONCAT:
	  Sciprintf("[");
	  ArgPrint(List,indent);
	  PrintOPname(L->type);
	  ArgPrint(List->next,indent);
	  Sciprintf("]");
	  break;
	case WHILE:
	  Sciprintf("while ") ;
	  ArgPrint(List,indent);
	  Sciprintf("do");
	  Sciprintf1(indent+2,"\n");
	  ArgPrint(List->next,indent);
	  Sciprintf1(indent,"\n");
	  Sciprintf("end");
	  break;
	case FUNCTION:
	  Sciprintf("function ") ;
	  ArgPrint(List,indent);
	  Sciprintf1(indent+2,"\n");
	  ArgPrint(List->next,indent);
	  Sciprintf1(indent,"\n");
	  Sciprintf("endfunction");
	  break;
	case FOR:
	  Sciprintf("for ") ;
	  ArgPrint(List,indent);
	  Sciprintf("= ") ;
	  ArgPrint(List->next,indent);
	  Sciprintf("do");
	  Sciprintf1(indent+2,"\n");
	  ArgPrint(List->next->next,indent);
	  Sciprintf("end");
	  break;
	case IF :
	  Sciprintf("if ") ;
	  ArgPrint(List,indent);
	  Sciprintf("then");Sciprintf1(indent+2,"\n");
	  ArgPrint(List->next,indent);
	  Sciprintf("else");Sciprintf1(indent+2,"\n");
	  ArgPrint(List->next->next,indent);
	  Sciprintf("end");
	  break;
	case TRYCATCH :
	  Sciprintf("try");Sciprintf1(indent+2,"\n");
	  ArgPrint(List,indent);
	  Sciprintf("catch");Sciprintf1(indent+2,"\n");
	  ArgPrint(List->next,indent);
	  if ( L->arity == 3 ) 
	    {
	      Sciprintf("finally");Sciprintf1(indent+2,"\n");
	      ArgPrint(List->next->next,indent);
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
	      ArgPrint(List,indent+2);
	      List = List->next;
	    }
	  Sciprintf("end");
	  break;
	case STATEMENTS :
	  Sciprintf("{") ;
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      ArgPrint(List,indent);
	      List = List->next;
	    }
	  Sciprintf("}");
	  Sciprintf1(indent,"\n");
	  break;
	case STATEMENTS1 :
	  Sciprintf("$") ;
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      ArgPrint(List,indent);
	      List = List->next;
	    }
	  Sciprintf("$");Sciprintf1(indent,"\n");
	  break;
	case PARENTH :
	  Sciprintf("(") ;
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      ArgPrint(List,indent);
	      List = List->next;
	    }
	  Sciprintf(":");Sciprintf1(indent,"\n");
	  break;
	case CASE :
	  Sciprintf("case ") ;
	  ArgPrint(List,indent);
	  Sciprintf("then ") ;
	  ArgPrint(List->next,indent);
	  break;
	case LASTCASE :
	  Sciprintf("else ") ;
	  ArgPrint(List,indent);
	  break;
	default:
	  Sciprintf("Warning in PlistPrint :");
	  s=Keycode2str(L->type);
	  if ( s != (char *) 0) Sciprintf(" %s ",s);
	}
    }
}

void PListPrint(PList L, int indent)
{
  PListPrint_I(L,indent);
}

void ArgPrint(PList L, int i)
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
      if ( L->arity != -1) Sciprintf("<%d>",L->arity);
      break;
    case NUMBER:
      Sciprintf("%s", ((parse_double *) L->O)->str);break;
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
      PListPrint_I((PList) L->O,i);
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


/*
 * Info on a PList 
 * If PList code a function : Only shows the calling sequence 
 * else call PrintPList on the whole PList 
 */

static void PListInfo_I(PList List, int indent)
{
  int j;
  for ( j=0 ; j < indent ; j++) Sciprintf(" ");
  if ( List->type == FUNCTION)
    {
      Sciprintf("function ") ;
      ArgPrint(List,indent);
      Sciprintf1(indent,"\n");
    }
  else 
    {
      PListPrint_I(List,indent);
    }
}

void PListInfo(PList L, int indent)
{
  PListInfo_I(L,indent);
}

/*
 * Show the Line number contained in a PList 
 */

int nsp_parser_get_line(PList L)
{
  if ( L == NULLPLIST ) return -1;
  switch ( L->type ) 
    {
    case COMMENT :
    case STRING:
    case NUMBER:
    case NAME :
    case OPNAME :
    case PLIST:
    case EMPTYMAT:
    case EMPTYCELL:
      return -1 ;
    default:
      return NSP_POINTER_TO_INT(L->O);
    }
} 

/*
 * Save a PList to a File 
 * XXXX Obsolete see NspObject xdr 
 */

#include <stdio.h>

static int PListSave_I(PList L, FILE *F)
{
  char *str;
  fprintf(F,"L");
  while ( L != NULLPLIST ) 
    {
      switch ( L->type ) 
	{
	case STRING:
	  fprintf(F,"S%d,%s",(int) strlen((char *) L->O),(char *) L->O);
	  break;
	case COMMENT:
	  fprintf(F,"C%d,%s",(int)strlen((char *) L->O),(char *) L->O);
	  break;
	case NUMBER:
	  str = ((parse_double *) L->O)->str;
	  fprintf(F,"D%d,%s",(int)strlen(str),str);
	  break;
	case NAME :
	  fprintf(F,"N%d,%s",(int)strlen((char *) L->O),(char *) L->O);
	  break;
	case OPNAME :
	  fprintf(F,"N%d,'%s'",(int) strlen((char *) L->O),(char *) L->O);
	  break;
	case PLIST:
	  PListSave_I( L->O,F);
	  break;
	case EMPTYMAT:
	case EMPTYCELL:
	  fprintf(F,"M%d,",NSP_POINTER_TO_INT(L->O));break;
	default:
	  fprintf(F,"O%d,%d,%d,",L->arity,L->type,NSP_POINTER_TO_INT(L->O));
	}
      L = L->next ;
    }
  fprintf(F,"E");
  return OK ;
}


int PListSave(PList L)
{
  FILE *F;
  F= fopen("pipo","w");
  if ( F == NULL) return FAIL;
  PListSave_I( L,F);
  fclose(F);
  return OK;
}



/*
 * XXXX Obsolete see NspObject xdr 
 */

#include <stdio.h>

static int PListLoad_I(PList *plist, FILE *F)
{
  int opar,op,d,oline;
  PList loc=NULLPLIST;
  PList loc1=NULLPLIST;
  char buf[256];
  int i=0;
  char c ;
  while ( 1) 
    {
      c= EOF;
      fscanf(F,"%c",&c);
      switch (c) 
	{
	case 'S' : break;
	  fscanf(F,"%d,",&d);
	  if ( d >= 256 ) return (FAIL);
	  for ( i = 0 ; i < d ; i++) 
	    buf[i] = getc(F);
	  buf[d]='\0';
	  if ( ParseAddString(plist,buf) == FAIL) return (FAIL);
	  break;
	case 'C' : break;
	  fscanf(F,"%d,",&d);
	  if ( d >= 256 ) return (FAIL);
	  for ( i = 0 ; i < d ; i++) 
	    buf[i] = getc(F);
	  buf[d]='\0';
	  if ( ParseAddComment(plist,buf) == FAIL) return (FAIL);
	  break;
	case 'D':
	  fscanf(F,"%d,",&d);
	  if ( d >= 256 ) return (FAIL);
	  for ( i = 0 ; i < d ; i++) 
	    buf[i] = getc(F);
	  buf[d]='\0';
	  if ( ParseAddDoubleI(plist,buf) == FAIL) return (FAIL);
	  break;
	case 'N':
	  fscanf(F,"%d,",&d);
	  if ( d >= 256 ) return (FAIL);
	  for ( i = 0 ; i < d ; i++) 
	    buf[i] = getc(F);
	  buf[d]='\0';
	  if ( ParseAddName(plist,buf) == FAIL) return (FAIL);
	  break;
	case 'L':
	  loc1 = loc = NULLPLIST;
	  if (PListLoad_I(&loc, F) == FAIL) return (FAIL);
	  if (ParseAddList1(&loc1,&loc) == FAIL) return (FAIL);
	  if (ParseAddList(plist,&loc1)== FAIL)  return (FAIL);
	  break;
	case 'E':
	  return(OK);
	  break;
	case 'M': 
	  fscanf(F,"%d,",&oline);
	  if ( ParseAdd(plist,EMPTYMAT,0,oline) == FAIL) return(FAIL);
	  break;
	case 'O':
	  fscanf(F,"%d,%d,%d,",&opar,&op,&oline);
	  if ( ParseAdd(plist,op,opar,oline) == FAIL) return(FAIL);
	  break;
	case EOF :
	  return OK;
	  break;
	default: 
	  Sciprintf("Something wrong in saved plist \n");
	  return FAIL;
	}
    }
  return OK ;
}


int PListLoad(PList *L)
{
  int rep;
  FILE *F;
  F= fopen("pipo","r");
  if ( F == NULL) return FAIL;
  *L =NULLPLIST; 
  rep = PListLoad_I(L,F);
  fclose(F);
  Sciprintf("After Reload -->\n");
  PListPrInt(*L);
  return rep;
}

/*
 * XXXX a retirer 
 */

NspSMatrix *PList2SMatrix(PList L, int indent) 
{
  NspSMatrix *res; 
  IOVFun def = SetScilabIO(Sciprint2string);
  PListPrettyPrint(L,0);
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
  if ( List->type > 0 && List->type  != '=' ) 
    {
      /* ignore */
    }
  else 
    {
      switch ( List->type ) 
	{
	case '=':
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
	  plist_arg_get_nargs(List,lhs,rhsp1);
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




/*
 * add informations on local objects.
 */

static void Arg_name_to_local_name(PList L,NspHash *H);

void plist_name_to_local_id(PList List,NspHash *H)
{
  PList L=List;
  char *s;
  int j;
  List = List->next;
  if ( L->type > 0 && L->type  != '=' ) 
    {
      /* operators **/
      switch ( L->arity ) 
	{
	case 0:
	  break;
	case 1:
	  switch ( L->type ) 
	    {
	    case  ',' : 
	    case  ';'  :
	      Arg_name_to_local_name(List,H);
	      break;
	    case '\'' : 
	      Arg_name_to_local_name(List,H);
	      break;
	    case '~' : 
	      Arg_name_to_local_name(List,H);
	      break;
	    case '\n' : 
	      Arg_name_to_local_name(List,H);
	      break;
	    default:
	      Arg_name_to_local_name(List,H);
	    }
	  break;
	case 2:
	  Arg_name_to_local_name(List,H);
	  Arg_name_to_local_name(List->next,H);
	  break;
	default :
	  for ( j =  0 ; j < L->arity ; j++)
	    {
	      Arg_name_to_local_name(List,H);
	      List = List->next;
	    }
	  break;
	}
    }
  else 
    {
      switch ( L->type ) 
	{
	case '=':
	case OPT:
	  Arg_name_to_local_name(List,H);
	  Arg_name_to_local_name(List->next,H);
	  break;
	case MLHS  :
	case ARGS :
	case CELLARGS :
	case METARGS :
	  for ( j =  0 ; j < L->arity ; j++)
	    {
	      Arg_name_to_local_name(List,H);
	      List = List->next;
	    }
	  break;
	case DOTARGS :
	  Arg_name_to_local_name(L->prev,H);
	  break;
	case CALLEVAL:
	case LISTEVAL :
	case FEVAL :
	  for ( j =  0 ; j < L->arity ; j++)
	    {
	      Arg_name_to_local_name(List,H);
	      List = List->next;
	    }
	  break;
	case PLIST :
	  if (L->next == NULLPLIST )
	    Arg_name_to_local_name(L,H);/* XXXXXXX */
	  break;
	case COMMENT :
	case NAME :
	case OPNAME :
	case NUMBER:
	case STRING:
	  Arg_name_to_local_name(L,H);
	  break;
	case OBJECT :
	  break;
	case EMPTYMAT:
	  break;
	case EMPTYCELL:
	  break;
	case P_MATRIX :
	case P_CELL :
	  Arg_name_to_local_name(List,H);
	  break;
	case ROWCONCAT:
	case COLCONCAT:
	case DIAGCONCAT:
	case CELLROWCONCAT:
	case CELLCOLCONCAT:
	case CELLDIAGCONCAT:
	  Arg_name_to_local_name(List,H);
	  Arg_name_to_local_name(List->next,H);
	  break;
	case WHILE:
	  Arg_name_to_local_name(List,H);
	  Arg_name_to_local_name(List->next,H);
	  break;
	case FUNCTION:
	  Arg_name_to_local_name(List,H);
	  Arg_name_to_local_name(List->next,H);
	  break;
	case FOR:
	  Arg_name_to_local_name(List,H);
	  Arg_name_to_local_name(List->next,H);
	  Arg_name_to_local_name(List->next->next,H);
	  break;
	case IF :
	  Arg_name_to_local_name(List,H);
	  Arg_name_to_local_name(List->next,H);
	  Arg_name_to_local_name(List->next->next,H);
	  break;
	case TRYCATCH :
	  Arg_name_to_local_name(List,H);
	  Arg_name_to_local_name(List->next,H);
	  if ( L->arity == 3 ) 
	    {
	      Arg_name_to_local_name(List->next->next,H);
	    }
	  break;
	case SELECT :
	case STATEMENTS :
	case STATEMENTS1 :
	case PARENTH :
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      Arg_name_to_local_name(List,H);
	      List = List->next;
	    }
	  break;
	case CASE :
	  Arg_name_to_local_name(List,H);
	  Arg_name_to_local_name(List->next,H);
	  break;
	case LASTCASE :
	  Arg_name_to_local_name(List,H);
	  break;
	default:
	  s=Keycode2str(L->type);
	}
    }
}


static void Arg_name_to_local_name(PList L,NspHash *H)
{
  NspObject *obj;
  if ( L == NULLPLIST ) 
    {
      Scierror("Something Strange: nullplist ....\n");
      return ; /* exit(1); */
    }
  switch (L->type) 
    {
    case NAME :
      if ( nsp_hash_find(H,(char *) L->O,&obj) == OK) 
	{
	  L->arity = (int) ((NspMatrix *) obj)->R[0];
	}
      break;
    case PLIST :
      plist_name_to_local_id((PList) L->O,H);
      break;
    }
}

