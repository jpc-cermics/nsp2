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
 * Parsing of nsp syntax 
 *--------------------------------------------------------------------------*/

#include <nsp/nsp.h> 
#include <nsp/plist.h> 
#include <nsp/plistc.h> 
#include <nsp/bhash.h> 
#include <nsp/cells.h> 
#include <nsp/interf.h>
#include <nsp/parse.h>

typedef int (*ExprsStop) (Tokenizer *T,int token);

static int parse_exprs(Tokenizer *T,NspBHash *symb_table,PList *plist, int funcflag, ExprsStop F);
static int parse_endstop(Tokenizer *T,int token);
static int parse_stmt(Tokenizer *T,NspBHash *symb_table,PList *plist);
static int parse_funcstop (Tokenizer *T,int token);
static int parse_function(Tokenizer *T,NspBHash *symb_table,PList *plist);
static int parse_functionright(Tokenizer *T,NspBHash *symb_table,PList *plist);
static int parse_functionleft(Tokenizer *T,NspBHash *symb_table,PList *plist);
static int parse_while(Tokenizer *T,NspBHash *symb_table,PList *plist);
static int parse_bkey(Tokenizer *T,int key1, int key2, char *str, PList *plist);
static int parse_nblines(Tokenizer *T);
static int parse_nblines_and_comments(Tokenizer *T);
static int parse_stopif (Tokenizer *T,int token);
static int parse_if(Tokenizer *T,NspBHash *symb_table,PList *plist);
static int parse_stopselect (Tokenizer *T,int token);
static int parse_select(Tokenizer *T,NspBHash *symb_table,PList *plist);
static int parse_for(Tokenizer *T,NspBHash *symb_table,PList *plist);
static int parse_equal(Tokenizer *T,NspBHash *symb_table,PList *plist, int flag);
static int parse_expr(Tokenizer *T,NspBHash *symb_table,PList *plist, char inmatrix);
static int parse_lexpr(Tokenizer *T,NspBHash *symb_table,PList *plist,char inmatrix);
static int parse_lterm(Tokenizer *T,NspBHash *symb_table,PList *plist,char inmatrix);
static int parse_lprim(Tokenizer *T,NspBHash *symb_table,PList *plist,char inmatrix);
static int parse_lprim1(Tokenizer *T,NspBHash *symb_table,PList *plist,char inmatrix);
static int parse_terms(Tokenizer *T,NspBHash *symb_table,PList *plist,char inmatrix);
static int parse_terme1(Tokenizer *T,NspBHash *symb_table,PList *plist,char inmatrix);
static int IstermOp(Tokenizer *T,int *op1);
static int parse_terme(Tokenizer *T,NspBHash *symb_table,PList *plist);
static int parse_nary(Tokenizer *T,NspBHash *symb_table,PList *plist, 
		      int (*parsef)(Tokenizer *T,NspBHash *symb_table,PList *plist),
		      int (*opfn)(Tokenizer *T,int *op), char *info);
static int IsFact(Tokenizer *T,int *op1);
static int parse_fact(Tokenizer *T,NspBHash *symb_table,PList *plist);
static int parse_fact2(Tokenizer *T,NspBHash *symb_table,PList *plist);
static int parse_fact3(Tokenizer *T,NspBHash *symb_table,PList *plist);
static int IsComa(Tokenizer *T,int *op);
static int parse_exprset(Tokenizer *T,NspBHash *symb_table,PList *plist,int *count);
static int parse_extsymb(Tokenizer *T,NspBHash *symb_table,PList *plist, char *id, int flag, int *count, char end_char);
static int IsColMatOp(Tokenizer *T,int *op,char opt);
static int IsRowMatOp(Tokenizer *T,int *op,char opt);
static int IsDiagMatOp(Tokenizer *T,int *op,char opt);

#ifndef NSP_PARSE_MATRIX_AS_CELLS 
static int parse_rowmatrix(Tokenizer *T,NspBHash *symb_table,PList *plist,char stop);
static int parse_colmatrix(Tokenizer *T,NspBHash *symb_table,PList *plist,char stop);
static int parse_matrix(Tokenizer *T,NspBHash *symb_table,PList *plist,char stop);
#endif

static int parse_cells(Tokenizer *T,NspBHash *symb_table,PList *plist,char stop);
static int func_or_matrix_with_arg(Tokenizer *T,NspBHash *symb_table,PList *plist, char *id, int *excnt, int fblank, char end_char);
static int Check_Func_Def(Tokenizer *T,NspBHash *symb_table,PList plist);
static int Check_Func_Call(Tokenizer *T,PList plist, int tag);
static int parse_try_catch(Tokenizer *T,NspBHash *symb_table,PList *plist);
static int nsp_parse_add_to_symbol_table(NspBHash *symb_table,PList plist);
static int parse_name(Tokenizer *T,NspBHash *symb_table,PList *plist);
static int parse_nary_keyword(Tokenizer *T,NspBHash *symb_table,PList *plist,int keyword, int line);
static int parse_declaration(Tokenizer *T,NspBHash *symb_table,PList *plist,int flag);
static int parse_nary_opt(Tokenizer *T,NspBHash *symb_table,PList *plist, 
			  int (*parsef)(Tokenizer *T,NspBHash *symb_table,PList *plist,char c),
			  int (*opfn)(Tokenizer *T,int *op,char c), char *info,char opt);
static int parse_listeval(Tokenizer *T,NspBHash *symb_table,PList *plist);

static int parse_nary_flat_opt(Tokenizer *T,NspBHash *symb_table,PList *plist, 
			       int (*parsef)(Tokenizer *T,NspBHash *symb_table,PList *plist,char c),
			       int (*opfn)(Tokenizer *T,int *op,char c), char *info,char opt,int flag);

#ifdef  WITH_SYMB_TABLE 
static int nsp_parse_symbols_table_set_id(NspBHash *symb_table) ;
#endif 

/**
 * nsp_parse_top:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * Top Parsing function 
 * Parses <stmt> op <stmt> op <stmt>  .... lastop
 *     where op = , | ; 
 *           lastop = op \n 
 * <xxxx> we also accept / comments 
 * 
 * 
 * Returns: %OK or %FAIL
 **/


int nsp_parse_top(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  int count=0;
  PList plist1 = NULLPLIST ;

  if (debug) scidebug(debugI++,"[top>");
  while (1) 
    {
      plist1 = NULLPLIST ;
      if (parse_stmt(T,symb_table,&plist1) == FAIL ) return(FAIL);
      /* a comment will lead to an empty plist1 so we check that case 
       */
      if ( plist1 != NULLPLIST ) 
	{
	  count++;
	}
      else 
	{
	  return(OK);
	}
      if ( T->tokenv.id == COMMA_OP || T->tokenv.id == SEMICOLON_OP
	   || T->tokenv.id == COMMA_RET_OP || T->tokenv.id == SEMICOLON_RET_OP )
	
	{
	  int op= T->tokenv.id;
	  /* XXX maybe there's a comment after , ; NextToken will swallow it */
	  /* if ( T->tokenv.NextC == '/'  ) T->NextToken(T,); */
	  if ( T->tokenv.NextC == '\n' ) T->NextToken(T);
	  if (nsp_parse_add(&plist1,op,1,T->tokenv.Line) == FAIL) return(FAIL);
	  if (nsp_parse_add_list1(&plist1,&plist1)==FAIL) return(FAIL);
	  if (nsp_parse_add_list(plist,&plist1)==FAIL) return(FAIL);
	  if ( T->tokenv.NextC == '\0') 
	    {
	      if (nsp_parse_add(plist,STATEMENTS1,count,T->tokenv.Line)==FAIL) return(FAIL);
	      break;
	    }
	  else 
	    {
	      T->NextToken(T);
	    }
	}
      else if ( T->tokenv.id == RETURN_OP || T->tokenv.id == COMMENT )
	{
	  /* when following argument is a comment we insert a comma op instead of return op */
	  if (nsp_parse_add(&plist1,(T->tokenv.id == RETURN_OP) ? RETURN_OP: COMMA_OP,1,T->tokenv.Line) == FAIL) return(FAIL);
	  if (nsp_parse_add_list1(&plist1,&plist1)==FAIL) return(FAIL);
	  if (nsp_parse_add_list(plist,&plist1)==FAIL) return(FAIL);
	  if ( T->tokenv.id == COMMENT) 
	    {
	      if (nsp_parse_add_comment(plist,T->tokenv.buf)==FAIL) return(FAIL);
	      count++;
	    }
	  if (nsp_parse_add(plist,STATEMENTS1,count,T->tokenv.Line)==FAIL) return(FAIL);

	  break;
	}
      else if ( T->tokenv.id == 0) 
	{
	  /* Reached end of file : appears when parsing a function terminated with eof */
	  if (nsp_parse_add(&plist1,RETURN_OP,1,T->tokenv.Line) == FAIL) return(FAIL);
	  if (nsp_parse_add_list1(&plist1,&plist1)==FAIL) return(FAIL);
	  if (nsp_parse_add_list(plist,&plist1)==FAIL) return(FAIL);
	  if (nsp_parse_add(plist,STATEMENTS1,count,T->tokenv.Line)==FAIL) return(FAIL);
	  return(OK);
	}
      else 
	{
	  T->ParseError(T,"Parse Error: token `%s' found while expecting , or ; or \\n\n",
			T->code2name(T,T->tokenv.id));
	  return(FAIL);
	}
    }
  if (debug) scidebug(--debugI,"<top]");
  return(OK);
}


/**
 * parse_exprs:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @funcflag: 
 * @F: 
 * 
 * Parsing sequence of expressions <exprs>
 * <exprs> :=  <stmt> op <stmt> ....<stmt> op <stop>
 *     where opb = , | ; | \n | \n* |  and 
 *     op = opb+ ( one or more opb ) 
 *     one or more <stmt> 
 * <stop> := symbols which must end a parse_exprs 
 * This symbol is controlled by the function given as argument 
 * 
 * Returns: %OK or %FAIL
 **/

static int parse_exprs(Tokenizer *T,NspBHash *symb_table,PList *plist, int funcflag, ExprsStop F)
{
  int count = 0; /* ,op=0; */
  PList plist1 = NULLPLIST ;
  PList plist2 = NULLPLIST ;
  PList plist2_last = NULLPLIST;
  if (debug) scidebug(debugI++,"[exprs>");
  while (1) 
    {
      /* Parsing one or more operators + dealing with end-of file 
       * for backward compatibility 
       */ 
      while (1) 
	{
	  if  ( T->tokenv.id ==  '\0' )
	    {
	      if ( funcflag == 1) 
		{
		  /* Backward compatibility : */
		  if ( T->FuncForceNextChar(T) == FAIL) break;
		}
	      else 
		{
		  /* Eof detected  : */
		  if ( T->ForceNextChar(T) == FAIL) return(FAIL); 
		}
	      if ( T->NextToken(T) == FAIL) return(FAIL);
	    }
	  else 
	    {
	      if ( T->tokenv.id == COMMA_OP || T->tokenv.id == SEMICOLON_OP
		   || T->tokenv.id == COMMA_RET_OP || T->tokenv.id == SEMICOLON_RET_OP
		   || T->tokenv.id == RETURN_OP )
		{
		  if ( T->NextToken(T) == FAIL) return(FAIL);
		}
	      else 
		break;
	    }
	}
      if ( (*F)(T,T->tokenv.id) ) break;
      /* Backward compatibility continued  */
      if ( funcflag == 1 &&  T->tokenv.id == 0 ) break;
      plist1= NULLPLIST;
      if (parse_stmt(T,symb_table,&plist1) == FAIL ) return(FAIL);
      if (T->tokenv.id != COMMA_OP && T->tokenv.id != SEMICOLON_OP
	  && T->tokenv.id != COMMA_RET_OP && T->tokenv.id != SEMICOLON_RET_OP 
	  && T->tokenv.id != RETURN_OP && T->tokenv.id != COMMENT &&  ~(*F)(T,T->tokenv.id) ) 
        {
	  if ( plist1 != NULLPLIST) 
	    {
	      if (nsp_parse_add(&plist1,COMMA_OP,1,T->tokenv.Line) == FAIL) return(FAIL);
	      if (nsp_parse_add_list1(&plist1,&plist1)== FAIL) return(FAIL);
	      /* keep track of last to accelerate insertion */
	      if ( plist2 == NULL) 
		{
		  if (nsp_parse_add_list(&plist2,&plist1)== FAIL) return(FAIL);
		  plist2_last = plist2; 
		  while ( plist2_last->next != NULL) plist2_last = plist2_last->next;
		}
	      else 
		{
		  if (nsp_parse_add_list(&plist2_last,&plist1)== FAIL) return(FAIL);
		  while ( plist2_last->next != NULL) plist2_last = plist2_last->next;
		}
	      count++;
	    }
          break ;
        }
      else 
        {
	  /* op=1; */
	  if ( debug) Sciprintf("{%d}",T->tokenv.id);
	  if ( plist1 != NULLPLIST) 
	    {
	      if ( (*F)(T,T->tokenv.id)) 
		{
		  T->ParseError(T,"Parse Error: missing ',' or ';' or '\\n' before %s\n",
				T->code2name(T,T->tokenv.id));
		  return FAIL;
		}		  
	      switch ( T->tokenv.id ) 
		{
		case COMMENT :
		  /* a statement followed by a comment we convert to <stmt>; // comm  */
		  if (nsp_parse_add(&plist1,SEMICOLON_OP,1,T->tokenv.Line) == FAIL) return(FAIL);
		  break;
		default :
		  if (nsp_parse_add(&plist1,T->tokenv.id,1,T->tokenv.Line) == FAIL) return(FAIL);
		  break;
		} 
	      if (nsp_parse_add_list1(&plist1,&plist1)== FAIL) return(FAIL);
	      if ( plist2 == NULL) 
		{
		  if (nsp_parse_add_list(&plist2,&plist1)== FAIL) return(FAIL);
		  plist2_last = plist2; 
		  while ( plist2_last->next != NULL) plist2_last = plist2_last->next;
		}
	      else 
		{
		  if (nsp_parse_add_list(&plist2_last,&plist1)== FAIL) return(FAIL);
		  while ( plist2_last->next != NULL) plist2_last = plist2_last->next;
		}
	      count++; 
	      if ( T->tokenv.id == COMMENT  )
		{
		  plist1=NULL;
		  if (nsp_parse_add_comment(&plist1,T->tokenv.buf) == FAIL) return(FAIL);
		  if (nsp_parse_add(&plist1,RETURN_OP,1,T->tokenv.Line) == FAIL) return(FAIL);
		  if ( plist2 == NULL) 
		    {
		      if (nsp_parse_add_list(&plist2,&plist1)== FAIL) return(FAIL);
		      count++;
		      plist2_last = plist2; 
		      while ( plist2_last->next != NULL) plist2_last = plist2_last->next;
		    }
		  else 
		    {
		      if (nsp_parse_add_list(&plist2_last,&plist1)== FAIL) return(FAIL);
		      count++;
		      while ( plist2_last->next != NULL) plist2_last = plist2_last->next;
		    }
		}
	    }
	  if ( T->NextToken(T) == FAIL) return(FAIL);
	}
    }
  if ( count == 0) 
    {
      plist2=NULLPLIST;
      if (nsp_parse_add(&plist2,STATEMENTS,count,T->tokenv.Line)==FAIL) return(FAIL);
      if (nsp_parse_add_list1(&plist2,&plist2)== FAIL) return(FAIL);
    }
  else 
    {
      /* add at the start */
      if (nsp_parse_add(&plist2,STATEMENTS,count,T->tokenv.Line)==FAIL) return(FAIL);
    }
  if (nsp_parse_add_list(plist,&plist2)==FAIL)  return(FAIL);
  if (debug) scidebug(--debugI,"<exprs]");
  return(OK);
}

static int parse_endstop(Tokenizer *T,int token)
{
  return ( token == END );
}

/**
 * parse_stmt:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * 
 * Parse statement 
 * <stmt> = <equal> | <clause> | <command> | <comment> |
 *    <clause> := <if>|<while>|<for>|<select>|<function>
 *    <command>:= <keyword> | <keyword> <keywordarg> 
 *  <keywordarg> (See T->ParseCommandArg(T))
 * 
 * Returns: %OK or %FAIL
 **/

static int parse_stmt(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  int id,line;
  PList plist1 = NULLPLIST ;
  switch ( T->tokenv.id ) 
    {
    case IF : return(parse_if(T,symb_table,plist));
    case WHILE: return(parse_while(T,symb_table,plist));
    case FOR : return(parse_for(T,symb_table,plist));
    case SELECT : return(parse_select(T,symb_table,plist));
    case TRYCATCH : return parse_try_catch(T,symb_table,plist);
    case FUNCTION : return(parse_function(T,symb_table,plist));
    case ABORT :  /* a set of commands */
    case CONTINUE:
    case BREAK : 
    case QUIT :  
    case WHAT :  
    case PRETURN: 
      if (nsp_parse_add(&plist1,T->tokenv.id,0,T->tokenv.Line) == FAIL) return(FAIL);
      if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
      if ( T->NextToken(T) == FAIL) return(FAIL);
      return(OK);
    case GLOBAL:
      /* n-ary prefix n>=1 */
      return  parse_declaration(T,symb_table,plist,TRUE); 
    case CLEAR:  
    case CLEARGLOBAL:
      /* n-ary prefix n>=0 */
      return  parse_declaration(T,symb_table,plist,FALSE); 
    case EXEC :
    case APROPOS:
      /* a set of commands with one string argument or function call */
      if ( T->tokenv.NextC == '(') 
	{
	  /* switch to function call mode */
	  T->tokenv.id = NAME;
	  return(parse_equal(T,symb_table,plist,0));
	}
      id = T->tokenv.id; line = T->tokenv.Line; 
      if ( T->ParseCommandArg(T) == FAIL) return(FAIL);
      if ( T->tokenv.buf[0] != '\0') 
	{
	  /* with special arity 2 to keep track of strings with quotes */
	  if (nsp_parse_add_string(&plist1,T->tokenv.buf,2) == FAIL) return(FAIL);
	  if (nsp_parse_add(&plist1,id,1,line) == FAIL) return(FAIL);
	}
      else 
	{
	  T->ParseError(T,"Parse Error: %s must have one argument \n",T->code2name(T,T->tokenv.id));
	  return(FAIL);
	}
      if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL); 
      if ( T->NextToken(T) == FAIL) return(FAIL);
      return(OK);
    case PAUSE:  
    case HELP  : 
    case WHO :   
    case CD_COMMAND:
    case LS_COMMAND:
    case PWD_COMMAND:
    case NSP_EXIT:
      /* a set of commands with zero or one argument  */
      if ( T->tokenv.NextC == '(') 
	{
	  /* switch to function call */
	  T->tokenv.id = NAME;
	  return(parse_equal(T,symb_table,plist,0));
	}
      id = T->tokenv.id; line = T->tokenv.Line; 
      if ( T->ParseCommandArg(T) == FAIL) return(FAIL);
      if ( T->tokenv.buf[0] != '\0') 
	{
	  /* added with 2 which is used to keep track of string type (see ast-print.c) */
	  if (nsp_parse_add_string(&plist1,T->tokenv.buf,2) == FAIL) return(FAIL);
	  if (nsp_parse_add(&plist1,id,1,line) == FAIL) return(FAIL);
	}
      else 
	{
	  if (nsp_parse_add(&plist1,id,0,line) == FAIL) return(FAIL);
	}
      if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL); 
      if ( T->NextToken(T) == FAIL) return(FAIL);
      return(OK);
    case '/':
      /* xxx : a comment will result in an empty plist */
      if ( T->tokenv.NextC == '/' ) 
	{
#if 1
	  if ( T->ParseComment(T) == FAIL ) return(FAIL);
#else
	  char c;
	  c=T->GetChar(T);
	  while (c != '\n') c=T->GetChar(T);
#endif
	  if ( T->NextToken(T) == FAIL) return(FAIL);
	  return(OK);
	}
    default:
      return(parse_equal(T,symb_table,plist,0));
    }
}



/**
 * parse_declaration:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @flag: 
 * 
 * for keyword <name> op <name>  op1 
 * op = , | ' '
 * op1 = ; | '\n' 
 *
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_declaration(Tokenizer *T,NspBHash *symb_table,PList *plist,int flag)
{
  int id,line;
  PList plist1 = NULLPLIST ;
  /* a set of commands with n-names arguments or function call */
  if ( T->tokenv.NextC == '(') 
    {
      /* switch to function call mode */
      T->tokenv.id = NAME;
      return(parse_equal(T,symb_table,plist,0));
    }
  id = T->tokenv.id; line = T->tokenv.Line;
  if ( T->NextToken(T) == FAIL) return(FAIL);
  if (  T->tokenv.id == SEMICOLON_OP || T->tokenv.id == SEMICOLON_RET_OP|| T->tokenv.id == RETURN_OP )
    {
      if ( flag == TRUE ) 
	{
	  T->ParseError(T,"Parse Error: %s must have at least one argument \n",T->code2name(T,id));
	  return(FAIL);
	}
      else 
	{
	  /* we accept 0-arry operator */
	  if (nsp_parse_add(plist,id,0,line) == FAIL) return(FAIL);
	  return OK;
	}
    }
  if ( parse_nary_keyword(T,symb_table,&plist1,id,line)== FAIL) 
    return FAIL;
  if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL); 
  return OK;
}


/**
 * parse_nary_keyword:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @keyword: 
 * @line: 
 * 
 * 
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_nary_keyword(Tokenizer *T,NspBHash *symb_table,PList *plist,int keyword, int line)
{
  PList plist1 = NULLPLIST ;
  int excnt=1, iter=1;
  /* x1,....,x2]=.... */
  while (iter) 
    {
      plist1=NULLPLIST;
      /* parse_name only here */
      if (parse_name(T,symb_table,&plist1) == FAIL ) 
	{
	  T->ParseError(T,"Parse Error: waiting for a variable name\n");
	  return (FAIL);
	}
      /* if (parse_equal(T,symb_table,&plist1,0) == FAIL ) return (FAIL); */
      if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
      switch ( T->tokenv.id ) 
	{
	case COMMA_OP :
	case COMMA_RET_OP :
	  ++excnt;       
	  if ( T->NextToken(T) == FAIL) return(FAIL);
	  if ( parse_nblines(T) == FAIL ) return FAIL;
	  break;
	case NAME: 
	  ++excnt;
	  break;
	case SEMICOLON_OP:
	case SEMICOLON_RET_OP:
	case RETURN_OP:
	  iter=0;
	  break;
	default:  
	  T->ParseError(T,"Parse Error: waiting for `,', `;', or '\\n' \n");
	  return(FAIL);
	}
    }
  /* if ( T->NextToken(T) == FAIL) return(FAIL); */
  if (debug) Sciprintf("[names:%d)",excnt);
  /* excnt counts the arguments + the function name f(a,b)==> excnt=3*/
  if (nsp_parse_add(plist,keyword,excnt,line) == FAIL) return(FAIL);
  /* add symbols to symbol_table */
  if ( nsp_parse_add_to_symbol_table(symb_table,*plist) == FAIL) return FAIL;
  return(OK);
}

extern NspObject * int_bhash_get_keys(void *Hv, char *attr);

#ifdef  WITH_SYMB_TABLE 
#ifdef  SMAT_SYMB_TABLE
static void nsp_parse_symbols_table_reset_id(NspBHash *symb_table,NspSMatrix *S) ;
#endif
#endif 

/**
 * parse_funcstop:
 * @T: 
 * @token: 
 * 
 * detects end of function definition.
 * 
 * Returns: %TRUE or %FALSE
 **/

static int parse_funcstop (Tokenizer *T,int token)
{
  return ( token == ENDFUNCTION );
}

/**
 * parse_function:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * Parsing <function> 
 * 
 *   <function>:= function <fdec>
 *                    <exprs> 
 *                endfunction
 *
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_function(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  int val;
  NspObject *cell = NULLOBJ, *cell1= NULLOBJ;
  NspBHash *symbols = NULLBHASH;
  PList plist1 = NULLPLIST ;
  PList plist2 = NULLPLIST ;
  int func_line = T->tokenv.Line;
  if (debug) scidebug(debugI++,"[function>");
  /* 
   * create a table for local symbols 
   */
#ifdef  WITH_SYMB_TABLE 
  if ((symbols = nsp_bhcreate("st",10)) == NULLBHASH ) return FAIL;
#endif
  if ( T->NextToken(T) == FAIL) goto fail;
  if ( T->tokenv.id == '[' ) 
    {
      /* [a,b,c,....]= */ 
      if ( T->NextToken(T) == FAIL) goto fail;
      if (parse_functionleft(T,symbols,&plist1) == FAIL) 
	goto fail;
      if ( T->tokenv.id != EQUAL_OP ) 
	{
	  T->ParseError(T,"Parse Error: expecting a `=' in a function first line\n");
	  goto fail;
	}
      if ( T->NextToken(T) == FAIL) goto fail;
      if ( parse_nblines_and_comments(T) == FAIL) goto fail;
    }
  else if ( T->tokenv.id == NAME && T->tokenv.NextC == '=' )
    {
      /* a= */
      char id[NAME_MAXL];
      strncpy(id,T->tokenv.syn,NAME_MAXL);
      if ( T->NextToken(T) == FAIL) goto fail;
      if ( T->tokenv.id != EQUAL_OP ) 
	{
	  T->ParseError(T,"Parse Error: expecting a `=' in a function first line\n");
	  goto fail;
	}
      if ( T->NextToken(T) == FAIL) goto fail;
      if ( parse_nblines_and_comments(T) == FAIL ) goto fail;

#ifdef  WITH_SYMB_TABLE
      if ( nsp_bhash_find(symbols,id,&val) == FAIL) 
	{
	  if (nsp_bhash_enter(symbols,id,0) == FAIL) return FAIL;
	}
#endif 
      if (nsp_parse_add_name(&plist1,id) == FAIL) goto fail;
      if (nsp_parse_add(&plist1,MLHS,1,T->tokenv.Line) == FAIL) goto fail;
    }
  else if ( T->tokenv.id == NAME && T->tokenv.NextC == '(' )
    {
      /* no arguments on the left  */
      if (nsp_parse_add(&plist1,MLHS,0,T->tokenv.Line) == FAIL) goto fail;
    }
  else 
    {
      T->ParseError(T,"Parse Error: Wrong expression after keyword <<function>>\n");
      return FAIL;
    }
  T->tokenv.FlagEqu = 0;
  if (nsp_parse_add_list1(&plist1,&plist1) == FAIL) goto fail;

  /* right hand side of function definition */
  
  if (parse_functionright(T,symbols,&plist2) == FAIL) goto fail;
  if (nsp_parse_add_list(&plist1,&plist2) == FAIL) goto fail;
  if (nsp_parse_add(&plist1,EQUAL_OP,2,T->tokenv.Line) == FAIL) goto fail;

  if (nsp_parse_add_list1(&plist1,&plist1) == FAIL) goto fail;
  if (debug) scidebug(debugI++,"[func>"); 

  if ( parse_nblines(T) == FAIL ) return FAIL;
  plist2=NULLPLIST;
  if ( parse_exprs(T,symbols,&plist2,1,parse_funcstop) == FAIL) goto fail;
  if (nsp_parse_add_list(&plist1,&plist2) == FAIL) goto fail;
  /* For backward compatibility : we accepts that a function 
   * ends with end of file 
   */
  if ( T->tokenv.id != 0 ) if ( parse_nblines(T) == FAIL ) return FAIL;
  if ( T->tokenv.id == 0 || T->tokenv.id == ENDFUNCTION )
    {
#ifdef  WITH_SYMB_TABLE 
#ifdef  SMAT_SYMB_TABLE
      NspSMatrix *symb_names;
#endif
      int nsymb;
#endif
      if ( T->tokenv.id == ENDFUNCTION && T->NextToken(T) == FAIL) goto fail;
      
      if ((cell= (NspObject *)nsp_cells_create("symbols",3,1)) == NULLOBJ) goto fail;
      if (nsp_parse_add_object(&plist1,NSP_OBJECT(cell)) == FAIL) goto fail;
      if (nsp_parse_add(&plist1,FUNCTION,3,func_line) == FAIL) goto fail;
      
      /* add persistent variables declared by persistent(...) to the symbols table  */
      nsp_plist_name_detect_persistent(plist1,symbols,0, TRUE);
      
      /* gives id as int to each local variables */
#ifdef  WITH_SYMB_TABLE 
      nsymb=nsp_parse_symbols_table_set_id(symbols);
      /* get local variables names */
#ifdef  SMAT_SYMB_TABLE
      symb_names= (NspSMatrix *) int_bhash_get_keys(symbols,NULL);
      nsp_qsort_nsp_string(symb_names->S,NULL,FALSE,symb_names->mn,'i');
      nsp_parse_symbols_table_reset_id(symbols,symb_names);
#endif 
      /* we keep the hash table in the cell 
       * Note that this could be dropped if refs in calling stacks are removed in nsp.
       */
#ifdef  SMAT_SYMB_TABLE
      ((NspCells *) cell)->objs[0]= (NspObject *) symb_names;
#else 
      ((NspCells *) cell)->objs[0]= (NspObject *) symbols;
#endif 
      if ((cell1= (NspObject *) nsp_cells_create("locals",nsymb,1)) == NULLOBJ) goto fail;
      ((NspCells *) cell)->objs[1]= (NspObject *) cell1;
      if ((cell1= (NspObject *) nsp_cells_create("persistents",nsymb,1)) == NULLOBJ) goto fail;
      ((NspCells *) cell)->objs[2]= (NspObject *) cell1;

      /* second pass to tag the persistent variables in symbols 
       * XXXX TO BE DONE: this second pass should be avoided 
       * in the first pass we have to tag the variables when they are persistent 
       * and we have to change nsp_parse_symbols_table_set_id in such a way that it 
       * does not remove the tagging.
       */
      nsp_plist_name_detect_persistent(plist1,symbols,0, FALSE);
      /* use symbol table to walk in plist and convert names to local id*/
      nsp_plist_name_to_local_id(plist1,symbols,0); 
#ifdef  SMAT_SYMB_TABLE
      if (symbols != NULLBHASH)  nsp_bhash_destroy(symbols);
#endif
#else 
      if (nsp_parse_add(&plist1,FUNCTION,2,func_line) == FAIL) goto fail;
#endif 
      if (nsp_parse_add_list(plist,&plist1) == FAIL) goto fail;
      if (debug) scidebug(--debugI,"<endfunc]"); 
      /* now we can forget symbols */
      /* if (symbols != NULLBHASH)  nsp_bhash_destroy(symbols); */
      return (OK) ;
    }
  else
    {
      /* utiliser plist2 pour montrer ou on s'est arr'et'e 
       */
      T->ParseError(T,"Parse Error: Missing end of function \n");
      goto fail;
    }
 fail: 
  if (symbols != NULLBHASH)  nsp_bhash_destroy(symbols);
  if (cell != NULLOBJ ) nsp_object_destroy(&cell);

  return FAIL;
}


/**
 * parse_name:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * parse a name 
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_name(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  char id[NAME_MAXL];
  if (debug) scidebug(--debugI,"[name>");
  if (  T->tokenv.id == '\0' ) 
    {
      if ( T->ForceNextChar(T) == FAIL) return(FAIL);
      if ( T->NextToken(T) == FAIL) return(FAIL);
    }
  switch ( T->tokenv.id ) 
    {
    case NAME :
      strncpy(id,T->tokenv.syn,NAME_MAXL);
      if (nsp_parse_add_name(plist,id) == FAIL) return(FAIL);
      if ( T->NextToken(T) == FAIL) return(FAIL);
      return OK;
    default:
      return(FAIL);
    }
  if (debug) scidebug(--debugI,"<name]");
  return(OK);
}


/**
 * nsp_function_name:
 * @plist: a #PList
 * 
 * 
 * returns the function name in a function PLIST 
 * but no check is done we expect that @plist has 
 * the correct shape.
 * 
 * Returns:  a string 
 **/

char *nsp_function_name(PList plist)
{
  return ((PList) (((PList) plist->next->O)->next->next->O))->next->O;
}


/**
 * parse_functionright:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * 
 * Parses the right  side of [...]=f(...) i.e f(..) or f 
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_functionright(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  PList plist1= NULLPLIST;
  char id[NAME_MAXL];
  if ( T->tokenv.id != NAME ) 
    {
      T->ParseError(T,"Parse Error: Unexpected token here %s. Expecting a name\n",T->code2name(T,T->tokenv.id));
      return(FAIL);
    }
  /*  ************    symb(arg1,....,argn) */
  strncpy(id,T->tokenv.syn,NAME_MAXL);
  if ( T->NextToken(T) == FAIL) return(FAIL);
  if (T->tokenv.id == '(') 
    {
      int excnt = 1;
      if (nsp_parse_add_name(&plist1,id) == FAIL) return(FAIL);
      if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
      if ( func_or_matrix_with_arg(T,symb_table,plist,id,&excnt,0,')')== FAIL) return(FAIL);
      if ( nsp_parse_add(plist,FEVAL,excnt,T->tokenv.Line) == FAIL) return(FAIL);
      if ( Check_Func_Def(T,symb_table,*plist) == FAIL) return(FAIL);
    }
  else 
    {
      if (nsp_parse_add_name(&plist1,id) == FAIL) return(FAIL);
      if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
      if (nsp_parse_add(plist,FEVAL,1,T->tokenv.Line) == FAIL) return(FAIL);
    }
  return(OK);
}


/**
 * parse_functionleft:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * Parses the left side of [...]=f()
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_functionleft(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  PList plist1 = NULLPLIST ;
  int excnt=1;
  /* x1,....,x2]=.... */
  while ( T->tokenv.id != ']' ) 
    {
      plist1=NULLPLIST;
      /* parse_name only here */
      if (parse_name(T,symb_table,&plist1) == FAIL ) return (FAIL);
      /* if (parse_equal(T,symb_table,&plist1,0) == FAIL ) return (FAIL); */
      if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
      switch ( T->tokenv.id ) 
	{
	case COMMA_OP :
	case COMMA_RET_OP :
	  ++excnt;       
	  if ( T->NextToken(T) == FAIL) return(FAIL);
	  if ( parse_nblines(T) == FAIL ) return FAIL;
	  break;
	case ']' : ++excnt;       
	  break;
	default:  
	  T->ParseError(T,"Parse Error: waiting for `]' \n");
	  return(FAIL);
	}
    }
  if ( T->NextToken(T) == FAIL) return(FAIL);
  if (debug) Sciprintf("[mlhs:%d)",excnt-1);
  /* excnt counts the arguments + the function name f(a,b)==> excnt=3*/
  if (nsp_parse_add(plist,MLHS,excnt-1,T->tokenv.Line) == FAIL) return(FAIL);
  /* add left returns to symbol_table */
  if ( nsp_parse_add_to_symbol_table(symb_table,*plist) == FAIL) return FAIL;
  return(OK);
}


/*
 * Parse Control structures 
 */


/**
 * parse_while:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * Parsing while loop 
 * <while>:=  while <expr> <begin-while> <exprs>
 * <begin-while> := <do> | , | ; | '\n' | ,<do>| ;<do> 
 * <do> = do | then 
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_while(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  PList plist1 = NULLPLIST ;
  PList plist2 = NULLPLIST ;
  int while_line = T->tokenv.Line;
  if (debug) scidebug(debugI++,"[while>");

  /* Parsing the while condition */
  if ( T->NextToken(T) == FAIL) return(FAIL);
  T->tokenv.FlagEqu = 0;
  if (parse_expr(T,symb_table,&plist1,'f') == FAIL ) 
    {
      T->tokenv.FlagEqu = 0;
      return(FAIL);
    }
#if XXX
  if ( plist1->type == COMMENT ) 
    {
      T->ParseError(T,"Parse Error: a comment was found while expecting a condition for while\n");
      return(FAIL);
    }
#endif 
  T->tokenv.FlagEqu = 0;
  if (debug) scidebug(debugI++,"[while-do>"); 
  /* Tokens for introducing the do Part */
  plist2=NULLPLIST;
  if (parse_bkey(T,THEN,DO,"while",&plist2) == FAIL) return(FAIL);
  plist2=NULLPLIST;
  if (parse_exprs(T,symb_table,&plist2,0,parse_endstop ) == FAIL) return(FAIL);
  if (nsp_parse_add_list(&plist1,&plist2) == FAIL) return(FAIL);
  /* xxx : a revoir */
  if ( parse_nblines(T) == FAIL ) return FAIL;
  if ( T->tokenv.id == END ) 
    {
      if ( T->NextToken(T) == FAIL) return(FAIL);
      if (nsp_parse_add(&plist1,WHILE,2,while_line) == FAIL) return(FAIL);
      if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
      if (debug) scidebug(--debugI,"<while-do]"); 
      if (debug) scidebug(--debugI,"<while]"); 
      return (OK) ;
    }
  else 
    {
      /* utiliser plist2 pour montrer ou on s'est arr'et'e */
      T->ParseError(T,"Parse Error: Missing end of while loop \n");
      return(FAIL);
    }
}


/**
 * parse_bkey:
 * @T: a #Tokenizer 
 * @key1: 
 * @key2: 
 * @str: 
 * @plist: 
 * 
 * Parse the key key1 or key2 followed by one or more '\n'
 * 
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_bkey(Tokenizer *T,int key1, int key2, char *str, PList *plist)
{
  int k;
  k=T->tokenv.id;
  if (k != key1 && k != key2  && k != COMMA_OP && k != SEMICOLON_OP
      && k != COMMA_RET_OP && k != SEMICOLON_RET_OP
      && k != RETURN_OP  && k != COMMENT )
    {
      /* FIXME a am'eliorer avec plist1 qui montre ou on s'est arret'e XXX 
       * plus le message d'erreur T->tokenv.id qui est pas bon
       */
      T->ParseError(T,"Parse Error: Unexpected Token %s found in a %s \n",T->code2name(T,T->tokenv.id),str);
      if ( key1 == key2 ) 
	Scierror("Expecting : %s or , or ; or '\\n'\n",T->code2name(T,key1));
      else 	
	Scierror("Expecting : %s or %d or , or ; or '\\n'\n",T->code2name(T,key1),T->code2name(T,key2));
      return(FAIL);
    }
  if ( T->NextToken(T) == FAIL) return(FAIL);
  if ( parse_nblines(T) == FAIL ) return FAIL;
  if ( T->tokenv.id == key1 || T->tokenv.id == key2  ) 
    {
      if ( T->NextToken(T) == FAIL) return(FAIL);
      if ( parse_nblines(T) == FAIL ) return FAIL;
    }
  return(OK);
}


/**
 * parse_nblines:
 * @T: a #Tokenizer 
 * 
 * Parses a set of blank lines 
 * and take care of '\0' 
 *
 * Returns:  %OK or %FAIL
 **/

static int parse_nblines(Tokenizer *T)
{
  while (1) 
    {
      if  ( T->tokenv.id ==  '\0' )
	{
	  if ( T->ForceNextChar(T) == FAIL) return(FAIL);
	  if ( T->NextToken(T) == FAIL) return(FAIL);
	}
      else 
	{
	  if ( T->tokenv.id == RETURN_OP ) 
	    {
	      if ( T->NextToken(T) == FAIL) return(FAIL);
	    }
	  else 
	    {
	      break;
	    }
	}
    }
  return OK;
}

/* similar to parse_nblines but accepts comments which are 
 * discarded 
 */

static int parse_nblines_and_comments(Tokenizer *T)
{
  while (1) 
    {
      if  ( T->tokenv.id ==  '\0' )
	{
	  if ( T->ForceNextChar(T) == FAIL) return(FAIL);
	  if ( T->NextToken(T) == FAIL) return(FAIL);
	}
      else 
	{
	  if ( T->tokenv.id == RETURN_OP ) 
	    { if ( T->NextToken(T) == FAIL) return(FAIL);}
	  else if ( T->tokenv.id == COMMENT )
	    {
	      { if ( T->NextToken(T) == FAIL) return(FAIL);}
	    }
	  else 
	    break;
	}
    }
  return OK;
}


/**
 * parse_stopif:
 * @T: a #Tokenizer 
 * @token: 
 * 
 * 
 * 
 * Returns: %TRUE or %FALSE
 **/
static int parse_stopif (Tokenizer *T,int token)
{
  return ( token == END || token == ELSEIF || token == ELSE );
}


/**
 * parse_if:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * 
 * Parsing if 
 * <if>:= if <expr> then <exprs> end 
 *        | if <expr> then <exprs> else <exprs> end 
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_if(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  int count = 0,flag=1, if_line = T->tokenv.Line;
  PList plist1 = NULLPLIST ;
  PList plist2 = NULLPLIST ;

  while(flag)
    {
      plist2= NULLPLIST;
      if ( T->NextToken(T) == FAIL) return(FAIL);
      if ( parse_nblines(T) == FAIL ) return FAIL;
      /* do not accept expr = expr1 */
      T->tokenv.FlagEqu = 0;
      /* Parse if condition */
      if (parse_expr(T,symb_table,&plist2,'f') == FAIL )
	{
	  T->tokenv.FlagEqu = 0;
	  return(FAIL);
	}
      T->tokenv.FlagEqu = 0;
      if (nsp_parse_add_list(&plist1,&plist2) == FAIL) return(FAIL);
      count++;
      plist2= NULLPLIST;
      if (debug) scidebug(debugI++,"[then>"); 

      if (parse_bkey(T,THEN,THEN,"if condition",&plist2) == FAIL) return(FAIL);
      plist2= NULLPLIST;
      if (parse_exprs(T,symb_table,&plist2,0,parse_stopif ) == FAIL) return(FAIL);
      if (nsp_parse_add_list(&plist1,&plist2) == FAIL) return(FAIL);
      count++;
      if ( parse_nblines(T) == FAIL ) return FAIL;
      switch ( T->tokenv.id )
	{
	case END :
	  if ( T->NextToken(T) == FAIL) return(FAIL);
	  flag= 0;
	  break;
	case  ELSEIF :
	  continue;
	  break;
	case ELSE :
	  if (debug) scidebug(debugI++,"[else>");
	  if ( T->NextToken(T) == FAIL) return(FAIL);
	  if ( parse_nblines(T) == FAIL ) return FAIL;
	  plist2= NULLPLIST;
	  if (parse_exprs(T,symb_table,&plist2,0,parse_endstop ) == FAIL) return(FAIL);
	  if (nsp_parse_add_list(&plist1,&plist2) == FAIL) return(FAIL);
	  count++;
	  if ( parse_nblines(T) == FAIL ) return FAIL;
	  if ( T->tokenv.id == END ) 
	    {
	      if ( T->NextToken(T) == FAIL) return(FAIL);
	      flag=0;
	      break;
	    }
	  else
	    {
	      T->ParseError(T,"Parse Error: Missing end at end of else part  \n");
	      return(FAIL);
	    }
	  break;
	default:
	  T->ParseError(T,"Parse Error: end of if not found\n");
	  return(FAIL);
	}
    }
  if (nsp_parse_add(&plist1,IF,count, if_line ) == FAIL) return(FAIL);
  if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
  if (debug) scidebug(--debugI,"<endif]");
  return (OK) ;
}


/**
 * parse_stopselect:
 * @T: a #Tokenizer 
 * @token: 
 * 
 * 
 * 
 * Returns: %TRUE or %FALSE
 **/

static int parse_stopselect (Tokenizer *T,int token)
{
  return ( token == END || token == CASE || token == ELSE );
}

/**
 * parse_select:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * Parsing select 
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_select(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  int case_line = -1, select_line = -1, else_line = -1;
  int kount = 1 ; /* counts the number of cases */
  PList plist1 = NULLPLIST ;
  PList plist2 = NULLPLIST ;
  PList plist3 = NULLPLIST ;
  if (debug) scidebug(debugI++,"[select>");
  select_line = T->tokenv.Line;
  /* select <expr> */
  if ( T->NextToken(T) == FAIL) return(FAIL);
  if (parse_expr(T,symb_table,&plist1,'f') == FAIL ) return(FAIL);
  if (nsp_parse_add_list(&plist2,&plist1) == FAIL) return(FAIL);
 CaseLoop:
  plist1 = NULLPLIST ;
  if (parse_bkey(T,CASE,CASE,"select",&plist1) == FAIL) return(FAIL);
  case_line = T->tokenv.Line;
  /* case <expr> */
  if (debug) scidebug(debugI++,"[case_exp >"); 
  plist1=NULLPLIST;
  if (parse_expr(T,symb_table,&plist1,'f') == FAIL ) return(FAIL);
  if ( plist1->type == COMMENT)
    {
      nsp_plist_destroy(&plist1);
      goto CaseLoop;
    }
  else
    {
      if (nsp_parse_add_list(&plist3,&plist1) == FAIL) return(FAIL);
      /* then part */
      plist1=NULLPLIST;
      if (parse_bkey(T,THEN,THEN,"select/case",&plist1) == FAIL) return(FAIL);

      if (debug) scidebug(--debugI,"<case_exp]"); 
      if (debug) scidebug(debugI++,"[case>"); 
  
      plist1=NULLPLIST;
      if (parse_exprs(T,symb_table,&plist1,0,parse_stopselect ) == FAIL) return(FAIL);
      if (nsp_parse_add_list(&plist3,&plist1) == FAIL) return(FAIL);
      if (nsp_parse_add(&plist3,CASE,2,case_line) == FAIL) return(FAIL);
      if (nsp_parse_add_list(&plist2,&plist3) == FAIL) return(FAIL);
      kount++;
    }
  switch ( T->tokenv.id ) 
    {
    case END : 
      if ( T->NextToken(T) == FAIL) return(FAIL);
      if (nsp_parse_add(&plist2,SELECT,kount,select_line) == FAIL) return(FAIL);
      if (nsp_parse_add_list(plist,&plist2) == FAIL) return(FAIL);
      if (debug) scidebug(--debugI,"<endselect]"); 
      return (OK) ;
    case CASE : 
      plist3=NULLPLIST;
      goto CaseLoop;
    case  ELSE :
      else_line = T->tokenv.Line;
      if ( T->NextToken(T) == FAIL) return(FAIL);
      plist1=NULLPLIST;
      if (parse_exprs(T,symb_table,&plist1,0,parse_endstop ) == FAIL) return(FAIL);
      if ( T->tokenv.id == END )
	{
	  plist3=NULLPLIST;
	  if (nsp_parse_add_list(&plist3,&plist1) == FAIL) return(FAIL);
	  if (nsp_parse_add(&plist3,LASTCASE,1,else_line ) == FAIL) return(FAIL);
	  if ( T->NextToken(T) == FAIL) return(FAIL);
	  if (nsp_parse_add_list(&plist2,&plist3) == FAIL) return(FAIL);
	  kount++;
	  if (nsp_parse_add(&plist2,SELECT,kount,select_line) == FAIL) return(FAIL);
	  if (nsp_parse_add_list(plist,&plist2) == FAIL) return(FAIL);
	  if (debug) scidebug(--debugI,"<endselect]"); 
	  return (OK) ;
	}
      else 
	{
	  /* utiliser plist2 */
	  T->ParseError(T,"Parse Error: Missing end in select  \n");
	  return(FAIL);
	}
    default:
      /* utiliser plist3 */
      T->ParseError(T,"Parse Error in select: waiting for a case or then keyword  \n");
      return(FAIL);
    }
}



/**
 * parse_stop_catch:
 * @T: 
 * @token: 
 * 
 * 
 * 
 * Returns: %TRUE or %FALSE
 **/

static int parse_stop_catch (Tokenizer *T,int token)
{
  return ( token == END || token == FINALLY  );
}

/**
 * parse_stop_try:
 * @T: 
 * @token: 
 * 
 * 
 * 
 * Returns: %TRUE or %FALSE
 **/

static int parse_stop_try (Tokenizer *T,int token)
{
  return ( token == CATCH);
}

/**
 * parse_try_catch:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * Parsing try catch finally 
 * 
 * Returns: 
 **/

static int parse_try_catch(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  PList plist1 = NULLPLIST ;
  PList plist2 = NULLPLIST ;
  int try_line = T->tokenv.Line;
  if (debug) scidebug(debugI++,"[try>"); 
  /* try <expr> */
  if ( T->NextToken(T) == FAIL) return(FAIL);
  if ( parse_exprs(T,symb_table,&plist1,0,parse_stop_try ) == FAIL) return(FAIL);
  if (nsp_parse_add_list(&plist2,&plist1) == FAIL) return(FAIL);
  plist1 = NULLPLIST ;
  if ( T->NextToken(T) == FAIL) return(FAIL);
  /* catch <expr> */
  if (debug) scidebug(debugI++,"[catch_exp >"); 
  plist1=NULLPLIST;
  if ( parse_exprs(T,symb_table,&plist1,0,parse_stop_catch ) == FAIL) return(FAIL);
  if (nsp_parse_add_list(&plist2,&plist1) == FAIL) return(FAIL);
  if (debug) scidebug(--debugI,"<catch_exp]"); 
  switch ( T->tokenv.id ) 
    {
    case END : 
      if ( T->NextToken(T) == FAIL) return(FAIL);
      if (nsp_parse_add(&plist2,TRYCATCH,2,try_line) == FAIL) return(FAIL);
      if (nsp_parse_add_list(plist,&plist2) == FAIL) return(FAIL);
      if (debug) scidebug(--debugI,"<endselect]"); 
      return (OK) ;
    case  FINALLY :
      if ( T->NextToken(T) == FAIL) return(FAIL);
      plist1=NULLPLIST;
      if (parse_exprs(T,symb_table,&plist1,0,parse_endstop ) == FAIL) return(FAIL);
      if ( T->tokenv.id == END )
	{
	  if (nsp_parse_add_list(&plist2,&plist1) == FAIL) return(FAIL);
	  if ( T->NextToken(T) == FAIL) return(FAIL);
	  if (nsp_parse_add(&plist2,TRYCATCH,3,try_line) == FAIL) return(FAIL);
	  if (nsp_parse_add_list(plist,&plist2) == FAIL) return(FAIL);
	  if (debug) scidebug(--debugI,"<endselect]"); 
	  return (OK) ;
	}
      else 
	{
	  T->ParseError(T,"Parse Error: Missing end in try/catch  \n");
	  return(FAIL);
	}
    default:
      T->ParseError(T,"Parse Error in try/catch: waiting for end or finally\n");
      return(FAIL);
    }
}

/**
 * parse_for:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * Parsing for loop 
 * for x=val <begin-for> <exprs> end 
 * <begin-for> = do | , | ; | '\n'
 * The recursion Stack is used 
 *        to store the loop var name 
 *        to store the number of ierations 
 * 
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_for(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  PList plist1 = NULLPLIST ;
  PList plist2 = NULLPLIST ;
  int for_line = T->tokenv.Line;

  /* Name of the loop var : for x=... */
  if (debug) scidebug(debugI++,"[for>");
  if ( T->NextToken(T) == FAIL) return(FAIL);
  if (T->tokenv.id != NAME)
    {
      T->ParseError(T,"Parse Error: Unexpected token after do : %s\n",T->code2name(T,T->tokenv.id));
      Scierror("\tExpecting a symbol\n");
      return(FAIL);
    }
  if (nsp_parse_add_name(&plist1,T->tokenv.syn) == FAIL) return(FAIL);
  if ( nsp_parse_add_to_symbol_table(symb_table,plist1) == FAIL) return FAIL;
  if (nsp_parse_add_list(&plist2,&plist1) == FAIL) return(FAIL);
  if ( T->NextToken(T) == FAIL) return(FAIL);

  if (T->tokenv.id != EQUAL_OP) 
    {
      T->ParseError(T,"Parse Error: Unexpected token %s\n",T->code2name(T,T->tokenv.id));
      Scierror("\tExpecting an `=' sign\n");
      return(FAIL);
    }
  
  /* Parsing val in x=val */

  if ( T->NextToken(T) == FAIL) return(FAIL);
  plist1=NULLPLIST;
  if (parse_expr(T,symb_table,&plist1,'f') == FAIL ) return(FAIL);
  if (nsp_parse_add_list(&plist2,&plist1) == FAIL) return(FAIL);
  if (debug) scidebug(debugI++,"[do>"); 
  plist1=NULLPLIST;
  if ( T->tokenv.id == COMMENT) 
    {
      plist1=NULLPLIST;
      if (nsp_parse_add_comment(&plist1,T->tokenv.buf)==FAIL) return(FAIL);
      if ( T->NextToken(T) == FAIL) return(FAIL);
      /* XXX the comment just after for x=expr // comment 
       * is lost here. We can keep it whith the next line 
       * but we have then to accept that for can have arity 3 or 4 
       * and this is to be changed in eval/print etc...
       * if (nsp_parse_add_list(&plist2,&plist1) == FAIL) return(FAIL); 
       */
      plist1=NULLPLIST;
    }
  /* Tokens for introducing the do Part */
  if (parse_bkey(T,DO,DO,"for loop",&plist1) == FAIL) return(FAIL);
  plist1=NULLPLIST;
  if (parse_exprs(T,symb_table,&plist1,0,parse_endstop ) == FAIL) return(FAIL);
  if (nsp_parse_add_list(&plist2,&plist1) == FAIL) return(FAIL);
  /* XXX : a revoir */
  if ( parse_nblines(T) == FAIL ) return FAIL;
  if ( T->tokenv.id == END ) 
    {
      if ( T->NextToken(T) == FAIL) return(FAIL);
      if (nsp_parse_add(&plist2,FOR,3,for_line) == FAIL) return(FAIL);
      if (nsp_parse_add_list(plist,&plist2) == FAIL) return(FAIL);
      if (debug) scidebug(--debugI,"<enddo]"); 
      return (OK) ;
    }
  else 
    {
      /* utiliser plist2 */
      T->ParseError(T,"Parse Error: Missing end of loop\n");
      return(FAIL);
    }
}



/**
 * parse_equal:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @flag: 
 * 
 * Parsing  <equal> 
 * <equal> :=  <expr> | <Lexpr> = <Rexpr>
 *         <Lexpr> := <expr> 
 *         <Rexpr> := <expr>
 * xxx A second Pass is necessary 
 * to check if = is affectation or just an == operator 
 * flag is used to detect optional argument in calling 
 *      list i.e f(....,x=10,....) 
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_equal(Tokenizer *T,NspBHash *symb_table,PList *plist, int flag)
{
  int kount =0,op = EQUAL_OP, equal_op_line ;
  PList plist1 = NULLPLIST, plist2=NULLPLIST,plist3=NULLPLIST;
  if (debug) scidebug(debugI++,"[equal>");
  if (parse_expr(T,symb_table,&plist1,'f') == FAIL ) return(FAIL);
  equal_op_line = T->tokenv.Line;
  if (T->tokenv.id != EQUAL_OP) 
    {
      if (nsp_parse_add_list(plist,&plist1) == FAIL ) return(FAIL);
      if (debug) scidebug(--debugI,"<equal]");
      return(OK);
    }
  else 
    {
      /* at this point operator can only be = if it was == it would 
	 have been parsed in <expr> */
      if ( T->NextToken(T) == FAIL) return(FAIL);
    }
  if ( parse_nblines(T) == FAIL ) return FAIL;
  if (parse_expr(T,symb_table,&plist2,'f') == FAIL ) return(FAIL);
  if ( flag == 1) 
    {
      /* MLHS must be reduced to one name */
      /* optional arguments can be given as 'string'=val */
      if ( plist1->type == STRING && plist1->next == NULLPLIST) plist1->type = OPNAME; 
      
      if ( !(plist1->type == NAME || plist1->type == OPNAME) || plist1->next != NULLPLIST)
	{
	  nsp_plist_print(plist1,0);
	  T->ParseError(T,"Parse Error: is not a correct expression for naming optional argument\n");
	  return FAIL;
	}
    }
  else 
    {
      if (nsp_check_is_mlhs(plist1,&plist3,&kount)== OK) 
	{
	  char *name;
	  /* we change expr1 into a mlhs */
	  if (nsp_parse_add(&plist3,MLHS,kount,   equal_op_line) == FAIL) return(FAIL);
	  if ( nsp_parse_add_to_symbol_table(symb_table,plist3) == FAIL) return FAIL;
	  if ( nsp_check_simple_mlhs(plist3) == OK) 
	    {
	      /* Sciprintf("A simple mlhs \n"); */
	    }
	  if ((name=nsp_check_unique_name_in_mlhs(plist3)) != NULL)
	    {
	      T->ParseError(T,"Parse Error: %s is wrong, names cannot appear duplicated in mlhs.\n",
			    name);
	      return FAIL;
	    }
	  nsp_plist_destroy(&plist1);
	  plist1=NULLPLIST;
	  if (nsp_parse_add_list1(&plist1,&plist3) == FAIL) return(FAIL);
	}
      else 
	{
	  /* XXXXX must clear plist3 */
	  Sciprintf("Warning: Obsolete use of =, assuming == \n");
	  op = EQ;
	}
    }
  if (nsp_parse_add_list(&plist1,&plist2) == FAIL) return(FAIL);
  if ( debug) Sciprintf("{%s:arity2}",(op==EQ) ? "==" : "=");
  if ( flag == 0 )
    {
      if (nsp_parse_add(&plist1,op,2,  equal_op_line) == FAIL) return(FAIL); 
    }
  else 
    { 
      
      if (nsp_parse_add(&plist1,OPT,2, equal_op_line) == FAIL) return(FAIL);
    }
  if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
  if (debug) scidebug(--debugI,"<equal]");
  return(OK);
}


/**
 * parse_expr:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @inmatrix: a flag 
 * 
 * Parsing expressions 
 * <expr> :  <lexpr> | : | <lexpr>:<lexpr>:<lexpr> | <lexpr>:<lexpr> 
 *
 * Returns:  %OK or %FAIL
 **/

static int parse_expr(Tokenizer *T,NspBHash *symb_table,PList *plist,char inmatrix)
{
  PList plist1 = NULLPLIST ;
  PList plist2 = NULLPLIST ;
  /* Local variables */
  int kount=1, colon_op_line;
  if (debug) scidebug(debugI++,"[expr>");
  colon_op_line = T->tokenv.Line;
  if (T->tokenv.id == COLON_OP) 
    {
      if (nsp_parse_add(&plist2,COLON_OP,0,T->tokenv.Line) == FAIL) return(FAIL);
      if (nsp_parse_add_list1(&plist1,&plist2) == FAIL) return(FAIL);
      if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
      if (debug) scidebug(--debugI,"<expr]");
      if ( T->NextToken(T) == FAIL) return(FAIL);
      return(OK);
    }
  while (1)
    {
      PList plist2 = NULLPLIST ;
      if (parse_lexpr(T,symb_table,&plist2,inmatrix) == FAIL) return(FAIL);
      if (nsp_parse_add_list(&plist1,&plist2) == FAIL) return(FAIL);
      if ( T->tokenv.id == COLON_OP ) 
	{
	  /* we have parsed `:' and search for more */
	  if ( T->NextToken(T) == FAIL) return(FAIL);
	  while ( T->tokenv.id == RETURN_OP) if ( T->NextToken(T) == FAIL) return(FAIL);
	  ++kount;
	}
      else 
	break;
    }
  if (kount > 3) 
    {
      T->ParseError(T,"Parse Error: Too many ':' \n");
      return(FAIL);
    }
  else if (kount > 1) 
    {
      if (debug) Sciprintf("{:arity%d}",kount);
      if (nsp_parse_add(&plist1,COLON_OP,kount, colon_op_line ) == FAIL) return(FAIL);
      if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
    }
  else if (kount ==1 ) 
    {
      if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
    }
  if (debug) scidebug(--debugI,"<expr]");
  return(OK);
}

/**
 * nsp_is_or_op:
 * @T: a #Tokenizer 
 * @op: an integer pointer 
 * @inmatrix: a character flag
 * 
 * detects or or sequential or operators.
 * 
 * Returns: %OK or %FAIL 
 **/

static int nsp_is_or_op(Tokenizer *T,int *op, char inmatrix)
{
  if  ( T->tokenv.id == OR_OP || T->tokenv.id == SEQOR )
    {
      *op =T->tokenv.id ;   if ( T->NextToken(T) == FAIL) return(FAIL); return(OK);
    }
  else 
    {
      *op = 0 ;    return(FAIL);
    }
}

/**
 * parse_lexpr:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * parsing <lexpr> := <lterm> op <lterm> op <lterm> ...
 * op = | or || 
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_lexpr(Tokenizer *T,NspBHash *symb_table,PList *plist,char inmatrix)
{
  return(parse_nary_opt(T,symb_table,plist,parse_lterm,nsp_is_or_op,"lexpr",inmatrix));
}


/**
 * nsp_is_and_op:
 * @T: a #Tokenizer 
 * @op: an integer pointer 
 * @inmatrix: a character flag
 * 
 * check and or sequential and
 * 
 * Returns:  %OK or %FAIL
 **/

static int nsp_is_and_op(Tokenizer *T,int *op,char inmatrix)
{
  if  (T->tokenv.id == AND_OP || T->tokenv.id == SEQAND )
    {
      *op = T->tokenv.id ;   if ( T->NextToken(T) == FAIL) return(FAIL); return(OK);
    }
  else 
    {
      *op = 0 ;   return(FAIL);
    }
}

/**
 * parse_lterm:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * parsing <lterm> := <lprim> op <lprim> op <lprim> ...
 * op = & | &&
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_lterm(Tokenizer *T,NspBHash *symb_table,PList *plist,char inmatrix)
{
  return(parse_nary_opt(T,symb_table,plist,parse_lprim,nsp_is_and_op,"lterm",inmatrix));
}

/**
 * IsLprimOp:
 * @T: a #Tokenizer 
 * @op: an integer pointer 
 * @inmatrix: a character flag
 * 
 * checks comparison operators  = | <> | ~=  | == 
 *
 * Returns:  %OK or %FAIL
 **/

static int IsLprimOp(Tokenizer *T,int *op,char inmatrix)
{
  int lop = T->tokenv.id;
  if ( lop == NEQ || lop == EQ || lop == DOTNEQ || lop == DOTEQ 
       || ( lop == EQUAL_OP && T->tokenv.FlagEqu == 1  ))
    {
      *op = T->tokenv.id ;if ( T->NextToken(T) == FAIL) return(FAIL); return(OK);
    }
  else 
    {
      *op = 0 ;  return(FAIL);
    }
}

/**
 * parse_lprim:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @inmatrix: a character flag
 * 
 * parsing logical expressions
 * <lprim> := <lprim1> op <lprim1> ...
 *    op :=  = | <> | ~=  | == 
 *    operator = is accepted or not according to a flag 
 *    T->tokenv.FlagEqu
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_lprim(Tokenizer *T,NspBHash *symb_table,PList *plist,char inmatrix)
{
  return(parse_nary_opt(T,symb_table,plist,parse_lprim1,IsLprimOp,"lprim",inmatrix));
} 

/**
 * IsLprim1Op:
 * @T: a #Tokenizer 
 * @op: an integer pointer 
 * @inmatrix: a character flag
 * 
 * checks   >= | <= | > | < 
 * 
 * Returns:  %OK or %FAIL
 **/

static int IsLprim1Op(Tokenizer *T,int *op,char inmatrix)
{
  int lop = T->tokenv.id;
  if ( lop == LT_OP || lop == GT_OP || lop ==  GEQ || lop == LEQ 
       ||  lop == DOTLT || lop == DOTGT || lop ==  DOTGEQ || lop == DOTLEQ)
    {
      *op = T->tokenv.id ;if ( T->NextToken(T) == FAIL) return(FAIL); return(OK);
    }
  else 
    {
      *op = 0 ;  return(FAIL);
    }
}

/**
 * parse_lprim1:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * logical primitive operators 
 * <lprim1> := <terms> op <terms> ...
 *    op :=   >= | <= | > | < 
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_lprim1(Tokenizer *T,NspBHash *symb_table,PList *plist,char inmatrix)
{
  return(parse_nary_opt(T,symb_table,plist,parse_terms,IsLprim1Op,"lprim1",inmatrix));
} 

/**
 * IstermsOp:
 * @T: a #Tokenizer 
 * @op: an integer pointer 
 * @inmatrix: a character flag
 * 
 * checks  + | - | .+
 * 
 * Returns:  %OK or %FAIL
 **/

static int IstermsOp(Tokenizer *T,int *op,char inmatrix)
{
  switch ( T->tokenv.id ) 
    {
    case PLUS_OP:
    case MINUS_OP: 
      {
	int before = T->curline.lpt2 -2;
	if (inmatrix == 't' 
	    &&  T->curline.buf[T->curline.lpt2] != ' '
	    && (T->curline.buf[before] == ' ' 
		|| T->curline.buf[before] == '\t' ))
	  {
	    *op = 0;
	    return FAIL;
	  }
	*op = T->tokenv.id ;if ( T->NextToken(T) == FAIL) return(FAIL); return(OK);
      }
      break;
    case DOTPLUS: 
      *op = T->tokenv.id ;if ( T->NextToken(T) == FAIL) return(FAIL); return(OK);
      break;
    default: 
      *op = 0 ;  return(FAIL);
      break;
    }
}

/**
 * parse_terms:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * 
 * parsing <terms> := <terme1> op <terme1> op <terme1> .....
 *   op = + | - | .+
 *   2-ary left associative + or - 
 *
 * Returns:  %OK or %FAIL
 **/

static int parse_terms(Tokenizer *T,NspBHash *symb_table,PList *plist,char inmatrix)
{
  return(parse_nary_opt(T,symb_table,plist,parse_terme1,IstermsOp,"terms",inmatrix));
} 

/**
 * parse_terme1:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * 
 * parsing <terme1>:= <terme> | +<terme>  | -<terme> | ~<terme>
 *     unary + or - or ~ with left associativity 
 *
 * Returns:  %OK or %FAIL
 **/

static int parse_terme1(Tokenizer *T,NspBHash *symb_table,PList *plist,char inmatrix)
{
  PList plist1 = NULLPLIST ;
  int op= 0, op_line = T->tokenv.Line;
  if (debug) scidebug(debugI++,"[terme1>");
  if (T->tokenv.id == PLUS_OP 
      || T->tokenv.id == MINUS_OP  || ( T->tokenv.id == TILDE_OP  && T->tokenv.NextC != '='))
    {
      op = T->tokenv.id;
      if ( T->NextToken(T) == FAIL) return(FAIL);
      while ( T->tokenv.id == RETURN_OP) if ( T->NextToken(T) == FAIL) return(FAIL);
    }
  if (parse_terme(T,symb_table,&plist1) == FAIL ) return(FAIL);
  if (op == MINUS_OP || op == TILDE_OP )
    { 
      if (nsp_parse_add(&plist1,op,1, op_line ) == FAIL) return(FAIL);
    }
  if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
  if (debug) scidebug(--debugI,"<(%s)terme1]",(op ==0) ? "" :nsp_astcode_to_name(op));
  return(OK);
}

/**
 * IstermOp:
 * @T: a #Tokenizer 
 * @op1: an integer pointer 
 * 
 * checks multiplications and divisions 
 * 
 * Returns:  %OK or %FAIL
 **/

static int IstermOp(Tokenizer *T,int *op1)
{
  int op =  T->tokenv.id;
  if (op  == STAR_OP || op == BACKSLASH_OP || op == SLASH_OP || op == DOTSTARDOT
      || op == DOTSTAR || op == DOTSLASHDOT || op == DOTSLASH 
      || op == DOTBSLASHDOT || op ==  DOTBSLASH || op == BSLASHDOT || op == SLASHDOT) 
    {
      *op1 = op;
      if ( T->NextToken(T) == FAIL) return(FAIL);
      return(OK);
    }
  *op1=0;
  return(FAIL);
}

/**
 * parse_terme:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 *
 * parsing <terme> := <fact> op <fact> op <fact> 
 *  op =  .* | ./ | .\ | .*. | ./. | .\. | * | / |  \  | /. | \.
 *
 * and all the op have the same priority 
 * the expresson is evaluated from left to right 
 *   <terme> = <fact> op <fact> op <fact> 
 *        --> (<fact> op <fact>) op <fact> 
 *
 *  searches operator like .* ./ .\ or .*. ./. .\.  * / \ 
 *  we code operators with : 
 *   .* --> op = '*' << 7 + '.' 
 *   .*. --> op = '.' << 14 + '*' << 7 + '.'
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_terme(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  return(parse_nary(T,symb_table,plist,parse_fact,IstermOp,"terme"));
} 


/**
 * IsFact:
 * @T: a #Tokenizer 
 * @op1: an integer pointer 
 * 
 * checks op = ^ | **  | .^ | .**
 * 
 * Returns:  %OK or %FAIL
 **/

static int IsFact(Tokenizer *T,int *op1)
{
  int op = T->tokenv.id ;
  if ( op == HAT_OP || op == DOTHAT )
    {
      if ( T->NextToken(T) == FAIL) return(FAIL);
      *op1 = op;
      return(OK) ;
    }
  *op1 = 0 ;
  return(FAIL);
}

/**
 * parse_fact:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * Parsing <fact> := <fact2> op <fact2> op <fact2> 
 *       op = ^ | **  | .^ | .**
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_fact(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  return(parse_nary(T,symb_table,plist,parse_fact2,IsFact,"fact"));
}

/**
 * parse_fact2:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * parsing <fact2> := <fact3> | <fact3>' | <fact3>.' 
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_fact2(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  int op;
  PList plist1 = NULLPLIST ;
  if (debug) scidebug(debugI++,"[fact2>");
  if (parse_fact3(T,symb_table,&plist1) == FAIL ) return(FAIL);
  /*   check for quote (transpose) */
  if ( ( op=T->IsTranspose(T)) == QUOTE_OP || op == DOTPRIM )
    {
      PList plist2 = NULLPLIST ;
      if (debug) Sciprintf("{%d}",op);
      if (nsp_parse_add_list(&plist2,&plist1) == FAIL) return FAIL;
      if (nsp_parse_add(&plist2,op,1,T->tokenv.Line) == FAIL) return FAIL;
      if (nsp_parse_add_list(plist,&plist2)==FAIL) return(FAIL);
      if ( T->NextToken(T) == FAIL) return(FAIL);
    }
  else
    {
      if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
    }
  if (debug) scidebug(--debugI,"<fact2]");
  return(OK);
}

/**
 * parse_fact3:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 *
 * <fact3> := <matrix> | string | number | <symb> |  <symb>() 
 *          | <symb>(<equal>,<equal>,....) 
 *          | ( <expr> ) | (<expr1>,<expr2>,....)
 *          | <terme1>
 *          | <comment>
 * 
 * <symb>:= symb | symb.<symb> 
 * 
 * Comments are accepter in fact3 
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_fact3(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  char id[NAME_MAXL];
  int count;
  if (debug) scidebug(debugI++,"[fact3>");  
  if (  T->tokenv.id == '\0' )
    {
      if ( T->ForceNextChar(T) == FAIL) return(FAIL);
      if ( T->NextToken(T) == FAIL) return(FAIL);
    }
  if (  T->tokenv.id == '/' && T->tokenv.NextC == '/' ) 
    {
#if 1
      if ( T->ParseComment(T) == FAIL ) return(FAIL);
#else
      char c;
      c=T->GetChar(T);
      while (c != '\n') c=T->GetChar(T);
      if ( T->ForceNextChar(T) == FAIL) return(FAIL);
#endif
      if ( T->NextToken(T) == FAIL) return(FAIL);
    }
  switch ( T->tokenv.id ) 
    {
    case '/':
      // bizarre 
      return(OK); 
    case PLUS_OP:
    case MINUS_OP: 
    case TILDE_OP: 
      return parse_terme1(T,symb_table,plist,'f');
    case '[' : 
      /*  *************    get a matrix */
      if (debug) scidebug(debugI++,"[mat>");
      if 
#ifdef NSP_PARSE_MATRIX_AS_CELLS 
	(parse_cells(T,symb_table,plist,']') == OK ) 
#else 
	(parse_matrix(T,symb_table,plist,']') == OK ) 
#endif 
	  {
	    if (debug) scidebug(--debugI,"<mat]");
	    break;
	  }
      else 
	{
	  return(FAIL);
	}
    case '{' :
      /*  *************    cells  */
      if (debug) scidebug(debugI++,"[mat>");
      if (parse_cells(T,symb_table,plist,'}') == OK ) 
	{
	  if (debug) scidebug(--debugI,"<mat]");
	  break;
	}
      else 
	{
	  return(FAIL);
	}
    case QUOTE_OP:  /* case '\'' : */
    case '"'  :
      /*  *************     get a string */
      if ( T->ParseString(T) == FAIL ) return FAIL;
      if (nsp_parse_add_string(plist,T->tokenv.buf,
			       (T->tokenv.id== QUOTE_OP) ? 0 : 1 ) == FAIL)
	return(FAIL);
      if ( T->NextToken(T) == FAIL) return(FAIL);
      break;
    case NUMBER  :
      /*  ************     get single number */
      if ( debug)  Sciprintf("[N:%f]",atof(T->tokenv.buf));
      if (nsp_parse_add_doublei(plist,T->tokenv.buf) == FAIL) return(FAIL);
      if ( T->NextToken(T) == FAIL) return(FAIL);
      break;
    case INUMBER32 :
    case INUMBER64 :
    case UNUMBER32 :
    case UNUMBER64 :
      if ( debug)  Sciprintf("[N:%f]",atof(T->tokenv.buf));
      if (nsp_parse_add_inti(plist,T->tokenv.buf, T->tokenv.id) == FAIL) return(FAIL);
      if ( T->NextToken(T) == FAIL) return(FAIL);
      break;
    case EXEC : /* command turned to function call */
    case GLOBAL:
    case APROPOS:
    case WHAT :  
    case WHO :   
      T->tokenv.id = -2;
    case NAME :
      /*  ************     function call (), . operator, {} operator and .meth[] 
       * f<exp1><exp2><...><expn> 
       *     where <expi>=(..) or .name 
       *     --> (f ( <exp1> ARGS) (<exp2> ARGS) .... (<expn> ARGS) LISTEVAL) 
       *         (<expi>) --> ( <expi> ARGS) 
       *         name   --> ( "name" DOTARGS)
       *         [<expi>] --> [ <expi> METARGS ] 
       *         {<expi>} --> [ <expi> CELLARGS ] 
       *     for example 
       *     f(10).a(20,30)(b,c).z[10] 
       *     --> (f ( 10 ARGS) ( "a" DOTARGS) ( 20 30 ARGS) ( b c  ARGS) 
       *            ("z" DOTARGS) ( 10 METARGS )
       *          LISTEVAL) 
       */
      strncpy(id,T->tokenv.syn,NAME_MAXL);
      if (nsp_parse_add_name(plist,id) == FAIL) return(FAIL);
      if ( T->NextToken(T) == FAIL) return(FAIL);
      if ( parse_listeval(T,symb_table,plist) == FAIL) return FAIL;
      /* nsp_plist_print_internal(*plist); */
      break;
    case  '(' : 
      /*  ************   ( expr,expr,expr ) */
      if ( T->NextToken(T) == FAIL) return(FAIL);
      if (parse_exprset(T,symb_table,plist,&count) == FAIL) return(FAIL);
      if ( T->tokenv.id != ')' ) 
	{
	  T->ParseError(T,"Parse Error: missing right parenthesis \n");
	  return(FAIL);
	}
      else 
	{
	  if ( T->NextToken(T) == FAIL) return(FAIL);
	  if ( count == 0) 
	    {
	      /* (expr) followed by (), . operator, {} operator and .meth[] */
	      if ( parse_listeval(T,symb_table,plist) == FAIL) return FAIL;
	    }
	  /* nsp_plist_print_internal(*plist);*/
	  break;
	}
    case COMMENT :
      if (nsp_parse_add_comment(plist,T->tokenv.buf) == FAIL) return(FAIL);
      if ( T->NextToken(T) == FAIL) return(FAIL);
      break;
#if 0 
      /* The following code could be used in order to 
       * accept the function...endfunction syntax in expressions 
       * Ex: intg(0,1,function y=f(x),y=x;endfunction) 
       * it works but the function f exists after the call and 
       * syntax error are not properly detected. It could be 
       * usefull to add a way to give anonymous function name.
       */
    case FUNCTION : 
      if ( parse_function(T,symb_table,plist) == FAIL) return FAIL;
      break;
#endif 
    default :
      /* XXXX : Any  keyword here is an error */
      if ( 0 && nsp_is_code_keyword(T->tokenv.id) == TRUE )
	{
	  if (debug) scidebug(--debugI,"<fact3]");
	  return(OK);
	}
      else 
	{
	  T->ParseError(T,"Parse Error: Unexpected token `%s' found\n",
			T->code2name(T,T->tokenv.id));
	  return(FAIL);
	}
    }
  if (debug) scidebug(--debugI,"<fact3]");
  return(OK);
}


static int parse_listeval(Tokenizer *T,NspBHash *symb_table,PList *plist)
{
  char id[NAME_MAXL];
  PList plist1= NULLPLIST;
  int count=1, w_flag=1, flag = 1, listeval_line;
  /* just name */
  if ( T->tokenv.id != '(' && T->tokenv.id != '[' && T->tokenv.id != '{' 
       && T->IsDotAlpha(T) != OK) 
    {
      /* XXX Here we could accept any characters up to \n , or ; 
       *   f foo.sci --> f('foo.sci') 
       */
      return OK;
    }
  listeval_line = T->tokenv.Line;
  while ( w_flag )
    {
      int dot;
      plist1=NULLPLIST;
      switch ( T->tokenv.id ) {
      case '(' : 
	if (parse_extsymb(T,symb_table,plist,id,flag,&count,')')==FAIL) return FAIL;
	dot = T->tokenv.id == '.' && T->tokenv.NextC != '\'';
	if ( !dot && T->tokenv.id != '('  
	     && T->tokenv.id != '[' && T->tokenv.id != '{' ) 
	  w_flag =0;
	break;
      case '[' :
	if (parse_extsymb(T,symb_table,plist,id,flag,&count,']')==FAIL) return FAIL;
	dot = T->tokenv.id == '.' && T->tokenv.NextC != '\'';
	if ( !dot  && T->tokenv.id != '('
	     && T->tokenv.id != '[' && T->tokenv.id != '{' ) 
	  w_flag =0;
	break;
      case '{' :
	if (parse_extsymb(T,symb_table,plist,id,flag,&count,'}')==FAIL) return FAIL;
	dot = T->tokenv.id == '.' && T->tokenv.NextC != '\'';
	if ( !dot && T->tokenv.id != '(' 
	     && T->tokenv.id != '[' && T->tokenv.id != '{' ) 
	  w_flag =0;
	break;
      case '.': 
	/*  * .<symb> */
	if (  T->tokenv.id == '.' && T->tokenv.NextC == '\'' )
	  {
	    w_flag = 0;
	    break;
	  }
	if ( T->IsDotAlpha(T) == FAIL)
	  {
	    T->ParseError(T,"Parse Error: token `%s' found while expecting , or ; or \\n\n",
			  T->code2name(T,T->tokenv.id));
	    return FAIL;
	  }
	if ( T->NextToken(T) == FAIL) return FAIL;
	if (nsp_parse_add_string(&plist1,T->tokenv.syn,1) == FAIL) return(FAIL);
	if (nsp_parse_add(&plist1,DOTARGS,1,T->tokenv.Line)== FAIL) return FAIL;
	if (nsp_parse_add_list(plist,&plist1)==FAIL) return(FAIL);
	if ( T->NextToken(T) == FAIL) return(FAIL);
	count++;
	break; 
      default :
	w_flag = 0;
	break;
      }
    }
  /* a listeval L()()()..() */
  if (nsp_parse_add(plist,LISTEVAL,count,   listeval_line )== FAIL) return(FAIL);
  /* check if this is a simple call or extract f(...) 
   * if yes LISTEVAL is changed to CALLEVAL to simplify evaluation 
   */
  if ( nsp_check_simple_listeval(*plist) == OK ) 
    {
      /* Sciprintf("This is a simple one\n");
       *nsp_plist_print_internal(*plist);
       */
    }
  if (debug)nsp_plist_print_internal(*plist);
  return OK;
}

/**
 * IsComa:
 * @T: 
 * @op: 
 * 
 * 
 * 
 * Returns:  %OK or %FAIL
 **/

static int IsComa(Tokenizer *T,int *op)
{
  if  (T->tokenv.id == COMMA_OP || T->tokenv.id == COMMA_RET_OP ) {
    *op = PARENTH ;   if ( T->NextToken(T) == FAIL) return(FAIL); return(OK);
  }
  else 
    *op = 0 ;
  return(FAIL);
}



/**
 * parse_exprset:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @count: an integer pointer
 * 
 * parses <lexpr> := <expr> , <expr> , <expr>  
 * when no more ',' are found the function returns and 
 * @count contains the number of comma operator found.
 * Similar to parse_nary except if only one expression is parsed.
 *
 * Returns: %OK or %FAIL
 **/

static int parse_exprset(Tokenizer *T,NspBHash *symb_table,PList *plist,int *count)
{
  int op;
  PList plist1 = NULLPLIST ;
  *count = 0;
  if (debug) scidebug(debugI++,"[parenth>");

  
  if (parse_expr(T,symb_table,&plist1,'f') == FAIL) return(FAIL);
  while( IsComa(T,&op) == OK )
    {
      PList plist2=NULLPLIST;
      (*count)++;
      if (debug) Sciprintf("-arg-");
      if ( parse_nblines_and_comments(T)==FAIL) return FAIL;
      plist2=NULLPLIST;
      if (parse_expr(T,symb_table,&plist2,'f') == FAIL ) return(FAIL);
      if (nsp_parse_add_list(&plist1,&plist2) == FAIL) return(FAIL);
      if (nsp_parse_add(&plist1,op,2,T->tokenv.Line) == FAIL) return(FAIL);
      plist2=plist1;
      if (nsp_parse_add_list1(&plist1,&plist2) == FAIL) return(FAIL);
    }
  if ( *count == 0 ) 
    {
      /* special case : (<expr>)  no op found */
      if (nsp_parse_add(&plist1,PARENTH,1,T->tokenv.Line) == FAIL) return(FAIL);
    }
  if (debug) Sciprintf("{t_op:%d}",op);
  if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
  if (debug) scidebug(--debugI,"<parenth]");
  return(OK);
} 

/**
 * parse_extsymb:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @id: 
 * @flag: 
 * @count: 
 * @end_char: 
 * 
 * nsp_parse(arg1,....,argn) <token> 
 * or    .<name>  <token> 
 * 
 * suppose plist=(a b c )
 * returns in plist = ( a b c ( arg1 ... argn ARGS) )
 * 
 * 
 * Returns: %OK or %FAIL
 **/

static int parse_extsymb(Tokenizer *T,NspBHash *symb_table,PList *plist, char *id, int flag, 
			 int *count, char end_char)
{
  int excnt=0;
  int build = (T->tokenv.id == '(' ) ? ARGS : (T->tokenv.id == '{' ) ? CELLARGS: METARGS ;
  PList plist1= NULLPLIST;  
  if (func_or_matrix_with_arg(T,symb_table,&plist1,id,&excnt,1,end_char)== FAIL) 
    return(FAIL);
  /* first list element extraction */
  if (nsp_parse_add(&plist1,build,excnt,T->tokenv.Line) == FAIL) return(FAIL);
  if ( Check_Func_Call(T,plist1,build) == FAIL) return(FAIL);
  if (nsp_parse_add_list1(&plist1,&plist1)==FAIL) return(FAIL);
  if (nsp_parse_add_list(plist,&plist1)==FAIL) return(FAIL);
  (*count)++;
  return OK;
}


/**
 * IsColMatOp:
 * @T: 
 * @op: 
 * @opt: 
 * 
 * 
 * 
 * Returns: %OK or %FAIL
 **/

static int IsColMatOp(Tokenizer *T,int *op,char opt)
{
  int rowconcat = (opt == ']') ? ROWCONCAT : CELLROWCONCAT;
  switch ( T->tokenv.id) 
    {
    case COMMENT :
      *op = rowconcat;
      if ( T->NextToken(T) == FAIL) return(FAIL);
      return OK;
    case RETURN_OP:
    case SEMICOLON_OP :
    case SEMICOLON_RET_OP :
      *op = rowconcat;
      if ( T->NextToken(T) == FAIL) return(FAIL);
      if ( T->tokenv.id == COMMENT) 
	{
	  /* swallow comments : FIXME should be stored somewhere */
	  if ( T->NextToken(T) == FAIL) return(FAIL);
	}
      return(OK);
    default: *op= rowconcat; return(FAIL);
    }
}


/**
 * IsRowMatOp:
 * @T: 
 * @op: 
 * @opt: 
 * 
 * row separator is , or white space 
 * but as in Matlab ,[ ]*\n is considered as colseparator
 * 
 * Returns: %OK or %FAIL
 **/

static int IsRowMatOp(Tokenizer *T,int *op,char opt)
{
  int colconcat = (opt == ']') ? COLCONCAT : CELLCOLCONCAT;
  *op = colconcat; 
  switch ( T->tokenv.id) 
    {
    case ' ' :
      if ( T->NextToken(T) == FAIL) return(FAIL);
      return(OK);
    case COMMA_OP :
    case COMMA_RET_OP : 
      if ( T->tokenv.NextC == '\n') 
	{
	  /* comma followed by \n is not considered as 
	   * row separator
	   */
	  T->NextToken(T);
	  return FAIL;
	}
      if ( T->NextToken(T) == FAIL) return(FAIL);
      return(OK);
    default: 
      return(FAIL);
    }
}

/**
 * IsDiagMatOp:
 * @T: 
 * @op: 
 * @opt: 
 * 
 * 
 * 
 * Returns: %OK or %FAIL
 **/

static int IsDiagMatOp(Tokenizer *T,int *op,char opt)
{
  int diagconcat = (opt == ']') ? DIAGCONCAT : CELLDIAGCONCAT;
  switch ( T->tokenv.id) 
    {
    case '#' : *op = diagconcat;
      if ( T->NextToken(T) == FAIL) return(FAIL);
      return(OK);
    default: *op= diagconcat ; return(FAIL);
    }
}

/**
 * parse_expr_opt:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @opt: 
 * 
 * 
 * 
 * Returns: %OK or %FAIL
 **/

static int parse_expr_opt(Tokenizer *T,NspBHash *symb_table,PList *plist,char opt)
{
  return parse_expr(T,symb_table,plist,'t');
}

#ifndef NSP_PARSE_MATRIX_AS_CELLS 
/**
 * parse_rowmatrix:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @stop: 
 * 
 * parse_rowmatrix should be just a 
 * parse_nary(&plist1,parse_expr,IsRowMatOp,"matrix")
 * but it's just a bit more complex since we want to accept ' ' 
 * as a column separator 
 *
 * Parsing a Matrix ( <colmatrix>) 
 *  <matrix> := <colmatrix>
 *  <colmatrix> := [ <rowmatrix> ; <rowmatrix> ; .... ] 
 *         0,1,...  <rowmatrix> 
 *  <rowmatrix> := <expr>,<expr>,.....
 * 
 *  ; or \n 
 *  , or ' ' <--- blanc XXXX pas autorise a v'erifier 
 *
 * 1998 : # add for diag concatenation 
 * <matrix> := <diagmatrix>
 *  <diagmatrix> := [ <colmatrix> # <colmatrix> # .... ] 
 * 
 * Returns: %OK or %FAIL
 **/

static int parse_rowmatrix(Tokenizer *T,NspBHash *symb_table,PList *plist,char stop)
{
  Stack *stack = nsp_get_stack();
  int emptymat = (stop == ']') ? EMPTYMAT : EMPTYCELL;
  int colconcat = (stop == ']') ? COLCONCAT : CELLCOLCONCAT;
  PList plist1 = NULLPLIST ;
  if ( T->tokenv.id == stop ) 
    {
      if (nsp_parse_add(&plist1,emptymat,0,T->tokenv.Line) == FAIL) return(FAIL);
      Sciprintf("Warning: ;] should not be used \n");
      if (  T->io == nsp_tok_file )
	Sciprintf("\tat line %d of file %s\n",T->tokenv.Line,NspFileName1(stack));
    }
  else if ( T->tokenv.id == SEMICOLON_OP ||  T->tokenv.id == SEMICOLON_RET_OP)
    {
      if (nsp_parse_add(&plist1,emptymat,0,T->tokenv.Line) == FAIL) return(FAIL);
      Sciprintf("Warning: ;; should not be used \n");
      if (  T->io == nsp_tok_file )
	Sciprintf("\tat line %d of file %s\n",T->tokenv.Line,NspFileName1(stack));
    }
  else
    {
      if (parse_nary_opt(T,symb_table,&plist1,parse_expr_opt,IsRowMatOp,"matrix",stop)== FAIL) 
	return(FAIL);
    }
  while (T->tokenv.id != RETURN_OP && T->tokenv.id != SEMICOLON_OP
	 && T->tokenv.id != SEMICOLON_RET_OP
	 && T->tokenv.id != stop && T->tokenv.id != '#' &&
	 T->tokenv.id != COMMENT )
    {
      PList plist2=NULLPLIST;
      if (parse_nary_opt(T,symb_table,&plist2,parse_expr_opt,IsRowMatOp,"matrix",stop)== FAIL) 
	return(FAIL);
      if (nsp_parse_add_list(&plist1,&plist2) == FAIL) return(FAIL);
      if (nsp_parse_add(&plist1,colconcat,2,T->tokenv.Line) == FAIL) return(FAIL);
      plist2=plist1;
      if (nsp_parse_add_list1(&plist1,&plist2) == FAIL) return(FAIL);
    }
  nsp_parse_add_list(plist,&plist1);
  return(OK);
}
#endif 

#ifndef NSP_PARSE_MATRIX_AS_CELLS 
/**
 * parse_colmatrix:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @stop: 
 * 
 * 
 * 
 * Returns: %OK or %FAIL
 **/

static int parse_colmatrix(Tokenizer *T,NspBHash *symb_table,PList *plist,char stop)
{
  return (parse_nary_opt(T,symb_table,plist,parse_rowmatrix,IsColMatOp,"matrix",stop));
}
#endif 

#ifndef NSP_PARSE_MATRIX_AS_CELLS 

/**
 * parse_matrix:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @stop: 
 * 
 * 
 * 
 * Returns: %OK or %FAIL
 **/

static int parse_matrix(Tokenizer *T,NspBHash *symb_table,PList *plist,char stop)
{
  int emptymat = (stop == ']') ? EMPTYMAT : EMPTYCELL;
  int type  = (stop == ']') ? P_MATRIX : P_CELL ;
  if (T->tokenv.NextC == stop ) 
    {
      /* empty matrix */
      if ( T->NextToken(T) == FAIL) return(FAIL);
      if ( T->NextToken(T) == FAIL) return(FAIL);
      if (nsp_parse_add(plist,emptymat,0,T->tokenv.Line) == FAIL) return(FAIL);
      return(OK);
    }
  if (T->tokenv.NextC == '\n') 
    {
      /* swallow return at begining of matrix definition */
      if ( T->NextToken(T) == FAIL) return(FAIL);
    }
  if ( T->NextToken(T) == FAIL) return(FAIL);

  /*  parse_nblines(T); */
  if (parse_nary_opt(T,symb_table,plist,parse_colmatrix,IsDiagMatOp,"matrix",stop)== FAIL) 
    return(FAIL);
  /*  parse_nblines(T); */
  if ( T->tokenv.id != stop )  
    {
      T->ParseError(T,"Parse Error while parsing a matrix: expecting a `%c'  \n",stop);
      return(FAIL);
    }
  if (nsp_parse_add(plist,type,1,T->tokenv.Line) == FAIL) return(FAIL);
  if ( T->NextToken(T) == FAIL) return(FAIL);
  return(OK);
}
#endif 

/**
 * parse_rowcells:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @stop: 
 * 
 * parses cells {,,,;,,,}
 * this is very similar to parse_matrix except that we use 
 * parse_nary_flat and not parse_nary 
 * which is necessary for cells evaluation later 
 * the semantic of cell creation being slighly different from 
 * the one fro matrix creation. 
 *  But note that parse_nary_flat could be used also for matrix 
 *  that's why we keep the code generic in order to remix 
 *  latter.
 * 
 * 
 * Returns: %OK or %FAIL
 **/

static int parse_rowcells(Tokenizer *T,NspBHash *symb_table,PList *plist,char stop)
{
  Stack *stack = nsp_get_stack();
  int tag = TRUE;
  int emptymat = (stop == ']') ? EMPTYMAT : EMPTYCELL;
  int colconcat = (stop == ']') ? COLCONCAT : CELLCOLCONCAT;
  PList plist1 = NULLPLIST ;
  /* we parse a row expression */
  if ( T->tokenv.id == stop ) 
    {
      /* row is empty and ended by ] */
      if (nsp_parse_add(&plist1,emptymat,0,T->tokenv.Line) == FAIL) return(FAIL);
      Sciprintf("Warning: ;] should not be used \n");
      if (  T->io == nsp_tok_file )
	Sciprintf("\tat line %d of file %s\n",T->tokenv.Line,NspFileName1(stack));

    }
  else if ( T->tokenv.id == SEMICOLON_OP ||  T->tokenv.id == SEMICOLON_RET_OP)
    {
      /* row is empty and ended by ; */
      if (nsp_parse_add(&plist1,emptymat,0,T->tokenv.Line) == FAIL) return(FAIL);
      Sciprintf("Warning: ;; should not be used \n");
      if (  T->io == nsp_tok_file )
	Sciprintf("\tat line %d of file %s\n",T->tokenv.Line,NspFileName1(stack));

    }
  else
    {
      /* parse a set of expressions separated by , the white space case is 
       * parsed below in the while 
       */ 
      if (parse_nary_flat_opt(T,symb_table,&plist1,parse_expr_opt,IsRowMatOp,"matrix",stop,tag)== FAIL) 
	return(FAIL);
    }
  while (T->tokenv.id != RETURN_OP && T->tokenv.id != SEMICOLON_OP && T->tokenv.id != SEMICOLON_RET_OP
	 && T->tokenv.id != stop 
	 && T->tokenv.id != '#' &&  T->tokenv.id != COMMENT )
    {
      PList plist2=NULLPLIST;
      if (parse_nary_flat_opt(T,symb_table,&plist2,parse_expr_opt,IsRowMatOp,"matrix",stop,tag)== FAIL) 
	return(FAIL);
      /* We have to take care here of the merge of CELLCOLCONCAT when plist1 and plist2 are 
       * both  CELLCOLCONCAT. This happens when colconcat is performed with white spaces. 
       */
      if ( plist1->type == PLIST && 
	   (((PList) plist1->O)->type == CELLCOLCONCAT || ((PList) plist1->O)->type == COLCONCAT ))
	{
	  PList loc1 = (PList) plist1->O;
	  PList loc2 = (PList) plist2->O;
	  if (  plist2->type == PLIST &&
		( loc2->type == CELLCOLCONCAT || loc2->type == COLCONCAT )) 
	    {
	      /* merge the two cellcolconcat */
	      if (nsp_parse_append(&loc1,&loc2->next) == FAIL) return(FAIL);
	      loc1->arity += loc2->arity;
	      /* free unused part of loc2 */
	      loc2->next = NULL;
	      loc2->arity= 0;
	      nsp_plist_destroy(&plist2);
	    }
	  else 
	    {
	      if (nsp_parse_add_list(&loc1,&plist2) == FAIL) return(FAIL);
	      loc1->arity++;
	    }
	}
      else 
	{
	  if (nsp_parse_add_list(&plist1,&plist2) == FAIL) return(FAIL);
	  if (nsp_parse_add(&plist1,colconcat,2,T->tokenv.Line) == FAIL) return(FAIL);
	  plist2=plist1;
	  if (nsp_parse_add_list1(&plist1,&plist2) == FAIL) return(FAIL);
	}
    }
  nsp_parse_add_list(plist,&plist1);
  return(OK);
}

/**
 * parse_colcells:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @stop: 
 * 
 * 
 * 
 * Returns: %OK or %FAIL
 **/

static int parse_colcells(Tokenizer *T,NspBHash *symb_table,PList *plist,char stop)
{
  return parse_nary_flat_opt(T,symb_table,plist,parse_rowcells,IsColMatOp,"matrix",stop,FALSE);
}

/**
 * parse_cells:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @stop: 
 * 
 * 
 * 
 * Returns: %OK or %FAIL
 **/

static int parse_cells(Tokenizer *T,NspBHash *symb_table,PList *plist,char stop)
{
  int emptymat = (stop == ']') ? EMPTYMAT : EMPTYCELL;
  int type  = (stop == ']') ? P_MATRIX : P_CELL ;
  if (T->tokenv.NextC == stop ) 
    {
      /* empty matrix */
      if ( T->NextToken(T) == FAIL) return(FAIL);
      if ( T->NextToken(T) == FAIL) return(FAIL);
      if (nsp_parse_add(plist,emptymat,0,T->tokenv.Line) == FAIL) return(FAIL);
      return(OK);
    }
  if (T->tokenv.NextC == '\n') 
    {
      /* swallow return at begining of matrix definition */
      if ( T->NextToken(T) == FAIL) return(FAIL);
    }
  if ( T->NextToken(T) == FAIL) return(FAIL);

  /*  parse_nblines(T); */
  if (parse_nary_flat_opt(T,symb_table,plist,parse_colcells,IsDiagMatOp,"matrix",stop,FALSE)== FAIL) 
    return(FAIL);
  /*  parse_nblines(T); */
  if ( T->tokenv.id != stop )  
    {
      T->ParseError(T,"Parse Error while parsing a matrix: expecting a `%c'  \n",stop);
      return(FAIL);
    }
  if (nsp_parse_add(plist,type,1,T->tokenv.Line) == FAIL) return(FAIL);
  if ( T->NextToken(T) == FAIL) return(FAIL);
  return(OK);
}

/**
 * func_or_matrix_with_arg:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @id: 
 * @excnt: 
 * @fblank: 
 * @end_char: 
 * 
 * Parses (<equal>,<equal>,<equal>) or [<equal>,<equal>,...]
 *       or {<equal>,<equal>,<equal>}
 * zero or more <equal> 
 * at first entry in the routine : T->tokenv.id == '(' or '[' or '{'
 * (fblank is a flag which is set to one is \n are accepted 
 *    between <equal>,<equal>
 * end_char is the character which finish the parse ')' or ']'
 * 
 * 
 * Returns: %OK or %FAIL
 **/

static int func_or_matrix_with_arg(Tokenizer *T,NspBHash *symb_table,PList *plist, char *id, int *excnt, int fblank, char end_char)
{
  int t_id = T->tokenv.id; 
  PList plist1 = NULLPLIST ;
  /* arg1,...,argn  parsing the calling list */
  if ( T->NextToken(T) == FAIL) return(FAIL);
  if ( fblank == 1)
    {
      if ((( t_id == '(' ) ? parse_nblines_and_comments(T) : parse_nblines(T)) == FAIL) return FAIL;
    }
  while ( T->tokenv.id != end_char ) 
    {
      plist1= NULLPLIST;
      if (parse_equal(T,symb_table,&plist1,1) == FAIL ) return (FAIL);
      if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
      if ( fblank == 1)
	{
	  if ((( t_id == '(' ) ? parse_nblines_and_comments(T) : parse_nblines(T)) == FAIL) return FAIL;
	}
      switch ( T->tokenv.id ) 
	{
	case COMMA_OP :
	case COMMA_RET_OP :
	  ++(*excnt);       
	  if ( T->NextToken(T) == FAIL) return(FAIL);
	  if ((( t_id == '(' ) ? parse_nblines_and_comments(T) : parse_nblines(T)) == FAIL) return FAIL;
	  if ( T->tokenv.id == end_char ) 
	    {
	      T->ParseError(T,"Parse Error: extra , or missing expression before %c\n",end_char);
	      return(FAIL);
	    }
	  break;
	case ']' : 
	case '}' : 
	case ')' : ++(*excnt);       
	  break;
	default:  
	  T->ParseError(T,"Parse Error: waiting for %c \n",end_char);
	  return(FAIL);
	}
    }
  if ( T->NextToken(T) == FAIL) return(FAIL);
  if (debug) Sciprintf("f(args:%d)",(*excnt));
  /* (*excnt) counts the arguments + the function name f(a,b)==> (*excnt)=3*/
  return(OK);
}

/**
 * Check_Func_Def:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * 
 * 
 * Check the result of func_or_matrix_with_arg
 * for function definition 
 * i.e check if plist is f(x1,...,xn,y1=..,y2=,..)
 * and insert symbols in symb_table
 * 
 * Returns: %OK or %FAIL
 **/

static int Check_Func_Def(Tokenizer *T,NspBHash *symb_table,PList plist)
{
  char *name;
  int type,count=0;
#ifdef  WITH_SYMB_TABLE
  int val=0;
#endif
  if ( plist == NULLPLIST ) return FAIL;
  if ( plist->type != FEVAL ) return FAIL;
  plist=plist->next; /* pass FEVAL*/
  type = plist->type ;   /* type NAME */
  name = (char *) plist->O; /* function name */
  /* jump function name */
  plist = plist->next ; count++;
  /* walk on names */
  while ( plist != NULLPLIST) 
    {
      if ( plist->type != type ) break;
#ifdef  WITH_SYMB_TABLE
      if ( nsp_bhash_find(symb_table,(char *) plist->O,&val) == FAIL) 
	{
	  if (nsp_bhash_enter(symb_table,(char *) plist->O,0) == FAIL) return FAIL;
	}
#endif 
      plist = plist->next ;
      count++;
    }
  /* now check for optional arguments */
  while ( plist != NULLPLIST ) 
    {
      PList Loc,Loc1;
      if ( plist->type != PLIST ) 
	{
	  T->ParseError(T,"Parse Error: Incorrect %d argument in %s(..)\n"
			,count,name);
	  return FAIL;
	}
      Loc1= (PList) plist->O;
      Loc = (Loc1->arity > 0) ? Loc1->next : Loc1;
      switch ( Loc->type ) 
	{
	case NAME : 
	case OPNAME : 
	case OPT: 
#ifdef  WITH_SYMB_TABLE
	  if ( nsp_bhash_find(symb_table,(char *) Loc->O,&val) == FAIL) 
	    {
	      if (nsp_bhash_enter(symb_table,(char *) Loc->O,0) == FAIL) return FAIL;
	    }
#endif
	  break;
	default : 
	  T->ParseError(T,"Parse Error: Incorrect %d argument in %s(...)\n"
			,count,name);
	  return FAIL;
	  break;
	}
      plist = plist->next ;
      count++;
    }
  return OK;
}

/**
 * Check_Func_Call:
 * @T: a #Tokenizer  
 * @plist: a #PList
 * @tag: 
 * 
 * Check the result of func_or_matrix_with_arg
 * for function call or object extraction 
 * i.e check if plist is f(exp1,....expn,y1=..,y2=,..)
 * Checks that named optional arguments are at the end
 * 
 * 
 * Returns: %OK or %FAIL
 **/

static int Check_Func_Call(Tokenizer *T,PList plist, int tag)
{
  int count=0;
  if ( plist == NULLPLIST ) return FAIL;
  if ( plist->type != tag)  return FAIL;
  plist = plist->next ;
  while ( plist  != NULLPLIST ) 
    {
      if ( plist->type == PLIST )
	{
	  PList Loc,Loc1;
	  Loc1 = (PList) plist->O;
	  Loc = (Loc1->arity > 0) ? Loc1->next : Loc1;
	  if (( Loc->type == NAME || Loc->type == OPNAME) && Loc1->type == OPT ) 
	    break;
	}
      plist = plist->next ;
      count++;
    }
  /* now check for optional arguments */
  while ( plist != NULLPLIST ) 
    {
      PList Loc,Loc1;
      if ( plist->type != PLIST ) 
	{
	  T->ParseError(T,"Parse Error: Incorrect %s (It should be given as x=val).\n"
			,ArgPosition(count+1));
	  return FAIL;
	}
      Loc1 = (PList) plist->O;
      Loc = (Loc1->arity > 0) ? Loc1->next : Loc1;
      if ( !(Loc->type == NAME|| Loc->type == OPNAME) || Loc1->type != OPT ) 
	{
	  T->ParseError(T,"Parse Error: Incorrect %s argument (It should be given as x=val).\n"
			,ArgPosition(count+1));
	  return FAIL;
	}
      plist = plist->next ;
      count++;
    }
  return OK;
}

/**
 * nsp_parse_add_to_symbol_table:
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 *
 * called when PList is a MLHS 
 * symbols in MLHS are to be inserted in symbol_table.
 * XXX this is unfinished ...
 * 
 * Return value: %OK or %FAIL
 **/

static int nsp_parse_add_to_symbol_table(NspBHash *symb_table,PList L)
{
  PList L1;
  int val ;
  /* NspObject *obj; */
  if ( symb_table == NULLBHASH ) return OK;
  while ( L != NULLPLIST ) 
    {
      switch ( L->type ) 
	{
	case NAME :
	  /* Sciprintf("%s",(char *) L->O); */
	  if ( nsp_bhash_find(symb_table,(char *) L->O,&val) == FAIL) 
	    {
	      if (nsp_bhash_enter(symb_table,(char *) L->O,0) == FAIL) return FAIL;
	    }
	  break;
	case PLIST: 
	  L1 = (PList) L->O;
	  if ( L1->type == CALLEVAL ||  L1->type == LISTEVAL) 
	    {
	      /* Sciprintf("[%s]",(char *) (L1->next->O)); */
	      if ( nsp_bhash_find(symb_table,(char *)(L1->next->O),&val) == FAIL) 
		{
		  if (nsp_bhash_enter(symb_table,(char *)(L1->next->O) ,0) == FAIL) return FAIL;
		}
	    }
	  break;
	}
      L = L->next ;
    }
  return OK;
}

/**
 * nsp_parse_symbols_table_set_id:
 * @symb_table: 
 * 
 * associate an id (integer) to each symbol of the symbol table.  
 * This function also adds nargin, nargout, nargopt and ans in the symbol table.
 * Return value: the number of ids (Note that 0 is not used it is a reserved id).
 **/

#ifdef  WITH_SYMB_TABLE 
static int nsp_parse_symbols_table_set_id(NspBHash *symb_table) 
{
  /* NspObject *Obj; */
  int i = 0,count=1;
  if ( symb_table == NULLBHASH) return 0;
  /* add default entries 
   * do not change the ordering which is used in 
   * frame_insert_var(int rhs,int opt,int lhs)
   */
  nsp_bhash_enter(symb_table,"ans",count++); /* 1 */
  nsp_bhash_enter(symb_table,"nargin",count++); /* 2 */
  nsp_bhash_enter(symb_table,"nargout",count++); /* 3 */
  nsp_bhash_enter(symb_table,"nargopt",count++); /* 4 */
  while (1) 
    {
      char *str=NULL;
      int val;
      int rep = nsp_bhash_get_next_object(symb_table,&i,&str,&val);
      if ( str != NULL && val == 0)
	{ 
	  nsp_bhash_enter_pos_i(symb_table,i-1,count++);
	}
      if (rep == FAIL) break;
    }
  return count;
}

#ifdef  SMAT_SYMB_TABLE
static void nsp_parse_symbols_table_reset_id(NspBHash *symb_table,NspSMatrix *S) 
{
  int i;
  if ( symb_table == NULLBHASH) return;
  for ( i = 0 ; i < S->mn ; i++)
    {
      nsp_bhash_enter(symb_table,S->S[i],i+1) ;
    }
}
#endif

#endif 






/**
 * parse_nary:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @parsef: 
 * 
 * a generic function for parsing a nary operator 
 *  <xxx> op <xxx> op <xxx> 
 *  - each argument <xxx> is parsed with the given function parsef
 *  - operator is tested with function opfn 
 *  to see an example see parse_terme 
 *  expression is parsed using left to right associativity
 *  - note that '\n' can be added after operator 
 *
 * This function add at end of plist 
 * (((arg1 arg2 op) arg3 op) arg4 op)
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_nary(Tokenizer *T,NspBHash *symb_table,PList *plist, 
		      int (*parsef)(Tokenizer *T,NspBHash *symb_table,PList *plist),
		      int (*opfn)(Tokenizer *T,int *op), char *info)
{
  int op;
  PList plist1 = NULLPLIST ;
  if (debug) scidebug(debugI++,"[nary:%s>",info);
  if ( (*parsef)(T,symb_table,&plist1) == FAIL) return(FAIL);
  while( (*opfn)(T,&op) == OK )
    {
      PList plist2=NULLPLIST;
      if (debug) scidebug(debugI,"{%s}",nsp_astcode_to_name(op));
      /* forget the comments in a parse_nary  */
      if ( parse_nblines_and_comments(T)==FAIL) return FAIL;
      plist2=NULLPLIST;
      if ( (*parsef)(T,symb_table,&plist2) == FAIL ) return(FAIL);
      if (nsp_parse_add_list(&plist1,&plist2) == FAIL) return(FAIL);
      if (nsp_parse_add(&plist1,op,2,T->tokenv.Line) == FAIL) return(FAIL);
      plist2=plist1;
      if (nsp_parse_add_list1(&plist1,&plist2) == FAIL) return(FAIL);
    }
  if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
  if (debug) scidebug(--debugI,"<nary:%s]",info);
  return(OK);
} 

/**
 * parse_nary_opt:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @parsef: 
 * 
 * almost parse_nary but the parse function 
 * transmited has an extra argument 
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_nary_opt(Tokenizer *T,NspBHash *symb_table,PList *plist, 
			  int (*parsef)(Tokenizer *T,NspBHash *symb_table,PList *plist,char c),
			  int (*opfn)(Tokenizer *T,int *op,char c), char *info,char opt)
{
  int op;
  PList plist1 = NULLPLIST ;
  if (debug) scidebug(debugI++,"[naryopt:%s>",info);
  if ( (*parsef)(T,symb_table,&plist1,opt) == FAIL) return(FAIL);
  while( (*opfn)(T,&op,opt) == OK )
    {
      PList plist2=NULLPLIST;
      int op_line = T->tokenv.Line;
      if (debug) scidebug(debugI,"{%s}",nsp_astcode_to_name(op));
      if ( parse_nblines_and_comments(T)==FAIL) return FAIL;
      plist2=NULLPLIST;
      if ( (*parsef)(T,symb_table,&plist2,opt) == FAIL ) return(FAIL);
      if (nsp_parse_add_list(&plist1,&plist2) == FAIL) return(FAIL);
      if (nsp_parse_add(&plist1,op,2,op_line) == FAIL) return(FAIL);
      plist2=plist1;
      if (nsp_parse_add_list1(&plist1,&plist2) == FAIL) return(FAIL);
    }
  if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
  if (debug) scidebug(--debugI,"<naryopt:%s]",info);
  return(OK);
} 


/**
 * parse_nary_flat_opt:
 * @T: a #Tokenizer 
 * @symb_table: a #NspBHash 
 * @plist: a #PList
 * @parsef: 
 * 
 * like the previous one but the 
 * returned parsed list is of the form 
 * (op arg1 ...  argn)
 * can be used when op is unique 
 * Note:  in this case int (*opfn)(Tokenizer *T,int *op,char c) 
 *        must always return the correct expected op
 * 
 * Returns:  %OK or %FAIL
 **/

static int parse_nary_flat_opt(Tokenizer *T,NspBHash *symb_table,PList *plist, 
			       int (*parsef)(Tokenizer *T,NspBHash *symb_table,PList *plist,char c),
			       int (*opfn)(Tokenizer *T,int *op,char c), char *info,char opt,int flag)
{
  int count = 1;
  int op;
  PList plist1 = NULLPLIST ;
  if (debug) scidebug(debugI++,"[%s>",info);
  if ( (*parsef)(T,symb_table,&plist1,opt) == FAIL) return(FAIL);
  /* op will be correct even if the first call returns FAIL*/
  while( (*opfn)(T,&op,opt) == OK )
    {
      PList plist2=NULLPLIST;
      if (debug) Sciprintf("-arg-");
      if ( parse_nblines(T) == FAIL ) return FAIL;
      plist2=NULLPLIST;
      if ( (*parsef)(T,symb_table,&plist2,opt) == FAIL ) return(FAIL);
      if (nsp_parse_add_list(&plist1,&plist2) == FAIL) return(FAIL);
      count++;
    }
  if (debug) Sciprintf("{t_op:%d}",op);
  if ( count == 1 ) 
    {
      if (flag)
	{
	  if (nsp_parse_add(&plist1,op,count,T->tokenv.Line) == FAIL) return(FAIL);
	}
    }
  else if ( count > 1)
    {
      if (nsp_parse_add(&plist1,op,count,T->tokenv.Line) == FAIL) return(FAIL);
    }
  if (nsp_parse_add_list(plist,&plist1) == FAIL) return(FAIL);
  if (debug) scidebug(--debugI,"<%s]",info);
  return(OK);
} 
