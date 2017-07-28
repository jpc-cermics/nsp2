/* -*- Mode: C -*- */
/* Nsp
 * Copyright (C) 1998-2017 Jean-Philippe Chancelier Enpc/Cermics
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
 * generic pretty printer for NspAst and PList ast representations
 */

#include <ctype.h>
#include <nsp/nsp.h>
#include <nsp/objects.h>
#include <nsp/plist.h> 
#include <nsp/plistc.h> 
#include <nsp/parse.h>
#include <nsp/ast.h>
#include <nsp/interf.h>
#include <nsp/seval.h>

/* exported functions
 * nsp_plist_generic_pretty_printer
 * nsp_ast_generic_pretty_printer
 * nsp_ast_printlength
 */

/* local code */

/* used to select a target when printing */

typedef enum { nsp_pprint_html=1 , nsp_pprint_gtk=2, nsp_pprint_latex=3, nsp_pprint_term=4 } nsp_pprint_target;

typedef struct _ast_wrap ast_wrap;

struct _ast_wrap {
  int (*get_op)(ast_wrap *ast);
  int (*get_arg)(ast_wrap *ast,int j,ast_wrap *arg);
  int (*get_length_args)(ast_wrap *ast);
  int (*get_arity)(ast_wrap *ast);
  int (*get_line)(ast_wrap *ast);
  char * (*get_str)(ast_wrap *ast);
  void *ast;
  nsp_pprint_target target;
  int use_sep_space;
  int use_color;
  int columns;
};

static int _nsp_ast_pprint_one_line_statements(ast_wrap *ast,int elt, int offset);
static void nsp_print_string_as_read_for_html(const char *str, char string_delim, int target);
static int _nsp_ast_pprint_check_newline(ast_wrap *ast,int elt,int pos, int offset);
static void nsp_print_comment_for_html(const char *str,int target);
static int _nsp_ast_pprint_arg_comment_ended(ast_wrap *ast,int elt);
static int _nsp_ast_pprint_op_comment_ended(ast_wrap *ast, int elt);
static int _nsp_ast_printlength_arg(ast_wrap *ast,int elt, int indent, int pos, int posret);
static int _nsp_ast_pprint(ast_wrap *ast, int indent, int pos, int posret, int tag);
static int _nsp_ast_pprint_arg(ast_wrap *ast,int elt, int i, int pos, int posret, int tag);
static int nsp_ast_pprint_opname(ast_wrap *ast, int indent, int pos, int pre,int post);
static int _nsp_ast_pprint_args(ast_wrap *ast, int start, int last, int indent, int pos,
				int posret, char *sep, int breakable,const char *breakstr);
static int _nsp_latin1_character_to_html(char c, char *str, int target);
static int _nsp_ast_equalop_mlhs_length(ast_wrap *ast);
static void nsp_ast_to_ast_wrap(NspAst *ast, ast_wrap *astwrap);
static void nsp_plist_to_ast_wrap(PList ast, ast_wrap *astwrap);
static int _nsp_ast_printlength(ast_wrap *ast, int indent, int pos, int posret);
static char *_nsp_ast_get_space(ast_wrap *ast);
static int _nsp_ast_pprint_statements(ast_wrap *ast, int start, int last, int indent, int pos, 
				      int posret, char *sep, int breakable, const char *breakstr);
static int _nsp_ast_pprint_statements1(ast_wrap *ast, int start, int last, int indent, int pos, 
				       int posret, char *sep, int breakable, const char *breakstr);

  
void nsp_ast_generic_pretty_printer(NspAst *ast, int indent, int color, int target, int space, int columns)
{
  ast_wrap loc;
  nsp_ast_to_ast_wrap(ast, &loc);
  loc.target  = target;
  loc.use_sep_space = space;
  loc.use_color = color;
  loc.columns = columns;
  _nsp_ast_pprint(&loc,indent,0,indent,0);
}

int nsp_ast_printlength(NspAst *ast, int indent)
{
  ast_wrap loc;
  nsp_ast_to_ast_wrap(ast, &loc);
  return _nsp_ast_printlength(&loc,indent,0,indent);
}

void nsp_plist_generic_pretty_printer(PList ast, int indent, int color,int target, int space, int columns)
{
  ast_wrap loc;
  nsp_plist_to_ast_wrap(ast, &loc);
  loc.target  = target;
  loc.use_sep_space = space;
  loc.use_color = color;
  loc.columns = columns;
  _nsp_ast_pprint(&loc,indent,0,indent,0);
}

/* set of colors that can be used in a terminal 
 * if 3 is replaced by 9 we obtain light colors.
 */

typedef enum { p_black =30, 
               p_red =31,
               p_green=32,
               p_yellow=33,
               p_blue=34,
               p_purple=35,
               p_cyan=36,
               p_white=37} nsp_term_colors;

typedef enum { type_comment,
	       type_string,
	       type_keyword,
	       type_number,
	       type_fname,
               type_operator} nsp_colorized_types;

/* used in a terminal */

static int color_from_key( nsp_colorized_types key)
{
  switch (key)
    {
    case type_comment: return p_blue;break;
    case type_string:  return p_red;break;
    case type_keyword: return p_green;break;
    case type_number:  return p_purple;break;
    case type_fname:   return p_blue;break;
    case type_operator:return p_black;break;
    }
  return p_black;
}

/* used in gtk (textview) */

static char* colorname_from_key( nsp_colorized_types key)
{
  switch (key)
    {
    case type_comment: return "blue";break;
    case type_string:  return "red";break;
    case type_keyword: return "green";break;
    case type_number:  return "purple";break;
    case type_fname:   return "blue";break;
    case type_operator:return "black";break;
    }
  return "black";
}

/* used for latex or html, real colors are defined elsewhere */

static char *colortag_from_key( nsp_colorized_types key)
{
  switch (key)
    {
    case type_comment: return "comment";break;
    case type_string:  return "string";break;
    case type_keyword: return "keyword";break;
    case type_number:  return "number";break;
    case type_fname:   return "function";break;
    case type_operator:   return "operator";break;
    }
  return "undefined";
}

static int nsp_ast_pprint_pre_tag_color(ast_wrap *ast, nsp_colorized_types key)
{
  if ( ast->use_color == TRUE )
    {
      switch (ast->target)
	{
	case nsp_pprint_html: Sciprintf("<span class=\"%s\">",colortag_from_key(key));break;
	case nsp_pprint_gtk:  Sciprintf("<span color=\"%s\">",colorname_from_key(key));break;
	case nsp_pprint_latex: Sciprintf("\\nsp%s{",colortag_from_key(key));break;
	case nsp_pprint_term: Sciprintf("\033[%dm",color_from_key(key));break;
	}
    }
  return 0;
}

static int nsp_ast_pprint_post_tag_color(ast_wrap *ast)
{
  if (ast->use_color == TRUE )
    {
      switch (ast->target)
	{
	case nsp_pprint_html:Sciprintf("</span>");break;
	case nsp_pprint_gtk:Sciprintf("</span>");break;
	case nsp_pprint_latex:Sciprintf("}");break;
	case nsp_pprint_term:Sciprintf("\033[0m");break;
	}
    }
  return 0;
}

static int nsp_ast_pprint_comment(int indent,ast_wrap *ast)
{
  const char *str = (const char *) ast->get_str(ast);
  int escape = ( strlen(str) >= 2 && *str == '@' && *(str+1) == '@' );
  int pos=0;
  pos += Sciprintf2(indent, _nsp_ast_get_space(ast),"");
  if (! escape) pos += nsp_ast_pprint_pre_tag_color(ast,type_comment);

  switch (ast->target)
    {
    case nsp_pprint_html:nsp_print_comment_for_html(str,ast->target);pos += strlen(str)+2;break;
    case nsp_pprint_gtk:nsp_print_comment_for_html(str,ast->target);pos += strlen(str)+2;break;
    case nsp_pprint_latex:nsp_print_comment_for_html(str,ast->target);pos += strlen(str)+2;break;
    case nsp_pprint_term: pos += Sciprintf("//%s",str);break;
    }
  if (! escape)  pos += nsp_ast_pprint_post_tag_color(ast);
  return pos;
}

static void nsp_print_comment_for_html(const char *str, int target)
{
  int escape = ( strlen(str) >= 2 && *str == '@' && *(str+1) == '@' );
  if ( ! escape )
    Sciprintf("//");
  else
    str = str+2;
  while ( *str != '\0') 
    {
      char buf[64];
      if ( _nsp_latin1_character_to_html(*str,buf,target))
	{
	  Sciprintf("%s",buf);
	}
      else
	{
	  if ( escape )
	    {
	      Sciprintf("%c",*str);
	    }
	  else
	    if ( target == nsp_pprint_latex)
	      {
		switch (*str) 
		  {
		  case '\\': Sciprintf("%s","\\nspbs{}");break;
		  case '%' : Sciprintf("%s","\\nsppc{}");break;
		  case '#' : Sciprintf("%s","\\nspsh{}");break;
		  case '~' : Sciprintf("%s","\\nspti{}");break;
		  case '{' : Sciprintf("%s","\\nspob{}");break;
		  case '}' : Sciprintf("%s","\\nspcb{}");break;
		  default :  Sciprintf("%c",*str);
		  }
	      }
	    else
	      {
		switch (*str) 
		  {
		  case '<' : Sciprintf("%s","&lt;");break;
		  case '>' : Sciprintf("%s","&gt;");break;
		  case '&' : Sciprintf("%s","&amp;");break;
		  default :  Sciprintf("%c",*str);
		  }
	      }
	}
      str++;
    }
}

static int nsp_ast_pprint_name(int indent, ast_wrap *ast)
{
  const char *name = ast->get_str(ast);
#ifdef WITH_SYMB_TABLE_DEBUG
  return Sciprintf2(indent,_nsp_ast_get_space(ast),"%s<%d>",name,ast->get_arity(ast));
#else 
  return Sciprintf2(indent,_nsp_ast_get_space(ast),"%s",name);
#endif 
}

static int nsp_ast_pprint_string(int indent, ast_wrap *ast)
{
  const char *str = (const char *) ast->get_str(ast);
  int flag = ast->get_arity(ast); /* used as delimiter flag */
  int pos=0;
  if ( flag == 2 )
    {
      /* special case for pause, help, cd, .... */
      pos += Sciprintf2(indent, _nsp_ast_get_space(ast),"%s",str);
      return pos;
    }
  const char string_delim = (flag==0) ? '\'': '\"';
  pos += Sciprintf2(indent, _nsp_ast_get_space(ast),"");
  pos += nsp_ast_pprint_pre_tag_color(ast,type_string);
  pos += 2 + strlen(str);

  switch (ast->target)
    {
    case nsp_pprint_html:nsp_print_string_as_read_for_html(str,string_delim,ast->target);break;
    case nsp_pprint_gtk:nsp_print_string_as_read_for_html(str,string_delim,ast->target);break;
    case nsp_pprint_latex:nsp_print_string_as_read_for_html(str,string_delim,ast->target);break;
    case nsp_pprint_term:nsp_print_string_as_read(str,string_delim);break;
    }
  pos += nsp_ast_pprint_post_tag_color(ast);
  return pos;
}

static void nsp_print_string_as_read_for_html(const char *str,char string_delim, int target)
{
  const char *bs = ( target ==  nsp_pprint_latex ) ? "\\nspbs{}" : "\\";
  Sciprintf("%c",string_delim);
  while ( *str != '\0') 
    {
      char buf[64];
      if ( _nsp_latin1_character_to_html(*str,buf,target))
	{
	  Sciprintf("%s",buf);
	}
      else
	{
	  switch (*str) 
	    {
	    case '%' :
	      if ( target ==  nsp_pprint_latex )
		Sciprintf("%s","\\nsppc{}");
	      else Sciprintf("%c",*str);break;
	    case '#' :
	      if ( target ==  nsp_pprint_latex )
		Sciprintf("%s","\\nspsh{}");
	      else Sciprintf("%c",*str);break;
	    case '~' :
	      if ( target ==  nsp_pprint_latex )
		Sciprintf("%s","\\nspti{}");
	      else Sciprintf("%c",*str);break;
	    case '{' :
	      if ( target ==  nsp_pprint_latex )
		Sciprintf("%s","\\nspob{}");
	      else Sciprintf("%c",*str);break;
	    case '}' :
	      if ( target ==  nsp_pprint_latex )
		Sciprintf("%s","\\nspcb{}");
	      else Sciprintf("%c",*str);
	      break;
	    case '\'' :  Sciprintf("%s","''");break;
	    case '\"' :  Sciprintf("%s","\"\"");break;
	    case '\\' :
	      if ( isdigit(*(str+1)) )
		{
		  Sciprintf("%s%s",bs,bs);
		}
	      else
		{
		  switch (*(str+1)) 
		    {
		    case 'a' :
		    case 'b' : 
		    case 'f' :  
		    case 'n' :  
		    case 'r' :  
		    case 't' :  
		    case 'v' :  
		    case '\a' :  
		    case '\b' : 
		    case '\f' : 
		    case '\n' : 
		    case '\r' : 
		    case '\t' : 
		    case '\v' :
		      Sciprintf("%s%s",bs,bs);break;
		    default: Sciprintf("%s",bs);break;
		    }
		}
	      break;
	    case '\a' :  Sciprintf("%s%s",bs,"a"); break;
	    case '\b' :  Sciprintf("%s%s",bs,"b"); break;
	    case '\f' :  Sciprintf("%s%s",bs,"f"); break;
	    case '\n' :  Sciprintf("%s%s",bs,"n"); break;
	    case '\r' :  Sciprintf("%s%s",bs,"r"); break;
	    case '\t' :  Sciprintf("%s%s",bs,"t"); break;
	    case '\v' :  Sciprintf("%s%s",bs,"v"); break;
	    default: 
	      if (isprint(*str)) 
		{
		  switch (*str ) {
		  case '<' : Sciprintf("%s","&lt;");break;
		  case '>' : Sciprintf("%s","&gt;");break;
		  default:
		    Sciprintf("%c",*str);
		  }
		}
	      else 
		{
		  unsigned char c = *str;
		  Sciprintf("%s%0.3o",bs,c);
		}
	    }
	}
      str++;
    }
  Sciprintf("%c",string_delim);
}

static int nsp_ast_pprint_number(int indent,ast_wrap *ast)
{
  const char *str = (const char *) ast->get_str(ast);
  int pos=0;
  pos += Sciprintf2(indent, _nsp_ast_get_space(ast),"");
  pos += nsp_ast_pprint_pre_tag_color(ast,type_number);
  pos += Sciprintf("%s",str);
  if ( ast->use_sep_space == FALSE )
    {
      int len = strlen(str);
      if ( len > 0 && str[len-1] == '.' )
	pos += Sciprintf("%s", _nsp_ast_get_space(ast) );
    }
  pos += nsp_ast_pprint_post_tag_color(ast);
  return pos;
}

static int nsp_ast_pprint_keyword(int indent,ast_wrap *ast,const char *str)
{
  int pos=0;
  pos += Sciprintf2(indent, _nsp_ast_get_space(ast),"");
  pos += nsp_ast_pprint_pre_tag_color(ast,type_keyword);
  pos += Sciprintf("%s",str);
  pos += nsp_ast_pprint_post_tag_color(ast);
  return pos;
}

/* print operator name after @indent blank characters 
 * and return the new position.
 * pos is giving the current position.
 */

static int nsp_ast_pprint_opname(ast_wrap *ast, int indent, int pos, int pre,int post)
{
  int seps=0, force_space = FALSE;
  const char *space = _nsp_ast_get_space(ast);
  int type = ast->get_op(ast);
  const char *s = nsp_astcode_to_name(type);
  const char *s1= s;
  //Sciprintf("opname [%s] [%s] [%d] [%d]\n",s,space,pre,post);
  switch (ast->target)
    {
    case nsp_pprint_html:
    case nsp_pprint_gtk:
      if ( strcmp(s,">") == 0)
	{ s1 = "&gt;"; }
      else if ( strcmp(s,"<") == 0)
	{s1 = "&lt;"; force_space  = TRUE;}
      else if ( strcmp(s,"<=") == 0)
	{s1 = "&lt;=";}
      else if ( strcmp(s,">=") == 0)
	{s1 = "&gt;=";}
      else if ( strcmp(s,"<>") == 0)
	{s1 = "&lt;&gt";}
      else if ( strcmp(s,"~") == 0 )
	{s1 = s;}
      else if ( strcmp(s,"&") == 0)
	{ s1 ="&amp;";}
      else if ( strcmp(s,"&&") == 0)
	{ s1 ="&amp;&amp;";}
      break;
    case nsp_pprint_latex:
      if ( strcmp(s,"\\") == 0) s1 = "\\nspbs{}";
      else if ( strcmp(s,".\\") == 0) s1 = ".\\nspbs{}";
      else if ( strcmp(s,"\\.") == 0) s1 = "\\nspbs{}.";
      else if ( strcmp(s,".\\.") == 0) s1 = ".\\nspbs{}.";
      break;
    case nsp_pprint_term: break;
    }
  if ( strcmp(s,">") == 0 ||
       strcmp(s,"<") == 0 ||
       strcmp(s,"<=") == 0||
       strcmp(s,">=") == 0||
       strcmp(s,"<>") == 0||
       strcmp(s,"&") == 0||
       strcmp(s,"&&") == 0||
       strcmp(s,"|") == 0||
       strcmp(s,"||") == 0)
    {
      force_space=TRUE;
    }

  nsp_ast_pprint_pre_tag_color(ast,type_operator);
  if ( force_space || ( pre && ( ast->use_sep_space || (strlen(s1) > 0 && s1[0]== '.'))))
    {Sciprintf("%s",space); seps++;}
  Sciprintf2(indent, space ,"%s",s1);
  if ( force_space || ( post && ast->use_sep_space ))
    {Sciprintf("%s",space);seps++;}
  nsp_ast_pprint_post_tag_color(ast);
  return pos + indent + seps + strlen(s);
}

/* posret: indentation to use if line-break. 
 *
 */

static int _nsp_ast_pprint(ast_wrap *ast, int indent, int pos, int posret, int tag1)
{
  const char *s;
  int j,newpos=0, arity=0;

  if ( pos < posret ) 
    {
      /* be sure that we are starting to print at least at column posret */
#ifdef WITH_SYMB_TABLE_DEBUG
      pos=Sciprintf2(posret-pos,_nsp_ast_get_space(ast),"");
#else
      if ( ast->get_op(ast) != OBJECT )
	pos=Sciprintf2(posret-pos, _nsp_ast_get_space(ast),"");
#endif
    }
  /* select print operation */
  if ( ast->get_op(ast) > 0 ) 
    {
      int len;
      /* operators **/
      switch ( ast->get_arity(ast) ) 
	{
	case 0: /* 0-ary operators */
	  return nsp_ast_pprint_opname(ast,indent,pos,0,0);
	  break;
	case 1:
	  switch ( ast->get_op(ast) ) 
	    {
	    case  COMMA_OP : 
	    case  SEMICOLON_OP  :
	      newpos =_nsp_ast_pprint_arg(ast,1,indent,pos,posret,0);
	      newpos = nsp_ast_pprint_opname(ast,0,newpos,0,0);
	      return newpos;
	      break;
	    case  COMMA_RET_OP : 
	    case  SEMICOLON_RET_OP  :
	      newpos =_nsp_ast_pprint_arg(ast,1,indent,pos,posret,0);
	      newpos = nsp_ast_pprint_opname(ast,0,newpos,0,0);
	      Sciprintf("%s","\n");newpos=0;
	      return newpos;
	      break;
	    case QUOTE_OP : 
	    case DOTPRIM:
	      newpos =_nsp_ast_pprint_arg(ast,1,indent,pos,posret,0);
	      newpos = nsp_ast_pprint_opname(ast,0,newpos,0,0);
	      return  newpos;
	      break;
	    case RETURN_OP : 
	      _nsp_ast_pprint_arg(ast,1,indent,pos,posret,0);
	      Sciprintf("%s","\n");newpos=0;
	      return newpos;
	      break;
	    case TILDE_OP : 
	    default:
	      newpos =nsp_ast_pprint_opname(ast,indent,pos,0,0);
	      newpos =_nsp_ast_pprint_arg(ast,1,0,newpos,posret,0);
	      return newpos;
	    }
	  break;
	case 2:
	  newpos =_nsp_ast_pprint_arg(ast,1,indent,pos,posret,0);
	  newpos =nsp_ast_pprint_opname(ast,0,newpos,1,1);
	  len =  _nsp_ast_printlength_arg(ast,2,0,newpos,posret);
	  if ( newpos > ast->columns || len > ast->columns )
	    {
	      /* we have the right to break line here 
	       * we indent on the next line with posret 
	       */
	      Sciprintf("%s"," ...\n");
	      newpos= Sciprintf2(posret,
				 _nsp_ast_get_space(ast),"");
	    }
	  newpos =_nsp_ast_pprint_arg(ast,2,0,newpos,posret,0);
	  return newpos;
	  break;
	default :
	  /* XXX make breaks as in the 2-ary ops */
	  newpos = pos;
	  for ( j = 0 ; j <  ast->get_arity(ast) ; j++ )
	    {
	      newpos =_nsp_ast_pprint_arg(ast,j+1,(j == 0) ? indent : 0,
					  newpos,posret,0);
	      if ( j != ast->get_arity(ast) -1 ) 
		{
		  newpos =nsp_ast_pprint_opname(ast,0,newpos,1,1);
		}
	    }
	  return newpos;
	  break;
	}
    }
  else 
    {
      switch ( ast->get_op(ast) ) 
	{
	case OPT:
	  /* val = value in a calling list */
	  newpos = _nsp_ast_pprint_arg(ast,1,indent,pos,posret,0);
	  newpos += Sciprintf(" = ");
	  newpos = _nsp_ast_pprint_arg(ast,2,0,newpos,posret,0);
	  return newpos;
	  break;
	case EQUAL_OP:
	  /* affectations */
	  newpos = _nsp_ast_pprint_arg(ast,1,indent,pos,posret,0);
	  if (  _nsp_ast_equalop_mlhs_length(ast) > 0 ) 
	    newpos += Sciprintf("=");
	  /* fix new return position after = */
	  newpos = _nsp_ast_pprint_arg(ast,2,0,newpos, newpos, tag1);
	  return newpos;
	  break;
	case MLHS  :
	  /* left hand side of an equality 
	   * we do not display the left and right bracket 
	   *  if arity is one 
	   */
	  newpos = pos +  Sciprintf2(indent, _nsp_ast_get_space(ast),
				     ( ast->get_arity(ast) > 1) ? "[" : "");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  if ( ast->get_arity(ast) > 1) newpos += Sciprintf("]");
	  return newpos;
	  break;
	case ARGS :
	  /* a sequence of expressions inside () for x()*/
	  newpos = pos +  Sciprintf2(indent, _nsp_ast_get_space(ast),"(");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  newpos += Sciprintf(")");
	  return newpos;
	  break;
	case CELLARGS :
	  {
	    const char *ob = (ast->target == nsp_pprint_latex) ? "\\nspob{}": "{";
	    const char *cb = (ast->target == nsp_pprint_latex) ? "\\nspcb{}": "}";
	    /* a sequence of expressions inside {} for x{} */
	    newpos = pos +  indent+1;
	    Sciprintf2(indent, _nsp_ast_get_space(ast), ob);
	    newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	    newpos += 1;
	    Sciprintf(cb);
	    return newpos;
	  }
	  break;
	case METARGS :
	  /* a sequence of expressions inside [] for x[] */
	  newpos = pos +  Sciprintf2(indent, _nsp_ast_get_space(ast),"[");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  newpos += Sciprintf("]");
	  return newpos;
	  break;
	case DOTARGS :
	  {
	    ast_wrap ast1;
	    if (ast->get_arg(ast,1,&ast1) == FAIL) return newpos;
	    if (ast1.get_op(&ast1) != STRING ) return newpos;
	    newpos = pos + Sciprintf2(0,_nsp_ast_get_space(ast),".%s",
				      (char *) ast1.get_str(&ast1));
	    return newpos;
	  }
	case CALLEVAL:
	case LISTEVAL :
	  newpos = pos +  Sciprintf2(indent, _nsp_ast_get_space(ast),"");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,"",FALSE,"");
	  return newpos;
	  break;
	case FEVAL :
	  if ( tag1 == 1) nsp_ast_pprint_pre_tag_color(ast, type_fname);
	  newpos =_nsp_ast_pprint_arg(ast,1,indent,pos,posret,0);
	  if ( tag1 == 1) nsp_ast_pprint_post_tag_color(ast);
	  newpos += Sciprintf("(");
	  newpos = _nsp_ast_pprint_args(ast,2,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  newpos += Sciprintf(")");
	  return newpos;
	  break;
	case PLIST :
	  /* such node should not appear here */
	  return newpos;
	  break;
	case COMMENT:
	  return pos+ nsp_ast_pprint_comment(indent,ast);
	  break;
	case NAME :
	  return pos+ nsp_ast_pprint_name(indent, ast);
	  break;
	case OPNAME :
	  return pos+Sciprintf2(indent,_nsp_ast_get_space(ast),"'%s'",
				(char *) ast->get_str(ast));
	  break;
	case NUMBER:
	  return pos + nsp_ast_pprint_number(indent,ast);
	  break;
	case STRING:
	  return pos + nsp_ast_pprint_string(indent, ast);
	  break;
	case INUMBER32 :
	case INUMBER64 :
	case UNUMBER32 :
	case UNUMBER64 :
	  return pos+  nsp_ast_pprint_number(indent,ast);
	  break;
	case OBJECT: 
#ifdef WITH_SYMB_TABLE_DEBUG      
	  n = pos+Sciprintf2(indent,_nsp_ast_get_space(ast),"{object:Ox%x}\n",
			     (unsigned int) ast->xobj);
	  nsp_object_print(ast->xobj,0,0,0);
	  return n;
#else 
	  return pos;
#endif
	  break;
	case EMPTYMAT:  return pos+Sciprintf2(indent, _nsp_ast_get_space(ast),"[]");break;
	case EMPTYCELL:
	  {
	    const char *obcb = (ast->target == nsp_pprint_latex) ? "\\nspob{}\\nspcb{}": "{}";
	    Sciprintf2(indent, _nsp_ast_get_space(ast), obcb);
	    return pos+indent+2;
	  }
	  break;
	case P_MATRIX :
	  newpos = pos + Sciprintf2(indent, _nsp_ast_get_space(ast),"[");
	  newpos =_nsp_ast_pprint_arg(ast,1,0,newpos,posret+1, 0 );
	  newpos += Sciprintf("]");
	  return newpos;
	  break;
	case P_CELL :
	  {
	    const char *ob = (ast->target == nsp_pprint_latex) ? "\\nspob{}": "{";
	    const char *cb = (ast->target == nsp_pprint_latex) ? "\\nspcb{}": "}";
	    newpos = pos + indent+1;
	    Sciprintf2(indent, _nsp_ast_get_space(ast), ob);
	    newpos =_nsp_ast_pprint_arg(ast,1,0,newpos,posret+1, 0);
	    newpos += 1;
	    Sciprintf(cb);
	    return newpos;
	  }
	  break;
	case ROWCONCAT:
	case COLCONCAT:
	case DIAGCONCAT:
	case CELLROWCONCAT:
	case CELLCOLCONCAT:
	case CELLDIAGCONCAT:
	  newpos = posret = pos;
	  for ( j = 0 ; j < ast->get_arity(ast) ; j++)
	    {
	      int is_comment,len;
	      /* get the length of child if the child is not cut 
	       * to decide if it's better to break here
	       */
	      len =  _nsp_ast_printlength_arg(ast,j+1,0,newpos,posret);
	      if ( ( j > 0) && ( newpos > ast->columns || len > ast->columns))
		{
		  if ( ast->get_op(ast) == ROWCONCAT || ast->get_op(ast) == CELLROWCONCAT)
		    Sciprintf("\n");
		  else
		    Sciprintf(" ...\n");
		  newpos= Sciprintf2(posret, _nsp_ast_get_space(ast),"");
		}
	      newpos =_nsp_ast_pprint_arg(ast,j+1,0,newpos,posret,0);
	      /* some child can be comment-ended, in that case a newline is requested */
	      is_comment =_nsp_ast_pprint_op_comment_ended(ast,j+1);
	      if ( j < ast->get_arity(ast)-1)
		{
		  if ( is_comment ) 
		    {
		      Sciprintf("\n");Sciprintf2(posret,_nsp_ast_get_space(ast),"");
		      newpos = posret;
		    }
		  else
		    {
		      newpos =nsp_ast_pprint_opname(ast,0,newpos,0,0);
		    }
		}
	    }
	  return newpos;
	  break;
	case WHILE:
	  newpos = pos + nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"while");
	  newpos =_nsp_ast_pprint_arg(ast,1,1,newpos,posret,0);
	  newpos += nsp_ast_pprint_keyword(1,ast,"do");
	  newpos =_nsp_ast_pprint_check_newline(ast,2,newpos,0);
	  newpos =_nsp_ast_pprint_arg(ast,2,0,newpos,posret+2, 0);
	  newpos = nsp_ast_pprint_keyword(Max(posret-newpos,0),ast,"end");
	  return newpos;
	  break;
	case FUNCTION:
	  newpos = pos + nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"function");
	  newpos = _nsp_ast_pprint_arg(ast,1,1,newpos,newpos, 1);
	  newpos = _nsp_ast_pprint_check_newline(ast,2,newpos,0);
	  newpos = _nsp_ast_pprint_arg(ast,2,0,newpos,posret+2, 0);
	  if ( ast->get_arity(ast) == 3 ) 
	    {
	      newpos =_nsp_ast_pprint_arg(ast,3,0,newpos,posret+2, 0);
	    }
	  newpos= nsp_ast_pprint_keyword(Max(posret-newpos,0),ast,"endfunction");
	  return newpos;
	  break;
	case FOR:
	  newpos = pos + nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"for");
	  newpos = _nsp_ast_pprint_arg(ast,1,1,newpos,posret,0);
	  newpos += Sciprintf("=") ;
	  newpos = _nsp_ast_pprint_arg(ast,2,0,newpos,newpos, 0);
	  newpos += nsp_ast_pprint_keyword(1,ast,"do");
	  /* check if we insert a newline after do or try to generate 
	   * a one line for 
	   */
	  newpos = _nsp_ast_pprint_check_newline(ast,3,newpos, 0);
	  newpos = _nsp_ast_pprint_arg(ast,3,0,newpos,posret+2, 0);
	  /* WIP: here: we should insert a newline before end if the one line for 
	   * option was rejected 
	   */
	  newpos += nsp_ast_pprint_keyword(Max(posret-newpos,0),ast,"end");
	  return newpos;
	  break;
	case IF:
	  /* a sequence of if elseif etc.... */
	  newpos = pos + nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"if");
	  for ( j = 0 ; j < ast->get_arity(ast)  ; j += 2 )
	    {
	      if ( j == ast->get_arity(ast)-1 ) 
		{
		  /* we have reached the last else **/
		  newpos +=  nsp_ast_pprint_keyword(Max(posret-newpos,0),ast,"else");
		  newpos =_nsp_ast_pprint_check_newline(ast,j+1,newpos, 0);
		  newpos =_nsp_ast_pprint_arg(ast,j+1,0,newpos,posret+2, 0);
		}
	      else 
		{ 
		  if ( j != 0) 
		    {
		      newpos +=nsp_ast_pprint_keyword(Max(posret-newpos,0),ast,"elseif");
		    }
		  newpos =_nsp_ast_pprint_arg(ast,j+1,1,newpos,posret+2, 0);
		  newpos += nsp_ast_pprint_keyword(1,ast,"then");
		  newpos =_nsp_ast_pprint_check_newline(ast,j+2,newpos, 0);
		  newpos =_nsp_ast_pprint_arg(ast,j+2,0,newpos,posret+2, 0);
		}
	    }
	  newpos +=  nsp_ast_pprint_keyword(Max(posret-newpos,0),ast,"end");
	  return newpos;
	  break;
	case TRYCATCH :
	  /* try catch sequence */
	  newpos = pos+ nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"try");
	  newpos =_nsp_ast_pprint_check_newline(ast,1,newpos, 0);
	  newpos =_nsp_ast_pprint_arg(ast,1,0,newpos,posret+2, 0);
	  newpos += nsp_ast_pprint_keyword(Max(posret-newpos,0),ast,"catch");
	  newpos =_nsp_ast_pprint_check_newline(ast,2,newpos, 0);
	  newpos =_nsp_ast_pprint_arg(ast,2,0,newpos,posret+2, 0);
	  if ( ast->get_arity(ast) == 3 ) 
	    {
	      newpos += nsp_ast_pprint_keyword(Max(posret-newpos,0),ast,"finally");
	      newpos =_nsp_ast_pprint_check_newline(ast,3,newpos, 0);
	      newpos =_nsp_ast_pprint_arg(ast,3,0,newpos,posret+2, 0);
	    }
	  newpos = nsp_ast_pprint_keyword(Max(posret-newpos,0),ast,"end");
	  return newpos;
	  break;
	case SELECT :
	  /* first argument is the test.
	   * next ones are cases 
	   */
	  arity = ast->get_arity(ast);
	  newpos = pos + nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"select");
	  for ( j = 0 ; j < arity ; j++)
	    {
	      if ( j==0) 
		{
		  _nsp_ast_pprint_arg(ast,j+1,1,newpos,posret,0);
		  Sciprintf("\n");newpos= 0;
		}
	      else
		{
		  newpos=_nsp_ast_pprint_arg(ast,j+1,0,newpos,posret+2, 0 );
		}
	    }
	  newpos = nsp_ast_pprint_keyword(Max(posret-newpos,0),ast,"end");
	  return newpos;
	  break;
	case STATEMENTS :
	  newpos=pos;
	  newpos= _nsp_ast_pprint_statements(ast,1,ast->get_arity(ast),0,newpos,posret,"",TRUE,"\n");
	  return newpos;
	  break;
	case STATEMENTS1 :
	  newpos=pos;
	  newpos= _nsp_ast_pprint_statements1(ast,1,ast->get_arity(ast),0,newpos,posret,"",TRUE,"\n");
	  return newpos;
	  break;
	case PARENTH :
	  newpos = pos + Sciprintf2(indent, _nsp_ast_get_space(ast),"(") ;
	  newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  newpos += Sciprintf(")");
	  return newpos;
	  break;
	case CASE : 
	  newpos = pos + nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"case");
	  newpos =_nsp_ast_pprint_arg(ast,1,1,newpos,posret+2, 0);
	  newpos += nsp_ast_pprint_keyword(1,ast,"then");
	  newpos = _nsp_ast_pprint_check_newline(ast,2,newpos, 1);
	  newpos =_nsp_ast_pprint_arg(ast,2,0,newpos,posret+1, 0);
	  return newpos;
	  break;
	case LASTCASE :
	  newpos = pos + nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"else");
	  newpos =_nsp_ast_pprint_check_newline(ast,1,newpos, 0);
	  newpos =_nsp_ast_pprint_arg(ast,1,0,newpos,posret+2, 0);
	  return newpos;
	  break;
	case GLOBAL:
	  /* n-ary global */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"global");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case CLEAR:
	  /* n-ary clear */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"clear");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case CLEARGLOBAL:
	  /* n-ary global */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"clearglobal");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case PAUSE:
	  /* can be 0 or 1-ary pause */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"pause");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case HELP:
	  /* 0 or  1-ary help */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"help");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case WHO:
	  /* 0 or 1-ary who */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"who");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case EXEC:
	  /* 1-ary exec */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"exec");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case APROPOS:
	  /* 1-ary apropos */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"apropos");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case CD_COMMAND:
	  /* 1-ary cd */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"cd");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case LS_COMMAND:
	  /* 1-ary ls */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0),ast,"ls");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case PWD_COMMAND:
	  /* 1-ary pwd */
	  newpos = nsp_ast_pprint_keyword(Max(posret-pos,0), ast, "pwd");Sciprintf(" ");
	  newpos = _nsp_ast_pprint_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;

	case BREAK: return pos+nsp_ast_pprint_keyword(indent,ast,"break");break;
	case PRETURN:  return pos+nsp_ast_pprint_keyword(indent,ast,"return"); break;
	case QUIT :   return pos+nsp_ast_pprint_keyword(indent,ast,"quit");   break;
	case NSP_EXIT :  return pos+nsp_ast_pprint_keyword(indent,ast,"exit");  break;
	case ABORT :  return pos+nsp_ast_pprint_keyword(indent,ast,"abort");  break;
	case CONTINUE : return pos+nsp_ast_pprint_keyword(indent,ast,"continue");  break;
	case WHAT :  return pos+nsp_ast_pprint_keyword(indent,ast,"what");  break;
	  
	default:
	  Sciprintf("Warning in PlistPrettyPrint :");
	  s=nsp_astcode_to_name(ast->get_op(ast));
	  if ( s != (char *) 0) Sciprintf(" %s ",s);
	}
    }
  return newpos;
}

/* similar to _nsp_ast_pprint_args 
 * but keep track of multi line returns between arguments
 */

static int _nsp_ast_pprint_statements(ast_wrap *ast, int start, int last, int indent, int pos, 
				      int posret, char *sep, int breakable, const char *breakstr)
{
  int j, len, newpos=pos, last_line = 0;
  const char *space = _nsp_ast_get_space(ast);
  for ( j = start ; j <= last ; j++)
    {
      ast_wrap ast1,ast2;
      if ( ast->get_arg(ast,j,&ast1) == FAIL) return 0;
      if ( j > start )
	{
	  int start_line = last_line;
	  int count;
	  if (ast1.get_length_args(&ast1) >= 1 && ast1.get_arg(&ast1,1,&ast2) == OK)
	    {
	      ast_wrap ast3;
	      if ( ast1.get_op(&ast1) == STATEMENTS1 )
		{
		  /* if ast1 is a STATEMENTS1, ast2 a separator, ast3 the first instruction */
		  if ( ast2.get_length_args(&ast2) >= 1 && ast2.get_arg(&ast2,1,&ast3) == OK)
		    {
		      start_line = ast3.get_line(&ast3);
		    }
		}
	      else
		{
		  /* if ast1 is a separator, ast2 the first instruction */
		  start_line = ast2.get_line(&ast2);
		}
	      for ( count = 0; count < start_line - last_line -1; count++)
		{
		  Sciprintf(breakstr);newpos= Sciprintf2(posret, space,"");
		}
	    }
	}
      len = _nsp_ast_printlength(&ast1,indent,newpos,posret);
      if (breakable==TRUE && ( posret < newpos ) && ( newpos > ast->columns || len > ast->columns))
	{
	  Sciprintf(breakstr);newpos= Sciprintf2(posret, space,"");
	}
      newpos = _nsp_ast_pprint(&ast1,indent,newpos,posret,0);
      if ( j != last ) newpos += Sciprintf(sep);
      last_line = ast1.get_line(&ast1);
    }
  return newpos;
}

static int _nsp_ast_pprint_statements1(ast_wrap *ast, int start, int last, int indent, int pos, 
				int posret, char *sep, int breakable, const char *breakstr)
{
  int j, len, newpos=pos;
  int last_is_comment = _nsp_ast_pprint_arg_comment_ended(ast,last);
  for ( j = start ; j <= last ; j++)
    {
      len =  _nsp_ast_printlength_arg(ast,j,indent,newpos,posret);
      if (breakable==TRUE && ( posret < newpos ) && ( newpos > ast->columns || len > ast->columns))
	{
	  Sciprintf(breakstr);newpos= Sciprintf2(posret, _nsp_ast_get_space(ast),"");
	}
      newpos = _nsp_ast_pprint_arg(ast,j,indent,newpos,posret,0);
      if ( j != last )
	{
	  newpos += Sciprintf(sep);
	}
      else
	{
	  if ( last_is_comment )
	    {
	      Sciprintf("%s","\n");newpos=0;
	    }
	}
    }
  return newpos;
}

static int _nsp_ast_pprint_args(ast_wrap *ast, int start, int last, int indent, int pos, 
				int posret, char *sep, int breakable, const char *breakstr)
{
  int j, len, newpos=pos;
  for ( j = start ; j <= last ; j++)
    {
      len =  _nsp_ast_printlength_arg(ast,j,indent,newpos,posret);
      if (breakable==TRUE && ( posret < newpos ) && ( newpos > ast->columns || len > ast->columns))
	{
	  Sciprintf(breakstr);newpos= Sciprintf2(posret, _nsp_ast_get_space(ast),"");
	}
      newpos = _nsp_ast_pprint_arg(ast,j,indent,newpos,posret,0);
      if ( j != last ) newpos += Sciprintf(sep);
    }
  return newpos;
}

static int _nsp_ast_pprint_arg(ast_wrap *ast,int elt, int indent, int pos, int posret,int tag )
{
  ast_wrap ast1;
  if ( ast->get_arg(ast,elt,&ast1) == FAIL) return 0;
  return _nsp_ast_pprint(&ast1,indent,pos,posret,tag);
}

static int _nsp_ast_pprint_arg_comment_ended(ast_wrap *ast,int elt)
{
  ast_wrap ast1;
  if ( ast->get_arg(ast,elt,&ast1) == FAIL) return 0;
  return (ast1.get_op(&ast1) == COMMENT);
}

static int _nsp_ast_pprint_op_comment_ended(ast_wrap *ast, int elt)
{
  ast_wrap ast1;
  int op;
  if ( ast->get_arg(ast,elt,&ast1) == FAIL) return FALSE;
  op = ast1.get_op(&ast1);
  if ( op == COLCONCAT || op ==CELLCOLCONCAT)
    {
      return _nsp_ast_pprint_arg_comment_ended(&ast1, ast1.get_length_args(&ast1));
    }
  return FALSE;
}

/*
 * returns the length of a mlhs  in an equal_op 
 */

static int _nsp_ast_equalop_mlhs_length(ast_wrap *ast)
{
  ast_wrap mlhs; 
  if ( ast->get_arg(ast,1,&mlhs) == FAIL) return -1;
  if ( mlhs.get_op(&mlhs) != MLHS ) return -1;
  return mlhs.get_length_args(&mlhs);
}

/* This routine returns true is the statements are on a single line
 */

static int _nsp_ast_pprint_one_line_statements(ast_wrap *ast,int elt, int offset)
{
  ast_wrap astel;
  // int l, j;
  // int current_line = ast->get_line(ast);
  if ( ast->get_arg(ast,elt,&astel) == FAIL) return FALSE;
  if ( astel.get_op(&astel) != STATEMENTS )
    {
      // Sciprintf("Should be statements\n");
      return FALSE;
    }
  return ( astel.get_line(&astel) <= ast->get_line(ast) + offset) ?  FALSE : TRUE ;
#if 0    
  l = astel.get_length_args(&astel);
  for ( j = 1 ; j <= l ; j++)
    {
      ast_wrap ast1;
      if ( astel.get_arg(&astel,j,&ast1)  == FAIL) return FALSE;
      switch (ast1.get_op(&ast1))
	{
	case RETURN_OP : 
	case COMMA_RET_OP : 
	case SEMICOLON_RET_OP  :
	  return TRUE;
	}
    }
  return FALSE;
#endif
}

#if 0
static int _nsp_ast_pprint_statements_newline_ended(ast_wrap *ast,int elt, int offset)
{
  ast_wrap astel;
  int l, j;
  // int current_line = ast->get_line(ast);
  if ( ast->get_arg(ast,elt,&astel) == FAIL) return FALSE;
  if ( astel.get_op(&astel) != STATEMENTS )
    {
      // Sciprintf("Should be statements\n");
      return FALSE;
    }
  l = astel.get_length_args(&astel);
  for ( j = 1 ; j <= l ; j++)
    {
      ast_wrap ast1;
      if ( astel.get_arg(&astel,j,&ast1)  == FAIL) return FALSE;
      switch (ast1.get_op(&ast1))
	{
	case RETURN_OP : 
	case COMMA_RET_OP : 
	case SEMICOLON_RET_OP  :
	  return TRUE;
	}
    }
  return FALSE;
}
#endif

/* this routine explores the given ast to decide if the 
 * pretty printing of the ast should start with a white space or 
 * a newline. This is usefull for example to decice if the do of 
 * a for statement should be followed by a newline or if the do can be 
 * displayed on a single line 
 */

static int _nsp_ast_pprint_check_newline(ast_wrap *ast,int elt,int pos, int offset)
{
  int newpos;
  if ( _nsp_ast_pprint_one_line_statements(ast,elt, offset) == TRUE) 
    {
      Sciprintf("\n"); newpos=0;
    }
  else
    {
      newpos = pos+ Sciprintf(" ");
    }
  return newpos;
}

static char* _nsp_ast_get_space(ast_wrap *ast)
{
  switch (ast->target)
    {
    case nsp_pprint_html: return "&nbsp;";break;
    case nsp_pprint_gtk:
    case nsp_pprint_latex:
    case nsp_pprint_term: return " ";break;
    }
  return " ";
}

static int _nsp_latin1_character_to_html(char c, char *str, int target)
{
  switch (c)
    {
    case '\300' : strcpy(str,"&Agrave;");break;
    case '\310' : strcpy(str,"&Egrave;");break;
    case '\314' : strcpy(str,"&Igrave;");break;
    case '\322' : strcpy(str,"&Ograve;");break;
    case '\331' : strcpy(str,"&Ugrave;");break;
    case '\340' : strcpy(str,"&agrave;");break;
    case '\350' : strcpy(str,"&egrave;");break;
    case '\354' : strcpy(str,"&igrave;");break;
    case '\362' : strcpy(str,"&ograve;");break;
    case '\371' : strcpy(str,"&ugrave;");break;
    case '\301' : strcpy(str,"&Aacute;");break;
    case '\311' : strcpy(str,"&Eacute;");break;
    case '\315' : strcpy(str,"&Iacute;");break;
    case '\323' : strcpy(str,"&Oacute;");break;
    case '\332' : strcpy(str,"&Uacute;");break;
    case '\335' : strcpy(str,"&Yacute;");break;
    case '\341' : strcpy(str,"&aacute;");break;
    case '\351' : strcpy(str,"&eacute;");break;
    case '\355' : strcpy(str,"&iacute;");break;
    case '\363' : strcpy(str,"&oacute;");break;
    case '\372' : strcpy(str,"&uacute;");break;
    case '\375' : strcpy(str,"&yacute;");break;
    case '\302' : strcpy(str,"&Acirc;");break;
    case '\312' : strcpy(str,"&Ecirc;");break;
    case '\316' : strcpy(str,"&Icirc;");break;
    case '\324' : strcpy(str,"&Ocirc;");break;
    case '\333' : strcpy(str,"&Ucirc;");break;
    case '\342' : strcpy(str,"&acirc;");break;
    case '\352' : strcpy(str,"&ecirc;");break;
    case '\356' : strcpy(str,"&icirc;");break;
    case '\364' : strcpy(str,"&ocirc;");break;
    case '\373' : strcpy(str,"&ucirc;");break;
    case '\303' : strcpy(str,"&Atilde;");break;
    case '\321' : strcpy(str,"&Ntilde;");break;
    case '\325' : strcpy(str,"&Otilde;");break;
    case '\343' : strcpy(str,"&atilde;");break;
    case '\361' : strcpy(str,"&ntilde;");break;
    case '\365' : strcpy(str,"&otilde;");break;
    case '\304' : strcpy(str,"&Auml;");break;
    case '\313' : strcpy(str,"&Euml;");break;
    case '\317' : strcpy(str,"&Iuml;");break;
    case '\326' : strcpy(str,"&Ouml;");break;
    case '\334' : strcpy(str,"&Uuml;");break;
    default: return FALSE;
    }

  if (target == nsp_pprint_latex)
    {
      sprintf(str,"%c",c);
    }
  return TRUE;
}

/* compute the lenght used to pprint given ast 
 */

static int _nsp_ast_printlength_args(ast_wrap *ast, int start, int last, int indent, int pos, 
				     int posret, char *sep, int breakable, const char *breakstr);
static int _nsp_ast_printlength_arg(ast_wrap *ast,int elt, int indent, int pos, int posret);
static int _nsp_ast_printlength_arg_comment_ended(ast_wrap *ast,int elt);
static int _nsp_ast_printlength_op_comment_ended(ast_wrap *ast, int elt);

static int nsp_ast_printlength_comment(int indent,const char *str)
{
  return indent + 2 + strlen(str); // 
}

static int nsp_ast_printlength_string(int indent,const char *str)
{
  return indent +  2 + strlen(str);
}

static int nsp_ast_printlength_number(int indent,const char *str)
{
  return indent + strlen(str);
}

static int nsp_ast_printlength_keyword(int indent,const char *str)
{
  return indent + strlen(str);
}

/* print operator name after @indent blank characters 
 * and return the new position.
 * pos is giving the current position.
 */

static int nsp_ast_printlength_opname(ast_wrap *ast, int indent, int pos, int pre,int post)
{
  int type = ast->get_op(ast);
  int seps=0;
  const char *s = nsp_astcode_to_name(type);
  if ( pre && ast->use_sep_space )  { seps++;}
  if ( post && ast->use_sep_space ) { seps++;}
  return pos + indent + seps + strlen(s);
}

/* posret: indentation to use if line-break. 
 *
 */

static int _nsp_ast_printlength(ast_wrap *ast, int indent, int pos, int posret)
{
  int j,newpos=0;
  
  /* be sure that we are starting to print at least at column postret */
  if ( pos < posret ) 
    {
      if ( ast->get_op(ast) != OBJECT ) pos= posret-pos +1;
    }
  /* select print operation */
  if ( ast->get_op(ast) > 0 ) 
    {
      /* operators **/
      switch ( ast->get_arity(ast) ) 
	{
	case 0: /* 0-ary operators */
	  return nsp_ast_printlength_opname(ast,indent,pos,0,0);
	  break;
	case 1:
	  switch ( ast->get_op(ast) ) 
	    {
	    case  COMMA_OP : 
	    case  SEMICOLON_OP  :
	      newpos =_nsp_ast_printlength_arg(ast,1,indent,pos,posret);
	      newpos = nsp_ast_printlength_opname(ast,0,newpos,0,0);
	      return newpos;
	      break;
	    case  COMMA_RET_OP : 
	    case  SEMICOLON_RET_OP  :
	      newpos =_nsp_ast_printlength_arg(ast,1,indent,pos,posret);
	      newpos = nsp_ast_printlength_opname(ast,0,newpos,0,0);
	      return newpos;
	      break;
	    case QUOTE_OP : 
	    case DOTPRIM:
	      newpos =_nsp_ast_printlength_arg(ast,1,indent,pos,posret);
	      newpos = nsp_ast_printlength_opname(ast,0,newpos,0,0);
	      return  newpos;
	      break;
	    case RETURN_OP : 
	      return _nsp_ast_printlength_arg(ast,1,indent,pos,posret);
	      break;
	    case TILDE_OP : 
	    default:
	      newpos =nsp_ast_printlength_opname(ast,indent,pos,0,0);
	      newpos =_nsp_ast_printlength_arg(ast,1,0,newpos,posret);
	      return newpos;
	    }
	  break;
	case 2:
	  newpos =_nsp_ast_printlength_arg(ast,1,indent,pos,posret);
	  newpos =nsp_ast_printlength_opname(ast,0,newpos,1,1);
	  newpos =_nsp_ast_printlength_arg(ast,2,0,newpos,posret);
	  return newpos;
	  break;
	default :
	  newpos = pos;
	  for ( j = 0 ; j <  ast->get_arity(ast) ; j++ )
	    {
	      newpos =_nsp_ast_printlength_arg(ast,j+1,(j == 0) ? indent : 0,
					   newpos,posret);
	      if ( j != ast->get_arity(ast) -1 ) 
		{
		  newpos =nsp_ast_printlength_opname(ast,0,newpos,1,1);
		}
	    }
	  return newpos;
	  break;
	}
    }
  else 
    {
      switch ( ast->get_op(ast) ) 
	{
	case OPT:
	  /* val = value in a calling list */
	  newpos = _nsp_ast_printlength_arg(ast,1,indent,pos,posret);
	  newpos += 3; /* Sciprintf(" = "); */
	  newpos = _nsp_ast_printlength_arg(ast,2,0,newpos,posret);
	  return newpos;
	  break;
	case EQUAL_OP:
	  /* affectations */
	  newpos = _nsp_ast_printlength_arg(ast,1,indent,pos,posret);
	  if (  _nsp_ast_equalop_mlhs_length(ast) > 0 ) 
	    newpos += 1; /*  Sciprintf("="); */
	  /* fix new return position after = */
	  newpos = _nsp_ast_printlength_arg(ast,2,0,newpos, newpos);
	  return newpos;
	  break;
	case MLHS  :
	  /* left hand side of an equality 
	   * we do not display the left and right bracket 
	   *  if arity is one 
	   */
	  newpos = pos +  indent + (( ast->get_arity(ast) > 1) ? 1 : 0);
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  if ( ast->get_arity(ast) > 1) newpos += 1;
	  return newpos;
	  break;
	case ARGS :
	  /* a sequence of expressions inside () for x()*/
	  newpos = pos +  1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE,"\n");
	  newpos += 1;
	  return newpos;
	  break;
	case CELLARGS :
	  /* a sequence of expressions inside {} for x{} */
	  newpos = pos + 1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE,"\n");
	  newpos += 1;
	  return newpos;
	  break;
	case METARGS :
	  /* a sequence of expressions inside [] for x[] */
	  newpos = pos + 1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE,"\n");
	  newpos += 1;
	  return newpos;
	  break;
	case DOTARGS :
	  {
	    ast_wrap ast1;
	    if ( ast->get_arg(ast,1,&ast1)  == FAIL) return FALSE;
	    if ( ast1.get_op(&ast1) != STRING ) return newpos;
	    newpos = pos + 1+ strlen((char *) ast1.get_str(&ast1));
	    return newpos;
	  }
	case CALLEVAL:
	case LISTEVAL :
	  newpos = pos +  1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,"",FALSE,"");
	  return newpos;
	  break;
	case FEVAL :
	  newpos =_nsp_ast_printlength_arg(ast,1,indent,pos,posret);
	  newpos += 1;
	  newpos = _nsp_ast_printlength_args(ast,2,ast->get_arity(ast),0,newpos,newpos,",",TRUE,"\n");
	  newpos += 1;
	  return newpos;
	  break;
	case PLIST :
	  /* such node should not appear here */
	  return newpos;
	  break;
	case COMMENT:
	  return pos+ nsp_ast_printlength_comment(indent,(const char *) ast->get_str(ast)); break;
	case NAME :
	  return pos+ indent + strlen((char *) ast->get_str(ast)); break;
	case OPNAME :
	  return pos+ indent + strlen((char *) ast->get_str(ast)); break;
	case NUMBER:
	  return pos + nsp_ast_printlength_number(indent,(const char *) ast->get_str(ast));break;
	case STRING:
	  return pos + nsp_ast_printlength_string(indent,(const char *) ast->get_str(ast));break;
	case INUMBER32 :
	case INUMBER64 :
	case UNUMBER32 :
	case UNUMBER64 :
	  return pos+  nsp_ast_printlength_number(indent,(const char *) ast->get_str(ast));
	  break;
	case OBJECT: 
	  return pos; break;
	case EMPTYMAT:  return pos+ 2;break;
	case EMPTYCELL: return pos+ 2;break;
	case P_MATRIX :
	  newpos = pos + 1;
	  newpos =_nsp_ast_printlength_arg(ast,1,0,newpos,posret+1);
	  newpos += 1;
	  return newpos;
	  break;
	case P_CELL :
	  newpos = pos + 1;
	  newpos =_nsp_ast_printlength_arg(ast,1,0,newpos,posret+1);
	  newpos += 1;
	  return newpos;
	  break;
	case ROWCONCAT:
	case COLCONCAT:
	case DIAGCONCAT:
	case CELLROWCONCAT:
	case CELLCOLCONCAT:
	case CELLDIAGCONCAT:
	  newpos = pos;
	  for ( j = 0 ; j < ast->get_arity(ast) ; j++)
	    {
	      int is_comment;
	      newpos =_nsp_ast_printlength_arg(ast,j+1,0,newpos,posret);
	      is_comment =_nsp_ast_printlength_op_comment_ended(ast,j+1);
	      if ( j < ast->get_arity(ast)-1)
		{
		  if ( is_comment ) 
		    {
		      newpos = posret;
		    }
		  else
		    {
		      newpos =nsp_ast_printlength_opname(ast,0,newpos,0,0);
		    }
		}
	    }
	  return newpos;
	  break;
	case WHILE:
	  newpos = pos + nsp_ast_printlength_keyword(Max(posret-pos,0),"while");
	  newpos =_nsp_ast_printlength_arg(ast,1,1,newpos,posret);
	  newpos += nsp_ast_printlength_keyword(1,"do");
	  newpos += 1;
	  newpos =_nsp_ast_printlength_arg(ast,2,0,newpos,posret+2);
	  newpos = nsp_ast_printlength_keyword(Max(posret-newpos,0),"end");
	  return newpos;
	  break;
	case FUNCTION:
	  newpos = pos + nsp_ast_printlength_keyword(Max(posret-pos,0),"function");
	  newpos = _nsp_ast_printlength_arg(ast,1,1,newpos,newpos);
	  newpos += 1;
	  newpos =_nsp_ast_printlength_arg(ast,2,0,newpos,posret+2);
	  if ( ast->get_arity(ast) == 3 ) 
	    {
	      newpos =_nsp_ast_printlength_arg(ast,3,0,newpos,posret+2);
	    }
	  newpos= nsp_ast_printlength_keyword(Max(posret-newpos,0),"endfunction");
	  return newpos;
	  break;
	case FOR:
	  newpos = pos + nsp_ast_printlength_keyword(Max(posret-pos,0),"for");
	  newpos =_nsp_ast_printlength_arg(ast,1,1,newpos,posret);
	  newpos += 1; /* = */
	  newpos =_nsp_ast_printlength_arg(ast,2,0,newpos,newpos);
	  newpos += nsp_ast_printlength_keyword(1,"do");
	  newpos += 1; /* checknewline */
	  newpos =_nsp_ast_printlength_arg(ast,3,0,newpos,posret+2);
	  newpos += nsp_ast_printlength_keyword(0,"end");
	  return newpos;
	  break;
	case IF:
	  /* a sequence of if elseif etc.... */
	  newpos = pos + nsp_ast_printlength_keyword(Max(posret-pos,0),"if");
	  for ( j = 0 ; j < ast->get_arity(ast)  ; j += 2 )
	    {
	      if ( j == ast->get_arity(ast)-1 ) 
		{
		  /* we have reached the last else **/
		  newpos +=  nsp_ast_printlength_keyword(Max(posret-newpos,0),"else");
		  newpos += 1;
		  newpos =_nsp_ast_printlength_arg(ast,j+1,0,newpos,posret+2);
		}
	      else 
		{ 
		  if ( j != 0) 
		    {
		      newpos +=nsp_ast_printlength_keyword(Max(posret-newpos,0),"elseif");
		    }
		  newpos =_nsp_ast_printlength_arg(ast,j+1,1,newpos,posret+2);
		  newpos += nsp_ast_printlength_keyword(1,"then");
		  newpos += 1;
		  newpos =_nsp_ast_printlength_arg(ast,j+2,0,newpos,posret+2);
		}
	    }
	  newpos +=  nsp_ast_printlength_keyword(Max(posret-newpos,0),"end");
	  return newpos;
	  break;
	case TRYCATCH :
	  /* try catch sequence */
	  newpos = pos+ nsp_ast_printlength_keyword(Max(posret-pos,0),"try");
	  newpos += 1;
	  newpos =_nsp_ast_printlength_arg(ast,1,0,newpos,posret+2);
	  newpos += nsp_ast_printlength_keyword(Max(posret-newpos,0),"catch");
	  newpos += 1;
	  newpos =_nsp_ast_printlength_arg(ast,2,0,newpos,posret+2);
	  if ( ast->get_arity(ast) == 3 ) 
	    {
	      newpos += nsp_ast_printlength_keyword(Max(posret-newpos,0),"finally");
	      newpos += 1;
	      newpos =_nsp_ast_printlength_arg(ast,3,0,newpos,posret+2);
	    }
	  newpos = nsp_ast_printlength_keyword(Max(posret-newpos,0),"end");
	  return newpos;
	  break;
	case SELECT :
	  /* first argument is the test.
	   * next ones are cases 
	   */
	  newpos = pos + nsp_ast_printlength_keyword(Max(posret-pos,0),"select");
	  for ( j = 0 ; j < ast->get_arity(ast) ; j++)
	    {
	      if ( j==0) 
		{
		  _nsp_ast_printlength_arg(ast,j+1,1,newpos,posret);
		  newpos= 0;
		}
	      else
		{
		  newpos=_nsp_ast_printlength_arg(ast,j+1,0,newpos,posret+2);
		}
	    }
	  newpos = nsp_ast_printlength_keyword(Max(posret-newpos,0),"end");
	  return newpos;
	  break;
	case STATEMENTS :
	  newpos=pos;
	  newpos= _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,posret,"",TRUE,"\n");
	  return newpos;
	  break;
	case STATEMENTS1 :
	  newpos=pos;
	  newpos= _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,posret,"",TRUE,"\n");
	  return newpos;
	  break;
	case PARENTH :
	  newpos = pos + 1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  newpos += 1;
	  return newpos;
	  break;
	case CASE : 
	  newpos = pos + nsp_ast_printlength_keyword(Max(posret-pos,0),"case");
	  newpos =_nsp_ast_printlength_arg(ast,1,1,newpos,posret+2);
	  nsp_ast_printlength_keyword(1,"then");
	  newpos += 1;
	  newpos =_nsp_ast_printlength_arg(ast,2,0,newpos,posret+2);
	  return newpos;
	  break;
	case LASTCASE :
	  newpos = pos + nsp_ast_printlength_keyword(Max(posret-pos,0),"else");
	  newpos += 1;
	  newpos =_nsp_ast_printlength_arg(ast,1,0,newpos,posret+2);
	  return newpos;
	  break;
	case GLOBAL:
	  /* n-ary global */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"global")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case CLEAR:
	  /* n-ary clear */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"clear")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case CLEARGLOBAL:
	  /* n-ary global */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"clearglobal")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case PAUSE:
	  /* can be 0 or 1-ary pause */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"pause")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case HELP:
	  /* 0 or  1-ary help */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"help")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case WHO:
	  /* 0 or 1-ary who */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"who")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case EXEC:
	  /* 1-ary exec */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"exec")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case APROPOS:
	  /* 1-ary apropos */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"apropos")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case CD_COMMAND:
	  /* 1-ary cd */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"cd")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case LS_COMMAND:
	  /* 1-ary ls */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"ls")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;
	case PWD_COMMAND:
	  /* 1-ary pwd */
	  newpos = nsp_ast_printlength_keyword(Max(posret-pos,0),"pwd")+1;
	  newpos = _nsp_ast_printlength_args(ast,1,ast->get_arity(ast),0,newpos,newpos,",",TRUE," ...\n");
	  return newpos;
	  break;

	case BREAK: return pos+nsp_ast_printlength_keyword(indent,"break");break;
	case PRETURN:  return pos+nsp_ast_printlength_keyword(indent,"return"); break;
	case QUIT :   return pos+nsp_ast_printlength_keyword(indent,"quit");   break;
	case NSP_EXIT :  return pos+nsp_ast_printlength_keyword(indent,"exit");  break;
	case ABORT :  return pos+nsp_ast_printlength_keyword(indent,"abort");  break;
	case CONTINUE : return pos+nsp_ast_printlength_keyword(indent,"continue");  break;
	case WHAT :  return pos+nsp_ast_printlength_keyword(indent,"what");  break;
	  
	default: break;
	}
    }
  return newpos;
}

/* a set of Args separated by sep */

static int _nsp_ast_printlength_args(ast_wrap *ast, int start, int last, int indent, int pos, 
				     int posret, char *sep, int breakable, const char *breakstr)
{
  int j,  newpos=pos;
  for ( j = start ; j <= last ; j++)
    {
      ast_wrap ast1;
      if ( ast->get_arg(ast,j,&ast1) == FAIL) return 0;
      newpos =_nsp_ast_printlength(&ast1,indent,newpos,posret);
      if ( j != last ) newpos += strlen(sep);
    }
  return newpos;
}

static int _nsp_ast_printlength_arg(ast_wrap *ast,int elt, int indent, int pos, int posret)
{
  ast_wrap ast1;
  if ( ast->get_arg(ast,elt,&ast1) == FAIL) return 0;
  return _nsp_ast_printlength(&ast1,indent,pos,posret);
}

static int _nsp_ast_printlength_arg_comment_ended(ast_wrap *ast,int elt)
{
  ast_wrap ast1;
  if ( ast->get_arg(ast,elt,&ast1) == FAIL) return FALSE;
  return ( ast1.get_op(&ast1)  == COMMENT);
}

static int _nsp_ast_printlength_op_comment_ended(ast_wrap *ast, int elt)
{
  int op;
  ast_wrap ast1;
  if ( ast->get_arg(ast,elt,&ast1) == FAIL) return FALSE;
  op = ast1.get_op(&ast1);
  if ( op == COLCONCAT ||  op ==CELLCOLCONCAT)
    {
      return _nsp_ast_printlength_arg_comment_ended(&ast1, ast1.get_length_args(&ast1));
    }
  return FALSE;
}

/* specific function for NspAst 
 */

static int nsp_ast_get_op(ast_wrap *ast)
{
  return ((NspAst *) (ast->ast))->op;
}

static int nsp_ast_get_arg(ast_wrap *ast, int i, ast_wrap *arg)
{
  NspObject *L;
  L= nsp_list_get_element(((NspAst *) (ast->ast))->args,i);
  if ( L == NULL ) return FAIL;
  *arg = *ast;
  arg->ast = L;
  return OK;
}

static int nsp_ast_get_length_args(ast_wrap *ast)
{
  return nsp_list_length(((NspAst *) (ast->ast))->args);
}

static int nsp_ast_get_arity(ast_wrap *ast)
{
  return ((NspAst *)(ast->ast))->arity;
}

static int nsp_ast_get_line(ast_wrap *ast)
{
  return ((NspAst *)(ast->ast))->line;
}

static char *nsp_ast_get_str(ast_wrap *ast)
{
  return ((NspAst *) (ast->ast))->str;
}

static void nsp_ast_to_ast_wrap(NspAst *ast, ast_wrap *astwrap)
{
  astwrap->get_str = nsp_ast_get_str;
  astwrap->get_op = nsp_ast_get_op;
  astwrap->get_arity = nsp_ast_get_arity;
  astwrap->get_line = nsp_ast_get_line;
  astwrap->get_length_args = nsp_ast_get_length_args;
  astwrap->get_arg = nsp_ast_get_arg;
  astwrap->ast = ast;
  astwrap->target = nsp_pprint_term;
  astwrap->use_sep_space = TRUE;
  astwrap->use_color = TRUE;
  astwrap->columns = 90;
}


/* specific function for PList
 */

static int nsp_plist_get_op(ast_wrap *ast)
{
  return ((PList) (ast->ast))->type;
}

static int nsp_plist_get_arg(ast_wrap *ast, int i, ast_wrap *arg)
{
  PList L = (PList) (ast->ast);
  int j;
  for ( j = 0 ; j < i ; j++) L=L->next;
  if ( L == NULL ) return FAIL;
  if ( L->type == PLIST) L= (PList) L->O;
  *arg = *ast;
  arg->ast = L;
  return OK;
}

static int nsp_plist_get_length_args(ast_wrap *ast)
{
  int i = -1;
  PList L = (PList) (ast->ast);
  while ( 1 ) 
    {
      if (L == NULL) return Max(i,0);
      L=L->next; i++;
    }
  return 0;
}

static int nsp_plist_get_arity(ast_wrap *ast)
{
  return ((PList)(ast->ast))->arity;
}

static int nsp_plist_get_line(ast_wrap *ast)
{
  return nsp_parser_get_line((PList)(ast->ast));
}

static char *nsp_plist_get_str(ast_wrap *ast)
{
  PList L = ((PList) (ast->ast));
  switch (L->type) 
    {
    case NAME : return (char *) L->O;break;
    case NUMBER: return ((parse_double *) L->O)->str;break;
    case INUMBER32 :
    case INUMBER64 :
    case UNUMBER32 :
    case UNUMBER64 : return ((parse_int *) L->O)->str;  break;
    case OPNAME :  return (char *) L->O; break;
    case OBJECT :  return NULL;break;
    case STRING:   return (char *) L->O; break;
    case COMMENT:  return (char *) L->O; break;
    default: return NULL; break;
    }
}

static void nsp_plist_to_ast_wrap(PList ast, ast_wrap *astwrap)
{
  astwrap->get_str = nsp_plist_get_str;
  astwrap->get_op = nsp_plist_get_op;
  astwrap->get_arity = nsp_plist_get_arity;
  astwrap->get_line = nsp_plist_get_line;
  astwrap->get_length_args = nsp_plist_get_length_args;
  astwrap->get_arg = nsp_plist_get_arg;
  astwrap->ast = ast;
  astwrap->target = nsp_pprint_term;
  astwrap->use_sep_space = TRUE;
  astwrap->use_color = TRUE;
  astwrap->columns = 90;
}


