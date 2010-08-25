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
 * Tokenizer : used by the parser to parse nsp input. 
 * 
 */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "nsp/object.h"
#include "nsp/sciio.h"
#include "nsp/plistc.h"
#include "nsp/parse.h"
#define Private_Tokenizer 
#include "nsp/tokenizer.h" 

static SciReadFunc SciFileReadLine;
static SciReadFunc SciStringReadLine;
static SciReadFunc SciSMatReadLine;

#define PERCENT_CHECK(c) (((c) == '%') && (T->mtlb == FALSE) )

/**
 * nsp_init_tokenizer:
 * @T: 
 * 
 * initialize a tokenizer 
 *
 **/

void nsp_init_tokenizer(Tokenizer *T)
{
  T->NextToken=next_token;
  T->ParseCommandArg=parse_command_arg;
  T->ParseString=parse_string;
  T->GetChar=get_char;
  T->ForceNextChar=force_next_char;
  T->FuncForceNextChar=func_force_next_char;
  T->IgnoreWSpaces=ignore_white_spaces;
  T->viewch=view_char;
  T->backch=back_char;
  T->IsDotDotDot=is_dot_dot_dot;
  T->IsDotDot=is_dot_dot;
  T->IsDotStarStar=is_dot_star_star;
  T->IsDotAlphaOld=is_dot_alpha_old;
  T->IsDotAlpha=is_dot_alpha;
  T->IsTranspose=is_transpose;
  T->ParseShowLine=parse_show_line;
  T->Getlin=get_line;
  T->TokenLineSet=token_line_set;
  T->ParseOperators=parse_operators;
  T->ParseNumber=parse_number;
  T->ParseSymb=parse_symb;
  T->ParseComment=parse_comment;
  T->ParseError=parse_error;
  T->code2name=code2name;
  T->tokenv.Line = 0;
  T->io = nsp_tok_stdio;
  T->token_readline = DefSciReadLine;
  T->mtlb = FALSE;
}


void nsp_tokeniser_file(Tokenizer *T,FILE *f)
{
  T->file = f;
  T->io = nsp_tok_file;
  T->token_readline = SciFileReadLine;
}

void nsp_tokeniser_string(Tokenizer *T,const char *str)
{
  T->string.str = str;
  T->string.pos = 0;
  T->io = nsp_tok_string;
  T->token_readline = SciStringReadLine;

}

/*
 * a NULL terminated array of strings 
 */

void nsp_tokeniser_strings(Tokenizer *T,char **S)
{
  T->strings.S = S;
  T->strings.pos = 0;
  T->strings.ind = -1;
  T->strings.line = NULL;
  T->io = nsp_tok_strings;
  T->token_readline = SciSMatReadLine;

}


void scanf_get_line(char *prompt, char *buffer, int buf_size, int *eof)
{
  Tokenizer T;
  int len_line;
  nsp_init_tokenizer(&T);
  T.token_readline(&T,prompt,buffer,&buf_size, &len_line, eof);
  /* we add  \n ( which was swallowed by T->tokenv_readline*/
  buffer[len_line] ='\n';
  buffer[len_line+1] = '\0';
  buffer[len_line+2] = '\0'; /* ??? xxxx*/
}

/**
 * next_token:
 * @T: tokenizer 
 * 
 * Parses a token (partially for certain operators) 
 * if *sym = num  -> returns a string for the number Token->buf
 * if *sym = name -> returns a name in Token.syn ( syn is a int syn[6] ) 
 *     If sym is a name we search Scilab Keywords
 *     * sym is set to an internal code (see ../Newstack/PListC.h )
 * else *sym is set to char1 and char1 is changed 
 *     ( char1 is the current  read character )
 * 
 * 
 * Return value: %OK or %FAIL
 */

static int next_token(Tokenizer *T)
{
  static int chcnt;
  T->IgnoreWSpaces(T);
  T->curline.lpt1 = T->curline.lpt2;
  T->curline.lpt2 = T->curline.lpt3;
  if ( isdigit(T->tokenv.NextC))
    {
      /*    --------------parsing a number  */
      if ( T->ParseNumber(T) == FAIL) return FAIL;
      T->IgnoreWSpaces(T);
      return(OK);
    }
  if (isalnum(T->tokenv.NextC) || PERCENT_CHECK(T->tokenv.NextC)
      || T->tokenv.NextC == '_' || T->tokenv.NextC == '$') 
    {
      int key;
      /*     -------------name or Scilab keyword */
      T->tokenv.id = NAME;
      if ( T->ParseSymb(T,T->tokenv.syn,&chcnt) == FAIL) return(FAIL);
      T->IgnoreWSpaces(T);
      key =nsp_is_nsp_keyword(T->tokenv.syn);
      if ( key != LASTCODE_NEG_OP ) 
	T->tokenv.id = key;
      return(OK);
    }
  T->tokenv.id = T->tokenv.NextC;
  if ( T->tokenv.id == '\n' ) 
    {
      T->GetChar(T);
      T->tokenv.id= RETURN_OP;
      return(OK);
    }
  T->GetChar(T);
  if ( T->tokenv.id == '.' ) 
    {
      if ( isdigit(T->tokenv.NextC )) 
	{
	  /*     ------------number with leading dot */
	  T->backch(T);T->backch(T);T->GetChar(T);
	  if ( T->ParseNumber(T) == FAIL) return FAIL;
	  T->IgnoreWSpaces(T);
	  return(OK) ;
	}
      else if ( T->IsDotDotDot(T) == OK ) 
	{
	  /* ...\n*/
	  char c;
	  c=T->GetChar(T);
	  while (c != '\n') c=T->GetChar(T);
	  c= T->GetChar(T);
	  if ( T->ForceNextChar(T) == FAIL) return FAIL;
	  return T->NextToken(T);
	}
      else if ( T->IsDotDot(T) == OK ) 
	{
	  /* ..\n*/
	  char c;
	  c=T->GetChar(T);
	  while (c != '\n') c=T->GetChar(T);
	  c= T->GetChar(T);
	  if ( T->ForceNextChar(T) == FAIL) return FAIL;
	  return T->NextToken(T);
	}
    }
  else if ( (T->mtlb == TRUE)  && ( T->tokenv.id == '%' ))
    {
      /*  ----------- comments : ignore up to end of line
	  char c;
	  c=T->GetChar(T);
	  while (c != '\n') c=T->GetChar(T);
	  T->tokenv.id = '\n';
	  return(OK);
      */
      return T->ParseComment(T);
    }
  else if ( T->mtlb == FALSE && ( T->tokenv.id == '/' && T->tokenv.NextC == '/' ))
    {
      /*  ----------- comments : ignore up to end of line
	  char c;
	  c=T->GetChar(T);
	  while (c != '\n') c=T->GetChar(T);
	  T->tokenv.id = '\n';
	  return(OK);
      */
      return T->ParseComment(T);
    }
  T->ParseOperators(T);
  T->IgnoreWSpaces(T);
  /* XXXXXX : changing EOL to 0 XXX */
  if ( T->tokenv.id == 0)  T->tokenv.id= '\0';
  return(OK);
} 

/*
 * Parses nsp operators 
 */

static void  parse_operators(Tokenizer *T)
{
  /* Comparaisons */
  switch ( T->tokenv.id ) 
    {
    case '>' :
      if (T->tokenv.NextC == '=' ) 
	{
	  T->tokenv.id = GEQ ;  T->GetChar(T);  break;
	}
      break;
    case '=' : 
      if (T->tokenv.NextC == '=' ) 
	{
	  T->tokenv.id = EQ ;  T->GetChar(T);  break;
	}
      break;
    case '<' :
      if (T->tokenv.NextC == '=' ) 
	{
	  T->tokenv.id = LEQ ;  T->GetChar(T);  break;
	}
      else if (T->tokenv.NextC == '>' ) 
	{
	  T->tokenv.id = NEQ ;  T->GetChar(T);  break;
	}
      break;
    case '~' :
      if (T->tokenv.NextC == '=' ) 
	{
	  T->tokenv.id = NEQ ;  T->GetChar(T);  break;
	}
      break;
    case '*' :
      if (T->tokenv.NextC == '*' ) 
	{
	  T->tokenv.id = HAT_OP ;  T->GetChar(T);  break;
	}
      break;
    case '.' :
      switch ( T->tokenv.NextC )
	{
	case '.' :
	  T->GetChar(T); 
	  if ( T->tokenv.NextC == '.' )
	    {
	      char c;
	      c=T->GetChar(T);
	      while (c != '\n') c=T->GetChar(T);
	      T->tokenv.id = RETURN_OP;
	      break;
	    }
	  else 
	    {
	      T->tokenv.id = ' '; /* XXX : operatuer .. : A finir*/
	      break;
	    }
	case '*' :
	  T->GetChar(T);
	  if ( T->tokenv.NextC == '.' ) 
	    {
	      T->tokenv.id = DOTSTARDOT; T->GetChar(T);  break;
	    }
	  else if ( T->tokenv.NextC == '*' )
	    {
	      T->tokenv.id = DOTHAT ; T->GetChar(T) ; break;
	    }
	  else 
	    {
	      T->tokenv.id = DOTSTAR; break;
	    }
	  break;
	case '+' :
	  T->GetChar(T);
	  T->tokenv.id = DOTPLUS; break;
	case '/' :
	  T->GetChar(T); 
	  if ( T->tokenv.NextC == '/') 
	    {
	      T->backch(T);
	      break;
	    }
	  else if ( T->tokenv.NextC == '.' ) 
	    {
	      T->tokenv.id = DOTSLASHDOT; T->GetChar(T);  break;
	    }
	  else
	    {
	      T->tokenv.id = DOTSLASH; break;
	    }
	  break;
	case '\\' :
	  T->GetChar(T); 
	  if ( T->tokenv.NextC == '.' ) 
	    {
	      T->tokenv.id = DOTBSLASHDOT; T->GetChar(T);  break;
	    }
	  else
	    {
	      T->tokenv.id = DOTBSLASH; break;
	    }
	  break;
	case '^' :
	  T->GetChar(T);
	  T->tokenv.id = DOTHAT ; 
	  break;
	case '>' :
	  T->GetChar(T);
	  if ( T->tokenv.NextC == '=' ) 
	    {
	      T->tokenv.id = DOTGEQ; T->GetChar(T);  break;
	    }
	  else 
	    {
	      T->tokenv.id = DOTGT; break;
	    }
	  break;
	case '<' :
	  T->GetChar(T);
	  if ( T->tokenv.NextC == '=' ) 
	    {
	      T->tokenv.id = DOTLEQ; T->GetChar(T);  break;
	    }
	  else if ( T->tokenv.NextC == '>' ) 
	    {
	      T->tokenv.id = DOTNEQ; T->GetChar(T);  break;
	    }
	  else 
	    {
	      T->tokenv.id = DOTLT; break;
	    }
	  break;
	case '~' :
	  T->GetChar(T);
	  if ( T->tokenv.NextC == '=' ) 
	    {
	      T->tokenv.id = DOTNEQ; T->GetChar(T);  break;
	    }
	  else 
	    {
	      T->backch(T);
	      break;
	    }
	  break;
	case '=' :
	  T->GetChar(T);
	  if ( T->tokenv.NextC == '=' ) 
	    {
	      T->tokenv.id = DOTEQ; T->GetChar(T);  break;
	    }
	  else 
	    {
	      T->backch(T);
	      break;
	    }
	  break;
	}
      break;
    case '/' :
      if ( T->tokenv.NextC == '.' )
	{
	  T->GetChar(T); 
	  if ( isdigit(T->tokenv.NextC) )
	    T->backch(T);
	  else
	    T->tokenv.id = SLASHDOT;
	  break;
	}
      break;
    case '\\' :
      if ( T->tokenv.NextC == '.' )
	{
	  T->GetChar(T); 
	  if ( isdigit(T->tokenv.NextC) )
	    T->backch(T);
	  else
	    T->tokenv.id = BSLASHDOT;
	  break;
	}
      break;
    case '&' : 
      if ( T->tokenv.NextC == '&' ) 
	{
	  T->GetChar(T); 
	  T->tokenv.id = SEQAND;
	}
      break;
    case '|' : 
      if ( T->tokenv.NextC == '|' ) 
	{
	  T->GetChar(T); 
	  T->tokenv.id = SEQOR;
	}
      break;
    }
  /* basic operators are coded here with their ascii code 
   * set them to internal code 
   */
  switch ( T->tokenv.id ) 
    {
    case '\'': T->tokenv.id =  QUOTE_OP ;break;
    case '*': T->tokenv.id =  STAR_OP ;break;
    case '+': 
      T->tokenv.id =  PLUS_OP ;
      /* 
      if ( T->curline.buf[T->curline.lpt1] == ' ' 
	   &&  T->curline.buf[T->curline.lpt2] != ' ' )
	Sciprintf("This is a unary PLUS in matrix context\n");
      */
      break;
    case '^': T->tokenv.id =  HAT_OP ;break;
    case ':': T->tokenv.id =  COLON_OP ;break;
    case '|': T->tokenv.id =  OR_OP ;break;
    case '&': T->tokenv.id =  AND_OP ;break;
    case '~': T->tokenv.id =  TILDE_OP ;break;
    case '\n': T->tokenv.id =  RETURN_OP ;break;
    case ',': T->tokenv.id =  COMMA_OP ;break;
    case ';': T->tokenv.id =  SEMICOLON_OP ;break;
    case '-': T->tokenv.id =  MINUS_OP ;break;
    case '/': T->tokenv.id =  SLASH_OP ;break;
    case '\\': T->tokenv.id =  BACKSLASH_OP ;break;
    case '>': T->tokenv.id =  GT_OP ;break;
    case '<': T->tokenv.id =  LT_OP ;break;
    case '=': T->tokenv.id =  EQUAL_OP ;break;
    default:
      break;
    }
}

/*
 * Parse a Scilab number 
 * as a string in T->tokenv.buf 
 */

static int parse_number(Tokenizer *T)
{
  double deno;
  int count=0,count1;
  char c;
  c = T->tokenv.NextC;
  T->tokenv.syv= 0.00;
  while ( isdigit(c) )
    {
      T->tokenv.buf[count++]=c;
      /* T->tokenv.syv = 10.00*(  T->tokenv.syv) + cdigit2num(c) ; */
      c=T->GetChar(T);
    }
  if ( c == 'x' ) 
    {
      /* hexa */ 
      if ( count == 1 && T->tokenv.buf[0]== '0') 
	{
	  T->tokenv.buf[count++]=c;
	  c=T->GetChar(T);
	  while (1) 
	    {
	      if ( isdigit(c) ) 
		{
		  T->tokenv.buf[count++]=c;
		  /* T->tokenv.syv = 16.00*(  T->tokenv.syv) + cdigit2num(c) ; */
		  c=T->GetChar(T);
		}
	      else  if ( (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
		{
		  T->tokenv.buf[count++]=c;
		  /* T->tokenv.syv = 16.00*(  T->tokenv.syv) + c - 'a' + 10 ; */
		  c=T->GetChar(T);
		}
	      else break;
	    }
	  T->tokenv.buf[count]='\0';
	  T->tokenv.id = NUMBER;
	  return OK;
	}
      else
	{
	  T->tokenv.buf[count]='\0';
	  T->ParseError(T,"Error while parsing a number: 'x' is preceeded by %s\n",T->tokenv.buf);
	  return FAIL;
	}
    }
  if ( c == 'i' || c == 'u'  )
    {
      char c1,c2; 
      T->tokenv.buf[count++]=c;
      T->tokenv.buf[count]='\0';
      T->tokenv.id = (c == 'i') ? INUMBER32 : UNUMBER32;
      c1 = T->GetChar(T);
      if ( c1 == '3' || c1 == '6') 
	{
	  c2 = T->GetChar(T);
	  if ( ( (c1 == '3' && c2 != '2') || (c1 == '6' && c2 != '4')))
	    {
	      T->ParseError(T,"Error while parsing a number %s is ended by '%c%c' \n",T->tokenv.buf,c1,c2);
	      return FAIL;
	    }
	  if ( c == 'i' && c2 == '4' ) T->tokenv.id = INUMBER64;
	  if ( c == 'u' && c2 == '4' ) T->tokenv.id = UNUMBER64;
	  T->tokenv.buf[count++]=c1;
	  T->tokenv.buf[count++]=c2;
	  T->tokenv.buf[count]='\0';
	  T->GetChar(T);
	}
      
      return OK;
    }
  if ( c == '.' ) 
    {
      /* if the dot introduce an operator we leave the dot to the 
       * operator 
       */
      char c1 = T->curline.buf[T->curline.lpt3];
      if ( c1 == '*' || c1 == '/' || c1 == '\\' || c1 == '^' 
	   || c1 == '=' || c1 == '<' || c1 == '>' ||  c1 == '\''  )
	{
	  T->tokenv.buf[count++]='E';
	}
      else
	{
	  T->tokenv.buf[count++]=c;
	  c=T->GetChar(T);
	  deno=0.00;
	  count1=0;
	  if ( isdigit(c) ) 
	    {
	      while ( isdigit(c) )
		{
		  T->tokenv.buf[count++]=c;
		  deno = 10.00*deno + cdigit2num(c) ;
		  c=T->GetChar(T);
		  count1++;
		}
	      /* T->tokenv.syv += (deno/pow(10.00,(double)count1)); */
	    }
	}
    }
  if ( c == 'e' || c == 'E' || c == 'd' || c == 'D') 
    {
      char sign = '+';
      T->tokenv.buf[count++]='E';
      /* parsing e+xxx or e-xxx or exxx*/
      c=T->GetChar(T);
      if ( c == '+' || c == '-' ) 
	{
	  sign = c;
	  T->tokenv.buf[count++]=c;
	  c=T->GetChar(T);
	}
      if ( isdigit(c)) 
	{
	  /* int ipow= 0; */
	  while ( isdigit(c) )
	    {
	      T->tokenv.buf[count++]=c;
	      /* ipow = 10*ipow + cdigit2num(c) ; */
	      c=T->GetChar(T);
	    }
	  /* T->tokenv.syv  *= pow(10.00,(double)((sign == '+') ? ipow : -ipow)); */
	}
      else if ( c == '\t' || c == ' ' ) 
	{
          while(c == '\t' || c == ' ') c=T->GetChar(T);
        }
      else if ( c != '\n' && c != ';' && c != ',') 
	{
	  /*xxxxxxxx T->ParseError(T,"Error while parsing a number\n"); */
	}
    }
  else 
    {
      /* xxx : ! is scilab Rc up to now*/
      if ( c == '\t' || c == ' ' ) 
	{
          while(c == '\t' || c == ' ') c=T->GetChar(T);
        }
      else if ( c != '\n' && c != ';' && c != ',') 
	{
	  /*xxxxx T->ParseError(T,"Error while parsing a number\n"); */
	}
    }
  T->tokenv.buf[count]='\0';
  T->tokenv.id = NUMBER;
  return OK;
}
      
static int cdigit2num(char c)
{
  return c - '0'; 
}

/*
 * Parse a Scilab symbol 
 */

static int parse_symb(Tokenizer *T,char *str, int *l)
{
  char c;
  c = T->tokenv.NextC;
  *l =0;
  /* 
     FIXME 
     to be improved 
     - % can only be used as first char 
     - # can only be used inside 
     and we would like to add . in order to have 
     + dealing with $ 
  */
  while ( (isalnum(c) || c =='_' || c == '#' || PERCENT_CHECK(c) || c == '$' ) && (*l) < NAME_MAXL )
    {
      str[(*l)++] = c;
      c=T->GetChar(T);
    }
  if ( (*l) == NAME_MAXL ) 
    {
      str[NAME_MAXL-1]='\0';
      T->ParseError(T,"Parse Error: symbol %s... is too long\n",str);
      return(FAIL);
    }
  str[(*l)] = '\0';
  if (debug) Sciprintf("[symb %s]",str);
  return(OK);
}


/*
 * Parses a Scilab COMMENT 
 */

static int parse_comment(Tokenizer *T)
{
  char c;
  int count =0; 
  while ( (c=T->GetChar(T)) != '\n') 
    {
      if ( count < TBUF-1 ) 
	{
	  T->tokenv.buf[count++]= c;
	}
      else
	{
	  T->tokenv.buf[Min(10,TBUF-1)] = '\0';
	  T->ParseError(T,"Parse Error: comment begining with %s... is too long (>%d)\n",T->tokenv.buf,TBUF);
	  return(FAIL);
	}
    }
  T->tokenv.buf[count]= '\0';
  T->tokenv.id = COMMENT;
  return(OK);
}


/**
 *parse_command_arg:
 * @T: Tokenizer 
 * 
 * parses a command argument (for help, exec etc...) 
 * very similar to ParseString except the exit condition 
 * with is \n or ; or , 
 *
 * Return value: 
 **/

static int parse_command_arg(Tokenizer *T)
{
  char c = T->tokenv.NextC;
  int count=0;
  /* back to the begining of string*/
  while ( count < TBUF ) 
    {
      if ( c == '\0' ) 
	{
	  break;
	}
      if ( c == '\n' )
	{ 
	  /* c= T->GetChar(T); */
	  break;
	}
      else if ( c == ';' || c == ',' ) 
	{
	  break;
	}
      else if ( c == '\\') 
	{
	  char c1;
	  int d;
	  c1= T->GetChar(T);
	  /* octal coded number \ddd d in [0-7] */
	  if ( isdigit(c1) ) 
	    { 
	      d = c1 - '0' ; 
	      c1= T->GetChar(T);
	      if ( isdigit(c1)) 
		{
		  d = (d << 3)+ (c1 -'0') ; 
		  c1= T->GetChar(T);
		  if ( isdigit(c1)) 
		    {
		      d = (d << 3)+ (c1 -'0') ;
		      T->tokenv.buf[count++]= d ;
		    }
		  else 
		    {
		      T->tokenv.buf[count++]= d ;
		      T->backch(T);
		    }
		}
	      else 
		{
		  T->tokenv.buf[count++]= d ;
		  T->backch(T);
		}
	    }
	  else 
	    {
	      switch (c1 )
		{
		case 'a' :  T->tokenv.buf[count++]='\a';break;
		case 'b' :  T->tokenv.buf[count++]='\b';break;
		case 'f' :  T->tokenv.buf[count++]='\f';break;
		case 'n' :  T->tokenv.buf[count++]='\n';break;
		case 'r' :  T->tokenv.buf[count++]='\r';break;
		case 't' :  T->tokenv.buf[count++]='\t';break;
		case 'v' :  T->tokenv.buf[count++]='\v';break;
		default: 
		  T->tokenv.buf[count++]= c ;
		  T->backch(T);
		  break;
		}
	    }
	}
      else 
	T->tokenv.buf[count++]=c;
      /* get next char */
      c=T->GetChar(T);
    }
  if ( count >= TBUF )
    {
      T->tokenv.buf[Min(10,TBUF-1)] = '\0';
      T->ParseError(T,"Parse Error: string begining with %s... is too long (>%d)\n",T->tokenv.buf,TBUF);
      return(FAIL);
    }
  while (count != 0 &&  T->tokenv.buf[count-1] == ' ') count--;
  T->tokenv.buf[count]='\0';
  if (debug) Sciprintf("[Str:%s]",T->tokenv.buf);
  return(OK);
} 

/**
 *parse_string:
 * @T: Tokenizer 
 * 
 * Parses a Scilab string 
 * 
 * Return value: 
 **/

static int parse_string(Tokenizer *T)
{
  char c;
  int count=0;
  /* back to the begining of string*/
  T->curline.lpt3 = T->curline.lpt2;
  while ( count < TBUF ) 
    {
      c=T->GetChar(T);
      if ( c == '\0' ) 
	{
	  /* Delete ...\n ..\n or \n from the stored string*/
	  if ( count >= 4 && strncmp(&T->tokenv.buf[count-4],"...\n",4)==0) 
	    count -= 4;
	  else if ( count >= 3 && strncmp(&T->tokenv.buf[count-3],"..\n",4)==0) 
	    count -= 3;
	  else if (T->tokenv.buf[count-1]== '\n') 
	    count--;
	  if ( T->ForceNextChar(T) == FAIL) return FAIL;
	  c= T->tokenv.NextC;
	}
      if ( c == '\'' || c == '\"' ) 
	{ 
	  c= T->GetChar(T);
	  if ( c == '\'' || c == '\"' ) 
	    {
	      T->tokenv.buf[count++]=c;
	    }
	  else 
	    {
	      break;
	    }
	}
      else if ( c == '\\') 
	{
	  char c1;
	  int d;
	  c1= T->GetChar(T);
	  /* octal coded number \ddd d in [0-7] */
	  if ( isdigit(c1) ) 
	    { 
	      d = c1 - '0' ; 
	      c1= T->GetChar(T);
	      if ( isdigit(c1)) 
		{
		  d = (d << 3)+ (c1 -'0') ; 
		  c1= T->GetChar(T);
		  if ( isdigit(c1)) 
		    {
		      d = (d << 3)+ (c1 -'0') ;
		      T->tokenv.buf[count++]= d ;
		    }
		  else 
		    {
		      T->tokenv.buf[count++]= d ;
		      T->backch(T);
		    }
		}
	      else 
		{
		  T->tokenv.buf[count++]= d ;
		  T->backch(T);
		}
	    }
	  else 
	    {
	      switch (c1 )
		{
		case 'a' :  T->tokenv.buf[count++]='\a';break;
		case 'b' :  T->tokenv.buf[count++]='\b';break;
		case 'f' :  T->tokenv.buf[count++]='\f';break;
		case 'n' :  T->tokenv.buf[count++]='\n';break;
		case 'r' :  T->tokenv.buf[count++]='\r';break;
		case 't' :  T->tokenv.buf[count++]='\t';break;
		case 'v' :  T->tokenv.buf[count++]='\v';break;
		case '\\' :  T->tokenv.buf[count++]='\\';break;
		default: 
		  T->tokenv.buf[count++]= c ;
		  T->backch(T);
		  break;
		}
	    }
	}
      else 
	T->tokenv.buf[count++]=c;
    }
  if ( count >= TBUF )
    {
      T->tokenv.buf[Min(10,TBUF-1)] = '\0';
      T->ParseError(T,"Parse Error: string begining with %s... is too long (>%d)\n",T->tokenv.buf,TBUF);
      return(FAIL);
    }
  T->tokenv.buf[count]='\0';
  if (debug) Sciprintf("[Str:%s]",T->tokenv.buf);
  return(OK);
} 

/*
 * basic functions with current character 
 */

/* get next character */

static int get_char(Tokenizer *T)
{
  T->tokenv.NextC = T->curline.buf[T->curline.lpt3];
  if (T->tokenv.NextC != '\0') 
    {
      T->curline.lpt3++ ;
    }
  return(T->tokenv.NextC) ;
} 

/* used when current char is '\0' and we need an other line*/

static int force_next_char(Tokenizer *T)
{
  T->tokenv.NextC = T->curline.buf[T->curline.lpt3];
  if (T->tokenv.NextC != '\0') 
    {
      Sciprintf("Parse Error: ForceNextChar should only be called \n");
      Sciprintf("\twhen curent char is 0\n");
    }
  else 
    {
      if (T->Getlin(T,nsp_force_prompt()) == TRUE ) 
	{
	  T->ParseError(T,"Parse Error: Eof reached\n");
	  return(FAIL);
	}
    }
  return(OK);
}

/* For backward compatibility to accept function ending with eof 
 * see : Parse function
 */

static int func_force_next_char(Tokenizer *T)
{
  T->tokenv.NextC = T->curline.buf[T->curline.lpt3];
  if (T->tokenv.NextC != '\0') 
    {
      Sciprintf("Parse Error: ForceNextChar should only be called \n");
      Sciprintf("\twhen curent char is 0\n");
    }
  else 
    {
      if (T->Getlin(T,nsp_force_prompt()) == TRUE ) 
	return(FAIL);
    }
  return(OK);
}

/* gobble white spaces */ 

static int ignore_white_spaces(Tokenizer *T)
{
  while ( 1)
    {
      if ( T->tokenv.NextC == ' ' || T->tokenv.NextC == '\t' ) 
	{
	  T->GetChar(T);
	}
      else if ( (unsigned char) T->tokenv.NextC == 194 )
	{
	  /* 194,160 is the utf8 code of &nbsp, which 
	   * is considered as a white space 
	   */
	  T->GetChar(T);
	  if  ( (unsigned char) T->tokenv.NextC == 160 )
	    {
	      T->GetChar(T);
	    }
	  else
	    {
	      T->backch(T);
	      return 0;
	    }
	}
      else 
	{
	  return 0;
	}
    }
  return 0;
}

/*  view next character */

static int view_char(Tokenizer *T)
{
  T->tokenv.NextC = T->curline.buf[T->curline.lpt3];
  return(T->tokenv.NextC) ;
} 

/* back character */

static int back_char(Tokenizer *T)
{
  T->curline.lpt3--;
  return 0;
} 

static int is_dot_dot_dot(Tokenizer *T)
{
  if ( T->tokenv.NextC == '.' && T->curline.buf[T->curline.lpt3] == '.') 
    return(OK);
  else
    return(FAIL);
}

static int is_dot_dot(Tokenizer *T)
{
  if ( T->tokenv.NextC == '.' && T->curline.buf[T->curline.lpt3] != '.')
    return(OK);
  else
    return(FAIL);
}

static int is_dot_star_star(Tokenizer *T)
{
  if ( T->tokenv.NextC == '*' && T->curline.buf[T->curline.lpt3] == '*' ) 
    return(OK);
  else
    return(FAIL);
}

/* try to detect .<symb> with a non white space before*/

static int is_dot_alpha_old(Tokenizer *T)
{
  char c = T->curline.buf[T->curline.lpt3];
  if ( T->tokenv.NextC == '.' &&  (isalnum(c) || c =='_' || PERCENT_CHECK(c) || c == '$' ) && !isdigit(c) )
    return(OK);
  else
    return(FAIL);
}

static int is_dot_alpha(Tokenizer *T)
{
  char c = T->curline.buf[T->curline.lpt2];
  if ( T->tokenv.id == '.' && T->curline.lpt2 -2 >= 0 &&  T->curline.buf[T->curline.lpt2-2] != ' ' 
       &&  (isalnum(c) || c =='_' || PERCENT_CHECK(c) || c == '$' ) && !isdigit(c) )
    return(OK);
  else
    return(FAIL);
}

static int is_transpose(Tokenizer *T)
{
  if ( T->tokenv.id == QUOTE_OP && T->curline.lpt2 -2 >= 0 
       &&  T->curline.buf[T->curline.lpt2-2] != ' '
       &&  T->curline.buf[T->curline.lpt2-2] != '\t' ) 
    {
      return QUOTE_OP;
    }
  else if ( T->tokenv.id == '.' && T->tokenv.NextC == '\'' 
	    && T->curline.lpt2 -2 >= 0 
	    &&  T->curline.buf[T->curline.lpt2-2] != ' '
	    &&  T->curline.buf[T->curline.lpt2-2] != '\t' ) 
    {
      T->tokenv.id = DOTPRIM ;  T->GetChar(T);
      return DOTPRIM;
    }
  else return(-1);
}

/*
 * get a new line of input if necessary and 
 * set the current char. 
 * return TRUE if eof is met 
 *
 */

static int token_read_line(Tokenizer *T,char *prompt, char *buffer, int *buf_size, int *eof);

static int get_line(Tokenizer *T,char *prompt)
{
  static int firstentry=0;
  int n,eof;
  int curlinesize= LINEMAXSIZE;/* -3 to store \n\0\0*/
  token_read_line(T,prompt,T->curline.buf,&curlinesize,&eof);
  if ( firstentry == 0 ) 
    {
      T->tokenv.Line = 0;
      firstentry++;
    }
  n= strlen(T->curline.buf);
  T->tokenv.Line++;
  T->curline.lpt3 = 0;
  T->curline.lpt2 = 0;
  T->curline.lpt1 = 0;
  T->GetChar(T) ;
  return(eof);
}


/*
 * Read one line into buffer 
 */

static int token_read_line(Tokenizer *T,char *prompt, char *buffer, int *buf_size, int *eof)
{
  int len_line;
  T->token_readline(T,prompt,buffer, buf_size, &len_line, eof);
  /* we add  \n ( which was swallowed by T->tokenv_readline*/
  buffer[len_line] ='\n';
  buffer[len_line+1] = '\0';
  buffer[len_line+2] = '\0'; /* ??? xxxx*/
  
  if ( eof != NULL && len_line != 0) 
    {
      /* we do not echo empty lines. This is usefull 
       * for not echoing extra prompt at the end when evaluation
       * is done by nsp_eval_str_in_textview or 
       * nsp_eval_pasted_from_clipboard.
       */
      if ( nsp_get_echo_input_line() ) 
	Sciprintf("%s%s",prompt,buffer);
      else 
	Sciprint_diary_only("%s%s",prompt,buffer);
    }
  return 0;
}



/*
 * Reset the Line counter 
 */

static int token_line_set(Tokenizer *T,int l)
{
  int rep =   T->tokenv.Line;
  T->tokenv.Line = l;
  return rep;
}



/*
 * shows the current parsed line 
 * with a <<pointer>> to the current parsed char 
 */

static int parse_show_line(Tokenizer *T)
{
  int i;
  Scierror("\t%s",T->curline.buf);
  Scierror("\t",T->curline.buf);
  for ( i= 0 ; i < T->curline.lpt2-1; i++) Scierror(" ");
  Scierror("^\n");
  return 0;
}


/*
 * ParseError :  Default value DefParseError
 *   similar to function printf(format,args,....) 
 *   for Scilab parse error output 
 *   SetScilabError : function which can be used to 
 *               change scilab Scierror function
 */

static int parse_error (Tokenizer *T,char *fmt,...)
{
  int n;
  va_list ap;
  T->ParseShowLine(T);
  va_start(ap,fmt);
  n=  nsp_error_vprintf(fmt,ap);
  va_end(ap);
  return n;
}

  
/*
 * code to str for parsed tokens 
 */

static const char *code2name(Tokenizer *T,int key)
{
  const char *s = nsp_astcode_to_name(key);
  if (s != NULL) return s;
  switch (key) 
    {
    case COMMENT : return(T->tokenv.buf);
    case NUMBER : return(T->tokenv.buf);
    case NAME   : return(T->tokenv.syn);
    case OPNAME : return(T->tokenv.buf);
    case STRING : return(T->tokenv.buf);
    default: return(" ");
    }
}

/*
 * Used when Scilab input is from a file 
 */

static void SciFileReadLine(Tokenizer *T, char *prompt, char *buffer, int *buf_size, int *len_line, int *eof)
{
  char *s;
  s = fgets(buffer, *buf_size,T->file);
  *eof = ( s == NULL) ;
  if ( feof(T->file) != 0) 
    {
      if ( s != NULL )
	{
	  Sciprintf("Warning:\t Missing newline at end of line %s\n",s);
	}
      else 
	{
	  *eof = 1;
	  *len_line = 0;
	}
    }
  *len_line = strlen(buffer);
  /* remove newline character if there */
  if(*len_line >= 2)
    {
      if ( buffer[*len_line - 2] == '\r' && buffer[*len_line - 1] == '\n' )
	*len_line -= 2;
      else if ( buffer[*len_line - 1] == '\n') (*len_line)--;
    }
  else if( *len_line >= 1) 
    {
      if ( buffer[*len_line - 1] == '\n') (*len_line)--;
    }
  return;
}

/*
 * Used when Scilab input is from a string matrix 
 * 
 */


static void SciSMatReadLine(Tokenizer *T,char *prompt, char *buffer, int *buf_size, int *len_line, int *eof)
{
  
  if ( T->strings.S == NULL || *(T->strings.S) == 0) 
    {
      *eof = 1 ; 
      *len_line = 0;
      return ;
    }
  if ( T->strings.ind == -1 ) 
    {
      T->strings.ind++;
      T->strings.line = T->strings.S[T->strings.ind];
    }
  while (1)
    {
      nsp_string_readline_internal(prompt,buffer,buf_size,len_line,eof,&T->strings.line,&T->strings.pos);
      if ( *eof == 1 && T->strings.S[T->strings.ind+1] != NULL) 
	{
	  T->strings.ind++;
	  T->strings.line = T->strings.S[T->strings.ind];
	}
      else 
	return;
    }
}


/*
 * Used when nsp input is from a string
 */

static void SciStringReadLine(Tokenizer *T,char *prompt, char *buffer, int *buf_size, int *len_line, int *eof)
{
  nsp_string_readline_internal(prompt,buffer,buf_size,len_line,eof,&(T->string.str),&(T->string.pos));
}


static void nsp_string_readline_internal(char *prompt, char *buffer, int *buf_size, int *len_line, int *eof, 
					 nsp_const_string *nsp_input_string,  int *nsp_input_pos )
{
  const char *s,*s1;
  *eof=0;
  if ( *nsp_input_string == NULL) 
    {
      *eof = 1 ; 
      *len_line = 0;
      return ;
    }
  s =  *nsp_input_string + *nsp_input_pos;
  if ( *s == '\0') 
    {
      *nsp_input_string = NULL;
      *nsp_input_pos = 0;
      *eof = 1 ; 
      *len_line = 0;
      return ;
    }
  s1 = strstr(s,"\n"); 
  if ( s1 == NULL) 
    {
      strncpy(buffer,s,*buf_size);
      *nsp_input_string = NULL;
      *nsp_input_pos = 0;
      *len_line = strlen(buffer);
    }
  else 
    {
      *len_line = s1-s;
      *nsp_input_pos += s1-s +1 ;
      strncpy(buffer,s,Min(*buf_size,*len_line));
      buffer[*len_line]='\0';
    }
  return ;
}


/*
 * scimore :
 *    More function : Default value and function 
 *    SetScilabMore to change its value 
 */

#define MORESTR "[More (y or n ) ?] "
 
static void nsp_default_more(int *n)
{
  Tokenizer T;
  char buf[2];
  int buf_size=2, len_line, eof;
  nsp_init_tokenizer(&T);
  T.token_readline(&T,MORESTR,buf, &buf_size, &len_line, &eof);
  *n = 1; 
  if (len_line == 0 || (len_line != 0 && buf[0] == 'y') ) *n=0;
}

void scimore_void(int *n)
{
  *n=1;
}

/**
 * nsp_set_nsp_more:
 * @F: a #Morefun 
 * 
 * used to change the interaction function activated 
 * during output operation to ask for more output lines.
 * 
 * Returns: a #Morefun 
 **/

MoreFun scimore = nsp_default_more;

MoreFun nsp_set_nsp_more(MoreFun F)
{
  MoreFun g = scimore;
  scimore = F;
  return g; 
}

/*
 * Input main routine !!!
 */

extern int Xorgetchar(void);

SciGetC Scigetchar = Xorgetchar;

SciGetC nsp_set_nsp_getchar(SciGetC F)
{
  SciGetC g =  Scigetchar;
  Scigetchar = F;
  return g;
}

/*
 * Prompts 
 * XXXX: should be move from static to Tokenizer 
 *       data 
 */

static char SciPrompt[]="-nsp->";
static char SciForcePrompt[]="==>";
static char SciPausePrompt[] = "-nsp-XXX->";
static int nsp_pause = 0;

char *nsp_prompt(void)
{
  if ( nsp_pause ) 
    return  SciPausePrompt;
  else 
    return  SciPrompt;
}

char *nsp_force_prompt(void)
{
  return  SciForcePrompt;
} 

void inc_pause_prompt(void)
{
  nsp_pause++;
  nsp_pause = Max(0,Min(nsp_pause,999));
  sprintf(SciPausePrompt,"-nsp-%d->",nsp_pause);
}

void dec_pause_prompt(void)
{
  nsp_pause--; 
  nsp_pause = Max(0,Min(nsp_pause,999));
  sprintf(SciPausePrompt,"-nsp-%d->",nsp_pause);
  /* clean: usefull when calls are reentrant */
  nsp_readline_clear_line();
}

void zero_pause_prompt(void)
{
  nsp_pause = 0;
}

