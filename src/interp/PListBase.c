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
 *
 * utilities and data for parsed expressions 
 *
 */

#include <string.h> 
#include <math.h>
#include "nsp/object.h"
#include "nsp/plistc.h"

/*
 * Scilab keywords an operators
 */

typedef  struct _keyword KeyWordTab;

struct _keyword {
  char *name;
  int code; 
};

/* XXX a enlver **/

static KeyWordTab cmd[]={
  {"abort",	ABORT }, 
  {"apropos",	APROPOS},
  {"break",	BREAK }, 
  {"case",	CASE },  
  {"catch",	CATCH },  
  {"clear",	CLEAR},  
  {"clearglobal",CLEARGLOBAL},  
  {"continue",CONTINUE},  
  {"do" ,	DO  },   
  {"else",	ELSE},   
  {"elseif",	ELSEIF}, 
  {"end",	END},    
  {"endfunction",  ENDFUNCTION},
  {"exec",    EXEC},
  {"exit",	NSP_EXIT },  
  {"finally", FINALLY},
  {"for",	FOR},    
  {"function",  FUNCTION},
  {"global",  GLOBAL},
  {"help",	HELP  }, 
  {"if",       IF},     
  {"pause",	PAUSE},  
  {"quit",	QUIT },  
  /*    {"resume",	RESUME}: changed resume is a function   **/
  {"return",	PRETURN}, 
  {"select",	SELECT}, 
  {"then" ,	THEN},  
  {"try",     TRYCATCH},
  {"what",	WHAT },  
  {"while",	WHILE},  
  {"who",	WHO },   
  {(char *) 0,NOTKEY}
};

/*
 * Checks if id is a Nsp keyword (i.e a word in the table cmd )
 * Returns the code for the keyword or NOTKEY value if 
 * id is not a keyword 
 */

int IsSciKeyWord(char *id)
{
  int i=0;
  while ( cmd[i].name != (char *) 0)
    {
      int j;
      j = strncmp(id,cmd[i].name,NAME_MAXL);
      if ( j == 0 ) 
	{
	  return(cmd[i].code);
	}
      else 
	{ 
	  if ( j <= 0)
	    {
	      return NOTKEY;
	    } 
	  else i++;
	}
    }
  return  NOTKEY;
}

/*
 * Checks if keyc is the code of a Scilab keyword
 * (see token.h ) 
 */

int IsCodeKeyword(int keyc)
{
  if ( keyc <= WHILE && keyc > NOTKEY  ) return(OK) ;
  else return(FAIL);
}


/*
 * returns the command name from its code
 * or 0 if keyc is not a keyword code 
 */

char *Keycode2str(int keyc)
{
  int i=0;
  while ( 1) 
    {
      if (cmd[i].code == keyc ) return(cmd[i].name);
      if (cmd[i].code == NOTKEY ) return NULL;
      i++;
    }
  return((char *) 0);
}

/*
 * structure for storing Nsp operators 
 */

typedef  struct _operator  OpWordTab;

struct _operator {
  char *name;
  char *nickname;
  int code; 
};

/* the order in this table must follow 
 * the order of plisttoken.h 
 */

static OpWordTab Ops[]={
  {"@","noop",   NOTCODE_OP},
  {"'","quote",  QUOTE_OP},  
  {"*","mult",   STAR_OP }, 	
  {"+","plus",   PLUS_OP },  
  {"^","hat",   HAT_OP},   
  {":","impl",   COLON_OP},
  {"|","or",   OR_OP},    
  {"&","and",   AND_OP},  
  {"~","not",   TILDE_OP},
  {"\\n","ret", RETURN_OP},
  {",","virg", COMMA_OP}, 
  {";","pvirg", SEMICOLON_OP},
  {"-","minus", MINUS_OP},   
  {"/","div", SLASH_OP},     
  {"\\","bdiv", BACKSLASH_OP},
  /* composed */
  {".*","dst", DOTSTAR},			          
  {"./","dsl",DOTSLASH},			          
  {".\\","dbs",DOTBSLASH},			        
  {".+","dplus",   DOTPLUS},			          
  {"*.", "std", STARDOT},			        
  {"/.", "sld",SLASHDOT},			          
  {"\\.", "bsd",BSLASHDOT},			          
  {".*.", "dstd",DOTSTARDOT},			          
  {"./.", "dsld",DOTSLASHDOT},			          
  {".\\.", "dbsd",DOTBSLASHDOT},		          
  {".^","dh", DOTHAT},				          
  {"==", "eq",EQ },				          
  {"<=", "le", LEQ},				          
  {">=", "ge",GEQ},				          
  {"<>", "ne",NEQ} ,				          
  {".==", "deq",DOTEQ },			          
  {".<=", "dle", DOTLEQ},			          
  {".<","dlt", DOTLT},				        
  {".>=", "dge",DOTGEQ},			        
  {".>","dgt", DOTGT},					
  {".<>", "dne",DOTNEQ} ,			          
  {".'","dprim", DOTPRIM},			          
  {"-",  "m",MOINS}, 
  {"&&","seq_and",  SEQAND},			          
  {"||","seq_or", SEQOR},			          
  {"<","lt",(int) LT_OP},				          
  {">","gt",(int) GT_OP},				          
  {"@","noop", LASTCODE_OP},
  /* negative code from FEVAL to LASTCODE_NEG_OP */
  {"FEVAL","fe",FEVAL},
  {";",  "rc", ROWCONCAT},
  {",",  "cc", COLCONCAT},
  {"LASTCASE","lc",LASTCASE},
  {"MLHS","mlhs",MLHS},
  {"MATRIX","mat",P_MATRIX},
  {"STATEMENTS","stm",STATEMENTS},
  {"STATEMENTS1","stm1",STATEMENTS1},
  {"OPT","opt",OPT},
  {"#",  "dc", DIAGCONCAT},
  {"LISTEVAL","listev",LISTEVAL},
  {"ARGS","args",ARGS},
  {"PARENTH","par",PARENTH},
  {"DOTARGS","dotargs",DOTARGS},
  {"METARGS","metargs",METARGS},
  {";",  "rce",CELLROWCONCAT},
  {",",  "cce",CELLCOLCONCAT},
  {"#",  "dce",CELLDIAGCONCAT},
  {"CELL","cells",P_CELL},
  {"CELLARGS","cellargs",CELLARGS},
  {"CALLEVAL","callev",CALLEVAL},
  {"@","noop", LASTCODE_NEG_OP},
  {(char *) 0,(char *)0, 0}
};

/*
 * OpCode2NickN() : from internal code to character string
 * for operators 
 */

char *OpCode2NickN(int keyc)
{
  if ( keyc > NOTCODE_OP && keyc < LASTCODE_OP ) 
    return Ops[keyc-NOTCODE_OP].nickname;
  if ( keyc > LASTCODE_NEG_OP && keyc <= FEVAL )
    {
      return Ops[(FEVAL-keyc)+LASTCODE_OP+1-NOTCODE_OP].nickname;
    }
  return("unknown");
}

/*
 * OpCode2Str() : from internal code to character string
 * for operators 
 */

char *OpCode2Str(int code)
{
  if ( code > NOTCODE_OP && code < LASTCODE_OP ) 
    return Ops[code-NOTCODE_OP].name;
  if ( code > LASTCODE_NEG_OP && code <= FEVAL )
    {
      int i=(FEVAL-code)+LASTCODE_OP+1-NOTCODE_OP;
      return Ops[i].name;
    }
  return("unknown");
}

/*
 * Prints the name of an operator from its internal code 
 */

int PrintOPname(int code)
{
  char *s;
  if ((s= OpCode2Str(code)) != (char*)0) 
    return Sciprintf(s);
  return 0;
}


char * nsp_astcode_to_string(int code)
{
  if ( code > NOTCODE_OP && code < LASTCODE_OP ) 
    {
      return Ops[code-NOTCODE_OP].name;
    }
  if ( code > LASTCODE_NEG_OP && code <= FEVAL )
    {
      int i=(FEVAL-code)+LASTCODE_OP+1-NOTCODE_OP;
      return Ops[i].name;
    }
  /* remaining keys */
  switch ( code )
    {
    case '=': return "=";
    case PLIST : return "PLIST";
    case COMMENT : return "COMMENT";
    case NAME : return "NAME";
    case OPNAME : return "OPNAME";
    case NUMBER: return "NUMBER";
    case STRING: return "STRING";
    case EMPTYMAT: return "EMPTYMAT";
    case EMPTYCELL: return "EMPTYCELL";
    case WHILE: return "WHILE";
    case FUNCTION: return "FUNCTION";
    case FOR: return "FOR";
    case IF : return "IF";
    case TRYCATCH : return "TRYCATCH";
    case SELECT : return "SELECT";
    case CASE : return "CASE";
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
