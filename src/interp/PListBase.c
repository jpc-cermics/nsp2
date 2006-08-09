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
  {"return",	PRETURN}, 
  {"select",	SELECT}, 
  {"then" ,	THEN},  
  {"try",     TRYCATCH},
  {"what",	WHAT },  
  {"while",	WHILE},  
  {"who",	WHO },   
  {(char *) 0,   LASTCODE_NEG_OP },
};

/*
 * Checks if id is a Nsp keyword (i.e a word in the table cmd )
 * Returns the code for the keyword or NOTKEY value if 
 * id is not a keyword 
 */

int nsp_is_nsp_keyword(char *id)
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
	      return LASTCODE_NEG_OP;
	    } 
	  else i++;
	}
    }
  return LASTCODE_NEG_OP;
}

/*
 * Checks if keyc is the code of a Scilab keyword
 */

int nsp_is_code_keyword(int keyc)
{
  return ( keyc < LASTCODE_NEG_OP && keyc >= WHILE ) ? OK : FAIL;
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
  /* operators */
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
  /* composed operators */
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
  {"=", "equal",EQUAL_OP},
  /* negative keys for keywords the table is just used to get their names */
  {"while","noop",	WHILE},  
  {"end","noop",	END},    
  {"select","noop",	SELECT}, 
  {"case","noop",	CASE },  
  {"quit","noop",	QUIT },  
  {"exit","noop",      NSP_EXIT},
  {"return","noop",    PRETURN},
  {"help","noop",	HELP  }, 
  {"what","noop",	WHAT },  
  {"who","noop",	WHO },   
  {"pause","noop",	PAUSE},  
  {"clear","noop",	CLEAR},  
  {"if","noop",       IF},     
  {"then","noop",     THEN},
  {"do" ,"noop",	DO  },   
  {"apropos","noop",	APROPOS},
  {"abort","noop",	ABORT },   
  {"break","noop",	BREAK }, 
  {"elseif","noop",	ELSEIF}, 
  {"else","noop",	ELSE},   
  {"for","noop",	FOR},    
  {"function","noop",  FUNCTION},
  {"endfunction","noop",  ENDFUNCTION},
  {"exec","noop",    EXEC},
  {"global","noop",  GLOBAL},
  {"clearglobal","noop",CLEARGLOBAL},  
  {"try","noop",     TRYCATCH},
  {"catch","noop",	CATCH },  
  {"finally","noop", FINALLY},
  {"continue","noop",CONTINUE},  
  {"@","noop", LASTCODE_NEG_OP},
  {(char *) 0,(char *)0, 0}
};

/*
 *nsp_opcode2nickname() : from internal code to character string
 * for operators 
 */

char *nsp_opcode2nickname(int code)
{
  if ( code > NOTCODE_OP && code < LASTCODE_OP ) 
    return Ops[code-NOTCODE_OP].nickname;
  if ( code < LASTCODE_NEG_OP && code >= FEVAL )
    {
      return Ops[(code-FEVAL)+LASTCODE_OP+1-NOTCODE_OP].nickname;
    }
  return("unknown");
}

/*
 *nsp_opcode2str() : from internal code to character string
 * for operators 
 */

char *nsp_opcode2str(int code)
{
  if ( code > NOTCODE_OP && code < LASTCODE_OP ) 
    return Ops[code-NOTCODE_OP].name;
  if ( code < LASTCODE_NEG_OP && code >= FEVAL )
    {
      return Ops[(code-FEVAL)+LASTCODE_OP+1-NOTCODE_OP].name;
    }
  return("unknown");
}

/*
 * returns the command name from its code
 */

char *nsp_keycode2str(int code)
{
  if ( code < LASTCODE_NEG_OP && code >= WHILE )
    {
      return Ops[(code-FEVAL)+LASTCODE_OP+1-NOTCODE_OP].name;
    }
  return NULL;
}



/*
 * Prints the name of an operator from its internal code 
 */

int nsp_print_opname(int code)
{
  char *s;
  if ((s=nsp_opcode2str(code)) != (char*)0) 
    return Sciprintf(s);
  return 0;
}


char * nsp_astcode_to_string(int code)
{
  if ( code > NOTCODE_OP && code < LASTCODE_OP ) 
    {
      return Ops[code-NOTCODE_OP].name;
    }
  if ( code < LASTCODE_NEG_OP && code >= FEVAL )
    {
      return Ops[(code-FEVAL)+LASTCODE_OP+1-NOTCODE_OP].name;
    }
  return("unknown");
}
