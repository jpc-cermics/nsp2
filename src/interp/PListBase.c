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

#include <string.h> 
#include <math.h>
#include "nsp/object.h"
#include "nsp/plistc.h"

/*
 * Scilab keywords an operators
 */


/*
 * structure for storing Nsp operators and keywords.
 * we have here all the possible values of node 
 * that we can find in an AST except the values of 
 * nsp_basic_types.
 * WARNING: The order in this table must follow the order of plisttoken.h 
 */

typedef  struct _operator  OpWordTab;

struct _operator {
  const char *name;
  const char *nickname;
  const int code; 
};

static OpWordTab Ops[]={
  {"@","noop",   NOTCODE_OP},
  /* operators 
   *--------------------------------------*/
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
  /* negative code from FEVAL to LASTCODE_NEG_OP 
   *--------------------------------------------*/
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
  {"{}","ecell",EMPTYCELL},
  {"[]","emath",EMPTYMAT},
  {"PLIST","plist",  PLIST},
  /* negative keys for keywords the table is just used to get their names 
   * Warning: take care that the nex lines are to be sorted alphabetically 
   * using first name and the keywords appearing in the last column are to 
   * be in the same order as in plisttoken.h. 
   * Note that ABORT is the first one and this is used in PListBase.c
   * ------------------------------------------
   */
  {"abort","noop",	ABORT },   
  {"apropos","noop",	APROPOS},
  {"break","noop",	BREAK }, 
  {"case","noop",	CASE },  
  {"catch","noop",	CATCH },  
  {"clear","noop",	CLEAR},  
  {"clearglobal","noop",CLEARGLOBAL},  
  {"continue","noop",CONTINUE},  
  {"do" ,"noop",	DO  },   
  {"else","noop",	ELSE},   
  {"elseif","noop",	ELSEIF}, 
  {"end","noop",	END},    
  {"endfunction","noop",  ENDFUNCTION},
  {"exec","noop",    EXEC},
  {"exit","noop",      NSP_EXIT},
  {"finally","noop", FINALLY},
  {"for","noop",	FOR},    
  {"function","noop",  FUNCTION},
  {"global","noop",  GLOBAL},
  {"help","noop",	HELP  }, 
  {"if","noop",       IF},     
  {"pause","noop",	PAUSE},  
  {"quit","noop",	QUIT },  
  {"return","noop",    PRETURN},
  {"select","noop",	SELECT}, 
  {"then","noop",     THEN},
  {"try","noop",     TRYCATCH},
  {"what","noop",	WHAT },  
  {"while","noop",	WHILE},  
  {"who","noop",	WHO },   
  {"@","noop", LASTCODE_NEG_OP},
  {(char *) 0,(char *)0, 0}
};



/**
 * nsp_astcode_to_nickname:
 * @code: 
 * 
 * returns the nickname of operators or keywords from their id code. 
 * 
 * 
 * Return value: a char pointer to the requested name or a pointer to "unknown";
 * 
 **/

const char *nsp_astcode_to_nickname(int code)
{
  if ( code > NOTCODE_OP && code < LASTCODE_OP ) 
    return Ops[code-NOTCODE_OP].nickname;
  if ( code < LASTCODE_NEG_OP && code >= FEVAL )
    {
      return Ops[(code-FEVAL)+LASTCODE_OP+1-NOTCODE_OP].nickname;
    }
  return("unknown");
}

/**
 * nsp_astcode_to_name:
 * @code: 
 * 
 * returns the name of operators or keywords from their id code. 
 * 
 * Return value: a char pointer to the requested name or a pointer to NULL;
 **/

const char *nsp_astcode_to_name(int code)
{
  if ( code > NOTCODE_OP && code < LASTCODE_OP ) 
    return Ops[code-NOTCODE_OP].name;
  if ( code < LASTCODE_NEG_OP && code >= FEVAL )
    {
      return Ops[(code-FEVAL)+LASTCODE_OP+1-NOTCODE_OP].name;
    }
  return NULL;
}

/**
 * nsp_is_nsp_keyword:
 * @id: 
 * 
 * checks if the given string is an Nsp keyword and returns its id code.
 * 
 * Return value: an id code as an integer or %LASTCODE_NEG_OP.
 **/

int nsp_is_nsp_keyword(const char *id)
{
  int code,i,j;
  for ( code = ABORT ; code < LASTCODE_NEG_OP ; code++) 
    {
      /* search in keywords (with are sorted) */
      i =(code-FEVAL)+LASTCODE_OP+1-NOTCODE_OP;
      j = strncmp(id,Ops[i].name,NAME_MAXL);
      if ( j == 0 ) 
	{
	  return(Ops[i].code);
	}
      else if ( j < 0)
	{
	  return LASTCODE_NEG_OP;
	} 
    }
  return LASTCODE_NEG_OP;
}

/**
 * nsp_is_code_keyword:
 * @keyc: 
 * 
 * checks if the given integer is a keyword id code.
 * 
 * Return value: %TRUE or %FALSE
 **/

int nsp_is_code_keyword(int keyc)
{
  return ( keyc < LASTCODE_NEG_OP && keyc >= ABORT ) ? TRUE  : FALSE;
}

/**
 * nsp_print_opname:
 * @code: 
 * 
 *
 * 
 * Return value: 
 **/

int nsp_print_opname(int code)
{
  const char *s = nsp_astcode_to_name(code);
  return (s != NULL) ?  Sciprintf(s): 0;
}


/**
 * nsp_ast_hash_create:
 * @void: 
 *
 * create a hash table containing all possible codes for ast nodes 
 * 
 * Return value: a new #NspHash object or %NULLHASH
 **/

NspHash *nsp_ast_hash_create(void)
{
  int i=0;
  NspObject *el;
  NspHash *ast_codes; 
  if ((ast_codes = nsp_hash_create("%ast",50))== NULLHASH) return NULLHASH;
  while (Ops[i].name != NULL)
    {
      if (Ops[i].name[0] != '@') 
	{
	  el = nsp_create_object_from_double(Ops[i].name,Ops[i].code);
	  if ( el == NULLOBJ) return NULLHASH;
	  if (nsp_hash_enter(ast_codes,el) == FAIL) return NULLHASH;
	}
      i++;
    }
  if ((el = nsp_create_object_from_double("NUMBER",NUMBER))== NULLOBJ) return NULLHASH;
  if (nsp_hash_enter(ast_codes,el) == FAIL) return NULLHASH;
  if ((el = nsp_create_object_from_double("NAME",NAME))== NULLOBJ) return NULLHASH;
  if (nsp_hash_enter(ast_codes,el) == FAIL) return NULLHASH;
  if ((el = nsp_create_object_from_double("STRING",STRING))== NULLOBJ) return NULLHASH;
  if (nsp_hash_enter(ast_codes,el) == FAIL) return NULLHASH;
  if ((el = nsp_create_object_from_double("COMMENT",COMMENT))== NULLOBJ) return NULLHASH;
  if (nsp_hash_enter(ast_codes,el) == FAIL) return NULLHASH;
  if ((el = nsp_create_object_from_double("OPNAME",OPNAME))== NULLOBJ) return NULLHASH;
  if (nsp_hash_enter(ast_codes,el) == FAIL) return NULLHASH;
  if ((el = nsp_create_object_from_double("OBJECT",OBJECT))== NULLOBJ) return NULLHASH;
  if (nsp_hash_enter(ast_codes,el) == FAIL) return NULLHASH;
  return ast_codes;
}
