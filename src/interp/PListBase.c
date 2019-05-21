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
 * functions dealing with codes which are used to identify nodes in PList.
 */

#include <nsp/nsp.h> 
#include <nsp/plist.h> 
#include <nsp/plistc.h> 
#include <nsp/hobj.h> 
#include <nsp/hash.h> 
#include <nsp/parse.h>

/*
 * structure for storing Nsp operators and keywords.
 * we have here all the possible values of node 
 * that we can find in an AST except the values of 
 * nsp_basic_types.
 * WARNING: The order in this table must follow the order of plisttoken.h 
 */

typedef  struct _operator  OpWordTab;

struct _operator {
  const char *name; /* name used in pprint */
  const char *nickname; /* name used for coding associated function */
  const int code;  /* internal code */
  const char *codename; /* a string associated to internal code */
};

/* Ops contains all the valid data which can be found in a 
 * Plist node: operators/extended-operators/keywords/data 
 */

static OpWordTab Ops[]={
  {"@","noop",NOTCODE_OP,"NOTCODE_OP"},
  /* operators 
   *--------------------------------------*/
  {"'","quote",QUOTE_OP,"QUOTE_OP"}, 
  {"*","mult",STAR_OP,"STAR_OP"}, 	
  {"+","plus",PLUS_OP,"PLUS_OP"}, 
  {"^","hat",HAT_OP,"HAT_OP"}, 
  {":","impl",COLON_OP,"COLON_OP"},
  {"|","or",OR_OP,"OR_OP"},  
  {"&","and",AND_OP,"AND_OP"}, 
  {"~","not",TILDE_OP,"TILDE_OP"},
  {"\\n","ret",RETURN_OP,"RETURN_OP"},
  {",","virg",COMMA_OP,"COMMA_OP"}, 
  {";","pvirg",SEMICOLON_OP,"SEMICOLON_OP"},
  {"-","minus",MINUS_OP,"MINUS_OP"}, 
  {"/","div",SLASH_OP,"SLASH_OP"},   
  {"\\","bdiv",BACKSLASH_OP,"BACKSLASH_OP"},
  {".*","dst",DOTSTAR,"DOTSTAR"},			          
  {"./","dsl",DOTSLASH,"DOTSLASH"},			          
  {".\\","dbs",DOTBSLASH,"DOTBSLASH"},			        
  {".+","dplus",DOTPLUS,"DOTPLUS"},			          
  {"*.","std",STARDOT,"STARDOT"},			        
  {"/.","sld",SLASHDOT,"SLASHDOT"},			          
  {"\\.","bsd",BSLASHDOT,"BSLASHDOT"},			          
  {".*.","dstd",DOTSTARDOT,"DOTSTARDOT"},			          
  {"./.","dsld",DOTSLASHDOT,"DOTSLASHDOT"},			          
  {".\\.","dbsd",DOTBSLASHDOT,"DOTBSLASHDOT"},		          
  {".^","dh",DOTHAT,"DOTHAT"},				          
  {"==","eq",EQ,"EQ"},				          
  {"<=","le",LEQ,"LEQ"},				          
  {">=","ge",GEQ,"GEQ"},				          
  {"<>","ne",NEQ,"NEQ"} ,				          
  {".==","deq",DOTEQ,"DOTEQ"},			          
  {".<=","dle",DOTLEQ,"DOTLEQ"},			          
  {".<","dlt",DOTLT,"DOTLT"},				        
  {".>=","dge",DOTGEQ,"DOTGEQ"},			        
  {".>","dgt",DOTGT,"DOTGT"},					
  {".<>","dne",DOTNEQ,"DOTNEQ"} ,			          
  {".'","dprim",DOTPRIM,"DOTPRIM"},			          
  {"-","m",MOINS,"MOINS"}, 
  {"&&","seq_and",SEQAND,"SEQAND"},			          
  {"||","seq_or",SEQOR,"SEQOR"},			          
  {"<","lt",(int) LT_OP,"LT_OP"},				          
  {">","gt",(int) GT_OP,"GT_OP"},				          
  {",","virg",COMMA_RET_OP,"COMMA_RET_OP"}, 
  {";","pvirg",SEMICOLON_RET_OP,"SEMICOLON_RET_OP"},
  {"@","noop",LASTCODE_OP,"LASTCODE_OP"},
  /* negative code from FEVAL to LASTCODE_NEG_OP 
   *--------------------------------------------*/
  {"FEVAL","fe",FEVAL,"FEVAL"},
  {";","rc",ROWCONCAT,"ROWCONCAT"},
  {",","cc",COLCONCAT,"COLCONCAT"},
  {"LASTCASE","lc",LASTCASE,"LASTCASE"},
  {"MLHS","mlhs",MLHS,"MLHS"},
  {"MATRIX","mat",P_MATRIX,"P_MATRIX"},
  {"STATEMENTS","stm",STATEMENTS,"STATEMENTS"},
  {"STATEMENTS1","stm1",STATEMENTS1,"STATEMENTS1"},
  {"OPT","opt",OPT,"OPT"},
  {"#","dc",DIAGCONCAT,"DIAGCONCAT"},
  {"LISTEVAL","listev",LISTEVAL,"LISTEVAL"},
  {"ARGS","args",ARGS,"ARGS"},
  {"PARENTH","par",PARENTH,"PARENTH"},
  {"DOTARGS","dotargs",DOTARGS,"DOTARGS"},
  {"METARGS","metargs",METARGS,"METARGS"},
  {";","rce",CELLROWCONCAT,"CELLROWCONCAT"},
  {",","cce",CELLCOLCONCAT,"CELLCOLCONCAT"},
  {"#","dce",CELLDIAGCONCAT,"CELLDIAGCONCAT"},
  {"CELL","cells",P_CELL,"P_CELL"},
  {"CELLARGS","cellargs",CELLARGS,"CELLARGS"},
  {"CALLEVAL","callev",CALLEVAL,"CALLEVAL"},
  {"=","equal",EQUAL_OP,"EQUAL_OP"},
  {"{}","ecell",EMPTYCELL,"EMPTYCELL"},
  {"[]","emath",EMPTYMAT,"EMPTYMAT"},
  {"PLIST","plist",PLIST,"PLIST"},
  /* negative keys for keywords the table is just used to get their names 
   * Warning: take care that the nex lines are to be sorted alphabetically 
   * using first name and the keywords appearing in the last column are to 
   * be in the same order as in plisttoken.h. 
   * Note that ABORT is the first one and this is used in PListBase.c
   * ------------------------------------------
   */
  {"abort","noop",ABORT,"ABORT"}, 
  {"apropos","noop",APROPOS,"APROPOS"},
  {"break","noop",BREAK,"BREAK"}, 
  {"case","noop",CASE,"CASE"}, 
  {"catch","noop",CATCH,"CATCH"}, 
  {"cd","noop",CD_COMMAND,"CD_COMMAND"}, 
  {"clear","noop",CLEAR,"CLEAR"}, 
  {"clearglobal","noop",CLEARGLOBAL,"CLEARGLOBAL"}, 
  {"continue","noop",CONTINUE,"CONTINUE"}, 
  {"do","noop",DO,"DO"}, 
  {"else","noop",ELSE,"ELSE"}, 
  {"elseif","noop",ELSEIF,"ELSEIF"}, 
  {"end","noop",END,"END"},  
  {"endfunction","noop",ENDFUNCTION,"ENDFUNCTION"},
  {"exec","noop",EXEC,"EXEC"},
  {"exit","noop",NSP_EXIT,"NSP_EXIT"},
  {"finally","noop",FINALLY,"FINALLY"},
  {"for","noop",FOR,"FOR"},  
  {"function","noop",FUNCTION,"FUNCTION"},
  {"global","noop",GLOBAL,"GLOBAL"},
  {"help","noop",HELP,"HELP"}, 
  {"if","noop",IF,"IF"},   
  {"ls","noop",LS_COMMAND,"LS_COMMAND"},   
  {"pause","noop",PAUSE,"PAUSE"}, 
  {"pwd","noop",PWD_COMMAND,"PWD_COMMAND"}, 
  {"quit","noop",QUIT,"QUIT"}, 
  {"return","noop",PRETURN,"PRETURN"},
  {"select","noop",SELECT,"SELECT"}, 
  {"then","noop",THEN,"THEN"},
  {"try","noop",TRYCATCH,"TRYCATCH"},
  {"what","noop",WHAT,"WHAT"}, 
  {"while","noop",WHILE,"WHILE"}, 
  {"who","noop",WHO,"WHO"}, 
  {"@","noop",LASTCODE_NEG_OP,"LASTCODE_NEG_OP"},
  /* basic types which can be stored in Plist */
  {"INUMBER32","noop", INUMBER32, "INUMBER32"},
  {"INUMBER64","noop", INUMBER64, "INUMBER64"},
  {"UNUMBER32","noop", UNUMBER32, "UNUMBER32"},
  {"UNUMBER64","noop", UNUMBER64, "UNUMBER64"},
  {"NUMBER","noop", NUMBER, "NUMBER"},
  {"NAME","noop", NAME, "NAME"},
  {"STRING","noop", STRING, "STRING"},
  {"COMMENT","noop", COMMENT, "COMMENT"},
  {"OPNAME","noop", OPNAME, "OPNAME"},
  {"OBJECT","noop", OBJECT, "OBJECT"},
  {(char *) 0,(char *)0, 0, (char *)0 }
};


/**
 * nsp_astcode_to_indice:
 * @code: an integer 
 * 
 * returns the position in Ops given a node id and -1 
 * in case of error.
 * 
 * Returns: an integer 
 **/

int nsp_astcode_to_indice(int code)
{
  if ( code > NOTCODE_OP && code < LASTCODE_OP ) 
    {
      /* operators */
      return code-NOTCODE_OP;
    }
  else if ( code < LASTCODE_NEG_OP && code >= FEVAL )
    {
      /* extended operators and keywords */
      return (code-FEVAL)+LASTCODE_OP+1-NOTCODE_OP;
    }
  else if ( code > BASIC_FIRST && code < BASIC_LAST ) 
    {
      /* data */
      return (code- BASIC_FIRST)+  (LASTCODE_NEG_OP-FEVAL)+LASTCODE_OP+1-NOTCODE_OP;
    }
  return -1;
}

/**
 * nsp_astcode_to_nickname:
 * @code: 
 * 
 * returns the nickname of operators or keywords from their id code. 
 *
 * Return value: a char pointer to the requested name or a pointer to "unknown";
 **/

const char *nsp_astcode_to_nickname(int code)
{
  int ind=nsp_astcode_to_indice(code);
  if ( ind == -1 ) return("unknown");
  return Ops[ind].nickname;
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
  int ind=nsp_astcode_to_indice(code);
  if ( ind == -1 ) return NULL;
  return Ops[ind].name;
}


/**
 * nsp_astcode_to_codename:
 * @code: 
 * 
 * returns the code as a string of operators or keywords from their id code. 
 *
 * Return value: a char pointer to the requested name or a pointer to "unknown";
 **/

const char *nsp_astcode_to_codename(int code)
{
  int ind=nsp_astcode_to_indice(code);
  if ( ind == -1 ) return("unknown");
  return Ops[ind].codename;
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
 * create a hash table containing all possible codes for ast nodes.
 * 
 * Return value: a new #NspHash object or %NULLHASH
 **/

NspHash *nsp_ast_hash_create(void)
{
  int i=0;
  NspObject *el;
  NspHash *ast_codes; 
  if ((ast_codes = nsp_hash_create("%ast",150))== NULLHASH) return NULLHASH;
  while (Ops[i].name != NULL)
    {
      if (Ops[i].name[0] != '@') 
	{
	  el = nsp_create_object_from_double(Ops[i].codename,Ops[i].code);
	  if ( el == NULLOBJ) return NULLHASH;
	  if (nsp_hash_enter(ast_codes,el) == FAIL) return NULLHASH;
	}
      i++;
    }
#if 1
  i=0;
  while (Ops[i].name != NULL)
    {
      if (Ops[i].name[0] != '@') 
	{
	  if (nsp_astcode_to_indice(Ops[i].code) != i ) 
	    {
	      Sciprintf("nsp_astcode_to_indice is wrong at line %d\n",i);
	    }
	}
      i++;
    }
#endif
  return ast_codes;
}
