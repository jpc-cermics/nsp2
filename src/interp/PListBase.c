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
 * structure for storing Scilab operator 
 * which are coded with more than one char,
 * or which have special meaning ( ex , can be column matrix 
 * separator or just expr separator )
 */

typedef  struct _operator  OpWordTab;

struct _operator {
  char *name;
  char *nickname;
  int code; 
};

static OpWordTab Ops[]={
  {"'","quote",  (int) '\''},
  {"*","mult",  (int) '*'},
  {"+","plus",   (int) '+'},
  {".+","dplus",   DOTPLUS},
  {"^","hat",   (int) '^'},
  {":","impl",   (int) ':'},
  {"|","or",   (int) '|'},
  {"||","seq_or", SEQOR},
  {"&","and",   (int) '&'},
  {"&&","seq_and",  SEQAND},
  {"~","not",   (int) '~'},
  {"\\n","ret", (int) '\n'},
  {",","virg", (int) ','},
  {";","pvirg", (int) ';'},
  {"-","minus", (int) '-'},
  {"/","div", (int) '/'},
  {"\\","bdiv", (int) '\\'},
  {".^","dh", DOTHAT},
  {".'","dprim", DOTPRIM},
  {".*","dst", DOTSTAR},
  {"./","dsl",DOTSLASH},
  {".\\","dbs",DOTBSLASH},
  {"*.", "std", STARDOT},
  {"/.", "sld",SLASHDOT},
  {"\\.", "bsd",BSLASHDOT},
  {".*.", "dstd",DOTSTARDOT},
  {"./.", "dsld",DOTSLASHDOT},
  {".\\.", "dbsd",DOTBSLASHDOT},
  {"==", "eq",EQ },
  {"<=", "le", LEQ},
  {">=", "ge",GEQ},
  {"<>", "ne",NEQ} ,
  {"<","lt",(int) '<'},
  {">","gt",(int) '>'},
  {".==", "deq",DOTEQ },
  {".<=", "dle", DOTLEQ},
  {".>=", "dge",DOTGEQ},
  {".<>", "dne",DOTNEQ} ,
  {".<","dlt", DOTLT},
  {".>","dgt", DOTGT},
  {"-",  "m",MOINS}, 
  {",",  "cc", COLCONCAT},
  {";",  "rc",ROWCONCAT},
  {"#",  "dc",DIAGCONCAT},
  {",",  "cce", CELLCOLCONCAT},
  {";",  "rce", CELLROWCONCAT},
  {"#",  "dce", CELLDIAGCONCAT},
  {"STATEMENTS","stm",STATEMENTS},
  {"STATEMENTS1","stm1",STATEMENTS1},
  {"LASTCASE","lc",LASTCASE},
  {"MLHS","mlhs",MLHS},
  {"OPT","opt",OPT},
  {"MATRIX","mat",P_MATRIX},
  {"FEVAL","fe",FEVAL},
  {"ARGS","args",ARGS},
  {"METARGS","metargs",METARGS},
  {"CELLARGS","cellargs",CELLARGS},
  {"DOTARGS","dotargs",DOTARGS},
  {"LISTEVAL","listev",LISTEVAL},
  {"CALLEVAL","callev",CALLEVAL},
  {"OPT","op",OPT},
  {"PARENTH","par",PARENTH},
  {(char *) 0,(char *)0, 0}
};

/*
 * OpCode2NickN() : from internal code to character string
 * for operators 
 */

char *OpCode2NickN(int keyc)
{
  int i =0;
  while ( Ops[i].name != (char *) 0) 
    {
      if ( Ops[i].code == keyc) 
	return (Ops[i].nickname);
      i++;
    }
  return("unknown");
}

/*
 * OpCode2Str() : from internal code to character string
 * for operators 
 */

char *OpCode2Str(int code)
{
  static char op[3];
  if ( 0 < code && code < 128) 
    {
      op[0] = code ;
      op[1] = '\0';
      return(op);
    }
  else if ( 0 <  code && code < 256 ) 
    {
      op[0] = (char) (code / 128);
      op[1] = (char) (code % 128) ;
      op[2] = '\0';
      return(op);
    }
  else 
    {
      int i=0;
      while ( Ops[i].code != NOTCODE )
	{
	  if ( Ops[i].code == code ) 
	    return(Ops[i].name);
	  i++ ;
	}
      op[0] = ' ';
      op[1] = '\0';
      return((char *) 0);
    }
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



		   

