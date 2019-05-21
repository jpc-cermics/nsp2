#ifndef NSP_INC_PLIST_TOKEN
#define NSP_INC_PLIST_TOKEN

/*
 * This Software is GPL (Copyright ENPC 1998-2019) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/*
 * BASIC TYPES (negative codes) 
 * they are treated in eval_arg;
 */

enum _nsp_basic_types { 
  BASIC_FIRST= -15,
  INUMBER32 = -14, 
  INUMBER64,
  UNUMBER32,
  UNUMBER64,
  NUMBER =-10,
  NAME, 
  STRING, 
  COMMENT,
  OPNAME ,
  OBJECT ,
  BASIC_LAST
};

typedef enum _nsp_basic_types nsp_basic_types;

/* Names are of max size */ 
#define NAME_MAXL 128

/*
 * OPERATORS
 * the code for operators are positives 
 */

enum _nsp_ops { 
  NOTCODE_OP =129, /* start after standard ascii codes */			    
  QUOTE_OP ,			      
  STAR_OP ,			      
  PLUS_OP ,			      
  HAT_OP ,			      
  COLON_OP ,			      
  OR_OP ,			      
  AND_OP ,			      
  TILDE_OP ,			      
  RETURN_OP ,			      
  COMMA_OP ,			      
  SEMICOLON_OP ,		      
  MINUS_OP ,			      
  SLASH_OP ,			      
  BACKSLASH_OP ,		      
  DOTSTAR   ,			      
  DOTSLASH  ,			      
  DOTBSLASH ,			      
  DOTPLUS   ,			      
  STARDOT   ,			      
  SLASHDOT  ,			      
  BSLASHDOT ,			      
  DOTSTARDOT  ,			      
  DOTSLASHDOT  ,		      
  DOTBSLASHDOT ,		      
  DOTHAT ,			      
  EQ     ,			      
  LEQ    ,			      
  GEQ    ,			      
  NEQ    ,			      
  DOTEQ     ,			      
  DOTLEQ    ,			      
  DOTLT    ,			      
  DOTGEQ    ,			      
  DOTGT    ,			      
  DOTNEQ    ,			      
  DOTPRIM ,			      
  MOINS   ,   /* unary minus */	      
  SEQAND   ,   /* sequential and */   
  SEQOR ,   /* sequential or */	      
  LT_OP,
  GT_OP,
  COMMA_RET_OP, /* ,\n */
  SEMICOLON_RET_OP, /* ;\n */
  LASTCODE_OP                         
};

typedef enum _nsp_ops nsp_ops; 

/*
 *  SPECIAL code used in internal list coding
 */

enum _nsp_codes {
  FEVAL = -200, /* must be the fisrt and such that LASTCODE_NEG_OP is negative <= nsp_basic */
  ROWCONCAT ,
  COLCONCAT ,
  LASTCASE ,
  MLHS ,
  P_MATRIX,
  STATEMENTS ,
  STATEMENTS1 ,
  OPT ,
  DIAGCONCAT ,
  LISTEVAL ,
  ARGS ,
  PARENTH ,
  DOTARGS ,
  METARGS ,
  CELLROWCONCAT ,
  CELLCOLCONCAT ,
  CELLDIAGCONCAT ,
  P_CELL,
  CELLARGS ,
  CALLEVAL ,
  EQUAL_OP ,
  EMPTYCELL,
  EMPTYMAT,
  PLIST, 
  /* language keywords the first must be ABORT */
  ABORT ,   
  APROPOS,
  BREAK , 
  CASE ,  
  CATCH ,  
  CD_COMMAND,
  CLEAR,  
  CLEARGLOBAL,  
  CONTINUE,  
  DO  ,   
  ELSE,   
  ELSEIF, 
  END,    
  ENDFUNCTION,
  EXEC,
  NSP_EXIT,
  FINALLY,
  FOR,    
  FUNCTION,
  GLOBAL,
  HELP  , 
  IF,     
  LS_COMMAND,
  PAUSE,  
  PWD_COMMAND,
  QUIT ,  
  PRETURN,
  SELECT, 
  THEN,
  TRYCATCH,
  WHAT ,  
  WHILE,  
  WHO ,   
  /* guard: take care that this one should be negative and smaller than basics */
  LASTCODE_NEG_OP  
};

typedef enum _nsp_codes nsp_codes ;


#endif 

