#ifndef NSP_INC_PLIST_TOKEN
#define NSP_INC_PLIST_TOKEN

/*
 * This Software is GPL (Copyright ENPC 1998-2006) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/*
 * BASIC TYPES (negative codes) 
 * they are treated in eval_arg;
 */

typedef enum _nsp_basic_types nsp_basic_types;
enum _nsp_basic_types { 
  NUMBER =-1,
  NAME =   -2,
  PLIST =   -3,
  STRING =   -4,
  EMPTYMAT = -5,
  COMMENT =  -6 ,
  EMPTYCELL = -7,
  OPNAME = -8,
  OBJECT = -9 
};

/* Names are of max size */ 
#define NAME_MAXL 52

/*
 * OPERATORS
 * the code for operators are positives 
 */

typedef enum _nsp_ops nsp_ops; 
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
  LASTCODE_OP                         
};


/*
 *  SPECIAL code used in internal list coding
 */

typedef enum _nsp_codes nsp_codes ;
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
  WHILE , /* language keywords the first must be WHILE */
  END ,
  SELECT ,
  CASE ,
  QUIT ,
  NSP_EXIT ,
  PRETURN ,
  HELP ,
  WHAT ,
  WHO ,
  PAUSE ,
  CLEAR ,
  IF ,
  THEN ,
  DO ,
  APROPOS ,
  ABORT ,
  BREAK ,
  ELSEIF ,
  ELSE ,
  FOR ,
  FUNCTION ,
  ENDFUNCTION ,
  EXEC ,
  GLOBAL ,
  CLEARGLOBAL ,
  TRYCATCH ,
  CATCH ,
  FINALLY ,
  CONTINUE ,
  LASTCODE_NEG_OP 
};

#endif 

