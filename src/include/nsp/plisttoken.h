#ifndef NSP_INC_PLIST_TOKEN
#define NSP_INC_PLIST_TOKEN

/*
 * This Software is GPL (Copyright ENPC 1998-2006) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/*
 * BASIC TYPES
 */

typedef enum _nsp_basic_types nsp_basic_types;
enum _nsp_basic_types { 
  NUMBER =-1,
  NAME =   -2,
  PLIST =   -3,
  STRING =   -4,
  EMPTYMAT = -5,
  COMMENT =  -6 ,
  EMPTYCELL =  -38,
  OPNAME = -30,
  OBJECT = -39 
};

/* Names are of max size */ 
#define NAME_MAXL 52

/*
 * KEYWORDS
 */

typedef enum _nsp_keywords nsp_keywords;
enum _nsp_keywords { 
  WHILE =   -7, /* WHILE must be the first */
  END =     -8,
  SELECT =  -9,
  CASE =    -10,
  QUIT =    -11,
  NSP_EXIT =-12,
  PRETURN = -13,
  HELP =    -14,
  WHAT =    -15,
  WHO =     -16,
  PAUSE =   -17,
  CLEAR =   -18,
  IF =      -19,
  THEN =    -20,
  DO =      -21,
  APROPOS = -22,
  ABORT =   -23,
  BREAK =   -24,
  ELSEIF =  -25,
  ELSE =    -26,
  FOR =     -27,
  FUNCTION = -28,
  ENDFUNCTION = -29,
  /* -30 is used */
  EXEC = -31,
  GLOBAL = -32,
  CLEARGLOBAL =   -33,
  TRYCATCH =    -34,
  CATCH =  -35 ,
  FINALLY =  -36 ,
  CONTINUE =  -37,
  /* -38 is used */
  /* -39 is used */
  NOTKEY =  -40 /* should be the last */
};

/*
 * OPERATORS
 * the code for operators are positives 
 */


#if 0 
typedef enum _nsp_ops nsp_ops; 
enum _nsp_ops { 
  QUOTE_OP =    ((int) '\''),
  STAR_OP =    ((int) '*'),
  PLUS_OP =    ((int) '+'),
  HAT_OP =    ((int) '^'),
  COLON_OP =    ((int) ':'),
  OR_OP =    ((int) '|'),
  AND_OP =    ((int) '&'),
  TILDE_OP =    ((int) '~'),
  RETURN_OP =  ((int) '\n'), 
  COMMA_OP =     ((int) ','),
  SEMICOLON_OP =    ((int) ';'),
  MINUS_OP =    ((int) '-'),
  SLASH_OP =    ((int) '/'),
  BACKSLASH_OP =    ((int) '\\'),

  DOTSTAR   =((((int) '*') << 7) + (int) '.' ),
  DOTSLASH  =((((int) '/') << 7) + (int) '.' ),
  DOTBSLASH =((((int) '\\') << 7) + (int) '.' ),

  DOTPLUS   =((((int) '+') << 7) + (int) '.' ),

  STARDOT   =((((int) '.') << 7) + (int) '*' ),
  SLASHDOT  =((((int) '.') << 7) + (int) '/' ),
  BSLASHDOT =((((int) '.') << 7) + (int) '\\' ),

  DOTSTARDOT  =(((int) '.') <<14 ) + (((int) '*') << 7) + (int) '.' ,
  DOTSLASHDOT  =(((int) '.') <<14 )+(((int) '/') << 7) + (int) '.' ,
  DOTBSLASHDOT =(((int) '.') <<14 )+ (((int) '\\') << 7) + (int) '.',

  DOTHAT =((((int) '^') << 7) + (int) '.' ),

  EQ     =((((int) '=') << 7) + (int) '=' ),
  LEQ    =((((int) '<') << 7) + (int) '=' ),
  GEQ    =((((int) '>') << 7) + (int) '=' ),
  NEQ    =((((int) '<') << 7) + (int) '>' ),

  DOTEQ     =((((int) '=') << 14) + (((int) '=') << 7)  + (int) '.' ),
  DOTLEQ    =((((int) '<') << 14) + (((int) '=') << 7)  + (int) '.' ),
  DOTLT    =((((int) '<') << 7) +  (int) '.' ),
  DOTGEQ    =((((int) '>') << 14) + (((int) '=') << 7)  + (int) '.' ),
  DOTGT    =((((int) '>') << 7) +  (int) '.' ),
  DOTNEQ    =((((int) '<') << 14) + (((int) '>') << 7)  + (int) '.' ),

  DOTPRIM =((((int) '.') << 7) + (int) '\'' ),

  /* Unary minus */

  MOINS   =((((int) '-') << 7) + (int) '-' ), 
  NOTCODE =0,

  /* and or sequential version */

  SEQAND   =((((int) '&') << 7) + (int) '&' ),  
  SEQOR    =((((int) '|') << 7) + (int) '|' ) 
};

#else

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

#endif 

/*
 *  SPECIAL code used in internal list coding
 */

typedef enum _nsp_codes nsp_codes ;
enum _nsp_codes {
  FEVAL = -100, /* must be the first */
  ROWCONCAT = -101,
  COLCONCAT = -102,
  LASTCASE =  -103,
  MLHS =      -104,
  P_MATRIX=  -105,
  STATEMENTS = -106,
  STATEMENTS1 = -107,
  OPT =        -108,
  DIAGCONCAT = -109,
  LISTEVAL =   -110,
  ARGS =       -111,
  PARENTH =    -112,
  DOTARGS =    -113,
  METARGS =    -114, 
  CELLROWCONCAT = -115,
  CELLCOLCONCAT = -116, 
  CELLDIAGCONCAT = -117, 
  P_CELL=  -118, 
  CELLARGS =    -119, 
  CALLEVAL =  -120, 
  EQUAL = -121,
  LASTCODE_NEG_OP = -122
};

#endif 

