/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

#ifndef PLISTTOKEN_H
#define PLISTTOKEN_H

/****************************
 * BASIC TYPES
 *****************************/

#define NUMBER -1
#define NAME   -2
#define PLIST   -3
#define STRING   -4
#define EMPTYMAT -5
#define COMMENT  -6 
#define OPNAME -30

/* Names are of max size */ 
#define NAME_MAXL 52

/****************************
 * KEYWORD 
 ****************************/

#define WHILE   -7
#define END     -8
#define SELECT  -9
#define CASE    -10
#define QUIT    -11
#define EXIT    -12 
#define RETURN  -13
#define HELP    -14
#define WHAT    -15
#define WHO     -16
#define PAUSE   -17
#define CLEAR   -18
#define IF      -19
#define THEN    -20
#define DO      -21
#define APROPOS -22
#define ABORT   -23
#define BREAK   -24
#define ELSEIF  -25
#define ELSE    -26
#define FOR     -27
#define FUNCTION -28
#define ENDFUNCTION -29
/* -30 is used */
#define EXEC -31
#define GLOBAL -32
#define CLEARGLOBAL   -33
#define NOTKEY  -34

/*****************************
 * OPERATORS
 * with name longer than one char 
 *****************************/

#define DOTSTAR   ((((int) '*') << 7) + (int) '.' )
#define DOTSLASH  ((((int) '/') << 7) + (int) '.' )
#define DOTBSLASH ((((int) '\\') << 7) + (int) '.' )

#define STARDOT   ((((int) '.') << 7) + (int) '*' )
#define SLASHDOT  ((((int) '.') << 7) + (int) '/' )
#define BSLASHDOT ((((int) '.') << 7) + (int) '\\' )

#define DOTSTARDOT  (((int) '.') <<14 ) + (((int) '*') << 7) + (int) '.' 
#define DOTSLASHDOT  (((int) '.') <<14 )+(((int) '/') << 7) + (int) '.' 
#define DOTBSLASHDOT (((int) '.') <<14 )+ (((int) '\\') << 7) + (int) '.'

#define DOTHAT ((((int) '^') << 7) + (int) '.' )

#define EQ     ((((int) '=') << 7) + (int) '=' )
#define LEQ    ((((int) '<') << 7) + (int) '=' )
#define GEQ    ((((int) '>') << 7) + (int) '=' )
#define NEQ    ((((int) '<') << 7) + (int) '>' )

#define DOTEQ     ((((int) '=') << 14) + (((int) '=') << 7)  + (int) '.' )
#define DOTLEQ    ((((int) '<') << 14) + (((int) '=') << 7)  + (int) '.' )
#define DOTLT    ((((int) '<') << 7) +  (int) '.' )
#define DOTGEQ    ((((int) '>') << 14) + (((int) '=') << 7)  + (int) '.' )
#define DOTGT    ((((int) '>') << 7) +  (int) '.' )
#define DOTNEQ    ((((int) '<') << 14) + (((int) '>') << 7)  + (int) '.' )

#define DOTPRIM ((((int) '.') << 7) + (int) '\'' )

/* Unary minus */

#define MOINS   ((((int) '-') << 7) + (int) '-' )
#define NOTCODE 0

/******************************************
 *  SPECIAL code used in internal list coding
 ******************************************/

#define FEVAL -100
#define ROWCONCAT -101
#define COLCONCAT -102
#define LASTCASE  -103
#define MLHS      -104
#define P_MATRIX  -105
#define STATEMENTS -106
#define STATEMENTS1 -107
#define OPT        -108
#define DIAGCONCAT -109
#define LISTEVAL   -110
#define ARGS       -111
#define PARENTH    -112
#define DOTARGS    -113
#define METARGS    -114

#endif 
