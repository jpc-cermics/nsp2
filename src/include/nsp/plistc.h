#ifndef PARSER_H
#define PARSER_H

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include "nsp/math.h"
#include "nsp/sciio.h" 
#include "nsp/plisttoken.h"

extern int debug; 
extern int debugI;

/******************************
 * structure for Token Parsing 
 ******************************/

#define TBUF 1024

typedef struct token 
{
  char buf[TBUF];
  char syn[NAME_MAXL];
  double syv;
  char NextC;
  int id;
  int FlagEqu;
  int Line;
} Token_Obj ;

extern Token_Obj Token ;


extern int nsp_is_nsp_keyword(char *id);
extern int nsp_is_code_keyword(    int keyc);
extern char *nsp_keycode2str(int keyc);
extern char *nsp_opcode2str(int code);
extern char *nsp_opcode2nickname(int code);
extern int nsp_print_opname(int code);
extern char *TokenCode2Name (int key);
extern char *SciGetLine (char *prompt);

#endif /* PARSER_H  */
