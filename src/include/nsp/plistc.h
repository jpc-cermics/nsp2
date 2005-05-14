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


extern int IsSciKeyWord (char *id);
extern int IsCodeKeyword  (    int keyc);
extern char *Keycode2str (int keyc);
extern char *OpCode2Str (int code);
extern char *OpCode2NickN (int code);
extern int PrintOPname (int code);
extern char *TokenCode2Name (int key);
extern char *SciGetLine (char *prompt);

#endif /* PARSER_H  */
