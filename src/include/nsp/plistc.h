#ifndef PARSER_H
#define PARSER_H

/*
 * This Software is GPL (Copyright ENPC 1998-2006) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include "nsp/math.h"
#include "nsp/sciio.h" 
#include "nsp/plisttoken.h"
#include <nsp/objectf.h>

extern int debug; 
extern int debugI;

/*
 * structure for Token Parsing 
 */

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

extern const char *nsp_astcode_to_nickname(int code);
extern const char *nsp_astcode_to_name(int code);
extern int nsp_is_nsp_keyword(const char *id);
extern int nsp_is_code_keyword(int keyc);
extern int nsp_print_opname(int code);
extern NspHash *nsp_ast_hash_create(void);

#endif /* PARSER_H  */
