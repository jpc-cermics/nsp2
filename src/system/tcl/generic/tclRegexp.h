/*
 * Definitions etc. for regexp(3) routines.
 * Caveat:  this is V8 regexp(3) [actually, a reimplementation thereof],
 * not the System V one.
 */

#ifndef _REGEXP
#define _REGEXP 1

/* opaque structure to hide regexp */

typedef struct Tcl_RegExp_ *Tcl_RegExp;

#define ckalloc(x) malloc(x)
#define ckfree(x)  free(x)
#define ckrealloc(x,y) realloc(x,y)

/*
 * NSUBEXP must be at least 10, and no greater than 117 or the parser
 * will not work properly.
 */

#define NSUBEXP  20

typedef struct regexp {
  char *startp[NSUBEXP];
  char *endp[NSUBEXP];
  char regstart;		/* Internal use only. */
  char reganch;		/* Internal use only. */
  char *regmust;		/* Internal use only. */
  int regmlen;		/* Internal use only. */
  char program[1];	/* Unwarranted chumminess with compiler. */
} regexp;


extern Tcl_RegExp	Tcl_RegExpCompile( char *string);
extern int		Tcl_RegExpExec( Tcl_RegExp regexp, char *string, char *start);
extern int		Tcl_RegExpMatch( char *string, char *pattern);
extern void		Tcl_RegExpRange(Tcl_RegExp regexp, int index, char **startPtr, char **endPtr);

extern regexp *TclRegComp (char *exp);
extern int TclRegExec (regexp *prog, char *string, char *start);
/* extern void TclRegSub (regexp *prog, char *source, char *dest); */
extern void TclRegError (char *msg);
extern char *TclGetRegError (void);

#include "dstring.h"
extern int nsp_tcl_regsub(char *str,Tcl_RegExp regExpr,char *subSpec,nsp_tcldstring *resultDString,
			  int *nmatch,int all);

#endif /* REGEXP */
