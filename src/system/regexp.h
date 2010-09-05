/*
 * Definitions etc. for regexp(3) routines.
 * Caveat:  this is V8 regexp(3) [actually, a reimplementation thereof],
 * not the System V one.
 */

#ifndef _REGEXP
#define _REGEXP 1

#include "nsp/dstring.h" 

/* opaque structure to hide regexp */

typedef struct Tcl_RegExp_ *Tcl_RegExp;

#define ckalloc(x) malloc(x)
#define ckfree(x)  free(x)
#define ckrealloc(x,y) realloc(x,y)

#ifndef UCHAR
#define UCHAR(c) ((unsigned char) (c))
#endif 

/*
 * NSUBEXP must be at least 10, and no greater than 117 or the parser
 * will not work properly.
 */

#define NSUBEXP  20

typedef struct regexp {
  const char *startp[NSUBEXP];
  const char *endp[NSUBEXP];
  char regstart;		/* Internal use only. */
  char reganch;		/* Internal use only. */
  char *regmust;		/* Internal use only. */
  int regmlen;		/* Internal use only. */
  char program[1];	/* Unwarranted chumminess with compiler. */
} regexp;


extern Tcl_RegExp	nsp_tclregexp_compile( char *string);
extern int		nsp_tclregexp_exec( Tcl_RegExp regexp,const char *string, char *start);
extern int		nsp_tclregexp_match( char *string, char *pattern);
extern void		nsp_tclregexp_range(Tcl_RegExp regexp, int index,const char **startPtr,const char **endPtr);

extern regexp *tcl_reg_comp (char *exp);
extern int tcl_reg_exec (regexp *prog,const char *string, const char *start);
/* extern void TclRegSub (regexp *prog, char *source, char *dest); */
extern void tcl_reg_error (char *msg);
extern char *tcl_get_reg_error (void);

extern int nsp_tcl_regsub(char *str,Tcl_RegExp regExpr,char *subSpec,nsp_tcldstring *resultDString,
			  int *nmatch,int all);

#endif /* REGEXP */
