#ifndef SYSTEM_FILES_H 
#define SYSTEM_FILES_H 
/*------------------------------------------------------------------
 * Copyright ENPC 2003 
 * Jean-Philippe Chancelier Enpc/Cermics
 * jpc@cermics.enpc.fr 
 *------------------------------------------------------------------*/

/*-------------------------------------------------------------------
 * dealing with files and directories 
 * XXXXX : SCI -> NSP 
 *-------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include "nsp/math.h"

#if defined(__STDC__) || defined(__MSC__) || defined(__CYGWIN__) ||  (defined __ABSC__)
#include <stdlib.h>
#ifndef WIN32
#include <sys/types.h>
#include <unistd.h>
#endif
#else 
extern  char  *getenv();
#endif

#if (defined __MSC__) || (defined __ABSC__) || defined(__MINGW32__) 
#ifdef __ABSC__
#define putenv(x) abs_putenv(x)
#define getpid() getpid()
#else
#define putenv(x) _putenv(x)
#endif
#endif

#include <string.h>
#include "nsp/machine.h"

#ifdef WIN32 
#if !(defined __CYGWIN32__) && !(defined __ABSC__)
/* WIN32 */
#include <direct.h>
#define chdir(x) _chdir(x)
#define GETCWD(x,y) _getcwd(x,y)
#else 
#ifndef __ABSC__
/* WIN32 CYGWIN */
#include <unistd.h>
extern void Sciprintf(char *fmt,...);
#define GETCWD(x,y) getcwd(x,y)
#else
/* WIN32 ABSOFT */
#define GETCWD(x,y) getcwd(x,y)
#endif
#endif 
#else  /* WIN32 */
#ifdef HAVE_GETCWD
#define GETCWD(x,y) getcwd(x,y)
#else
#define GETCWD(x,y) getwd(x)
#endif
#endif /* WIN32 */

#define FSIZE 1024

extern char *get_sci_data_strings(int n);
extern void set_nsp_tmpdir(void);
extern char *get_nsp_tmpdir(void);
extern void clean_tmpdir(void);
extern int nsp_change_curdir(char *path);
extern char * nsp_get_curdir(void);
extern void nsp_path_expand(char *in_name, char *out_name, int out_size);

#endif 
