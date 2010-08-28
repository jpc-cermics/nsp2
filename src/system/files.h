#ifndef NSP_INC_SYSTEM_FILES_H 
#define NSP_INC_SYSTEM_FILES_H 

/*
 * Copyright (C) 2003-2010 Jean-Philippe Chancelier Enpc/Cermics
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <string.h>
#include <stdio.h>
#include <string.h>
#include <nsp/math.h>
#include <nsp/machine.h>

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
extern void nsp_path_expand(const char *in_name, char *out_name, int out_size);


#endif 
