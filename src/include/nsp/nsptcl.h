/*
 * nsptcl.h 
 * Declarations of things used in nsp and defined in tcl 
 *
 */

#ifndef _NSPTCL
#define _NSPTCL

#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <stdlib.h>
#include <nsp/machine.h> 

#include "nsp/object.h"
#include "nsp/stack.h"
#include "nsp/interf.h"
#include "nsp/dstring.h"

extern char *nsp_get_cwd (void);
extern const char *nsp_get_extension (const char *name);
extern char *nsp_get_user_home (char *name, nsp_tcldstring *bufferPtr);
extern char *nsp_getenv (const char *name);
extern void nsp_setenv(const char *name,const char *value);
extern void nsp_unsetenv(const char *name);
extern int nsp_chdir (char *dirName);
extern int nsp_copy_directory (char *source,char *dest, nsp_tcldstring *errorPtr);
extern int nsp_copy_file (char *source, char *dest);
extern int nsp_create_directory (char *path);
extern int nsp_delete_file (char *path);
extern int nsp_do_glob (char *separators, nsp_tcldstring *headPtr, char *tail,NspSMatrix *S);
extern int nsp_file_copy_cmd (int argc, char **argv,int force ) ;
extern int nsp_file_delete_cmd (int argc, char **argv,int force );
extern int nsp_file_make_dirs_cmd ( int argc, char **argv) ;
extern int nsp_file_rename_cmd ( int argc, char **argv,int force);
extern int nsp_list_volumes (Stack stack,int n);
extern int nsp_match_files(char *separators,  nsp_tcldstring *dirPtr, char *pattern, char *tail,NspSMatrix *S);
extern int nsp_putenv(const char *string);
extern int nsp_remove_directory (char *path, int recursive, nsp_tcldstring *errorPtr);
extern int nsp_rename_file (char *source, char *dest);
extern int nsp_string_match(char *string,char * pattern);
extern nsp_string nsp_absolute_file_name( char *fname);
extern nsp_string nsp_dirname (char *fileName);
extern nsp_string nsp_tail(char *fileName);
extern void nsp_create_exit_handler();
extern void nsp_finalize_environment (void);
extern void nsp_tclplatform_init (void);
extern void update_exec_dir(char *filename,char *exec_dir,char *filename_exec,unsigned int length);
extern void update_exec_dir_from_dir(char *dirname,char *exec_dir,unsigned int length);

#endif /* _TCLINT */

