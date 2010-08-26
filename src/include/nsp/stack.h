#ifndef NSP_INC_STACK 
#define NSP_INC_STACK 

/*
 * This Software is GPL (Copyright ENPC 1998-2010) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include "nsp/math.h"
#include "nsp/object.h"

extern void update_exec_dir(char *filename,char *exec_dir,char *filename_exec,unsigned int length);
extern void update_exec_dir_from_dir(char *dirname,char *exec_dir,unsigned int length);
extern void nsp_expand_file_with_exec_dir(Stack *stack,char *filename,char *filename_exec);
extern void nsp_expand_file_and_update_exec_dir(Stack *stack,char *old,char *filename,char *filename_exec);
extern void nsp_reset_exec_dir(Stack *stack,char *old);
extern void nsp_expand_dir_and_update_exec_dir(Stack *stack,char *old,char *dirname,char *dirname_exec);

#endif


  






