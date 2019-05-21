#ifndef NSP_INC_STACK_H
#define NSP_INC_STACK_H 

/*
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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

#include <nsp/math.h>
#include <nsp/objectf.h>


extern void update_exec_dir(char *filename,char *exec_dir,char *filename_exec,unsigned int length);
extern void update_exec_dir_from_dir(char *dirname,char *exec_dir,unsigned int length);
extern void nsp_expand_file_with_exec_dir(Stack *stack,char *filename,char *filename_exec);
extern void nsp_expand_file_and_update_exec_dir(Stack *stack,char *old,char *filename,char *filename_exec);
extern void nsp_reset_exec_dir(Stack *stack,char *old);
extern void nsp_expand_dir_and_update_exec_dir(Stack *stack,char *old,char *dirname,char *dirname_exec);
extern int nsp_error_count_lines(Stack *stack);

#endif


  






