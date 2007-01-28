/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
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

#include <math.h>
#include "nsp/stack.h"
#include "nsp/interf.h"
#include "../system/files.h" /* FSIZE */

static void error(Stack *stack,char *fmt,...);

NspObject *S[STACK_SIZE];

/*
 * Info on Stack 
 */

void StackInfo(void)
{
  
}

void InitStack()
{
  nsp_init_stack(&SciStack,S);
}

void nsp_init_stack(Stack *stack,NspObject **S)
{
  stack->first = 0;
  stack->fname = NULL;
  stack->file_name = NULL;
  /* XXX : should check here that stack->val != NULL */
  stack->val = calloc(1,sizeof(Stack_ref));
  stack->val->S = S ;
  stack->val->L = S + STACK_SIZE;
  if ( stack->val->error_msg == NULL) 
    stack->val->error_msg = (NspObject *) nsp_smatrix_create(NVOID,0,0,NULL,0);
  stack->val->error = error;
  stack->val->errcatch = FALSE;
  stack->val->pause = TRUE;
  stack->val->symbols = NULL;
  stack->val->current_exec_dir = calloc(FSIZE+1,sizeof(char));
}

Stack SciStack={0,NULL};

/* store object o at position pos (relative from first ) */ 

void StackStore(Stack stack, NspObject * o,int pos)
{
  NthObj(pos) = o;
}

static void error(Stack *stack,char *fmt,...)
{
  const int size=256;
  char buf[size];
  int n;
  va_list ap;
  va_start(ap,fmt); 
  n=  vsnprintf(buf,size, fmt, ap );
  if ( n > -1 ) 
    {
      nsp_row_smatrix_append_string((NspSMatrix *) stack->val->error_msg,buf);
      va_end(ap);
    }
}

