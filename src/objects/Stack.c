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
  stack->fname = NULL;
  stack->file_name = NULL;
  stack->first = 0;
  stack->S = S ;
  stack->L = S + STACK_SIZE;
  stack->error_msg = (NspObject *) nsp_smatrix_create(NVOID,0,0,NULL,0);
  stack->error = error;
  stack->errcatch = FALSE;
  stack->pause = TRUE;
}

Stack SciStack ;

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
      nsp_row_smatrix_append_string((NspSMatrix *) stack->error_msg,buf);
      va_end(ap);
    }
}

