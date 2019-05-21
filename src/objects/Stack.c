/* Nsp
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

#include <nsp/nsp.h>
#include <nsp/stack.h>
#include <nsp/interf.h>
#include <nsp/system.h> /* FSIZE */
#include <nsp/smatrix.h>
#include <nsp/nspthreads.h>

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

static void error(Stack *stack,char *fmt,...);

typedef struct _nsp_interp nsp_interp; 
struct _nsp_interp {
  GThread *thread;
  Stack stack;
  void *datas;
};

static nsp_interp nsp_data_interp[128];
static int nsp_interp_count = 0;

#ifdef NSP_WITH_MAIN_GTK_THREAD
G_LOCK_DEFINE(nsp_interp_count);
#endif 

int nsp_new_interp(GThread *thread,int argc, char **argv)
{
#ifdef NSP_WITH_MAIN_GTK_THREAD
  G_LOCK (nsp_interp_count);
#endif 
  nsp_init_stack(&nsp_data_interp[nsp_interp_count].stack);
  nsp_data_interp[nsp_interp_count].thread= thread;
  nsp_init_frames(&nsp_data_interp[nsp_interp_count].datas,argc,argv);
  nsp_interp_count++;
#ifdef NSP_WITH_MAIN_GTK_THREAD
  G_UNLOCK (nsp_interp_count);
#endif 
  return OK;
}


static int nsp_get_interp()
{
  GThread *self = g_thread_self();
  int i = 0;
#ifdef NSP_WITH_MAIN_GTK_THREAD
  G_LOCK (nsp_interp_count);
  int imax = nsp_interp_count;
  G_UNLOCK (nsp_interp_count);
#else 
  int imax = nsp_interp_count;
#endif 
  for ( i=0; i <  imax ; i++)
    {
      if ( self == nsp_data_interp[i].thread) 
	{
	  /* printf("Found interp %d\n",i); */
	  return i;
	}
    }
  /* printf("Returns the default interp \n"); */
  return 0;
}

Stack *nsp_get_stack()
{
  int i= nsp_get_interp();
  return &nsp_data_interp[i].stack;
}

void *nsp_get_datas()
{
  int i= nsp_get_interp();
  return nsp_data_interp[i].datas;

}

void nsp_init_stack(Stack *stack)
{
  stack->first = 0;
  stack->fname = NULL;
  stack->file_name = NULL;
  stack->dollar = -1;
  /* XXX : should check here that stack->val != NULL */
  stack->val = calloc(1,sizeof(Stack_ref));
  stack->val->S = calloc(STACK_SIZE, sizeof(NspObject *));
  stack->val->L = stack->val->S + STACK_SIZE; /* tag for end of stack */
  if ( stack->val->error_msg == NULL) 
    stack->val->error_msg = (NspObject *) nsp_smatrix_create(NVOID,0,0,NULL,0);
  stack->val->error = error;
  stack->val->errcatch = FALSE;
  stack->val->pause = TRUE;
  stack->val->symbols = NULL;
  stack->val->current_exec_dir = calloc(FSIZE+1,sizeof(char));
}

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

/* counts number of lines in error messages 
 *
 */

int nsp_error_count_lines(Stack *stack)
{
  if ( stack->val->error_msg == NULL) 
    return 0;
  else 
    return ((NspSMatrix *) stack->val->error_msg)->m;
}
