/* Nsp
 * Copyright (C) 2012-2012 Jean-Philippe Chancelier Enpc/Cermics
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
 *
 * jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <nsp/nspthreads.h>

void nsp_check_threads(const char *str)
{
#ifdef NSP_WITH_MAIN_GTK_THREAD
  if ( g_thread_self() != thmain ) 
    {
      printf("Warning: nsp_check_threads was called outside thmain thread. %s\n",str);
    }
#endif
}

/**
 * nsp_interface_idle_in_main_thread:
 * @user_data: a gpointer 
 * 
 * Utility function executed once by gtk main thread through 
 * a idle_add.
 *
 * Returns: FALSE 
 **/

static gboolean nsp_interface_idle_in_main_thread(gpointer user_data)
{
  nsp_thread_interface *data = user_data;
  gdk_threads_enter();
  printf("-->execution of interface in the main gtk thread%d\n",data->i);
  data->ans = (*(data->fun))(*(data->stack), data->rhs, data->opt, data->lhs);
  gdk_threads_leave();
  printf("<--execution of interface in the main gtk thread%d\n",data->i);
  g_async_queue_push(data->queue,data);
  return FALSE;
}

/**
 * nsp_interface_executed_in_main_thread:
 * @i: integer 
 * @f: interface function 
 * @stack: nsp execution stack
 * @rhs: integer 
 * @opt: integer 
 * @lhs: integer 
 * 
 * call an nsp interface but the call is executed through g_idle_add
 * to force the execution in the main gtk thread. 
 * TO BE DONE ? : only one thread should call this function.
 *
 * Returns: an integer 
 **/

int nsp_interface_executed_in_main_thread(int i, function f,Stack *stack, int rhs, int opt, int lhs)
{
  static GAsyncQueue *queue = NULL;
  nsp_thread_interface data = {stack,f,i,rhs,opt,lhs,0,queue};
  /* checks if we are already in gtk thread */
  if ( g_thread_self() == thmain ) 
    {
      int n;
      gdk_threads_enter();  
      n= (*f)(stack,rhs,opt,lhs);
      gdk_threads_leave();  
      return n;
    }
  
  if ( queue == NULL) 
    {
      queue= g_async_queue_new ();
      data.queue = queue;
    }
  g_idle_add( nsp_interface_idle_in_main_thread,(gpointer) &data);
  g_async_queue_pop(data.queue);
  printf("-->Quit the async queue returning %d\n",data.ans);
  return data.ans;
}
