/* Nsp
 * Copyright (C) 2009-2010 Jean-Philippe Chancelier Enpc/Cermics
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
 * Interface with the portaudio library. 
 *
 * utilities which should be compiled without optimization. 
 *
 */

#include <glib.h>
#include <gtk/gtk.h>
#include <sndfile.h>
#include <portaudio.h>
#include <nsp/interf.h>
#include "pansp.h"

static nsp_pa_status nsp_pa_thread_status = NSP_PA_INACTIVE ;


gint timeout_portaudio (void *data)
{
  if ( nsp_pa_thread_status == NSP_PA_INACTIVE ) 
    {
      gtk_main_quit();
    }
  return TRUE;
}

extern void controlC_handler (int sig);

void controlC_handler_portaudio(int sig)
{
  /* ask play_file to stop */
  if ( nsp_pa_thread_status == NSP_PA_ACTIVE ) 
    nsp_pa_thread_status =  NSP_PA_END;
}

/* a menu callback 
 */

void nsp_pa_stop()
{
  if ( nsp_pa_thread_status == NSP_PA_ACTIVE ) 
    nsp_pa_thread_status = NSP_PA_END;
}

/* tell portaudio to finish active thread
 */

void nsp_finish_pa_thread()
{
  if ( nsp_pa_thread_status == NSP_PA_ACTIVE ) 
    {
      /* finish active thread */
      nsp_pa_thread_set_status(NSP_PA_END);
      while (  nsp_pa_thread_get_status()== NSP_PA_END ) {} ;
      /* now the child have fixed active to NSP_PA_INACTIVE  */
    }
}

void nsp_pa_thread_set_status( nsp_pa_status st)
{
  nsp_pa_thread_status = st;
}

nsp_pa_status nsp_pa_thread_get_status()
{
  return nsp_pa_thread_status;
}

