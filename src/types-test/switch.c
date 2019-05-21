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
 *
 * Graphic library
 * jpc@cermics.enpc.fr
 * store a list of graphic contexts
 * this is gui independant the gui dependant part is
 * delegated to private member of winxgc
 *--------------------------------------------------------------------------*/

#include <gtk/gtk.h>
#include "nsp/graphics-new/Graphics.h"
#include "nsp/graphics-new/switch.h"

void nsp_switch_graphic_functions(void);

int use_new_graphics=TRUE;

BCG *window_list_search(int winnum)
{
  return window_list_search_new(winnum);
}

BCG *check_graphic_window(void)
{
  return check_graphic_window_new() ;
}

BCG *set_graphic_window(int num)
{
  return set_graphic_window_new(num);
}

void nsp_gr_raise(int win_num)
{
  nsp_gr_new_raise(win_num);
}

int nsp_gr_change(int win_num)
{
  return  nsp_gr_new_change(win_num);
}

int nsp_graphic_new(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,
			double *viewport_pos,int *wpos)
{
  return nsp_graphic_new_new(win,box,v2,wdim,wpdim,viewport_pos,wpos);
}


#ifdef WITH_OPENGL
int nsp_graphic_new_gl(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,
			double *viewport_pos,int *wpos)
{
  return nsp_graphic_new_gl_new(win,box,v2,wdim,wpdim,viewport_pos,wpos);
}
#endif

#ifdef WITH_CAIRO
int nsp_graphic_new_cairo(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,
			double *viewport_pos,int *wpos)
{
  return nsp_graphic_new_cairo_new(win,box,v2,wdim,wpdim,viewport_pos,wpos);
}
#endif


/*
 *
 *
 */

#include <nsp/callfunc.h>
#include <nsp/funtab.h>

void nsp_switch_graphics(void)
{
}


int nsp_new_graphics(void)
{
  return use_new_graphics;
}
