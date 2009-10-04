/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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
#include "nsp/graphics-old/Graphics.h" 
#include "nsp/graphics-new/switch.h" 

void nsp_switch_graphic_functions(void);

BCG *window_list_search_new(int winnum);
BCG *window_list_search_old(int winnum);
BCG *check_graphic_window_new(void);
BCG *check_graphic_window_old(void);
BCG *set_graphic_window_new(int num) ;
BCG *set_graphic_window_old(int num) ;
void nsp_gr_new_raise(int win_num);
void nsp_gr_old_raise(int win_num);
int nsp_gr_new_change(int win_num);
int nsp_gr_old_change(int win_num);
int nsp_graphic_new_new(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,
		    double *viewport_pos,int *wpos);
int nsp_graphic_new_old(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,
		    double *viewport_pos,int *wpos);
int nsp_graphic_new_gl_new(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,
		       double *viewport_pos,int *wpos);
int nsp_graphic_new_gl_old(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,
		       double *viewport_pos,int *wpos);
int nsp_graphic_new_cairo_new(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,
		       double *viewport_pos,int *wpos);
int nsp_graphic_new_cairo_old(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,
		       double *viewport_pos,int *wpos);


int use_new_graphics=FALSE;

BCG *window_list_search(int winnum)
{ 
  if (use_new_graphics)
    return window_list_search_new(winnum);
  else
    return window_list_search_old(winnum);
}

BCG *check_graphic_window(void) 
{
  if (use_new_graphics)
    return check_graphic_window_new() ;
  else
    return check_graphic_window_old() ;
}

BCG *set_graphic_window(int num) 
{
  if (use_new_graphics)
    return set_graphic_window_new(num);
  else
    return set_graphic_window_old(num);
}

void nsp_gr_raise(int win_num)
{
  if (use_new_graphics)
    nsp_gr_new_raise(win_num);
  else
    nsp_gr_old_raise(win_num);
}

int nsp_gr_change(int win_num)
{
  if (use_new_graphics)
    return  nsp_gr_new_change(win_num);
  else
    return  nsp_gr_old_change(win_num);
}

int nsp_graphic_new(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,
			double *viewport_pos,int *wpos)
{
  if (use_new_graphics)
    return nsp_graphic_new_new(win,box,v2,wdim,wpdim,viewport_pos,wpos);
  else
    return nsp_graphic_new_old(win,box,v2,wdim,wpdim,viewport_pos,wpos);
}

int nsp_graphic_new_gl(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,
			double *viewport_pos,int *wpos)
{
  if (use_new_graphics)
    return nsp_graphic_new_gl_new(win,box,v2,wdim,wpdim,viewport_pos,wpos);
  else
    return nsp_graphic_new_gl_old(win,box,v2,wdim,wpdim,viewport_pos,wpos);
}

int nsp_graphic_new_cairo(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,
			double *viewport_pos,int *wpos)
{
  if (use_new_graphics)
    return nsp_graphic_new_cairo_new(win,box,v2,wdim,wpdim,viewport_pos,wpos);
  else
    return nsp_graphic_new_cairo_old(win,box,v2,wdim,wpdim,viewport_pos,wpos);
}


/*
 *
 *
 */ 

#include "../functions/callfunc.h" 
#include "../functions/FunTab.h"

extern void Graphics_Interf_Info(int i, char **fname, function (**f));
extern void GraphicsOld_Interf_Info(int i, char **fname, function (**f));
extern OpGrTab GraphicsOld_func[];
extern OpGrTab Graphics_func[];

void nsp_switch_graphics(void)
{
  int i=0, gr_new=-1,gr_old=-1,k;
  while (1) 
    {
      /* interfaces */
      interface_info *info = Interfaces[i].info;
      if ( info == NULL) break;
      if ( info == Graphics_Interf_Info)
	{
	  gr_new = i;
	}
      else if ( info == GraphicsOld_Interf_Info)
	{
	  gr_old = i;

	}
      i++;
    }
  if ( gr_new == -1 || gr_old == -1 ) return;

  use_new_graphics = ! (use_new_graphics);


  k=0;
  while (1) 
    {
      if ( Graphics_func[k].name1 == NULL ) break;
      if ( use_new_graphics ) 
	nsp_enter_function(Graphics_func[k].name2,gr_new,k);
      else 
	nsp_enter_function(Graphics_func[k].name1,gr_new,k);
      k++;
    }
  k=0;
  while (1) 
    {
      if ( GraphicsOld_func[k].name1 == NULL ) break;
      if ( use_new_graphics )
	nsp_enter_function(GraphicsOld_func[k].name2,gr_old,k);
      else
	nsp_enter_function(GraphicsOld_func[k].name1,gr_old,k);
      k++;
    }
}


int nsp_new_graphics(void)
{
  return use_new_graphics;
}
