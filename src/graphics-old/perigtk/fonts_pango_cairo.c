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
 *
 * Graphic library
 * jpc@cermics.enpc.fr
 * Gtk driver 
 *--------------------------------------------------------------------------*/

/*
 * Text and symbols with pango 
 */

#define FONTNUMBER 7 
#define FONTMAXSIZE 6
#define SYMBOLNUMBER 12

/* Must be of size FONTMAXSIZE */

static const int pango_size[] = { 8 ,10,12,14,18,24};

static char *pango_fonttab[] ={"Courier", "Standard Symbols L","Sans","Sans","Sans","Sans"};

static void nsp_fonts_finalize(BCG *Xgc)
{
  if ( Xgc->private->layout != NULL) 
    {
      g_object_unref ( Xgc->private->layout);Xgc->private->layout=NULL;
      g_object_unref ( Xgc->private->mark_layout);Xgc->private->mark_layout=NULL;
      pango_font_description_free ( Xgc->private->desc); Xgc->private->desc=NULL;
      pango_font_description_free ( Xgc->private->mark_desc);Xgc->private->mark_desc=NULL;
    }
}

static void nsp_fonts_initialize(BCG *Xgc)
{
  if ( Xgc->private->layout == NULL) 
    {
      Xgc->private->layout = pango_cairo_create_layout(Xgc->private->cairo_cr);
      Xgc->private->mark_layout = pango_cairo_create_layout(Xgc->private->cairo_cr);
      Xgc->private->desc = pango_font_description_new();
      Xgc->private->mark_desc = pango_font_description_from_string(pango_fonttab[1]);
    }
}

static void loadfamily(char *name, int *j)
{
  /* */
}

static void queryfamily(char *name, int *j,int *v3)
{
  /* */
}

static void xset_font(BCG *Xgc,int fontid, int fontsize)
{ 
  int i,fsiz;
  i = Min(FONTNUMBER-1,Max(fontid,0));
  fsiz = Min(FONTMAXSIZE-1,Max(fontsize,0));
  if ( Xgc->fontId != i || Xgc->fontSize != fsiz )
    {
      Xgc->fontId = i;
      Xgc->fontSize = fsiz;
      pango_font_description_set_family(Xgc->private->desc, pango_fonttab[i]);
      /* pango_font_description_set_size (Xgc->private->desc, pango_size[fsiz] * PANGO_SCALE);*/
      pango_font_description_set_absolute_size (Xgc->private->desc, pango_size[fsiz] * PANGO_SCALE);

      pango_layout_set_font_description (Xgc->private->layout, Xgc->private->desc);
    }
}

/* To get the  id and size of the current font */

static void  xget_font(BCG *Xgc,int *font)
{
  font[0] = Xgc->fontId ;
  font[1] = Xgc->fontSize ;
}

static void xset_mark(BCG *Xgc,int number, int size)
{ 
  int n_size;
  Xgc->CurHardSymb = Max(Min(SYMBOLNUMBER-1,number),0);
  n_size  = Max(Min(FONTMAXSIZE-1,size),0);
  if ( Xgc->CurHardSymbSize != n_size )
    {
      Xgc->CurHardSymbSize = n_size;
      /* pango_font_description_set_size (Xgc->private->mark_desc, pango_size[fsiz] * PANGO_SCALE);*/
      pango_font_description_set_absolute_size (Xgc->private->mark_desc, 
						pango_size[Xgc->CurHardSymbSize] * PANGO_SCALE);
      pango_layout_set_font_description (Xgc->private->mark_layout, Xgc->private->mark_desc);
    }
}

/* To get the current mark id */

static void xget_mark(BCG *Xgc,int *symb)
{
  symb[0] = Xgc->CurHardSymb ;
  symb[1] = Xgc->CurHardSymbSize ;
}


/* drawing marks with pango 
 */

static const int symbols[] = 
  {
    0x22C5, /* lozenge */
    0x002B, /* plus sign */
    0x00D7, /* multiplication sign */
    0x2217, /* asterisk operator */
    0x2666, /* black diamond suit */
    0x25CA, /* lozenge */
    0x0394, /* greek capital letter delta */
    0x2207, /* nabla  */
    0x2663, /* black club suit */
    0x2295, /* circled plus */
    0x2665, /* black heart suit */
    0x2660, /* black spade suit */
    0x2297, /* circled times */
    0x2022, /* bullet */
    0x00B0  /* degree sign */
  };

/* utility to rotate a string 
 *
 */


static void get_rotated_layout_bounds (PangoLayout  *layout,PangoContext *context,
				       const PangoMatrix *matrix, GdkRectangle *rect)
{
  gdouble x_min = 0, x_max = 0, y_min = 0, y_max = 0;
  PangoRectangle logical_rect;
  gint i, j;
  pango_layout_get_extents (layout, NULL, &logical_rect);
  for (i = 0; i < 2; i++)
    {
      gdouble x = (i == 0) ? logical_rect.x : logical_rect.x + logical_rect.width;
      for (j = 0; j < 2; j++)
	{
	  gdouble y = (j == 0) ? logical_rect.y : logical_rect.y + logical_rect.height;
	  
	  gdouble xt = (x * matrix->xx + y * matrix->xy) / PANGO_SCALE + matrix->x0;
	  gdouble yt = (x * matrix->yx + y * matrix->yy) / PANGO_SCALE + matrix->y0;
	  
	  if (i == 0 && j == 0)
	    {
	      x_min = x_max = xt;
	      y_min = y_max = yt;
	    }
	  else
	    {
	      if (xt < x_min)
		x_min = xt;
	      if (yt < y_min)
		y_min = yt;
	      if (xt > x_max)
		x_max = xt;
	      if (yt > y_max)
		y_max = yt;
	    }
	}
    }
  
  rect->x = floor (x_min);
  rect->width = ceil (x_max) - rect->x;
  rect->y = floor (y_min);
  rect->height = floor (y_max) - rect->y;
}

