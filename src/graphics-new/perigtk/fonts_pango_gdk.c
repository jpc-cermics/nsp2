/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
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

#define FONTNUMBER 6
#define FONTMAXSIZE 6
#define SYMBOLNUMBER 12

/* Must be of size FONTMAXSIZE */

static const int pango_size[] = { 8 ,10,12,14,18,24};

static char *pango_fonttab[] ={"Courier", "Standard Symbols L","Sans","Sans","Sans","Sans"};

#undef HAVE_FREETYPE

static void nsp_fonts_finalize(BCG *Xgc)
{
  if ( Xgc->private->layout != NULL) 
    {
      g_object_unref ( Xgc->private->layout);Xgc->private->layout=NULL;
      g_object_unref ( Xgc->private->mark_layout);Xgc->private->mark_layout=NULL;
      pango_font_description_free ( Xgc->private->desc); Xgc->private->desc=NULL;
      pango_font_description_free ( Xgc->private->mark_desc);Xgc->private->mark_desc=NULL;
#ifdef HAVE_FREETYPE
      g_object_unref (G_OBJECT (Xgc->private->context));
#endif
    }
}

static void nsp_fonts_initialize(BCG *Xgc)
{
  if ( Xgc->private->layout == NULL) 
    {
#ifdef HAVE_FREETYPE
      PangoFT2FontMap* pango_fm;
      /* a revoir deprecated XXXX */
      pango_fm = (PangoFT2FontMap *) pango_ft2_font_map_new();
      pango_ft2_font_map_set_resolution (pango_fm,72,72);
      Xgc->private->context= pango_ft2_font_map_create_context(pango_fm);
      g_object_unref(pango_fm);
      /* Xgc->private->context = pango_ft2_get_context (72, 72); */
      Xgc->private->layout = pango_layout_new (Xgc->private->context);
      Xgc->private->mark_layout = pango_layout_new (Xgc->private->context);
#else 
      Xgc->private->context = gtk_widget_get_pango_context (Xgc->private->drawing);
      Xgc->private->layout = pango_layout_new (Xgc->private->context);
      Xgc->private->mark_layout = pango_layout_new (Xgc->private->context);
#endif 
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


/* utility to rotate a string */

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


/* 
 * FIXME: flag is unused since the rectangle is drawn in Graphics-IN.c 
 *        to deal with the case when string can be on multiple lines 
 *        but pango could make this directly. 
 * maybe we should change the display an consider that (x,y) is the baseline 
 * of the string not its lower left boundary.
 * Note that if the string contains \n then the layout will have multiple lines 
 * 
 */

static void displaystring(BCG *Xgc,char *str, int x, int y, int flag,double angle)
{
  PangoRectangle ink_rect,logical_rect;
  int  height,width;
  ;
  pango_layout_set_text (Xgc->private->layout, str, -1);
  /*  PangoLayoutLine *line;
   *  nline = pango_layout_get_line_count(Xgc->private->layout); 
   *  if ( nline == 1 ) 
   *    {
   *   / * we want (x,y) to be at the baseline of the first string position * /
   *   line = pango_layout_get_line(Xgc->private->layout,0);
   *   pango_layout_line_get_extents(line, &ink_rect,&logical_rect);
   *   height = - logical_rect.y/PANGO_SCALE;
   *   width = logical_rect.width/PANGO_SCALE;
   */
  /* used to position the descent of the last line of layout at y */
  pango_layout_get_pixel_size (Xgc->private->layout, &width, &height); 
  if ( Abs(angle) >= 0.1) 
    {
      double xt,yt;
      GdkRectangle rect;
      PangoMatrix matrix = PANGO_MATRIX_INIT; 
      pango_matrix_rotate (&matrix, - angle );
      pango_context_set_matrix (Xgc->private->context, &matrix);
      pango_layout_context_changed (Xgc->private->layout);
      pango_layout_get_extents(Xgc->private->layout,&ink_rect,&logical_rect);
      /* 
       * in gdk_draw_layout x and y specify the position of the top left corner 
       * of the bounding box (in device space) of the transformed layout. 
       * Here when alpha = 0, (x,y) is the lower left point of the bounding box 
       * of the string we want the string to rotate around this point. 
       * thus we cannot call gdk_draw_layout with (x,y) directly.
       */
      xt = 0 * matrix.xx + -height * matrix.xy + matrix.x0;
      yt = 0 * matrix.yx + -height * matrix.yy + matrix.y0;
      get_rotated_layout_bounds (Xgc->private->layout,Xgc->private->context, 
				 &matrix,&rect);
      gdk_draw_layout (Xgc->private->drawable,Xgc->private->wgc,
		       x+rect.x+xt,y+rect.y+yt,Xgc->private->layout);
      pango_context_set_matrix (Xgc->private->context,NULL);
      pango_layout_context_changed (Xgc->private->layout);
      if (1) 
	{
	  /* draw the bounding box */
	  int vx[]={x,x,x,x},vy[]={y,y,y,y};
	  double dx,dy;
	  dx =  0 * matrix.xx + -height * matrix.xy + matrix.x0;
	  dy =  0 * matrix.yx + -height * matrix.yy + matrix.x0;
	  vx[1] += dx; vy[1] += dy;
	  dx =  width * matrix.xx + -height * matrix.xy + matrix.x0;
	  dy =  width * matrix.yx + -height * matrix.yy + matrix.x0;
	  vx[2] += dx; vy[2] += dy;
	  dx =  width * matrix.xx + 0 * matrix.xy + matrix.x0;
	  dy =  width * matrix.yx + 0 * matrix.yy + matrix.x0;
	  vx[3] += dx; vy[3] += dy;
	  drawpolyline(Xgc,vx, vy,4,1);
	}
    }
  else
    {
      /* horizontal string */
      if (0)
	gdk_draw_rectangle(Xgc->private->drawable, Xgc->private->wgc, FALSE,x,y - height,width,height);
      gdk_draw_layout (Xgc->private->drawable,Xgc->private->wgc,x,y - height,Xgc->private->layout);
    }
}


static void boundingbox(BCG *Xgc,char *string, int x, int y, int *rect)
{
  int width, height;
  pango_layout_set_text (Xgc->private->layout, string, -1);
  pango_layout_get_pixel_size (Xgc->private->layout, &width, &height); 
  rect[0]=x;rect[1]=y+height;rect[2]=width;rect[3]=height;
}

static void draw_mark(BCG *Xgc,int *x, int *y)
{
  double dx,dy;
  PangoRectangle ink_rect;
  int code = symbols[Xgc->CurHardSymb]; 
  gchar symbol_code[4], *iter = symbol_code;
  ;
  g_unichar_to_utf8(code, iter);
  iter = g_utf8_next_char(iter);
  g_unichar_to_utf8(0x0, iter);
  pango_layout_set_text (Xgc->private->mark_layout,symbol_code, -1);
  pango_layout_get_extents(Xgc->private->mark_layout,&ink_rect,NULL);
  dx = PANGO_PIXELS(( ink_rect.x + ink_rect.width/2.0));
  dy = PANGO_PIXELS(( ink_rect.y + ink_rect.height/2.0));
  gdk_draw_layout (Xgc->private->drawable,Xgc->private->wgc,*x-dx,*y-dy,Xgc->private->mark_layout);
  if (0) 
    {
      /* draw the ink_rectangle aroud the mark */
      int i;
      double rect[]={ink_rect.x,ink_rect.y,ink_rect.width,ink_rect.height};
      int myrect[]={*x-dx,*y-dy,0,0};
      for ( i=0; i < 4 ; i++) myrect[i] += PANGO_PIXELS(rect[i]);
      drawrectangle(Xgc,myrect);
    }
}


void xstring_pango(BCG *Xgc,char *str,int rect[],char *font,int size,int markup,int position)
{
  
}




