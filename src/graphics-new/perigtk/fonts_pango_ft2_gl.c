/* Nsp
 * Copyright (C) 2001-2005 Jean-Philippe Chancelier Enpc/Cermics
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
 *
 * Using pango_ft2 and for opengl fonts drawing
 *
 *--------------------------------------------------------------------------*/

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
      g_object_unref (G_OBJECT (Xgc->private->ft2_context));
    }
}

static void nsp_fonts_initialize(BCG *Xgc)
{
  if ( Xgc->private->layout == NULL) 
    {
      PangoFT2FontMap* pango_fm;
      Xgc->private->context = gtk_widget_get_pango_context (Xgc->private->drawing);
      /* a revoir deprecated XXXX */
#ifdef HAVE_FREETYPE
      pango_fm = (PangoFT2FontMap *) pango_ft2_font_map_new();
      pango_ft2_font_map_set_resolution (pango_fm,72,72);
      Xgc->private->ft2_context= pango_ft2_font_map_create_context(pango_fm);
      g_object_unref(pango_fm);
      /* Xgc->private->ft2_context = pango_ft2_get_context (72, 72); */
      Xgc->private->layout = pango_layout_new (Xgc->private->ft2_context);
      Xgc->private->mark_layout = pango_layout_new (Xgc->private->ft2_context);
#else 
      Xgc->private->layout = NULL;
      Xgc->private->mark_layout = NULL;
#endif 
      Xgc->private->desc = pango_font_description_new();
      Xgc->private->mark_desc = pango_font_description_from_string(pango_fonttab[1]);
    }
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


/* To get the  id and size of the current font 
 */

static void  xget_font(BCG *Xgc,int *font)
{
  font[0] = Xgc->fontId ;
  font[1] = Xgc->fontSize ;
}

/* To get the current mark id */

static void xget_mark(BCG *Xgc,int *symb)
{
  symb[0] = Xgc->CurHardSymb ;
  symb[1] = Xgc->CurHardSymbSize ;
}


static void loadfamily(char *name, int *j)
{ 
}

static void queryfamily(char *name, int *j,int *v3)
{ 
}

/*
 * rendering text with PangoFT2 
 * from: font-pangoft2.c written by Naofumi Yasufuku  
 * <naofumi@users.sourceforge.net>
 * Note that if the layout uses a transformation matrix 
 * the enclosing rectangle is given by e_rect
 */

#ifndef HAVE_FREETYPE

static void gl_pango_ft2_render_layout (PangoLayout *layout,      GdkRectangle *e_rect)
{
  Sciprintf("Cannot render string in OpenGL missing pangoft2\n");
}

#else 

static void gl_pango_ft2_render_layout (PangoLayout *layout,      GdkRectangle *e_rect)
{
  int x,y;
  PangoRectangle logical_rect;
  FT_Bitmap bitmap;
  GLvoid *pixels;
  guint32 *p;
  GLfloat color[4];
  guint32 rgb;
  GLfloat a;
  guint8 *row, *row_end;
  int i;

  pango_layout_get_extents (layout, NULL, &logical_rect);
  if (logical_rect.width == 0 || logical_rect.height == 0)
    return;

  if ( e_rect == NULL )
    {
      bitmap.rows = PANGO_PIXELS (logical_rect.height);
      bitmap.width = PANGO_PIXELS (logical_rect.width);
      bitmap.pitch = bitmap.width;
      x= PANGO_PIXELS (-logical_rect.x);
      y= 0;
    }
  else
    {
      /* rotated string: we use the enclosing rectangle */
      bitmap.rows = e_rect->height;
      bitmap.width = e_rect->width;
      bitmap.pitch = bitmap.width;
      /* note that displaystring will pass e_rect here is such that x=y=0 */
      x = - e_rect->x;
      y = - e_rect->y ;
    }
  bitmap.buffer = g_malloc (bitmap.rows * bitmap.width);
  bitmap.num_grays = 256;
  bitmap.pixel_mode = ft_pixel_mode_grays;

  memset (bitmap.buffer, 0, bitmap.rows * bitmap.width);
  
  pango_ft2_render_layout (&bitmap, layout,x,y);

  pixels = g_malloc (bitmap.rows * bitmap.width * 4);
  p = (guint32 *) pixels;

  glGetFloatv (GL_CURRENT_COLOR, color);
#if !defined(GL_VERSION_1_2) && G_BYTE_ORDER == G_LITTLE_ENDIAN
  rgb =  ((guint32) (color[0] * 255.0))        |
        (((guint32) (color[1] * 255.0)) << 8)  |
        (((guint32) (color[2] * 255.0)) << 16);
#else
  rgb = (((guint32) (color[0] * 255.0)) << 24) |
        (((guint32) (color[1] * 255.0)) << 16) |
        (((guint32) (color[2] * 255.0)) << 8);
#endif
  a = color[3];

  row = bitmap.buffer + bitmap.rows * bitmap.width; /* past-the-end */
  row_end = bitmap.buffer;      /* beginning */

  if (a == 1.0)
    {
      do
        {
          row -= bitmap.width;
          for (i = 0; i < bitmap.width; i++)
#if !defined(GL_VERSION_1_2) && G_BYTE_ORDER == G_LITTLE_ENDIAN
            *p++ = rgb | (((guint32) row[i]) << 24);
#else
            *p++ = rgb | ((guint32) row[i]);
#endif
        }
      while (row != row_end);
    }
  else
    {
      do
        {
          row -= bitmap.width;
          for (i = 0; i < bitmap.width; i++)
#if !defined(GL_VERSION_1_2) && G_BYTE_ORDER == G_LITTLE_ENDIAN
            *p++ = rgb | (((guint32) (a * row[i])) << 24);
#else
            *p++ = rgb | ((guint32) (a * row[i]));
#endif
        }
      while (row != row_end);
    }

  glPixelStorei (GL_UNPACK_ALIGNMENT, 4);

  glEnable (GL_BLEND);
  glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

#if !defined(GL_VERSION_1_2)
  glDrawPixels (bitmap.width, bitmap.rows,
                GL_RGBA, GL_UNSIGNED_BYTE,
                pixels);
#else
  glDrawPixels (bitmap.width, bitmap.rows,
                GL_RGBA, GL_UNSIGNED_INT_8_8_8_8,
                pixels);
#endif

  glDisable (GL_BLEND);

  g_free (bitmap.buffer);
  g_free (pixels);
}

#endif 
