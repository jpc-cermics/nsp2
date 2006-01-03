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
 * A set of functions making links between 
 * nsp graphics and gtk object i.e gdkpixbuf gtkimage etc...
 *
 */

#include <gtk/gtk.h> 
#include <gdk/gdk.h>

#include "nsp/object.h"
#include "nsp/gtk/gdkpixbuf.h"
#include "nsp/interf.h"

static NspCells *GetImageCells(Stack stack,int pos) ;

/**
 * int_cellstopixbuf:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * converts a cells containing 3 or four matrices 
 * to a GtkPixbuf. The matrices must be of same sizes
 * and contains data in the range [0,255];
 * the cells is used as a RGB or RGBA data for the pixbuf
 * 
 * Return value: 
 **/

int int_cellstopixbuf(Stack stack, int rhs, int opt, int lhs)
{
  NspCells *C;
  int width,height,pixbuf_mode,rowstride,ch,col,row;
  NspObject *ret;
  guchar *pixels, *p;
  GdkPixbuf* pix;

  CheckRhs(1, 1);
  CheckLhs(1, 1);

  if ((C= GetImageCells(stack,1)) == NULL) return RET_BUG;
  if ( C->mn != 3 && C->mn != 4) 
    {
      Scierror( "Error: %s cells must contains 3 (RGB) or 4 (RGBA) matrices \n", stack.fname);
      return RET_BUG;
    }
  width = ((NspMatrix *) C->objs[0])->n;
  height = ((NspMatrix *) C->objs[0])->m;
  pixbuf_mode =  GDK_COLORSPACE_RGB;
  if ((pix = gdk_pixbuf_new(pixbuf_mode,FALSE, 8, width,height)) == NULL) 
    {
      Scierror("Error: %f error in pixbuf new\n",stack.fname);
      return RET_BUG;
    }
  rowstride = gdk_pixbuf_get_rowstride (pix);
  pixels = gdk_pixbuf_get_pixels (pix);

  for(ch = 0; ch < C->mn ; ch++) 
    for(col =0; col < width; col++)
      for(row = 0; row < height; row++)
	{
	  p =  pixels + row * rowstride + col * C->mn;
	  *(p+ch) = (guchar) ((NspMatrix *) C->objs[ch])->R[row+height*col];
	}
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((ret = (NspObject *) gobject_create(NVOID,(GObject *)pix, (NspTypeBase *) nsp_type_gdkpixbuf))== NULL) 
    return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;
}  


/**
 * int_pixbuftocells:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * extract RGB or RGBA as a nsp cells {R,G,B,A}
 * R,G,B,A are double matrices .... 
 * 
 * Return value: 
 **/

int int_pixbuftocells(Stack stack, int rhs, int opt, int lhs)
{
  GdkPixbuf *pix;
  NspGdkPixbuf *nsp_pix;
  NspCells *C;
  int nChannels,width,height,rowstride,ch,col,row,i;
  guchar *pixels, *p;
  CheckRhs(1, 1);
  CheckLhs(1, 1);

  if ((nsp_pix = GetGdkPixbuf(stack,1)) == NULL) return RET_BUG;
  pix = GDK_PIXBUF(nsp_pix->obj);
  nChannels = gdk_pixbuf_get_n_channels(pix);
  width = gdk_pixbuf_get_width(pix);
  height = gdk_pixbuf_get_height(pix);
  rowstride = gdk_pixbuf_get_rowstride (pix);
  pixels = gdk_pixbuf_get_pixels (pix);

  /* create a NspCells */
  if ( (C =nsp_cells_create(NVOID,1,nChannels)) == NULLCELLS) return RET_BUG;
  for ( i = 0 ; i < nChannels ; i++ ) 
    {
      NspMatrix *mat;
      if ((mat = nsp_matrix_create("ce",'r',height,width))== NULLMAT) return RET_BUG;
      C->objs[i]= NSP_OBJECT(mat);
    }
  /* fills the cell C */

  for(ch = 0; ch < C->mn ; ch++) 
    for(col =0; col < width; col++)
      for(row = 0; row < height; row++)
	{
	  p =  pixels + row * rowstride + col * nChannels;
	  ((NspMatrix *) C->objs[ch])->R[row+height*col] = (double) *(p+ch);
	}
  MoveObj(stack,1,NSP_OBJECT(C));
  return 1;
}  


/**
 * GetImageCells:
 * @stack: 
 * @pos: 
 * 
 * get a NspCells composed of 3 or 4 real matrices of same size 
 * 
 * Return value: 
 **/

static NspCells *GetImageCells(Stack stack,int pos) 
{
  int i,nRow,nCol;
  NspCells *C; 
  if ((C = GetCells(stack,pos)) == NULL) return NULL;
  /* first checks that all the matrices in the cell are of same dimension */
  for ( i = 0 ; i < C->mn ; i++) 
    {
      if ( check_cast(C->objs[i],nsp_type_matrix_id) == FALSE) 
	{
	  Scierror("Error: element %d of cell array is not a Matrix\n",i+1);
	  return NULL;
	}
      if ( i==0) 
	{  nRow = ((NspMatrix *) C->objs[i])->m; nCol = ((NspMatrix *) C->objs[i])->n;}
      else 
	{
	  if (( nRow != ((NspMatrix *) C->objs[i])->m) || nCol != ((NspMatrix *) C->objs[i])->n) 
	    {
	      Scierror("Error: cell array elements should be of the same size\n",i+1);
	      return NULL;
	    }
	}
      if ( ((NspMatrix *) C->objs[i])->rc_type != 'r') 
	{
	  Scierror("Error: cell array elements should be a real matrix\n",i+1);
	  return NULL;
	}
    }
  return C;
}
  




