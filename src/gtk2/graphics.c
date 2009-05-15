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
 * A set of functions making links between 
 * nsp graphics and gtk object i.e gdkpixbuf gtkimage etc...
 *
 */

#include <gtk/gtk.h> 
#include <gdk/gdk.h>

#include "nsp/object.h"
#include "nsp/gtk/gdkpixbuf.h"
#include "nsp/interf.h"

#if 0
static void nsp_pixbuf_to_ps(FILE *psout,GdkPixbuf *pixbuf,gint xdest, gint ydest);
#endif 

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
 * If the cells contains only one matrix or the argument is 
 * a matrix. Then the matrix is used for R,G,and B
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

  if ( IsCellsObj(stack,1) )
    {
      if ((C= GetImageCells(stack,1)) == NULL) return RET_BUG;
      if ( C->mn != 3 && C->mn != 4 && C->mn != 1 ) 
	{
	  Scierror( "Error: %s cells must contains 1 (R=G=B) or 3 (RGB) or 4 (RGBA) matrices \n", NspFname(stack));
	  return RET_BUG;
	}
      width = ((NspMatrix *) C->objs[0])->n;
      height = ((NspMatrix *) C->objs[0])->m;
      pixbuf_mode =  GDK_COLORSPACE_RGB;
      if ((pix = gdk_pixbuf_new(pixbuf_mode,FALSE, 8, width,height)) == NULL) 
	{
	  Scierror("Error: %f error in pixbuf new\n",NspFname(stack));
	  return RET_BUG;
	}
      rowstride = gdk_pixbuf_get_rowstride (pix);
      pixels = gdk_pixbuf_get_pixels (pix);
      
      for(ch = 0; ch < Max(C->mn,3) ; ch++) 
	for(col =0; col < width; col++)
	  for(row = 0; row < height; row++)
	    {
	      p =  pixels + row * rowstride + col * Max(C->mn,3);
	      *(p+ch) = (guchar) ((NspMatrix *) C->objs[(C->mn==1) ? 0 : ch])->R[row+height*col];
	    }
      nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
      if ((ret = (NspObject *) gobject_create(NVOID,(GObject *)pix, (NspTypeBase *) nsp_type_gdkpixbuf))== NULL) 
	return RET_BUG;
    }
  else
    {
      NspMatrix *M;
      if ((M=GetRealMat(stack,1))== NULL) return RET_BUG;
      width = M->n;
      height = M->m;
      pixbuf_mode =  GDK_COLORSPACE_RGB;
      if ((pix = gdk_pixbuf_new(pixbuf_mode,FALSE, 8, width,height)) == NULL) 
	{
	  Scierror("Error: %f error in pixbuf new\n",NspFname(stack));
	  return RET_BUG;
	}
      rowstride = gdk_pixbuf_get_rowstride (pix);
      pixels = gdk_pixbuf_get_pixels (pix);
      
      for(ch = 0; ch < 3 ; ch++) 
	for(col =0; col < width; col++)
	  for(row = 0; row < height; row++)
	    {
	      p =  pixels + row * rowstride + col * Max(C->mn,3);
	      *(p+ch) = (guchar) M->R[row+height*col];
	    }
      nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
      if ((ret = (NspObject *) gobject_create(NVOID,(GObject *)pix, (NspTypeBase *) nsp_type_gdkpixbuf))== NULL) 
	return RET_BUG;
    }
  MoveObj(stack,1,ret);
  return 1;
}  


/**
 * int_pixbuf_set_from_cells:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * Return value: 
 **/

int int_pixbuf_set_from_cells(Stack stack, int rhs, int opt, int lhs)
{
  GdkPixbuf *pix;
  NspGdkPixbuf *nsp_pix;
  NspCells *C;
  int nChannels,width,height,rowstride,ch,col,row;
  guchar *pixels, *p;
  CheckRhs(2, 2);
  CheckLhs(1, 1);

  if ((nsp_pix = GetGdkPixbuf(stack,1)) == NULL) return RET_BUG;

  pix = GDK_PIXBUF(nsp_pix->obj);
  nChannels = gdk_pixbuf_get_n_channels(pix);
  width = gdk_pixbuf_get_width(pix);
  height = gdk_pixbuf_get_height(pix);
  rowstride = gdk_pixbuf_get_rowstride (pix);
  pixels = gdk_pixbuf_get_pixels (pix);

  if ((C= GetImageCells(stack,2)) == NULL) return RET_BUG;
  if ( C->mn != 3 && C->mn != 4 && C->mn != 1 ) 
    {
      Scierror( "Error: %s cells must contains 1 (R=G=B) or 3 (RGB) or 4 (RGBA) matrices \n", 
		NspFname(stack));
      return RET_BUG;
    }
  if ( ((NspMatrix *) C->objs[0])->n != width 
       || ((NspMatrix *) C->objs[0])->m != height )
    {
      Scierror("Error: %s cells must contains %dx%d matrices \n", 
	       NspFname(stack),width,height);
      return RET_BUG;
    }
  for(ch = 0; ch < Max(C->mn,3) ; ch++) 
    for(col =0; col < width; col++)
      for(row = 0; row < height; row++)
	{
	  p =  pixels + row * rowstride + col * Max(C->mn,3);
	  *(p+ch) = (guchar) ((NspMatrix *) C->objs[(C->mn==1) ? 0 : ch])->R[row+height*col];
	}
  return 0;
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

int int_pixbuf_get_channel(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  GdkPixbuf *pix;
  NspGdkPixbuf *nsp_pix;
  int nChannels,width,height,rowstride,ch,col,row,channel=1;
  guchar *pixels, *p;
  CheckStdRhs(1, 2);
  CheckLhs(1, 1);

  if ((nsp_pix = GetGdkPixbuf(stack,1)) == NULL) return RET_BUG;
  if ( rhs == 2 ) 
    {
      if (GetScalarInt(stack,2,&channel) == FAIL) return RET_BUG;
    }
  
  pix = GDK_PIXBUF(nsp_pix->obj);
  nChannels = gdk_pixbuf_get_n_channels(pix);
  width = gdk_pixbuf_get_width(pix);
  height = gdk_pixbuf_get_height(pix);
  rowstride = gdk_pixbuf_get_rowstride (pix);
  pixels = gdk_pixbuf_get_pixels (pix);

  if ( channel < 1 || channel > nChannels) 
    {
      Scierror("Error: %s channel argument should be in the range [1,%d]\n", 
	       NspFname(stack),nChannels);
      return RET_BUG;
    }
  if ((M = nsp_matrix_create(NVOID,'r',height,width))== NULLMAT) return RET_BUG;

  /* fills the matrix C */
  ch = channel-1;
  for(col =0; col < width; col++)
    for(row = 0; row < height; row++)
      {
	p =  pixels + row * rowstride + col * nChannels;
	M->R[row+height*col] = (double) *(p+ch);
      }
  MoveObj(stack,1,NSP_OBJECT(M));
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
  int i,nRow=0,nCol=0;
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

#if 0
static void nsp_pixbuf_to_ps(FILE *psout,GdkPixbuf *pixbuf,gint xdest, gint ydest)
{
  int row,col,ch;
  gdouble scale_x=1, scale_y=1;
  guchar *pixels, *p;
  int nChannels = gdk_pixbuf_get_n_channels(pixbuf);
  int width = gdk_pixbuf_get_width(pixbuf);
  int height = gdk_pixbuf_get_height(pixbuf);
  int rowstride = gdk_pixbuf_get_rowstride (pixbuf);
  pixels = gdk_pixbuf_get_pixels (pixbuf);

  fprintf(psout, "gsave\n");
  fprintf(psout, "%d %g translate\n", xdest, ydest + height * scale_y);
  fprintf(psout, "%g %g scale\n",width * scale_x, height * scale_y);
  fprintf(psout, "%d %d 8 [%d 0 0 %d 0 %d]\n",width, height, width, height, height);
  fprintf(psout, "/scanline %d 3 mul string def\n", width);
  fprintf(psout, "{ currentfile scanline readhexstring pop } false 3\n");
  fprintf(psout, "colorimage\n");

  for(row = height-1 ; row >= 0 ; row-- )
    {
      for(col =0; col < width; col++)
	{
	  p =  pixels + row * rowstride + col * nChannels;
	  for ( ch = 0 ; ch < 3 ; ch++)
	    {
	      guchar n= *(p+ch),n1,n2;
	      n1 = n & 0x0f; 
	      n2 = (n & 0xf0 ) >> 4;
	      fprintf(psout,"%c",(n1 < 10) ?  '0' + n1 : 'A' + n1 - 10);
	      fprintf(psout,"%c",(n2 < 10) ?  '0' + n2 : 'A' + n2 - 10);
	    }
	  if(fmod(col + 1, 13) == 0) fprintf(psout, "\n");
	}
      fprintf(psout,"\n");
    }
  fprintf(psout, "grestore\n");
}
#endif 
