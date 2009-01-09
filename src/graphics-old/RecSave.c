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
 * save graphic code in a file  
 * --------------------------------------------------------------------------*/

#include <string.h> /* in case of dbmalloc use */
#include <stdio.h>
#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"
#include "nsp/graphics/Rec.h"

static int save_D  (XDR *xdrs,double x);
static int save_LI (XDR *xdrs,int ix);
static int save_C (XDR *xdrs,char *c, int lc); /* save a string with know size which is
						* not to be realocated at recload 
						*/
static int save_VectLI (XDR *xdrs,int *nx, int l);
static int save_VectF (XDR *xdrs,double *nx, int l); 
static int save_VectC (XDR *xdrs,char *nx, int l); /* FIXME: second argument is to be removed */
static int save_VectS (XDR *xdrs,char **nx);

static int save_colormap (BCG *Xgc,void *);
static int save_Ech  (BCG *Xgc,void *); 
static int save_Plot  (BCG *Xgc,void *); 
static int save_SciAxis  (BCG *Xgc,void *); 
static int save_Grid  (BCG *Xgc,void *); 
static int save_Param3D  (BCG *Xgc,void *); 
static int save_Param3D1  (BCG *Xgc,void *); 
static int save_Plot3D  (BCG *Xgc,void *); 
static int save_Fac3D  (BCG *Xgc,void *); 
static int save_Fec  (BCG *Xgc,void *); 
static int save_Contour  (BCG *Xgc,void *); 
static int save_Contour2D  (BCG *Xgc,void *); 
static int save_Gray  (BCG *Xgc,void *); 
static int save_Champ  (BCG *Xgc,void *); 

/*---------------------------------------------------------------------
 * basic primitives 
 *---------------------------------------------------------------------------*/

static int save_clipping_p(BCG *Xgc,void *theplot)
{
  struct rec_int4 *lplot  = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectLI(Xgc->xdrs,lplot->vals,4) == 0) return(0);
  return 1;
}

static int save_clipgrf(BCG *Xgc,void * theplot ) 
{
  if ( save_LI(Xgc->xdrs,((struct rec_void *) theplot)->code)==0) return(0);
  return 1;
}

static int save_int(BCG *Xgc,void *theplot) 
{
  struct rec_int *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->val) == 0) return(0);
  return 1;
}

static int save_alufunction1(BCG *Xgc,void *theplot) { return save_int(Xgc,theplot); }

static int save_background(BCG *Xgc,void *theplot)  { return save_int(Xgc,theplot); }

static int save_unclip(BCG *Xgc,void * theplot )
{
  struct rec_void *lplot = theplot ;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  return 1;
}

static int save_test(BCG *Xgc,void * theplot )
{
  struct rec_void *lplot = theplot ;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  return 1;
}


static int save_clip(BCG *Xgc,void *theplot)
{
  struct rec_int4 *lplot  = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectLI(Xgc->xdrs,lplot->vals,4) == 0) return(0);
  return 1;
}

static int save_pattern(BCG *Xgc,void *theplot)  { return save_int(Xgc,theplot); }

static int save_font_size(BCG *Xgc,void *theplot)  { return save_int(Xgc,theplot); }


static int save_font(BCG *Xgc,void *theplot)
{
  struct rec_int2 *lplot  = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->val) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->val1) == 0) return(0);
  return 1;
}

static int save_foreground(BCG *Xgc,void *theplot)  { return save_int(Xgc,theplot); }

static int save_hidden3d(BCG *Xgc,void *theplot)  { return save_int(Xgc,theplot); }

static int save_absourel(BCG *Xgc,void *theplot)  { return save_int(Xgc,theplot); }

static int save_dash(BCG *Xgc,void *theplot)  { return save_int(Xgc,theplot); }

static int save_mark_size(BCG *Xgc,void *theplot)  { return save_int(Xgc,theplot); }

static int save_mark(BCG *Xgc,void *theplot)  
{
  struct rec_int2 *lplot  = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->val) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->val1) == 0) return(0);
  return 1;
}

static int save_pixmapOn(BCG *Xgc,void *theplot)  { return save_int(Xgc,theplot); }
static int save_thickness(BCG *Xgc,void *theplot)  { return save_int(Xgc,theplot); }
static int save_usecolor(BCG *Xgc,void *theplot)  { return save_int(Xgc,theplot); }

static int save_show(BCG *Xgc,void * theplot ) 
{
  if ( save_LI(Xgc->xdrs,((struct rec_void *) theplot)->code)==0) return(0);
  return 1;
}

static int save_pixmapclear(BCG *Xgc,void * theplot )
{
  if ( save_LI(Xgc->xdrs,((struct rec_void *) theplot)->code)==0) return(0);
  return 1;
}

static int save_fpf(BCG *Xgc,void * theplot )
{
  struct rec_str* lplot = theplot;
  if ( save_C(Xgc->xdrs, lplot->str,strlen(lplot->str)) == 0) return(0);
  return 1;
}

static int save_fpf_def(BCG *Xgc,void * theplot )
{
  if ( save_LI(Xgc->xdrs,((struct rec_void *) theplot)->code)==0) return(0);
  return 1;
}


static int save_init(BCG *Xgc,void * theplot ) 
{
  if ( save_LI(Xgc->xdrs,((struct rec_void *) theplot)->code)==0) return(0);
  return 1;
}


/*-----------------------------------------------------------------------------
 * colormap
 *-----------------------------------------------------------------------------*/

static int save_colormap(BCG *Xgc,void *the_plot)
{
  struct rec_colormap *lplot = the_plot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->m)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n)==0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->colors,lplot->m*lplot->n) == 0) return(0);
  return 1;
}

static int save_default_colormap(BCG *Xgc,void *the_plot)
{
  if ( save_LI(Xgc->xdrs,((struct rec_void *) the_plot)->code)==0) return(0);
  return 1;
}


/*-----------------------------------------------------------------------------
 *  drawarc_1
 *-----------------------------------------------------------------------------*/

static int save_drawarc_1(BCG *Xgc,void  *theplot)
{
  int i;
  struct rec_drawarc *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  for ( i = 0 ; i < 6 ; i++) 
    {
      if ( save_D(Xgc->xdrs,lplot->arc[i]) == 0) return(0);
    }
  return 1;
}

/*-----------------------------------------------------------------------------
 * 
 *-----------------------------------------------------------------------------*/

static int save_fillarcs_1(BCG *Xgc,void  *theplot)
{
  struct rec_fillarcs *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vects,lplot->n) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,lplot->fillvect,lplot->n) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  
 *-----------------------------------------------------------------------------*/

static int save_drawarcs_1(BCG *Xgc,void  *theplot)
{
  struct rec_fillarcs *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vects,lplot->n) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,lplot->fillvect,lplot->n) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  
 *-----------------------------------------------------------------------------*/

static int save_fillpolyline_1(BCG *Xgc,void  *theplot)
{
  struct rec_fillpolyline *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->closeflag) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vx,lplot->n) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vy,lplot->n) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  arrows
 *-----------------------------------------------------------------------------*/

static int save_drawarrows_1(BCG *Xgc,void  *theplot)
{
  struct rec_arrows *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->iflag) == 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->as) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vx,lplot->n) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vy,lplot->n) == 0) return(0);
  if ( lplot->iflag != 0 )
    { 
      if ( save_VectLI(Xgc->xdrs,lplot->style,lplot->n/2) == 0) return(0);
    }
  else 
    {
      if ( save_LI(Xgc->xdrs,lplot->def_style) == 0) return(0);
    }
  return 1;
}

/*-----------------------------------------------------------------------------
 * axis 
 *-----------------------------------------------------------------------------*/

static int save_drawaxis_1(BCG *Xgc,void  *theplot)
{
  struct rec_drawaxis *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_D(Xgc->xdrs,lplot->alpha) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->nsteps) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->initpoint,2) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->size,3) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  cleararea
 *-----------------------------------------------------------------------------*/

static int save_cleararea_1(BCG *Xgc,void  *theplot)
{
  struct rec_double4 *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vals,4) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *   fillarc
 *-----------------------------------------------------------------------------*/

static int save_fillarc_1(BCG *Xgc,void  *theplot)
{
  int i;
  struct rec_drawarc *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  for ( i = 0 ; i < 6 ; i++) 
    {
      if ( save_D(Xgc->xdrs,lplot->arc[i]) == 0) return(0);
    }
  return 1;
}

/*-----------------------------------------------------------------------------
 *  fillrectangle
 *-----------------------------------------------------------------------------*/

static int save_fillrectangle_1(BCG *Xgc,void  *theplot)
{
  struct rec_double4 *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vals,4) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  drawpolyline
 *-----------------------------------------------------------------------------*/

static int save_drawpolyline_1(BCG *Xgc,void  *theplot)
{
  struct rec_fillpolyline *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->closeflag) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vx,lplot->n) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vy,lplot->n) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  fillpolylines
 *-----------------------------------------------------------------------------*/

static int save_fillpolylines_1(BCG *Xgc,void  *theplot)
{
  struct rec_fillpolylines *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->p) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->v1) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vx,lplot->n*lplot->p) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vy,lplot->n*lplot->p) == 0) return(0);
  if ( lplot->v1 == 2 ) 
    {
      if ( save_VectLI(Xgc->xdrs,lplot->fillvect,lplot->n*lplot->p) == 0) return(0);
    }
  else 
    {
      if ( save_VectLI(Xgc->xdrs,lplot->fillvect,lplot->n) == 0) return(0);
    }
  return 1;
}

/*-----------------------------------------------------------------------------
 *  drawpolymark
 *-----------------------------------------------------------------------------*/

static int save_drawpolymark_1(BCG *Xgc,void  *theplot)
{
  struct rec_fillpolyline *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->closeflag) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vx,lplot->n) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vy,lplot->n) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  displaynumbers
 *-----------------------------------------------------------------------------*/

static int save_displaynumbers_1(BCG *Xgc,void  *theplot)
{
  struct rec_displaynumbers *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->flag) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->x,lplot->n) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->y,lplot->n) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->z,lplot->n) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->alpha,lplot->n) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *   drawpolylines
 *-----------------------------------------------------------------------------*/

static int save_drawpolylines_1(BCG *Xgc,void  *theplot)
{
  struct rec_drawpolylines *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->p) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vx,lplot->n*lplot->p) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vy,lplot->n*lplot->p) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,lplot->drawvect,lplot->n) == 0) return(0);
  return 1;
}


/*-----------------------------------------------------------------------------
 *   drawrectangle
 *-----------------------------------------------------------------------------*/

static int save_drawrectangle_1(BCG *Xgc,void  *theplot)
{
  struct rec_double4 *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vals,4) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *   drawrectangles
 *-----------------------------------------------------------------------------*/

static int save_drawrectangles_1(BCG *Xgc,void  *theplot)
{
  struct rec_fillarcs *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vects,lplot->n) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,lplot->fillvect,lplot->n) == 0) return(0);
  return 1;
}


/*-----------------------------------------------------------------------------
 *  drawsegments
 *-----------------------------------------------------------------------------*/

static int save_drawsegments_1(BCG *Xgc,void  *theplot)
{
  struct rec_segment *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vx,lplot->n) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->vy,lplot->n) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->iflag) == 0) return(0);
  if ( lplot->iflag != 0 ) 
    {
      if ( save_VectLI(Xgc->xdrs,lplot->style,lplot->n/2) == 0) return(0);
    }
  else 
    {
      if ( save_VectLI(Xgc->xdrs,lplot->style,1) == 0) return(0);
    }
  return 1;
}

/*-----------------------------------------------------------------------------
 *  displaystring
 *-----------------------------------------------------------------------------*/

static int save_displaystring_1(BCG *Xgc,void  *theplot)
{
  struct rec_displaystring *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectC(Xgc->xdrs,lplot->string,strlen(lplot->string)+1) == 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->x) == 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->y) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->flag) == 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->angle) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  displaystringa
 *-----------------------------------------------------------------------------*/

static int save_displaystringa_1(BCG *Xgc,void  *theplot)
{
  struct rec_displaystringa *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectC(Xgc->xdrs,lplot->string,strlen(lplot->string)+1) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->ipos) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 * a string in a bounded box : with font size change to fit into the 
 * specified box (only works with driver which properly estimate string sizes)
 *-----------------------------------------------------------------------------*/

static int save_xstringb_1(BCG *Xgc,void  *theplot)
{
  struct rec_xstringb *lplot = theplot;
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectC(Xgc->xdrs,lplot->string,strlen(lplot->string)+1) == 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->x) == 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->y) == 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->wd) == 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->hd) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->flag) == 0) return(0);
  return 1;
}

/*---------------------------------------------------------------------
 * scale 
 * ---------------------------------------------------------------------------*/

static int save_Ech(BCG *Xgc,void *plot)
{
  struct rec_scale *lplot = (struct rec_scale *) plot;
  if ( save_C(Xgc->xdrs,lplot->logflag,2L)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->Wrect),4L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->Frect),4L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->Frect_kp),4L) == 0) return(0);
  return(1);
}

static int save_NEch(BCG *Xgc,void *plot)
{
  struct rec_nscale *lplot = (struct rec_nscale *) plot;
  if ( save_C(Xgc->xdrs,lplot->logflag,2L)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->flag),((int)strlen(lplot->flag))+1) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->Wrect),4L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->Frect),4L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->Arect),4L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->Frect_kp),4L) == 0) return(0);
  return(1);
}
  
/*---------------------------------------------------------------------
 * plot2d 
 *---------------------------------------------------------------------------*/

static int save_Plot(BCG *Xgc,void *plot)
{
  int n=0, nstyle;
  struct rec_plot2d *lplot = (struct rec_plot2d *) plot;
  if (lplot->n1==1 ) nstyle= lplot->n1+1;else nstyle= lplot->n1;
  switch (lplot->xf[0])
    {
    case 'g': n=(lplot->n1)*(lplot->n2);break;
    case 'e': n=0;break;
    case 'o': n=(lplot->n2);break;
    }
  if ( save_LI(Xgc->xdrs,lplot->n1)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n2)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->xf),((int)strlen(lplot->xf))+1) == 0) return(0);
  if ( ((n == 0) ? 1 : save_VectF(Xgc->xdrs,(lplot->x),n)) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->y),(lplot->n1)*(lplot->n2)) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,(lplot->style),nstyle) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->strflag),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->strflag_kp),((int)strlen(lplot->strflag_kp))+1) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->legend),((int)strlen(lplot->legend))+1) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->legend_pos) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->brect),4L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->brect_kp),4L) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,(lplot->aint),4L) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,(lplot->aint_kp),4L) == 0) return(0);
  return(1);
}
  
/*---------------------------------------------------------------------
 * axis 
 *---------------------------------------------------------------------------*/

static int save_SciAxis(BCG *Xgc,void *plot)
{
  struct rec_sciaxis *lplot = (struct rec_sciaxis *) plot;
  char type[3] ;
  type[0] = lplot->pos;
  type[1] = lplot->xy_type;
  type[2] = lplot->logflag;
  if ( save_C(Xgc->xdrs,type,3L)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->nx)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->ny)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->subtics)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->fontsize)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->textcolor)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->ticscolor)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->seg_flag)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->f_l)==0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( lplot->f_l == 1) { if ( save_VectC(Xgc->xdrs,(lplot->format),((int)strlen(lplot->format))+1) == 0) return(0);} 
  if ( save_VectF(Xgc->xdrs,lplot->x,lplot->nx) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,lplot->y,lplot->ny) == 0) return(0);
  if ( save_VectS(Xgc->xdrs,lplot->str) == 0) return(0);
  return(1);
}

/*---------------------------------------------------------------------
 * xgrid 
 *---------------------------------------------------------------------------*/

static int save_Grid(BCG *Xgc,void *plot)
{
  struct rec_xgrid *lplot = (struct rec_xgrid *) plot;
  if ( save_LI(Xgc->xdrs,lplot->style)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  return(1);
}


/*---------------------------------------------------------------------
 * param3d 
 *---------------------------------------------------------------------------*/

static int save_Param3D(BCG *Xgc,void *plot)
{
  struct rec_param3d *lplot = (struct rec_param3d *) plot;
  if ( save_LI(Xgc->xdrs,lplot->n)== 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->teta)== 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->alpha)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->x),lplot->n) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->y),lplot->n) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->z),lplot->n) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->legend), ((int)strlen(lplot->legend))+1) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,(lplot->flag),3L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->bbox),6L)== 0) return(0);
  return(1);
}

static int save_Param3D1(BCG *Xgc,void *plot)
{
  struct rec_param3d1 *lplot = (struct rec_param3d1 *) plot;
  if ( save_LI(Xgc->xdrs,lplot->m)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->iflag)== 0) return(0);
  if ( lplot->iflag == 1) 
    if ( save_VectLI(Xgc->xdrs,(lplot->colors),lplot->n) == 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->teta)== 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->alpha)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->x),lplot->n*lplot->m) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->y),lplot->n*lplot->m) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->z),lplot->n*lplot->m) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->legend), ((int)strlen(lplot->legend))+1) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,(lplot->flag),3L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->bbox),6L)== 0) return(0);
  return(1);
}


/*---------------------------------------------------------------------
 * plot3d  
 *---------------------------------------------------------------------------*/

static int save_Plot3D(BCG *Xgc,void *plot)
{
  struct rec_plot3d *lplot = (struct rec_plot3d *) plot;
  if ( save_LI(Xgc->xdrs,lplot->p)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->q)== 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->teta)== 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->alpha)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->x),lplot->p) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->y),lplot->q) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->z),(lplot->p)*(lplot->q)) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->legend), ((int)strlen(lplot->legend))+1) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,(lplot->flag),3L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->bbox),6L)== 0) return(0);
  return(1);
}


/*---------------------------------------------------------------------
 * fac3d 
 *---------------------------------------------------------------------------*/

static int save_Fac3D(BCG *Xgc,void *plot)
{
  struct rec_fac3d *lplot = (struct rec_fac3d *) plot;
  if ( save_LI(Xgc->xdrs,lplot->p)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->q)== 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->teta)== 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->alpha)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( lplot->code == CODEFac3D2) 
    if ( save_VectLI(Xgc->xdrs,(lplot->cvect),(lplot->q)) == 0) return(0);
  if ( lplot->code == CODEFac3D3) 
    if ( save_VectLI(Xgc->xdrs,(lplot->cvect),(lplot->p)*(lplot->q)) == 0) return(0); /****** added by polpoth 4/5/2000 ******/
  if ( save_VectF(Xgc->xdrs,(lplot->x),(lplot->p)*(lplot->q)) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->y),(lplot->p)*(lplot->q)) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->z),(lplot->p)*(lplot->q)) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->legend), ((int)strlen(lplot->legend))+1) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,(lplot->flag),3L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->bbox),6L)== 0) return(0);
  return(1);
}

/*---------------------------------------------------------------------
 * fec 
 *---------------------------------------------------------------------------*/

static int save_Fec(BCG *Xgc,void *plot)
{
  struct rec_fec *lplot = (struct rec_fec *) plot;
  if ( save_LI(Xgc->xdrs,lplot->Nnode)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->Ntr)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->x),lplot->Nnode) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->y),lplot->Nnode) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->triangles),(lplot->Ntr)*5) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->func),lplot->Nnode ) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->brect),4L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->brect_kp),4L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->zminmax),2L) == 0) return(0);   /* added by bruno */
  if ( save_VectLI(Xgc->xdrs,(lplot->colminmax),2L) == 0) return(0);/* ============== */
  if ( save_VectLI(Xgc->xdrs,(lplot->aaint),4L) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,(lplot->aaint_kp),4L) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->strflag),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->strflag_kp),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->legend),((int)strlen(lplot->legend))+1)  == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->draw)==0) return(0);
  return(1);
}

/*---------------------------------------------------------------------
 * contour 
 *---------------------------------------------------------------------------*/

static int save_Contour(BCG *Xgc,void *plot)
{
  struct rec_contour *lplot = (struct rec_contour *) plot;
  if ( save_LI(Xgc->xdrs,lplot->n1)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n2)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->nz)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->flagnz)== 0) return(0);
  if (lplot->flagnz != 0) 
    if ( save_VectF(Xgc->xdrs,(lplot->zz),lplot->nz) == 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->teta)== 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->alpha)== 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->zlev)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->x),lplot->n1) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->y),lplot->n2) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->z),(lplot->n1)*(lplot->n2)) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->legend), ((int)strlen(lplot->legend))+1) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,(lplot->flag),3L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->bbox),6L) == 0) return(0);
  return(1);
}

static int save_Contour2D(BCG *Xgc,void *plot)
{
  int nstyle;
  struct rec_contour2d *lplot = (struct rec_contour2d *) plot;
  if (lplot->n1==1 ) nstyle= lplot->n1+1;else nstyle= lplot->n1;
  if ( save_LI(Xgc->xdrs,lplot->n1)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n2)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->nz)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->flagnz)== 0) return(0);
  if (lplot->flagnz != 0) 
    if ( save_VectF(Xgc->xdrs,(lplot->zz),lplot->nz) == 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->x),lplot->n1) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->y),lplot->n2) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->z),(lplot->n1)*(lplot->n2)) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,(lplot->style),nstyle) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->strflag),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->strflag_kp),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->legend),((int)strlen(lplot->legend))+1) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->brect),4L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->brect_kp),4L) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,(lplot->aint),4L) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,(lplot->aint_kp),4L) == 0) return(0);
  return(1);
}

/*---------------------------------------------------------------------
 * xgray 
 *---------------------------------------------------------------------------*/

static int save_Gray(BCG *Xgc,void *plot)
{
  int flag;
  struct rec_gray *lplot = (struct rec_gray *) plot;
  if ( save_LI(Xgc->xdrs,lplot->n1)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n2)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->x),lplot->n1) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->y),lplot->n2) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->z),(lplot->n1)*(lplot->n2)) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->strflag),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->strflag_kp),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->brect),4L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->brect_kp),4L) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,(lplot->aaint),4L)  == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,(lplot->aaint_kp),4L) == 0) return(0);
  flag = lplot->remap | ((lplot->colminmax != NULL) ? (1<<4) : 0)  
    | ((lplot->zminmax != NULL) ? (1 <<8) :0)
    | ((lplot->colminmax != NULL) ? (1 <<12) :0);
  if ( save_LI(Xgc->xdrs,flag)==0) return(0);
  if ( lplot->colminmax != NULL )  
    { if ( save_VectLI(Xgc->xdrs,(lplot->colminmax),2L) == 0) return(0);}
  if ( lplot->zminmax != NULL )  
    { if ( save_VectF(Xgc->xdrs,(lplot->zminmax),2L) == 0) return(0);}
  if ( lplot->colout != NULL )  
    { if ( save_VectLI(Xgc->xdrs,(lplot->colout),2L) == 0) return(0);}
  if ( save_LI(Xgc->xdrs,lplot->shade)==0) return(0);
  return(1);
}

static int save_Gray1(BCG *Xgc,void *plot)
{
  int flag;
  struct rec_gray1 *lplot = (struct rec_gray1 *) plot;
  if ( save_LI(Xgc->xdrs,lplot->n1)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n2)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->z),(lplot->n1)*(lplot->n2)) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->strflag),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->strflag_kp),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->brect),4L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->brect_kp),4L) == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,(lplot->aaint),4L)  == 0) return(0);
  if ( save_VectLI(Xgc->xdrs,(lplot->aaint_kp),4L) == 0) return(0);
  flag = lplot->remap | ((lplot->colminmax != NULL) ? (1<<4) : 0)  
    | ((lplot->zminmax != NULL) ? (1 <<8) :0);
  if ( save_LI(Xgc->xdrs,flag)==0) return(0);
  if ( lplot->colminmax != NULL )  
    { if ( save_VectLI(Xgc->xdrs,(lplot->colminmax),2L) == 0) return(0);}
  if ( lplot->zminmax != NULL )  
    { if ( save_VectF(Xgc->xdrs,(lplot->zminmax),2L) == 0) return(0);}
  return(1);
}

static int save_Gray2(BCG *Xgc,void *plot)
{
  int flag;
  struct rec_gray_2 *lplot = (struct rec_gray_2 *) plot;
  if ( save_LI(Xgc->xdrs,lplot->n1)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n2)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->z),(lplot->n1)*(lplot->n2)) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->xrect),4L) == 0) return(0);
  flag = lplot->remap | ((lplot->colminmax != NULL) ? (1<<4) : 0)  
    | ((lplot->zminmax != NULL) ? (1 <<8) :0);
  if ( save_LI(Xgc->xdrs,flag)==0) return(0);
  if ( lplot->colminmax != NULL )  
    { if ( save_VectLI(Xgc->xdrs,(lplot->colminmax),2L) == 0) return(0);}
  if ( lplot->zminmax != NULL )  
    { if ( save_VectF(Xgc->xdrs,(lplot->zminmax),2L) == 0) return(0);}
  return(1);
}

/*---------------------------------------------------------------------
 * champ 
 *---------------------------------------------------------------------------*/

static int save_Champ(BCG *Xgc,void *plot)
{
  struct rec_champ *lplot = (struct rec_champ *) plot;
  if ( save_LI(Xgc->xdrs,lplot->n1)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->n2)== 0) return(0);
  if ( save_D(Xgc->xdrs,lplot->arfact)== 0) return(0);
  if ( save_LI(Xgc->xdrs,lplot->code)==0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->x),(lplot->n1)) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->y),(lplot->n2)) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->fx),(lplot->n1)*(lplot->n2)) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->fy),(lplot->n1)*(lplot->n2)) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->strflag),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC(Xgc->xdrs,(lplot->strflag_kp),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->vrect),4L) == 0) return(0);
  if ( save_VectF(Xgc->xdrs,(lplot->vrect_kp),4L)== 0) return(0);
  return(1);
}


/* unimplemented
 *
 */

static int save_3dobj(BCG *Xgc,void *the_plot)
{
  Sciprintf("save unimplemented for 3dobj\n");
  return 1;
}

static int save_pixbuf(BCG *Xgc,void *the_plot)
{
  Sciprintf("save unimplemented for pixbuf\n");
  return 1;
}

static int save_pixbuf_from_file(BCG *Xgc,void *the_plot)
{
  Sciprintf("save unimplemented for pixbuf_from_file\n");
  return 1;
}

static int save_object(BCG *Xgc,void * theplot )
{
  struct rec_object *lplot= theplot ;
  NspObject *obj = (NspObject *) lplot->obj;
  if ( obj->type->save(Xgc->xdrs,obj) == FAIL) return 0;
  return 1;
}

/*---------------------------------------------------------------------
 * Saving  
 *---------------------------------------------------------------------------*/

#include "nsp/xdr.h"

#define assert(ex) {if (!(ex)){ sciprint("Graphic Load/save_ Error\n");return(0);}} 

typedef  struct  {
  int code;
  char *name;
  int  (*save)(BCG *Xgc,void *);
} Save_Table;

static Save_Table save_table [] ={
  {CODEclipping_p            ,"clipping_p",       save_clipping_p },
  {CODEclipgrf		     ,"clipgrf",	  save_clipgrf },
  {CODEalufunction1	     ,"alufunction1",	  save_alufunction1 },
  {CODEbackground	     ,"background",	  save_background },
  {CODEunclip		     ,"unclip",		  save_unclip },
  {CODEclip		     ,"clip",		  save_clip },
  {CODEpattern		     ,"pattern",	  save_pattern },
  {CODEfont_size	     ,"font_size",	  save_font_size },
  {CODEfont		     ,"font",		  save_font },
  {CODEforeground	     ,"foreground",	  save_foreground },
  {CODEhidden3d		     ,"hidden3d",	  save_hidden3d },
  {CODEabsourel		     ,"absourel",	  save_absourel },
  {CODEdash		     ,"dash",		  save_dash },
  {CODEmark_size	     ,"mark_size",	  save_mark_size },
  {CODEmark		     ,"mark",		  save_mark },
  {CODEpixmapOn		     ,"pixmapOn",	  save_pixmapOn },
  {CODEthickness      	     ,"thickness",	  save_thickness },
  {CODEusecolor		     ,"usecolor",	  save_usecolor },
  {CODEshow		     ,"show",		  save_show },
  {CODEpixmapclear	     ,"pixmapclear",	  save_pixmapclear },
  {CODEdrawarc_1       	     ,"drawarc_1",	  save_drawarc_1 },
  {CODEfillarcs_1      	     ,"fillarcs_1",	  save_fillarcs_1 },
  {CODEdrawarcs_1      	     ,"drawarcs_1",	  save_drawarcs_1 },
  {CODEfillpolyline_1	     ,"fillpolyline_1",	  save_fillpolyline_1 },
  {CODEdrawarrows_1	     ,"drawarrows_1",	  save_drawarrows_1 },
  {CODEdrawaxis_1      	     ,"drawaxis_1",	  save_drawaxis_1 },
  {CODEcleararea_1	     ,"cleararea_1",	  save_cleararea_1 },
  {CODEfillarc_1       	     ,"fillarc_1",	  save_fillarc_1 },
  {CODEfillrectangle_1	     ,"fillrectangle_1",  save_fillrectangle_1 },
  {CODEdrawpolyline_1	     ,"drawpolyline_1",	  save_drawpolyline_1 },
  {CODEfillpolylines_1	     ,"fillpolylines_1",  save_fillpolylines_1 },
  {CODEdrawpolymark_1	     ,"drawpolymark_1",	  save_drawpolymark_1 },
  {CODEdisplaynumbers_1	     ,"displaynumbers_1", save_displaynumbers_1 },
  {CODEdrawpolylines_1	     ,"drawpolylines_1",  save_drawpolylines_1 },
  {CODEdrawrectangle_1	     ,"drawrectangle_1",  save_drawrectangle_1 },
  {CODEdrawrectangles_1	     ,"drawrectangles_1", save_drawrectangles_1 },
  {CODEdrawsegments_1	     ,"drawsegments_1",	  save_drawsegments_1 },
  {CODEdisplaystring_1	     ,"displaystring_1",  save_displaystring_1 },
  {CODEdisplaystringa_1	     ,"displaystringa_1", save_displaystringa_1 },
  {CODExstringb_1      	     ,"xstringb_1",	  save_xstringb_1 },
  {CODEEch		     ,"Ech",		  save_Ech },
  {CODENEch		     ,"NEch",		  save_NEch },
  {CODEPlot		     ,"Plot",		  save_Plot },
  {CODEPlot1		     ,"Plot",		  save_Plot },
  {CODEPlot2		     ,"Plot",		  save_Plot },
  {CODEPlot3		     ,"Plot",		  save_Plot },
  {CODEPlot4		     ,"Plot",		  save_Plot },
  {CODESciAxis		     ,"SciAxis",	  save_SciAxis },
  {CODEGrid		     ,"Grid",		  save_Grid },
  {CODEParam3D		     ,"Param3D",	  save_Param3D },
  {CODEParam3D1		     ,"Param3D1",	  save_Param3D1 },
  {CODEPlot3D		     ,"Plot3D",		  save_Plot3D },
  {CODEPlot3D1		     ,"Plot3D1",	  save_Plot3D },
  {CODEFac3D		     ,"Fac3D",		  save_Fac3D },
  {CODEFac3D1		     ,"Fac3D1",		  save_Fac3D },
  {CODEFac3D2		     ,"Fac3D2",		  save_Fac3D },
  {CODEFac3D3		     ,"Fac3D3",		  save_Fac3D },
  {CODEFec		     ,"Fec",		  save_Fec },
  {CODEFecN		     ,"FecN",		  save_Fec },
  {CODEContour		     ,"Contour",	  save_Contour },
  {CODEContour2D       	     ,"Contour2D",	  save_Contour2D },
  {CODEGray		     ,"Gray",		  save_Gray },
  {CODEGray1		     ,"Gray1",		  save_Gray1 },
  {CODEGray2		     ,"Gray2",		  save_Gray2 },
  {CODEChamp		     ,"Champ",		  save_Champ },
  {CODEChamp1		     ,"Champ",		  save_Champ },
  {CODEfpf_def   	     ,"fpf_def",          save_fpf_def},
  {CODEfpf   	             ,"fpf",              save_fpf},
  {CODEinitialize_gc         ,"init",             save_init},
  {CODEColormap		     ,"Colormap",	  save_colormap },
  {CODEdefault_colormap	     ,"default_colormap", save_default_colormap },
  {CODE3dobj	             ,"default_colormap", save_3dobj },
  {CODEpixbuf		     ,"pixbuf",           save_pixbuf },
  {CODEpixbuf_file	     ,"pixbuf_file",      save_pixbuf_from_file },
  {CODEtest	             ,"test",             save_test },
  {CODEobject       	     ,"object",           save_object }
};     	


#ifdef __MSC__
#ifndef __STDC__
#define __STDC__
#endif
#endif 

int tape_save(BCG *Xgc,const char *fname1, int winnumber)
{
  char fname[128]; /* FIXME */
  FILE *F ;
  XDR xdrs[1] ;
  list_plot *list = Xgc->plots ;
  static char scig[]={"Nsp_1.0"};
  if ( Xgc->record_flag == FALSE ) return 0 ;
#ifdef lint 
  *unused;
#endif
  strncpy(fname,fname1,128);
#ifdef __STDC__
  F = fopen(fname,"wb") ;
#else
  F = fopen(fname,"w") ;
#endif
  if( F == NULL)
    {
      sciprint("fopen failed\n") ;
      return(0);
    }
  xdrstdio_create(xdrs, F, XDR_ENCODE) ;
  Xgc->xdrs = xdrs;
  save_VectC(Xgc->xdrs,scig,((int)strlen(scig))+1) ;
  
  while (list)
    {
      if ( list->theplot != NULL) 
	{
	  int code = ((plot_code *) list->theplot)->code;
	  /* sciprint("XXXsaved code %d\n",code); */
	  if (save_LI(Xgc->xdrs,code) == 0) break;
	  if (save_table[code].save(Xgc,list->theplot) == 0) break;
	}
      list =list->next;
    }
  save_LI(Xgc->xdrs,CODEendplots);
  assert(fflush((FILE *)xdrs->x_private) != EOF) ; 
  assert(fclose(F) != EOF) ;
  return(0);
}

/*---------------------------------------------------------------------------
 * utilities 
 *---------------------------------------------------------------------------*/

static int save_D(XDR *xdrs,double x)
{
  u_int szof = sizeof(double) ;
  u_int count = 1;
  assert( xdr_vector(xdrs, (char *) &x, count, szof, (xdrproc_t) xdr_double)) ;
  return(1);
}

static int save_LI(XDR *xdrs,int ix)
{
  u_int szof = sizeof(int) ;
  u_int count = 1;
  assert( xdr_vector(xdrs, (char *)&ix, count, szof, (xdrproc_t) xdr_int)) ;
  return(1);
}

static int save_C(XDR *xdrs,char *c, int lc)
{
  u_int szof = lc*sizeof(char);
  assert( xdr_vector(xdrs,(char *) &szof,(unsigned)1,(unsigned) sizeof(unsigned), (xdrproc_t) xdr_u_int)) ;
  assert( xdr_opaque(xdrs,c,szof));
  return(1);
}

static int save_VectLI(XDR *xdrs,int *nx, int l)
{ 
  int nx1=1;
  /* Attention int peut etre un long int **/
  u_int szof = sizeof(int) ;
  u_int count = (int) l;
  assert( xdr_vector(xdrs,(char *) &count,(unsigned)1,(unsigned) sizeof(unsigned), (xdrproc_t) xdr_u_int)) ;
  if ( nx == (int  *) NULL && l == (int) 1)
    {
      assert( xdr_vector(xdrs, (char *)&nx1, count, szof, (xdrproc_t) xdr_int)) ;
    }
  else
    {
      assert( xdr_vector(xdrs, (char *)nx, count, szof, (xdrproc_t) xdr_int)) ;
    }
  return(1);
}

static int save_VectF(XDR *xdrs,double *nx, int l)
{
  double nx1=0.0;
  u_int szof = sizeof(double) ;
  u_int count = (int) l;
  assert( xdr_vector(xdrs,(char *) &count,(unsigned)1,(unsigned) sizeof(unsigned), (xdrproc_t) xdr_u_int)) ;
  if ( nx == (double  *) NULL && l == (int) 1)
    { assert( xdr_vector(xdrs, (char *)&nx1, count, szof, (xdrproc_t) xdr_double)) ; } 
  else
    { assert( xdr_vector(xdrs, (char *)nx, count, szof, (xdrproc_t) xdr_double)) ; } 
  return(1);
}

static int save_VectC(XDR *xdrs,char *nx, int l)
{ 
  char nx1='1';
  u_int szof = l*sizeof(char);
  assert( xdr_vector(xdrs,(char *) &szof,(unsigned)1,(unsigned) sizeof(unsigned), (xdrproc_t) xdr_u_int)) ;
  if ( nx == (char  *) NULL && l == (int) 1)
    { assert( xdr_opaque(xdrs, &nx1,szof)); } 
  else 
    { assert( xdr_opaque(xdrs, nx,szof)); }
  return(1);
}

static int save_VectS(XDR *xdrs,char **nx)
{
  u_int count;
  int scount = 0,i;
  if (nx != NULL) { while ( nx[scount] != NULL) scount++;} 
  /* save the number of strings */
  count = scount;
  assert( xdr_vector(xdrs,(char *) &count,(unsigned)1,(unsigned) sizeof(unsigned), (xdrproc_t) xdr_u_int)) ;
  /* now save each string */ 
  for ( i=0; i < scount ; i++) 
    {
      if ( save_VectC(xdrs, nx[i], strlen(nx[i])+1) == 0 ) return 0;
    }
  return 1;
}


