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
 * reload graphic code from a file  
 * --------------------------------------------------------------------------*/

#include <string.h> 
#include <stdio.h>
#include <stdlib.h>
#include "nsp/math.h"
#include "nsp/xdr.h"
#include "nsp/graphics/Graphics.h"
#include "nsp/graphics/Rec.h"

#define assert(ex) {if (!(ex)){ sciprint("Graphic load_/Save Error \r\n");return(0);}}

static char *SciF_version;

static int load_D(XDR *xdrs,double *x);
static int load_DS(XDR *xdrs,double *x,int n);
static int load_LI(XDR *xdrs, int *ix);
static int load_LIS(XDR *xdrs, int *ix,int n);
static int load_C(XDR *xdrs,char *c);
static int load_VectLI(XDR *xdrs, int **nx);
static int load_VectF(XDR *xdrs, double **nx);
static int load_VectC(XDR *xdrs, char **nx);
static int load_VectS(XDR *xdrs, char ***nx );

static int load_Plot  (BCG *Xgc); 
static int load_SciAxis  (BCG *Xgc); 
static int load_Grid (BCG *Xgc); 
static int load_Param3D (BCG *Xgc); 
static int load_Param3D1 (BCG *Xgc); 
static int load_Plot3D (BCG *Xgc); 
static int load_Fac3D (BCG *Xgc); 
static int load_Fec (BCG *Xgc); 
static int load_Contour (BCG *Xgc); 
static int load_Contour2D (BCG *Xgc); 
static int load_Gray (BCG *Xgc); 
static int load_Gray1 (BCG *Xgc); 
static int load_Gray2 (BCG *Xgc); 
static int load_Champ (BCG *Xgc); 
static int load_Ech (BCG *Xgc); 

static int load_clipping_p (BCG *Xgc) ;
static int load_clipgrf (BCG *Xgc) ;
static int load_alufunction1 (BCG *Xgc) ;
static int load_background (BCG *Xgc) ;
static int load_unclip (BCG *Xgc) ;
static int load_test (BCG *Xgc) ;
static int load_clip (BCG *Xgc) ;
static int load_pattern (BCG *Xgc) ;
static int load_font_size (BCG *Xgc) ;
static int load_font (BCG *Xgc) ;
static int load_foreground (BCG *Xgc) ;
static int load_hidden3d (BCG *Xgc) ;
static int load_absourel (BCG *Xgc) ;
static int load_dash (BCG *Xgc) ;
static int load_mark_size (BCG *Xgc) ;
static int load_mark (BCG *Xgc) ;
static int load_pixmapOn (BCG *Xgc) ;
static int load_thickness (BCG *Xgc) ;
static int load_usecolor (BCG *Xgc) ;
static int load_show (BCG *Xgc) ;
static int load_pixmapclear (BCG *Xgc) ;
static int load_drawarc_1 (BCG *Xgc) ;
static int load_fillarcs_1 (BCG *Xgc) ;
static int load_drawarcs_1 (BCG *Xgc) ;
static int load_fillpolyline_1 (BCG *Xgc) ;
static int load_drawarrows_1 (BCG *Xgc) ;
static int load_drawaxis_1 (BCG *Xgc) ;
static int load_cleararea_1 (BCG *Xgc) ;
static int load_fillarc_1 (BCG *Xgc) ;
static int load_fillrectangle_1 (BCG *Xgc) ;
static int load_drawpolyline_1 (BCG *Xgc) ;
static int load_fillpolylines_1 (BCG *Xgc) ;
static int load_drawpolymark_1 (BCG *Xgc) ;
static int load_displaynumbers_1 (BCG *Xgc) ;
static int load_drawpolylines_1 (BCG *Xgc) ;
static int load_drawrectangle_1 (BCG *Xgc) ;
static int load_drawrectangles_1 (BCG *Xgc) ;
static int load_drawsegments_1 (BCG *Xgc) ;
static int load_displaystring_1 (BCG *Xgc) ;
static int load_displaystringa_1 (BCG *Xgc) ;
static int load_xstringb_1 (BCG *Xgc) ;

/*---------------------------------------------------------------------
 * basic primitives 
 *---------------------------------------------------------------------------*/

static int load_clipping_p(BCG *Xgc)
{
  struct rec_int4 *lplot  = MALLOC(sizeof(struct rec_int4));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_LIS(Xgc->xdrs,lplot->vals,4) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

static int load_clipgrf(BCG *Xgc)
{
  struct rec_void *lplot  = MALLOC(sizeof(struct rec_void));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&((struct rec_void *) lplot)->code)==0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

static int load_int(BCG *Xgc) 
{
  struct rec_int *lplot = MALLOC(sizeof(struct rec_int));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->val) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

static int load_alufunction1(BCG *Xgc) { return load_int(Xgc); }

static int load_background(BCG *Xgc)  { return load_int(Xgc); }

static int load_unclip(BCG *Xgc)
{
  struct rec_void *lplot  = MALLOC(sizeof(struct rec_void));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

static int load_test(BCG *Xgc)
{
  struct rec_void *lplot  = MALLOC(sizeof(struct rec_void));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}


static int load_clip(BCG *Xgc)
{
  struct rec_int4 *lplot  = MALLOC(sizeof(struct rec_int4));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LIS(Xgc->xdrs,lplot->vals,4) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

static int load_pattern(BCG *Xgc)  { return load_int(Xgc); }

static int load_font_size(BCG *Xgc)  { return load_int(Xgc); }


static int load_font(BCG *Xgc)
{
  struct rec_int2 *lplot  = MALLOC(sizeof(struct rec_int2));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->val) == 0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->val1) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

static int load_foreground(BCG *Xgc)  { return load_int(Xgc); }

static int load_hidden3d(BCG *Xgc)  { return load_int(Xgc); }

static int load_absourel(BCG *Xgc)  { return load_int(Xgc); }

static int load_dash(BCG *Xgc)  { return load_int(Xgc); }

static int load_mark_size(BCG *Xgc)  { return load_int(Xgc); }

static int load_mark(BCG *Xgc)  
{
  struct rec_int2 *lplot  = MALLOC(sizeof(struct rec_int2));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->val) == 0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->val1) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

static int load_pixmapOn(BCG *Xgc)  { return load_int(Xgc); }
static int load_thickness(BCG *Xgc)  { return load_int(Xgc); }
static int load_usecolor(BCG *Xgc)  { return load_int(Xgc); }

static int load_show(BCG *Xgc) 
{
  struct rec_void *lplot  = MALLOC(sizeof(struct rec_void));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&((struct rec_void *) lplot)->code)==0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

static int load_pixmapclear(BCG *Xgc)
{
  struct rec_void *lplot  = MALLOC(sizeof(struct rec_void));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&((struct rec_void *) lplot)->code)==0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

static int load_fpf_def(BCG *Xgc)
{
  struct rec_void *lplot  = MALLOC(sizeof(struct rec_void));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&((struct rec_void *) lplot)->code)==0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

static int load_fpf(BCG *Xgc)
{
  struct rec_str *lplot  = MALLOC(sizeof(struct rec_str));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_C(Xgc->xdrs,lplot->str) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

static int load_init(BCG *Xgc) 
{
  struct rec_void *lplot  = MALLOC(sizeof(struct rec_void));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&((struct rec_void *) lplot)->code)==0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

static int load_colormap(BCG *Xgc)
{
  struct rec_colormap *lplot = MALLOC(sizeof(struct rec_colormap));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->m)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->n)==0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->colors) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

static int load_default_colormap(BCG *Xgc)
{
  struct rec_void *lplot  = MALLOC(sizeof(struct rec_void));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&((struct rec_void *) lplot)->code)==0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}


/*-----------------------------------------------------------------------------
 *  drawarc_1
 *-----------------------------------------------------------------------------*/

static int load_drawarc_1(BCG *Xgc)
{
  int i;
  struct rec_drawarc *lplot = MALLOC(sizeof(struct rec_drawarc));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  for ( i = 0 ; i < 6 ; i++) 
    {
      if ( load_D(Xgc->xdrs,lplot->arc+i) == 0) return(0);
    }
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 * 
 *-----------------------------------------------------------------------------*/

static int load_fillarcs_1(BCG *Xgc)
{
  struct rec_fillarcs *lplot = MALLOC(sizeof(struct rec_fillarcs));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->n) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vects) == 0) return(0);
  if ( load_VectLI(Xgc->xdrs,&lplot->fillvect) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  
 *-----------------------------------------------------------------------------*/

static int load_drawarcs_1(BCG *Xgc)
{
  struct rec_fillarcs *lplot = MALLOC(sizeof(struct rec_fillarcs));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->n) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vects) == 0) return(0);
  if ( load_VectLI(Xgc->xdrs,&lplot->fillvect) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  
 *-----------------------------------------------------------------------------*/

static int load_fillpolyline_1(BCG *Xgc)
{
  struct rec_fillpolyline *lplot = MALLOC(sizeof(struct rec_fillpolyline));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->n) == 0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->closeflag) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vx) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vy) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  arrows
 *-----------------------------------------------------------------------------*/

static int load_drawarrows_1(BCG *Xgc)
{
  struct rec_arrows *lplot = MALLOC(sizeof(struct rec_arrows));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->n) == 0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->iflag) == 0) return(0);
  if ( load_D(Xgc->xdrs,&lplot->as) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vx) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vy) == 0) return(0);
  if ( lplot->iflag != 0 ) 
    {
      if ( load_VectLI(Xgc->xdrs,&lplot->style) == 0) return(0);
    }
  else 
    {
      if ( load_LI(Xgc->xdrs,&lplot->def_style)==0) return(0);
    }
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 * axis 
 *-----------------------------------------------------------------------------*/

static int load_drawaxis_1(BCG *Xgc)
{
  struct rec_drawaxis *lplot = MALLOC(sizeof(struct rec_drawaxis));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_D(Xgc->xdrs,&lplot->alpha) == 0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->nsteps) == 0) return(0);
  if ( load_DS(Xgc->xdrs,lplot->initpoint,2) == 0) return(0);
  if ( load_DS(Xgc->xdrs,lplot->size,3) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  cleararea
 *-----------------------------------------------------------------------------*/

static int load_cleararea_1(BCG *Xgc)
{
  struct rec_double4 *lplot = MALLOC(sizeof(struct rec_double4));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_DS(Xgc->xdrs,lplot->vals,4) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 *   fillarc
 *-----------------------------------------------------------------------------*/

static int load_fillarc_1(BCG *Xgc)
{
  int i;
  struct rec_drawarc *lplot = MALLOC(sizeof(struct rec_drawarc));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  for ( i = 0 ; i < 6 ; i++) 
    {
      if ( load_D(Xgc->xdrs,lplot->arc+i) == 0) return(0);
    }
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  fillrectangle
 *-----------------------------------------------------------------------------*/

static int load_fillrectangle_1(BCG *Xgc)
{
  struct rec_double4 *lplot = MALLOC(sizeof(struct rec_double4));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_DS(Xgc->xdrs,lplot->vals,4) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  drawpolyline
 *-----------------------------------------------------------------------------*/

static int load_drawpolyline_1(BCG *Xgc)
{
  struct rec_fillpolyline *lplot = MALLOC(sizeof(struct rec_fillpolyline));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->n) == 0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->closeflag) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vx) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vy) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  fillpolylines
 *-----------------------------------------------------------------------------*/

static int load_fillpolylines_1(BCG *Xgc)
{
  struct rec_fillpolylines *lplot = MALLOC(sizeof(struct rec_fillpolylines));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->n) == 0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->p) == 0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->v1) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vx) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vy) == 0) return(0);
  if ( lplot->v1 == 2 ) 
    {
      if ( load_VectLI(Xgc->xdrs,&lplot->fillvect) == 0) return(0);
    }
  else 
    {
      if ( load_VectLI(Xgc->xdrs,&lplot->fillvect) == 0) return(0);
    }
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  drawpolymark
 *-----------------------------------------------------------------------------*/

static int load_drawpolymark_1(BCG *Xgc)
{
  struct rec_fillpolyline *lplot = MALLOC(sizeof(struct rec_fillpolyline));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->n) == 0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->closeflag) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vx) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vy) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  displaynumbers
 *-----------------------------------------------------------------------------*/

static int load_displaynumbers_1(BCG *Xgc)
{
  struct rec_displaynumbers *lplot = MALLOC(sizeof(struct rec_displaynumbers));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->n) == 0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->flag) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->x) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->y) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->z) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->alpha) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 *   drawpolylines
 *-----------------------------------------------------------------------------*/

static int load_drawpolylines_1(BCG *Xgc)
{
  struct rec_drawpolylines *lplot = MALLOC(sizeof(struct rec_drawpolylines));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->n) == 0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->p) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vx) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vy) == 0) return(0);
  if ( load_VectLI(Xgc->xdrs,&lplot->drawvect) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}


/*-----------------------------------------------------------------------------
 *   drawrectangle
 *-----------------------------------------------------------------------------*/

static int load_drawrectangle_1(BCG *Xgc)
{
  struct rec_double4 *lplot = MALLOC(sizeof(struct rec_double4));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_DS(Xgc->xdrs,lplot->vals,4) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 *   drawrectangles
 *-----------------------------------------------------------------------------*/

static int load_drawrectangles_1(BCG *Xgc)
{
  struct rec_fillarcs *lplot = MALLOC(sizeof(struct rec_fillarcs));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->n) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vects) == 0) return(0);
  if ( load_VectLI(Xgc->xdrs,&lplot->fillvect) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  drawsegments
 *-----------------------------------------------------------------------------*/

static int load_drawsegments_1(BCG *Xgc)
{
  struct rec_segment *lplot = MALLOC(sizeof(struct rec_arrows));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->n) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vx) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->vy) == 0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->iflag) == 0) return(0);
  if ( load_VectLI(Xgc->xdrs,&lplot->style) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  displaystring
 *-----------------------------------------------------------------------------*/

static int load_displaystring_1(BCG *Xgc)
{
  struct rec_displaystring *lplot = MALLOC(sizeof(struct rec_displaystring));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_VectC(Xgc->xdrs,&lplot->string) == 0) return(0);
  if ( load_D(Xgc->xdrs,&lplot->x) == 0) return(0);
  if ( load_D(Xgc->xdrs,&lplot->y) == 0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->flag) == 0) return(0);
  if ( load_D(Xgc->xdrs,&lplot->angle) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  displaystringa
 *-----------------------------------------------------------------------------*/

static int load_displaystringa_1(BCG *Xgc)
{
  struct rec_displaystringa *lplot = MALLOC(sizeof(struct rec_displaystringa));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_VectC(Xgc->xdrs,&lplot->string) == 0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->ipos) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*-----------------------------------------------------------------------------
 * a string in a bounded box : with font size change to fit into the 
 * specified box (only works with driver which properly estimate string sizes)
 *-----------------------------------------------------------------------------*/

static int load_xstringb_1(BCG *Xgc)
{
  struct rec_xstringb *lplot = MALLOC(sizeof(struct rec_xstringb));
  if (lplot == NULL) {Scistring("running out of memory \n");return 0;}
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( load_VectC(Xgc->xdrs,&lplot->string) == 0) return(0);
  if ( load_D(Xgc->xdrs,&lplot->x) == 0) return(0);
  if ( load_D(Xgc->xdrs,&lplot->y) == 0) return(0);
  if ( load_D(Xgc->xdrs,&lplot->wd) == 0) return(0);
  if ( load_D(Xgc->xdrs,&lplot->hd) == 0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->flag) == 0) return(0);
  store_record(Xgc,lplot->code,lplot);
  return 1;
}

/*---------------------------------------------------------------------
 * scales 
 *---------------------------------------------------------------------------*/

static int load_Ech(BCG *Xgc)
{
  struct rec_scale *lplot = ((struct rec_scale *) MALLOC(sizeof(struct rec_scale)));
  if (lplot != NULL)
    {
      if ( load_C(Xgc->xdrs,lplot->logflag) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->Wrect)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->Frect)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->Frect_kp))   == 0) return(0);
      if (store_record(Xgc,CODEEch,(char *) lplot) == 0) return(0);
    }
  else 
    {
      Scistring("\nload_ Plot (Ech): No more place \n");
      return(0);
    }
  return(1);
}

static int load_NEch(BCG *Xgc)
{
  struct rec_nscale *lplot;
  lplot= ((struct rec_nscale *) MALLOC(sizeof(struct rec_nscale)));
  if (lplot != NULL)
    {
      if ( load_C(Xgc->xdrs,lplot->logflag) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->flag)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->Wrect)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->Frect)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->Arect)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->Frect_kp))   == 0) return(0);
      if (store_record(Xgc,CODENEch,(char *) lplot) == 0) return(0);
    }
  else 
    {
      Scistring("\nload_ Plot (Ech): No more place \n");
      return(0);
    }
  return(1);
}

/*---------------------------------------------------------------------
 * 2D plots  
 *---------------------------------------------------------------------------*/

static int load_Plot(BCG *Xgc)
{
  int n=0, nstyle;
  struct rec_plot2d *lplot;
  lplot= ((struct rec_plot2d *) MALLOC(sizeof(struct rec_plot2d)));
  if (lplot != NULL)
    {
      if ( load_LI(Xgc->xdrs,&lplot->n1) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->n2) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->xf)) == 0) return(0);
      if (lplot->n1==1 ) nstyle= lplot->n1+1;else nstyle= lplot->n1;
      switch (lplot->xf[0])
	{
	case 'g': n=(lplot->n1)*(lplot->n2);break;
	case 'e': n=0;break;
	case 'o': n=(lplot->n2);break;
	}
      if ( n != 0) 
	{
	  if ( load_VectF(Xgc->xdrs,&(lplot->x)) == 0) return(0);
	}
      if ( load_VectF(Xgc->xdrs,&(lplot->y)) == 0) return(0);
      if ( load_VectLI(Xgc->xdrs,&(lplot->style)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->strflag)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->strflag_kp)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->legend)) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&(lplot->legend_pos)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->brect)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->brect_kp)) == 0) return(0);
      if ( load_VectLI(Xgc->xdrs,&(lplot->aint)) == 0) return(0);
      if ( load_VectLI(Xgc->xdrs,&(lplot->aint_kp)) == 0) return(0);
      if (store_record(Xgc,CODEPlot,(char *) lplot) == 0) return(0);
    }
  else 
    {
      Scistring("\nload_ Plot (plot): No more place \n");
      return(0);
    }
  return(1);
}


/*---------------------------------------------------------------------
 * xgrid 
 *---------------------------------------------------------------------------*/

static int load_SciAxis(BCG *Xgc)
{
  char type[3] ;
  struct rec_sciaxis *lplot;
  if ((lplot= ((struct rec_sciaxis *) MALLOC(sizeof(struct rec_sciaxis)))) == NULL) 
    {
      Scistring("\nload_ Plot (grid): No more place \n");
      return(0);
    }
  if ( load_C(Xgc->xdrs,type)== 0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->nx)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->ny)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->subtics)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->fontsize)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->textcolor)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->ticscolor)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->seg_flag)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->f_l)==0) return(0);
  if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
  if ( lplot->f_l == 1) 
    {
      if ( load_VectC(Xgc->xdrs, &(lplot->format)) == 0) return(0);
    }
  else
    lplot->format =0;
  if ( load_VectF(Xgc->xdrs,&lplot->x) == 0) return(0);
  if ( load_VectF(Xgc->xdrs,&lplot->y) == 0) return(0);
  if ( load_VectS(Xgc->xdrs,&lplot->str) == 0) return(0);
  
  lplot->pos =  type[0] ;
  lplot->xy_type =  type[1];
  lplot->logflag  = type[2];
  if (store_record(Xgc,CODESciAxis,(char *) lplot) == 0) return(0);
  return(1);
}

/*---------------------------------------------------------------------
 * xgrid 
 *---------------------------------------------------------------------------*/

static int load_Grid(BCG *Xgc)
{ 
  struct rec_xgrid *lplot ;
  lplot= ((struct rec_xgrid *) MALLOC(sizeof(struct rec_xgrid)));
  if (lplot != NULL)
    {
      if ( load_LI(Xgc->xdrs,&lplot->style) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
      if (store_record(Xgc,CODEGrid,(char *) lplot) == 0) return(0);
      
    }
  else 
    {
      Scistring("\nload_ Plot (grid): No more place \n");
      return(0);
    }
  return(1);
}


/*---------------------------------------------------------------------
 * param3d 
 *---------------------------------------------------------------------------*/

static int load_Param3D(BCG *Xgc)
{
  struct rec_param3d *lplot;
  lplot= ((struct rec_param3d *) MALLOC(sizeof(struct rec_param3d)));
  if (lplot != NULL)
    {
      if ( load_LI(Xgc->xdrs,&lplot->n) == 0) return(0);
      if ( load_D(Xgc->xdrs,&lplot->teta) ==0) return(0);
      if ( load_D(Xgc->xdrs,&lplot->alpha) ==0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->x)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->y)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->z)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->legend)) == 0) return(0);
      if ( load_VectLI(Xgc->xdrs,&(lplot->flag)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->bbox))== 0) return(0);
      if (store_record(Xgc,CODEParam3D,(char *) lplot) == 0) return(0);
    }
  else
    {
      Scistring("\nload_ Plot (param3d): No more place \n");
      return(0);
    }
  return(1);
}

static int load_Param3D1(BCG *Xgc)
{
  struct rec_param3d1 *lplot;
  lplot= ((struct rec_param3d1 *) MALLOC(sizeof(struct rec_param3d1)));
  if (lplot != NULL)
    {
      if ( load_LI(Xgc->xdrs,&lplot->m) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->n) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->iflag) == 0) return(0);
      if (lplot->iflag == 1) if ( load_VectLI(Xgc->xdrs,&(lplot->colors)) == 0) return(0);
      if ( load_D(Xgc->xdrs,&lplot->teta) ==0) return(0);
      if ( load_D(Xgc->xdrs,&lplot->alpha) ==0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->x)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->y)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->z)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->legend)) == 0) return(0);
      if ( load_VectLI(Xgc->xdrs,&(lplot->flag)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->bbox))== 0) return(0);
      if (store_record(Xgc,CODEParam3D1,(char *) lplot) == 0) return(0);
    }
  else
    {
      Scistring("\nload_ Plot (param3d1): No more place \n");
      return(0);
    }
  return(1);
}

/*---------------------------------------------------------------------
 * plot3d 
 *---------------------------------------------------------------------------*/

static int load_Plot3D(BCG *Xgc)
{
  struct rec_plot3d *lplot ;
  lplot= ((struct rec_plot3d *) MALLOC(sizeof(struct rec_plot3d)));
  if (lplot != NULL)
    {
      if ( load_LI(Xgc->xdrs,&lplot->p) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->q) == 0) return(0);
      if ( load_D(Xgc->xdrs,&lplot->teta) == 0) return(0);
      if ( load_D(Xgc->xdrs,&lplot->alpha) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);      
      if ( load_VectF(Xgc->xdrs,&(lplot->x)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->y)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->z)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->legend)) == 0) return(0);
      if ( load_VectLI(Xgc->xdrs,&(lplot->flag)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->bbox))== 0) return(0);
      if (store_record(Xgc,lplot->code,(char *) lplot) == 0) return(0);
    }
  else 
    {
      Scistring("\nload_ Plot (plot3d): No more place \n");
      return(0);
    }
  return(1);
}

/*---------------------------------------------------------------------
 * fac3d 
 *---------------------------------------------------------------------------*/

static int load_Fac3D(BCG *Xgc)
{
  struct rec_fac3d *lplot;
  lplot= ((struct rec_fac3d *) MALLOC(sizeof(struct rec_fac3d)));
  if (lplot != NULL)
    {
      if ( load_LI(Xgc->xdrs,&lplot->p) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->q) == 0) return(0);
      if ( load_D(Xgc->xdrs,&lplot->teta) == 0) return(0);
      if ( load_D(Xgc->xdrs,&lplot->alpha) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
      if ( lplot->code == CODEFac3D2) 
	if ( load_VectLI(Xgc->xdrs,&(lplot->cvect)) == 0) return(0);
      if ( lplot->code == CODEFac3D3) 
	if ( load_VectLI(Xgc->xdrs,&(lplot->cvect)) == 0) return(0); /****** entry added by polpoth 4/5/2000 ******/
      if ( load_VectF(Xgc->xdrs,&(lplot->x)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->y)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->z)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->legend)) == 0) return(0);
      if ( load_VectLI(Xgc->xdrs,&(lplot->flag)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->bbox))== 0) return(0);
      if (store_record(Xgc,CODEFac3D,(char *) lplot) == 0) return(0);
    }
  else 
    {
      Scistring("\nload_ Plot (fac3d): No more place \n");
      return(0);
    }
  return(1);
}

/*---------------------------------------------------------------------
 * fec 
 *---------------------------------------------------------------------------*/

/* pas compatibility */

static int load_Fec(BCG *Xgc)
{
  struct rec_fec *lplot;
  lplot= ((struct rec_fec *) MALLOC(sizeof(struct rec_fec)));
  if (lplot != NULL)
    {
      if ( load_LI(Xgc->xdrs,&lplot->Nnode) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->Ntr) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->x)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->y)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->triangles)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->func) ) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->brect)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->brect_kp)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->zminmax)) == 0) return(0);    /* added by bruno */
      if ( load_VectLI(Xgc->xdrs,&(lplot->colminmax)) == 0) return(0); /* added by bruno */
      if ( load_VectLI(Xgc->xdrs,&(lplot->aaint)) == 0) return(0);
      if ( load_VectLI(Xgc->xdrs,&(lplot->aaint_kp)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->strflag)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->strflag_kp)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->legend))  == 0) return(0);
      if (store_record(Xgc,CODEFecN,(char *) lplot) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->draw)==0) return(0);
    }
  else 
    {
      Scistring("\nload_ Plot (fec): No more place \n");
      return(0);
    }
  return(1);
}


/*---------------------------------------------------------------------
 * contour 
 *---------------------------------------------------------------------------*/

static int load_Contour(BCG *Xgc)
{
  struct rec_contour *lplot;
  lplot= ((struct rec_contour *) MALLOC(sizeof(struct rec_contour)));
  if (lplot != NULL)
    {
      if ( load_LI(Xgc->xdrs,&lplot->n1) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->n2) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->nz) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->flagnz) == 0) return(0);
      if (lplot->flagnz != 0) 
	{
	  if ( load_VectF(Xgc->xdrs,&(lplot->zz))== 0) return(0);
	}
      if ( load_D(Xgc->xdrs,&lplot->teta)== 0) return(0);
      if ( load_D(Xgc->xdrs,&lplot->alpha)== 0) return(0);
      if ( load_D(Xgc->xdrs,&lplot->zlev)== 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->x)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->y)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->z)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->legend)) == 0) return(0);
      if ( load_VectLI(Xgc->xdrs,&(lplot->flag)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->bbox)) == 0) return(0);
      if (store_record(Xgc,CODEContour,(char *) lplot) == 0) return(0);
      
    }
  else 
    {
      Scistring("\nload_ Plot (contour): No more place \n");
      return(0);
    }
  return(1);
}


static int load_Contour2D(BCG *Xgc)
{
  struct rec_contour2d *lplot;
  lplot= ((struct rec_contour2d *) MALLOC(sizeof(struct rec_contour2d)));
  if (lplot != NULL)
    {
      if ( load_LI(Xgc->xdrs,&lplot->n1) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->n2) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->nz) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->flagnz) == 0) return(0);
      if (lplot->flagnz != 0) 
	{
	  if ( load_VectF(Xgc->xdrs,&(lplot->zz))== 0) return(0);
	}
      if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->x)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->y)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->z)) == 0) return(0);
      if ( load_VectLI(Xgc->xdrs,&(lplot->style)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->strflag)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->strflag_kp)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->legend)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->brect)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->brect_kp)) == 0) return(0);
      if ( load_VectLI(Xgc->xdrs,&(lplot->aint)) == 0) return(0);
      if ( load_VectLI(Xgc->xdrs,&(lplot->aint_kp)) == 0) return(0);
      if (store_record(Xgc,CODEContour2D,(char *) lplot) == 0) return(0);
    }
  else 
    {
      Scistring("\nload_ Plot (contour): No more place \n");
      return(0);
    }
  return(1);
}


/*---------------------------------------------------------------------
 * gray plot 
 * ---------------------------------------------------------------------------*/

static int load_Gray(BCG *Xgc)
{
  int flag;
  struct rec_gray *lplot;
  lplot= ((struct rec_gray *) MALLOC(sizeof(struct rec_gray)));
  if (lplot != NULL)
    {
      if ( load_LI(Xgc->xdrs,&lplot->n1) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->n2) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->x)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->y)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->z)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->strflag)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->strflag_kp)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->brect)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->brect_kp)) == 0) return(0);
      if ( load_VectLI(Xgc->xdrs,&(lplot->aaint))  == 0) return(0);
      if ( load_VectLI(Xgc->xdrs,&(lplot->aaint_kp)) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&flag) == 0) return(0);
      lplot->remap= flag & 0x00f ;
      lplot->colminmax = NULL;
      lplot->zminmax = NULL;
      lplot->colout = NULL;
      if ( (flag & 0x0f0 ) != 0 )
	{ if ( load_VectLI(Xgc->xdrs,&(lplot->colminmax)) == 0) return(0);}
      if ( (flag & 0xf00 ) != 0 )
	{ if ( load_VectF(Xgc->xdrs,&(lplot->zminmax)) == 0) return(0);}
      if ( (flag & 0xf000 ) != 0 )
	{ if ( load_VectLI(Xgc->xdrs,&(lplot->colout)) == 0) return(0);}
      if (store_record(Xgc,CODEGray,(char *) lplot) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->shade)==0) return(0);
    }
  else 
    {
      Scistring("\nload_ Plot (gray): No more place \n");
      return(0);
    }
  return(1);
}

static int load_Gray1(BCG *Xgc)
{
  int flag;
  struct rec_gray1 *lplot;
  lplot= ((struct rec_gray1 *) MALLOC(sizeof(struct rec_gray1)));
  if (lplot != NULL)
    {
      lplot->x = lplot->y = NULL; /* unused in Gray1 */
      if ( load_LI(Xgc->xdrs,&lplot->n1) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->n2) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->z)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->strflag)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->strflag_kp)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->brect)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->brect_kp)) == 0) return(0);
      if ( load_VectLI(Xgc->xdrs,&(lplot->aaint))  == 0) return(0);
      if ( load_VectLI(Xgc->xdrs,&(lplot->aaint_kp)) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&flag) == 0) return(0);
      lplot->remap= flag & 0x00f ;
      lplot->colminmax = NULL;
      lplot->zminmax = NULL;
      if ( (flag & 0x0f0 ) != 0 )
	{ if ( load_VectLI(Xgc->xdrs,&(lplot->colminmax)) == 0) return(0);}
      if ( (flag & 0xf00 ) != 0 )
	{ if ( load_VectF(Xgc->xdrs,&(lplot->zminmax)) == 0) return(0);}
      if (store_record(Xgc,CODEGray1,(char *) lplot) == 0) return(0);
    }
  else 
    {
      Scistring("\nload_ Plot (gray): No more place \n");
      return(0);
    }
  return(1);
}

static int load_Gray2(BCG *Xgc)
{
  int flag;
  struct rec_gray_2 *lplot;
  lplot= ((struct rec_gray_2 *) MALLOC(sizeof(struct rec_gray_2)));
  if (lplot != NULL)
    {
      if ( load_LI(Xgc->xdrs,&lplot->n1) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->n2) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->z)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->xrect)) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&flag) == 0) return(0);
      lplot->remap= flag & 0x00f ;
      lplot->colminmax = NULL;
      lplot->zminmax = NULL;
      if ( (flag & 0x0f0 ) != 0 )
	{ if ( load_VectLI(Xgc->xdrs,&(lplot->colminmax)) == 0) return(0);}
      if ( (flag & 0xf00 ) != 0 )
	{ if ( load_VectF(Xgc->xdrs,&(lplot->zminmax)) == 0) return(0);}
      if (store_record(Xgc,CODEGray1,(char *) lplot) == 0) return(0);
    }
  else 
    {
      Scistring("\nload_ Plot (gray): No more place \n");
      return(0);
    }
  return(1);
}


/*---------------------------------------------------------------------
 * champ 
 * ---------------------------------------------------------------------------*/

static int load_Champ(BCG *Xgc)
{
  struct rec_champ *lplot;
  lplot= ((struct rec_champ *) MALLOC(sizeof(struct rec_champ)));
  if (lplot != NULL)
    {

      if ( load_LI(Xgc->xdrs,&lplot->n1) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->n2) == 0) return(0);
      if ( load_D(Xgc->xdrs,&lplot->arfact) == 0) return(0);
      if ( load_LI(Xgc->xdrs,&lplot->code)==0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->x)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->y)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->fx)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->fy)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->strflag)) == 0) return(0);
      if ( load_VectC(Xgc->xdrs,&(lplot->strflag_kp)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->vrect)) == 0) return(0);
      if ( load_VectF(Xgc->xdrs,&(lplot->vrect_kp))== 0) return(0);
      if (store_record(Xgc,CODEChamp,(char *) lplot) == 0) return(0);
    }
  else 
    {
      Scistring("\nload_ Plot (champ): No more place \n");
      return(0);
    }
  return(1);
}

/*---------------------------------------------------------------------
 * object 
 * ---------------------------------------------------------------------------*/

static int load_object(BCG *Xgc)
{
  struct rec_object *lplot;
  lplot= ((struct rec_object *) MALLOC(sizeof(struct rec_object)));
  if (lplot != NULL)
    {
      NspObject *obj = nsp_object_xdr_load(Xgc->xdrs);
      if ( obj == NULL) return 0;
      lplot->obj = obj ;
      if (store_record(Xgc,CODEobject,(char *) lplot) == 0) return(0);
    }
  else 
    {
      Scistring("\nload_ Plot (champ): No more place \n");
      return(0);
    }
  return(1);
}

/* for object which cannot be saved
 *
 */

static int load_void(BCG *Xgc)
{
  return 1;
}


/*---------------------------------------------------------------------
 *  code for reload operations 
 *---------------------------------------------------------------------------*/

static char RFname[128];
static FILE *RF ;

/* 
   static XDR rxdrs[1] ;
   static u_int rcount ;
   static u_int rszof ;
*/




typedef  struct  {
  int code;
  char *name;
  int  (*load)(BCG *);
} Load_Table;

static Load_Table load_table [] ={
  {CODEclipping_p            ,"clipping_p",       load_clipping_p },
  {CODEclipgrf		     ,"clipgrf",	  load_clipgrf },
  {CODEalufunction1	     ,"alufunction1",	  load_alufunction1 },
  {CODEbackground	     ,"background",	  load_background },
  {CODEunclip		     ,"unclip",		  load_unclip },
  {CODEclip		     ,"clip",		  load_clip },
  {CODEpattern		     ,"pattern",	  load_pattern },
  {CODEfont_size	     ,"font_size",	  load_font_size },
  {CODEfont		     ,"font",		  load_font },
  {CODEforeground	     ,"foreground",	  load_foreground },
  {CODEhidden3d		     ,"hidden3d",	  load_hidden3d },
  {CODEabsourel		     ,"absourel",	  load_absourel },
  {CODEdash		     ,"dash",		  load_dash },
  {CODEmark_size	     ,"mark_size",	  load_mark_size },
  {CODEmark		     ,"mark",		  load_mark },
  {CODEpixmapOn		     ,"pixmapOn",	  load_pixmapOn },
  {CODEthickness      	     ,"thickness",	  load_thickness },
  {CODEusecolor		     ,"usecolor",	  load_usecolor },
  {CODEshow		     ,"show",		  load_show },
  {CODEpixmapclear	     ,"pixmapclear",	  load_pixmapclear },
  {CODEdrawarc_1       	     ,"drawarc_1",	  load_drawarc_1 },
  {CODEfillarcs_1      	     ,"fillarcs_1",	  load_fillarcs_1 },
  {CODEdrawarcs_1      	     ,"drawarcs_1",	  load_drawarcs_1 },
  {CODEfillpolyline_1	     ,"fillpolyline_1",	  load_fillpolyline_1 },
  {CODEdrawarrows_1	     ,"drawarrows_1",	  load_drawarrows_1 },
  {CODEdrawaxis_1      	     ,"drawaxis_1",	  load_drawaxis_1 },
  {CODEcleararea_1	     ,"cleararea_1",	  load_cleararea_1 },
  {CODEfillarc_1       	     ,"fillarc_1",	  load_fillarc_1 },
  {CODEfillrectangle_1	     ,"fillrectangle_1",  load_fillrectangle_1 },
  {CODEdrawpolyline_1	     ,"drawpolyline_1",	  load_drawpolyline_1 },
  {CODEfillpolylines_1	     ,"fillpolylines_1",  load_fillpolylines_1 },
  {CODEdrawpolymark_1	     ,"drawpolymark_1",	  load_drawpolymark_1 },
  {CODEdisplaynumbers_1	     ,"displaynumbers_1", load_displaynumbers_1 },
  {CODEdrawpolylines_1	     ,"drawpolylines_1",  load_drawpolylines_1 },
  {CODEdrawrectangle_1	     ,"drawrectangle_1",  load_drawrectangle_1 },
  {CODEdrawrectangles_1	     ,"drawrectangles_1", load_drawrectangles_1 },
  {CODEdrawsegments_1	     ,"drawsegments_1",	  load_drawsegments_1 },
  {CODEdisplaystring_1	     ,"displaystring_1",  load_displaystring_1 },
  {CODEdisplaystringa_1	     ,"displaystringa_1", load_displaystringa_1 },
  {CODExstringb_1      	     ,"xstringb_1",	  load_xstringb_1 },
  {CODEEch		     ,"Ech",		  load_Ech },
  {CODENEch		     ,"NEch",		  load_NEch },
  {CODEPlot		     ,"Plot",		  load_Plot },
  {CODEPlot1		     ,"Plot",		  load_Plot },
  {CODEPlot2		     ,"Plot",		  load_Plot },
  {CODEPlot3		     ,"Plot",		  load_Plot },
  {CODEPlot4		     ,"Plot",		  load_Plot },
  {CODESciAxis		     ,"SciAxis",	  load_SciAxis },
  {CODEGrid		     ,"Grid",		  load_Grid },
  {CODEParam3D		     ,"Param3D",	  load_Param3D },
  {CODEParam3D1		     ,"Param3D1",	  load_Param3D1 },
  {CODEPlot3D		     ,"Plot3D",		  load_Plot3D },
  {CODEPlot3D1		     ,"Plot3D1",	  load_Plot3D },
  {CODEFac3D		     ,"Fac3D",		  load_Fac3D },
  {CODEFac3D1		     ,"Fac3D1",		  load_Fac3D },
  {CODEFac3D2		     ,"Fac3D2",		  load_Fac3D },
  {CODEFac3D3		     ,"Fac3D3",		  load_Fac3D },
  {CODEFec		     ,"Fec",		  load_Fec },
  {CODEFecN		     ,"FecN",		  load_Fec },
  {CODEContour		     ,"Contour",	  load_Contour },
  {CODEContour2D       	     ,"Contour2D",	  load_Contour2D },
  {CODEGray		     ,"Gray",		  load_Gray },
  {CODEGray1		     ,"Gray1",		  load_Gray1 },
  {CODEGray2		     ,"Gray2",		  load_Gray2 },
  {CODEChamp		     ,"Champ",		  load_Champ },
  {CODEChamp1		     ,"Champ",		  load_Champ },
  {CODEfpf_def   	     ,"fpf_def",          load_fpf_def},
  {CODEfpf   	             ,"fpf",              load_fpf},
  {CODEinitialize_gc         ,"init",             load_init},
  {CODEColormap		     ,"Colormap",	  load_colormap },
  {CODEdefault_colormap	     ,"default_colormap", load_default_colormap },
  {CODE3dobj	             ,"default_colormap", load_void },
  {CODEpixbuf		     ,"pixbuf",           load_void },
  {CODEpixbuf_file	     ,"pixbuf_file",      load_void },
  {CODEtest	             ,"test",             load_void },
  {CODEobject       	     ,"object",           load_object }
};     	

#ifdef __MSC__
#ifndef __STDC__
#define __STDC__
#endif
#endif 

int tape_load(BCG *Xgc,const char *fname1)
{
  XDR xdrs[1];
  int type,record;
  strncpy(RFname,fname1,128);
#ifdef __STDC__
  RF = fopen(RFname,"rb") ;
#else
  RF = fopen(RFname,"r") ;
#endif
  if( RF == NULL)
    {
      sciprint("fopen failed\r\n") ;
      return(0);
    }
  xdrstdio_create(xdrs, RF, XDR_DECODE) ;
  Xgc->xdrs = xdrs;
  if ( load_VectC(Xgc->xdrs,&SciF_version) == 0 ) 
    {
      sciprint("Wrong plot file : %s\n\n",fname1);
      return(0);
    }

  if ( strncmp(SciF_version,"Nsp",3) != 0 )
    {
      sciprint("Not a save graphics file: %s\n\n",fname1);
      return(0);
    }
  if ( strcmp(SciF_version,"Nsp_1.0") != 0 )
    {
      sciprint("Wrong version of saved graphics %s : %s\n\n",
	       SciF_version,fname1);
      return(0);
    }

  while ( load_LI(Xgc->xdrs,&type) != 0 ) 
    {
      /* sciprint("XXXsaved code %d\n",type); */
      if ( type < 0 || type >  CODEendplots) 
	{
	  sciprint("Something wrong while reloading %s\n\n",fname1);
	  break;
	}
      if ( type ==  CODEendplots ) break; /* normal end */
      if ( load_table[type].load(Xgc) == 0 ) break;
    }
  assert(fflush((FILE *)xdrs->x_private) != EOF) ; 
  assert(fclose(RF) != EOF) ;

  /** we plot the load_ed graphics **/
  record= Xgc->graphic_engine->xget_recording(Xgc);
  Xgc->graphic_engine->xset_recording(Xgc,TRUE);
  Xgc->graphic_engine->pixmap_resize(Xgc);
  Xgc->graphic_engine->clearwindow(Xgc);
  Xgc->graphic_engine->tape_replay(Xgc,Xgc->CurWindow);
  Xgc->graphic_engine->xset_recording(Xgc,record);
  return(0);
}

/*-------------------------------------------------------------
 * utilities for xdr coded data reloading 
 *-------------------------------------------------------------*/

static int load_D(XDR *xdrs,double *x)
{
  u_int rszof = sizeof(double) ;
  u_int rcount = (u_int) 1;
  assert( xdr_vector(xdrs, (char *)x, rcount, rszof, (xdrproc_t) xdr_double)) ;
  return(1);
}

/*
 * reload an array in a static array (i.e non allocated)
 * n must match the number of stored values in the xdr array
 */

static int load_DS(XDR *xdrs,double *x,int n)
{
  u_int rszof = sizeof(double) ;
  u_int rcount;
  assert( xdr_vector(xdrs,(char *) &rcount,(u_int)1,(u_int) sizeof(u_int), (xdrproc_t) xdr_u_int)) ;
  if (rcount != (u_int) n ) return 0;
  assert( xdr_vector(xdrs, (char *) x, rcount, rszof, (xdrproc_t) xdr_double)) ;
  return(1);
}

static int load_LI(XDR *xdrs, int *ix)
{
  u_int rszof = sizeof(int) ;
  u_int rcount = (u_int)1;
  assert( xdr_vector(xdrs, (char *)ix, rcount, rszof, (xdrproc_t) xdr_int)) ;
  return(1);
}


static int load_LIS(XDR *xdrs, int *ix, int n)
{
  int i;
  u_int rszof = sizeof(int) ;
  u_int rcount = (u_int)1;
  for ( i= 0 ; i > n ; i++) 
    assert( xdr_vector(xdrs, (char *) (ix+i) , rcount, rszof, (xdrproc_t) xdr_int)) ;
  return(1);
}
	
static int load_C(XDR *xdrs,char *c)
{
  u_int rszof;
  assert( xdr_vector(xdrs,(char *) &rszof,(u_int)1,(u_int) sizeof(u_int), (xdrproc_t) xdr_u_int)) ;
  assert( xdr_opaque(xdrs,c,rszof));
  return(1);
}

static int load_VectLI(XDR *xdrs,int **nx)
{ 
  /* Attention int peut etre un long int **/
  u_int rszof = sizeof(int) ;
  u_int rcount;
  assert( xdr_vector(xdrs,(char *) &rcount,(u_int)1,(u_int) sizeof(u_int), (xdrproc_t) xdr_u_int)) ;
  *nx = (int *)  MALLOC(rcount*sizeof(int));
  if ( *nx == NULL) return(0);
  assert( xdr_vector(xdrs, (char *)*nx, rcount, rszof, (xdrproc_t) xdr_int)) ;
  return(1);
}

static int    load_VectF(XDR *xdrs, double **nx)
{
  u_int rszof = sizeof(double) ;
  u_int rcount;
  assert( xdr_vector(xdrs,(char *) &rcount,(u_int)1,(u_int) sizeof(u_int), (xdrproc_t) xdr_u_int)) ;
  *nx = (double *)  MALLOC(rcount*sizeof(double));
  if ( *nx == NULL) return(0);
  assert( xdr_vector(xdrs, (char *) *nx, rcount, rszof, (xdrproc_t) xdr_double)) ;
  return(1);
}

static int load_VectC(XDR *xdrs, char **nx)
{
  u_int rszof;
  assert( xdr_vector(xdrs,(char *) &rszof,(u_int)1,(u_int) sizeof(u_int), (xdrproc_t) xdr_u_int)) ;
  *nx = (char *)  MALLOC(rszof);
  if ( *nx == NULL) return(0);
  assert( xdr_opaque(xdrs, *nx,rszof));
  return(1);
}

static int load_VectS(XDR *xdrs, char ***nx )
{
  int i;
  char **loc;
  u_int rcount;
  assert( xdr_vector(xdrs,(char *) &rcount,(u_int)1,(u_int) sizeof(u_int), (xdrproc_t) xdr_u_int)) ;
  if ( rcount == 0) { *nx = 0 ; return 1;} 
  if (( loc = (char **) MALLOC( (rcount+1)* sizeof(char*))) == NULL) return 0;
  for ( i=0; i < rcount ; i++) 
    {
      if ( load_VectC(xdrs,&loc[i]) == 0 ) return 0;
    }
  loc[rcount]= 0;
  *nx = loc ;
  return 1;
}

