/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/
/*------------------------------------------------------------------------
 * save graphic code in a file  
 * --------------------------------------------------------------------------*/

#include <string.h> /* in case of dbmalloc use */
#include <stdio.h>
#include "nsp/math.h"
#include "Graphics.h"
#include "Rec.h"

static int save_D  (double x);
static int save_LI (int ix);
static int save_C (char *c, int lc);
static int save_VectLI (int *nx, int l);
static int save_VectF (double *nx, int l); 
static int save_VectC (char *nx, int l);
static int save_VectS   (char **nx);
static int save_Colormap (void *);
static int save_Ech  (void *); 
static int save_Plot  (void *); 
static int save_SciAxis  (void *); 
static int save_Grid  (void *); 
static int save_Param3D  (void *); 
static int save_Param3D1  (void *); 
static int save_Plot3D  (void *); 
static int save_Fac3D  (void *); 
static int save_Fec  (void *); 
static int save_Contour  (void *); 
static int save_Contour2D  (void *); 
static int save_Gray  (void *); 
static int save_Champ  (void *); 

/*---------------------------------------------------------------------
 * save plots using xdr 
 * exproted function int C2F(xsaveplots) (int *winnumber,char *,int lxv); 
 *---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------
 * basic primitives 
 *---------------------------------------------------------------------------*/

static int save_clipping_p(void *theplot)
{
  struct rec_int4 *lplot  = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectLI(lplot->vals,4) == 0) return(0);
  return 1;
}

static int save_clipgrf(void * theplot ) 
{
  if ( save_LI(((struct rec_void *) theplot)->code)==0) return(0);
  return 1;
}

static int save_int(void *theplot) 
{
  struct rec_int *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_LI(lplot->val) == 0) return(0);
  return 1;
}

static int save_alufunction1(void *theplot) { return save_int(theplot); }

static int save_background(void *theplot)  { return save_int(theplot); }

static int save_unclip(void * theplot )
{
  if ( save_LI(((struct rec_void *) theplot)->code)==0) return(0);
  return 1;
}

static int save_clip(void *theplot)
{
  struct rec_int4 *lplot  = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectLI(lplot->vals,4) == 0) return(0);
  return 1;
}

static int save_pattern(void *theplot)  { return save_int(theplot); }

static int save_font_size(void *theplot)  { return save_int(theplot); }


static int save_font(void *theplot)
{
  struct rec_int2 *lplot  = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_LI(lplot->val) == 0) return(0);
  if ( save_LI(lplot->val1) == 0) return(0);
  return 1;
}

static int save_foreground(void *theplot)  { return save_int(theplot); }

static int save_hidden3d(void *theplot)  { return save_int(theplot); }

static int save_absourel(void *theplot)  { return save_int(theplot); }

static int save_dash(void *theplot)  { return save_int(theplot); }

static int save_mark_size(void *theplot)  { return save_int(theplot); }

static int save_mark(void *theplot)  
{
  struct rec_int2 *lplot  = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_LI(lplot->val) == 0) return(0);
  if ( save_LI(lplot->val1) == 0) return(0);
  return 1;
}

static int save_pixmapOn(void *theplot)  { return save_int(theplot); }
static int save_thickness(void *theplot)  { return save_int(theplot); }
static int save_usecolor(void *theplot)  { return save_int(theplot); }

static int save_show(void * theplot ) 
{
  if ( save_LI(((struct rec_void *) theplot)->code)==0) return(0);
  return 1;
}

static int save_pixmapclear(void * theplot )
{
  if ( save_LI(((struct rec_void *) theplot)->code)==0) return(0);
  return 1;
}

static int save_fpf(void * theplot )
{
  struct rec_str* lplot = theplot;
  if ( save_C( lplot->str,strlen(lplot->str)) == 0) return(0);
  return 1;
}

static int save_fpf_def(void * theplot )
{
  if ( save_LI(((struct rec_void *) theplot)->code)==0) return(0);
  return 1;
}



/*-----------------------------------------------------------------------------
 *  drawarc_1
 *-----------------------------------------------------------------------------*/

static int save_drawarc_1(void  *theplot)
{
  int i;
  struct rec_drawarc *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  for ( i = 0 ; i < 6 ; i++) 
    {
      if ( save_D(lplot->arc[i]) == 0) return(0);
    }
  return 1;
}

/*-----------------------------------------------------------------------------
 * 
 *-----------------------------------------------------------------------------*/

static int save_fillarcs_1(void  *theplot)
{
  struct rec_fillarcs *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_LI(lplot->n) == 0) return(0);
  if ( save_VectF(lplot->vects,lplot->n) == 0) return(0);
  if ( save_VectLI(lplot->fillvect,lplot->n) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  
 *-----------------------------------------------------------------------------*/

static int save_drawarcs_1(void  *theplot)
{
  struct rec_fillarcs *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_LI(lplot->n) == 0) return(0);
  if ( save_VectF(lplot->vects,lplot->n) == 0) return(0);
  if ( save_VectLI(lplot->fillvect,lplot->n) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  
 *-----------------------------------------------------------------------------*/

static int save_fillpolyline_1(void  *theplot)
{
  struct rec_fillpolyline *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_LI(lplot->n) == 0) return(0);
  if ( save_LI(lplot->closeflag) == 0) return(0);
  if ( save_VectF(lplot->vx,lplot->n) == 0) return(0);
  if ( save_VectF(lplot->vy,lplot->n) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  arrows
 *-----------------------------------------------------------------------------*/

static int save_drawarrows_1(void  *theplot)
{
  struct rec_arrows *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_LI(lplot->n) == 0) return(0);
  if ( save_D(lplot->as) == 0) return(0);
  if ( save_VectF(lplot->vx,lplot->n) == 0) return(0);
  if ( save_VectF(lplot->vy,lplot->n) == 0) return(0);
  if ( lplot->iflag != 0 )
    { 
      if ( save_VectLI(lplot->style,lplot->n/2) == 0) return(0);
    }
  else 
    {
      if ( save_LI(lplot->def_style) == 0) return(0);
    }
  return 1;
}

/*-----------------------------------------------------------------------------
 * axis 
 *-----------------------------------------------------------------------------*/

static int save_drawaxis_1(void  *theplot)
{
  struct rec_drawaxis *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_D(lplot->alpha) == 0) return(0);
  if ( save_LI(lplot->nsteps) == 0) return(0);
  if ( save_VectF(lplot->initpoint,2) == 0) return(0);
  if ( save_VectF(lplot->size,3) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  cleararea
 *-----------------------------------------------------------------------------*/

static int save_cleararea_1(void  *theplot)
{
  struct rec_double4 *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectF(lplot->vals,4) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *   fillarc
 *-----------------------------------------------------------------------------*/

static int save_fillarc_1(void  *theplot)
{
  int i;
  struct rec_drawarc *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  for ( i = 0 ; i < 6 ; i++) 
    {
      if ( save_D(lplot->arc[i]) == 0) return(0);
    }
  return 1;
}

/*-----------------------------------------------------------------------------
 *  fillrectangle
 *-----------------------------------------------------------------------------*/

static int save_fillrectangle_1(void  *theplot)
{
  struct rec_double4 *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectF(lplot->vals,4) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  drawpolyline
 *-----------------------------------------------------------------------------*/

static int save_drawpolyline_1(void  *theplot)
{
  struct rec_fillpolyline *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_LI(lplot->n) == 0) return(0);
  if ( save_LI(lplot->closeflag) == 0) return(0);
  if ( save_VectF(lplot->vx,lplot->n) == 0) return(0);
  if ( save_VectF(lplot->vy,lplot->n) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  fillpolylines
 *-----------------------------------------------------------------------------*/

static int save_fillpolylines_1(void  *theplot)
{
  struct rec_fillpolylines *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_LI(lplot->n) == 0) return(0);
  if ( save_LI(lplot->p) == 0) return(0);
  if ( save_LI(lplot->v1) == 0) return(0);
  if ( save_VectF(lplot->vx,lplot->n*lplot->p) == 0) return(0);
  if ( save_VectF(lplot->vy,lplot->n*lplot->p) == 0) return(0);
  if ( lplot->v1 == 2 ) 
    {
      if ( save_VectLI(lplot->fillvect,lplot->n*lplot->p) == 0) return(0);
    }
  else 
    {
      if ( save_VectLI(lplot->fillvect,lplot->n) == 0) return(0);
    }
  return 1;
}

/*-----------------------------------------------------------------------------
 *  drawpolymark
 *-----------------------------------------------------------------------------*/

static int save_drawpolymark_1(void  *theplot)
{
  struct rec_fillpolyline *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_LI(lplot->n) == 0) return(0);
  if ( save_LI(lplot->closeflag) == 0) return(0);
  if ( save_VectF(lplot->vx,lplot->n) == 0) return(0);
  if ( save_VectF(lplot->vy,lplot->n) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  displaynumbers
 *-----------------------------------------------------------------------------*/

static int save_displaynumbers_1(void  *theplot)
{
  struct rec_displaynumbers *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_LI(lplot->n) == 0) return(0);
  if ( save_LI(lplot->flag) == 0) return(0);
  if ( save_VectF(lplot->x,lplot->n) == 0) return(0);
  if ( save_VectF(lplot->y,lplot->n) == 0) return(0);
  if ( save_VectF(lplot->z,lplot->n) == 0) return(0);
  if ( save_VectF(lplot->alpha,lplot->n) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *   drawpolylines
 *-----------------------------------------------------------------------------*/

static int save_drawpolylines_1(void  *theplot)
{
  struct rec_drawpolylines *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_LI(lplot->n) == 0) return(0);
  if ( save_LI(lplot->p) == 0) return(0);
  if ( save_VectF(lplot->vx,lplot->n*lplot->p) == 0) return(0);
  if ( save_VectF(lplot->vy,lplot->n*lplot->p) == 0) return(0);
  if ( save_VectLI(lplot->drawvect,lplot->n) == 0) return(0);
  return 1;
}


/*-----------------------------------------------------------------------------
 *   drawrectangle
 *-----------------------------------------------------------------------------*/

static int save_drawrectangle_1(void  *theplot)
{
  struct rec_double4 *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectF(lplot->vals,4) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *   drawrectangles
 *-----------------------------------------------------------------------------*/

static int save_drawrectangles_1(void  *theplot)
{
  struct rec_fillarcs *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_LI(lplot->n) == 0) return(0);
  if ( save_VectF(lplot->vects,lplot->n) == 0) return(0);
  if ( save_VectLI(lplot->fillvect,lplot->n) == 0) return(0);
  return 1;
}


/*-----------------------------------------------------------------------------
 *  drawsegments
 *-----------------------------------------------------------------------------*/

static int save_drawsegments_1(void  *theplot)
{
  struct rec_segment *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_LI(lplot->n) == 0) return(0);
  if ( save_VectF(lplot->vx,lplot->n) == 0) return(0);
  if ( save_VectF(lplot->vy,lplot->n) == 0) return(0);
  if ( lplot->iflag != 0 ) 
    if ( save_VectLI(lplot->style,lplot->n/2) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  displaystring
 *-----------------------------------------------------------------------------*/

static int save_displaystring_1(void  *theplot)
{
  struct rec_displaystring *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_C(lplot->string,strlen(lplot->string)) == 0) return(0);
  if ( save_D(lplot->x) == 0) return(0);
  if ( save_D(lplot->y) == 0) return(0);
  if ( save_LI(lplot->flag) == 0) return(0);
  if ( save_D(lplot->angle) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 *  displaystringa
 *-----------------------------------------------------------------------------*/

static int save_displaystringa_1(void  *theplot)
{
  struct rec_displaystringa *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_C(lplot->string,strlen(lplot->string)) == 0) return(0);
  if ( save_LI(lplot->ipos) == 0) return(0);
  return 1;
}

/*-----------------------------------------------------------------------------
 * a string in a bounded box : with font size change to fit into the 
 * specified box (only works with driver which properly estimate string sizes)
 *-----------------------------------------------------------------------------*/

static int save_xstringb_1(void  *theplot)
{
  struct rec_xstringb *lplot = theplot;
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_C(lplot->string,strlen(lplot->string)) == 0) return(0);
  if ( save_D(lplot->x) == 0) return(0);
  if ( save_D(lplot->y) == 0) return(0);
  if ( save_D(lplot->wd) == 0) return(0);
  if ( save_D(lplot->hd) == 0) return(0);
  if ( save_LI(lplot->flag) == 0) return(0);
  return 1;
}

/*---------------------------------------------------------------------
 * scale 
 * ---------------------------------------------------------------------------*/

static int save_Ech(void *plot)
{
  struct rec_scale *lplot = (struct rec_scale *) plot;
  if ( save_C(lplot->logflag,2L)== 0) return(0);
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectF((lplot->Wrect),4L) == 0) return(0);
  if ( save_VectF((lplot->Frect),4L) == 0) return(0);
  if ( save_VectF((lplot->Frect_kp),4L) == 0) return(0);
  return(1);
}

static int save_NEch(void *plot)
{
  struct rec_nscale *lplot = (struct rec_nscale *) plot;
  if ( save_C(lplot->logflag,2L)== 0) return(0);
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectC((lplot->flag),((int)strlen(lplot->flag))+1) == 0) return(0);
  if ( save_VectF((lplot->Wrect),4L) == 0) return(0);
  if ( save_VectF((lplot->Frect),4L) == 0) return(0);
  if ( save_VectF((lplot->Arect),4L) == 0) return(0);
  if ( save_VectF((lplot->Frect_kp),4L) == 0) return(0);
  return(1);
}
  
/*---------------------------------------------------------------------
 * plot2d 
 *---------------------------------------------------------------------------*/

static int save_Plot(void *plot)
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
  if ( save_LI(lplot->n1)== 0) return(0);
  if ( save_LI(lplot->n2)== 0) return(0);
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectC((lplot->xf),((int)strlen(lplot->xf))+1) == 0) return(0);
  if ( ((n == 0) ? 1 : save_VectF((lplot->x),n)) == 0) return(0);
  if ( save_VectF((lplot->y),(lplot->n1)*(lplot->n2)) == 0) return(0);
  if ( save_VectLI((lplot->style),nstyle) == 0) return(0);
  if ( save_VectC((lplot->strflag),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC((lplot->strflag_kp),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC((lplot->legend),((int)strlen(lplot->legend))+1) == 0) return(0);
  if ( save_VectF((lplot->brect),4L) == 0) return(0);
  if ( save_VectF((lplot->brect_kp),4L) == 0) return(0);
  if ( save_VectLI((lplot->aint),4L) == 0) return(0);
  if ( save_VectLI((lplot->aint_kp),4L) == 0) return(0);
  return(1);
}
  
/*---------------------------------------------------------------------
 * axis 
 *---------------------------------------------------------------------------*/

static int save_SciAxis(void *plot)
{
  struct rec_sciaxis *lplot = (struct rec_sciaxis *) plot;
  char type[3] ;
  type[0] = lplot->pos;
  type[1] = lplot->xy_type;
  type[2] = lplot->logflag;
  if ( save_C(type,3L)== 0) return(0);
  if ( save_LI(lplot->nx)==0) return(0);
  if ( save_LI(lplot->ny)==0) return(0);
  if ( save_LI(lplot->subtics)==0) return(0);
  if ( save_LI(lplot->fontsize)==0) return(0);
  if ( save_LI(lplot->textcolor)==0) return(0);
  if ( save_LI(lplot->ticscolor)==0) return(0);
  if ( save_LI(lplot->seg_flag)==0) return(0);
  if ( save_LI(lplot->f_l)==0) return(0);
  if ( save_LI(lplot->code)==0) return(0);
  if ( lplot->f_l == 1) { if ( save_VectC((lplot->format),((int)strlen(lplot->format))+1) == 0) return(0);} 
  if ( save_VectF(lplot->x,lplot->nx) == 0) return(0);
  if ( save_VectF(lplot->y,lplot->ny) == 0) return(0);
  if ( save_VectS(lplot->str) == 0) return(0);
  return(1);
}

/*---------------------------------------------------------------------
 * xgrid 
 *---------------------------------------------------------------------------*/

static int save_Grid(void *plot)
{
  struct rec_xgrid *lplot = (struct rec_xgrid *) plot;
  if ( save_LI(lplot->style)== 0) return(0);
  if ( save_LI(lplot->code)==0) return(0);
  return(1);
}


/*---------------------------------------------------------------------
 * param3d 
 *---------------------------------------------------------------------------*/

static int save_Param3D(void *plot)
{
  struct rec_param3d *lplot = (struct rec_param3d *) plot;
  if ( save_LI(lplot->n)== 0) return(0);
  if ( save_D(lplot->teta)== 0) return(0);
  if ( save_D(lplot->alpha)== 0) return(0);
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectF((lplot->x),lplot->n) == 0) return(0);
  if ( save_VectF((lplot->y),lplot->n) == 0) return(0);
  if ( save_VectF((lplot->z),lplot->n) == 0) return(0);
  if ( save_VectC((lplot->legend), ((int)strlen(lplot->legend))+1) == 0) return(0);
  if ( save_VectLI((lplot->flag),3L) == 0) return(0);
  if ( save_VectF((lplot->bbox),6L)== 0) return(0);
  return(1);
}

static int save_Param3D1(void *plot)
{
  struct rec_param3d1 *lplot = (struct rec_param3d1 *) plot;
  if ( save_LI(lplot->m)== 0) return(0);
  if ( save_LI(lplot->n)== 0) return(0);
  if ( save_LI(lplot->iflag)== 0) return(0);
  if ( lplot->iflag == 1) 
    if ( save_VectLI((lplot->colors),lplot->n) == 0) return(0);
  if ( save_D(lplot->teta)== 0) return(0);
  if ( save_D(lplot->alpha)== 0) return(0);
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectF((lplot->x),lplot->n*lplot->m) == 0) return(0);
  if ( save_VectF((lplot->y),lplot->n*lplot->m) == 0) return(0);
  if ( save_VectF((lplot->z),lplot->n*lplot->m) == 0) return(0);
  if ( save_VectC((lplot->legend), ((int)strlen(lplot->legend))+1) == 0) return(0);
  if ( save_VectLI((lplot->flag),3L) == 0) return(0);
  if ( save_VectF((lplot->bbox),6L)== 0) return(0);
  return(1);
}


/*---------------------------------------------------------------------
 * plot3d  
 *---------------------------------------------------------------------------*/

static int save_Plot3D(void *plot)
{
  struct rec_plot3d *lplot = (struct rec_plot3d *) plot;
  if ( save_LI(lplot->p)== 0) return(0);
  if ( save_LI(lplot->q)== 0) return(0);
  if ( save_D(lplot->teta)== 0) return(0);
  if ( save_D(lplot->alpha)== 0) return(0);
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectF((lplot->x),lplot->p) == 0) return(0);
  if ( save_VectF((lplot->y),lplot->q) == 0) return(0);
  if ( save_VectF((lplot->z),(lplot->p)*(lplot->q)) == 0) return(0);
  if ( save_VectC((lplot->legend), ((int)strlen(lplot->legend))+1) == 0) return(0);
  if ( save_VectLI((lplot->flag),3L) == 0) return(0);
  if ( save_VectF((lplot->bbox),6L)== 0) return(0);
  return(1);
}

/*---------------------------------------------------------------------
 * fac3d 
 *---------------------------------------------------------------------------*/

static int save_Fac3D(void *plot)
{
  struct rec_fac3d *lplot = (struct rec_fac3d *) plot;
  if ( save_LI(lplot->p)== 0) return(0);
  if ( save_LI(lplot->q)== 0) return(0);
  if ( save_D(lplot->teta)== 0) return(0);
  if ( save_D(lplot->alpha)== 0) return(0);
  if ( save_LI(lplot->code)==0) return(0);
  if ( lplot->code == CODEFac3D2) 
    if ( save_VectLI((lplot->cvect),(lplot->q)) == 0) return(0);
  if ( lplot->code == CODEFac3D3) 
    if ( save_VectLI((lplot->cvect),(lplot->p)*(lplot->q)) == 0) return(0); /****** added by polpoth 4/5/2000 ******/
  if ( save_VectF((lplot->x),(lplot->p)*(lplot->q)) == 0) return(0);
  if ( save_VectF((lplot->y),(lplot->p)*(lplot->q)) == 0) return(0);
  if ( save_VectF((lplot->z),(lplot->p)*(lplot->q)) == 0) return(0);
  if ( save_VectC((lplot->legend), ((int)strlen(lplot->legend))+1) == 0) return(0);
  if ( save_VectLI((lplot->flag),3L) == 0) return(0);
  if ( save_VectF((lplot->bbox),6L)== 0) return(0);
  return(1);
}

/*---------------------------------------------------------------------
 * fec 
 *---------------------------------------------------------------------------*/

static int save_Fec(void *plot)
{
  struct rec_fec *lplot = (struct rec_fec *) plot;
  if ( save_LI(lplot->Nnode)== 0) return(0);
  if ( save_LI(lplot->Ntr)== 0) return(0);
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectF((lplot->x),lplot->Nnode) == 0) return(0);
  if ( save_VectF((lplot->y),lplot->Nnode) == 0) return(0);
  if ( save_VectF((lplot->triangles),(lplot->Ntr)*5) == 0) return(0);
  if ( save_VectF((lplot->func),lplot->Nnode ) == 0) return(0);
  if ( save_VectF((lplot->brect),4L) == 0) return(0);
  if ( save_VectF((lplot->brect_kp),4L) == 0) return(0);
  if ( save_VectF((lplot->zminmax),2L) == 0) return(0);   /* added by bruno */
  if ( save_VectLI((lplot->colminmax),2L) == 0) return(0);/* ============== */
  if ( save_VectLI((lplot->aaint),4L) == 0) return(0);
  if ( save_VectLI((lplot->aaint_kp),4L) == 0) return(0);
  if ( save_VectC((lplot->strflag),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC((lplot->strflag_kp),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC((lplot->legend),((int)strlen(lplot->legend))+1)  == 0) return(0);
  return(1);
}

/*---------------------------------------------------------------------
 * contour 
 *---------------------------------------------------------------------------*/

static int save_Contour(void *plot)
{
  struct rec_contour *lplot = (struct rec_contour *) plot;
  if ( save_LI(lplot->n1)== 0) return(0);
  if ( save_LI(lplot->n2)== 0) return(0);
  if ( save_LI(lplot->nz)== 0) return(0);
  if ( save_LI(lplot->flagnz)== 0) return(0);
  if (lplot->flagnz != 0) 
    if ( save_VectF((lplot->zz),lplot->nz) == 0) return(0);
  if ( save_D(lplot->teta)== 0) return(0);
  if ( save_D(lplot->alpha)== 0) return(0);
  if ( save_D(lplot->zlev)== 0) return(0);
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectF((lplot->x),lplot->n1) == 0) return(0);
  if ( save_VectF((lplot->y),lplot->n2) == 0) return(0);
  if ( save_VectF((lplot->z),(lplot->n1)*(lplot->n2)) == 0) return(0);
  if ( save_VectC((lplot->legend), ((int)strlen(lplot->legend))+1) == 0) return(0);
  if ( save_VectLI((lplot->flag),3L) == 0) return(0);
  if ( save_VectF((lplot->bbox),6L) == 0) return(0);
  return(1);
}

static int save_Contour2D(void *plot)
{
  int nstyle;
  struct rec_contour2d *lplot = (struct rec_contour2d *) plot;
  if (lplot->n1==1 ) nstyle= lplot->n1+1;else nstyle= lplot->n1;
  if ( save_LI(lplot->n1)== 0) return(0);
  if ( save_LI(lplot->n2)== 0) return(0);
  if ( save_LI(lplot->nz)== 0) return(0);
  if ( save_LI(lplot->flagnz)== 0) return(0);
  if (lplot->flagnz != 0) 
    if ( save_VectF((lplot->zz),lplot->nz) == 0) return(0);
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectF((lplot->x),lplot->n1) == 0) return(0);
  if ( save_VectF((lplot->y),lplot->n2) == 0) return(0);
  if ( save_VectF((lplot->z),(lplot->n1)*(lplot->n2)) == 0) return(0);
  if ( save_VectLI((lplot->style),nstyle) == 0) return(0);
  if ( save_VectC((lplot->strflag),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC((lplot->strflag_kp),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC((lplot->legend),((int)strlen(lplot->legend))+1) == 0) return(0);
  if ( save_VectF((lplot->brect),4L) == 0) return(0);
  if ( save_VectF((lplot->brect_kp),4L) == 0) return(0);
  if ( save_VectLI((lplot->aint),4L) == 0) return(0);
  if ( save_VectLI((lplot->aint_kp),4L) == 0) return(0);
  return(1);
}

/*---------------------------------------------------------------------
 * xgray 
 *---------------------------------------------------------------------------*/

static int save_Gray(void *plot)
{
  struct rec_gray *lplot = (struct rec_gray *) plot;
  if ( save_LI(lplot->n1)== 0) return(0);
  if ( save_LI(lplot->n2)== 0) return(0);
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectF((lplot->x),lplot->n1) == 0) return(0);
  if ( save_VectF((lplot->y),lplot->n2) == 0) return(0);
  if ( save_VectF((lplot->z),(lplot->n1)*(lplot->n2)) == 0) return(0);
  if ( save_VectC((lplot->strflag),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC((lplot->strflag_kp),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectF((lplot->brect),4L) == 0) return(0);
  if ( save_VectF((lplot->brect_kp),4L) == 0) return(0);
  if ( save_VectLI((lplot->aaint),4L)  == 0) return(0);
  if ( save_VectLI((lplot->aaint_kp),4L) == 0) return(0);
  return(1);
}

static int save_Gray1(void *plot)
{
  struct rec_gray *lplot = (struct rec_gray *) plot;
  if ( save_LI(lplot->n1)== 0) return(0);
  if ( save_LI(lplot->n2)== 0) return(0);
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectF((lplot->z),(lplot->n1)*(lplot->n2)) == 0) return(0);
  if ( save_VectC((lplot->strflag),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC((lplot->strflag_kp),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectF((lplot->brect),4L) == 0) return(0);
  if ( save_VectF((lplot->brect_kp),4L) == 0) return(0);
  if ( save_VectLI((lplot->aaint),4L)  == 0) return(0);
  if ( save_VectLI((lplot->aaint_kp),4L) == 0) return(0);
  return(1);
}

static int save_Gray2(void *plot)
{
  struct rec_gray_2 *lplot = (struct rec_gray_2 *) plot;
  if ( save_LI(lplot->n1)== 0) return(0);
  if ( save_LI(lplot->n2)== 0) return(0);
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectF((lplot->z),(lplot->n1)*(lplot->n2)) == 0) return(0);
  if ( save_VectF((lplot->xrect),4L) == 0) return(0);
  return(1);
}

/*---------------------------------------------------------------------
 * champ 
 *---------------------------------------------------------------------------*/

static int save_Champ(void *plot)
{
  struct rec_champ *lplot = (struct rec_champ *) plot;
  if ( save_LI(lplot->n1)== 0) return(0);
  if ( save_LI(lplot->n2)== 0) return(0);
  if ( save_D(lplot->arfact)== 0) return(0);
  if ( save_LI(lplot->code)==0) return(0);
  if ( save_VectF((lplot->x),(lplot->n1)) == 0) return(0);
  if ( save_VectF((lplot->y),(lplot->n2)) == 0) return(0);
  if ( save_VectF((lplot->fx),(lplot->n1)*(lplot->n2)) == 0) return(0);
  if ( save_VectF((lplot->fy),(lplot->n1)*(lplot->n2)) == 0) return(0);
  if ( save_VectC((lplot->strflag),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectC((lplot->strflag_kp),((int)strlen(lplot->strflag))+1) == 0) return(0);
  if ( save_VectF((lplot->vrect),4L) == 0) return(0);
  if ( save_VectF((lplot->vrect_kp),4L)== 0) return(0);
  return(1);
}

/*---------------------------------------------------------------------
 * Saving  
 *---------------------------------------------------------------------------*/

#ifdef macintosh
#	include "types.h"
#else /* not macintosh */
#       ifndef VMS
#	ifndef __ABSC__
#   	include <sys/types.h>	/* for <netinet/in.h> on some systems */
#	endif
#   	if (!defined __MSC__) && !(defined __ABSC__) && !(defined __MINGW32__) 
#          include <netinet/in.h>	/* for htonl() */
#   	endif
#	endif
#endif /* not macintosh */


#define assert(ex) {if (!(ex)){ sciprint("Graphic Load/save_ Error \r\n");return(0);}} 

#ifdef WIN32 
#include "../xdr/rpc/types.h"
#include "../xdr/rpc/xdr.h"
#else 
#include <rpc/types.h>
#include <rpc/xdr.h>
#endif

static char fname[128];
static FILE *F ;
static XDR xdrs[1] ;
static u_int count ;
static u_int szof ;


typedef  struct  {
  int code;
  char *name;
  int  (*save)(void *);
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
  {CODEColormap		     ,"Colormap",	  save_Colormap }
};     	


#ifdef __MSC__
#define __STDC__
#endif 

int tape_save(const char *fname1, int winnumber)
{
  static char scig[]={"Nsp_1.0"};
  list_plot *list;
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
      sciprint("fopen failed\r\n") ;
      return(0);
    }
  xdrstdio_create(xdrs, F, XDR_ENCODE) ;
  save_VectC(scig,((int)strlen(scig))+1) ;
  list=ListPFirst;
  if ( save_Colormap(NULL) == 0) 
    {
      sciprint("save: saving colormap failed\r\n") ;
      return(0);
    }
  while (list)
    {
      if (list->window == winnumber && list->theplot != NULL) 
	{
	  int code = ((plot_code *) list->theplot)->code;
	  if (save_LI(code) == 0) break;
	  if (save_table[code].save(list->theplot) == 0) break;
	}
      list =list->next;
    }
  save_LI(CODEendplots);
  assert(fflush((FILE *)xdrs->x_private) != EOF) ; 
  assert(fclose(F) != EOF) ;
  return(0);
}

/*---------------------------------------------------------------------------
 * utilities 
 *---------------------------------------------------------------------------*/

static int save_D(double x)
{
  szof = sizeof(double) ;
  count = 1;
  assert( xdr_vector(xdrs, (char *) &x, count, szof, (xdrproc_t) xdr_double)) ;
  return(1);
}

static int save_F(float x)
{
  double z=x;
  save_D(z);
  /** sciprint("saving %f\r\n",z); **/
  /** 
  szof = sizeof(float) ;
  count = 1;
  assert( xdr_vector(xdrs, (char *) &x, count, szof, (xdrproc_t) xdr_float)) ;
  sciprint("saving %f\r\n",x);
  **/
  return(1);
}

static int save_LI(int ix)
{
  szof = sizeof(int) ;
  count = 1;
  assert( xdr_vector(xdrs, (char *)&ix, count, szof, (xdrproc_t) xdr_int)) ;
  return(1);
}

static int save_C(char *c, int lc)
{
  szof = lc*sizeof(char);
  assert( xdr_vector(xdrs,(char *) &szof,(unsigned)1,(unsigned) sizeof(unsigned), (xdrproc_t) xdr_u_int)) ;
  assert( xdr_opaque(xdrs,c,szof));
  return(1);
}

/**  unused 

static int save_VectI(nx,l)
     int *nx;
     int l;
{ 
  int nx1=1;
  szof = sizeof(int) ;
  count = (int) l;
  assert( xdr_vector(xdrs,(char *) &count,(unsigned)1,(unsigned) sizeof(unsigned), (xdrproc_t) xdr_u_int)) ;
  if ( nx == (int *) NULL && l == (int) 1)
    {
      assert( xdr_vector(xdrs, (char *)&nx1, count, szof, (xdrproc_t) xdr_int));
    }
  else 
    {
      assert( xdr_vector(xdrs, (char *)nx, count, szof, (xdrproc_t) xdr_int)) ;
    }
  return(1);
}

**/

static int save_VectLI(int *nx, int l)
{ 
  int nx1=1;
  /** Attention int peut etre un long int **/
  szof = sizeof(int) ;
  count = (int) l;
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

static int save_VectF(double *nx, int l)
{
  double nx1=0.0;
  szof = sizeof(double) ;
  count = (int) l;
  assert( xdr_vector(xdrs,(char *) &count,(unsigned)1,(unsigned) sizeof(unsigned), (xdrproc_t) xdr_u_int)) ;
  if ( nx == (double  *) NULL && l == (int) 1)
    { assert( xdr_vector(xdrs, (char *)&nx1, count, szof, (xdrproc_t) xdr_double)) ; } 
  else
    { assert( xdr_vector(xdrs, (char *)nx, count, szof, (xdrproc_t) xdr_double)) ; } 
  return(1);
}

static int save_VectC(char *nx, int l)
{ 
  char nx1='1';
  szof = l*sizeof(char);
  assert( xdr_vector(xdrs,(char *) &szof,(unsigned)1,(unsigned) sizeof(unsigned), (xdrproc_t) xdr_u_int)) ;
  if ( nx == (char  *) NULL && l == (int) 1)
    { assert( xdr_opaque(xdrs, &nx1,szof)); } 
  else 
    { assert( xdr_opaque(xdrs, nx,szof)); }
  return(1);
}

static int save_VectS(char **nx)
{
  int scount = 0,i;
  if (nx != NULL) { while ( nx[scount] != NULL) scount++;} 
  /* save the number of strings */
  count = scount;
  assert( xdr_vector(xdrs,(char *) &count,(unsigned)1,(unsigned) sizeof(unsigned), (xdrproc_t) xdr_u_int)) ;
  /* now save each string */ 
  for ( i=0; i < scount ; i++) 
    {
      if ( save_VectC( nx[i], strlen(nx[i])+1) == 0 ) return 0;
    }
  return 1;
}

/** save the colormap if necessary **/
/** If the X window exists we check its colormap **/

static int save_Colormap(void *unused)
{
  int m;
  if (  CheckColormap(&m) == 1) 
    { 
      int i;
      float r,g,b;
      if ( save_LI(CODEColormap) == 0) return(0);
      if ( save_LI(m)== 0) return(0);
      for ( i=0; i < m ; i++)
	{
	  get_r(i,&r);
	  if ( save_F(r) == 0) return(0);
	}
      for ( i=0; i < m ; i++) 
	{
	  get_g(i,&g);
	  if ( save_F(g) == 0) return(0);
	}
      for ( i=0; i < m; i++)
	{
	  get_b(i,&b);
	  if ( save_F(b) == 0) return(0);
	}
    }
  return(1);
}

