/* Nsp
 * Copyright (C) 2015-2019 Jean-Philippe Chancelier Enpc/Cermics
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
 *--------------------------------------------------------------------------*/

#include <nsp/nsp.h>
#define PERI_PRIVATE 1
#include <nsp/object.h>
#include <nsp/matrix.h>
#include <nsp/sciio.h>
#include <nsp/graphics-new/periTikz.h>
#include <nsp/version.h>
#include <nsp/graphics-new/color.h>
#include <nsp/system.h> /* FSIZE */
#include <nsp/figure.h>
#include <nsp/axes.h>

#include <gdk/gdk.h>

#define SCALE 500.0

extern char *nsp_getenv (const char *name);
extern double nsp_predef_colors[];

static void nsp_get_color_rgb(BCG *Xgc,int color,double *rgb, NspMatrix *colors);
/* static void idfromname (char *name1, int *num); */
static double ascentPos(BCG *Xgc);
/* static int fontsizePos (BCG *Xgc); */
static const char*nsp_tikz_print_color(BCG *Xgc,int color);

#define Char2Int(x)   ( x & 0x000000ff )

#if defined(__CYGWIN32__) || defined(__MINGW32__) || defined(__GNUC__) || defined(__MSC__)
static FILE *file= (FILE *) 0;
#define FPRINTF(x) ( file != (FILE*) 0) ?  fprintf x  : 0
#else
#define FPRINTF(x) fprintf x
static FILE *file= stdout ;
#endif

/* Structure to keep the graphic state  */

BCG  ScilabGCTikz ; /* sans doute à changer XXX */

/*-----------------------------------------------------
  \encadre{General routines}
  -----------------------------------------------------*/

/** To select the graphic Window  **/

static void xselgraphic(BCG *Xgc) {}

/** End of graphic (close the file) **/

static void xendgraphic(BCG *Xgc)
{
  if (file != stdout && file != (FILE*) 0)
    {
      FPRINTF((file,"\\end{tikzpicture}\n"));
      fclose(file);
      file=stdout;
    }
}

static void xend(BCG *Xgc)
{
  xendgraphic(Xgc);
}

static void delete_window(BCG *dd,int intnum) {}

/** Clear the current graphic window     **/
/** In Postscript : nothing      **/

static void clearwindow(BCG *Xgc) {}

/** To generate a pause : Empty here **/

static void xpause(int sec_time,int events) {}

static void invalidate(BCG *Xgc,void *rect) {}
static void process_updates(BCG *Xgc) {}

/*-----------------------------------------------------------------
 * Changes the graphic window popupname
 *-----------------------------------------------------------------*/

static void setpopupname(BCG *Xgc,char *name){}

/** Wait for mouse click in graphic window : Empty here **/

static void xset_win_protect(BCG *Xgc, int val){};

static void xclick(BCG *Xgc,char *str, int *ibutton,int *imask, int *x1, int *yy1, int iflag, int motion,int release,int key,int istr) {}

static void xclick_any(BCG *Xgc,char *str, int *ibutton, int *imask,int *x1,int *yy1, int *iwin, int iflag,int getmotion,int getrelease,int getkey,int lstr) {};

static void xgetmouse(BCG *Xgc,char *str, int *ibutton,int *imask, int *x1, int *yy1, int queue,int motion,int release,int key){};

/** Clear a rectangle**/

void cleararea(BCG *Xgc,const GdkRectangle *r)
{
  FPRINTF((file,"%%[ %d %d %d %d ] clearzone\n",
	   r->x,r->y,r->width,r->height));
}

/************************************************************************
 * graphic context modifications
 ************************************************************************/

/* record or not the graphic commands */

static int xget_recording(BCG *Xgc)
{
  return TRUE; /* Xgc->record_flag; */
}

static void xset_recording(BCG *Xgc, int val)
{
  /* Xgc->record_flag = FALSE;*/ /* never record with Pos */
}


/** to get the window upper-left point coordinates **/

static void xget_windowpos(BCG *Xgc,int *x, int *y)
{
  *x = *y = 0;
}

/** to set the window upper-left point position (Void) **/

static void xset_windowpos(BCG *Xgc,int x, int y)
{
}

/** To get the window size **/
/** In Postscript we choose (600,424) **/
/** This size was chosen to have good compatibility with X11 **/
/** for line thickness etc \ldots **/

static int prec_fact =10;
static int def_width =600;
static int def_height =424;

static void xget_windowdim(BCG *Xgc,int *x, int *y)
{
  *x= def_width*prec_fact;
  *y= def_height*prec_fact;
}

/** To change the window dimensions : do Nothing in Postscript  **/

static void xset_windowdim(BCG *Xgc,int x, int y)
{
}

/** To get the popup  window size **/

static void xget_popupdim(BCG *Xgc,int *x, int *y)
{
  *x= def_width*prec_fact;
  *y= def_height*prec_fact;
}

/** To change the popup window size  **/

static void xset_popupdim(BCG *Xgc,int x, int y)
{
}

/** To get the viewport Upper/Left point Position **/

static void xget_viewport(BCG *Xgc,int *x, int *y) {       *x = *y =0;}

/** To change the window size  **/

static void xset_viewport(BCG *Xgc,int x, int y) {}

/** Select a graphic Window : Empty for Postscript **/

static int xset_curwin(int intnum, int set_menu)
{
  BCG *Xgc = &ScilabGCTikz;
  int i =  Xgc->CurWindow;
  Xgc->CurWindow  = intnum;
  return i;
}

/** Get the id number of the Current Graphic Window **/

static int xget_curwin(void)
{
  BCG *Xgc = &ScilabGCTikz;
  return  Xgc->CurWindow ;
}

/** Set a clip zone (rectangle ) **/

static void xset_clip(BCG *Xgc,const GdkRectangle *r)
{
  Xgc->ClipRegionSet = 1;
  Xgc->CurClipRegion = *r;
  FPRINTF((file,"\\clip (%f, %f) rectangle (%f, %f);\n",
	   r->x/SCALE,- r->y/SCALE,
	   (r->x+r->width)/SCALE,-(r->y+r->height)/SCALE));
}

/** unset clip zone **/

static void xset_unclip(BCG *Xgc)
{
  GdkRectangle r ={ -1,-1,200000,200000};
  Xgc->ClipRegionSet = 0;
  Xgc->CurClipRegion = r;
  FPRINTF((file,"\\clip (%f, %f) rectangle (%f, %f);\n",
	   r.x/SCALE,- r.y/SCALE,
	   (r.x+r.width)/SCALE,-(r.y+r.height)/SCALE));
}

/** Get the boundaries of the current clip zone **/

static void xget_clip(BCG *Xgc,int *x)
{
  x[0] = Xgc->ClipRegionSet;
  if ( x[0] == 1)
    {
      x[1] =Xgc->CurClipRegion.x;
      x[2] =Xgc->CurClipRegion.y;
      x[3] =Xgc->CurClipRegion.width;
      x[4] =Xgc->CurClipRegion.height;
    }
}

/*----------------------------------------------------------
  \encadre{For the drawing functions dealing with vectors of
  points, the following routine is used to select the mode
  absolute or relative }
  Absolute mode if *num==0, relative mode if *num != 0
  ------------------------------------------------------------*/

static void xset_absourel(BCG *Xgc,int num)
{
  if (num == 0 )
    Xgc->CurVectorStyle =  CoordModeOrigin;
  else
    Xgc->CurVectorStyle =  CoordModePrevious ;
}

/** to get information on absolute or relative mode **/

static int xget_absourel(BCG *Xgc)
{
  return Xgc->CurVectorStyle  ;
}

/** to set the thickness of lines : 0 is a possible value **/
/** give the thinest line **/

#define Thick_prec 5

static int xset_thickness(BCG *Xgc,int value)
{
  int old = Xgc->CurLineWidth;
  Xgc->CurLineWidth =Max(0, value);
  FPRINTF((file,"%% %d Thickness\n",
	   (int)Max(0,value*Thick_prec)));
  return old ;
}

/** to get the thicknes value **/

static int xget_thickness(BCG *Xgc)
{
  return  Xgc->CurLineWidth ;
}

/*-------------------------------------------------
  \encadre{To set grey level for filing areas.
  from black (*num =0 ) to white
  you must use the get function to get the id of
  the white pattern }
  ----------------------------------------------------*/

static int xset_color(BCG *Xgc,int color)
{
  int old = Xgc->CurColor;
  color = Max(1,color);
  if ( Xgc->CurColor == color ) return old;
  color = Max(0,Min(color-1,Xgc->Numcolors+1));
  Xgc->CurColor = color ;
  return old;
}

/** To get the id of the current pattern  **/

static int xget_color(BCG *Xgc)
{
  if ( Xgc->CurColorStatus ==1)
    {
      return  Xgc->CurColor +1 ;
    }
  else
    {
      return  Xgc->CurPattern +1 ;
    }
}


/** To get the id of the last pattern **/

static int xget_last(BCG *Xgc)
{
  return  Xgc->IDLastPattern +1 ;
}

/** To set dash-style : **/
/**  use a table of dashes and set default dashes to **/
/**  one of the possible value. value point **/
/**  to a strictly positive int **/
/**  the first entry in DashTabPos is not used since  **/
/**  0 => solid line **/

#define MAXDASH 6
static int DashTabPos[MAXDASH][4] = {
  {2,5,2,5}, {5,2,5,2},  {5,3,2,3}, {8,3,2,3},
  {11,3,2,3}, {11,3,5,3}};


static int xset_dash(BCG *Xgc,int value)
{
  int old = xget_dash(Xgc);
  int l2=4,l3 ;
  l3 = Max(0,Min(MAXDASH - 1,value - 1));
  xset_dashstyle(Xgc,l3,DashTabPos[l3],&l2);
  Xgc->CurDashStyle = l3;
  return old;
}

/* style arguments sets either dash style either color */

static void xset_line_style(BCG *Xgc,int value)
{
  if (Xgc->CurColorStatus == 0)
    xset_dash(Xgc,value);
  else {
    xset_dash(Xgc,Xgc->CurDashStyle + 1);
    xset_color(Xgc,value);
  }
}

/** To change The Pos-default dash style **/
/** if *value == 0, use a solid line, if *value != 0 **/
/** the dash style is specified by the xx vector of n values **/
/** xx[3]={5,3,7} and *n == 3 means :  5white 3 void 7 white \ldots **/

static void xset_dashstyle(BCG *Xgc,int value, int *xx, int *n)
{
  int i ;
  if ( value == 0) FPRINTF((file,"%% [] 0 setdash\n"));
  else
    {
      FPRINTF((file,"%% ["));
      for (i=0;i<*n;i++)
	FPRINTF((file,"%d ",(int) (xx[i]*prec_fact)));
      FPRINTF((file,"] 0 setdash\n"));
    }
}


/** to get the current dash-style **/

static int xget_dash(BCG *Xgc)
{
  return Xgc->CurDashStyle+1;
  /*
     if ( *value == 1)
     { if (*verbose == 1) Scistring("\nLine style = Line Solid");}
     else
     {
     value[1]=4;
     *narg = value[1]+2;
     for ( i =0 ; i < value[1]; i++) value[i+2]=DashTabPos[*value-1][i];
     if (*verbose ==1 )
     {
     sciprint("\nDash Style %d:<",(int)*value);
     for ( i =0 ; i < value[1]; i++)
     sciprint("%d ",(int)value[i+2]);
     Scistring(">\n");
     }
     }
  */
}

static void xset_usecolor(BCG *Xgc,int num)
{
  int i;
  i =  Min(Max(num,0),1);
  FPRINTF((file,"%%--use color %d\n",i));
  if ( Xgc->CurColorStatus != (int)i)
    {
      if (Xgc->CurColorStatus == 1)
	{
	  /* je passe de Couleur a n&b */
	  /* remise des couleurs a vide */
	  Xgc->CurColorStatus = 1;
	  xset_color(Xgc,1);
	  /* passage en n&b */
	  Xgc->CurColorStatus = 0;
	  i= Xgc->CurPattern+1;
	  xset_color(Xgc,i);
	  i= Xgc->CurDashStyle+1;
	  xset_dash(Xgc,i);
	  Xgc->IDLastPattern = GREYNUMBER - 1;
	  FPRINTF((file,"%%/WhiteLev %d def\n",Xgc->IDLastPattern));
	  FPRINTF((file,"%%/Setgray { WhiteLev div setgray } def \n"));
	  FPRINTF((file,"%%/Setcolor { WhiteLev div setgray } def \n"));
	  FPRINTF((file,"%%/usecolor 0  def \n"));
	}
      else
	{
	  /* je passe en couleur */
	  /* remise a zero des patterns et dash */
	  /* remise des couleurs a vide */
	  Xgc->CurColorStatus = 0;
	  xset_color(Xgc,1);
	  xset_dash(Xgc,1);
	  /* passage en couleur  */
	  Xgc->CurColorStatus = 1;
	  i= Xgc->CurColor+1;
	  xset_color(Xgc,i);
	  Xgc->IDLastPattern = Xgc->Numcolors - 1;
	  FPRINTF((file,"%%/WhiteLev %d def",Xgc->IDLastPattern));
	  FPRINTF((file,"%%/Setgray {/i exch def ColorR i get ColorG i get ColorB i get setrgbcolor } def \n"));
	  FPRINTF((file,"%%/Setcolor {/i exch def ColorR i get ColorG i get ColorB i get setrgbcolor } def \n"));
	  FPRINTF((file,"%%/usecolor 1 def \n"));
	}
    }
  FPRINTF((file,"%%--end use color %d\n",Xgc->CurColorStatus));
}


static int xget_usecolor(BCG *Xgc)
{
  return Xgc->CurColorStatus;
}

/* private pixmap for double buffering
 */


static void xset_pixmapOn(BCG *Xgc,int num) { }

static int xget_pixmapOn(BCG *Xgc) {  return Xgc->CurPixmapStatus;}

static void xset_wresize(BCG *Xgc,int num) { }

static int xget_wresize(BCG *Xgc)
{
  return Xgc->CurResizeStatus;
}




/**
 * sedeco:
 * @flag:
 *
 * setting the default colormap with colors defined in color.h
 *
 *
 * Returns:
 **/
static int set_default_colormap_flag = 1;

static void sedeco(int flag)
{
  set_default_colormap_flag = flag;
}


/**
 * xset_default_colormap:
 * @Xgc: a #BCG
 *
 * set_default_colormap is called when raising a window for the first
 * time by xset('window',...) or by getting back to default by
 * xset('default',...)
 *
 **/

static int xset_default_colormap(BCG *Xgc)
{
  int i,m= DEFAULTNUMCOLORS;
  NspMatrix *colors;
  /*  we don't want to set the default colormap at window creation
   *  if the command was xset("colormap");
   */
  if ( Xgc->CmapFlag == 1 ) return OK ; /* default colormap already */
  if (set_default_colormap_flag == 0) return OK;

  /* Allocate a new matrix for storing the default colors
   * don't forget to add three colors at the end black, white, gray
   */

  if ((colors = nsp_matrix_create("colors",'r',m,3))== NULL)
    {
      return FAIL;
    }
  else
    {
      if ( Xgc->private->a_colors != NULL)
	nsp_matrix_destroy(Xgc->private->a_colors);
      Xgc->private->a_colors = colors;
    }

  for (i = 0; i < m; i++) {
    colors->R[i] = (default_colors[3*i]/(double) 255);
    colors->R[i+ colors->m] = (default_colors[3*i+1]/(double) 255);
    colors->R[i+2*colors->m] = (default_colors[3*i+2]/(double) 255);
  }
  nsp_set_colormap_constants(Xgc,m);
  return OK;
}


/**
 * xset_colormap:
 * @Xgc: a #BCG
 * @a: should be a #NspMatrix
 *
 * a must be a m x 3 double RGB matrix:
 * a[i] = RED
 * a[i+m] = GREEN
 * a[i+2*m] = BLUE
 *
 * Returns: %OK or %FAIL
 **/

static int xset_colormap(BCG *Xgc,void *a)
{
  NspMatrix *colors, *A = (NspMatrix *) a;
  int i ;
  /* 3 colors reserved for black and white and gray */
  if ( A->n != 3 || A->m  < 0 )
    {
      Scierror("Error: a colormap must be a m x 3 array of doubles\n");
      return FAIL;
    }
  /* Checking RGB values */
  for (i = 0; i < A->m; i++)
    {
      if (A->R[i] < 0 || A->R[i] > 1 || A->R[i+A->m] < 0 || A->R[i+A->m] > 1 ||
	  A->R[i+2*A->m] < 0 || A->R[i+2*A->m]> 1)
	{
	  Scierror("Error: RGB values must be between 0 and 1\n");
	  return FAIL;
	}
    }

  /* Allocate a new matrix for storing the default colors */
  if ((colors = nsp_matrix_create("colors",'r',A->m,3))== NULL)
    {
      return FAIL;
    }
  else
    {
      if ( Xgc->private->a_colors != NULL)
	nsp_matrix_destroy(Xgc->private->a_colors);
      Xgc->private->a_colors = colors;
    }

  for (i = 0; i < A->m; i++) {
    colors->R[i] = A->R[i];
    colors->R[i+ colors->m] =A->R[i+A->m];
    colors->R[i+2*colors->m] =A->R[i+2*A->m];
  }
  nsp_set_colormap_constants(Xgc,A->m);
  return OK;
}

/**
 * xget_colormap:
 * @Xgc: a #BCG
 * @num:
 * @val:
 * @color_id:
 *
 *
 * Returns:
 **/

static void xget_colormap(BCG *Xgc, int *num,  double *val,int color_id)
{
  NspMatrix *colors = Xgc->private->a_colors;
  int m = Xgc->Numcolors,  i;
  NspFigure *F = Xgc->figure;
  NspFigureData *Gc = NULL;
  if ( F != NULL )
    {
      Gc = F->obj->gc;
      if ( Gc->colormap != NULL && Gc->colormap->mn != 0 )
	{
	  colors= Gc->colormap;
	  m = colors->m;
	}
    }
  *num = m;
  if ( val == NULL ) return;
  if ( color_id != 0 )
    {
      /* just return one color: remember that we have
       * extra predef colors at the end and that
       * colors start at 1 for nsp_get_color_rgb
       */
      nsp_get_color_rgb(Xgc,color_id,val,colors);
    }
  else
    {
      /* get all colors */
      for (i = 0; i < m; i++)
	{
	  val[i] = colors->R[i];
	  val[i+m] =  colors->R[i+colors->m];
	  val[i+2*m] = colors->R[i+2*colors->m];
	}
    }
}

/**
 * xpush_colormap:
 * @Xgc: a #BCG
 *
 * Returns:
 **/

static int xpush_colormap(BCG *Xgc,void *colors)
{
  if ( ((NspMatrix *)colors)->mn == 0) return FAIL;
  if ( Xgc->private->q_colors == NULL)
    Xgc->private->q_colors = g_queue_new();
  if ( Xgc->private->q_colors == NULL) return FAIL;
  g_queue_push_head( Xgc->private->q_colors,Xgc->private->a_colors);
  Xgc->private->a_colors = colors ;
  nsp_set_colormap_constants(Xgc,((NspMatrix *) colors)->m);
  return OK;
}

/**
 * xpop_colormap:
 * @Xgc: a #BCG
 *
 * Returns:
 **/

static int xpop_colormap(BCG *Xgc)
{
  NspMatrix *C = g_queue_pop_head(Xgc->private->q_colors);
  if ( C == NULL ) return FAIL;
  Xgc->private->a_colors = C;
  nsp_set_colormap_constants(Xgc, C->m);
  return OK;
}

/**
 * nsp_get_color_rgb:
 * @Xgc:
 * @color:
 * @rgb:
 * @colors:
 *
 *
 **/

static void nsp_get_color_rgb(BCG *Xgc,int color,double *rgb, NspMatrix *colors)
{
  if ( color <= colors->m )
    {
      rgb[0]= colors->R[color-1];
      rgb[1]= colors->R[color-1+colors->m];
      rgb[2]= colors->R[color-1+2*colors->m];
    }
  else
    {
      color -= colors->m +1 ;
      color  = Min(8,color);
      rgb[0] = nsp_predef_colors[3*color];
      rgb[1] = nsp_predef_colors[3*color+1];
      rgb[2] = nsp_predef_colors[3*color+2];
    }
}

/**
 * xset_background:
 * @Xgc: a #BCG
 * @num:
 *
 * sets the background color.
 *
 * Returns:
 **/

static void xset_background(BCG *Xgc,int num)
{
  const int predef =9; /* see nsp_predef_colors */
  if (Xgc->CurColorStatus == 1)
    {
      Xgc->NumBackground =  Max(1,Min(num,Xgc->Numcolors+predef));
    }
}

/**
 * xget_background:
 * @Xgc: a #BCG
 *
 *
 *
 * Returns:
 **/

static int  xget_background(BCG *Xgc)
{
  return ( Xgc->CurColorStatus == 1 ) ? Xgc->NumBackground: 1;
}

/**
 * xset_foreground:
 * @Xgc: a #BCG
 * @num:
 *
 *
 *
 * Returns:
 **/

static void xset_foreground(BCG *Xgc,int num)
{
  const int predef =9; /* see nsp_predef_colors */
  if (Xgc->CurColorStatus == 1)
    {
      Xgc->NumForeground =  Max(1,Min(num,Xgc->Numcolors+predef));
    }
}

/**
 * xget_foreground:
 * @Xgc: a #BCG
 *
 *
 *
 * Returns:
 **/

static int xget_foreground(BCG *Xgc)
{
  return ( Xgc->CurColorStatus == 1 ) ? Xgc->NumForeground: 1;
}

/** set and get the number of the hidden3d color */

static void xset_hidden3d(BCG *Xgc,int num)
{
  if (Xgc->CurColorStatus == 1)
    {
      /* es: Max(0,... -> Max(-1,... */
      Xgc->NumHidden3d = Max(-1,Min(num - 1,Xgc->Numcolors + 1));
    }
}

static int xget_hidden3d(BCG *Xgc)
{
  if ( Xgc->CurColorStatus == 1 )
    {
      return Xgc->NumHidden3d + 1;
    }
  else
    {
      return 1; /** the hidden3d is a solid line style in b&w */
    }
}


static void xset_autoclear(BCG *Xgc,int num)
{
  Xgc->Autoclear = Max(0,Min(1,num));
}

static void xset_autoclear_def(BCG *Xgc)
{
  Xgc->Autoclear = 0;
}

static int xget_autoclear(BCG *Xgc)
{
  return  Xgc->Autoclear;
}

static char *xget_fpf(BCG *Xgc)
{
  return( Xgc->fp_format);
}

static void xset_fpf(BCG *Xgc,char *fmt)
{
  strncpy(Xgc->fp_format,fmt,32);
  Xgc->fp_format[31]='\0';
}


static void xset_fpf_def(BCG *Xgc)
{
  Xgc->fp_format[0]='\0';
}


static void xset_pixmapclear(BCG *Xgc)
{
}

static void xset_show(BCG *Xgc)
{
  FPRINTF((file,"%% SPLIT HERE\n"));
}

static void pixmap_resize(BCG *Xgc)
{

}

/*-----------------------------------------------------------
  \encadre{Functions for drawing}
  -----------------------------------------------------------*/

/*----------------------------------------------------
  \encadre{display of a string
  at (x,y) position whith angle (alpha). Angles in degree
  positive when clockwise. If *flag ==1 a framed  box is added
  around the string.}
  -----------------------------------------------------*/

static void displaystring(BCG *Xgc,const char *string, double x, double y, int flag, double angle,
			  gr_str_posx posx, gr_str_posy posy)
{
  double rect[4] ;
  double yn = y + ascentPos(Xgc);
  boundingbox(Xgc,string,x,yn,rect);
  FPRINTF((file,"%% (%s) %5.2f %5.2f %d %5.2f [%5.2f %5.2f %5.2f %5.2f] Show\n",
	   string, x,yn ,flag,angle,rect[0],rect[1],rect[2],rect[3]));
  FPRINTF((file,"\\draw (%f, %f) node{%s};\n",x/SCALE,- yn/SCALE,string));
}

#if 0
static double bsizePos[6][4]=
  {{ 0.0,-7.0,4.63,9.0  },  /* normalement inutilise ici avec les modifs suivantes */
   { 0.0,-9.0,5.74,12.0 },
   { 0.0,-11.0,6.74,14.0},
   { 0.0,-12.0,7.79,15.0},
   {0.0, -15.0,9.72,19.0 },
   {0.0,-20.0,13.41,26.0}};
#endif

/* ajouts q&d en attendant mieux.... Bruno (le 24 Nov 2002) */

struct posfont  /* a data type for handling a postscript font in scilab */
{
  char *name;
  int asc;              /* max ascender of all printable ascii char */
  int des;              /* min descender of all printable ascii char */
  int mean_char_width;  /* the width used for all char if fixed_pitch=1 or for */
                        /* printable non ascii characters when fixed_pitch=0 */
  int fixed_pitch;
  int *char_width;      /* NULL if fixed_pitch = 1 (mean_char_width is used instead)  */
                        /* else give the width of all printable ascii character (from 32 to 126) */
};

typedef struct posfont PosFont;

/**   datas for postscript font : Courier   **/
static PosFont Courier = { "Courier",
			   750,         /* Ascender */
			   -250,         /* Descender */
			   600,         /* Mean Width */
			   1,           /* Is Fixed Pitch */
			   NULL };


/**   datas for postscript font : Symbol   **/
static int WidthSymbol[] = { 250 , 333 , 713 , 500 , 549 , 833 , 778 , 439 , 333 , 333 ,
			     500 , 549 , 250 , 549 , 250 , 278 , 500 , 500 , 500 , 500 ,
			     500 , 500 , 500 , 500 , 500 , 500 , 278 , 278 , 549 , 549 ,
			     549 , 444 , 549 , 722 , 667 , 722 , 612 , 611 , 763 , 603 ,
			     722 , 333 , 631 , 722 , 686 , 889 , 722 , 722 , 768 , 741 ,
			     556 , 592 , 611 , 690 , 439 , 768 , 645 , 795 , 611 , 333 ,
			     863 , 333 , 658 , 500 , 500 , 631 , 549 , 549 , 494 , 439 ,
			     521 , 411 , 603 , 329 , 603 , 549 , 549 , 576 , 521 , 549 ,
			     549 , 521 , 549 , 603 , 439 , 576 , 713 , 686 , 493 , 686 ,
			     494 , 480 , 200 , 480 , 549 };
static PosFont Symbol = { "Symbol",
			  917,         /* Ascender */
			  -252,         /* Descender */
			  548,         /* Mean Width */
			  0,           /* Is Fixed Pitch */
			  WidthSymbol };


/**   datas for postscript font : Times-Roman   **/
static int WidthTimesR[] = { 250 , 333 , 408 , 500 , 500 , 833 , 778 , 333 , 333 , 333 ,
			     500 , 564 , 250 , 333 , 250 , 278 , 500 , 500 , 500 , 500 ,
			     500 , 500 , 500 , 500 , 500 , 500 , 278 , 278 , 564 , 564 ,
			     564 , 444 , 921 , 722 , 667 , 667 , 722 , 611 , 556 , 722 ,
			     722 , 333 , 389 , 722 , 611 , 889 , 722 , 722 , 556 , 722 ,
			     667 , 556 , 611 , 722 , 722 , 944 , 722 , 722 , 611 , 333 ,
			     278 , 333 , 469 , 500 , 333 , 444 , 500 , 444 , 500 , 444 ,
			     333 , 500 , 500 , 278 , 278 , 500 , 278 , 778 , 500 , 500 ,
			     500 , 500 , 333 , 389 , 278 , 500 , 500 , 722 , 500 , 500 ,
			     444 , 480 , 200 , 480 , 541 };
static PosFont TimesR = { "Times-Roman",
			  727,         /* Ascender */
			  -218,         /* Descender */
			  512,         /* Mean Width */
			  0,           /* Is Fixed Pitch */
			  WidthTimesR };


/**   datas for postscript font : Times-Italic   **/
static int WidthTimesI[] = { 250 , 333 , 420 , 500 , 500 , 833 , 778 , 333 , 333 , 333 ,
			     500 , 675 , 250 , 333 , 250 , 278 , 500 , 500 , 500 , 500 ,
			     500 , 500 , 500 , 500 , 500 , 500 , 333 , 333 , 675 , 675 ,
			     675 , 500 , 920 , 611 , 611 , 667 , 722 , 611 , 611 , 722 ,
			     722 , 333 , 444 , 667 , 556 , 833 , 667 , 722 , 611 , 722 ,
			     611 , 500 , 556 , 722 , 611 , 833 , 611 , 556 , 556 , 389 ,
			     278 , 389 , 422 , 500 , 333 , 500 , 500 , 444 , 500 , 444 ,
			     278 , 500 , 500 , 278 , 278 , 444 , 278 , 722 , 500 , 500 ,
			     500 , 500 , 389 , 389 , 278 , 500 , 444 , 667 , 444 , 444 ,
			     389 , 400 , 275 , 400 , 541 };
static PosFont TimesI = { "Times-Italic",
			  731,         /* Ascender */
			  -209,         /* Descender */
			  505,         /* Mean Width */
			  0,           /* Is Fixed Pitch */
			  WidthTimesI };


/**   datas for postscript font : Times-Bold   **/
static int WidthTimesB[] = { 250 , 333 , 555 , 500 , 500 ,1000 , 833 , 333 , 333 , 333 ,
			     500 , 570 , 250 , 333 , 250 , 278 , 500 , 500 , 500 , 500 ,
			     500 , 500 , 500 , 500 , 500 , 500 , 333 , 333 , 570 , 570 ,
			     570 , 500 , 930 , 722 , 667 , 722 , 722 , 667 , 611 , 778 ,
			     778 , 389 , 500 , 778 , 667 , 944 , 722 , 778 , 611 , 778 ,
			     722 , 556 , 667 , 722 , 722 ,1000 , 722 , 722 , 667 , 333 ,
			     278 , 333 , 581 , 500 , 333 , 500 , 556 , 444 , 556 , 444 ,
			     333 , 500 , 556 , 278 , 333 , 556 , 278 , 833 , 556 , 500 ,
			     556 , 556 , 444 , 389 , 333 , 556 , 500 , 722 , 500 , 500 ,
			     444 , 394 , 220 , 394 , 520 };
static PosFont TimesB = { "Times-Bold",
			  750,         /* Ascender */
			  -206,         /* Descender */
			  536,         /* Mean Width */
			  0,           /* Is Fixed Pitch */
			  WidthTimesB };


/**   datas for postscript font : Times-BoldItalic   **/
static int WidthTimesBI[] = { 250 , 389 , 555 , 500 , 500 , 833 , 778 , 333 , 333 , 333 ,
			      500 , 570 , 250 , 333 , 250 , 278 , 500 , 500 , 500 , 500 ,
			      500 , 500 , 500 , 500 , 500 , 500 , 333 , 333 , 570 , 570 ,
			      570 , 500 , 832 , 667 , 667 , 667 , 722 , 667 , 667 , 722 ,
			      778 , 389 , 500 , 667 , 611 , 889 , 722 , 722 , 611 , 722 ,
			      667 , 556 , 611 , 722 , 667 , 889 , 667 , 611 , 611 , 333 ,
			      278 , 333 , 570 , 500 , 333 , 500 , 500 , 444 , 500 , 444 ,
			      333 , 500 , 556 , 278 , 278 , 500 , 278 , 778 , 556 , 500 ,
			      500 , 500 , 389 , 389 , 278 , 556 , 444 , 667 , 500 , 444 ,
			      389 , 348 , 220 , 348 , 570 };
static PosFont TimesBI = { "Times-BoldItalic",
			   733,         /* Ascender */
			   -208,         /* Descender */
			   515,         /* Mean Width */
			   0,           /* Is Fixed Pitch */
			   WidthTimesBI };

#define NB_MAX_POS_FONT 6
static PosFont *FontArray[NB_MAX_POS_FONT] = {&Courier, &Symbol, &TimesR, &TimesI, &TimesB, &TimesBI};

#define NB_MAX_SIZES 6
static int Font_Size_in_pts[NB_MAX_SIZES] = {8, 10, 12, 14, 18, 24};


static void PosStrBox(const char *str, int id_font, int id_size,
		      double *w, double *h)
{
  /*
   *   PURPOSE : computes the width w and the height h of a string in postscript
   *
   *      NOTE : the computed h is for most cases too large because I use the
   *             max ascender and descender of the font (the previus datas
   *             font tables contains only the width of all the printable
   *             ascii characters).
   */

  PosFont *font=FontArray[id_font];
  int nb_pts = Font_Size_in_pts[id_size];

  /* computes w */
  if ( font->fixed_pitch )
    *w =  strlen(str) * font->mean_char_width*0.001*nb_pts;
  else
    {
      int l=0, c = *str;
      while (c != '\0')
	{
	  if (32 <= c && c <= 126)
	    l += font->char_width[c-32];
	  else
	    l += font->mean_char_width;
	  c = *(++str);
	}
      *w = l*0.001*nb_pts;
    }

  /* computes h */
  *h = (font->asc - font->des)*0.001*nb_pts;
}

static double PosStrAsc(int id_font, int id_size)
{
  /*  correction pour centrer verticalement une chaine postscript il me semble ?
   *             a partir de la hauteur de la boite, h (= ascender max - descender max)
   *             on calcule  Dy = h/2 pour centrer verticalement mais ceci n'est pas
   *    -    -   tres precis => il faut une correction (et descender/2 a l'air de
   *    |    |                                          fonctionner...)
   *    |    | ascender max
   *    | h  |
   *    |    -
   *    |    | descender max (valeur negative)
   *    -    -
   *
   */
  PosFont *font=FontArray[id_font];
  int nb_pts = Font_Size_in_pts[id_size];

  return ( font->des*0.5*0.001*nb_pts );
}

/*** fin des ajouts de Bruno  ***/

/** To get the bounding rectangle of a string **/
/** we can't ask Postscript directly so we have an **/
/** approximative result in Postscript : use the X11 driver **/
/** with the same current font to have a good result **/

/*** modified by Bruno by using the previus datas and functions ***/

void boundingbox(BCG *Xgc,const char *string, int x, int y, double rect[])
{
  int font[2];
  double h, w;
  xget_font(Xgc,font,TRUE);
  PosStrBox(string, font[0], font[1], &w, &h);
  rect[0]= x;
  rect[1]= (int)(y-h*prec_fact);
  rect[2]= (int)(w*prec_fact);
  rect[3]= (int)(h*prec_fact);
  /* old code
     rect[0]= (int)(*x+bsizePos[font[1]][0]*((double) prec_fact));
     rect[1]= (int)(*y+bsizePos[font[1]][1]*((double) prec_fact));
     rect[2]= (int)(bsizePos[font[1]][2]*((double)prec_fact)*(int)strlen(string));
     rect[3]= (int)(bsizePos[font[1]][3]*((double)prec_fact));
  */
}

/* approximation of ascent using (asc + dsc) /2  */
/* modified by Bruno */

static double ascentPos(BCG *Xgc)
{
  int font[2];
  xget_font(Xgc,font, TRUE);
  return (PosStrAsc(font[0], font[1]) * prec_fact);
  /* old: return ((bsizePos[font[1]][1] +(bsizePos[font[1]][3]/2.0) ))*((double) prec_fact); */
}

/* Draw a single line in current style */

static void drawline(BCG *Xgc,double xx1, double yy1, double x2, double y2)
{
  int color=xget_color(Xgc);
  int thickness=xget_thickness(Xgc);
  FPRINTF((file,"\\draw[%s,line width=%5.2fmm] (%f, %f) --  (%f, %f); %% color=%d\n",
	   nsp_tikz_print_color(Xgc,color),
	   Max(thickness/3.0,0.33),
	   xx1/SCALE,-yy1/SCALE,x2/SCALE,-y2/SCALE,
	   color));
}

/* Draw a set of segments
 * segments are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1])
 */

static void drawsegments(BCG *Xgc, double *vx, double *vy, int n, int *style, int *width)
{
  int def_color=xget_color(Xgc), i;
  for ( i=0 ; i < n/2 ; i++)
    {
      int color = (style == NULL) ? def_color :  style[i];
      FPRINTF((file,"\\draw[%s] (%f, %f) --  (%f, %f); %% color=%d\n",
	       nsp_tikz_print_color(Xgc,color),
	       vx[2*i]/SCALE,-vy[2*i]/SCALE,
	       vx[2*i+1]/SCALE,-vy[2*i+1]/SCALE,color));
    }
}

/* Draw a set of arrows */

static void drawarrows(BCG *Xgc, double *vx, double *vy, int n, int as, int *style, int iflag)
{
  int def_color=xget_color(Xgc), i;
  for ( i=0 ; i < n/2 ; i++)
    {
      int color = (style == NULL) ? def_color :  style[i];
      FPRINTF((file,"\\draw[->,%s]",
	       nsp_tikz_print_color(Xgc,color)));
      FPRINTF((file," (%f, %f) --  (%f, %f); %% color=%d\n",
	       vx[2*i]/SCALE,-vy[2*i]/SCALE,vx[2*i+1]/SCALE,-vy[2*i+1]/SCALE,color));
    }
}


/* Draw or fill a set of rectangle
 * rectangles are defined by (vect[i],vect[i+1],vect[i+2],vect[i+3])
 * for i=0 step 4
 * (*n) : number of rectangles
 *  fillvect[*n] : specify the action (see periX11.c)
 */
#if 0
static void drawrectangles(BCG *Xgc,const double *vects,const int *fillvect, int n)
{
  Xgc->graphic_engine->generic->drawrectangles(Xgc,vects,fillvect,n);
}
#endif 

/* Draw one rectangle using current color and line width */

static void drawrectangle(BCG *Xgc, const double rect[])
{
  int color=xget_color(Xgc);
  int thickness=xget_thickness(Xgc);
  FPRINTF((file,"\\draw[%s,line width=%5.2fmm]",
	   nsp_tikz_print_color(Xgc,color),
	   Max(thickness/3.0,0.33)));
  FPRINTF((file," (%f, %f) rectangle (%f, %f);\n",
	   rect[0]/SCALE,-rect[1]/SCALE,
	   (rect[0]+rect[2])/SCALE,-(rect[1]+rect[3])/SCALE));
}

/* fill a rectangle */

static void fillrectangle(BCG *Xgc,const double rect[])
{
  int color=xget_color(Xgc);
  FPRINTF((file,"\\fill[%s] (%f, %f) rectangle (%f, %f);%% should be filled",
	   nsp_tikz_print_color(Xgc,color),
	   rect[0]/SCALE,-rect[1]/SCALE,
	   (rect[0]+rect[2])/SCALE,-(rect[1]+rect[3])/SCALE));
}

/* Draw a single ellipsis or part of it */

static void drawarc(BCG *Xgc, double arc[])
{
  /*
  int x= (arc[0] + (arc[0]+arc[2]))/2;
  int y= (arc[1] + (arc[1]+arc[3]))/2;
  */
  /* rectangle and 2 angles arc[0],arc[1],arc[2],arc[3],arc[4],arc[5]); */
}

/* Fill a single elipsis or part of it */

static void fillarc(BCG *Xgc, double arc[])
{
  /*
  int x= (arc[0] + (arc[0]+arc[2]))/2;
  int y= (arc[1] + (arc[1]+arc[3]))/2;
  */
  /* rectangle and 2 angles arc[0],arc[1],arc[2],arc[3],arc[4],arc[5]); */
}

/*
 * fills a set of polygons each of which is defined by
 * (*p) points (*n) is the number of polygons
 * the polygon is closed by the routine
 * fillvect[*n] :
 * if fillvect[i] == 0 draw the boundaries with current color
 * if fillvect[i] > 0  draw the boundaries with current color
 *               then fill with pattern fillvect[i]
 * if fillvect[i] < 0  fill with pattern - fillvect[i]
 *
 */

static void fillpolylines(BCG *Xgc, const double *vectsx, const double *vectsy, int *fillvect,int n, int p)
{
  Xgc->graphic_engine->generic->fillpolylines(Xgc,vectsx,vectsy,fillvect,n,p);
}

/** Only draw one polygon with current line style **/
/** according to *closeflag : it's a polyline or a polygon **/

static void drawpolyline( BCG *Xgc, const double *vx, const double *vy, int n,int closeflag)
{
  int i;
  int color=xget_color(Xgc);
  int thickness=xget_thickness(Xgc);
  FPRINTF((file,"\\draw[%s,line width=%5.2fmm] %% color=%d\n",
	   nsp_tikz_print_color(Xgc,color),
	   Max(thickness/3.0,0.33),color));
  /* if ( Xgc->CurVectorStyle !=  CoordModeOrigin) */
  for (i=0 ; i < n ; i++)
    {
      FPRINTF((file," (%f,%f) ",vx[i]/SCALE,- vy[i]/SCALE));
      if ( i != n-1 )
	FPRINTF((file," -- "));
    }
  if (closeflag == 1 )
    FPRINTF((file," -- cycle;\n"));
  else
    FPRINTF((file,";\n"));
}

/* Fill the polygon */

static void fillpolyline(BCG *Xgc, const double *vx, const double *vy, int n, int closeflag,int stroke_color)
{
  int i;
  int color=xget_color(Xgc);
  FPRINTF((file,"\\fill[%s] ",
	   nsp_tikz_print_color(Xgc,color)));
  for (i=0 ; i < n ; i++)
    {
      FPRINTF((file," (%f,%f) ",vx[i]/SCALE,-vy[i]/SCALE));
      if ( i != n-1 )
	FPRINTF((file," -- "));
    }
  if (closeflag == 1 )
    FPRINTF((file," -- cycle;\n"));
  else
    FPRINTF((file,";\n"));
  if (stroke_color >= 0) 
    {
      xset_color(Xgc,stroke_color);
      drawpolyline( Xgc, vx, vy, n, closeflag);
      xset_color(Xgc,color);
    }
}

/* Draw a set of  current mark centred at points defined */
/** by vx and vy (vx[i],vy[i]) */

static void drawpolymark( BCG *Xgc, double *vx, double *vy,int n)
{
  static char *symbols[] =
    {
      "$.$", /* lozenge */
      "$+$", /* plus sign */
      "$\\times$", /* multiplication sign */
      "$\\ast$", /* asterisk operator */
      "$\\diamondsuit$", /* black diamond suit */
      "$\\diamond$", /* lozenge */
      "$\\triangle$", /* greek capital letter delta */
      "$\\bigtriangledown$", /* nabla  */
      "$\\clubsuit$", /* black club suit */
      "$\\oplus$", /* circled plus */
      "$\\heartsuit$", /* black heart suit */
      "$\\spadesuit$", /* black spade suit */
      "$\\otimes$", /* circled times */
      "$\\bullet$", /* bullet */
      "$\\circ$", /* degree sign */
      "$\\blacksquare$", /* black square: ams */
      "$\\Box$"  /* white square */
    };
  int symb[2],i;
  xget_mark(Xgc,symb);
  for ( i = 0 ; i < n ; i++)
    {
      FPRINTF((file,"\\draw (%f, %f) node{%s};\n",
	       vx[i]/SCALE,-vy[i]/SCALE,
	       symbols[symb[0]]));
    }
}

/* Initialize the tikz driver */

static void *initgraphic(const char *string, int *num,int *wdim,int *wpdim,
			 double *viewport_pos,int *wpos,char mode,void *data,void *f)
{
  int x[2];
  char string1[256];
  static int EntryCounter = 0;
  int fnum;
  BCG *Xgc = &ScilabGCTikz;
  Xgc->graphic_engine = &Tikz_gengine ; /* the graphic engine associated to this graphic window */

  /* private Xgc data initialized to 0 */
  if (Xgc->private == NULL)
    {
      if ((Xgc->private = calloc(1,sizeof(gui_private)))== NULL)
	{
	  Sciprintf("initgraphics: running out of memory \n");
	  return NULL;
	}
    }

  Xgc->private->a_colors= NULL;
  Xgc->private->q_colors= NULL;

  if (EntryCounter >= 1) xendgraphic(Xgc);/* XXXX */
  strncpy(string1,string,256);
  string1[255]='\0';
  
  file=fopen(string1,"w");
  if (file == 0)
    {
      Sciprintf("Can't open file %s, I'll use stdout\r\n",string1);
      file =stdout;
    }
  if (EntryCounter == 0)
    {
      fnum=0;      loadfamily("Courier",&fnum);
      fnum=1;      loadfamily("Symbol",&fnum);
      fnum=2;      loadfamily("Times-Roman",&fnum);
      fnum=3;      loadfamily("Times-Italic",&fnum);
      fnum=4;      loadfamily("Times-Bold",&fnum);
      fnum=5;      loadfamily("Times-BoldItalic",&fnum);
    }
  Xgc->CurResizeStatus = -1;  /* to be sure that next will initialize */
  Xgc->CurColorStatus = -1;  /* to be sure that next will initialize */
  Xgc->CurPixmapStatus = -1; /* to be sure that next will initialize */

  if ( wdim != 0)
    {
      x[0]=wdim[0]*prec_fact;x[1]=wdim[1]*prec_fact;
    }
  else
    {
      xget_windowdim(Xgc,x,x+1);
    }

  FPRINTF((file,"%% -*- Mode: LaTeX -*- \n"));
  FPRINTF((file,"\\begin{tikzpicture} %%(%d,%d)(0,0)\n",
	   def_width*prec_fact,  def_height*prec_fact));

  Xgc->graphic_engine->scale->initialize_gc(Xgc);
  Xgc->scales = NULL;
  xgc_add_default_scale(Xgc);
  Xgc->CurWindow =EntryCounter;
  EntryCounter =EntryCounter +1;

  return NULL;
}


/*---------------------------------------------------------------------------
 * writes a message in the info widget associated to the current scilab window
 *----------------------------------------------------------------------------*/

static void xinfo(BCG *Xgc,char *format,...) {}


/*--------------------------------------------------------
  \encadre{Initialisation of the graphic context. Used also
  to come back to the default graphic state}
  ---------------------------------------------------------*/

static void xset_default(BCG *Xgc)
{
  nsp_initialize_gc(Xgc);
}

/* Global variables to deal with fonts */

#define FONTNUMBER 6
#define FONTMAXSIZE 6
#define SYMBOLNUMBER 17

int FontsListPos[FONTNUMBER][FONTMAXSIZE];
/* static int  isizePos[] = { 8 ,10,12,14,18,24}; */

struct SciFontInfo { int ok;
  char fname[20];} FontInfoTabPos[FONTNUMBER];

static void xset_font(BCG *Xgc,int fontid, int fontsize,int full)
{
  int i,fsiz;
  i = Min(FONTNUMBER-1,Max(fontid,0));
  fsiz = Min(FONTMAXSIZE-1,Max(fontsize,0));
  if ( FontInfoTabPos[i].ok !=1 )
    Sciprintf("\n Sorry This Font is Not available ");
  else
    {
      Xgc->fontId = i;
      Xgc->fontSize = fsiz;
      /* FontInfoTabPos[i].fname, (int) (isizePos[fsiz]*prec_fact))); */
    }
}

/** To get the values id and size of the current font **/

static void xget_font(BCG *Xgc,int *font, int full)
{
  font[0]= Xgc->fontId ;
  font[1] =Xgc->fontSize ;
}

/* To set the current mark : using the symbol font of adobe */

static void xset_mark(BCG *Xgc,int number, int size)
{
  Xgc->CurHardSymb =  Max(Min(SYMBOLNUMBER-1,number),0);
  Xgc->CurHardSymbSize =  Max(Min(FONTMAXSIZE-1,size),0);
}

/* To get the current mark id */

static void xget_mark(BCG *Xgc,int *symb)
{
  symb[0] = Xgc->CurHardSymb ;
  symb[1] = Xgc->CurHardSymbSize ;
}

/*-------------------------------------------------------
  \encadre{Check if a specified family of font exist in
  Postscript }
  -------------------------------------------------------*/

static void loadfamily(char *name, int *j)
{
  int i ;
  for ( i = 0; i < FONTMAXSIZE ; i++)
    {
      FontsListPos[*j][i] = 1;
    }
  if  (FontsListPos[*j][0] == 0 )
    Sciprintf("\n unknown font family : %s \r\n",name);
  else
    {FontInfoTabPos[*j].ok = 1;
    strcpy(FontInfoTabPos[*j].fname,name) ;}
}

/* */

static void queryfamily(char *name, int *j,int *v3)
{
    int i ;
  name[0]='\0';
  for (i=0;i<FONTNUMBER;i++) {
    strcat(name,FontInfoTabPos[i].fname);
    v3[i]=strlen(FontInfoTabPos[i].fname);
  }
  *j=FONTNUMBER;
}


static void draw_pixbuf(BCG *Xgc,void *pix,int src_x,int src_y,int dest_x,int dest_y,int width,int height)
{
  /* Xgc->graphic_engine->generic->draw_pixbuf(Xgc,pix,src_x,src_y,dest_x,dest_y,width,height);*/
  GdkPixbuf *pixbuf = pix;
  int row,col,ch;
  gdouble scale_x=10, scale_y=10;
  guchar *pixels, *p;
  int nChannels = gdk_pixbuf_get_n_channels(pixbuf);
  int pwidth = gdk_pixbuf_get_width(pixbuf);
  int pheight = gdk_pixbuf_get_height(pixbuf);
  int rowstride = gdk_pixbuf_get_rowstride (pixbuf);
  pixels = gdk_pixbuf_get_pixels (pixbuf);

  width = (width == -1 ) ? pwidth : Min(pwidth,width);
  height = (height == -1 ) ? pheight : Min(pheight,height);
  /* must change to use src_x, src_y */

  FPRINTF((file, "%% gsave\n"));
  FPRINTF((file, "%% %d %g translate\n", dest_x, dest_y + height * scale_y));
  FPRINTF((file, "%% %g %g scale\n",width * scale_x, height * scale_y));
  FPRINTF((file, "%% %d %d 8 [%d 0 0 %d 0 %d]\n",width, height, width, height, height));
  FPRINTF((file, "%%/scanline %d 3 mul string def\n", width));
  FPRINTF((file, "%%{ currentfile scanline readhexstring pop } false 3\n"));
  FPRINTF((file, "%%colorimage\n"));

  /*for(row = height-1 ; row >= 0 ; row-- )*/
  for(row = 0 ; row  < height ; row++ )
    {
      for(col =0; col < width; col++)
	{
	  p =  pixels + row * rowstride + col * nChannels;
	  for ( ch = 0 ; ch < 3 ; ch++)
	    {
	      guchar n= *(p+ch),n1,n2;
	      n1 = n & 0x0f;
	      n2 = (n & 0xf0 ) >> 4;
	      FPRINTF((file,"%c",(n2 < 10) ?  '0' + n2 : 'A' + n2 - 10));
	      FPRINTF((file,"%c",(n1 < 10) ?  '0' + n1 : 'A' + n1 - 10));
	    }
	  if(fmod(col + 1, 13) == 0) FPRINTF((file, "\n"));
	}
      FPRINTF((file,"\n"));
    }
  FPRINTF((file, "grestore\n"));
}

static void draw_pixbuf_from_file(BCG *Xgc,const char *pix,int src_x,int src_y,int dest_x,int dest_y,int width,int height)
{
  Xgc->graphic_engine->generic->draw_pixbuf_from_file(Xgc,pix,src_x,src_y,dest_x,dest_y,width,height);
}

void xstring_pango(BCG *Xgc,char *str,int rect[],char *font,int size,int markup,int position)
{

}

/* utility function: returns a string containing a color description
 */

static const char*nsp_tikz_print_color(BCG *Xgc,int color)
{
  static char buffer[256];
  double rgb[3];
  int i,black = FALSE;
  guint irgb[3];
  nsp_get_color_rgb(Xgc,color,rgb,Xgc->private->a_colors);
  //Sciprintf("color %d= ",color);
  for ( i = 0 ; i < 3 ; i++)
    {
      //Sciprintf("%f ",rgb[i]);
      irgb[i]= (guint)  (rgb[i]*255);
      //Sciprintf("(%d)",irgb[i]);
    }
  if ( irgb[0]== 0 && irgb[1]==0 && irgb[2]==0 ) black=TRUE;
  //Sciprintf("black=%d\n",black);
  if ( black )
    strcpy(buffer,"black");
  else
    sprintf(buffer,"color={rgb:red,%d;green,%d;blue,%d}",
	    irgb[0], irgb[1], irgb[2]);
  return buffer;
}
