/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#define PERI_PRIVATE 1
#include <nsp/object.h>
#include <nsp/matrix.h>
#include "nsp/sciio.h"
#include "nsp/math.h"
#include "nsp/graphics-new/periPos.h"
#include "nsp/version.h"
#include "nsp/graphics-new/color.h"
#include <nsp/system.h> /* FSIZE */

extern char *nsp_getenv (const char *name);
static void WriteGeneric1(char *string, int nobjpos, int objbeg, int sizeobj,const double *vx,const double *vy, int flag,const int *fvect);
static void Write2Vect(const double *vx,const double *vy, int from, int n, char *string, int flag, int fv);
static void WriteGeneric(char *string, int nobj, int sizeobj, const double *vx,const double *vy, int sizev, int flag, const int *fvect);
static void set_c_Pos(BCG *Xgc,int i);
/* static void idfromname (char *name1, int *num); */
static double ascentPos(BCG *Xgc);
static int fontsizePos (BCG *Xgc);
static int PosQueryFont(char *name);
static void displaysymbols( BCG *Xgc,double *vx, double *vy,int n);
static void WriteColorRGB(BCG *Xgc,char *str, void *tab, int ind);
static void WriteColorRGBDef(BCG *Xgc,char *str,void *tab, int ind);

static void get_ps_data(char mode,char *bbox,char *geom, int wdim[2]);
static int nsp_ps_header(FILE *out,char *bbox);

#define Char2Int(x)   ( x & 0x000000ff )

#if defined(__CYGWIN32__) || defined(__MINGW32__) || defined(__GNUC__) || defined(__MSC__)
static FILE *file= (FILE *) 0;
#define FPRINTF(x) ( file != (FILE*) 0) ?  fprintf x  : 0
#else
#define FPRINTF(x) fprintf x
static FILE *file= stdout ;
#endif

/** Structure to keep the graphic state  **/

BCG  ScilabGCPos ; /* sans doute à changer XXX */


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
      FPRINTF((file,"\n%%Latex:\\end{picture}"));
      FPRINTF((file,"\n showpage\n"));
      FPRINTF((file,"\n end saved restore \n"));
      fclose(file);
      file=stdout;
    }
  /* XXXXX attention, il faut nettoyer les echelles */
}

static void xend(BCG *Xgc)
{
  xendgraphic(Xgc);
}

static void delete_window(BCG *dd,int intnum) {};

/** Clear the current graphic window     **/
/** In Postscript : nothing      **/

static void clearwindow(BCG *Xgc)
{
  /* FPRINTF((file,"\n showpage")); */
  /* Sending the scale etc.. in case we want an other plot */
  /* FileInit(file); */
}

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

/** Clear a rectangle **/

void cleararea(BCG *Xgc,const GdkRectangle *r)
{
  FPRINTF((file,"\n [ %d %d %d %d ] clearzone",r->x,r->y,r->width,r->height));
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
  BCG *Xgc = &ScilabGCPos;
  int i =  Xgc->CurWindow;
  Xgc->CurWindow  = intnum;
  return i;
}

/** Get the id number of the Current Graphic Window **/

static int xget_curwin(void)
{
  BCG *Xgc = &ScilabGCPos;
  return  Xgc->CurWindow ;
}

/** Set a clip zone (rectangle ) **/

static void xset_clip(BCG *Xgc,const GdkRectangle *r)
{
  Xgc->ClipRegionSet = 1;
  Xgc->CurClipRegion = *r;
  FPRINTF((file,"\n%d %d %d %d setclipzone",r->x,r->y,r->width,r->height));
}

/** unset clip zone **/

static void xset_unclip(BCG *Xgc)
{
  Xgc->ClipRegionSet = 0;
  Xgc->CurClipRegion.x= -1;
  Xgc->CurClipRegion.y= -1;
  Xgc->CurClipRegion.width= 200000;
  Xgc->CurClipRegion.height= 200000;
  FPRINTF((file,"\n%d %d %d %d setclipzone",-1,-1,200000,200000));
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
  FPRINTF((file,"\n%d Thickness",(int)Max(0,value*Thick_prec)));
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
  the white color }
  ----------------------------------------------------*/

static int xset_color(BCG *Xgc,int color)
{
  int old = Xgc->CurColor;
  color = Max(1,color);
  if ( Xgc->CurColor == color ) return old;
  color= Max(0,Min( color-1,Xgc->Numcolors+1));
  Xgc->CurColor = color ;
  set_c_Pos(Xgc, color);
  return old;
}

#if 0
static int xset_pattern(BCG *Xgc,int num)
{
  /* used when printing from color to b&white color after GREYNUMBER
     are translated to black */
  if ( num-1 > GREYNUMBER -1 )
    i=0;
  else
    i= Max(0,Min(num-1,GREYNUMBER-1));
  Xgc->CurPattern = i;
  if (i ==0)
    FPRINTF((file,"\nfillsolid"));
  else
    FPRINTF((file,"\n%d Setgray",(int)i));
  return old;
}
#endif 

/** To get the id of the current color  **/

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


/** To get the id of the last color **/

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

static void xset_dash_and_color(BCG *Xgc,int dash,int color)
{
  xset_dash(Xgc,dash);
  xset_color(Xgc,color);
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
  if ( value == 0) FPRINTF((file,"\n[] 0 setdash"));
  else
    {
      FPRINTF((file,"\n["));
      for (i=0;i<*n;i++)
	FPRINTF((file,"%d ",(int) (xx[i]*prec_fact)));
      FPRINTF((file,"] 0 setdash"));
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

static void xget_dash_and_color(BCG *Xgc,int *dash,int *color)
{
  *dash= xget_dash(Xgc);
  *color=xget_color(Xgc);
}

static void xset_usecolor(BCG *Xgc,int num)
{
  int i;
  i =  Min(Max(num,0),1);
  FPRINTF((file,"\n%%--use color %d ",i));
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
	  FPRINTF((file,"\n/WhiteLev %d def",Xgc->IDLastPattern));
	  FPRINTF((file,"\n/Setgray { WhiteLev div setgray } def "));
	  FPRINTF((file,"\n/Setcolor { WhiteLev div setgray } def "));
	  FPRINTF((file,"\n/usecolor 0  def "));
	}
      else
	{
	  /* je passe en couleur */
	  /* remise a zero des colors et dash */
	  /* remise des couleurs a vide */
	  Xgc->CurColorStatus = 0;
	  xset_color(Xgc,1);
	  xset_dash(Xgc,1);
	  /* passage en couleur  */
	  Xgc->CurColorStatus = 1;
	  i= Xgc->CurColor+1;
	  xset_color(Xgc,i);
	  Xgc->IDLastPattern = Xgc->Numcolors - 1;
	  FPRINTF((file,"\n/WhiteLev %d def",Xgc->IDLastPattern));
	  FPRINTF((file,"\n/Setgray {/i exch def ColorR i get ColorG i get ColorB i get setrgbcolor } def "));
	  FPRINTF((file,"\n/Setcolor {/i exch def ColorR i get ColorG i get ColorB i get setrgbcolor } def "));
	  FPRINTF((file,"\n/usecolor 1 def "));
	}
    }
  FPRINTF((file,"\n%%--end use color %d ",Xgc->CurColorStatus));
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

/* setting the default colormap with colors defined in color.h */

static int set_default_colormap_flag = 1;

static void sedeco(int flag)
{
  set_default_colormap_flag = flag;
}


/*******************************************************
 * Setting the colormap
 * WARNING
 * -------
 *   This function is only used when the Postscript driver is on
 *   and xset('colormap',..) is used
 *   (i.e driver('Pos');xset('colormap',....)
 *   In the usual case (i.e when someone exports a graphic
 *   which is displayed in a window) only the graphics
 *   recorded commands are replayed and xset('colormap') belongs
 *   to the non-recorded Scilab graphic commands
 *
 *   Only the <<current colormap>> of the window is translated
 *   to Postscript when the Postscript file is opened
 *   ( see  if (  CheckColormap(&m) == 1) in FileInt)
 ******************************************************/

static int check_colors(BCG *Xgc,int m,int n,void *colors);
static int check_colors_def(BCG *Xgc,int m,int n,void *colors);

typedef void (*write_c)(BCG *Xgc,char *str, void *colors,int flag);
typedef int (*check_c)(BCG *Xgc,int m,int n,void *colors);

static int xset_colormap_gen(BCG *Xgc,int m,int n,void *colors,write_c func,check_c check)
{
  if ( check(colors,m,n,colors) == FAIL) return FAIL;
  Xgc->Numcolors = m;
  Xgc->IDLastPattern = m - 1;
  Xgc->NumForeground = m;
  Xgc->NumBackground = m + 1;
  func(Xgc,"R",colors,0);
  func(Xgc,"G",colors,1);
  func(Xgc,"B",colors,2);
  xset_usecolor(Xgc,1);
  FPRINTF((file,"\n/WhiteLev %d def",Xgc->IDLastPattern));
  FPRINTF((file,"\n/Setgray {/i exch def ColorR i get ColorG i get ColorB i get setrgbcolor } def "));
  FPRINTF((file,"\n/Setcolor {/i exch def ColorR i get ColorG i get ColorB i get setrgbcolor } def "));
  xset_color(Xgc,Xgc->NumForeground+1);
  xset_foreground(Xgc,Xgc->NumForeground+1);
  xset_background(Xgc,Xgc->NumForeground+2);
  return OK;
}


static int xset_colormap(BCG *Xgc,void *colors)
{
  NspMatrix *C = colors;
  return xset_colormap_gen(Xgc,C->m,C->n,C->R,WriteColorRGB,check_colors);
}

static int xset_default_colormap(BCG *Xgc)
{
  int   m = DEFAULTNUMCOLORS;
  return xset_colormap_gen(Xgc,m,3,default_colors,WriteColorRGBDef,check_colors_def);
}


static int check_colors_def(BCG *Xgc,int m,int n,void *colors)
{
  return OK;
}

static void WriteColorRGBDef(BCG *Xgc,char *str,void *colors, int ind)
{
  unsigned short int *tab=colors;
  int i;
  FPRINTF((file,"\n%%-- default colors "));
  FPRINTF((file,"\n/Color%s [",str));
  for ( i=0; i < Xgc->Numcolors; i++)
    {
      FPRINTF((file,"%f ",(float) tab[3*i+ind]/255.0));
      if ( (i % 10 ) == 0 ) FPRINTF((file,"\n"));
    }
  FPRINTF((file,"0.0 1.0] def"));
}

static int check_colors(BCG *Xgc,int m,int n,void *colors)
{
  int i;
  double *a=colors;
  if (n  != 3 ||  m < 0) {
    Sciprintf("Colormap must be a m x 3 array \n");
    return FAIL;
  }
  /* Checking RGB values */
  for (i = 0; i < m; i++) {
    if (a[i] < 0 || a[i] > 1 || a[i+m] < 0 || a[i+m] > 1 ||
	a[i+2*m] < 0 || a[i+2*m]> 1) {
      Sciprintf("RGB values must be between 0 and 1\n");
      return FAIL;
    }
  }
  return OK;
}

static void WriteColorRGB(BCG *Xgc,char *str, void *colors,int ind)
{
  double *tab=colors;
  int i;
  FPRINTF((file,"\n/Color%s [",str));
  for ( i=0; i < Xgc->Numcolors; i++)
    {
      FPRINTF((file,"%f ",(float) tab[i+Xgc->Numcolors*ind]));
      if ( (i % 10 ) == 0 ) FPRINTF((file,"\n"));
    }
  FPRINTF((file,"0.0 1.0] def"));
}

/* getting the colormap XXXX */

static void xget_colormap(BCG *Xgc, int *num,  double *val,int color_id)
{
  *num=0 ; /* XXX */
}

/**
 * xpush_colormap:
 * @Xgc: a #BCG
 *
 * Returns:
 **/

static int xpush_colormap(BCG *Xgc,void *colors)
{
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
  return OK;
}

/**
    Initial colormap : The arrays were filled with the numbers that we get with xget("colormap")
**/


static void set_c_Pos(BCG *Xgc,int i)
{
  int j;
  j=Max(Min(i,Xgc->Numcolors+1),0);
  FPRINTF((file,"\n%d Setcolor",(int)j));
}


/** set and get the number of the background or foreground */

static void xset_background(BCG *Xgc,int num)
{
  if (Xgc->CurColorStatus == 1)
    {
      Xgc->NumBackground = Max(0,Min(num - 1,Xgc->Numcolors + 1));
      FPRINTF((file,"\n/background %d def PaintBackground",Xgc->NumBackground));
    }
}

static int xget_background(BCG *Xgc)
{
  if ( Xgc->CurColorStatus == 1 )
    {
      return  Xgc->NumBackground + 1;
    }
  else
    {
      return 1;
    }
}


/** set and get the number of the background or foreground */

static void xset_foreground(BCG *Xgc,int num)
{
  if (Xgc->CurColorStatus == 1)
    {
      Xgc->NumForeground = Max(0,Min(num - 1,Xgc->Numcolors + 1));
    }
}

static int xget_foreground(BCG *Xgc)
{
  if ( Xgc->CurColorStatus == 1 )
    {
      return  Xgc->NumForeground + 1;
    }
  else
    {
      return  1; /** the foreground is a solid line style in b&w */
    }
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
  FPRINTF((file,"\n%% SPLIT HERE"));
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
  int i;
  double rect[4], yn = y + ascentPos(Xgc);
  boundingbox(Xgc,string,x,yn,rect);
  FPRINTF((file,"\n("));
  for ( i=0; i < (int)strlen(string);i++)
    {
      if (string[i]== '(' || string[i] == ')' )
	FPRINTF((file,"%c%c",'\\',string[i]));
      else
	FPRINTF((file,"%c",string[i]));
    }
  FPRINTF((file,") %5.2f %5.2f %d %5.2f [%5.2f %5.2f %5.2f %5.2f] Show", x,yn ,flag,angle,rect[0],rect[1],rect[2],rect[3]));
  FPRINTF((file,"\n%%Latex:\\myput{%5.2f}{%5.2f}{%d}{%s}",x,def_height*prec_fact - yn, fontsizePos(Xgc), string));
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
  xget_font(Xgc,font,FALSE);
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
  xget_font(Xgc,font, FALSE);
  return (PosStrAsc(font[0], font[1]) * prec_fact);
  /* old: return ((bsizePos[font[1]][1] +(bsizePos[font[1]][3]/2.0) ))*((double) prec_fact); */
}

/* Draw a single line in current style */

static void drawline(BCG *Xgc,double xx1, double yy1, double x2, double y2)
{
  FPRINTF((file,"\n %5.2f %5.2f %5.2f %5.2f L",xx1,yy1,x2,y2));
}

/* Draw a set of segments
 * segments are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1])
 * for i=0 step 2
 * XXX must take care of width
 */

static void drawsegments(BCG *Xgc, double *vx, double *vy, int n, int *style, int *width)
{
  int dash,color,i;
  xget_dash_and_color(Xgc,&dash,&color);
  /* store the current values */
  if ( style == NULL)
    {
      /* all segments have the same color or dash style */
      int lstyle = color;
      WriteGeneric("drawsegs",(int)1L,n*2,vx,vy,n,(int)1L,&lstyle);
    }
  else
    {
      /* each segment has its own color */
      for ( i=0 ; i < n/2 ; i++)
	{
	  int NDvalue =  style[i];
	  WriteGeneric("drawsegs",(int)1L,(int)4L,&vx[2*i],&vy[2*i],(int)2L,(int)1L,&NDvalue);
	}
    }
  xset_dash_and_color(Xgc,dash,color);
}

/* Draw a set of arrows */

static void drawarrows(BCG *Xgc, double *vx, double *vy, int n, int as, int *style, int iflag)
{
  int dash,color;
  xget_dash_and_color(Xgc,&dash,&color);
  /* store the current values */
  if ( iflag == 0 )
    {
      /** all the arrows have the same color **/
      int color1 = (*style < 1) ?  color : *style;
      xset_line_style(Xgc,color1);
      WriteGeneric("drawarrows",(int)1L,n*2,vx,vy,n,(int)1L,&as);
    }
  else
    {
      int i;
      for ( i=0 ; i < n/2 ; i++)
	{
	  xset_line_style(Xgc,style[i]);
	  /*setpattern(&NDvalue,PI0,PI0,PI0); commented out ss 13/09/00 */
	  WriteGeneric("drawarrows",(int)1L,(int)4L,&vx[2*i],&vy[2*i],(int)2L,(int)1L,&as);
	}
    }
  xset_dash_and_color(Xgc,dash,color);
}


/* Draw or fill a set of rectangle
 * rectangles are defined by (vect[i],vect[i+1],vect[i+2],vect[i+3])
 * for i=0 step 4
 * (*n) : number of rectangles
 *  fillvect[*n] : specify the action (see periX11.c)
 */

static void drawrectangles(BCG *Xgc,const double *vects,const int *fillvect, int n)
{
  int cpat =  xget_color(Xgc);
  WriteGeneric("drawbox",n,(int)4L,vects,vects,4*n,(int)0L,fillvect);
  xset_color(Xgc,cpat);
}

/* Draw one rectangle **/

static void drawrectangle(BCG *Xgc,const double rect[])
{
  int fvect= 0;
  drawrectangles(Xgc,rect,&fvect,1);
}

/** Draw a filled rectangle **/

static void fillrectangle(BCG *Xgc,const double rect[])
{
  int cpat = xget_color(Xgc);
  drawrectangles(Xgc,rect,&cpat,1);
}

/** Draw or fill a set of ellipsis or part of ellipsis **/
/** Each is defined by 6-parameters, **/
/** fillvect[*n] : specify the action <?> **/
/** caution angle=degreangle*64          **/

static void fillarcs(BCG *Xgc, double *vects, int *fillvect, int n)
{
  int cpat =  xget_color(Xgc);
  WriteGeneric("fillarc",n,(int)6L,vects,vects,6*n,(int)0L,fillvect);
  xset_color(Xgc,cpat);
}

/** Draw a single ellipsis or part of it **/
/** caution angle=degreAngle*64          **/

static void drawarc(BCG *Xgc, double arc[])
{
  int fvect;
  /** fvect set to tell that we only want to draw not to fill  */
  fvect = Xgc->IDLastPattern + 2  ;
  fillarcs(Xgc,arc,&fvect,1);
}

/** Fill a single elipsis or part of it **/
/** with current pattern **/

static void fillarc(BCG *Xgc, double arc[])
{
  int cpat= xget_color(Xgc);
  fillarcs(Xgc,arc,&cpat,1);
}

/*--------------------------------------------------------------
  \encadre{Filling or Drawing Polylines and Polygons}
  ---------------------------------------------------------------*/

/**************************************************************
  fill a set of polygons each of which is defined by
 (*p) points (*n) is the number of polygons
 the polygon is closed by the routine
 fillvect[*n] :
 if fillvect[i] == 0 draw the boundaries with current color
 if fillvect[i] > 0  draw the boundaries with current color
                then fill with pattern fillvect[i]
 if fillvect[i] < 0  fill with pattern - fillvect[i]
**************************************************************/

static void fillpolylines(BCG *Xgc, const double *vectsx, const double *vectsy, int *fillvect, int n, int p)
{
  int cpat;
  if ( Xgc->CurVectorStyle !=  CoordModeOrigin)
    FPRINTF((file,"\n/absolu false def"));
  cpat=xget_color(Xgc);
  WriteGeneric("drawpoly",n,(p)*2,vectsx,vectsy,(p)*(n),(int)1L,fillvect);
  xset_color(Xgc,cpat);
  FPRINTF((file,"\n/absolu true def"));
}

/** Only draw one polygon with current line style **/
/** according to *closeflag : it's a polyline or a polygon **/

static void drawpolyline( BCG *Xgc, const double *vx, const double *vy, int n,int closeflag)
{
  int fvect=0;
  if (closeflag == 1 )
    FPRINTF((file,"\n/closeflag true def"));
  else
    FPRINTF((file,"\n/closeflag false def"));
  fillpolylines(Xgc,vx,vy,&fvect,1,n);
}

/** Fill the polygon **/

static void fillpolyline(BCG *Xgc, const double *vx, const double *vy, int n, int closeflag, int stroke_color)
{
  int cpat = xget_color(Xgc);
  /* just fill  ==> cpat < 0 */
  cpat = -cpat;
  fillpolylines(Xgc,vx,vy,&cpat,1,n);
  if ( stroke_color >=0 ) 
    {
      xset_color(Xgc,stroke_color);
      drawpolyline( Xgc, vx, vy, n,closeflag);
      xset_color(Xgc,-cpat);
    }
}

/** Draw a set of  current mark centred at points defined **/
/** by vx and vy (vx[i],vy[i]) **/

static void drawpolymark( BCG *Xgc, double *vx, double *vy, int n)
{
  int keepid,keepsize,i=1,sz=Xgc->CurHardSymbSize;
  keepid =  Xgc->fontId;
  keepsize= Xgc->fontSize;
  xset_font(Xgc,i,sz, FALSE);
  displaysymbols(Xgc,vx,vy,n);
  xset_font(Xgc,keepid,keepsize, FALSE);
}

/*-----------------------------------------------------
 *
 * Initialize the postscrip driver
 * if wdim is non null it will be used as window dimensions
 * mode = 'p': portrait
 *        'l': landscape
 *        'k': keep window sizes
 *        'd': default
 *        'n': no_header
 *------------------------------------------------------*/

static void *initgraphic(const char *string, int *num,int *wdim,int *wpdim,double *viewport_pos,int *wpos,char mode,void *data,void *f)
{
  char bbox[256],geom[256];
  int x[2];
  char string1[256];
  static int EntryCounter = 0;
  int fnum;
  BCG *Xgc = &ScilabGCPos;
  Xgc->graphic_engine = &Pos_gengine ; /* the graphic engine associated to this graphic window */

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

  get_ps_data(mode,bbox,geom,x);

  switch (mode)
    {
    case 'p':
    case 'l':
    case 'k':
    case 'd':
      nsp_ps_header(file,bbox);
      FPRINTF((file,"%s",geom));
      break;
    default:
      /* this is just for the no header case */
      FPRINTF((file,"\n%%scipos_w=%d\n%%scipos_h=%d",(int)x[0]/2,(int)x[1]/2));
      FPRINTF((file,"\n%% Dessin en bas a gauche de taille %d,%d",(int)x[0]/2,(int)x[1]/2));
      FPRINTF((file,"\n[0.5 %d div 0 0 0.5 %d div neg  0 %d %d div] concat",
	       (int)prec_fact, (int)prec_fact,(int)x[1]/2,(int) prec_fact ));
      break;
    }

  FPRINTF((file,"\n%% Init driver "));
  FPRINTF((file,"\n/PaintBackground {WhiteLev 2 add background eq {}{ (drawbox) 4 [background 1 add] [0 0 %d %d] dogrey}ifelse } def", x[0],x[1]));
  FPRINTF((file,"\n%% End init driver "));
  FPRINTF((file,"\n%%Latex:\\begin{picture}(%d,%d)(0,0)", def_width*prec_fact,  def_height*prec_fact));

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

/*-------------------------------------------------------
  \encadre{General routine for generating Postscript Code
  to deal with Vectors. The difficulty is that the size
  of vectors is limited by Postscript, so the routine
  must check size and cut into pieces big objects}
  \begin{verbatim}
  clear (string) sizeobj [fvect[0],...,fvect[nobj]]
  (si flag=1)  [ vx[0] vy[0] vx[1] vy[1] ...... vx[sizev] vy[sizev]]
  (si flag=0)  [ vx[0] vx[1] ..... vx[sizev] ] dogrey
  \end{verbatim}
  ----------------------------------------------------------*/

/** WARNING  MAXSIZE must be a multiple of  2 4 8 et 6  **/
/** 432=8*6*9 **/

#define MAXSIZE 8320
#define PERLINE 20
#define FORMATNUM "%d "

static void WriteGeneric(char *string, int nobj, int sizeobj,const double *vx,
			 const double *vy, int sizev, int flag,const  int *fvect)
{
  int nobjpos,objbeg;
  objbeg= 0 ;
  /**-- si MAXSIZE/sizeobj vaut zero chaque objet est trop gros **/
  /** calcule le nombre d'object que l'on peut imprimer \`a la fois**/
  /** sans risquer un overflow dans un array postscript **/
  if (nobj ==0 || sizeobj ==0) return;
  nobjpos =Min( Max(1,MAXSIZE /sizeobj),nobj);
  while ( objbeg < nobj)
    {
      int objres;
      objres= nobj-objbeg;
      WriteGeneric1(string,Min(nobjpos,objres),objbeg,sizeobj,vx,vy,flag,fvect);
      objbeg = objbeg+nobjpos;
    }

}

static void WriteGeneric1(char *string, int nobjpos, int objbeg, int sizeobj, const double *vx,
			  const double *vy, int flag,const  int *fvect)
{
  int from,n,i;
  if (flag == 1)
    {  from= (objbeg*sizeobj)/2;
    n= (nobjpos*sizeobj)/2;}
  else
    {  from= (objbeg*sizeobj);
    n= (nobjpos*sizeobj);
    }
  FPRINTF((file,"\n (%s) %d [",string,(int)Min(sizeobj,MAXSIZE)));
  /** exept for the drawarrows case fvect[i] is a pattern **/
  /** and we must change it **/
  if ( strcmp(string,"drawarrows")== 0
       || strcmp(string,"drawbox")==0
       || strcmp(string,"drawpoly")==0)
    {
      for ( i =objbeg  ; i < (nobjpos+objbeg) ; i++)
	{
	  FPRINTF((file," %d",(int)fvect[i]));
	}
    }
  else
    {
      for ( i =objbeg  ; i < (nobjpos+objbeg) ; i++)
	{
	  int pat;
	  pat = Max(0,(int)fvect[i]-1);
	  FPRINTF((file," %d",pat));
	}
    }

  FPRINTF((file,"]\n"));
  /* Reste un probleme car si un unique objet doit etre dessine
     et qu'il est trop gros cet objet est decoupe en bout mais
     il manque alors les raccords eventuels */
  Write2Vect(vx,vy,from,n,string,flag,fvect[objbeg]);
}

/*--------------------------------------------------
  [  perline*valeurs de vx et vy
  ......
  .....
  ] string
  [

  ] string
  chaque zone entre [] ne doit pas contenir plus de
  maxsize valeurs.
  -------------------------------------------------------*/


void Write2Vect(const double *vx,const double *vy, int from, int n, char *string, int flag, int fv)
{
  int i,j,k,co,nco;
  int fv1;
  if ( flag == 1) nco=2*n;else nco=n;
  co = 1;
  i =0;

  if ( strcmp(string,"drawarrows")== 0)
    {
      fv1= fv ;
    }
  else
    {
      fv1 = Max(0,fv-1);
    }

  while( i < n)
    {
      if ( i > 0)
	FPRINTF((file,"\n (%s) %d [%d]\n",
		 string,(int)Min(MAXSIZE,nco-(co-1)*MAXSIZE),fv1));
      co = co +1;
      j =0;
      FPRINTF((file,"["));
      while ( j < MAXSIZE && i <n )
	{
	  k=0;
	  while ( k < PERLINE && i < n && j < MAXSIZE)
	    {
	      FPRINTF((file,FORMATNUM,(int)vx[i+from]));
	      if (flag == 1)
		{ FPRINTF((file,FORMATNUM,(int) vy[i+from]));
		k=k+2;i=i+1;j=j+2;}
	      else
		{k=k+1;i=i+1;j=j+1;}}
	  FPRINTF((file,"\n"));
	}
      FPRINTF((file,"] dogrey "));
    }
}

/** Global variables to deal with fonts **/

#define FONTNUMBER 7
#define FONTMAXSIZE 6
#define SYMBOLNUMBER 10
int FontsListPos[FONTNUMBER][FONTMAXSIZE];
struct SciFontInfo { int ok;
  char fname[20];} FontInfoTabPos[FONTNUMBER];

/* static char *sizePos[] = { "08" ,"10","12","14","18","24"}; */
static int  isizePos[] = { 8 ,10,12,14,18,24};

/** To set the current font id of font and size **/

static int fontsizePos (BCG *Xgc)
{
  return isizePos[Xgc->fontSize];
}



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
      FPRINTF((file,"\n/%s findfont %d scf mul scalefont setfont",
	       FontInfoTabPos[i].fname,
	       (int) (isizePos[fsiz]*prec_fact)));
    }
}

/** To get the values id and size of the current font **/

static void xget_font(BCG *Xgc,int *font, int full)
{
  font[0]= Xgc->fontId ;
  font[1] =Xgc->fontSize ;
}


/** To set the current mark : using the symbol font of adobe **/

static void xset_mark(BCG *Xgc,int number, int size)
{
  Xgc->CurHardSymb =  Max(Min(SYMBOLNUMBER-1,number),0);
  Xgc->CurHardSymbSize =  Max(Min(FONTMAXSIZE-1,size),0);
  ;}

/** To get the current mark id **/

static void xget_mark(BCG *Xgc,int *symb)
{
  symb[0] = Xgc->CurHardSymb ;
  symb[1] = Xgc->CurHardSymbSize ;
}

static char symb_listPos[] = {
  /*
    0x2e : . alors que 0xb7 est un o plein trop gros
    ., +,X,*,diamond(filled),diamond,triangle up,triangle down,trefle,circle*/
  (char)0x2e,(char)0x2b,(char)0xb4,(char)0xc5,(char)0xa8,
  (char)0xe0,(char)0x44,(char)0xd1,(char)0xa7,(char)0x4f};

static void displaysymbols(BCG *Xgc, double *vx, double *vy,int n)
{
  int fvect=  ( Xgc->CurColorStatus ==1) ? Xgc->CurColor : Xgc->CurPattern ;
  if ( Xgc->CurVectorStyle !=  CoordModeOrigin)
    FPRINTF((file,"\n/absolu false def"));
  FPRINTF((file,"\nHardMark 0 16#%x put",
	   Char2Int( symb_listPos[Xgc->CurHardSymb])));
  WriteGeneric("drawpolymark",(int)1L,(n)*2,vx,vy,n,(int)1L,&fvect);
  FPRINTF((file,"\n/absolu true def"));
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
      FontsListPos[*j][i] = PosQueryFont(name);
    }
  if  (FontsListPos[*j][0] == 0 )
    Sciprintf("\n unknown font family : %s \r\n",name);
  else
    {FontInfoTabPos[*j].ok = 1;
    strcpy(FontInfoTabPos[*j].fname,name) ;}
}

/*--------------------------------------------
  \encadre{always answer ok. Must be Finished}
  ---------------------------------------------*/

static int PosQueryFont(char *name)
{
  return(1);
}

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


/**
 * get_ps_data:
 * @orientation:
 * @bbox:
 * @geom:
 *
 * Utility function for postscript generation
 * return in bbox a correct bounding box
 * and in geom the proper geometric transformation
 * for postscript according to mode
 * mode = 'p' 'l' 'k' 'e';
 * wdim is only used for 'k'
 **/

static void get_ps_data(char mode,char *bbox,char *geom, int wdim[2])
{
  const int wdef = 6000, hdef = 4240;
  /* A4 paper in mm */
  double ccm = 28.346457;
  double wp = ccm*210;
  double hp = ccm*297;
  double ws=0,hs=0,sc=1.0,scx,scy,marg= ccm*5; /* margin 5mm */
  int w=wdef,h=hdef;

  if ( mode == 'k' )   {    w=wdim[0] ; h = wdim[1];   }

  /* Compute proper bounding boxes */
  switch (mode )
    {
    case 'p' : /* portrait */
      ws = (wp-2*marg)/((double) w);
      hs = (hp-2*marg)/((double) h);
      sc = Min(ws,hs);
      ws = w*sc;
      hs = h*sc;
      sprintf(bbox,"\n%%%%BoundingBox: %d %d %d %d \n",
	      (int) ((wp - ws)/(2*10.0)),
	      (int) ((hp - hs)/(2*10.0)),
	      (int) (ws/10.0 + (wp - ws)/(2*10.0)),
	      (int) (hs/10.0 + (hp - hs)/(2*10.0))
	      );
      break;
    case 'l': /* landscape */
      ws = (hp-2*marg)/((double) w);
      hs = (wp-2*marg)/((double) h);
      sc = Min(ws,hs);
      ws = w*sc;
      hs = h*sc;
      sprintf(bbox,"\n%%%%BoundingBox: %d %d %d %d \n",
	      (int) ((wp - hs)/(2*10.0)),
	      (int) ((hp - ws)/(2*10.0)),
	      (int) (hs/10.0 + (wp - hs)/(2*10.0)) ,
	      (int) (ws/10.0 + (hp - ws)/(2*10.0))
	      );
      break;
    case 'd' : /* default */
      sprintf(bbox,"\n%%%%BoundingBox: %d %d %d %d \n", 0,0,(int) (wdef/20.0),(int) (hdef/20.0));
      break;
    default:
      ws = w;
      hs = h;
      sprintf(bbox,"\n%%%%BoundingBox: %d %d %d %d \n", 0,0,(int) (ws/10.0),(int) (hs/10.0));
    }

  /* now compute the scales */

  switch (mode )
    {
    case 'p' :
      sprintf(geom,"\n[%f 20 div 0 0 %f 20 div neg %d 10 div %d 10 div] concat",
	      2*sc,2*sc,(int) ((wp - ws)/(2)), (int) ((hp - hs)/(2) + hs));
      break;
    case 'l':
      sprintf(geom,"\n90 rotate 0 neg %d neg 10 div translate\n[%f 20 div 0 0 %f 20 div neg %d 10 div %d 10 div] concat",
	      (int)(h/2.0) + (int) ((wp - hs)/(2.0)) ,2*sc,2*sc,(int) ((hp - ws)/2), (int)(h/2.0));
      break;
    case 'k':
      scx = ws/(wdef/2.0);
      scy = hs/(hdef/2.0);
      sprintf(geom,"\n[%f 20 div 0 0 %f 20 div neg %d 10 div %d 10 div] concat",
	      scx,scy,0,(int) hs);
      break;
    default:
      sprintf(geom,"\n[0.5 10 div 0 0 0.5 10 div neg  0 %d 10 div] concat",(int) (hdef/2.0));
    }
}

static void read_one_line(char **buff,int *stop,FILE *fd,int *buflen);

static int nsp_ps_header(FILE *out,char *bbox)
{
  static char *buff = NULL;
  static int buflen = 512;
  char header[FSIZE];
  char *env = nsp_getenv("SCI");
  FILE *fd;
  int stop=0;
  if (env == NULL)
    {
      Sciprintf("Environment variable SCI must be defined\n");
      return FAIL;
    }
  sprintf(header,"%s/libs/NperiPos.ps",env);
  fd=fopen(header,"r");
  if (fd == NULL)
    {
      Sciprintf("Cannot open file %s\n",header);
      return FAIL;
    }

  if ( buff == NULL)
    {
      buff = malloc(buflen*sizeof(char));
      if ( buff == NULL)
	{
	  Sciprintf("Running out of space\n");
	  return FAIL;
	}
    }
  /* ommit first line */
  read_one_line (&buff,&stop,fd,&buflen);
  FPRINTF((out,"%s","%!PS-Adobe-2.0 EPSF-2.0"));
  FPRINTF((out,"%s",bbox));
  while ( stop != 1)
    {
      read_one_line (&buff,&stop,fd,&buflen);
      FPRINTF((out,"%s",buff));
    }
  fclose(fd);
  return OK;
}


static void read_one_line(char **buff,int *stop,FILE *fd,int *buflen)
{
  int i ,c ;
  for ( i = 0 ;  (c =getc(fd)) !=  '\n' && c != EOF ; i++)
    {
      if ( i == *buflen -1 )
	{
	  *buflen += 512;
	  *buff = realloc(*buff,*buflen*sizeof(char));
	  if ( *buff == NULL)
	    {
	      fprintf(stderr,"Running out of space \n");
	      exit(1);
	    }
	}
      (*buff)[i]= c ;
    }
  if ( i+1 >= *buflen - 1 )
    {
      *buflen += 512;
      *buff = realloc(*buff,*buflen*sizeof(char));
      if ( *buff == NULL)
	{
	  fprintf(stderr,"Running out of space \n");
	  exit(1);
	}
    }
  (*buff)[i]='\n';
  (*buff)[i+1]='\0';
  if ( c == EOF) {*stop = 1;}
}

#include <gdk/gdk.h>

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

  FPRINTF((file, "\ngsave\n"));
  FPRINTF((file, "%d %g translate\n", dest_x, dest_y + height * scale_y));
  FPRINTF((file, "%g %g scale\n",width * scale_x, height * scale_y));
  FPRINTF((file, "%d %d 8 [%d 0 0 %d 0 %d]\n",width, height, width, height, height));
  FPRINTF((file, "/scanline %d 3 mul string def\n", width));
  FPRINTF((file, "{ currentfile scanline readhexstring pop } false 3\n"));
  FPRINTF((file, "colorimage\n"));

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
