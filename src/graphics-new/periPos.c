/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#define PERI_PRIVATE 1
#include "nsp/sciio.h"
#include "nsp/math.h"
#include "periPos.h"
#include "../version.h"
#include "color.h"

static void WriteGeneric1(char *string, int nobjpos, int objbeg, int sizeobj,const int *vx,const int *vy, int flag,const int *fvect);
static void Write2Vect(const int *vx,const  int *vy, int from, int n, char *string, int flag, int fv);
static void WriteGeneric(char *string, int nobj, int sizeobj, const int *vx,const  int *vy, int sizev, int flag, const int *fvect);
static void InitScilabGCPos(void);
static void set_c_Pos(int i);
static void idfromname (char *name1, int *num);
static double ascentPos(void);
static int fontsizePos (void);
static int PosQueryFont(char *name);
static void displaysymbols( int *vx, int *vy,int n);
static void WriteColorRGB(char *str, double *tab, int ind);
static void WriteColorRGBDef(char *str, short unsigned int *tab, int ind);

#define Char2Int(x)   ( x & 0x000000ff )

#if defined(__CYGWIN32__) || defined(__MINGW32__) || defined(__GNUC__) || defined(__MSC__)
static FILE *file= (FILE *) 0;
#define FPRINTF(x) ( file != (FILE*) 0) ?  fprintf x  : 0 
#else 
#define FPRINTF(x) fprintf x  
static FILE *file= stdout ;
#endif

void FileInit  (void);

/** Structure to keep the graphic state  **/

static struct BCG  ScilabGCPos ;

/*-----------------------------------------------------
\encadre{General routines}
-----------------------------------------------------*/

/** To select the graphic Window  **/

static void xselgraphic(void ) {}

/** End of graphic (close the file) **/

static void xendgraphic(void)
{
  if (file != stdout && file != (FILE*) 0) {
    FPRINTF((file,"\n%%Latex:\\end{picture}"));
    FPRINTF((file,"\n showpage\n"));
    FPRINTF((file,"\n end saved restore \n"));
    fclose(file);
    file=stdout;}
}

static void xend(void) 
{
  xendgraphic();
}


/** Clear the current graphic window     **/
/** In Postscript : nothing      **/

static void clearwindow(void) 
{
  /* FPRINTF((file,"\n showpage")); */
  /** Sending the scale etc.. in case we want an other plot **/
  /* FileInit(file); */
}

/** To generate a pause : Empty here **/

static void xpause(int sec_time) {}

/*-----------------------------------------------------------------
 * Changes the graphic window popupname 
 *-----------------------------------------------------------------*/

static void setpopupname(char *name){}

/** Wait for mouse click in graphic window : Empty here **/

static void xclick(char *str, int *ibutton, int *x1, int *yy1, int iflag, int motion,int release,int key,int istr) {} 

static void xclick_any(char *str, int *ibutton, int *x1, int *yy1, int *iwin, int iflag, int motion,int release,int key,int istr) {} 

static void xgetmouse(char *str, int *ibutton, int *x1, int *yy1, int queue,int motion,int release,int key){};

/** Clear a rectangle **/

void cleararea(int x, int y, int w, int h)
{
  FPRINTF((file,"\n [ %d %d %d %d ] clearzone",x,y,w,h));
}


/************************************************************************
 * graphic context modifications 
 ************************************************************************/

/** to get the window upper-left point coordinates **/

static void xget_windowpos(int *x, int *y)
{
  *x = *y = 0;
 }

/** to set the window upper-left point position (Void) **/

static void xset_windowpos(int x, int y)
{
}

/** To get the window size **/
/** In Postscript we choose (600,424) **/
/** This size was chosen to have good compatibility with X11 **/
/** for line thickness etc \ldots **/

static int prec_fact =10;
static int def_width =600;
static int def_height =424;

static void xget_windowdim(int *x, int *y)
{     
  *x= def_width*prec_fact;
  *y= def_height*prec_fact;
} 

/** To change the window dimensions : do Nothing in Postscript  **/

static void xset_windowdim(int x, int y)
{
}

/** To get the popup  window size **/

static void xget_popupdim(int *x, int *y)
{
  *x= def_width*prec_fact;
  *y= def_height*prec_fact;
} 

/** To change the popup window size  **/

static void xset_popupdim(int x, int y)
{
}

/** To get the viewport Upper/Left point Position **/

static void xget_viewport(int *x, int *y) {       *x = *y =0;} 

/** To change the window size  **/

static void xset_viewport(int x, int y) {}

/** Select a graphic Window : Empty for Postscript **/

static int xset_curwin(int intnum, int set_menu)
{
  int i =  ScilabGCPos.CurWindow;
 ScilabGCPos.CurWindow  = intnum;
 return i;
}

/** Get the id number of the Current Graphic Window **/

static int xget_curwin(void)
{
  return  ScilabGCPos.CurWindow ;
}

/** Set a clip zone (rectangle ) **/

static void xset_clip(int x[])
{
  int i;
  ScilabGCPos.ClipRegionSet = 1;
  for ( i = 0 ; i < 4 ; i++)   ScilabGCPos.CurClipRegion[i]= x[i];
  FPRINTF((file,"\n%d %d %d %d setclipzone",*x,*(x+1),*(x+2),*(x+3)));
}

/** unset clip zone **/

static void xset_unclip(void)
{
  ScilabGCPos.ClipRegionSet = 0;
  ScilabGCPos.CurClipRegion[0]= -1;
  ScilabGCPos.CurClipRegion[1]= -1;
  ScilabGCPos.CurClipRegion[2]= 200000;
  ScilabGCPos.CurClipRegion[3]= 200000;
  FPRINTF((file,"\n%d %d %d %d setclipzone",-1,-1,200000,200000));
}

/** Get the boundaries of the current clip zone **/

static void xget_clip(int *x)
{
  x[0] = ScilabGCPos.ClipRegionSet;
  if ( x[0] == 1)
    {
      x[1] =ScilabGCPos.CurClipRegion[0];
      x[2] =ScilabGCPos.CurClipRegion[1];
      x[3] =ScilabGCPos.CurClipRegion[2];
      x[4] =ScilabGCPos.CurClipRegion[3];
    }
}

/*----------------------------------------------------------
\encadre{For the drawing functions dealing with vectors of 
 points, the following routine is used to select the mode 
 absolute or relative }
 Absolute mode if *num==0, relative mode if *num != 0
------------------------------------------------------------*/

static void xset_absourel(int num)
{
  if (num == 0 )
    ScilabGCPos.CurVectorStyle =  CoordModeOrigin;
  else 
    ScilabGCPos.CurVectorStyle =  CoordModePrevious ;
}

/** to get information on absolute or relative mode **/

static int xget_absourel()
{
  return ScilabGCPos.CurVectorStyle  ;
}


/** The alu function for drawing : Works only with X11 **/
/** Not in Postscript **/

static void xset_alufunction(char *string)
{    
  int value;
  
  idfromname(string,&value);
  if ( value != -1)
    {
      ScilabGCPos.CurDrawFunction = value;
      FPRINTF((file,"\n%% %d setalufunction",(int)value));
    }
}

/** All the possibilities : Read The X11 manual to get more informations **/

struct alinfo { 
  char *name;
  char id;
  char *info;} AluStrucPos[] =
{ 
  {"GXclear" ,GXclear," 0 "},
  {"GXand" ,GXand," src AND dst "},
  {"GXandReverse" ,GXandReverse," src AND NOT dst "},
  {"GXcopy" ,GXcopy," src "},
  {"GXandInverted" ,GXandInverted," NOT src AND dst "},
  {"GXnoop" ,GXnoop," dst "},
  {"GXxor" ,GXxor," src XOR dst "},
  {"GXor" ,GXor," src OR dst "},
  {"GXnor" ,GXnor," NOT src AND NOT dst "},
  {"GXequiv" ,GXequiv," NOT src XOR dst "},
  {"GXinvert" ,GXinvert," NOT dst "},
  {"GXorReverse" ,GXorReverse," src OR NOT dst "},
  {"GXcopyInverted" ,GXcopyInverted," NOT src "},
  {"GXorInverted" ,GXorInverted," NOT src OR dst "},
  {"GXnand" ,GXnand," NOT src OR NOT dst "},
  {"GXset" ,GXset," 1 "}
};

void idfromname(char *name1, int *num)
{int i;
 *num = -1;
 for ( i =0 ; i < 16;i++)
   if (strcmp(AluStrucPos[i].name,name1)== 0) 
     *num=AluStrucPos[i].id;
 if (*num == -1 ) 
   {
     Scistring("\n Use the following keys :");
     for ( i=0 ; i < 16 ; i++)
       sciprint("\nkey %s -> %s\r\n",AluStrucPos[i].name,
	       AluStrucPos[i].info);
   }
}


static void xset_alufunction1(int num)
{     
  int value;
  value=AluStrucPos[Min(15,Max(0,num))].id;
  if ( value != -1)
    {
      ScilabGCPos.CurDrawFunction = value;
      /* to be done */
    }
}

/** To get the value of the alufunction **/

static int xget_alufunction()
{ 
  return  ScilabGCPos.CurDrawFunction ;
}

/** to set the thickness of lines : 0 is a possible value **/
/** give the thinest line **/

#define Thick_prec 5

static void xset_thickness(int value)
{ 
  ScilabGCPos.CurLineWidth =Max(0, value);
  FPRINTF((file,"\n%d Thickness",(int)Max(0,value*Thick_prec)));
}

/** to get the thicknes value **/

static int xget_thickness()
{
  return  ScilabGCPos.CurLineWidth ;
}
     

/*-------------------------------------------------
\encadre{To set grey level for filing areas.
  from black (*num =0 ) to white 
  you must use the get function to get the id of 
  the white pattern }
----------------------------------------------------*/

static int xset_pattern(int num)
{
  int i;
  int old = xget_pattern();
  if ( ScilabGCPos.CurColorStatus ==1) 
    {
      i= Max(0,Min(num-1,ScilabGCPos.Numcolors+1));
      ScilabGCPos.CurColor = i ;
      set_c_Pos(i);
    }
  else 
    {
      /* used when printing from color to b&white color after GREYNUMBER 
       are translated to black */
      if ( num-1 > GREYNUMBER -1 ) 
	i=0;
      else 
	i= Max(0,Min(num-1,GREYNUMBER-1));
      ScilabGCPos.CurPattern = i;
      if (i ==0)
	FPRINTF((file,"\nfillsolid"));
      else 
	FPRINTF((file,"\n%d Setgray",(int)i));
    }
  return old;
}

/** To get the id of the current pattern  **/

static int xget_pattern()
{ 
  if ( ScilabGCPos.CurColorStatus ==1) 
    {
      return  ScilabGCPos.CurColor +1 ;
    }
  else 
    {
      return  ScilabGCPos.CurPattern +1 ;
    }
}


/** To get the id of the last pattern **/

static int xget_last()
{
  return  ScilabGCPos.IDLastPattern +1 ;
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

/* old version of setdashPos retained for compatibility */

static void xset_dash_or_color(int value)
{
  static int  l2=4,l3 ;

  if ( ScilabGCPos.CurColorStatus == 1) 
    {
      int i;
      i= Max(0,Min(value-1,ScilabGCPos.Numcolors+1));
      ScilabGCPos.CurColor =i;
      set_c_Pos(i);
    }
  else 
    {
      l3 = Max(0,Min(MAXDASH - 1,value - 1));
      xset_dashstyle(l3,DashTabPos[l3],&l2);
      ScilabGCPos.CurDashStyle = l3;
    }
}

static int xset_dash(int value)
{
  int old = xget_dash();
  int l2=4,l3 ;
  l3 = Max(0,Min(MAXDASH - 1,value - 1));
  xset_dashstyle(l3,DashTabPos[l3],&l2);
  ScilabGCPos.CurDashStyle = l3;
  return old;
}

static void xset_dash_and_color(int dash,int color)
{
  xset_dash(dash);
  xset_pattern(color);
}

/* style arguments sets either dash style either color */

static void xset_line_style(int value)
{
  if (ScilabGCPos.CurColorStatus == 0)
    xset_dash(value);
  else {
    xset_dash(ScilabGCPos.CurDashStyle + 1);
    xset_pattern(value);
  }
}

/** To change The Pos-default dash style **/
/** if *value == 0, use a solid line, if *value != 0 **/
/** the dash style is specified by the xx vector of n values **/
/** xx[3]={5,3,7} and *n == 3 means :  5white 3 void 7 white \ldots **/
  
static void xset_dashstyle(int value, int *xx, int *n)
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
/* old version of getdashPos retained for compatibility */


static int xget_dash_or_color()
{
  return ( ScilabGCPos.CurColorStatus ==1) ?  ScilabGCPos.CurColor + 1 :  xget_dash();
}

static int xget_dash()
{
  return ScilabGCPos.CurDashStyle+1;
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

static void xget_dash_and_color(int *dash,int *color)
{
  *dash= xget_dash();
  *color=xget_pattern();
}

static void xset_usecolor(int num)
{
  int i;
  i =  Min(Max(num,0),1);
  FPRINTF((file,"\n%%--use color %d ",i));
  if ( ScilabGCPos.CurColorStatus != (int)i)
    {
      if (ScilabGCPos.CurColorStatus == 1) 
	{
	  /* je passe de Couleur a n&b */
	  /* remise des couleurs a vide */
	  ScilabGCPos.CurColorStatus = 1;
	  xset_pattern(1);
	  /* passage en n&b */
	  ScilabGCPos.CurColorStatus = 0;
	  i= ScilabGCPos.CurPattern+1;
	  xset_pattern(i);
	  i= ScilabGCPos.CurDashStyle+1;
	  xset_dash(i);
	  ScilabGCPos.IDLastPattern = GREYNUMBER - 1;
	  FPRINTF((file,"\n/WhiteLev %d def",ScilabGCPos.IDLastPattern));
	  FPRINTF((file,"\n/Setgray { WhiteLev div setgray } def "));
	  FPRINTF((file,"\n/Setcolor { WhiteLev div setgray } def "));
	  FPRINTF((file,"\n/usecolor 0  def "));
	}
      else 
	{
	  /* je passe en couleur */
	  /* remise a zero des patterns et dash */
	  /* remise des couleurs a vide */
	  ScilabGCPos.CurColorStatus = 0;
	  xset_pattern(1);
	  xset_dash(1);
	  /* passage en couleur  */
	  ScilabGCPos.CurColorStatus = 1;
	  i= ScilabGCPos.CurColor+1;
	  xset_pattern(i);
	  ScilabGCPos.IDLastPattern = ScilabGCPos.Numcolors - 1;
	  FPRINTF((file,"\n/WhiteLev %d def",ScilabGCPos.IDLastPattern));
	  FPRINTF((file,"\n/Setgray {/i exch def ColorR i get ColorG i get ColorB i get setrgbcolor } def "));
	  FPRINTF((file,"\n/Setcolor {/i exch def ColorR i get ColorG i get ColorB i get setrgbcolor } def "));
	  FPRINTF((file,"\n/usecolor 1 def "));
	}
    }
    FPRINTF((file,"\n%%--end use color %d ",ScilabGCPos.CurColorStatus));
}


static int xget_usecolor()
{
  return ScilabGCPos.CurColorStatus;
}

/* private pixmap for double buffering 
 */


static void xset_pixmapOn(int num) { }

static int xget_pixmapOn() {  return ScilabGCPos.CurPixmapStatus;}

static void xset_wresize(int num) { }

static int xget_wresize()
{
  return ScilabGCPos.CurResizeStatus;
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

static void xset_colormap(int m,int n, double *a)
{
  int i;
  if (n  != 3 ||  m < 0) {
    Scistring("Colormap must be a m x 3 array \n");
    return;
  }
  /* Checking RGB values */
  for (i = 0; i < m; i++) {
    if (a[i] < 0 || a[i] > 1 || a[i+m] < 0 || a[i+m] > 1 ||
	a[i+2*m] < 0 || a[i+2*m]> 1) {
      Scistring("RGB values must be between 0 and 1\n");
      return;
    }
  }
  ScilabGCPos.Numcolors = m;
  ScilabGCPos.IDLastPattern = m - 1;
  ScilabGCPos.NumForeground = m;
  ScilabGCPos.NumBackground = m + 1;
  WriteColorRGB("R",a,0);
  WriteColorRGB("G",a,1);
  WriteColorRGB("B",a,2);
  xset_usecolor(1);
  xset_alufunction1(3);
  xset_pattern(ScilabGCPos.NumForeground+1);
  xset_foreground(ScilabGCPos.NumForeground+1);
  xset_background(ScilabGCPos.NumForeground+2);
}

/* getting the colormap XXXX */

static void xget_colormap( int *num,  double *val)
{
  *num=0 ; /* XXX */
}

static void WriteColorRGB(char *str, double *tab, int ind)
{
  int i;
  FPRINTF((file,"\n/Color%s [",str));
  for ( i=0; i < ScilabGCPos.Numcolors; i++)
    {
      FPRINTF((file,"%f ",(float) tab[3*i+ind]));
      if ( (i % 10 ) == 0 ) FPRINTF((file,"\n"));
    }
  FPRINTF((file,"0.0 1.0] def"));
}

/** 
  Initial colormap : The arrays were filled with the numbers that we get with xget("colormap") 
**/

void ColorInit(void)
{
  int   m = DEFAULTNUMCOLORS;
  ScilabGCPos.Numcolors = m;
  WriteColorRGBDef("R",default_colors,0);
  WriteColorRGBDef("G",default_colors,1);
  WriteColorRGBDef("B",default_colors,2);
}

static void WriteColorRGBDef(char *str, short unsigned int *tab, int ind)
{
  int i;
  FPRINTF((file,"\n/Color%s [",str));
  for ( i=0; i < ScilabGCPos.Numcolors; i++)
    {
      FPRINTF((file,"%f ",(float) tab[3*i+ind]/255.0));
      if ( (i % 10 ) == 0 ) FPRINTF((file,"\n"));
    }
  FPRINTF((file,"0.0 1.0] def"));
}

static void set_c_Pos(int i)
{
  int j;
  j=Max(Min(i,ScilabGCPos.Numcolors+1),0);
  FPRINTF((file,"\n%d Setcolor",(int)j));
}


/** set and get the number of the background or foreground */

static void xset_background(int num)
{ 
  if (ScilabGCPos.CurColorStatus == 1) 
    {
      ScilabGCPos.NumBackground = Max(0,Min(num - 1,ScilabGCPos.Numcolors + 1));
      FPRINTF((file,"\n/background %d def PaintBackground",ScilabGCPos.NumBackground));
    }
}

static int xget_background(void)
{ 
  if ( ScilabGCPos.CurColorStatus == 1 ) 
    {
      return  ScilabGCPos.NumBackground + 1;
    }
  else 
    {
      return 1;
    }
}


/** set and get the number of the background or foreground */

static void xset_foreground(int num)
{ 
  if (ScilabGCPos.CurColorStatus == 1) 
    {
      ScilabGCPos.NumForeground = Max(0,Min(num - 1,ScilabGCPos.Numcolors + 1));
    }
}

static int xget_foreground()
{ 
  if ( ScilabGCPos.CurColorStatus == 1 ) 
    {
      return  ScilabGCPos.NumForeground + 1;
    }
  else 
    {
      return  1; /** the foreground is a solid line style in b&w */
    }
}



/** set and get the number of the hidden3d color */

static void xset_hidden3d(int num)
{ 
  if (ScilabGCPos.CurColorStatus == 1) 
    {
      /* es: Max(0,... -> Max(-1,... */
      ScilabGCPos.NumHidden3d = Max(-1,Min(num - 1,ScilabGCPos.Numcolors + 1));
    }
}

static int xget_hidden3d()
{ 
  if ( ScilabGCPos.CurColorStatus == 1 ) 
    {
      return ScilabGCPos.NumHidden3d + 1;
    }
  else 
    {
      return 1; /** the hidden3d is a solid line style in b&w */
    }
}


static void xset_autoclear(int num)
{ 
  ScilabGCPos.Autoclear = Max(0,Min(1,num));
}

static void xset_autoclear_def() 
{
  ScilabGCPos.Autoclear = 0;
}

static int xget_autoclear(void)
{ 
  return  ScilabGCPos.Autoclear;
}

static char *xget_fpf(void)
{
  return( ScilabGCPos.fp_format);
}

static void xset_fpf(char *fmt) 
{
  strncpy(ScilabGCPos.fp_format,fmt,32);
}

static void xset_fpf_def() 
{
  ScilabGCPos.fp_format[0]='\0';
}


static void xset_pixmapclear(void)
{
}

static void xset_show(void)
{
  FPRINTF((file,"\n%% SPLIT HERE"));
}

static void pixmap_resize(void)
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

static void displaystring(char *string, int x, int y, int flag, double angle)
{     
  int i,rect[4] ;
  int yn = (int) (y + ascentPos());
  boundingbox(string,x,yn,rect);
  FPRINTF((file,"\n("));
  for ( i=0; i < (int)strlen(string);i++)
    {
      if (string[i]== '(' || string[i] == ')' )
	FPRINTF((file,"%c%c",'\\',string[i]));
      else 
	FPRINTF((file,"%c",string[i]));
    }
  FPRINTF((file,") %d %d %d %5.2f [%d %d %d %d] Show", x,yn ,flag,angle,rect[0],rect[1],rect[2],rect[3]));
  FPRINTF((file,"\n%%Latex:\\myput{%d}{%d}{%d}{%s}",x,def_height*prec_fact - yn, fontsizePos(), string));
 }


double bsizePos[6][4]= {{ 0.0,-7.0,4.63,9.0  },  /* normalement inutilise ici avec les modifs suivantes */
		{ 0.0,-9.0,5.74,12.0 },          
		{ 0.0,-11.0,6.74,14.0},
		{ 0.0,-12.0,7.79,15.0},
		{0.0, -15.0,9.72,19.0 },
		{0.0,-20.0,13.41,26.0}};


/*** ajouts q&d en attendant mieux.... Bruno (le 24 Nov 2002) ***/

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


static void PosStrBox(char *str, int id_font, int id_size, 
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

void boundingbox(char *string, int x, int y, int rect[])
{
  int font[2];
  double h, w;
  xget_font(font);
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
/** modified by bruno **/

static double ascentPos() 
{ 
  int font[2];
  xget_font(font);
  return (PosStrAsc(font[0], font[1]) * prec_fact);
  /* old: return ((bsizePos[font[1]][1] +(bsizePos[font[1]][3]/2.0) ))*((double) prec_fact); */
}

/** Draw a single line in current style **/

static void drawline(int *xx1, int *yy1, int *x2, int *y2)
{
    FPRINTF((file,"\n %d %d %d %d L",(int)*xx1,(int)*yy1,(int)*x2,(int)*y2));
  }

/** Draw a set of segments **/
/** segments are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1]) **/
/** for i=0 step 2 **/

static void drawsegments(int *vx, int *vy, int n, int *style, int iflag)
{
  int dash,color,i;
  xget_dash_and_color(&dash,&color);
  /* store the current values */
  if ( iflag == 0 )
    {
      /** all segments have the same color or dash style */
      int lstyle = (*style < 1) ? color : *style; 
      WriteGeneric("drawsegs",(int)1L,n*2,vx,vy,n,(int)1L,&lstyle); 
    }
  else
    {
      for ( i=0 ; i < n/2 ; i++) 
	{
	  int NDvalue =  style[i];
	  WriteGeneric("drawsegs",(int)1L,(int)4L,&vx[2*i],&vy[2*i],(int)2L,(int)1L,&NDvalue);
	}
    }
  xset_dash_and_color(dash,color);
}

/** Draw a set of arrows **/

static void drawarrows(int *vx, int *vy, int n, int as, int *style, int iflag)
{
  int dash,color;
  xget_dash_and_color(&dash,&color);
  /* store the current values */
  if ( iflag == 0 )
    {
      /** all the arrows have the same color **/
      int color1 = (*style < 1) ?  color : *style;
      xset_line_style(color1);
      WriteGeneric("drawarrows",(int)1L,n*2,vx,vy,n,(int)1L,&as); 
    }
  else
    {
      int i;
      for ( i=0 ; i < n/2 ; i++) 
	{
	  xset_line_style(style[i]);
	  /*setpattern(&NDvalue,PI0,PI0,PI0); commented out ss 13/09/00 */
	  WriteGeneric("drawarrows",(int)1L,(int)4L,&vx[2*i],&vy[2*i],(int)2L,(int)1L,&as);
	}
    }
  xset_dash_and_color(dash,color);
}


/** Draw or fill a set of rectangle **/
/** rectangles are defined by (vect[i],vect[i+1],vect[i+2],vect[i+3]) **/
/** for i=0 step 4 **/
/** (*n) : number of rectangles **/
/** fillvect[*n] : specify the action (see periX11.c) **/

static void drawrectangles(const int *vects,const int *fillvect, int n)
{
  int cpat =  xget_pattern();
  WriteGeneric("drawbox",n,(int)4L,vects,vects,4*n,(int)0L,fillvect);
  xset_pattern(cpat);
}

/** Draw one rectangle **/


static void drawrectangle(const int rect[])
{
  int fvect= 0;
  drawrectangles(rect,&fvect,1);
}

/** Draw a filled rectangle **/

static void fillrectangle(const int rect[])
{ 
  int cpat = xget_pattern();
  drawrectangles(rect,&cpat,1);
}


/*----------------------------------------------------------------------------------
 * accelerated draw a set of rectangles, not implemented for Pos 
 *----------------------------------------------------------------------------------*/

static void fill_grid_rectangles(int *x, int *y, double *z, int n1, int n2)
{
}

/*----------------------------------------------------------------------------------
 * accelerated draw a set of rectangles, not implemented for Pos 
 *----------------------------------------------------------------------------------*/

static void fill_grid_rectangles1(int *x, int *y, double *z, int n1, int n2)
{
}

/** Draw or fill a set of ellipsis or part of ellipsis **/
/** Each is defined by 6-parameters, **/
/** fillvect[*n] : specify the action <?> **/
/** caution angle=degreangle*64          **/

static void fillarcs( int *vects, int *fillvect, int n)
{
  int cpat =  xget_pattern();
  WriteGeneric("fillarc",n,(int)6L,vects,vects,6*n,(int)0L,fillvect);
  xset_pattern(cpat);
}

/** Draw a set of ellipsis or part of ellipsis **/
/** Each is defined by 6-parameters, **/
/** ellipsis i is specified by $vect[6*i+k]_{k=0,5}= x,y,width,height,angle1,angle2$ **/
/** <x,y,width,height> is the bounding box **/
/** angle1,angle2 specifies the portion of the ellipsis **/
/** caution : angle=degreangle*64          **/

static void drawarcs( int *vects, int *style, int n)
{
  int i,dash,color;
  /* store the current values */
  xget_dash_and_color(&dash,&color);
  for ( i=0 ; i < n ; i++) 
    {
      int fvect,na=1;
      /** to fix the style */
      xset_line_style(style[i]);

      /** to say that we don't want to fill the arcs */
      fvect = ScilabGCPos.IDLastPattern  +2;
      fillarcs(&vects[(6)*i],&fvect,na);
    }
  xset_dash_and_color(dash,color);
}


/** Draw a single ellipsis or part of it **/
/** caution angle=degreAngle*64          **/

static void drawarc(int arc[])
{ 
  int fvect;
  /** fvect set to tell that we only want to draw not to fill  */
  fvect = ScilabGCPos.IDLastPattern + 2  ;
  fillarcs(arc,&fvect,1);
}

/** Fill a single elipsis or part of it **/
/** with current pattern **/

static void fillarc( int arc[])
{ 
  int cpat= xget_pattern();
  fillarcs(arc,&cpat,1);
 }

/*--------------------------------------------------------------
\encadre{Filling or Drawing Polylines and Polygons}
---------------------------------------------------------------*/

/** Draw a set of *n polylines (each of which have (*p) points) **/
/** with lines or marks **/
/** drawvect[i] >= use a mark for polyline i **/
/** drawvect[i] < 0 use a line style for polyline i **/

static void drawpolylines( int *vectsx, int *vectsy, int *drawvect, int n, int p)
{
  int symb[2],i,dash,color;
  /* get the current values */
  xget_mark(symb);
  xget_dash_and_color(&dash,&color);
  for (i=0 ; i< n ; i++)
    {
      if (drawvect[i] <= 0)
	{ /** on utilise la marque de numero associ\'ee **/
	  xset_mark(- drawvect[i],symb[1]);
	  drawpolymark(vectsx+(p)*i,vectsy+(p)*i,p);
	}
      else
	{/** on utilise un style pointill\'e  **/
	  xset_line_style(drawvect[i]);
	  drawpolyline(vectsx+(p)*i,vectsy+(p)*i,p,0);
	}
    }
  /** back to default values **/
  xset_dash_and_color(dash,color);
  xset_mark(symb[0],symb[1]);
}

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

static void fillpolylines( int *vectsx, int *vectsy, int *fillvect, int n, int p)
{
  int cpat;
  if ( ScilabGCPos.CurVectorStyle !=  CoordModeOrigin)
    FPRINTF((file,"\n/absolu false def"));
  cpat=xget_pattern();
  WriteGeneric("drawpoly",n,(p)*2,vectsx,vectsy,(p)*(n),(int)1L,fillvect);
  xset_pattern(cpat);
  FPRINTF((file,"\n/absolu true def"));
}

/** Only draw one polygon with current line style **/
/** according to *closeflag : it's a polyline or a polygon **/

static void drawpolyline(  int *vx, int *vy, int n,int closeflag)
{ 
  int fvect=0;
  if (closeflag == 1 )
    FPRINTF((file,"\n/closeflag true def"));
  else 
    FPRINTF((file,"\n/closeflag false def"));
  fillpolylines(vx,vy,&fvect,1,n);
}

/** Fill the polygon **/

static void fillpolyline(  int *vx, int *vy,int n, int closeflag)
{
  int cpat = xget_pattern();
  /** just fill  ==> cpat < 0 **/
  cpat = -cpat;
  fillpolylines(vx,vy,&cpat,1,n);
}

/** Draw a set of  current mark centred at points defined **/
/** by vx and vy (vx[i],vy[i]) **/

static void drawpolymark( int *vx, int *vy,int n)
{ 
  int keepid,keepsize,i=1,sz=ScilabGCPos.CurHardSymbSize;
  keepid =  ScilabGCPos.fontId;
  keepsize= ScilabGCPos.fontSize;
  xset_font(i,sz);
  displaysymbols(vx,vy,n);
  xset_font(keepid,keepsize);
}

/*-----------------------------------------------------
\encadre{Routine for initialisation}
------------------------------------------------------*/

static void initgraphic(char *string,int *num)
{ 
  char string1[256];
  static int EntryCounter = 0;
  int fnum;

  if (EntryCounter >= 1) xendgraphic();
  strncpy(string1,string,256);

  /* Not so useful   
     sprintf(string2,"%d",(int)EntryCounter);
     strcat(string1,string2); */
  file=fopen(string1,"w");
  if (file == 0) 
    {
      sciprint("Can't open file %s, I'll use stdout\r\n",string1);
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
  FileInit();
  ScilabGCPos.CurWindow =EntryCounter;
  EntryCounter =EntryCounter +1;
}

void FileInit(void)
{
  int m;
  /** Just send Postscript commands to define scales etc....**/
  int x[2];
  xget_windowdim(x,x+1);
  ColorInit();
  FPRINTF((file,"\n%% Dessin en bas a gauche de taille %d,%d",(int)x[0]/2,(int)x[1]/2));
  FPRINTF((file,"\n[0.5 %d div 0 0 0.5 %d div neg  0 %d %d div] concat",
	  (int)prec_fact, (int)prec_fact,(int)x[1]/2,(int) prec_fact ));
  FPRINTF((file,"\n%% Init driver "));
  FPRINTF((file,"\n/PaintBackground {WhiteLev 2 add background eq {}{ (drawbox) 4 [background 1 add] [0 0 %d %d] dogrey}ifelse } def", x[0],x[1]));

  InitScilabGCPos();
  FPRINTF((file,"\n%% End init driver "));
  FPRINTF((file,"\n/WhiteLev %d def",ScilabGCPos.IDLastPattern));
  /** If the X window exists we check its colormap **/
  if (  CheckColormap(&m) == 1) 
    { 
      int i;
      float r,g,b;
      ScilabGCPos.Numcolors = m;
      ScilabGCPos.NumForeground = m;
      ScilabGCPos.NumBackground = m + 1;
      if (ScilabGCPos.CurColorStatus == 1) 
	{
	  ScilabGCPos.IDLastPattern = ScilabGCPos.Numcolors - 1;
	  FPRINTF((file,"\n/WhiteLev %d def",ScilabGCPos.IDLastPattern));
	}
      FPRINTF((file,"\n/ColorR ["));
      for ( i=0; i < m ; i++)
	{
	  get_r(i,&r);
	  FPRINTF((file,"%f ",r));
	  if ( (i % 10 ) == 0 ) FPRINTF((file,"\n"));
	}
      FPRINTF((file,"0.0 1.0 ] def"));
      FPRINTF((file,"\n/ColorG ["));
      for ( i=0; i < m ; i++) 
	{
	  get_g(i,&g);
	  FPRINTF((file,"%f ",g));
	  if ( (i % 10 ) == 0 ) FPRINTF((file,"\n"));
	}
      FPRINTF((file,"0.0 1.0] def"));
      FPRINTF((file,"\n/ColorB ["));
      for ( i=0; i < m; i++)
	{
	  get_b(i,&b);
	  FPRINTF((file,"%f ",b));
	  if ( (i % 10 ) == 0 ) FPRINTF((file,"\n"));
	}
      FPRINTF((file,"0.0 1.0] def"));
    }
  FPRINTF((file,"\n%%Latex:\\begin{picture}(%d,%d)(0,0)",
	   def_width*prec_fact,
	   def_height*prec_fact));
}

/*---------------------------------------------------------------------------
 * writes a message in the info widget associated to the current scilab window 
 *----------------------------------------------------------------------------*/

static void xinfo(char *format,...) {}


/*--------------------------------------------------------
\encadre{Initialisation of the graphic context. Used also 
to come back to the default graphic state}
---------------------------------------------------------*/

static void xset_default(void)
{
  InitScilabGCPos();
}

static void InitScilabGCPos(void)
{ 
  int i,j,col;
  ScilabGCPos.IDLastPattern = GREYNUMBER-1;
  ScilabGCPos.CurLineWidth=0 ;
  i=1;
  xset_thickness(1);
  xset_alufunction("GXcopy");
  /** retirer le clipping **/
  i=j= -1;
  xset_unclip();
  xset_dash(0);
  xset_font(2,1);
  xset_mark(0,0);
  /** trac\'e absolu **/
  ScilabGCPos.CurVectorStyle = CoordModeOrigin ;
  /* initialisation des pattern dash par defaut en n&b */
  ScilabGCPos.CurColorStatus =0;
  xset_pattern(1);
  xset_dash(1);
  xset_hidden3d(1);
  /* initialisation de la couleur par defaut */ 
  ScilabGCPos.Numcolors = DEFAULTNUMCOLORS;
  ScilabGCPos.NumForeground = DEFAULTNUMCOLORS;
  ScilabGCPos.CurColorStatus = 1 ;
  xset_pattern(1);
  xset_foreground(ScilabGCPos.NumForeground+1);
  xset_background(ScilabGCPos.NumForeground+2);
  xset_hidden3d(4);
  /* Choix du mode par defaut (decide dans initgraphic_ */
  getcolordef(&col);
  /** we force CurColorStatus to the opposite value of col 
   * to force usecolorPos to perform initialisations 
   **/
  ScilabGCPos.CurColorStatus = (col == 1) ? 0: 1;
  xset_usecolor(col);
  if (col == 1) ScilabGCPos.IDLastPattern = ScilabGCPos.Numcolors - 1;
  strcpy(ScilabGCPos.CurNumberDispFormat,"%-5.2g");
}


/*-----------------------------------------------------
\encadre{Draw an axis whith a slope of alpha degree (clockwise)
 . Along the axis marks are set in the direction ( alpha + pi/2), in the 
  following way :
\begin{itemize}
\item   $n=<n1,n2>$,
\begin{verbatim}
     |            |           |
     |----|---|---|---|---|---|
     <-----n1---->                 
     <-------------n2-------->
\end{verbatim}
$n1$and $n2$ are int numbers for interval numbers.
\item $size=<dl,r,coeff>$. $dl$ distance in points between 
     two marks, $r$ size in points of small mark, $r*coeff$ 
     size in points of big marks. (they are doubleing points numbers)
\item $init$. Initial point $<x,y>$. 
\end{itemize}
}

-------------------------------------------------------------*/

static void drawaxis( int alpha, int *nsteps, int *initpoint, double *size)
{
  FPRINTF((file,"\n %d [%d %d] [%f %f %f] [%d %d] drawaxis",
	   alpha,nsteps[0],nsteps[1],size[0],size[1],size[2],
	   initpoint[0],initpoint[1]));
}


/*-----------------------------------------------------
\encadre{Display numbers z[i] at location (x[i],y[i])
  with a slope alpha[i] (see displaystring_), if flag==1
  add a box around the string.
-----------------------------------------------------*/

static void displaynumbers(int *x, int *y, int n, int flag, double *z, double *alpha)
{ int i ;
  char buf[20];
  for (i=0 ; i < n ; i++)
    { 
      sprintf(buf,ScilabGCPos.CurNumberDispFormat,z[i]);
      displaystring(buf,x[i],y[i],flag,(alpha[i]));
    }
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

static void WriteGeneric(char *string, int nobj, int sizeobj,const  int *vx, 
			 const int *vy, int sizev, int flag,const  int *fvect)
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

static void WriteGeneric1(char *string, int nobjpos, int objbeg, int sizeobj, const int *vx, 
			  const int *vy, int flag,const  int *fvect)
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


void Write2Vect(const int *vx,const int *vy, int from, int n, char *string, int flag, int fv)
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

static int fontsizePos (void)
{ 
  return isizePos[ScilabGCPos.fontSize];
}
	 


static void xset_font(int fontid, int fontsize)
{ 
  int i,fsiz;
  i = Min(FONTNUMBER-1,Max(fontid,0));
  fsiz = Min(FONTMAXSIZE-1,Max(fontsize,0));
  if ( FontInfoTabPos[i].ok !=1 )
    Scistring("\n Sorry This Font is Not available ");
  else 
   {
     ScilabGCPos.fontId = i;
     ScilabGCPos.fontSize = fsiz;
     FPRINTF((file,"\n/%s findfont %d scf mul scalefont setfont",
	     FontInfoTabPos[i].fname,
	     (int) (isizePos[fsiz]*prec_fact)));
   }
}

/** To get the values id and size of the current font **/

static void xget_font(int *font)
{
  font[0]= ScilabGCPos.fontId ;
  font[1] =ScilabGCPos.fontSize ;
}


/** To set the current mark : using the symbol font of adobe **/

static void xset_mark(int number, int size)
{ 
  ScilabGCPos.CurHardSymb =  Max(Min(SYMBOLNUMBER-1,number),0);
  ScilabGCPos.CurHardSymbSize =  Max(Min(FONTMAXSIZE-1,size),0);
;}

/** To get the current mark id **/

static void xget_mark(int *symb)
{
  symb[0] = ScilabGCPos.CurHardSymb ;
  symb[1] = ScilabGCPos.CurHardSymbSize ;
}

char symb_listPos[] = {
  /*
     0x2e : . alors que 0xb7 est un o plein trop gros 
     ., +,X,*,diamond(filled),diamond,triangle up,triangle down,trefle,circle*/
  (char)0x2e,(char)0x2b,(char)0xb4,(char)0xc5,(char)0xa8,
  (char)0xe0,(char)0x44,(char)0xd1,(char)0xa7,(char)0x4f};

static void displaysymbols(int *vx, int *vy,int n)
{
  int fvect=  ( ScilabGCPos.CurColorStatus ==1) ? ScilabGCPos.CurColor : ScilabGCPos.CurPattern ;
  if ( ScilabGCPos.CurVectorStyle !=  CoordModeOrigin)
    FPRINTF((file,"\n/absolu false def"));
  FPRINTF((file,"\nHardMark 0 16#%x put",
      Char2Int( symb_listPos[ScilabGCPos.CurHardSymb])));
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
	  sciprint("\n unknown font family : %s \r\n",name);
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

/*------------------------END--------------------*/
