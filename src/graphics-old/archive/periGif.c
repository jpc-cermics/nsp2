/*------------------------------------------------------------------------

SCILAB GIF Output based on GD Library from: http://www.boutell.com/gd
Modelled after the original PostScript Driver in periPos.c

Copyright (C) 1999, Tom Leitner, tom@finwds01.tu-graz.ac.at
http://wiis.tu-graz.ac.at/people/tom.html

NOTE: This needs the GD Library installed in the "gd"
subdirectory of this directory.
    
WARNING: The following things are not implemented yet:

- Thick lines.

Usage of the driver:

driver ('GIF');
xinit ('test.gif');
xset('wdim', 800, 600);
......  make your drawings ....
xend();

will produce a 800 by 600 GIF file called test.gif. If you omit the
xset(wdim,...) call, the default image size is 640 by 480.
    
--------------------------------------------------------------------------*/

/*----------------------BEGIN----------------------
  \def\encadre#1{\paragraph{}\fbox{\begin{minipage}[t]{15cm}#1 \end{minipage}}}
  \section{A GIF Driver}
  ---------------------------------------------------*/

#include <stdio.h>
#include <math.h>
#include <string.h>

#ifdef __STDC__
#include <stdlib.h>
#else
#include <malloc.h>
extern  char  *getenv();
#endif

#define CoordModePrevious 1
#define CoordModeOrigin 0
#define GXclear 0
#define GXand 1
#define GXandReverse 2
#define GXcopy 3
#define GXandInverted 4
#define GXnoop 5
#define GXxor 6
#define GXor 7
#define GXnor 8
#define GXequiv 9
#define GXinvert 10
#define GXorReverse 11
#define GXcopyInverted 12
#define GXorInverted 13
#define GXnand 14
#define GXset 15


#define Char2Int(x)   ( x & 0x000000ff )
#if defined(__CYGWIN32__) || defined(__MINGW32__) || defined(__GNUC__) || defined(__MSC__)
static FILE *file= (FILE *) 0;
#define FPRINTF(x) ( file != (FILE*) 0) ?  fprintf x  : 0 
#else 
#define FPRINTF(x) fprintf x  
static FILE *file= stdout ;
#endif

#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"
#include "nsp/graphics/periGif.h"
#include "nsp/graphics/color.h"
#include "../gd/gd.h"

void C2F(WriteGeneric1Gif)();
void C2F(xgetmarkGif)(int *verbose, int *symb, int *narg, double *dummy);
void C2F(xsetmarkGif)(int *number, int *size, int *v3, int *v4);
void C2F(xgetfontGif)(int *verbose, int *font, int *nargs, double *dummy);
void C2F(xsetfontGif)(int *fontid, int *fontsize, int *v3, int *v4);
void C2F(setdashstyleGif)(int *value, int *xx, int *n);
void C2F(Write2VectGif)();
void C2F(WriteGenericGif)(char *string, int nobj, int sizeobj, int *vx, int *vy, int sizev, int flag, int *fvect);
void C2F(InitScilabGCGif)(int *v1, int *v2, int *v3, int *v4);
void C2F(setforegroundGif)(int *num, int *v2, int *v3, int *v4);
void C2F(ScilabGCGetorSetGif)(char *str, int flag, int *verbose, int *x1, int *x2, int *x3, int *x4, int *x5, int *x6, double *dx1);
void C2F(setbackgroundGif)(int *num, int *v2, int *v3, int *v4);
void C2F(set_cGif)(int i);
void C2F(idfromnameGif) (char *name1, int *num);
void C2F(getdashGif)(int *verbose, int *value, int *narg, double *dummy);




/* static int C2F(GifQueryFont)(); */
static void C2F(displaysymbolsGif)(char *str, int *n, int *vx, int *vy);
extern int ReadbdfFont();
extern void C2F(nues1)();
extern int CheckScilabXgc();


static double *vdouble = 0; /* used when a double argument is needed */

static gdImagePtr GifIm = (gdImagePtr)0;
static gdFontPtr  GifFont;
static int GifDashes[50], nGifDashes;
static int col_white;
static void FileInitGif  (void);
static int GifLineColor (void);
static int GifPatternColor (int pat);
static void LoadFontsGif(void);

/** Structure to keep the graphic state  **/

struct BCG 
{ 
  int FontSize;
  int FontId;
  int CurWindowWidth;
  int CurWindowHeight;
  int CurHardSymb;
  int CurHardSymbSize;
  int CurLineWidth;
  int CurPattern;
  int CurColor;
  int CurWindow;
  int CurVectorStyle;
  int CurDrawFunction;
  int ClipRegionSet;
  int CurClipRegion[4];
  int CurDashStyle;
  char CurNumberDispFormat[20];
  int CurColorStatus;
  int IDLastPattern; /* number of last patter or color */
  int Numcolors; /* number of colors */
  int NumBackground;  /* number of Background in the color table */
  int NumForeground; /* number of Foreground in the color table */
  int NumHidden3d;  /* color for hidden 3d facets **/
}  ScilabGCGif ;

static int col_index[gdMaxColors];
static int fillpolylines_closeflag = 0;

/*-----------------------------------------------------
  \encadre{General routines}
  -----------------------------------------------------*/

/** To select the graphic Window  **/

void C2F(xselgraphicGif)(char *v1, int *v2, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{
  if (file != stdout && file != 0 ) {
    fclose(file);
    file=stdout;
  }
  if (GifIm != (gdImagePtr)0) {
    gdImageDestroy(GifIm);
    GifIm = (gdImagePtr)0;
  }
}

/** End of graphic (close the file) **/

void GetDriverName(char *DriverName)
{    int *v2;
 int *v3;
 int *v4;
 int *v5;
 int *v6;
 int *v7;
 double *dv1;
 double *dv2;
 double *dv3;
 double *dv4;
 nsp_gengine->scale->get_driver_name(DriverName, v2, v3, v4, v5, v6, v7, dv1, dv2, dv3, dv4);
}

void C2F(xendgraphicGif)(void)
{
  char DriverName[10];
  int num;
  if (file != stdout && file != (FILE*) 0 && GifIm != (gdImagePtr)0) {
    num = ScilabGCGif.NumBackground;
    gdImageChangeColor(GifIm,col_white,col_index[num]);
    gdSetBackground(GifIm,col_index[num] );
    GetDriverName(DriverName);
    if (strcmp(DriverName,"GIF")==0)
      gdImageGif(GifIm, file);
    else if (strcmp(DriverName,"PPM")==0)
      gdImagePPM(GifIm, file);
    fclose(file);
    gdImageDestroy(GifIm);
    file=stdout;
  }
}

void C2F(xendGif)(char *v1, int *v2, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{
  C2F(xendgraphicGif)();
}


/** Clear the current graphic window     **/
/** In GIF : nothing      **/

void C2F(clearwindowGif)(char *v1, int *v2, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{
  /* FPRINTF((file,"\n showpage")); */
  /** Sending the scale etc.. in case we want an other plot **/
  /* FileInitGif(file); */
}

/** To generate a pause : Empty here **/

void C2F(xpauseGif)(char *str, int *sec_time, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{}

/** Wait for mouse click in graphic window : Empty here **/

void C2F(xclickGif)(char *str, int *ibutton, int *xx1, int *yy1, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{ }

void C2F(xclick_anyGif)(char *str, int *ibutton, int *xx1, int *yy1, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{ }

void C2F(xgetmouseGif)(char *str, int *ibutton, int *xx1, int *yy1, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{ }

/** Clear a rectangle **/

void C2F(clearareaGif)(char *str, int *x, int *y, int *w, int *h, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{
  /*  FPRINTF((file,"\n [ %d %d %d %d ] clearzone",(int)*x,(int)*y,(int)*w,(int)*h));*/
}



/*---------------------------------------------------------------------
  \section{Function for graphic context modification}
  ------------------------------------------------------------------------*/

/** to get the window upper-left point coordinates **/

void C2F(getwindowposGif)(int *verbose, int *x, int *narg, double *dummy)
{
  *narg = 2;
  x[0]= x[1]=0;
  if (*verbose == 1) 
    sciprint("\n CWindow position :%d,%d\r\n",(int)x[0],(int)x[1]);
}

/** to set the window upper-left point position (Void) **/

void C2F(setwindowposGif)(int *x, int *y, int *v3, int *v4)
{
}

/** To get the window size **/

void C2F(getwindowdimGif)(int *verbose, int *x, int *narg, double *dummy)
{     
  *narg = 2;
  x[0]= ScilabGCGif.CurWindowWidth;
  x[1]= ScilabGCGif.CurWindowHeight;
  if (*verbose == 1) 
    sciprint("\n CWindow dim :%d,%d\r\n",(int)x[0],(int)x[1]);
} 

/** To change the window dimensions */

void C2F(setwindowdimGif)(int *x, int *y, int *v3, int *v4)
{

  gdImagePtr GifImOld = GifIm;

  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  GifIm = gdImageCreate(*x, *y);

  /*  ScilabGCGif.CurWindowWidth  = *x;
      ScilabGCGif.CurWindowHeight = *y; */

  FileInitGif();
  gdImageFilledRectangle(GifIm, 0, 0, (*x) - 1, (*y) - 1, col_white);

  gdImageCopyResized(GifIm, GifImOld, 0, 0, 0, 0, *x, *y, 
		     ScilabGCGif.CurWindowWidth,
		     ScilabGCGif.CurWindowHeight);
  gdImageDestroy(GifImOld);
  ScilabGCGif.CurWindowWidth  = *x;
  ScilabGCGif.CurWindowHeight = *y;
}

/** Select a graphic Window : Empty for GIF **/

void C2F(setcurwinGif)(int *intnum, int *v2, int *v3, int *v4)
{
  ScilabGCGif.CurWindow = *intnum;
}

/** Get the id number of the Current Graphic Window **/

void C2F(getcurwinGif)(int *verbose, int *intnum, int *narg, double *dummy)
{
  *narg =1 ;
  *intnum = ScilabGCGif.CurWindow ;
  if (*verbose == 1) 
    Scistring("\nJust one graphic page at a time ");
}

/** Set a clip zone (rectangle ) **/

void C2F(setclipGif)(int *x, int *y, int *w, int *h)
{
  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  ScilabGCGif.ClipRegionSet = 1;
  ScilabGCGif.CurClipRegion[0]= *x;
  ScilabGCGif.CurClipRegion[1]= *y;
  ScilabGCGif.CurClipRegion[2]= *w;
  ScilabGCGif.CurClipRegion[3]= *h;
  gdSetClipping(GifIm,*x,*y,*x+*w,*y+*h); 
  /*  FPRINTF((file,"\n%d %d %d %d setclipzone",(int)*x,(int)*y,(int)*w,(int)*h));*/
}

/** unset clip zone **/

void C2F(unsetclipGif)(int *v1, int *v2, int *v3, int *v4)
{
  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  ScilabGCGif.ClipRegionSet = 0;
  ScilabGCGif.CurClipRegion[0]= -1;
  ScilabGCGif.CurClipRegion[1]= -1;
  ScilabGCGif.CurClipRegion[2]= 200000;
  ScilabGCGif.CurClipRegion[3]= 200000;
  gdUnsetClipping(GifIm);
  /*  FPRINTF((file,"\n%d %d %d %d setclipzone",-1,-1,200000,200000));*/
}

/** Get the boundaries of the current clip zone **/

void C2F(getclipGif)(int *verbose, int *x, int *narg, double *dummy)
{
  x[0] = ScilabGCGif.ClipRegionSet;
  if ( x[0] == 1)
    {
      *narg = 5;
      x[1] =ScilabGCGif.CurClipRegion[0];
      x[2] =ScilabGCGif.CurClipRegion[1];
      x[3] =ScilabGCGif.CurClipRegion[2];
      x[4] =ScilabGCGif.CurClipRegion[3];
    }
  else *narg = 1;
  if (*verbose == 1)
    {
      if (ScilabGCGif.ClipRegionSet == 1)
	sciprint("\nThere's a Clip Region :x:%d,y:%d,w:%d,h:%d\r\n",
		 ScilabGCGif.CurClipRegion[0],
		 ScilabGCGif.CurClipRegion[1],
		 ScilabGCGif.CurClipRegion[2],
		 ScilabGCGif.CurClipRegion[3]);
      else 
	Scistring("\nNo Clip Region");
    }
}

/*----------------------------------------------------------
  \encadre{For the drawing functions dealing with vectors of 
  points, the following routine is used to select the mode 
  absolute or relative }
  Absolute mode if *num==0, relative mode if *num != 0
  ------------------------------------------------------------*/

void C2F(setabsourelGif)(int *num, int *v2, int *v3, int *v4)
{
  if (*num == 0 )
    ScilabGCGif.CurVectorStyle =  CoordModeOrigin;
  else 
    ScilabGCGif.CurVectorStyle =  CoordModePrevious ;
}

/** to get information on absolute or relative mode **/

void C2F(getabsourelGif)(int *verbose, int *num, int *narg, double *dummy)
{
  *narg = 1;
  *num = ScilabGCGif.CurVectorStyle  ;
  if (*verbose == 1) 
    {
      if (ScilabGCGif.CurVectorStyle == CoordModeOrigin)
	Scistring("\nTrace Absolu");
      else 
	Scistring("\nTrace Relatif");
    }
}


void C2F(setalufunctionGif)(char *string)
{    
  int value;
  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  C2F(idfromnameGif)(string,&value);
  if ( value != -1) {
    ScilabGCGif.CurDrawFunction = value;
    gdSetAlu(GifIm, value);
    /*     FPRINTF((file,"\n%% %d setalufunction",(int)value)); */
  }
}

/** All the possibilities : Read The X11 manual to get more informations **/

struct alinfo { 
  char *name;
  char id;
  char *info;} AluStrucGif[] =
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

void C2F(idfromnameGif)(char *name1, int *num)
{int i;
 *num = -1;
 for ( i =0 ; i < 16;i++)
   if (strcmp(AluStrucGif[i].name,name1)== 0) 
     *num=AluStrucGif[i].id;
 if (*num == -1 ) 
   {
     Scistring("\n Use the following keys :");
     for ( i=0 ; i < 16 ; i++)
       sciprint("\nkey %s -> %s\r\n",AluStrucGif[i].name,
		AluStrucGif[i].info);
   }
}


void C2F(setalufunction1Gif)(int *num, int *v2, int *v3, int *v4)
{     
  int value;

  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  value=AluStrucGif[Min(15,Max(0,*num))].id;
  if ( value != -1)
    {
      ScilabGCGif.CurDrawFunction = value;
      gdSetAlu(GifIm, value);
    }
}

/** To get the value of the alufunction **/

void C2F(getalufunctionGif)(int *verbose, int *value, int *narg, double *dummy)
{ 
  *narg =1 ;
  *value = ScilabGCGif.CurDrawFunction ;
  if (*verbose ==1 ) 
    { sciprint("\nThe Alufunction is %s -> <%s>\r\n",
	       AluStrucGif[*value].name,
	       AluStrucGif[*value].info);}
}

int GetAluGif(void)
{
  return ScilabGCGif.CurDrawFunction;
}
/** to set the thickness of lines : 0 is a possible value **/
/** give the thinest line **/

#define Thick_prec 5

void C2F(setthicknessGif)(int *value, int *v2, int *v3, int *v4)
{ 
  ScilabGCGif.CurLineWidth =Max(0, *value);
  /*  FPRINTF((file,"\n%d Thickness",(int)Max(0,*value*Thick_prec))); */
}

/** to get the thicknes value **/

void C2F(getthicknessGif)(int *verbose, int *value, int *narg, double *dummy)
{
  *narg =1 ;
  *value = ScilabGCGif.CurLineWidth ;
  if (*verbose ==1 ) 
    sciprint("\nLine Width:%d\r\n",
	     ScilabGCGif.CurLineWidth ) ;
}
     

/*-------------------------------------------------
  \encadre{To set grey level for filing areas.
  from black (*num =0 ) to white 
  you must use the get function to get the id of 
  the white pattern }
  ----------------------------------------------------*/

void C2F(setpatternGif)(int *num, int *v2, int *v3, int *v4)
{
  int i ; 
  if ( ScilabGCGif.CurColorStatus ==1) 
    {
      i= Max(0,Min(*num-1,ScilabGCGif.Numcolors+1));
      ScilabGCGif.CurColor = i ;
      C2F(set_cGif)(i);
    }
  else 
    {
      /* used when printing from color to b&white color after GREYNUMBER 
	 are translated to black */
      if ( *num-1 > GREYNUMBER -1 ) 
	i=0;
      else 
	i= Max(0,Min(*num-1,GREYNUMBER-1));
      ScilabGCGif.CurPattern = i;
      /*      if (i ==0)
	      FPRINTF((file,"\nfillsolid"));
	      else 
	      FPRINTF((file,"\n%d Setgray",(int)i)); */
    }
}

/** To get the id of the current pattern  **/

void C2F(getpatternGif)(int *verbose, int *num, int *narg, double *dummy)
{ 

  *narg=1;
  if ( ScilabGCGif.CurColorStatus ==1) 
    {
      *num = ScilabGCGif.CurColor +1 ;
      if (*verbose == 1) 
	sciprint("\n Color : %d\r\n",*num);
    }
  else 
    {
      *num = ScilabGCGif.CurPattern +1 ;
      if (*verbose == 1) 
	sciprint("\n Pattern : %d\r\n",*num);
    }
}

/** To get the id of the last pattern **/

void C2F(getlastGif)(int *verbose, int *num, int *narg, double *dummy)
{
  *num = ScilabGCGif.IDLastPattern +1 ;
  if (*verbose==1) 
    sciprint("\n Id of White Pattern %d\r\n",(int)*num);
  *narg=1;
}

/** To set dash-style : **/
/**  use a table of dashes and set default dashes to **/
/**  one of the possible value. value point **/
/**  to a strictly positive int **/

static int DashTabGif[6][4] = {
  {2,5,2,5}, {5,2,5,2},  {5,3,2,3}, {8,3,2,3},
  {11,3,2,3}, {11,3,5,3}};

/* old version of setdashGif retained for compatibility */
void C2F(set_dash_or_color_Gif)(int *value, int *v2, int *v3, int *v4)
{
  static int maxdash = 6, l2=4,l3 ;

  if ( ScilabGCGif.CurColorStatus == 1) 
    {
      int i;
      i= Max(0,Min(*value-1,ScilabGCGif.Numcolors+1));
      ScilabGCGif.CurColor =i;
      C2F(set_cGif)(i);
    }
  else 
    {
      l3 = Max(0,Min(maxdash - 1,*value - 1));
      C2F(setdashstyleGif)(&l3,DashTabGif[l3],&l2);
      ScilabGCGif.CurDashStyle = l3;
    }
}
void C2F(setdashGif)(int *value, int *v2, int *v3, int *v4)
{
  static int maxdash = 6, l2=4,l3 ;
  l3 = Max(0,Min(maxdash - 1,*value - 1));
  C2F(setdashstyleGif)(&l3,DashTabGif[l3],&l2);
  ScilabGCGif.CurDashStyle = l3;
}

static int GifLineColor (void)
{
  int i, c = col_index[ScilabGCGif.CurColor];

  if (GifIm == (gdImagePtr)0 ) {
    sciprint("xinit must be called before any action \r\n");
    return 0;
  }
  if (c < 0) c = 0;
  if (ScilabGCGif.CurDashStyle == 0) return c;
  for (i = 0; i < nGifDashes; i++) {
    if (GifDashes[i] != gdTransparent) GifDashes[i] = c;
  }
  gdImageSetStyle(GifIm, GifDashes, nGifDashes);
  return gdStyled;
}

static int GifPatternColor(int pat)
{
  int c = col_index[pat - 1];
  if (c < 0) c = 0;
  return c;
}

/** To change The Gif-default dash style **/
/** if *value == 0, use a solid line, if *value != 0 **/
/** the dash style is specified by the xx vector of n values **/
/** xx[3]={5,3,7} and *n == 3 means :  5white 3 void 7 white \ldots **/

void C2F(setdashstyleGif)(int *value, int *xx, int *n)
{
  int i, j, cn, c1, c = col_index[ScilabGCGif.CurColor];

  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  if (*value != 0) {
    cn = 0;
    c1 = c;
    for (i = 0; i < *n; i++) {
      for (j = 0; j < xx[i]; j++) {
	GifDashes[cn] = c1;
	cn++;
      }
      if (c1 != gdTransparent) c1 = gdTransparent;
      else                     c1 = c;
    }
    nGifDashes = cn;
    gdImageSetStyle(GifIm, GifDashes, nGifDashes);
  } else {
    nGifDashes = 0;
  }
}
static void C2F(set_dash_and_color_Gif)(int *value, int *v2, int *v3, int *v4)
{
  C2F(setdashGif)(value, v2, v3, v4);
  C2F(setpatternGif)(value+6, v2, v3, v4);
}

void C2F(set_line_style_Gif)(int *value, int *v2, int *v3, int *v4)
{
  int j;
  if (ScilabGCGif.CurColorStatus == 0)
    C2F(setdashGif)(value,PI0,PI0,PI0);
  else {
    j= ScilabGCGif.CurDashStyle + 1;
    C2F(setdashGif)(&j,PI0,PI0,PI0);
    C2F(setpatternGif)(value,PI0,PI0,PI0);
  }
}


/** to get the current dash-style **/
/* old version of getdashGif retained for compatibility */
void C2F(get_dash_or_color_Gif)(int *verbose, int *value, int *narg, double *dummy)
{int i ;
 *narg =1 ;
 if ( ScilabGCGif.CurColorStatus ==1) 
   {
     *value= ScilabGCGif.CurColor + 1;
     if (*verbose == 1) sciprint("Color %d",(int)*value);
     return;
   }
 *value=ScilabGCGif.CurDashStyle+1;
 if ( *value == 1) 
   { if (*verbose == 1) Scistring("\nLine style = Line Solid");}
 else 
   {
     value[1]=4;
     *narg = value[1]+2;
     for ( i =0 ; i < value[1]; i++) value[i+2]=DashTabGif[*value-1][i];
     if (*verbose ==1 ) 
       {
	 sciprint("\nDash Style %d:<",(int)*value);
	 for ( i =0 ; i < value[1]; i++)
	   sciprint("%d ",(int)value[i+2]);
	 Scistring(">\n");
       }
   }
}

void C2F(getdashGif)(int *verbose, int *value, int *narg, double *dummy)
{int i ;
 *narg =1 ;
 *value=ScilabGCGif.CurDashStyle+1;
 if ( *value == 1) 
   { if (*verbose == 1) Scistring("\nLine style = Line Solid");}
 else 
   {
     value[1]=4;
     *narg = value[1]+2;
     for ( i =0 ; i < value[1]; i++) value[i+2]=DashTabGif[*value-1][i];
     if (*verbose ==1 ) 
       {
	 sciprint("\nDash Style %d:<",(int)*value);
	 for ( i =0 ; i < value[1]; i++)
	   sciprint("%d ",(int)value[i+2]);
	 Scistring(">\n");
       }
   }
}
void C2F(get_dash_and_color_Gif)(int *verbose, int *value, int *narg, double *dummy)
{
  /*may be improved replacing 6 by narg */
  C2F(getdashGif)(verbose, value, narg,dummy);
  C2F(getpatternGif)(verbose, value+6, narg,dummy);
  *narg = 6;
}

void C2F(usecolorGif)(int *num, int *v2, int *v3, int *v4)
{
  int i;
  i =  Min(Max(*num,0),1);
  if ( ScilabGCGif.CurColorStatus != (int)i)
    {
      if (ScilabGCGif.CurColorStatus == 1) 
	{
	  /* je passe de Couleur a n&b */
	  /* remise des couleurs a vide */
	  ScilabGCGif.CurColorStatus = 1;
	  C2F(setpatternGif)((i=1,&i),PI0,PI0,PI0);
	  /* passage en n&b */
	  ScilabGCGif.CurColorStatus = 0;
	  i= ScilabGCGif.CurPattern+1;
	  C2F(setpatternGif)(&i,PI0,PI0,PI0);
	  i= ScilabGCGif.CurDashStyle+1;
	  C2F(setdashGif)(&i,PI0,PI0,PI0);
	  ScilabGCGif.IDLastPattern = GREYNUMBER - 1;
	}
      else 
	{
	  /* je passe en couleur */
	  /* remise a zero des patterns et dash */
	  /* remise des couleurs a vide */
	  ScilabGCGif.CurColorStatus = 0;
	  C2F(setpatternGif)((i=1,&i),PI0,PI0,PI0);
	  C2F(setdashGif)((i=1,&i),PI0,PI0,PI0);
	  /* passage en couleur  */
	  ScilabGCGif.CurColorStatus = 1;
	  i= ScilabGCGif.CurColor+1;
	  C2F(setpatternGif)(&i,PI0,PI0,PI0);
	  ScilabGCGif.IDLastPattern = ScilabGCGif.Numcolors - 1;
	}
    }
}


void C2F(getusecolorGif)(int *verbose, int *num, int *narg, double *dummy)
{
  *num = ScilabGCGif.CurColorStatus;
  if (*verbose == 1) 
    sciprint("\n Use color %d\r\n",(int)*num);
  *narg=1;
}


/*******************************************************
 * Setting the colormap 
 * WARNING 
 * -------
 *   This function is only used when the GIF driver is on 
 *   and xset('colormap',..) is used 
 *   (i.e driver('Pos');xset('colormap',....) 
 *   In the usual case (i.e when someone exports a graphic 
 *   which is displayed in a window) only the graphics 
 *   recorded commands are replayed and xset('colormap') belongs 
 *   to the non-recorded Scilab graphic commands 
 *   
 *   Only the <<current colormap>> of the window is translated 
 *   to GIF when the GIF file is opened 
 *   ( see  if (  CheckColormap(&m) == 1) in FileInt) 
 ******************************************************/

void C2F(setcolormapGif)(int *v1, int *v2, int *v3, int *v4, int *v5, int *v6, double *a)
{
  int i,m,r,g,b,c,ierr,m1;
  double *cmap;
  int *ind,i1;

  if (*v2 != 3 ||  *v1 < 0 ) {
    Scistring("Colormap must be a m x 3 array \n");
    return;
  }
  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  m = *v1;
  /* Checking RGB values */
  for (i = 0; i < m; i++) {
    if (a[i] < 0 || a[i] > 1 || a[i+m] < 0 || a[i+m] > 1 ||
	a[i+2*m] < 0 || a[i+2*m]> 1) {
      Scistring("RGB values must be between 0 and 1\n");
      return;
    }
  }

   
  /* deallocate old colors*/
  for ( i=0; i < GifIm->colorsTotal; i++) 
    gdImageColorDeallocate(GifIm, i);
  for ( i=0;i < ScilabGCGif.Numcolors+2; i++) 
    col_index[i] = -1;

  if (m>gdMaxColors-3) {/* reduce the number of colors */
    m1 = gdMaxColors-2;
    if ( (cmap = (double*) malloc(3*m1 * sizeof(double)))== NULL) {
      Scistring("Not enough memory\n");
      return;
    }
    if ( (ind = (int*) malloc(m * sizeof(int)))== NULL) {
      Scistring("Not enough memory\n");
      free(cmap);
      return;
    }

    C2F(nues1)(a,&m,cmap,&m1,ind,&ierr);
    /* create new colormap */
    ScilabGCGif.Numcolors = m;
    for ( i=0; i < ScilabGCGif.Numcolors; i++) {
      i1 = ind[i] - 1;
      r=(int)(cmap[i1] * 255);
      g=(int)(cmap[i1 + m1] * 255);
      b=(int)(cmap[i1 + 2*m1] * 255);
      if (r==255 && g==255 && b==255) {
	/* move white a little to distinguish it from the background */
	r=254;g=254;b=254; }
      c = gdImageColorExact(GifIm, r,g,b);
      if (c == -1)
	c = gdImageColorAllocate(GifIm,r,g,b);
      col_index[i] = c;
    }
    free(ind);
    free(cmap);
  }
  else {
    /* create new colormap */
    ScilabGCGif.Numcolors = m;
    for ( i=0; i < ScilabGCGif.Numcolors; i++) {
      r=(int)(a[i] * 255);
      g=(int)(a[i + m] * 255);
      b=(int)(a[i + 2*m] * 255);
      if (r==255&&g==255&&b==255) {
	/* move white a little to distinguish it from the background */
	r=254;g=254;b=254; }
      c = gdImageColorExact(GifIm, r,g,b);
      if (c == -1)
	c = gdImageColorAllocate(GifIm,r,g,b);
      col_index[i] = c;
    }
  }
  /* adding white and black color at the end */
  c = gdImageColorExact(GifIm, 0,0,0);
  if (c == -1) 
    c = gdImageColorAllocate(GifIm,0,0,0);
  col_index[m]=c;
  c = gdImageColorExact(GifIm, 255,255,255);
  if (c == -1) 
    c = gdImageColorAllocate(GifIm,255,255,255);
  col_index[m+1]=c;
  col_white=c;

  ScilabGCGif.IDLastPattern = m - 1;
  ScilabGCGif.NumForeground = m;
  ScilabGCGif.NumBackground = m + 1;


  C2F(usecolorGif)((i=1,&i) ,PI0,PI0,PI0);
  C2F(setalufunction1Gif)((i=3,&i),PI0,PI0,PI0);
  C2F(setpatternGif)((i=ScilabGCGif.NumForeground+1,&i),PI0,PI0,PI0);  
}

/** 
    Initial (default) colormap 
**/

static void ColorInitGif(void)
{
  int m,i,r,g,b,c;

  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }

  m = DEFAULTNUMCOLORS;
  ScilabGCGif.Numcolors = m;
  for ( i=0; i < ScilabGCGif.Numcolors; i++) {
    r = default_colors[3 * i];
    g = default_colors[3 * i + 1];
    b = default_colors[3 * i + 2];
    if (r==255&&g==255&&b==255) {
      /* move white a little to distinguish it from the background */
      r=254;g=254;b=254; }
    col_index[i] = gdImageColorAllocate(GifIm,r,g,b);
  }
  /* add black and white at the end of the colormap */
  c = gdImageColorExact(GifIm, 0,0,0);
  if (c == -1) 
    c = gdImageColorAllocate(GifIm,0,0,0);
  col_index[m]=c;
  c = gdImageColorExact(GifIm, 255,255,255);
  if (c == -1) 
    c = gdImageColorAllocate(GifIm,255,255,255);
  col_index[m+1]=c;
  col_white=col_index[m+1];
  ScilabGCGif.NumForeground = m;
  ScilabGCGif.NumBackground = m + 1;
  C2F(setpatternGif)((i=ScilabGCGif.NumForeground+1,&i),PI0,PI0,PI0); 
  if (ScilabGCGif.CurColorStatus == 1) 
    {
      ScilabGCGif.IDLastPattern = ScilabGCGif.Numcolors - 1;
    }

}


void C2F(set_cGif)(int i)
{
  int j;
  j=Max(Min(i,ScilabGCGif.Numcolors+1),0);
  /*  FPRINTF((file,"\n%d Setcolorcolormap",(int)j)); */
}


/** set and get the number of the background or foreground */

void C2F(setbackgroundGif)(int *num, int *v2, int *v3, int *v4)
{
  if (ScilabGCGif.CurColorStatus == 1) 
    {
      ScilabGCGif.NumBackground = Max(0,Min(*num - 1,ScilabGCGif.Numcolors + 2));
    }
}

void C2F(getbackgroundGif)(int *verbose, int *num, int *narg, double *dummy)
{ 
  *narg=1;
  if ( ScilabGCGif.CurColorStatus == 1 ) 
    {
      *num = ScilabGCGif.NumBackground + 1;
    }
  else 
    {
      *num = 1;
    }
  if (*verbose == 1) 
    sciprint("\n Background : %d\r\n",*num);
}


/** set and get the number of the background or foreground */

void C2F(setforegroundGif)(int *num, int *v2, int *v3, int *v4)
{
  if (ScilabGCGif.CurColorStatus == 1) 
    {
      ScilabGCGif.NumForeground = Max(0,Min(*num - 1,ScilabGCGif.Numcolors + 1));
    }
}

void C2F(getforegroundGif)(int *verbose, int *num, int *narg, double *dummy)
{ 
  *narg=1;
  if ( ScilabGCGif.CurColorStatus == 1 ) 
    {
      *num = ScilabGCGif.NumForeground + 1;
    }
  else 
    {
      *num = 1; /** the foreground is a solid line style in b&w */
    }
  if (*verbose == 1) 
    sciprint("\n Foreground : %d\r\n",*num);
}



/** set and get the number of the hidden3d color */

void C2F(sethidden3dGif)(int *num, int *v2, int *v3, int *v4)
{ 
  if (ScilabGCGif.CurColorStatus == 1) 
    {
      /* e Segre: Max(0,... -> Max(-1,... */
      ScilabGCGif.NumHidden3d = Max(-1,Min(*num - 1,ScilabGCGif.Numcolors + 1));
    }
}

void C2F(gethidden3dGif)(int *verbose, int *num, int *narg, double *dummy)
{ 
  *narg=1;
  if ( ScilabGCGif.CurColorStatus == 1 ) 
    {
      *num = ScilabGCGif.NumHidden3d + 1;
    }
  else 
    {
      *num = 1; /** the hidden3d is a solid line style in b&w */
    }
  if (*verbose == 1) 
    sciprint("\n Hidden3d : %d\r\n",*num);
}



/*--------------------------------------------------------
  \encadre{general routines accessing the  set<> or get<>
  routines } 
  -------------------------------------------------------*/

void C2F(semptyGif)(int *verbose, int *v2, int *v3, int *v4)
{
  if ( *verbose ==1 ) Scistring("\n No operation ");
}
void C2F(setwwhowGif)(int *verbose, int *v2, int *v3, int *v4)
{
  /*  FPRINTF((file,"\n%% SPLIT HERE")); */
}

void C2F(gemptyGif)(int *verbose, int *v2, int *v3, double *dummy)
{
  if ( *verbose ==1 ) Scistring("\n No operation ");
}

#define NUMSETFONC 28

/** Table in lexicographic order **/

struct bgc { char *name ;
  void  (*setfonc )() ;
  void (*getfonc )() ;}
ScilabGCTabGif[] = {
  {"alufunction",C2F(setalufunction1Gif),C2F(getalufunctionGif)},
  {"background",C2F(setbackgroundGif),C2F(getbackgroundGif)},
  {"clipoff",C2F(unsetclipGif),C2F(getclipGif)},
  {"clipping",C2F(setclipGif),C2F(getclipGif)},
  {"color",C2F(setpatternGif),C2F(getpatternGif)},
  {"colormap",C2F(setcolormapGif),C2F(gemptyGif)},
  {"dashes",C2F(set_dash_or_color_Gif),C2F(get_dash_or_color_Gif)},
  {"default",InitScilabGCGif, C2F(gemptyGif)},
  {"font",C2F(xsetfontGif),C2F(xgetfontGif)},
  {"foreground",C2F(setforegroundGif),C2F(getforegroundGif)},
  {"hidden3d",C2F(sethidden3dGif),C2F(gethidden3dGif)},
  {"lastpattern",C2F(semptyGif),C2F(getlastGif)},
  {"line mode",C2F(setabsourelGif),C2F(getabsourelGif)},
  {"line style",C2F(setdashGif),C2F(getdashGif)},
  {"mark",C2F(xsetmarkGif),C2F(xgetmarkGif)},
  {"pattern",C2F(setpatternGif),C2F(getpatternGif)},
  {"pixmap",C2F(semptyGif),C2F(gemptyGif)},
  {"thickness",C2F(setthicknessGif),C2F(getthicknessGif)},
  {"use color",C2F(usecolorGif),C2F(getusecolorGif)},
  {"viewport",C2F(semptyGif),C2F(gemptyGif)},
  {"wdim",C2F(setwindowdimGif),C2F(getwindowdimGif)},
  {"white",C2F(semptyGif),C2F(getlastGif)},
  {"window",C2F(setcurwinGif),C2F(getcurwinGif)},
  {"wpdim",C2F(semptyGif),C2F(gemptyGif)},
  {"wpos",C2F(setwindowposGif),C2F(getwindowposGif)},
  {"wresize",C2F(semptyGif),C2F(gemptyGif)},
  {"wshow",C2F(setwwhowGif),C2F(gemptyGif)},
  {"wwpc",C2F(semptyGif),C2F(gemptyGif)}
};

#ifdef lint

/* pour forcer lint a verifier ca */

static  test(str,flag,verbose,x1,x2,x3,x4,x5)
     char str[];
     int flag ;
     int  *verbose,*x1,*x2,*x3,*x4,*x5;
{ 
  double *dv;
  C2F(setalufunction1Gif)(x1,x2,x3,x4);C2F(getalufunctionGif)(verbose,x1,x2,dv);
  C2F(setclipGif)(x1,x2,x3,x4);C2F(getclipGif)(verbose,x1,x2,dv);
  C2F(setdashGif)(x1,x2,x3,x4);C2F(getdashGif)(verbose,x1,x2,dv);
  InitScilabGCGif(x1,x2,x3,x4); C2F(gemptyGif)(verbose,x1,x2,dv);
  C2F(xsetfontGif)(x1,x2,x3,x4);C2F(xgetfontGif)(verbose,x1,x2,dv);
  C2F(setabsourelGif)(x1,x2,x3,x4);C2F(getabsourelGif)(verbose,x1,x2,dv);
  C2F(xsetmarkGif)(x1,x2,x3,x4);C2F(xgetmarkGif)(verbose,x1,x2,dv);
  C2F(setpatternGif)(x1,x2,x3,x4);C2F(getpatternGif)(verbose,x1,x2,dv);
  C2F(setthicknessGif)(x1,x2,x3,x4);C2F(getthicknessGif)(verbose,x1,x2,dv);
  C2F(usecolorGif)(x1,x2,x3,x4);C2F(gemptyGif)(verbose,x1,x2,dv);
  C2F(setwindowdimGif)(x1,x2,x3,x4);C2F(getwindowdimGif)(verbose,x1,x2,dv);
  C2F(semptyGif)(x1,x2,x3,x4);C2F(getlastGif)(verbose,x1,x2,dv);
  C2F(setcurwinGif)(x1,x2,x3,x4);C2F(getcurwinGif)(verbose,x1,x2,dv);
  C2F(setwindowposGif)(x1,x2,x3,x4);getwindowposPos(verbose,x1,x2,dv);
}

#endif 


void C2F(scilabgcgetGif)(char *str, int *verbose, int *x1, int *x2, int *x3, int *x4, int *x5, double *dv1, double *dv2, double *dv3, double *dv4)
{
  int x6=0;
  C2F(ScilabGCGetorSetGif)(str,(int)1L,verbose,x1,x2,x3,x4,x5,&x6,dv1);
}

void C2F(scilabgcsetGif)(char *str, int *x1, int *x2, int *x3, int *x4, int *x5, int *x6, double *dv1, double *dv2, double *dv3, double *dv4)
{
  int verbose ;
  verbose = 0 ;
  C2F(ScilabGCGetorSetGif)(str,(int)0L,&verbose,x1,x2,x3,x4,x5,x6,dv1);}

void C2F(ScilabGCGetorSetGif)(char *str, int flag, int *verbose, int *x1, int *x2, int *x3, int *x4, int *x5, int *x6, double *dv1)
{ int i ;
 for (i=0; i < NUMSETFONC ; i++)
   {
     int j;
     j = strcmp(str,ScilabGCTabGif[i].name);
     if ( j == 0 ) 
       { if (*verbose == 1)
	 sciprint("\nGettting Info on %s\r\n",str);
       if (flag == 1)
	 (ScilabGCTabGif[i].getfonc)(verbose,x1,x2,dv1);
       else 
	 (ScilabGCTabGif[i].setfonc)(x1,x2,x3,x4,x5,x6,dv1);
       return;}
     else 
       { if ( j <= 0)
	 {
	   sciprint("\nUnknow GIF operator <%s>\r\n",str);
	   return;
	 }
       }
   }
 sciprint("\n Unknow GIF operator <%s>\r\n",str);
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
void C2F(DispStringAngleGif)(int *x0, int *yy0, char *string, double *angle)
{
  int i;
  int x,y, rect[4];
  double sina ,cosa,l;
  char str1[2];
  int verbose, Dnarg,Dvalue[10],j;
  verbose =0 ;

  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }

  str1[1]='\0';
  x= *x0;
  y= *yy0;
  sina= sin(*angle * M_PI/180.0);
  cosa= cos(*angle * M_PI/180.0);
  str1[0]=string[0];
  C2F(boundingboxGif)(str1,&x,&y,rect,PI0,PI0,PI0,PD0,PD0,PD0,PD0);

  C2F(getdashGif)(&verbose,Dvalue,&Dnarg,vdouble);
  C2F(setdashGif)((j=1,&j),PI0,PI0,PI0);
  for ( i = 0 ; i < (int)strlen(string); i++)
    { 
      str1[0]=string[i];
      gdImageString(GifIm, GifFont, x, y - rect[3], (unsigned char *)str1,
		    GifLineColor());
      C2F(boundingboxGif)(str1,&x,&y,rect,PI0,PI0,PI0,PD0,PD0,PD0,PD0);

      if ( cosa <= 0.0 && i < (int)strlen(string)-1)
	{ char str2[2];
	/** si le cosinus est negatif le deplacement est a calculer **/
	/** sur la boite du caractere suivant **/
	str2[1]='\0';str2[0]=string[i+1];
	C2F(boundingboxGif)(str2,&x,&y,rect,PI0,PI0,PI0,PD0,PD0,PD0,PD0);
	}
      if ( Abs(cosa) >= 1.e-8 )
	{
	  if ( Abs(sina/cosa) <= Abs(((double)rect[3])/((double)rect[2])))
	    l = Abs(rect[2]/cosa);
	  else 
	    l = Abs(rect[3]/sina);
	}
      else 
	l = Abs(rect[3]/sina);
      x +=  (int)(cosa*l*1.1);
      y +=  (int)(sina*l*1.1);
    }
  C2F(setdashGif)(Dvalue,PI0,PI0,PI0);
}

void C2F(displaystringGif)(char *string, int *x, int *y, int *v1, int *flag, int *v6, int *v7, double *angle, double *dv2, double *dv3, double *dv4)
{     
  int rect[4],x1=0,y1=0;

  int verbose, Dnarg,Dvalue[10],j;
  verbose =0 ;
  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  if ( Abs(*angle) <= 0.1) {
    C2F(boundingboxGif)(string,&x1,&y1,rect,PI0,PI0,PI0,PD0,PD0,PD0,PD0);
    C2F(getdashGif)(&verbose,Dvalue,&Dnarg,vdouble);
    C2F(setdashGif)((j=1,&j),PI0,PI0,PI0);
    gdImageString(GifIm, GifFont, *x, *y - rect[3], (unsigned char*) string,
		  GifLineColor());
    C2F(setdashGif)(Dvalue,PI0,PI0,PI0);
  }
  else 
    C2F(DispStringAngleGif)(x,y,string,angle);
}


void C2F(boundingboxGif)(char *string, int *x, int *y, int *rect, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{
  int k,width;
  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  width = 0;
  for (k=0;k<strlen(string);k++) 
    width += gdCharWidth(GifFont, string[k]);
  rect[0]= (int)(*x);
  rect[1]= (int)(*y);
  rect[2]= width;
  rect[3]= GifFont->h;
}

/** Draw a single line in current style **/

void C2F(drawlineGif)(int *xx1, int *yy1, int *x2, int *y2)
{
  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  gdImageThickLine(GifIm, *xx1, *yy1, *x2, *y2, GifLineColor(),
		   Max(1,ScilabGCGif.CurLineWidth));
}

/** Draw a set of segments **/
/** segments are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1]) **/
/** for i=0 step 2 **/

void C2F(drawsegmentsGif)(char *str, int *vx, int *vy, int *n, int *style, int *iflag, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{
  int verbose=0,Dnarg,Dvalue[10],NDvalue;
  int i;
  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }


  /* store the current values */


  C2F(get_dash_and_color_Gif)(&verbose,Dvalue,&Dnarg,vdouble);
  if ((int)  *iflag == 0 )
    {
      /** all segments have the same color or dash style */

      NDvalue= (*style < 1) ? Dvalue[0] : *style;
      C2F(set_dash_or_color_Gif)(&NDvalue,PI0,PI0,PI0);
      for ( i=0 ; i < *n/2 ; i++) {
	gdImageThickLine(GifIm, vx[2*i], vy[2*i],
			 vx[2*i + 1], vy[2*i + 1],
			 GifLineColor(),Max(1,ScilabGCGif.CurLineWidth));
      }
    }
  else
    {
      for ( i=0 ; i < *n/2 ; i++) 
	{
	  int NDvalue;
	  NDvalue = style[i];
	  C2F(setpatternGif)(&NDvalue,PI0,PI0,PI0);
          gdImageThickLine(GifIm, vx[2*i], vy[2*i],
			   vx[2*i + 1], vy[2*i + 1],
			   GifLineColor(),Max(1,ScilabGCGif.CurLineWidth));
	}
    }
  C2F(set_dash_and_color_Gif)( Dvalue,PI0,PI0,PI0);
}

/** Draw a set of arrows **/
/** arrows are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1]) **/
/** for i=0 step 2 **/
/** n is the size of vx and vy **/
/** as is 10*arsize (arsize) the size of the arrow head in pixels **/

void C2F(drawarrowsGif)(char *str, int *vx, int *vy, int *n, int *as, int *style, int *iflag, double *dv1, double *dv2, double *dv3, double *dv4)
{ 
  int verbose=0,Dnarg,Dvalue[10],NDvalue,i;
  double cos20=cos(20.0*M_PI/180.0);
  double sin20=sin(20.0*M_PI/180.0);
  int polyx[4],polyy[4]; 

  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }

  C2F(get_dash_and_color_Gif)(&verbose,Dvalue,&Dnarg,vdouble);
  for (i=0 ; i < *n/2 ; i++)
    { 
      double dx,dy,norm;
      if ( (int) *iflag == 1) 
	NDvalue = style[i];
      else 
	NDvalue=(*style < 1) ?  Dvalue[0] : *style;
      C2F(set_line_style_Gif)(&NDvalue,PI0,PI0,PI0);

      gdImageThickLine(GifIm, vx[2*i], vy[2*i],
		       vx[2*i + 1], vy[2*i + 1],
		       GifLineColor(),Max(1,ScilabGCGif.CurLineWidth));
      dx=( vx[2*i+1]-vx[2*i]);
      dy=( vy[2*i+1]-vy[2*i]);
      norm = sqrt(dx*dx+dy*dy);
      if ( Abs(norm) >  SMDOUBLE ) 
	{ int nn=1,p=3;
	dx=(*as/10.0)*dx/norm;dy=(*as/10.0)*dy/norm;
	polyx[0]= polyx[3]=inint(vx[2*i+1]+dx*cos20);
	polyx[1]= inint(polyx[0]  - cos20*dx -sin20*dy );
	polyx[2]= inint(polyx[0]  - cos20*dx + sin20*dy);
	polyy[0]= polyy[3]=inint(vy[2*i+1]+dy*cos20);
	polyy[1]= inint(polyy[0] + sin20*dx -cos20*dy) ;
	polyy[2]= inint(polyy[0] - sin20*dx - cos20*dy) ;
	C2F(fillpolylinesGif)("v",polyx,polyy,&NDvalue, &nn,&p,PI0,PD0,PD0,PD0,PD0);
	}
    }
  C2F(set_dash_and_color_Gif)( Dvalue,PI0,PI0,PI0);
}


/** Draw one rectangle **/

/** Draw or fill a set of rectangle **/
/** rectangles are defined by (vect[i],vect[i+1],vect[i+2],vect[i+3]) **/
/** for i=0 step 4 **/
/** (*n) : number of rectangles **/
/** fillvect[*n] : specify the action (see periX11.c) **/

void C2F(drawrectanglesGif)(char *str, int *vects, int *fillvect, int *n, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{
  int i,cpat,verb=0,num,cd[10],thick;

  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  C2F(getpatternGif)(&verb,&cpat,&num,vdouble);
  C2F(get_dash_and_color_Gif)(&verb,cd,&num,vdouble);
  thick = Max(1,ScilabGCGif.CurLineWidth);
  for (i = 0; i < *n; i++) {
    int x, y, w, h;
    x = vects[4 * i];
    y = vects[4 * i + 1];
    w = vects[4 * i + 2];
    h = vects[4 * i + 3];
    if ( fillvect[i] < 0 ) {
      int dash = - fillvect[i];
      C2F(set_line_style_Gif)(&dash,PI0,PI0,PI0);
      gdImageThickRectangle(GifIm, x, y, x + w, y + h, GifLineColor(),thick);
    }
    else if ( fillvect[i] == 0 ) 
      gdImageThickRectangle(GifIm, x, y, x + w, y + h, GifLineColor(),thick);
    else
      gdImageFilledRectangle(GifIm, x, y, x + w, y + h,
			     GifPatternColor(abs(fillvect[i])));
  }
  C2F(set_dash_and_color_Gif)(cd,PI0,PI0,PI0);
}

void C2F(drawrectangleGif)(char *str, int *x, int *y, int *width, int *height, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{
  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  gdImageRectangle(GifIm, *x, *y, *x + *width, *y + *height, GifLineColor());
}

/** Draw a filled rectangle **/

void C2F(fillrectangleGif)(char *str, int *x, int *y, int *width, int *height, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{ 

  int cpat,verb=0,num;

  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  C2F(getpatternGif)(&verb,&cpat,&num,vdouble);
  gdImageFilledRectangle(GifIm, *x, *y, *x + *width, *y + *height,
                         GifPatternColor(cpat));
}
/** Draw or fill a set of ellipsis or part of ellipsis **/
/** Each is defined by 6-parameters, **/
/** fillvect[*n] : specify the action <?> **/
/** caution angle=degreangle*64          **/

void C2F(fillarcsGif)(char *str, int *vects, int *fillvect, int *n, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{
  int verbose=0,Dnarg,pat;
  int i,i6;

  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  /* store the current values */
  C2F(getpatternGif)(&verbose,&pat,&Dnarg,vdouble);
  for ( i=0 ; i < *n ; i++) 
    {
      /** to fix the style */
      C2F(setpatternGif)(&fillvect[i],PI0,PI0,PI0);
      i6=6*i;
      C2F(fillarcGif)(str,&(vects[i6]),&(vects[i6+1]),&(vects[i6+2]),
		      &(vects[i6+3]),&(vects[i6+4]),&(vects[i6+5]) ,dv1, dv2, dv3, dv4);

    }
  C2F(setpatternGif)(&pat,PI0,PI0,PI0);
}

/** Draw a set of ellipsis or part of ellipsis **/
/** Each is defined by 6-parameters, **/
/** ellipsis i is specified by $vect[6*i+k]_{k=0,5}= x,y,width,height,angle1,angle2$ **/
/** <x,y,width,height> is the bounding box **/
/** angle1,angle2 specifies the portion of the ellipsis **/
/** caution : angle=degreangle*64          **/

void C2F(drawarcsGif)(char *str, int *vects, int *style, int *n, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{
  int verbose=0,Dnarg,Dvalue[10],NDvalue;
  int i,i6;

  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  /* store the current values */
  C2F(get_dash_and_color_Gif)(&verbose,Dvalue,&Dnarg,vdouble);
  for ( i=0 ; i < *n ; i++) 
    {
      /** to fix the style */
      NDvalue = style[i];
      C2F(set_line_style_Gif)(&NDvalue,PI0,PI0,PI0);
      i6=6*i;
      C2F(drawarcGif)(str,vects+i6,vects+(i6+1),vects+(i6+2),vects+(i6+3),
		      vects+(i6+4),vects+(i6+5) , dv1, dv2, dv3, dv4);
    }
  C2F(set_dash_and_color_Gif)( Dvalue,PI0,PI0,PI0);
}


/** Draw a single ellipsis or part of it **/
/** caution angle=degreAngle*64          **/

void C2F(drawarcGif)(char *str, int *x, int *y, int *width, int *height, int *angle1, int *angle2, double *dv1, double *dv2, double *dv3, double *dv4)
{ 
  int vx[365],vy[365],k,n;
  float alpha,fact=0.01745329251994330,w,h;
  int close = 0;

  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }

  w = (*width)/2.0;
  h = (*height)/2.0;
  n=Min((*angle2/64),360);
  for (k = 0; k < n; ++k) {
    alpha=((*angle1/64)+k)*fact;
    vx[k] = *x + w*(cos(alpha)+1.0);
    vy[k] = *y + h*(-sin(alpha)+1.0);}
    
  C2F(drawpolylineGif)(str, &n, vx, vy, &close, PI0, PI0, dv1, dv2, dv3, dv4);
}

/** Fill a single elipsis or part of it **/
/** with current pattern **/

void C2F(fillarcGif)(char *str, int *x, int *y, int *width, int *height, int *angle1, int *angle2, double *dv1, double *dv2, double *dv3, double *dv4)
{ 
  int vx[365],vy[365],k,k0,kmax,n;
  float alpha,fact=0.01745329251994330,w,h;
  int close = 1;
  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  n=Min((*angle2/64),360);

  w = (*width)/2.0;
  h = (*height)/2.0;
  k0 = 0;
  kmax = n-1;

  if (n != 360) {
    vx[0] = *x + w;
    vy[0] = *y + h;
    k0 = 1;
    kmax = n;}

  for (k = k0; k <= kmax; ++k) {
    alpha=((*angle1/64)+k)*fact;
    vx[k] = *x + w*(cos(alpha)+1.0);
    vy[k] = *y + h*(-sin(alpha)+1.0);}
  if (n != 360) {
    n++;
    vx[n] = *x + ((*width)/2.0);
    vy[n] = *y + ((*height)/2.0); 
    n++;
  }
  C2F(fillpolylineGif)(str, &n, vx, vy, &close, PI0, PI0, dv1, dv2, dv3, dv4);
}

/*--------------------------------------------------------------
  \encadre{Filling or Drawing Polylines and Polygons}
  ---------------------------------------------------------------*/

/** Draw a set of *n polylines (each of which have (*p) points) **/
/** with lines or marks **/
/** drawvect[i] >= use a mark for polyline i **/
/** drawvect[i] < 0 use a line style for polyline i **/

void C2F(drawpolylinesGif)(char *str, int *vectsx, int *vectsy, int *drawvect, int *n, int *p, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{ int verbose ,symb[2],Mnarg,Dnarg,Dvalue[10],NDvalue,i,j,close;
 verbose =0 ;
 if (GifIm == (gdImagePtr)0 ) {
   sciprint(" xinit must be called before any action \r\n");
   return;
 }
 /* store the current values */
 C2F(xgetmarkGif)(&verbose,symb,&Mnarg,vdouble);
 C2F(get_dash_and_color_Gif)(&verbose,Dvalue,&Dnarg,vdouble);
 for (i=0 ; i< *n ; i++)
   {
     if (drawvect[i] <= 0)
       { /** on utilise la marque de numero associ\'ee **/
	 NDvalue = - drawvect[i];
	 C2F(setdashGif)((j=1,&j),PI0,PI0,PI0);
	 C2F(xsetmarkGif)(&NDvalue,symb+1,PI0,PI0);
	  
	 C2F(drawpolymarkGif)(str,p,vectsx+(*p)*i,vectsy+(*p)*i,PI0,PI0,PI0,PD0,PD0,PD0,PD0);
	 C2F(setdashGif)(Dvalue,PI0,PI0,PI0);
       }
     else
       {/** on utilise un style pointill\'e  **/

	 C2F(set_line_style_Gif)(drawvect+i,PI0,PI0,PI0);
	 close = 0;
	 C2F(drawpolylineGif)(str,p,vectsx+(*p)*i,vectsy+(*p)*i,&close,PI0,PI0,PD0,PD0,PD0,PD0);
       }
   }
 /** back to default values **/
 C2F(set_dash_and_color_Gif)(Dvalue,PI0,PI0,PI0);
 C2F(xsetmarkGif)(symb,symb+1,PI0,PI0);
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

void C2F(fillpolylinesGif)(char *str, int *vectsx, int *vectsy, int *fillvect, int *n, int *p, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{
  int n1,i,j,o;

  gdPoint *points;
  int c,thick;
  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  n1 = *p;
  if (fillpolylines_closeflag) n1++;
  points = (gdPoint*) malloc(n1 * sizeof(gdPoint));
  if (points == (gdPoint*) NULL) return;
  for (j = 0; j < *n; j++) {
    o = j * (*p);
    for (i = 0; i < *p; i++) {
      points[i].x = vectsx[o + i];
      points[i].y = vectsy[o + i];
    }
    if (fillpolylines_closeflag) {
      points[*p].x = vectsx[o];
      points[*p].y = vectsy[o];
    }
    if (fillvect[j] != 0) {
      gdImageFilledPolygon(GifIm, points, n1,
			   GifPatternColor(abs(fillvect[j])));
    }
    if (fillvect[j] >= 0) {
      c = GifLineColor();
      thick = Max(1,ScilabGCGif.CurLineWidth);
      gdImagePolyLine(GifIm, &(vectsx[o]), &(vectsy[o]),
		      *p,c,thick,fillpolylines_closeflag);
    }
  }
  free(points);
}

/** Only draw one polygon with current line style **/
/** according to *closeflag : it's a polyline or a polygon **/

void C2F(drawpolylineGif)(char *str, int *n, int *vx, int *vy, int *closeflag, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{
  int thick,n1;
  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  thick = Max(1,ScilabGCGif.CurLineWidth);
  n1 = *n;
  gdImagePolyLine(GifIm,vx,vy,n1,GifLineColor(),thick,*closeflag);

}

/** Fill the polygon **/

void C2F(fillpolylineGif)(char *str, int *n, int *vx, int *vy, int *closeareaflag, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{
  int i =1;
  int cpat,verb=0,num;
  if (GifIm == (gdImagePtr)0 ) {
    sciprint(" xinit must be called before any action \r\n");
    return;
  }
  C2F(getpatternGif)(&verb,&cpat,&num,vdouble); 
  /** just fill  ==> cpat < 0 **/
  cpat = -cpat;
  fillpolylines_closeflag = *closeareaflag;
  C2F(fillpolylinesGif)(str,vx,vy,&cpat,&i,n,PI0,PD0,PD0,PD0,PD0);
}

/** Draw a set of  current mark centred at points defined **/
/** by vx and vy (vx[i],vy[i]) **/

void C2F(drawpolymarkGif)(char *str, int *n, int *vx, int *vy, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{ 
  int keepid,keepsize,i=1,sz=ScilabGCGif.CurHardSymbSize;

  keepid =  ScilabGCGif.FontId;
  keepsize= ScilabGCGif.FontSize;
  C2F(xsetfontGif)(&i,&sz,PI0,PI0);
  C2F(displaysymbolsGif)(str,n,vx,vy);
  C2F(xsetfontGif)(&keepid,&keepsize,PI0,PI0);
}

/*-----------------------------------------------------
  \encadre{Routine for initialisation}
  ------------------------------------------------------*/

void C2F(
	 initgraphicGif)(char *string, int *v2, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4, void *data)
{ 
  char string1[256];
  static int EntryCounter = 0;
  int  x[2],verbose=0,narg;
  double dummy;

  if (EntryCounter >= 1) C2F(xendgraphicGif)();
  strncpy(string1,string,256);

  file=fopen(string1,"wb");
  if (file == 0) 
    {
      sciprint("Can't open file %s\n");
      return;
    }
  x[0]=640;
  x[1]=480;

  if (CheckScilabXgc()) { 
    C2F(getwindowdim)(&verbose, x, &narg,&dummy);
  }
  
  LoadFontsGif();
  GifIm = gdImageCreate(x[0], x[1]);

  /*GifFont = gdFontSmall;*/
  ScilabGCGif.CurWindowWidth  = x[0];
  ScilabGCGif.CurWindowHeight = x[1];
  FileInitGif();
  ScilabGCGif.CurWindow =EntryCounter;
  EntryCounter =EntryCounter +1;
  
  gdImageFilledRectangle(GifIm, 0, 0, x[0]-1, x[1]-1, col_white); 

}

static void FileInitGif(void)
{
  int m,r,g,b,c,i;
  float R,G,B;
  double *bigcmap,*cmap;
  int *ind,m1,ierr,i1;
  int x[2],verbose,narg;

  verbose = 0; 
  C2F(getwindowdimGif)(&verbose,x,&narg,vdouble);
  ColorInitGif();
  InitScilabGCGif(PI0,PI0,PI0,PI0) ;

  if (  CheckColormap(&m) == 1) { /* a previously defined colormap */

    /* deallocate old colors*/
    for ( i=0; i < GifIm->colorsTotal; i++) 
      gdImageColorDeallocate(GifIm, i);
    for ( i=0;i < ScilabGCGif.Numcolors+2; i++) 
      col_index[i] = -1;

    if (m>gdMaxColors-3) {/* reduce the number of colors */
      if ( (bigcmap = (double*) malloc(3*m * sizeof(double)))== NULL) {
	Scistring("Not enough memory\n");
	return;
      }
      for ( i=0; i < m; i++) { /* get the previously defined colormap */
	get_r(i,&R);
	get_g(i,&G);
	get_b(i,&B);
        bigcmap[i] = R;
	bigcmap[i + m] = G;
	bigcmap[i + 2 * m] = B;
      }
      m1 = gdMaxColors-2;
      if ( (cmap = (double*) malloc(3*m1 * sizeof(double)))== NULL) {
	Scistring("Not enough memory\n");
	free(bigcmap);
	return;
      }
      if ( (ind = (int*) malloc(m * sizeof(int)))== NULL) {
	Scistring("Not enough memory\n");
	free(bigcmap);
	free(cmap);
	return;
      }

      C2F(nues1)(bigcmap,&m,cmap,&m1,ind,&ierr); /* compute new colormap */
      /* create new colormap */
      ScilabGCGif.Numcolors = m;
      for ( i=0; i < ScilabGCGif.Numcolors; i++) {
	i1 = ind[i] - 1;
	r=(int)(cmap[i1] * 255);
	g=(int)(cmap[i1 + m1] * 255);
	b=(int)(cmap[i1 + 2 * m1] * 255);
	if (r==255&&g==255&&b==255) {
	  /* move white a little to distinguish it from the background */
	  r=254;g=254;b=254; }
	c = gdImageColorExact(GifIm, r,g,b);
	if (c == -1)
	  c = gdImageColorAllocate(GifIm,r,g,b);
	col_index[i] = c;
      }
      free(ind);
      free(cmap);
      free(bigcmap);
    }
    else {
      /* create new color map */
      ScilabGCGif.Numcolors = m;
      for ( i=0; i < ScilabGCGif.Numcolors; i++) {
	get_r(i,&R);
	get_g(i,&G);
	get_b(i,&B);
	r = (int)(R*255);
	g = (int)(G*255);
	b = (int)(B*255);
	c = gdImageColorExact(GifIm, r,g,b);
	if (c == -1)
	  c = gdImageColorAllocate(GifIm,r,g,b);
	col_index[i] = c;
      }
    }
    /* add black and white at the end of the colormap */
    c = gdImageColorExact(GifIm, 0,0,0);
    if (c == -1) 
      c = gdImageColorAllocate(GifIm,0,0,0);
    col_index[m]=c;
    c = gdImageColorExact(GifIm, 255,255,255);
    if (c == -1) 
      c = gdImageColorAllocate(GifIm,255,255,255);
    col_index[m+1]=c;
    col_white=col_index[m+1];
    ScilabGCGif.NumForeground = m;
    ScilabGCGif.NumBackground = m + 1;
  }
  C2F(setpatternGif)((i=ScilabGCGif.NumForeground+1,&i),PI0,PI0,PI0); 
  if (ScilabGCGif.CurColorStatus == 1) 
    {
      ScilabGCGif.IDLastPattern = ScilabGCGif.Numcolors - 1;
    }

}

/*--------------------------------------------------------
  \encadre{Initialisation of the graphic context. Used also 
  to come back to the default graphic state}
  ---------------------------------------------------------*/


void InitScilabGCGif(int *v1, int *v2, int *v3, int *v4)
{ int i,j,col;
 ScilabGCGif.IDLastPattern = GREYNUMBER-1;
 ScilabGCGif.CurLineWidth=0 ;
 i=1;
 C2F(setthicknessGif)(&i,PI0,PI0,PI0);
 C2F(setalufunctionGif)("GXcopy");
 /** retirer le clipping **/
 i=j= -1;
 C2F(unsetclipGif)(PI0,PI0,PI0,PI0);
 C2F(xsetfontGif)((i=2,&i),(j=1,&j),PI0,PI0);
 C2F(xsetmarkGif)((i=0,&i),(j=0,&j),PI0,PI0);
 /** trac\'e absolu **/
 ScilabGCGif.CurVectorStyle = CoordModeOrigin ;
 /* initialisation des pattern dash par defaut en n&b */
 ScilabGCGif.CurColorStatus =0;
 C2F(setpatternGif)((i=1,&i),PI0,PI0,PI0);
 C2F(setdashGif)((i=1,&i),PI0,PI0,PI0);

 /* initialisation de la couleur par defaut */ 
 ScilabGCGif.Numcolors = DEFAULTNUMCOLORS;
 ScilabGCGif.CurColorStatus = 1 ;
 C2F(setpatternGif)((i=1,&i),PI0,PI0,PI0);
 C2F(setforegroundGif)((i=ScilabGCGif.NumForeground+1,&i),PI0,PI0,PI0);
 C2F(setbackgroundGif)((i=ScilabGCGif.NumForeground+2,&i),PI0,PI0,PI0);
 C2F(sethidden3dGif)((i=4,&i),PI0,PI0,PI0);
 /* Choix du mode par defaut (decide dans initgraphic_ */
 getcolordef(&col);
 /** we force CurColorStatus to the opposite value of col 
     to force usecolorPos to perform initialisations 
 **/
 ScilabGCGif.CurColorStatus = (col == 1) ? 0: 1;
 C2F(usecolorGif)(&col,PI0,PI0,PI0);
 if (col == 1) ScilabGCGif.IDLastPattern = ScilabGCGif.Numcolors - 1;
 strcpy(ScilabGCGif.CurNumberDispFormat,"%-5.2g");
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
void C2F(drawaxisGif)(char *str, int *alpha, int *nsteps, int *v2, int *initpoint, int *v6, int *v7, double *size, double *dx2, double *dx3, double *dx4)
{ int i;
 double xi,yi,xf,yf;
 double cosal,sinal;

 if (GifIm == (gdImagePtr)0 ) {
   sciprint(" xinit must be called before any action \r\n");
   return;
 }
 cosal= cos( (double)M_PI * (*alpha)/180.0);
 sinal= sin( (double)M_PI * (*alpha)/180.0);
 for (i=0; i <= nsteps[0]*nsteps[1]; i++)
   {
     if (( i % nsteps[0]) != 0)
       {
	 xi = initpoint[0]+i*size[0]*cosal;
	 yi = initpoint[1]+i*size[0]*sinal;
	 xf = xi - ( size[1]*sinal);
	 yf = yi + ( size[1]*cosal);
	 gdImageThickLine(GifIm,inint(xi),inint(yi),inint(xf),inint(yf), 
			  GifLineColor(),Max(1,ScilabGCGif.CurLineWidth));
       }
   }
 for (i=0; i <= nsteps[1]; i++)
   { xi = initpoint[0]+i*nsteps[0]*size[0]*cosal;
   yi = initpoint[1]+i*nsteps[0]*size[0]*sinal;
   xf = xi - ( size[1]*size[2]*sinal);
   yf = yi + ( size[1]*size[2]*cosal);
   gdImageThickLine(GifIm,inint(xi),inint(yi),inint(xf),inint(yf), 
		    GifLineColor(),Max(1,ScilabGCGif.CurLineWidth));
   }

 xi = initpoint[0]; yi= initpoint[1];
 xf = initpoint[0]+ nsteps[0]*nsteps[1]*size[0]*cosal;
 yf = initpoint[1]+ nsteps[0]*nsteps[1]*size[0]*sinal;
 gdImageThickLine(GifIm, inint(xi),inint(yi),inint(xf),inint(yf),
		  GifLineColor(),Max(1,ScilabGCGif.CurLineWidth));
}


/*-----------------------------------------------------
  \encadre{Display numbers z[i] at location (x[i],y[i])
  with a slope alpha[i] (see displaystring_), if flag==1
  add a box around the string.
  -----------------------------------------------------*/
void C2F(displaynumbersGif)(char *str, int *x, int *y, int *v1, int *v2, int *n, int *flag, double *z, double *alpha, double *dx3, double *dx4)
{ int i ;
 char buf[20];
 for (i=0 ; i< *n ; i++)
   { 
     sprintf(buf,ScilabGCGif.CurNumberDispFormat,z[i]);
     C2F(displaystringGif)(buf,&(x[i]),&(y[i]),PI0,flag,PI0,PI0,&(alpha[i]),PD0,PD0,PD0) ;
   }
}

/** Global variables to deal with fonts **/

#define FONTNUMBER 6
#define FONTMAXSIZE 6
#define SYMBOLNUMBER 10


static char *sizeGif[] = { "08" ,"10","12","14","18","24"};
static int  isizeGif[FONTNUMBER] = { 8 ,10,12,14,18,24 };

/* FontsList : stockage des structures des fonts 
   la font i a la taille fsiz se trouve ds 
   FontsList[i][fsiz]->fid
*/
gdFont FontListGif[FONTNUMBER][FONTMAXSIZE];


/* Dans FontInfoTabGif : on se garde des information sur les 
   fonts la fonts i a pour nom fname et ok vaut 1 si 
   elle a ete chargee ds le serveur 
   c'est loadfamilyGif qui se charge de charger une font a diverses 
   taille ds le serveur.
*/

struct FontInfo { int ok;
  char fname[100];
} FontInfoTabGif[FONTNUMBER];

typedef  struct  {
  char *alias;
  char *name;
}  FontAlias;

/** ce qui suit marche sur 75dpi ou 100dpi **/

static FontAlias fonttab[] ={
  {"courR", "-adobe-courier-medium-r-normal--*-%s0-*-*-m-*-iso8859-1"},
  {"symb", "-adobe-symbol-medium-r-normal--*-%s0-*-*-p-*-adobe-fontspecific"},
  {"timR", "-adobe-times-medium-r-normal--*-%s0-*-*-p-*-iso8859-1"},
  {"timI", "-adobe-times-medium-i-normal--*-%s0-*-*-p-*-iso8859-1"},
  {"timB", "-adobe-times-bold-r-normal--*-%s0-*-*-p-*-iso8859-1"},
  {"timBI", "-adobe-times-bold-i-normal--*-%s0-*-*-p-*-iso8859-1"},
  {(char *) NULL,( char *) NULL}
};

/** To set the current font id of font and size **/

void C2F(xsetfontGif)(int *fontid, int *fontsize, int *v3, int *v4)
{ int i,fsiz;
 i = Min(FONTNUMBER-1,Max(*fontid,0));
 fsiz = Min(FONTMAXSIZE-1,Max(*fontsize,0));
 if ( FontInfoTabGif[i].ok !=1 )
   { 
     if (i != 6 )
       {
	 C2F(loadfamilyGif)(fonttab[i].alias,&i,PI0,PI0,PI0,PI0,PI0,PD0,PD0,PD0,PD0);
       }
     else 
       {
	 sciprint(" The Font Id %d is not affected \r\n",(int)i);
	 Scistring(" use xlfont to set it \n");
	 return;
       }
   }
 ScilabGCGif.FontId = i;
 ScilabGCGif.FontSize = fsiz;
 GifFont = &(FontListGif[i][fsiz]);
}

/** To get the values id and size of the current font **/

void C2F(xgetfontGif)(int *verbose, int *font, int *nargs, double *dummy)
{
  *nargs=2;
  font[0]= ScilabGCGif.FontId ;
  font[1] =ScilabGCGif.FontSize ;
  if (*verbose == 1) 
    {
      sciprint("\nFontId : %d ",	      ScilabGCGif.FontId );
      sciprint("--> %s at size %s pts\r\n",
	       "GifFont",
	       sizeGif[ScilabGCGif.FontSize]);
    }
}

/** To set the current mark : using the symbol font of adobe **/

void C2F(xsetmarkGif)(int *number, int *size, int *v3, int *v4)
{ 
  ScilabGCGif.CurHardSymb = Max(Min(SYMBOLNUMBER-1,*number),0);
  ScilabGCGif.CurHardSymbSize = Max(Min(FONTMAXSIZE-1,*size),0);
  ;}

/** To get the current mark id **/

void C2F(xgetmarkGif)(int *verbose, int *symb, int *narg, double *dummy)
{
  *narg =2 ;
  symb[0] = ScilabGCGif.CurHardSymb ;
  symb[1] = ScilabGCGif.CurHardSymbSize ;
  if (*verbose == 1) 
    sciprint("\nMark : %d at size %d pts\r\n",
	     ScilabGCGif.CurHardSymb,
	     isizeGif[ScilabGCGif.CurHardSymbSize]);
}

char symb_listGif[] = {
  /*
    0x2e : . alors que 0xb7 est un o plein trop gros 
    ., +,X,*,diamond(filled),diamond,triangle up,triangle down,trefle,circle*/
  (char)0x2e,(char)0x2b,(char)0xb4,(char)0xc5,(char)0xa8,
  (char)0xe0,(char)0x44,(char)0xd1,(char)0xa7,(char)0x4f};

static void C2F(displaysymbolsGif)(char *str, int *n, int *vx, int *vy)
{
  int col, i, c, sz;
  col = ( ScilabGCGif.CurColorStatus ==1) ? ScilabGCGif.CurColor : ScilabGCGif.CurPattern ;
  for (i = 0; i < *n; i++) {
    c = Char2Int(symb_listGif[ScilabGCGif.CurHardSymb]);
    gdImageSymb(GifIm, GifFont, vx[i], vy[i], c,GifLineColor());
    /*            sz = isizeGif[ScilabGCGif.CurHardSymbSize];
		  gdImageChar(GifIm, GifFont, vx[i], vy[i]-sz, c,GifLineColor());*/
  }
}

/*-------------------------------------------------------
  \encadre{Check if a specified family of font exist in GIF }
  -------------------------------------------------------*/

void C2F(loadfamilyGif)(char *name, int *j, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{
  FILE *ff;
  char fname[200];
  int i,ierr;
  char *SciPath;
  gdFontPtr Font;

  /** generic name with % **/
  if ( strchr(name,'%') != (char *) NULL)
    {
      sciprint("Generic font names are not supported by gif driver\n");
      return;
    }
  SciPath=getenv("SCI");
  if (SciPath==NULL)
    {
      Scistring("The SCI environment variable is not set\n");
      return;
    }
  fname[0]=0;

  if (FontInfoTabGif[*j].ok == 1) { /* Font number already used */
    if (strcmp(FontInfoTabGif[*j].fname,name)!=0) { /* by a different font */
      /* unload this font */
      FontInfoTabGif[*j].ok = 0;
      for (i=0;i<FONTMAXSIZE;i++) {
	Font = &(FontListGif[*j][i]);
	if (Font != NULL) free(Font->data);
      }
    }
  }
  if (FontInfoTabGif[*j].ok == 0) {
    for (i=0;i<FONTMAXSIZE;i++) {
      sprintf(fname,"%s/imp/giffonts/75dpi/%s%s.bdf",
	      SciPath,name,sizeGif[i]);
      ff=fopen(fname,"r");
      if (ff == 0) 
	{
	  sciprint("Can't open font file %s\n",fname);
	  return;
	}
      ierr=ReadbdfFont(ff,&(FontListGif[*j][i]),fname);
      fclose(ff);
      if  (ierr==1) {
	sciprint("\n Cannot allocate memory for font : %s%s\n",name,sizeGif[i]);
	return;
      }
      if  (ierr==2) {
	sciprint("\n Font not found: %s%s\n",name,sizeGif[i]);
	return;
      }
      

      FontInfoTabGif[*j].ok = 1;
      strcpy(FontInfoTabGif[*j].fname,name);
    }
  }
}

void C2F(queryfamilyGif)(char *name, int *j, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4)
{ 
  int i ;
  name[0]='\0';
  for (i=0;i<FONTNUMBER;i++) {
    v3[i]=strlen(FontInfoTabGif[i].fname);
    strcat(name,FontInfoTabGif[i].fname);
  }
  *j=FONTNUMBER;
}


/*------------------------END--------------------*/
static void LoadFontsGif(void)
{
  int i;

  /*CourR */
  i = 0;
  C2F(loadfamilyGif)("courR",&i,PI0,PI0,PI0,PI0,PI0,PD0,PD0,PD0,PD0); 

  /*Symb */
  i = 1;
  C2F(loadfamilyGif)("symb",&i,PI0,PI0,PI0,PI0,PI0,PD0,PD0,PD0,PD0); 

  /*TimR */
  i = 2;
  C2F(loadfamilyGif)("timR",&i,PI0,PI0,PI0,PI0,PI0,PD0,PD0,PD0,PD0);
 
  /*timI */
  /* i = 3;
     C2F(loadfamilyGif)("timI",&i,PI0,PI0,PI0,PI0,PI0,PD0,PD0,PD0,PD0);*/

  /*timB */
  /* i = 4;
     C2F(loadfamilyGif)("timB",&i,PI0,PI0,PI0,PI0,PI0,PD0,PD0,PD0,PD0);*/

  /*timBI */
  /* i = 5;
     C2F(loadfamilyGif)("timBI",&i,PI0,PI0,PI0,PI0,PI0,PD0,PD0,PD0,PD0);*/

}
