/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/

/*----------------------BEGIN----------------------
\def\encadre#1{\paragraph{}\fbox{\begin{minipage}[t]{15cm}#1 \end{minipage}}}
\section{A Fig Driver}
---------------------------------------------------*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#define PERI_PRIVATE 1
#include "nsp/sciio.h"
#include "nsp/math.h"
#include "nsp/graphics/periFig.h"
#include "nsp/version.h"
#include "nsp/graphics/color.h"

#define WHITE 7
#define BLACK 0

static void Write2Vect(const int *vx,const  int *vy, int n, int flag); 
static void WriteGeneric(BCG *Xgc,char *string, int nobj, int sizeobj,const int *vx, const int *vy, int sizev, int flag,const int *fvect);
static void InitScilabGCXfig(BCG *Xgc);
static void set_c_Fig(BCG *Xgc,int i);
static void idfromname (char *name1, int *num);
static void displaysymbols (BCG *Xgc,int *vx, int *vy,int n);
static int FigQueryFont(char *name);
static void fig_set_color(BCG *Xgc,int c, int *color);

#define Char2Int(x)   ( x & 0x000000ff )

/* Global variables to deal with fonts **/

#define FONTNUMBER 7
#define FONTMAXSIZE 6
#define SYMBOLNUMBER 10
int FontsListXfig_[FONTNUMBER][FONTMAXSIZE];
struct SciFontInfo { int ok;
		  char fname[20];} FontInfoTabXfig_[FONTNUMBER];
/* xfig code for our fonts **/
static int  xfig_font[]= { 12,32,0,1,2,3,0};
/* static char *sizeXfig_[] = { "08" ,"10","12","14","18","24"};*/
static int  isizeXfig_[] = { 8,10,12,14,18,24};

#if defined(__CYGWIN32__) || defined(__MINGW32__) || defined(__GNUC__) || defined(__MSC__)
static FILE *file= (FILE *) 0;
#define FPRINTF(x) ( file != (FILE*) 0) ?  fprintf x  : 0 
#else 
static FILE *file= stdout ;
#define FPRINTF(x) fprintf x  
#endif

static void FileInit (BCG *Xgc);

static char symb_list[] = {
  /*
     0x2e : . alors que 0xb7 est un o plein trop gros 
     ., +,X,*,diamond(filled),diamond,triangle up,triangle down,trefle,circle*/
  (char)0x2e,(char)0x2b,(char)0xb4,(char)0xc5,(char)0xa8,
  (char)0xe0,(char)0x44,(char)0xd1,(char)0xa7,(char)0x4f};


/* Structure to keep the graphic state  **/

BCG  ScilabGCXfig ;

/*-----------------------------------------------------
 * default colormap coming from Xfig 
 *-----------------------------------------------------*/

/* These DEFAULTNUMCOLORS colors come from Xfig */

unsigned short  default_colors[] = {
  0,   0,   0, /* Black: DEFAULTBLACK */
  0,   0, 255, /* Blue */
  0, 255,   0, /* Green */
  0, 255, 255, /* Cyan */
  255,   0,   0, /* Red */
  255,   0, 255, /* Magenta */
  255,   0,   0, /* Yellow */
  255, 255, 255, /* White: DEFAULTWHITE */
  0,   0, 144, /* Blue4 */
  0,   0, 176, /* Blue3 */
  0,   0, 208, /* Blue2 */
  135, 206, 255, /* LtBlue */
  0, 144,   0, /* Green4 */
  0, 176,   0, /* Green3 */
  0, 208,   0, /* Green2 */
  0, 144, 144, /* Cyan4 */
  0, 176, 176, /* Cyan3 */
  0, 208, 208, /* Cyan2 */
  144,   0,   0, /* Red4 */
  176,   0,   0, /* Red3 */
  208,   0,   0, /* Red2 */
  144,   0, 144, /* Magenta4 */
  176,   0, 176, /* Magenta3 */
  208,   0, 208, /* Magenta2 */
  128,  48,   0, /* Brown4 */
  160,  64,   0, /* Brown3 */
  192,  96,   0, /* Brown2 */
  255, 128, 128, /* Pink4 */
  255, 160, 160, /* Pink3 */
  255, 192, 192, /* Pink2 */
  255, 224, 224, /* Pink */
  255, 215,   0  /* Gold */
};

/*-----------------------------------------------------
\encadre{General routines}
-----------------------------------------------------*/

/* To select the graphic Window  **/

static void xselgraphic(BCG *Xgc) {}

/* End of graphic (close the file)  **/

static void xendgraphic(BCG *Xgc)
{
  if (file != stdout && file != (FILE*) 0) {
    fclose(file);
    file=stdout;
  }
}

static void xend(BCG *Xgc) 
{
  xendgraphic(Xgc);
}

static void delete_window(BCG *dd,int intnum) {};


/* Clear the current graphic window     **/
/* In Fig : nothing      **/

static void clearwindow(BCG *Xgc) {}


/* To generate a pause : Empty here **/

static void xpause(int sec_time) {}

static void force_redraw(BCG *Xgc) {};


/*-----------------------------------------------------------------
 * Changes the graphic window popupname 
 *-----------------------------------------------------------------*/

static void setpopupname(BCG *Xgc,char *name){}

static void xset_winprotect(BCG *Xgc, int val) {};

/* Wait for mouse click in graphic window : Empty here **/

static void xclick(BCG *Xgc,char *str, int *ibutton, int *x1, int *yy1, int iflag, int motion,int release,int key,int istr) {} 

static void xclick_any(char *str, int *ibutton, int *x1, int *yy1, int *iwin, int iflag, int motion,int release,int key,int istr) {} 

static void xgetmouse(BCG *Xgc,char *str, int *ibutton, int *x1, int *yy1, int queue,int motion,int release,int key){};

/* Clear a rectangle **/

void cleararea(BCG *Xgc,int x, int y, int w, int h)
{
  FPRINTF((file,"# %d %d %d %d clearzone\n",x,y,w,h));
}

/*
 * graphic context modifications 
 */

/* record or not the graphic commands */

static int xget_recording(BCG *Xgc)
{
  return Xgc->record_flag;
}

static void xset_recording(BCG *Xgc, int val)
{
  Xgc->record_flag = FALSE; /* never record with Xfig */
}

/* to get the window upper-left point coordinates **/

void xget_windowpos(BCG *Xgc,int *x,int *y) 
{
  *x = *y = 0;
}

/* to set the window upper-left point position (Void) **/

static void xset_windowpos(BCG *Xgc,int x, int y){}


/* To get the window size **/
/* The default fig box    **/
/* for line thickness etc \ldots **/
static int prec_fact =16;


static void xget_windowdim(BCG *Xgc,int *x,int *y)
{     
  *x =  600*prec_fact;
  *y =  424*prec_fact;
} 

/* To change the window dimensions : do Nothing in Postscript  **/

static void xset_windowdim(BCG *Xgc,int x, int y){}
/* To get the popup  window size **/

static void xget_popupdim(BCG *Xgc,int *x, int *y)
{
  *x= 600*prec_fact;
  *y= 424*prec_fact;
} 

/* To change the popup window size  **/

static void xset_popupdim(BCG *Xgc,int x, int y)
{
}

/* To get the viewport Upper/Left point Position **/

static void xget_viewport(BCG *Xgc,int *x, int *y) {       *x = *y =0;} 

/* To change the window size  **/

static void xset_viewport(BCG *Xgc,int x, int y) {}

/* Select a graphic Window : Empty for Postscript **/

static int xset_curwin(int intnum, int set_menu)
{
  BCG *Xgc = &ScilabGCXfig; 
  int i =  Xgc->CurWindow;
  Xgc->CurWindow  = intnum;
 return i;
}

/* Get the id number of the Current Graphic Window **/

static int xget_curwin(void)
{
  BCG *Xgc = &ScilabGCXfig; 
  return  Xgc->CurWindow ;
}


/* Set a clip zone (rectangle ) **/

static void xset_clip(BCG *Xgc,int x[])
{
  int i;
  Xgc->ClipRegionSet = 1;
  for ( i = 0 ; i < 4 ; i++)   Xgc->CurClipRegion[i]= x[i];
  FPRINTF((file,"# %d %d %d %d setclipzone\n",*x,*(x+1),*(x+2),*(x+3)));
}


/* unset clip zone **/

static void xset_unclip(BCG *Xgc)
{
  Xgc->ClipRegionSet = 0;
  Xgc->CurClipRegion[0]= -1;
  Xgc->CurClipRegion[1]= -1;
  Xgc->CurClipRegion[2]= 200000;
  Xgc->CurClipRegion[3]= 200000;
  FPRINTF((file,"# %d %d %d %d setclipzone\n",-1,-1,200000,200000));
}

/* Get the boundaries of the current clip zone **/

static void xget_clip(BCG *Xgc, int *x)
{
  x[0] = Xgc->ClipRegionSet;
  if ( x[0] == 1)
    {
      x[1] =Xgc->CurClipRegion[0];
      x[2] =Xgc->CurClipRegion[1];
      x[3] =Xgc->CurClipRegion[2];
      x[4] =Xgc->CurClipRegion[3];
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


/* to get information on absolute or relative mode **/

static int xget_absourel(BCG *Xgc)
{
  return  Xgc->CurVectorStyle  ;
}


/* The alu function for drawing : Works only with X11 **/
/* Not in Postscript **/

static void xset_alufunction(BCG *Xgc,char *string)
{     
  int value;
  idfromname(string,&value);
  if ( value != -1)
    {
      Xgc->CurDrawFunction = value;
      FPRINTF((file,"# %d setalufunction\n",(int)value));
    }
}

/* All the possibilities : Read The X11 manual to get more informations **/

struct alinfo { 
  char *name;
  char id;
  char *info;} AluStrucXfig_[] =
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
{
  int i;
 *num = -1;
 for ( i =0 ; i < 16;i++)
   if (strcmp(AluStrucXfig_[i].name,name1)== 0) 
     *num=AluStrucXfig_[i].id;
 if (*num == -1 ) 
   {
     Scistring("\n Use the following keys :");
     for ( i=0 ; i < 16 ; i++)
       {
	 sciprint("\nkey %s ",AluStrucXfig_[i].name);
	 sciprint("-> %s\r\n",AluStrucXfig_[i].info);
       }
   }
}


static void xset_alufunction1(BCG *Xgc,int num)
{     
  int value;
  value=AluStrucXfig_[Min(16,Max(0,num))].id;
  if ( value != -1)
    {
      Xgc->CurDrawFunction = value;
      /* to be done */
    }
}

/* To get the value of the alufunction **/

static int xget_alufunction(BCG *Xgc)
{ 
  return  Xgc->CurDrawFunction ;
}

/* to set the thickness of lines :min is 1 is a possible value **/
/* give the thinest line **/

static void xset_thickness(BCG *Xgc,int value)
{ 
  Xgc->CurLineWidth =Max(1,value);
  FPRINTF((file,"# %d Thickness\n",  Xgc->CurLineWidth ));
}

/* to xget_ the thicknes value **/

static int xget_thickness(BCG *Xgc)
{
  return Xgc->CurLineWidth ;
}
     
/*-------------------------------------------------
\encadre{To set grey level for filing areas.
  from black (*num =0 ) to white 
  you must use the get function to get the id of 
  the white pattern }
----------------------------------------------------*/

static int xset_pattern(BCG *Xgc,int num)
{ 
  int i ; 
  int old = xget_pattern(Xgc);
  if (  Xgc->CurColorStatus ==1) 
    {
      i= Max(0,Min(num-1,Xgc->Numcolors+1));
      Xgc->CurColor = i ;
      set_c_Fig(Xgc,i);
    }
  else 
    {
      i= Max(0,Min(num-1,GREYNUMBER-1));
      Xgc->CurPattern = i;
      if (i ==0)
	FPRINTF((file,"# fillsolid\n"));
      else 
	FPRINTF((file,"# %d Setgray\n",(int)i));
    }
  return old;
}

/* To get the id of the current pattern  **/

static int xget_pattern(BCG *Xgc)
{ 
  if ( Xgc->CurColorStatus ==1) 
    {
      return Xgc->CurColor+1 ;
    }
  else 
    {
      return  Xgc->CurPattern +1;
    }
}

/* To get the id of the last pattern **/

static int xget_last(BCG *Xgc)
{
  return Xgc->IDLastPattern +1 ;
}


/* To set dash-style : **/
/*  use a table of dashes and set default dashes to **/
/*  one of the possible value. value int **/
/*  to a strictly positive int **/

#define MAXDASH 6
/* dot specification for line style  **/
/* solid dash dotted dash-dot dash-dot-dot dash-dot-dot-dot   */
static int DashTab[6] =      {0,1,1,2,2,2};
static int DashTabStyle[6] = {0,2,4,2,4,8};



/* style arguments sets either dash style either color */

static void xset_line_style(BCG *Xgc,int value)
{
  if (Xgc->CurColorStatus == 0) {
    xset_dash(Xgc,value);
    xset_pattern(Xgc,1);
  }
  else {
    xset_dash(Xgc,Xgc->CurDashStyle + 1);
    xset_pattern(Xgc,value);
  }
}

/* To change The Pos-default dash style **/
/* if *value == 0, use a solid line, if *value != 0 **/
/* the dash style is specified by the xx vector of n values **/
/* xx[3]={5,3,7} and *n == 3 means :  5white 3 void 7 white \ldots **/
  
static void xset_dashstyle(BCG *Xgc,int value, int *xx, int *n)
{
  
}


/* old version of setdashXfig retained for compatibility */

static void xset_dash_or_color(BCG *Xgc,int value)
{
  static int maxdash = 6,l3 ;
  if ( Xgc->CurColorStatus ==1) 
    {
      int i;
      i= Max(0,Min(value-1,Xgc->Numcolors+1));
      Xgc->CurColor =i;
      set_c_Fig(Xgc,i);
    }
  else
    {
      l3 = Max(0,Min(maxdash-1,value-1));
      Xgc->CurDashStyle = l3;
    }
}

static int xset_dash(BCG *Xgc,int value)
{
  int old = xget_dash(Xgc);
  int maxdash = 6,l3 ;
  l3 = Max(0,Min(maxdash-1,value-1));
  Xgc->CurDashStyle = l3;
  return old;
}

static void xset_dash_and_color(BCG *Xgc,int dash,int color)
{
  xset_dash(Xgc,dash);
  xset_pattern(Xgc,color);
}

/* to get the current dash-style **/

static int xget_dash(BCG *Xgc)
{
  return Xgc->CurDashStyle+1; 
  /* 
  *narg = 3;
  value[0]=i+1;
  value[1]=DashTab[i];
  value[2]=DashTabStyle[i];
  if (*verbose ==1 ) 
    {
      sciprint("\nDash Style %d.",(int) *value);
      sciprint("%d %d ",(int)value[1],(int)value[2]);
      Scistring(">\n");
    }
  */
 
}

static void xget_dash_and_color(BCG *Xgc,int *dash,int *color)
{
  *dash= xget_dash(Xgc);
  *color=xget_pattern(Xgc);
}


static void xset_usecolor(BCG *Xgc,int num)
{
  int i;
  i =  Min(Max(num,0),1);
  if (  Xgc->CurColorStatus != (int) i) 
    {
      if (Xgc->CurColorStatus == 1) 
	{
	  /* je passe de Couleur a n&b */
	  /* remise des couleurs a vide */
	  Xgc->CurColorStatus = 1;
	  xset_pattern(Xgc,1);
	  /* passage en n&b */
	  Xgc->CurColorStatus = 0;
	  i= Xgc->CurPattern+1;
	  xset_pattern(Xgc,i);
	  i= Xgc->CurDashStyle+1;
	  xset_dash(Xgc,i);
          Xgc->IDLastPattern = GREYNUMBER - 1;
	}
      else 
	{
	  /* je passe en couleur */
	  /* remise a zero des patterns et dash */
	  /* remise des couleurs a vide */
	  Xgc->CurColorStatus = 0;
	  xset_pattern(Xgc,1);
	  xset_dash(Xgc,1);
	  /* passage en couleur  */
	  Xgc->CurColorStatus = 1;
	  i= Xgc->CurColor+1;
	  xset_pattern(Xgc,i);
	  Xgc->IDLastPattern = Xgc->Numcolors -1;
	}
    }
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




/*
 * Setting the colormap
 * Attention :
 *   cette fonction n'est utilis'ee que si l''on est sous
 *   le peripherique Xfig et que l'on appelle
 *   explicitement xset("colormap",....)
 * ds le cas usuel comme cette fonction n'est pas
 * enregistree ds Rec.c elle ne doit pas etre appellee
 */


static void xset_colormap(BCG *Xgc,int m,int n, double *a)
{
  int i;
  Scistring("Warning : you will have to move the colors definition\n");
  Scistring(" at the top of the xfig file \n");
  if ( n != 3 ||  m < 0) {
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
  Xgc->Numcolors = m;
  Xgc->IDLastPattern = m - 1;
  Xgc->NumForeground = m;
  Xgc->NumBackground = m + 1;
  for ( i=0; i < m ; i++)
    {
      unsigned short ur,ug,ub;
      ur = (unsigned short) (255.0*a[i]);
      ug = (unsigned short) (255.0*a[i+m]);
      ub = (unsigned short) (255.0*a[i+2*m]);
      FPRINTF((file,"0 %d #%02x%02x%02x \n",32+i,ur,ug,ub));
    }
  FPRINTF((file,"0 %d #%02x%02x%02x \n",32+m,0,0,0));
  FPRINTF((file,"0 %d #%02x%02x%02x \n",32+m+1,255,255,255));
  xset_usecolor(Xgc,i);
  xset_alufunction1(Xgc,3);
  xset_pattern(Xgc,Xgc->NumForeground+1);
  xset_foreground(Xgc,Xgc->NumForeground+1);
  xset_background(Xgc,Xgc->NumForeground+2);
}

static void xset_default_colormap(BCG *Xgc)
{
  unsigned short *a = default_colors;
  int   m = DEFAULTNUMCOLORS;
  int i;
  Scistring("Warning : you will have to move the colors definition\n");
  Scistring("at the top of the xfig file \n");
  Xgc->Numcolors = m;
  Xgc->IDLastPattern = m - 1;
  Xgc->NumForeground = m;
  Xgc->NumBackground = m + 1;
  for ( i=0; i < m ; i++)
    {
      unsigned short ur,ug,ub;
      ur = a[i];
      ug = a[i+m];
      ub = a[i+2*m];
      FPRINTF((file,"0 %d #%02x%02x%02x \n",32+i,ur,ug,ub));
    }
  FPRINTF((file,"0 %d #%02x%02x%02x \n",32+m,0,0,0));
  FPRINTF((file,"0 %d #%02x%02x%02x \n",32+m+1,255,255,255));
  xset_usecolor(Xgc,i);
  xset_alufunction1(Xgc,3);
  xset_pattern(Xgc,Xgc->NumForeground+1);
  xset_foreground(Xgc,Xgc->NumForeground+1);
  xset_background(Xgc,Xgc->NumForeground+2);
}

/* getting the colormap XXXX */

static void xget_colormap(BCG *Xgc, int *num,  double *val)
{
  *num=0 ; /* XXX */
}


static void set_c_Fig(BCG *Xgc,int i)
{
  int j;
  j=Max(Min(i,Xgc->Numcolors+1),0);
  Xgc->CurColor=j;
  FPRINTF((file,"\n# %d Setcolor\n",(int)i));
}

/* set and get the number of the background or foreground */

static void xset_background(BCG *Xgc,int num)
{ 
  if (Xgc->CurColorStatus == 1) 
    {
      Xgc->NumBackground = Max(0,Min(num - 1,Xgc->Numcolors+1));
    }
}

static int xget_background(BCG *Xgc)
{ 
  if ( Xgc->CurColorStatus == 1 ) 
    {
      return Xgc->NumBackground + 1;
    }
  else 
    {
      return  1;
    }
}


/* set and get the number of the background or foreground */

static void xset_foreground(BCG *Xgc,int num)
{ 
  if (Xgc->CurColorStatus == 1) 
    {
      Xgc->NumForeground = Max(0,Min(num - 1,Xgc->Numcolors+1));
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
      return Xgc->IDLastPattern + 1;
    }
}

/* set and get the number of the hidden3d color */

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
      return  Xgc->NumHidden3d + 1;
    }
  else 
    {
      return 1;
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

static void displaystring(BCG *Xgc,char *string, int x, int y, int flag, double angle)
{    
  int rect[4], font=-1,font_flag=2;
  int Dvalue1;
  int pen_color;
  boundingbox(Xgc,string,x,y,rect);
  if (string[0]== '$') 
    {
      font=-1;
      font_flag=2;
    }
  else 
    {
      font =  xfig_font[Xgc->fontId];
      font_flag= 4; 
    };
  Dvalue1 = xget_pattern(Xgc);
  fig_set_color(Xgc,Dvalue1,&pen_color);
  FPRINTF((file,"4 0 %d 0 0 %d %d %5.2f %d %5.2f %5.2f %d %d %s\\001\n",
	  pen_color,
	  (int)font,
	  (int)isizeXfig_[Xgc->fontSize],/*prec_fact,*/
	  -(M_PI/180.0)*(angle),
	  (int)font_flag,
	  (double) rect[3],
	  (double) rect[2],
	   x,y, string));
  if ( flag == 1) 
    {
      rect[0]=rect[0]-4;rect[2]=rect[2]+6;
      drawrectangle(Xgc,rect);
    }
}

int bsizeXfig_[6][4]= {{ 0,-7,463,9  },
		{ 0,-9,574,12 },
		{ 0,-11,674,14},
		{ 0,-12,779,15},
		{0, -15,972,19 },
		{0,-20,1341,26}};

/* To get the bounding rectangle of a string **/

void boundingbox(BCG *Xgc,char *string, int x, int y, int rect[])
{
  int font[2];
  xget_font(Xgc,font);
  rect[0]= (int)(x+bsizeXfig_[font[1]][0]*((double) prec_fact));
  rect[1]= (int)(y+bsizeXfig_[font[1]][1]*((double) prec_fact));
  rect[2]= (int)(((double)prec_fact)*(bsizeXfig_[font[1]][2]/100.0)*((double)strlen(string)));
  rect[3]= (int)(bsizeXfig_[font[1]][3]*((double) prec_fact));
}

/* 
  Bounding box for marks : we have used xfig to get the  bounding boxes
  ie the correct w and h ( rect[2],rect[3]) 
**/


int symb_xw[FONTMAXSIZE][SYMBOLNUMBER]={
  {15,75,60,90,90,90,90,90,90,90},
  {30,75,75,105,75,105,105,105,75,105},
  {15,75,75,135,90,135,135,135,105,135},
  {30,105,105,135,105,165,150,165,120,150},
  {30,120,135,195,135,210,195,195,135,195},
  {45,150,150,255,195,270,255,270,195,255}
};

int symb_yh[FONTMAXSIZE][SYMBOLNUMBER]={
  {30,75,75,90,90,90,90,90,90,90},
  {30,90,90,120,105,105,105,105,105,105},
  {45,105,105,150,90,110,120,135,110,135},
  {45,120,120,150,105,165,135,160,125,150},
  {60,150,150,210,135,195,165,195,165,195},
  {90,180,195,270,180,270,230,270,195,270}
};


/* To get the bounding rectangle of a symbol 
  in fact just rect[3] is really used 
  **/


static void boundingboxM(BCG *Xgc,char *string, int x, int y, int *rect)
{
  int font[2];
  xget_font(Xgc,font);
  rect[0]= (int)(x+bsizeXfig_[font[1]][0]*((double) prec_fact));
  rect[1]= (int)(y+bsizeXfig_[font[1]][1]*((double) prec_fact));
  rect[2]= (int)(symb_xw[Xgc->CurHardSymbSize][Xgc->CurHardSymb]);
  rect[3]= (int)(symb_yh[Xgc->CurHardSymbSize][Xgc->CurHardSymb]);
}



/* Draw a single line in current style **/
/* Unused in fact **/ 

/* static void drawline(int *x1, int *yy1, int *x2, int *y2) */
/* { */
/*   FPRINTF((file,"# %d %d %d %d L\n",(int)*x1,(int)*yy1,(int)*x2,(int)*y2)); */
/* } */

/* Draw a set of segments **/
/* segments are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1]) **/
/* for i=0 step 2 **/
/*   if iflag == 1 style[i] gives the style for each segment
      if iflag == 0 (if *style >0 ) it   gives the style for all the  segment 
                    (if *style <0 ) The default style is used for all the  segment 
**/

static void drawsegments(BCG *Xgc,int *vx, int *vy, int n, int *style, int iflag)
{
  int NDvalue,i;
  int l_style,style_val,pen_color,fill_color,areafill;
  int Dvalue1;
  /* store the current values */
  int dash,color;
  xget_dash_and_color(Xgc,&dash,&color);
  for ( i =0 ; i < n/2 ; i++)
    {
      if ( (int) iflag == 0) 
	NDvalue=(*style < 0) ? color : *style;
      else
	NDvalue=(int) style[i];
      /* in case of min(max()) **/
      fig_set_color(Xgc,NDvalue,&pen_color);
      Dvalue1= xget_dash(Xgc);
      xset_dash(Xgc,Dvalue1);
      areafill = -1;
      fill_color = WHITE;
      FPRINTF((file,"# Object : %d %s -<%d>- \n", (int)i,"segments", Xgc->CurPattern));
      FPRINTF((file,"2 1 %d %d %d %d 0 0 -1 %d.000 0 0 0 0 0 2\n",
	      l_style,
	      Xgc->CurLineWidth*prec_fact/16,
	      pen_color,fill_color,
	      style_val
	      ));
      FPRINTF((file," %d %d %d %d \n",
	      (int)vx[2*i], (int)vy[2*i], (int)vx[2*i+1], (int)vy[2*i+1]));
    }
}

/* Draw a set of arrows 
  if iflag == 1 style[i] gives the style for each arrow 
  if iflag == 0 *style   gives the style for all the arrows
**/

static void drawarrows(BCG *Xgc,int *vx, int *vy, int n, int as, int *style, int iflag)
{
  int i;
  int l_style,style_val,pen_color,fill_color,areafill;
  int NDvalue;
  /* store the current values */
  int dash,color;
  xget_dash_and_color(Xgc,&dash,&color);

  for ( i = 0 ; i < n/2 ; i++)
    {
      if ( iflag == 0) 
	NDvalue=(*style < 0) ? dash : *style;
      else
	NDvalue=(int) style[i];
      fig_set_color(Xgc,NDvalue,&pen_color);
      dash = xget_dash(Xgc);
      xset_dash(Xgc,dash);
      /* Only draws **/
      areafill = -1;
      fill_color = WHITE;
      FPRINTF((file,"# Object : %d %s -<%d>-\n", (int)i,"arrows", Xgc->CurPattern));
      FPRINTF((file,"2 1 %d %d %d %d 0 0 -1 %d.000 0 0 0 1 0 2\n",
	      l_style,
	      Xgc->CurLineWidth*prec_fact/16,
	      pen_color,fill_color,
	      style_val
	      ));
      FPRINTF((file,"    0 0 %d %d %d\n",
	      (int)(1*prec_fact/16), (int)(3*prec_fact), (int) (6*prec_fact)));
      FPRINTF((file," %d %d %d %d \n",
	      (int)vx[2*i], (int)vy[2*i], (int)vx[2*i+1], (int)vy[2*i+1]));
    }
}



/* Draw or fill a set of rectangle **/
/* rectangles are defined by (vect[i],vect[i+1],vect[i+2],vect[i+3]) **/
/* for i=0 step 4 **/
/* (*n) : number of rectangles **/
/* fillvect[*n] : specify the action to perform fill or draw  **/
/* ( see periX11.c ) **/

static void drawrectangles(BCG *Xgc,const int *vects,const int *fillvect, int n)
{
  int cpat =  xget_pattern(Xgc);
  WriteGeneric(Xgc,"drawbox",n,(int)4L,vects,vects,4*(n),(int)0L,fillvect);
  xset_pattern(Xgc,cpat);
}


/* Draw one rectangle **/

static void drawrectangle(BCG *Xgc,const int rect[])
{  
  int fvect= 0;
  drawrectangles(Xgc,rect,&fvect,1);
}

/* Draw a filled rectangle **/

static void fillrectangle(BCG *Xgc,const int rect[])
{ 
  int cpat = xget_pattern(Xgc);
  drawrectangles(Xgc,rect,&cpat,1);
}

/*----------------------------------------------------------------------------------
 * accelerated draw a set of rectangles, not implemented for Pos 
 *----------------------------------------------------------------------------------*/

static void fill_grid_rectangles(BCG *Xgc,int *x, int *y, double *z, int nx,int ny,
				  int remap,const int *colminmax,const double *zminmax)
{
  fill_grid_rectangles_gen(Xgc,x,y,z,nx,ny,remap,colminmax,zminmax);
}

/*----------------------------------------------------------------------------------
 * accelerated draw a set of rectangles, not implemented for Pos 
 *----------------------------------------------------------------------------------*/

static void fill_grid_rectangles1(BCG *Xgc,int *x, int *y, double *z, int nx,int ny,
				  int remap,const int *colminmax,const double *zminmax)
{
  fill_grid_rectangles1_gen(Xgc,x,y,z,nx,ny,remap,colminmax,zminmax);
}

/* Draw or fill a set of ellipsis or part of ellipsis **/
/* Each is defined by 6-parameters, **/
/* fillvect[*n] : specify the action <?> **/
/* caution angle=degreangle*64          **/
/* old version no more used because it allows only full ellipse */

static void fillarcs(BCG *Xgc, int *vects, int *fillvect, int n)
{
  int i, pat =  xget_pattern(Xgc);	
  for ( i=0 ; i < n ; i++) 
    {
      /* to fix the style */
      xset_pattern(Xgc,fillvect[i]);
      fillarc(Xgc,vects+6*i);
    }
  xset_pattern(Xgc,pat);
}

/* Draw a set of ellipsis or part of ellipsis **/
/* Each is defined by 6-parameters, **/
/* ellipsis i is specified by $vect[6*i+k]_{k=0,5}= x,y,width,height,angle1,angle2$ **/
/* <x,y,width,height> is the bounding box **/
/* angle1,angle2 specifies the portion of the ellipsis **/
/* caution : angle=degreangle*64          **/

/* Old definition no more used because it allows only full ellipse */


static void drawarcs(BCG *Xgc, int *vects, int *style, int n)
{
  int dash,color, i;
  xget_dash_and_color(Xgc,&dash,&color);
  for ( i=0 ; i < n ; i++) 
    {
      xset_line_style(Xgc,style[i]);
      drawarc(Xgc,vects+6*i);
    }
  xset_dash_and_color(Xgc,dash,color);
}

/* Draw a single ellipsis or part of it **/
/* caution angle=degreAngle*64          **/

/*  Old definition no more used  because it allows only full ellipse */

void drawarc_old(BCG *Xgc,int arc[])
{ 
  int fvect;
  /* fvect set to tell that we only want to draw not to fill  */
  fvect = Xgc->IDLastPattern + 2  ;
  fillarcs(Xgc,arc,&fvect,1);
}


static void drawarc(BCG *Xgc,int arc[])
{ 
  int vx[365],vy[365],k,n;
  float alpha,fact=0.01745329251994330,w,h;
  int close = 0;
  w = arc[2]/2.0;
  h = arc[3]/2.0;
  n = Min((arc[5]/64),360);
  for (k = 0; k < n; ++k) {
    alpha=(( arc[4]/64)+k)*fact;
    vx[k] = arc[0] + w*(cos(alpha)+1.0);
    vy[k] = arc[1] + h*(-sin(alpha)+1.0);
  }
  drawpolyline(Xgc,vx, vy,n, close);
}


/* Fill a single elipsis or part of it **/
/* with current pattern **/

/* Old definition commented out because it allows only full ellipse */

void fillarc_old(BCG *Xgc, int arc[])
{ 
  int cpat= xget_pattern(Xgc);
  fillarcs(Xgc,arc,&cpat,1);
}

static void fillarc( BCG *Xgc,int arc[])
{ 
  int vx[365],vy[365],k,k0,kmax,n;
  float alpha,fact=0.01745329251994330,w,h;
  int close = 1;

  w = arc[2]/2.0;
  h = arc[3]/2.0;
  n = Min((arc[5]/64),360);

  k0 = 0;
  kmax = n-1;

  if (n != 360) 
    {
      vx[0] = arc[0] + w;
      vy[0] = arc[1] + h;
      k0 = 1;
      kmax = n;
    }
  for (k = k0; k <= kmax; ++k) {
    alpha=(( arc[4]/64)+k)*fact;
    vx[k] = arc[0] + w*(cos(alpha)+1.0);
    vy[k] = arc[1] * h*(-sin(alpha)+1.0);}
  if (n != 360) 
    {
      n++;
      vx[n] = arc[0] + w;
      vy[n] = arc[1] + h;
      n++;
    }
  fillpolyline(Xgc,vx, vy,n,close);
}

/*--------------------------------------------------------------
 * Draw a set of n polylines (each of which have (p) points) 
 * with lines or marks 
 * drawvect[i] >= use a mark for polyline i 
 * drawvect[i] < 0 use a line style for polyline i
 *--------------------------------------------------------------*/

static void drawpolylines(BCG *Xgc, int *vectsx, int *vectsy, int *drawvect, int n, int p)
{
  int symb[2],i, dash,color;
  xget_mark(Xgc,symb);
  xget_dash_and_color(Xgc,&dash,&color);
  for (i=0 ; i< n ; i++)
    {
      if (drawvect[i] <= 0)
	{ 
	  /* using mark */
	  xset_mark(Xgc,- drawvect[i],symb[1]);
          xset_dash(Xgc,dash);
	  drawpolymark(Xgc,vectsx+(p)*i,vectsy+(p)*i,p);
	}
      else 
	{/* using a dash style  **/
	  xset_line_style(Xgc,drawvect[i]);
	  drawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p, 0);
	}
    }
  /* back to default values **/
  xset_dash_and_color(Xgc,dash,color);
  xset_mark(Xgc,symb[0],symb[1]);
}

/* fill a set of polygons each of which is defined by **/
/* (*p) points (*n) is the number of polygons **/
/* the polygon is closed by the routine **/
/*  if fillvect[i] == 0 draw the boundaries with current color 
    if fillvect[i] > 0  draw the boundaries with current color 
    then fill with pattern fillvect[i]
    if fillvect[i] < 0  fill with pattern - fillvect[i]
*/

static void fillpolylines(BCG *Xgc, int *vectsx, int *vectsy, int *fillvect, int n, int p)
{
  int cpat;
  if ( Xgc->CurVectorStyle !=  CoordModeOrigin) FPRINTF((file,"#/absolu false def\n"));
  cpat = xget_pattern(Xgc);
  WriteGeneric(Xgc,"drawpoly",n,(p)*2,vectsx,vectsy,(p)*(n),(int)1L,fillvect);
  xset_pattern(Xgc,cpat);
  FPRINTF((file,"#/absolu true def\n"));
}

/* Only draw one polygon with current line style **/
/* according to *closeflag : it's a polyline or a polygon **/
/* XXXXXX To be done Closeflag is not used **/

static void drawpolyline( BCG *Xgc, int *vx, int *vy, int n,int closeflag)
{ 
  int i=1,fvect=0;
  if (closeflag == 1 )
    FPRINTF((file,"#/closeflag true def\n"));
  else 
    FPRINTF((file,"#/closeflag false def\n"));
  if (Xgc->ClipRegionSet ==1 )
    {
      /* FIXME 
	 analyze_points(Xgc,n, vx, vy, closeflag);
      */
      fillpolylines(Xgc,vx,vy,&fvect,i,n);
    }
  else 
    {
      fillpolylines(Xgc,vx,vy,&fvect,i,n);
    }
}

/* Fill the polygon **/

static void fillpolyline( BCG *Xgc, int *vx, int *vy,int n, int closeflag)
{
  int i =1,  cpat = - xget_pattern(Xgc);
  /* just fill  ==> cpat < 0 **/
  fillpolylines(Xgc,vx,vy,&cpat,i,n);
}


/* Draw a set of  current mark centred at points defined **/
/* by vx and vy (vx[i],vy[i]) **/

static void drawpolymark( BCG *Xgc,int *vx, int *vy,int n)
{
  int keepid,keepsize,  i=1, sz=Xgc->CurHardSymbSize;
  keepid =  Xgc->fontId;
  keepsize= Xgc->fontSize;
  xset_font(Xgc,i,sz);
  displaysymbols(Xgc,vx,vy,n);
  xset_font(Xgc,keepid,keepsize);
}
 
/*-----------------------------------------------------
\encadre{Routine for initialisation}
------------------------------------------------------*/

static void initgraphic(char *string, int *num,int *wdim,int *wpdim,double *viewport_pos,int *wpos,char mode)
{ 
  char string1[256];
  static int EntryCounter = 0;
  int fnum;
  BCG *Xgc = &ScilabGCXfig; 
  Xgc->graphic_engine = &XFig_gengine ; /* the graphic engine associated to this graphic window */
  if (EntryCounter >= 1) xendgraphic(Xgc);
  strcpy(string1,string);
  file=fopen(string1,"w");
  if (file == 0) 
    {
      sciprint("Can't open file %s, I'll use stdout\r\n",string1);
      file = stdout;
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
  FileInit(Xgc);
  Xgc->CurWindow =EntryCounter;
  EntryCounter =EntryCounter +1;
}

static void FileInit(BCG *Xgc)
{
  int m,  x[2];
  xget_windowdim(Xgc,x,x+1);
  FPRINTF((file,"#FIG 3.1\nPortrait\nCenter\nInches\n1200 2\n"));
  InitScilabGCXfig(Xgc);

  /* the default_colors are the xfig default colors **/
  m = DEFAULTNUMCOLORS;
  Xgc->Numcolors = m;
  Xgc->IDLastPattern = m - 1;
  Xgc->NumForeground = m;
  Xgc->NumBackground = m + 1;
  FPRINTF((file,"0 %d #%02x%02x%02x \n",32+m,0,0,0));
  FPRINTF((file,"0 %d #%02x%02x%02x \n",32+m+1,255,255,255));
  
  FPRINTF((file,"2 2 0 0 -1 -1 0 0 -1 0.000 0 0 0 0 0 5\n"));
  FPRINTF((file," %d %d %d %d %d %d %d %d %d %d \n",
	  0,0,(int)x[0],0,(int)x[0],(int)x[1],0,(int)x[1],0,0));
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
  InitScilabGCXfig(Xgc);
}


void InitScilabGCXfig(BCG *Xgc)
{ 
  int i,j,col;
  Xgc->IDLastPattern = GREYNUMBER - 1; /* bug ?? **/
  Xgc->CurLineWidth=1 ;
  i=1;
  xset_thickness(Xgc,1);
  xset_alufunction(Xgc,"GXcopy");
  /* retirer le clipping **/
  i=j= -1;
  xset_unclip(Xgc);
  xset_dash(Xgc,0);
  xset_font(Xgc,2,1);
  xset_mark(Xgc,0,0);
  /* trac\'e absolu **/
  Xgc->CurVectorStyle = CoordModeOrigin ;
  /* initialisation des pattern dash par defaut en n&b */
  Xgc->CurColorStatus =0;
  xset_pattern(Xgc,1);
  xset_dash(Xgc,1);
  xset_hidden3d(Xgc,1);
  /* initialisation de la couleur par defaut */ 
  Xgc->Numcolors = DEFAULTNUMCOLORS;
  Xgc->CurColorStatus = 1 ;
  xset_pattern(Xgc,1);
  xset_foreground(Xgc,Xgc->NumForeground+1);
  xset_background(Xgc,Xgc->NumForeground+2);
  xset_hidden3d(Xgc,4);
  /* Choix du mode par defaut (decide dans initgraphic_ */
  getcolordef(&col);
  /*
   * we force CurColorStatus to th eopposite value of col 
   * to force usecolorFig to perform initialisations 
   **/
  Xgc->CurColorStatus = (col == 1) ? 0: 1;
  xset_usecolor(Xgc,col);
  if (col == 1) Xgc->IDLastPattern = Xgc->Numcolors - 1;
  strcpy(Xgc->CurNumberDispFormat,"%-5.2g");
}




/*------------------------------------------------------
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

static void drawaxis(BCG *Xgc, int alpha, int *nsteps, int *initpoint, double *size)
{
  int i;
  int pen_color;
  double xi,yi,xf,yf;
  double cosal,sinal;
  int dash= xget_dash(Xgc);
  xset_dash_or_color(Xgc,dash);
  FPRINTF((file,"# Begin Axis \n"));
  if ( alpha == 90 )
    {cosal = 0.0 ; sinal =1.0 ;}
  else 
   {
     if ( alpha == -90 )
       {cosal = 0.0 ; sinal = -1.0 ;}
     else 
       {
	 cosal= cos( M_PI * (alpha)/180.0);
	 sinal= sin( M_PI * (alpha)/180.0);
       }
   }
  for (i=0; i <= nsteps[0]*nsteps[1]; i++)
    { xi = initpoint[0]+i*size[0]*cosal;
      yi = initpoint[1]+i*size[0]*sinal;
      xf = xi - ( size[1]*sinal);
      yf = yi + ( size[1]*cosal);
      FPRINTF((file,"2 1 0 %d %d %d 0 0 -1 0.000 0 0 0 0 0 2\n",
	      Xgc->CurLineWidth*prec_fact/16,
	      pen_color,
	      pen_color
	      ));
      FPRINTF((file," %d %d %d %d \n",(int)xi, (int)yi,  (int)  xf, (int)yf));
    }
  for (i=0; i <= nsteps[1]; i++)
    { xi = initpoint[0]+i*nsteps[0]*size[0]*cosal;
      yi = initpoint[1]+i*nsteps[0]*size[0]*sinal;
      xf = xi - ( size[1]*size[2]*sinal);
      yf = yi + ( size[1]*size[2]*cosal);
      FPRINTF((file,"2 1 0 %d %d %d 0 0 -1 0.000 0 0 0 0 0 2\n",
	      Xgc->CurLineWidth*prec_fact/16,
	      pen_color,
	      pen_color
	      ));
      FPRINTF((file," %d %d %d %d \n", (int)xi, (int) yi, (int)xf, (int)yf));
    }
  xi = initpoint[0]; yi= initpoint[1];
  xf = initpoint[0]+ nsteps[0]*nsteps[1]*size[0]*cosal;
  yf = initpoint[1]+ nsteps[0]*nsteps[1]*size[0]*sinal;
  FPRINTF((file,"2 1 0 %d %d %d 0 0 -1 0.000 0 0 0 0 0 2\n",
	  Xgc->CurLineWidth*prec_fact/16,
	  pen_color,
	  pen_color));
  FPRINTF((file," %d %d %d %d \n",  (int)xi,  (int)yi, (int) xf, (int)yf));
  FPRINTF((file,"# End Of Axis \n"));
}


/*-----------------------------------------------------
\encadre{Display numbers z[i] at location (x[i],y[i])
  with a slope alpha[i] (see displaystring_), if flag==1
  add a box around the string.
-----------------------------------------------------*/


static void displaynumbers(BCG *Xgc,int *x, int *y, int n, int flag, double *z, double *alpha)
{ int i ;
  char buf[20];
  for (i=0 ; i< n ; i++)
    { 
      sprintf(buf,Xgc->CurNumberDispFormat,z[i]);
      displaystring(Xgc,buf,x[i],y[i],flag,alpha[i]);
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

/*
 * give the correct pattern for xfig 0=white-> 20=black 
 * from our pattern coding 0=black    Xgc->IDLastPattern=white 
 *  we use xfig as follows : 
 *  when use_color == 1 we use the 32 standard colors of xfig with 20 ( full saturation )
 *  as fill area 
 *  when use_color == 0 we use the white color + areafill from 0 to 20 to generate 
 *  shades of gray 
 * XXXXXX : xset("colormap") must be implemented 
 */


#define AREAF(x) Max(0,Min(20,(int) (20.0*((double) x) /((double) GREYNUMBER -1 ))))

/* FIXME: */

static int CheckColormap(BCG *Xgc,int *m) 
{
  return 0;
}

static void xset_pattern_or_color(BCG *Xgc,int pat, int *areafill, int *color)
{
  if (  Xgc->CurColorStatus == 1) 
    {
      int m;
      *color = pat-1 ; /* color value **/
      if (  CheckColormap(Xgc,&m) == 1) 
	{
	  /* fix the currennt color : if a colormap is set 
	    we must have an ofset of 32 **/
	  *color += 32;
	}
      else 
	{
	  /* special case for B&white in the default colormap */
	  if ( *color == 32 ) 
	    {
	      *color = DEFAULTBLACK;
	    }
	  else if ( *color == 33 ) 
	    {
	      *color = DEFAULTWHITE;
	    }
	}
      *areafill = 20 ; /* full color saturation **/
    }
  else 
    {
      *color = WHITE ; 
      *areafill = AREAF(pat-1); /* shade of gray **/
    }
}

static void fig_set_color(BCG *Xgc,int c, int *color)
{
  int m;
  if (  Xgc->CurColorStatus == 0) {
    *color=0;
    return;
  }
  *color = c-1 ; /* color value **/
  if (  CheckColormap(Xgc,&m) == 1) 
    {
      /* fix the current color : if a colormap is set 
	  we must have an ofset of 32 **/
      *color += 32;
    }
  else 
    {
      /* special case for B&white in the default colormap */
      if ( *color == 32 ) 
	{
	  *color = DEFAULTBLACK;
	}
      else if ( *color == 33 ) 
	{
	  *color = DEFAULTWHITE;
	}
    }
}

static void fig_set_dash(BCG *Xgc,int dash, int *l_style, int *style_val)
{
  int i;
  i = Max(Min(MAXDASH -1,dash-1),0);
  *l_style = DashTab[i];
  *style_val = DashTabStyle[i];
}

static void fig_set_dash_or_color(BCG *Xgc,int dash, int *l_style, int *style_val, int *color)
{
  int j;
  if (  Xgc->CurColorStatus == 1) 
    {
      j= Xgc->CurDashStyle + 1;
      fig_set_dash(Xgc,j,l_style,style_val);
      fig_set_color(Xgc,dash,color);
      *l_style = 0 ;/* solid line **/
      *style_val=0;
    }
  else 
    {
      fig_set_dash(Xgc,dash,l_style,style_val);
      *color = BLACK;
    }
}


#define PERLINE 15
/* ne pas oublier le blanc aprse %d **/
#define FORMATNUM "%d "

static void WriteGeneric(BCG *Xgc,char *string, int nobj, int sizeobj,const int *vx,const int *vy, int sizev, int flag,const int *fvect)
{ 
  int i, cpat, lg,type=1 ;
  int areafill,fill_color,pen_color,l_style,style_val;
  int dash = xget_dash(Xgc);
  cpat = xget_pattern(Xgc);
  if ( nobj==0|| sizeobj==0) return;
  if ( strcmp(string,"drawpoly")==0)
    {
      for ( i =0 ; i < nobj ; i++)
	{
	  if (fvect[i] < 0 )
	   {
	     /* only fill **/

	     xset_pattern_or_color(Xgc, - fvect[i],&areafill,&fill_color);
	     l_style = 0;
	     style_val = 0;
	     pen_color = fill_color;
	     type = 3;
	   }
	  else if (fvect[i] == 0 )
	    {
	      /* only draws th polyline **/
	      fig_set_color(Xgc,cpat,&pen_color);
	      fig_set_dash(Xgc,dash,&l_style,&style_val);
	      areafill=-1;
	      fill_color = WHITE;
	    }
	  else 
	    /* fill with pattern  and draw with current dash **/
	    { 
	      xset_pattern_or_color(Xgc,fvect[i],&areafill,&fill_color);
	      fig_set_color(Xgc,cpat,&pen_color);
	      fig_set_dash(Xgc,dash,&l_style,&style_val);
	      /*set_dash_or_color(Dvalue[0],&l_style,&style_val,&pen_color);*/
	      type=3;
	    }
	  lg=sizeobj/2;
	  FPRINTF((file,"# Object : %d %s -<pat:%d,areafill=%d,white=%d>- \n", (int)i,string,
		  (int)fvect[i],
		  (int)areafill,
		  Xgc->IDLastPattern));
	  FPRINTF((file,"2 %d %d %d %d %d 0 0 %d %d.00 0 0 -1 0 0 %d\n",
		   (int)type,l_style, Xgc->CurLineWidth*prec_fact/16,
		  pen_color,fill_color,areafill,style_val, (int)lg
		  ));
	  Write2Vect(&vx[i*lg],&vy[i*lg],lg,flag);
	}
    }
  else 
  if ( strcmp(string,"drawbox")==0)
    {
      for ( i =0 ; i < nobj ; i++)
	{
	  int deb;
	  if (fvect[i] < 0  )
	    {
	      /* Only draws the rectangle **/
	      fig_set_dash(Xgc,dash,&l_style,&style_val);
	      fig_set_color( Xgc,-fvect[i],&pen_color);
	      /*set_dash_or_color( -fvect[i],&l_style,&style_val,&pen_color);*/
	      areafill = -1;
	      fill_color = WHITE;
	    }
	  else 	  if (fvect[i] == 0  )
	    {
	      /* Only draws the rectangle **/
	      fig_set_dash(Xgc,dash,&l_style,&style_val);
	      fig_set_color(Xgc,cpat,&pen_color);
	      /*set_dash_or_color(Dvalue[0],&l_style,&style_val,&pen_color);*/
	      areafill = -1;
	      fill_color = WHITE;
	    }
	  else 
	    {
	      /* fills the rectangle **/
	      xset_pattern_or_color(Xgc,fvect[i],&areafill,&fill_color);
	      pen_color = fill_color;
	      l_style = 0;
	      style_val = 0;
	      type = 3;
	    }
	  FPRINTF((file,"# Object : %d %s -<%d>- \n", (int)i,string, (int)fvect[i]));
	  FPRINTF((file,"2 2 %d %d %d %d 0 0 %d %d.000 0 0 0 0 0 5\n",
		  l_style, Xgc->CurLineWidth*prec_fact/16,
		  pen_color,fill_color,areafill,style_val));
	  deb=i*sizeobj;
	  FPRINTF((file," %d %d %d %d %d %d %d %d %d %d \n",
		  (int)vx[deb]                , (int)vx[1+deb],
		  (int)vx[deb]+ (int)vx[2+deb], (int)vx[1+deb],
		  (int)vx[deb]+ (int)vx[2+deb], (int)vx[1+deb]+ (int)vx[3+deb],
		  (int)vx[deb]                , (int)vx[1+deb]+ (int)vx[3+deb],
		  (int)vx[deb]                , (int)vx[1+deb]));
	}
    }
  else if ( strcmp(string,"drawsegs")==0)      
    {
      /* see drawsegsXfig **/
    }
  else if ( strcmp(string,"drawarrows")==0)      
    {
      /* see drawarrowsXfig **/
    }
  else if ( strcmp(string,"drawarc")==0)      
    {
      for ( i = 0 ; i < nobj ; i++)
	{
	  if (fvect[i] > Xgc->IDLastPattern+1 )
	    {
	      /* Only draws the arc **/
	      fig_set_dash_or_color(Xgc,dash,&l_style,&style_val,&pen_color);
	      areafill = -1;
	      fill_color = WHITE;
	    }
	  else 
	    {
	      /* fills the arc **/
	      xset_pattern_or_color(Xgc,fvect[i],&areafill,&fill_color);
	      pen_color = fill_color;
	      l_style = 0;
	      style_val = 0;
	      type = 3;
	    }
	  FPRINTF((file,"# Object : %d %s -<%d>-\n", (int)i,string, (int)fvect[0]));
	  FPRINTF((file,
		  "1 2 %d %d %d %d 0 0 %d %d.000 1 0.00 %d %d %d %d %d %d %d %d \n",
		  l_style,
		  Xgc->CurLineWidth*prec_fact/16,
		  pen_color,fill_color,areafill,style_val,
		  (int)vx[6*i]+ (int)vx[6*i+2]/2, 
		  (int)vx[6*i+1]+ (int)vx[6*i+3]/2,
		  (int)vx[6*i+2]/2, (int)vx[6*i+3]/2,
		  (int)vx[6*i]+ (int)vx[6*i+2]/2, (int)vx[6*i+1],
		  (int)vx[6*i]+ (int)vx[6*i+2]/2, (int)vx[6*i+1]));
	}
    }
  else if ( strcmp(string,"Rdrawarc")==0)      
    {
      /* store the current values */
      for ( i = 0 ; i < nobj ; i++)
	{
	  /*setdash(&fvect[i],PI0,PI0,PI0);*/

	  fig_set_dash(Xgc,dash,&l_style,&style_val);
	  fig_set_color(Xgc,fvect[i],&pen_color);

	  /* in case of min(max()) **/
	  /*getdash(&verbose,Dvalue1,&Dnarg,vdouble);
	    set_dash_or_color(Dvalue1[0],&l_style,&style_val,&pen_color);*/
	  areafill = -1;
	  fill_color = WHITE;
	  FPRINTF((file,"# Object : %d %s -<%d>-\n", (int)i,string, (int)fvect[0]));
	  FPRINTF((file,
		  "1 2 %d %d %d %d 0 0 %d %d.000 1 0.00 %d %d %d %d %d %d %d %d \n",
		  l_style,
		  (int) (Xgc->CurLineWidth*prec_fact/16),
		  pen_color,fill_color,
		  areafill, 
		  style_val,
		  (int) (vx[6*i]+vx[6*i+2]/2), 
		  (int) (vx[6*i+1]+vx[6*i+3]/2),
		  (int) (vx[6*i+2]/2), (int) (vx[6*i+3]/2),
		  (int) (vx[6*i]+ vx[6*i+2]/2), (int)vx[6*i+1],
		  (int) (vx[6*i]+ vx[6*i+2]/2), (int)vx[6*i+1]));
	}
      /*setdash( Dvalue,PI0,PI0,PI0);*/
    }
  else if ( strcmp(string,"drawpolymark")==0)      
    {
      int rect[4],x=0,y=0;
      int Dvalue1 = xget_pattern(Xgc);
      fig_set_color(Xgc,Dvalue1,&pen_color);
      l_style=0;style_val=0;
      boundingboxM(Xgc,"x",x,y,rect);
      FPRINTF((file,"# Object : %d %s -<%d>- \n", (int)0,string, (int)fvect[0]));
      for ( i =0 ; i < sizev ; i++)
	{
	  int flag = 1;
	  if ( Xgc->ClipRegionSet == 1 ) 
	    {
	      if ( vx[i] > Xgc->CurClipRegion[0] 
		   +Xgc->CurClipRegion[2]
		   || vx[i] <  Xgc->CurClipRegion[0] 
		   || vy[i] > Xgc->CurClipRegion[1] 
		   +Xgc->CurClipRegion[3]
		   || vy[i] < Xgc->CurClipRegion[1] )
		flag = 0;
	    }
	  
	  /* polymarks are x-center justified sub-type =1  **/
	  if ( flag == 1) 
	  FPRINTF((file,"4 1 %d 0 0 %d %d %5.2f %d %5.2f %5.2f %d %d \\%o\\001\n",
		  pen_color,
		  32, /* Postscript font */
		  (int)isizeXfig_[Xgc->fontSize], /*prec_fact,*/
		  0.0,
		  4,  
		  (double) rect[3],
		  (double) rect[2],
		  (int)vx[i],
		  (int)vy[i] + rect[3]/2,
		  Char2Int( symb_list[Xgc->CurHardSymb])
		   ));

	}
    }
  else
    sciprint("Can't translate %s\r\n",string);
}

static void Write2Vect(const int *vx,const int *vy, int n, int flag)
{
  int i,k;
  i=0;
  while( i < n)
    {
      k=0;
      while ( k < PERLINE && i < n )
	{
	  FPRINTF((file,FORMATNUM,(int) vx[i]));
	  if (flag == 1) 
	    {FPRINTF((file,FORMATNUM,(int) vy[i]));
	      k += 2;i++;}
	  else 
	    {k++;i++;}
	}
      FPRINTF((file,"\n"));
    }
}

/*---------------------------------------------------------
 * Dealing with fonts 
 *---------------------------------------------------------*/


/* To set the current font id of font and size **/

static void xset_font(BCG *Xgc,int fontid, int fontsize)
{ 
  int i,fsiz;
  i = Min(FONTNUMBER-1,Max(fontid,0));
  fsiz = Min(FONTMAXSIZE-1,Max(fontsize,0));
  if ( FontInfoTabXfig_[i].ok !=1 )
    Scistring("\n Sorry This Font is Not available\n");
  else 
   {
     Xgc->fontId = i;
     Xgc->fontSize = fsiz;
     FPRINTF((file,"#/%s findfont %d scalefont setfont\n",
     	     FontInfoTabXfig_[i].fname,
	     (int)isizeXfig_[fsiz]*prec_fact));
   }
}

/* To get the values id and size of the current font **/

static void xget_font(BCG *Xgc, int *font)
{
  font[0]= Xgc->fontId ;
  font[1] =Xgc->fontSize ;
}

/* To set the current mark : using the symbol font of adobe **/

void xset_mark(BCG *Xgc,int number,int size)
{ 
  Xgc->CurHardSymb =  Max(Min(SYMBOLNUMBER-1,number),0);
  Xgc->CurHardSymbSize =  Max(Min(FONTMAXSIZE-1,size),0);
;}

/* To get the current mark id **/

static void xget_mark(BCG *Xgc,int *symb)
{
  symb[0] = Xgc->CurHardSymb ;
  symb[1] = Xgc->CurHardSymbSize ;
}


static void displaysymbols(BCG *Xgc, int *vx, int *vy,int n)
{
  int fvect =  Xgc->CurPattern;
  if ( Xgc->CurVectorStyle !=  CoordModeOrigin)
    FPRINTF((file,"#/absolu false def\n"));
  FPRINTF((file,"#HardMark 0 16#%x put\n",
      Char2Int( symb_list[Xgc->CurHardSymb])));
  WriteGeneric(Xgc,"drawpolymark",(int)1L,(n)*2,vx,vy,n,(int)1L,&fvect);
  FPRINTF((file,"#/absolu true def\n"));
}


/*-------------------------------------------------------
\encadre{Check if a specified family of font exist in 
Figtscript }
-------------------------------------------------------*/

static void loadfamily(char *name, int *j)
{ 
  int i ;
  for ( i = 0; i < FONTMAXSIZE ; i++)
    {
      FontsListXfig_[*j][i] = FigQueryFont(name);
    }
  if  (FontsListXfig_[*j][0] == 0 )
	  sciprint("\n unknown font family : %s\r\n",name);
  else 
    {FontInfoTabXfig_[*j].ok = 1;
     strcpy(FontInfoTabXfig_[*j].fname,name) ;}
}

/*--------------------------------------------
\encadre{always answer ok. Must be Finished}
---------------------------------------------*/

static int FigQueryFont(char *name) { return(1);}

static void queryfamily(char *name, int *j,int *v3)
{ 
  int i ;
  name[0]='\0';
  for (i=0;i<FONTNUMBER;i++) {
    strcat(name,FontInfoTabXfig_[i].fname);
    v3[i]=strlen(FontInfoTabXfig_[i].fname);
  }
  *j=FONTNUMBER;
}


