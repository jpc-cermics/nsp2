#ifndef PERIGEN_BCG 
#define PERIGEN_BCG 

/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/

#include "nsp/plisttoken.h" /* for name */

/*---------------------------------------------------------------------------
 *  shared by all graphic engines 
 *---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------
 *  graphic engine data 
 *---------------------------------------------------------------------------*/

typedef struct _nsp_gengine1 {

  void (*set_driver)(char *x0) ;
  void (*get_driver_name)(char *str);
  char (*get_driver)(void ) ;
  int  (*get_driver_id)(void );

  void (*drawarc_1)(double arc[]);
  void (*fillarcs_1)(double vects[],int fillvect[], int n);
  void (*drawarcs_1)(double vects[], int style[], int n);
  void (*fillpolyline_1)(double vx[], double vy[],int n,int closeflag);
  void (*drawarrows_1)(double vx[],double vy[],int n,double as, int style[], int iflag);
  void (*drawaxis_1)(double *alpha, int *nsteps, double *initpoint, double *size);
  void (*cleararea_1)(double x, double y, double w, double h);
  void (*xclick_1)(char *str,int *ibutton, double *x, double *y, int iflag,int motion,int release,int key, int istr);
  void (*xclick_any_1)(char *str, int *ibutton, double *x, double *y, int *iwin,int iflag,int motion,int release,int key,int istr);
  void (*xgetmouse_1)(char *str, int *ibutton, double *x, double *y, int iflag,int motion,int release,int key);
  void (*fillarc_1)( double arc[]);
  void (*fillrectangle_1)(double rect[]);
  void (*drawpolyline_1)( double *vx, double *vy ,int n, int closeflag);
  void (*fillpolylines_1)( double *vx, double *vy, int *fillvect, int n, int p,int v1);
  void (*drawpolymark_1)(double *vx, double *vy,int n);
  void (*displaynumbers_1)(double *x, double *y,int n, int flag,double *z, double *alpha);
  void (*drawpolylines_1)(double *vx, double *vy, int *drawvect,int n, int p);
  void (*drawrectangle_1)(double rect[]);
  void (*drawrectangles_1)(double vects[],int fillvect[], int n);
  void (*drawsegments_1)(double *vx, double *vy,int n, int *style, int iflag);
  void (*displaystring_1)(char *string,double x, double y,int flag, double angle);
  void (*displaystringa_1)(char *string, int ipos);
  void (*boundingbox_1)(char *string, double x, double y, double *rect);
  void (*xstringb_1)(char *str,int *fflag, double *xd, double *yd, double *wd, double *hd);


  void (*xset1_clipping_p)(double x,double y,double w,double h);
  void (*xset1_clipgrf)();
  void (*xset1_alufunction1)(int val);
  void (*xset1_background)(int val);
  void (*xset1_unclip)();
  void (*xset1_clip)(double x[]);
  void (*xset1_pattern)(int val);
  void (*xset1_colormap)(int m, double val[]);
  void (*xset1_default)(void) ;
  void (*xset1_font_size)(int val);
  void (*xset1_font)(int val,int val1);
  void (*xset1_foreground)(int val);
  void (*xset1_hidden3d)(int val);
  void (*xset1_absourel)(int val);
  void (*xset1_dash)(int val);
  void (*xset1_mark_size)(int val);
  void (*xset1_mark)(int val,int val1);
  void (*xset1_pixmapOn)(int val);
  void (*xset1_thickness)(int val);
  void (*xset1_usecolor)(int val);
  void (*xset1_viewport)(int val,int val1);
  void (*xset1_windowdim)(int val,int val1);
  void (*xset1_popupdim)(int val,int val1);
  void (*xset1_windowpos)(int val,int val1);
  void (*xset1_wresize)(int val);
  void (*xset1_autoclear)(int num);
  void (*xset1_autoclear_def)(void);
  void (*xset1_fpf)(char *fmt) ;
  void (*xset1_fpf_def)() ;

  void (*xset1_show)(void);
  void (*xset1_pixmapclear)(void);

} Gengine1;

extern Gengine1 nsp_gengine1 ;

/*---------------------------------------------------------------------------
 *  graphic engine data 
 *---------------------------------------------------------------------------*/

/* structure for storing scale informations associated to a graphic window */

typedef struct wcscalelist 
{
  int flag ;                            /* zero when this is a default scale */
  int    wdim[2];                       /* currend windo dim in pixels */
  double subwin_rect[4];                /* subwindow specification */
  double frect[4];                      /* bounds in the <<real>> space: xmin,ymin,xmax,ymax */
  double axis[4];                       /* position of the axis rectangle */
                                        /* = [mfact_xl, mfact_xr,mfact_yu,mfact_yd]; */
  double xtics[4],ytics[4];             /* [xmin,ymin,nint] or [kmin,kmax,ar,nint]           */
  /* XXXX : c'est redondant avec aaint et quelquefois avec frect ? */
  double Wxofset1,Wyofset1,Wscx1,Wscy1; /* ofsets and scale factor for pixel<->double transf.*/
  char logflag[2];                      /* are we using logscale */
  integer WIRect1[4];                   /* frame bounds in pixel */
  integer Waaint1[4];                   /* tics and subtics numbers: [xint,xsubint,yint,ysubint] */
  double m[3][3];                       /* 3d geometric transformation */
  double bbox1[6];                      /* 3d bounds */
  double alpha,theta;                   /* polar coordinates of visualization point */
  integer metric3d;                     /* added by es - metric mode
                                           for 3d -> 2d */
  struct wcscalelist *next;             /* points to next one */
  struct wcscalelist *prev;             /* points to previous one */
} window_scale_list;

extern window_scale_list current_scale; /* the current_scale */

extern void window_scale_delete(int win);

#ifdef GUI_PRIVATE
#define BCG_PRIVATE  gui_private 
#else 
#define BCG_PRIVATE  void 
#endif 

/* structure for storing data associated to a graphic window */

typedef struct BCG 
{ 
  int CurWindow ;   /** Id of window **/
  int CWindowWidth ; /** graphic window width **/
  int CWindowHeight ; /** graphic window height **/
  int fontId ;
  int fontSize ;
  int CurHardSymb;
  int CurHardSymbSize;
  int CurLineWidth;
  int CurPattern;
  int CurColor;
  int CurPixmapStatus;
  int CurResizeStatus;
  int CurVectorStyle;
  int CurDrawFunction;
  int ClipRegionSet;
  int CurClipRegion[4];
  int CurDashStyle;
  char CurNumberDispFormat[32];
  int CurColorStatus;
  int IDLastPattern; /* number of last pattern or color  in color mode = Numcolors - 1 */
  int CmapFlag ; /* set to 1 if the Cmap has default colors */
  int Numcolors; /* number of colors */
  int NumBackground;  /* number of Background in the color table */
  int NumForeground; /* number of Foreground in the color table */
  int NumHidden3d;  /* color for hidden 3d facets **/
  char EventHandler[NAME_MAXL+1]; /* name of window event handler XXXX */
  char fp_format[32]; 
  int Autoclear;

  window_scale_list *scales; /* scales associated to graphic window subwins */

  BCG_PRIVATE *private ;  /* only visible when inside gtk */ 

} BCG; 

/*---------------------------------------------------------------------------
 * graphic engine functions 
 *---------------------------------------------------------------------------*/

#ifdef PERI_PRIVATE

static void fill_grid_rectangles ( int *x, int *y, double *z, int n1, int n2);
static void fill_grid_rectangles1 ( int *x, int *y, double *z, int n1, int n2);
static void boundingbox(char *string, int x, int y, int *rect);
static void cleararea( int x, int y, int w, int h);
static void clearwindow(void);
static void displaynumbers(int *x, int *y, int n, int flag, double *z, double *alpha);
static void displaystring(char *string, int x, int y, int flag, double angle);
static void drawarc(int arc[]);
static void drawarcs( int *vects, int *style, int n);
static void drawarrows(int *vx, int *vy, int n, int as, int *style, int iflag);
static void drawaxis( int alpha, int *nsteps, int *initpoint, double *size);
static void drawpolyline(  int *vx, int *vy, int n,int closeflag);
static void drawpolylines( int *vectsx, int *vectsy, int *drawvect, int n, int p);
static void drawpolymark( int *vx, int *vy,int n);
static void drawrectangle(const int rect[]);
static void drawrectangles(const int *vects,const int *fillvect, int n);
static void drawsegments(int *vx, int *vy, int n, int *style, int iflag);
static void fillarc( int arc[]);
static void fillarcs( int *vects, int *fillvect, int n);
static void fillpolyline(  int *vx, int *vy,int n, int closeflag);
static void fillpolylines( int *vectsx, int *vectsy, int *fillvect, int n, int p);
static void fillrectangle(const int rect[]);
static void initgraphic(char *string,int *num);
static void loadfamily(char *name, int *j);
static void queryfamily(char *name, int *j,int *v3);
static void setpopupname(char *x0);
static void xclick(char *str, int *ibutton, int *x1, int *yy1, int iflag,int motion,int release, int key, int istr);
static void xclick_any(char *str, int *ibutton, int *x1, int *yy1, int *iwin,int iflag,int motion,int release,int key,int istr);
static void xend(void);
static void xgetmouse(char *str, int *ibutton, int *x1, int *yy1, int queue,int motion,int release,int key);
static void xinfo(char *message,...);
static void xpause( int sec_time);
static void xselgraphic (void);
static void sedeco(int );

static void xget_windowpos(int *x,int *y);
static void xset_windowpos(int x, int y);
static void xget_windowdim(int *x, int *y);
static void xset_windowdim(int x, int y);
static void xget_popupdim(int *x, int *y);
static void xset_popupdim(int x, int y);
static void xget_viewport(int *x, int *y);
static void xset_viewport(int x, int y);
static int xset_curwin(int intnum,int set_menu);
static int xget_curwin(void);
static void xset_clip(int x[]);
static void xset_unclip(void);
static void xget_clip(int *x);
static void xset_absourel(int flag);
static int xget_absourel(void);
static void xset_alufunction1(int num);
static int xget_alufunction(void);
static void xset_thickness(int value);
static int xget_thickness(void);
static int xset_pattern(int num);
static int xget_pattern(void);
static int xget_last(void);
static int xset_dash(int value);
static void xset_dash_or_color(int value);
static void xset_dash_and_color(int dash,int color);
static void xset_line_style(int value);
static void xset_dashstyle(int value, int *xx, int *n);
static int xget_dash_or_color(void);
static int xget_dash(void);
static void xget_dash_and_color( int *dash, int *color );
static void xset_usecolor(int num);
static int xget_usecolor(void);
static void xset_pixmapOn(int num);
static int xget_pixmapOn(void);
static void xset_wresize(int num);
static int xget_wresize(void);
static void xset_colormap(int m,int n , double *a);
static void xget_colormap( int *num,  double *val);
static void xset_background(int num);
static int  xget_background(void);
static void xset_foreground(int num);
static int xget_foreground(void);
static void xset_hidden3d(int num);
static int xget_hidden3d(void);
static void xset_mark(int number, int size);
static void xget_mark(int *mark);
static void xset_font(int fontid, int fontsize);
static void  xget_font(int *font);
static void xset_autoclear(int num);
static void xset_autoclear_def(void);
static int  xget_autoclear(void);
static char *xget_fpf(void);
static void xset_fpf(char *fmt) ;
static void xset_fpf_def() ;
static void xset_pixmapclear(void);
static void xset_show(void);

static void xset_pixmapclear(void);
static void xset_show(void);
static void xset_default(void);
static void pixmap_resize(void);

#endif 


/*---------------------------------------------------------------------------
 * graphic engine functions stored in structure 
 *---------------------------------------------------------------------------*/

typedef struct nsp_gengine {
  char *name;
  int id;

  void (*fill_grid_rectangles )( int *x, int *y, double *z, int n1, int n2);
  void (*fill_grid_rectangles1 )( int *x, int *y, double *z, int n1, int n2);
  void (*boundingbox)(char *string, int x, int y, int *rect);
  void (*cleararea)( int x, int y, int w, int h);
  void (*clearwindow)(void);
  void (*displaynumbers)(int *x, int *y, int n, int flag, double *z, double *alpha);
  void (*displaystring)(char *string, int x, int y, int flag, double angle);
  void (*drawarc)( int arc[]);
  void (*drawarcs)( int *vects, int *style, int n);
  void (*drawarrows)(int *vx, int *vy, int n, int as, int *style, int iflag);
  void (*drawaxis)( int alpha, int *nsteps,  int *initpoint, double *size);
  void (*drawpolyline)(  int *vx, int *vy, int n,int closeflag);
  void (*drawpolylines)( int *vectsx, int *vectsy, int *drawvect, int n, int p);
  void (*drawpolymark)(  int *vx, int *vy,int n);
  void (*drawrectangle)(const int rect[]);
  void (*drawrectangles)(const int *vects,const int *fillvect, int n);
  void (*drawsegments)(int *vx, int *vy, int n, int *style, int iflag);
  void (*fillarc)( int arc[]);
  void (*fillarcs)( int *vects, int *fillvect, int n);
  void (*fillpolyline)(  int *vx, int *vy,int n, int closeflag);
  void (*fillpolylines)( int *vectsx, int *vectsy, int *fillvect, int n, int p);
  void (*fillrectangle)(const int rect[]);
  void (*window_list_get_ids)(int *Num,int ids[],int flag); 
  void (*initgraphic)(char *string,int *num);
  void (*loadfamily)(char *name, int *j);
  void (*queryfamily)(char *name, int *j,int *v3);
  void (*setpopupname)(char *x0);
  void (*xclick)(char *str, int *ibutton, int *x1, int *yy1, int iflag,int motion,int release,int key, int istr);
  void (*xclick_any)(char *str, int *ibutton, int *x1, int *yy1, int *iwin,int iflag,int motion,int release,int key,int istr);
  void (*xend)(void);
  void (*xgetmouse)(char *str, int *ibutton, int *x1, int *yy1, int iflag,int motion,int release,int key);
  void (*xinfo)(char *message,...);
  void (*xpause)( int sec_time);
  void (*xselgraphic )(void);
  void (*sedeco)(int );

  void (*tape_replay) (int winnumber);
  void (*tape_clean_plots) (int winnumber);
  void (*tape_replay_new_angles) (int winnumber,int *iflag, int *flag,double *theta, double *alpha, double *bbox);
  void (*tape_replay_new_scale) (int winnumber, int *flag, int *aaint,  double *bbox);
  void (*tape_replay_undo_scale) (int winnumber);
  int  (*tape_check_recorded_3D) (int winnumber);

  void (*xget_windowpos)(int *x,int *y);
  void (*xset_windowpos)(int x, int y);
  void (*xget_windowdim)(int *x, int *y);
  void (*xset_windowdim)(int x, int y);
  void (*xget_popupdim)(int *x, int *y);
  void (*xset_popupdim)(int x, int y);
  void (*xget_viewport)(int *x, int *y);
  void (*xset_viewport)(int x, int y);
  int  (*xset_curwin)(int intnum,int set_menu);
  int (*xget_curwin)(void);
  void (*xset_clip)(int x[]);
  void (*xset_unclip)(void);
  void (*xget_clip)(int *x);
  void (*xset_absourel)(int flag);
  int (*xget_absourel)(void);
  void (*xset_alufunction1)(int num);
  int (*xget_alufunction)(void);
  void (*xset_thickness)(int value);
  int (*xget_thickness)(void);
  int (*xset_pattern)(int num);
  int (*xget_pattern)(void);
  int (*xget_last)(void);
  int  (*xset_dash)(int value);
  void (*xset_line_style)(int value);
  void (*xset_dashstyle)(int value, int *xx, int *n);
  int  (*xget_dash)(void);
  void (*xset_usecolor)(int num);
  int (*xget_usecolor)(void);
  void (*xset_pixmapOn)(int num);
  int (*xget_pixmapOn)(void);
  void (*xset_wresize)(int num);
  int (*xget_wresize)(void);
  void (*xset_colormap)(int m,int n , double *a);
  void (*xget_colormap)( int *num,  double *val);
  void (*xset_background)(int num);
  int (* xget_background)(void);
  void (*xset_foreground)(int num);
  int (*xget_foreground)(void);
  void (*xset_hidden3d)(int num);
  int (*xget_hidden3d)(void);
  void (*xset_mark)(int number, int size);
  void (*xget_mark)(int *mark);
  void (*xset_font)(int fontid, int fontsize);
  void (* xget_font)(int *font);
  void (*xset_autoclear)(int num);
  void (*xset_autoclear_def)(void);
  int  (*xget_autoclear)(void);
  char *(*xget_fpf)(void);
  void (*xset_fpf)(char *fmt) ;
  void (*xset_fpf_def)() ;
  void (*xset_pixmapclear)(void);
  void (*xset_show)(void);
  void (*xset_default)(void);
  void (*pixmap_resize)(void);

} Gengine;

extern Gengine * nsp_gengine ;

/* A revoir */

#define CoordModeOrigin 0
#define CoordModePrevious 1

void * graphic_initial_menu(int winid);

/** jpc_SGraph.c **/

extern int CheckClickQueue   (integer *,integer *x,integer *y,integer *ibut);
extern int ClearClickQueue  (integer);
extern int PushClickQueue (int,int ,int y,int ibut,int m,int r);

/** jpc_Xloop.c **/

extern integer C2F(ismenu) (void);
extern int C2F(getmen) (char *btn_cmd,integer *lb,integer *entry);
extern void MenuFixCurrentWin ( int ivalue);

extern struct BCG *GetWindowXgcNumber  (integer i);

extern void   set_no_delete_win_mode(void);
extern void   set_delete_win_mode(void);

/* scales */

extern window_scale_list current_scale;

/*
 * Current geometric transformation : from double to pixel 
 */


#define XScale_d(x)    ( current_scale.Wscx1*((x) -current_scale.frect[0]) + current_scale.Wxofset1)
#define XLogScale_d(x) ( current_scale.Wscx1*(log10(x) -current_scale.frect[0]) + current_scale.Wxofset1)
#define YScale_d(y)    ( current_scale.Wscy1*(-(y)+current_scale.frect[3]) + current_scale.Wyofset1)
#define YLogScale_d(y) ( current_scale.Wscy1*(-log10(y)+current_scale.frect[3]) + current_scale.Wyofset1)

#define XScale(x)    inint( XScale_d(x) )
#define XLogScale(x) inint( XLogScale_d(x))
#define YScale(y)    inint( YScale_d(y)   )
#define YLogScale(y) inint( YLogScale_d(y))
#define XDouble2Pixel(x) ((current_scale.logflag[0] == 'n') ? ( XScale(x)) : ( XLogScale(x)))
#define YDouble2Pixel(y) ((current_scale.logflag[1] == 'n') ? ( YScale(y)) : ( YLogScale(y)))

/*
 * Current geometric transformation : from pixel to double 
 */

#define XPi2R(x)  current_scale.frect[0] + (1.0/current_scale.Wscx1)*((x) - current_scale.Wxofset1)
#define YPi2R(y)  current_scale.frect[3] - (1.0/current_scale.Wscy1)*((y) - current_scale.Wyofset1)
#define XPi2LogR(x)  exp10( XPi2R(x))
#define YPi2LogR(y)  exp10( YPi2R(y))
#define XPixel2Double(x)  (( current_scale.logflag[0] == 'l') ? XPi2LogR(x) : XPi2R(x))
#define YPixel2Double(y)  (( current_scale.logflag[1] == 'l') ? YPi2LogR(y) : YPi2R(y))

/*
 * Current geometric transformation : 3D plots 
 */

#define TRX(x1,y1,z1) ( current_scale.m[0][0]*(x1) +current_scale.m[0][1]*(y1) +current_scale.m[0][2]*(z1))
#define TRY(x1,y1,z1) ( current_scale.m[1][0]*(x1) +current_scale.m[1][1]*(y1) +current_scale.m[1][2]*(z1))
#define TRZ(x1,y1,z1) ( current_scale.m[2][0]*(x1) +current_scale.m[2][1]*(y1) +current_scale.m[2][2]*(z1))
#define GEOX(x1,y1,z1)  XScale(TRX(x1,y1,z1))
#define GEOY(x1,y1,z1)  YScale(TRY(x1,y1,z1))

void show_scales(void);


#endif 

