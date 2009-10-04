#ifndef NSP_PERIGEN_H 
#define NSP_PERIGEN_H 

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 *
 * 
 */

#include "nsp/plisttoken.h" /* for name */

typedef struct __BCG BCG;
typedef struct nsp_gengine Gengine;    /* drawing */

#include "scale.h" 
#include "actions.h"
#include "driver.h" 

/*---------------------------------------------------------------------------
 *  graphic engine data 
 *---------------------------------------------------------------------------*/

/* all the plot records can be cast to a plot code */

typedef struct _plot_code {
  int code; 
} plot_code ; 

typedef struct _listplot {
  /* int  window; */
  void *theplot; 
  struct _listplot   *next;
  struct _listplot   *previous;
} list_plot ;


/* structure for storing scale informations associated to a graphic window */

typedef struct wcscalelist 
{
  int    scale_flag ;                   /* zero when this is a default scale */
  int    scale_flag3d ;                 /* set to != 0 when 3d scales are active */
  int  scale_3drot_flag;                /* test */
  int    wdim[2];                       /* currend windo dim in pixels */
  double subwin_rect[4];                /* subwindow specification */
  double frect[4];                      /* bounds in the <<real>> space: xmin,ymin,xmax,ymax */
  double zfrect[2];                     /* zmin,zmax after transformation for plot3d */
  double axis[4];                       /* position of the axis rectangle */
                                        /* = [mfact_xl, mfact_xr,mfact_yu,mfact_yd]; */
  double xtics[4],ytics[4];             /* [xmin,ymin,nint] or [kmin,kmax,ar,nint]           */
  char   strflag;                       /* the default mode used by stored graphics: 
					 * supposed to be 1 or 3 or 5 
					 */
  /* XXXX : c'est redondant avec aaint et quelquefois avec frect ? */
  double Wxofset1,Wyofset1,Wscx1,Wscy1; /* ofsets and scale factor for pixel<->double transf.*/
  char logflag[2];                      /* are we using logscale */
  int WIRect1[4];                       /* frame bounds in pixel */
  int Waaint1[4];                       /* tics and subtics numbers: [xint,xsubint,yint,ysubint] */
  double m[3][3];                       /* 3d geometric transformation */
  double bbox1[6];                      /* 3d bounds */
  double c[3] ;                          /* center of 3d box */
  double alpha,theta;                   /* polar coordinates of visualization point */
  int metric3d;                         /* added by es - metric mode  for 3d -> 2d */
  double cosa,sina;                     /* test ! */
  struct wcscalelist *next;             /* points to next one */
  struct wcscalelist *prev;             /* points to previous one */
  
} window_scale_list;


extern void window_scale_delete(int win);


/*
 * a queue for storing mouse events in drivers 
 */

typedef struct _nsp_gwin_event nsp_gwin_event ;
struct _nsp_gwin_event {
  int win,x,y,ibutton,mask,motion,release;
};

typedef enum { nsp_ev_motion = 0x1 , nsp_ev_release = 0x10, nsp_ev_getkey = 0x100, nsp_ev_getmenu = 0x1000 } _nsp_ev_code;


#define MaxCB 50

typedef struct _nsp_event_queue nsp_event_queue;

struct  _nsp_event_queue {
  int in , out, size;
  nsp_gwin_event elems[MaxCB];
};

/* 
 * structure for storing data associated to a graphic window  
 */

#ifdef GUI_PRIVATE
#define BCG_PRIVATE  gui_private 
#else 
#define BCG_PRIVATE  void 
#endif 

struct __BCG 
{ 
  Gengine *graphic_engine; /* the graphic engine associated to this graphic window */
  Nsp_gc_actions *actions; /* a set of actions */
  int CurWindow ;          /* Id of window */
  int CWindowWidth ;       /* graphic window width */
  int CWindowHeight ;      /* graphic window height */
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

  int record_flag ; /* a flag to decide if graphics are recorded */
  list_plot *plots ; /* list of recorded plots */
  list_plot *last_plot; /* direct acces to last record */ 
  void *xdrs;            /* used to pass a xdr structure for saving data */
  int zrect[4];          /* rectangle to be superposed on graphic window 
			  * used for zoom 
			  */

#ifdef __cplusplus
  /* private is a reserved keyword in C++ */
  BCG_PRIVATE *private_gc ;  /* only visible when inside a specific driver */ 
#else 
  BCG_PRIVATE *private ;  /* only visible when inside a specific driver */ 
#endif 
  nsp_event_queue queue; /* a queue for storing event when outside xclick */
} ;


extern void nsp_drawpolyline_clip(BCG *Xgc,int *vx, int *vy,int n, int *clip_box , int onemore);

/* FIXME: A revoir */
#ifndef CoordModeOrigin 
#define CoordModeOrigin 0
#endif 
#ifndef CoordModePrevious 
#define CoordModePrevious 1
#endif 

void * graphic_initial_menu(int winid);

/* jpc_SGraph.c **/

extern int CheckClickQueue   (int *,int *x,int *y,int *ibut);
extern int ClearClickQueue  (int);
extern int PushClickQueue (int,int ,int y,int ibut,int m,int r);

/* jpc_Xloop.c **/

extern int C2F(ismenu) (void);
extern int C2F(getmen) (char *btn_cmd,int *lb,int *entry);
extern void MenuFixCurrentWin ( int ivalue);

extern BCG *GetWindowXgcNumber  (int i);

extern void nsp_initialize_gc( BCG *Xgc );

/* 
 * 3d bounding box 
 */

typedef struct _nsp_box_3d {
  double bbox[6];             /* {xmin,xmax,ymin,ymax,zmin,zmax} FIXME: redondant avec Xgc->scales */
  double x_r[8],y_r[8],z_r[8];/* coordinates of the bounding box in real space  */
  double x[8],y[8],z[8];      /* coordinates of the bounding box after rotation */
  int xind[6];                /* indices of convex hull */
  int ix[7],iy[7];            /* coordinates of convex hull ( point 6 == point 0 )
			       * after ratation and scaling */
  double xh[7],yh[7],zh[7];   /* coordinates of convex hull in in real space */
  int InsideU[4],InsideD[4];  /* indices of internal and external <<triedres>> */
} nsp_box_3d; 


/* scales */

/*
 * Current geometric transformation : from double to pixel 
 */


#define XScale_d(x)    ( Xgc->scales->Wscx1*((x) -Xgc->scales->frect[0]) + Xgc->scales->Wxofset1) 
#define XLogScale_d(x) ( Xgc->scales->Wscx1*(log10(x) -Xgc->scales->frect[0]) + Xgc->scales->Wxofset1)
#define YScale_d(y)    ( Xgc->scales->Wscy1*(-(y)+Xgc->scales->frect[3]) + Xgc->scales->Wyofset1)
#define YLogScale_d(y) ( Xgc->scales->Wscy1*(-log10(y)+Xgc->scales->frect[3]) + Xgc->scales->Wyofset1)

#define XScaleR_d(x,y)   ((Xgc->scales->cosa==1.0) ? ( Xgc->scales->Wscx1*((x) -Xgc->scales->frect[0]) + Xgc->scales->Wxofset1) : \
			  ( Xgc->scales->cosa*Xgc->scales->Wscx1*((x) -Xgc->scales->frect[0]) - Xgc->scales->sina*Xgc->scales->Wscy1*(-(y)+Xgc->scales->frect[3])) + Xgc->scales->Wxofset1) 
#define YScaleR_d(x,y)   ((Xgc->scales->cosa==1.0) ? ( Xgc->scales->Wscy1*(-(y)+Xgc->scales->frect[3]) + Xgc->scales->Wyofset1): \
			  ( Xgc->scales->sina*Xgc->scales->Wscx1*((x) -Xgc->scales->frect[0]) + Xgc->scales->cosa*Xgc->scales->Wscy1*(-(y)+Xgc->scales->frect[3])) + Xgc->scales->Wyofset1)


#define XScale(x)    inint( XScale_d(x) )
#define XLogScale(x) inint( XLogScale_d(x))
#define YScale(y)    inint( YScale_d(y)   )
#define YLogScale(y) inint( YLogScale_d(y))

#define XDouble2Pixel_d(x) ((Xgc->scales->logflag[0] == 'n') ? ( XScale_d(x)) : ( XLogScale_d(x)))
#define YDouble2Pixel_d(y) ((Xgc->scales->logflag[1] == 'n') ? ( YScale_d(y)) : ( YLogScale_d(y)))

#define XDouble2Pixel(x) ((Xgc->scales->logflag[0] == 'n') ? ( XScale_d(x)) : ( XLogScale_d(x)))
#define YDouble2Pixel(y) ((Xgc->scales->logflag[1] == 'n') ? ( YScale_d(y)) : ( YLogScale_d(y)))

/*
 * Current geometric transformation : from pixel to double 
 */

#define XPi2R(x)  Xgc->scales->frect[0] + (1.0/Xgc->scales->Wscx1)*((x) - Xgc->scales->Wxofset1)
#define YPi2R(y)  Xgc->scales->frect[3] - (1.0/Xgc->scales->Wscy1)*((y) - Xgc->scales->Wyofset1)
#define XPi2LogR(x)  exp10( XPi2R(x))
#define YPi2LogR(y)  exp10( YPi2R(y))
#define XPixel2Double(x)  (( Xgc->scales->logflag[0] == 'l') ? XPi2LogR(x) : XPi2R(x))
#define YPixel2Double(y)  (( Xgc->scales->logflag[1] == 'l') ? YPi2LogR(y) : YPi2R(y))

/*
 * Current geometric transformation : 3D plots 
 */

#define TRX(x1,y1,z1) ( Xgc->scales->m[0][0]*(x1-Xgc->scales->c[0]) +Xgc->scales->m[0][1]*(y1-Xgc->scales->c[1]) +Xgc->scales->m[0][2]*(z1-Xgc->scales->c[2]))
#define TRY(x1,y1,z1) ( Xgc->scales->m[1][0]*(x1-Xgc->scales->c[0]) +Xgc->scales->m[1][1]*(y1-Xgc->scales->c[1]) +Xgc->scales->m[1][2]*(z1-Xgc->scales->c[2]))
#define TRZ(x1,y1,z1) ( Xgc->scales->m[2][0]*(x1-Xgc->scales->c[0]) +Xgc->scales->m[2][1]*(y1-Xgc->scales->c[1]) +Xgc->scales->m[2][2]*(z1-Xgc->scales->c[2]))
#define GEOX(x1,y1,z1)  XScale(TRX(x1,y1,z1))
#define GEOY(x1,y1,z1)  YScale(TRY(x1,y1,z1))
#define GEOZ(x1,y1,z1)  TRZ(x1,y1,z1)

extern void show_scales( BCG *Xgc);

#ifdef WITH_GTKGLEXT 
extern void nsp_ogl_set_2dview(BCG *Xgc);
extern void nsp_ogl_set_3dview(BCG *Xgc);
extern void nsp_ogl_set_view(BCG *Xgc);
#endif

extern int nsp_enqueue(nsp_event_queue *q, nsp_gwin_event *ev);
extern nsp_gwin_event nsp_dequeue(nsp_event_queue *q);
extern nsp_gwin_event nsp_peekqueue(nsp_event_queue *q);
extern int nsp_queue_empty(nsp_event_queue *q);
extern void nsp_clear_queue(nsp_event_queue *q);
extern int window_list_check_queue(BCG *Xgc,nsp_gwin_event *ev);
extern void window_list_clear_queue(BCG *Xgc);
extern int window_list_search_from_drawing(void *win);

#endif 

