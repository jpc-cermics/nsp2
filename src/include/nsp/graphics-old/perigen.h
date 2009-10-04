#ifndef PERIGEN_BCG_OLD
#define PERIGEN_BCG_OLD 

/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 2001-2004 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

#include "nsp/plisttoken.h" /* for name */

typedef struct __BCG BCG;
typedef struct _nsp_gengine1 Gengine1; /* recording and scaling */
typedef struct nsp_gengine Gengine;    /* drawing */

#include "actions.h" 



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


extern void window_scale_delete_old(int win);


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
  int CurWindow ;          /* Id of window  */
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

/*---------------------------------------------------------------------------
 * graphic engine functions 
 *---------------------------------------------------------------------------*/

extern void nsp_drawpolyline_clip(BCG *Xgc,int *vx, int *vy,int n, int *clip_box , int onemore);

/* function implemented by drivers */

typedef void driver_fill_grid_rectangles(BCG *Xgc,const int x[],const int y[],const double z[], int nx, int ny,
					 int remap,const int *colminmax,const double *zminmax,const int *colout);
typedef void driver_fill_grid_rectangles1(BCG *Xgc,const int x[],const int y[],const double z[], int nr, int nc,
					  int remap,const int *colminmax,const double *zminmax);
typedef void driver_boundingbox( BCG *gc,char *string, int x, int y, int *rect);
typedef void driver_cleararea( BCG *gc, int x, int y, int w, int h);
typedef void driver_clearwindow( BCG *gc);
typedef void driver_displaynumbers( BCG *gc,int *x, int *y, int n, int flag, double *z, double *alpha);
typedef void driver_displaystring( BCG *gc,char *string, int x, int y, int flag, double angle);
typedef void driver_drawarc( BCG *gc, int arc[]);
typedef void driver_drawarcs( BCG *gc, int *vects, int *style, int n);
typedef void driver_drawarrows( BCG *gc,int *vx, int *vy, int n, int as, int *style, int iflag);
typedef void driver_drawaxis( BCG *gc, int alpha, int *nsteps,  int *initpoint, double *size);
typedef void driver_drawpolyline( BCG *gc,  int *vx, int *vy, int n,int closeflag);
typedef void driver_drawpolyline_clip( BCG *gc,  int *vx, int *vy, int n,int *clip_box,int closeflag);
typedef void driver_drawpolylines( BCG *gc, int *vectsx, int *vectsy, int *drawvect, int n, int p);
typedef void driver_drawpolymark( BCG *gc,  int *vx, int *vy,int n);
typedef void driver_drawrectangle( BCG *gc,const int rect[]);
typedef void driver_drawrectangles( BCG *gc,const int *vects,const int *fillvect, int n);
typedef void driver_drawsegments( BCG *gc,int *vx, int *vy, int n, int *style, int iflag);
typedef void driver_drawline(BCG *Xgc,int x1, int yy1, int x2, int y2);
typedef void driver_fillarc( BCG *gc, int arc[]);
typedef void driver_fillarcs( BCG *gc, int *vects, int *fillvect, int n);
typedef void driver_fillpolyline( BCG *gc,  int *vx, int *vy,int n, int closeflag);
typedef void driver_fillpolylines( BCG *gc, int *vectsx, int *vectsy, int *fillvect, int n, int p);
typedef void driver_fillrectangle( BCG *gc,const int rect[]);
typedef void driver_window_list_get_ids(int *Num,int ids[],int flag); 
typedef int driver_initgraphic(const char *string,int *num,int *wdim,int *wpdim,double *viewport_pos,int *wpos,char mode,void *data);
typedef void driver_loadfamily(char *name, int *j);
typedef void driver_queryfamily(char *name, int *j,int *v3);
typedef void driver_setpopupname( BCG *gc, char *x0);
typedef void driver_xclick( BCG *gc,char *str, int *ibutton,int *imask, int *x1, int *yy1, int iflag,int motion,int release,int key, int istr);
typedef void driver_xclick_any(BCG *Xgc,char *str, int *ibutton,int *imask, int *x1,int *yy1, int *iwin, int iflag,int motion,int release,int key,int istr);
typedef void driver_xend( BCG *gc);
typedef void driver_xgetmouse( BCG *gc,char *str, int *ibutton,int *imask, int *x1, int *yy1, int iflag,int motion,int release,int key);
typedef void driver_xinfo( BCG *gc,char *message,...);
typedef void driver_xpause( int sec_time,int events);
typedef void driver_xselgraphic ( BCG *gc);
typedef void driver_sedeco( int );

typedef void driver_tape_replay(BCG *gc,int winnumber);
typedef void driver_tape_clean_plots(BCG *gc,int winnumber);
typedef void driver_tape_replay_new_angles(BCG *gc,int winnumber,int *iflag, int *flag,double *theta, double *alpha, double *bbox);
typedef void driver_tape_replay_new_scale(BCG *gc,int winnumber, int *flag, int *aaint,  double *bbox, int *ibbox);
typedef void driver_tape_replay_undo_scale(BCG *gc,int winnumber);
typedef int  driver_tape_check_recorded_3D(BCG *gc,int winnumber);

typedef void driver_xget_windowpos(BCG *gc,int *x,int *y);
typedef void driver_xset_windowpos(BCG *gc,int x, int y);
typedef void driver_xget_windowdim(BCG *gc,int *x, int *y);
typedef void driver_xset_windowdim(BCG *gc,int x, int y);
typedef void driver_xget_popupdim(BCG *gc,int *x, int *y);
typedef void driver_xset_popupdim(BCG *gc,int x, int y);
typedef void driver_xget_viewport(BCG *gc,int *x, int *y);
typedef void driver_xset_viewport(BCG *gc,int x, int y);
typedef int  driver_xset_curwin(int intnum,int set_menu);
typedef int driver_xget_curwin(void);
typedef void driver_xset_clip(BCG *gc,int x[]);
typedef void driver_xset_unclip(BCG *gc);
typedef void driver_xset_test(BCG *gc);
typedef void driver_xget_clip(BCG *gc,int *x);
typedef void driver_xset_absourel(BCG *gc,int flag);
typedef int driver_xget_absourel(BCG *gc);
typedef void driver_xset_alufunction1(BCG *gc,int num);
typedef int driver_xget_alufunction(BCG *gc);
typedef void driver_xset_thickness(BCG *gc,int value);
typedef int driver_xget_thickness(BCG *gc);
typedef int driver_xset_pattern(BCG *gc,int num);
typedef int driver_xget_pattern(BCG *gc);
typedef int driver_xget_last(BCG *gc);
typedef int  driver_xset_dash(BCG *gc,int value);
typedef void driver_xset_line_style(BCG *gc,int value);
typedef void driver_xset_dashstyle(BCG *gc,int value, int *xx, int *n);
typedef int  driver_xget_dash(BCG *gc);
typedef void driver_xset_usecolor(BCG *gc,int num);
typedef int driver_xget_usecolor(BCG *gc);
typedef void driver_xset_pixmapOn(BCG *gc,int num);
typedef int driver_xget_pixmapOn(BCG *gc);
typedef void driver_xset_wresize(BCG *gc,int num);
typedef int driver_xget_wresize(BCG *gc);
typedef void driver_xset_colormap(BCG *gc,int m,int n , double *a);
typedef void driver_xset_default_colormap(BCG *gc);
typedef void driver_xget_colormap(BCG *gc, int *num,  double *val,int color_id);
typedef void driver_xset_background(BCG *gc,int num);
typedef int driver_xget_background(BCG *gc);
typedef void driver_xset_foreground(BCG *gc,int num);
typedef int driver_xget_foreground(BCG *gc);
typedef void driver_xset_hidden3d(BCG *gc,int num);
typedef int driver_xget_hidden3d(BCG *gc);
typedef void driver_xset_mark(BCG *gc,int number, int size);
typedef void driver_xget_mark(BCG *gc,int *mark);
typedef void driver_xset_font(BCG *gc,int fontid, int fontsize);
typedef void driver_xget_font(BCG *gc,int *font);
typedef void driver_xset_autoclear(BCG *gc,int num);
typedef void driver_xset_autoclear_def(BCG *gc);
typedef int  driver_xget_autoclear(BCG *gcvoid);
typedef char *driver_xget_fpf(BCG *gc);
typedef void driver_xset_fpf(BCG *gc,char *fmt) ;
typedef void driver_xset_fpf_def(BCG *gc) ;
typedef void driver_xset_pixmapclear(BCG *gc);
typedef void driver_xset_show(BCG *gc);
typedef void driver_xset_default(BCG *gc);
typedef void driver_pixmap_resize(BCG *gc);
typedef int driver_xget_recording(BCG *Xgc);
typedef void driver_xset_recording(BCG *Xgc, int val);
typedef void driver_xset_win_protect(BCG *Xgc,int val);
typedef void driver_delete_window(BCG *gc,int wid);/* delete window: use gc is non null or wid as window number */
typedef void driver_force_redraw(BCG *gc);

typedef void driver_draw_pixbuf(BCG *Xgc,void *pix,int src_x,int src_y,int dest_x,int dest_y,int width,int height);
typedef void driver_draw_pixbuf_from_file(BCG *Xgc,const char *fname,int src_x,int src_y,int dest_x,int dest_y,int width,int height);

#ifdef PERI_PRIVATE

/* forward definitions */

static driver_fill_grid_rectangles1 fill_grid_rectangles1;
static driver_fill_grid_rectangles fill_grid_rectangles;
static driver_boundingbox boundingbox;
static driver_cleararea cleararea;
static driver_clearwindow clearwindow;
static driver_displaynumbers displaynumbers;
static driver_displaystring displaystring;
static driver_drawarc drawarc;
static driver_drawarcs drawarcs;
static driver_drawarrows drawarrows;
static driver_drawaxis drawaxis;
static driver_drawpolyline drawpolyline;
static driver_drawpolylines drawpolylines;
static driver_drawpolymark drawpolymark;
static driver_drawrectangle drawrectangle;
static driver_drawrectangles drawrectangles;
static driver_drawsegments drawsegments;
static driver_drawline drawline;
static driver_fillarc fillarc;
static driver_fillarcs fillarcs;
static driver_fillpolyline fillpolyline;
static driver_fillpolylines fillpolylines;
static driver_fillrectangle fillrectangle;
static driver_initgraphic initgraphic;
static driver_loadfamily loadfamily;
static driver_queryfamily queryfamily;
static driver_setpopupname setpopupname;
static driver_xclick xclick;
static driver_xclick_any xclick_any;
static driver_xend xend;
static driver_xgetmouse xgetmouse;
static driver_xinfo xinfo;
static driver_xpause xpause;
static driver_xselgraphic  xselgraphic ;
static driver_sedeco sedeco;
static driver_xget_windowpos xget_windowpos;
static driver_xset_windowpos xset_windowpos;
static driver_xget_windowdim xget_windowdim;
static driver_xset_windowdim xset_windowdim;
static driver_xget_popupdim xget_popupdim;
static driver_xset_popupdim xset_popupdim;
static driver_xget_viewport xget_viewport;
static driver_xset_viewport xset_viewport;
static driver_xset_curwin xset_curwin;
static driver_xget_curwin xget_curwin;
static driver_xset_clip xset_clip;
static driver_xset_unclip xset_unclip;
static driver_xset_test xset_test;
static driver_xget_clip xget_clip;
static driver_xset_absourel xset_absourel;
static driver_xget_absourel xget_absourel;
static driver_xset_alufunction1 xset_alufunction1;
static driver_xget_alufunction xget_alufunction;
static driver_xset_thickness xset_thickness;
static driver_xget_thickness xget_thickness;
static driver_xset_pattern xset_pattern;
static driver_xget_pattern xget_pattern;
static driver_xget_last xget_last;
static driver_xset_dash xset_dash;
static driver_xset_line_style xset_line_style;
static driver_xset_dashstyle xset_dashstyle;
static driver_xget_dash xget_dash;
static driver_xset_usecolor xset_usecolor;
static driver_xget_usecolor xget_usecolor;
static driver_xset_pixmapOn xset_pixmapOn;
static driver_xget_pixmapOn xget_pixmapOn;
static driver_xset_wresize xset_wresize;
static driver_xget_wresize xget_wresize;
static driver_xset_colormap xset_colormap;
static driver_xset_default_colormap xset_default_colormap;
static driver_xget_colormap xget_colormap;
static driver_xset_background xset_background;
static driver_xget_background  xget_background;
static driver_xset_foreground xset_foreground;
static driver_xget_foreground xget_foreground;
static driver_xset_hidden3d xset_hidden3d;
static driver_xget_hidden3d xget_hidden3d;
static driver_xset_mark xset_mark;
static driver_xget_mark xget_mark;
static driver_xset_font xset_font;
static driver_xget_font  xget_font;
static driver_xset_autoclear xset_autoclear;
static driver_xset_autoclear_def xset_autoclear_def;
static driver_xget_autoclear  xget_autoclear;
static driver_xget_fpf xget_fpf;
static driver_xset_fpf xset_fpf;
static driver_xset_fpf_def xset_fpf_def;
static driver_xset_pixmapclear xset_pixmapclear;
static driver_xset_show xset_show;
static driver_xset_pixmapclear xset_pixmapclear;
static driver_xset_show xset_show;
static driver_xset_default xset_default;
static driver_pixmap_resize pixmap_resize;
static driver_xget_recording xget_recording;
static driver_xset_recording xset_recording;
static driver_delete_window delete_window;
static driver_xset_win_protect xset_win_protect;
static driver_force_redraw force_redraw;
static driver_draw_pixbuf draw_pixbuf;
static driver_draw_pixbuf_from_file draw_pixbuf_from_file;

#endif 

/*---------------------------------------------------------------------------
 * graphic engine functions stored in structure 
 *---------------------------------------------------------------------------*/

/* a set of generic functions which can be used or 
 * not by each driver 
 */

typedef struct _nsp_gengine_generic {
  driver_fill_grid_rectangles *fill_grid_rectangles;
  driver_fill_grid_rectangles1 *fill_grid_rectangles1 ;
  driver_drawarrows *drawarrows;
  driver_drawsegments *drawsegments;
  driver_drawrectangles *drawrectangles;
  driver_drawarcs *drawarcs;
  driver_fillarcs *fillarcs;
  driver_drawpolylines *drawpolylines;
  driver_fillpolylines *fillpolylines;
  driver_displaynumbers *displaynumbers;
  driver_drawaxis *drawaxis;
  driver_drawarc *drawarc;
  driver_fillarc *fillarc;
  driver_draw_pixbuf *draw_pixbuf;
  driver_draw_pixbuf_from_file *draw_pixbuf_from_file;
  driver_xset_test *xset_test;

} nsp_gengine_generic ;

extern nsp_gengine_generic nsp_peri_generic_old; 

struct nsp_gengine {
  nsp_gengine_generic *generic; /* A set of genric functions hidden here */
  char *name;
  int id;
  Gengine1 *scale;  /* */
  driver_fill_grid_rectangles *fill_grid_rectangles;
  driver_fill_grid_rectangles1 *fill_grid_rectangles1 ;
  driver_boundingbox *boundingbox;
  driver_cleararea *cleararea;
  driver_clearwindow *clearwindow;
  driver_displaynumbers *displaynumbers;
  driver_displaystring *displaystring;
  driver_drawarc *drawarc;
  driver_drawarcs *drawarcs;
  driver_drawarrows *drawarrows;
  driver_drawaxis *drawaxis;
  driver_drawpolyline *drawpolyline;
  driver_drawpolyline_clip *drawpolyline_clip;
  driver_drawpolylines *drawpolylines;
  driver_drawpolymark *drawpolymark;
  driver_drawrectangle *drawrectangle;
  driver_drawrectangles *drawrectangles;
  driver_drawsegments *drawsegments;
  driver_drawline *drawline;
  driver_fillarc *fillarc;
  driver_fillarcs *fillarcs;
  driver_fillpolyline *fillpolyline;
  driver_fillpolylines *fillpolylines;
  driver_fillrectangle *fillrectangle;
  driver_window_list_get_ids *window_list_get_ids;
  driver_initgraphic *initgraphic;
  driver_loadfamily *loadfamily;
  driver_queryfamily *queryfamily;
  driver_setpopupname *setpopupname;
  driver_xclick *xclick;
  driver_xclick_any *xclick_any;
  driver_xend *xend;
  driver_xgetmouse *xgetmouse;
  driver_xinfo *xinfo;
  driver_xpause *xpause;
  driver_xselgraphic  *xselgraphic ;
  driver_sedeco *sedeco;
  driver_tape_replay *tape_replay;
  driver_tape_clean_plots *tape_clean_plots;
  driver_tape_replay_new_angles *tape_replay_new_angles;
  driver_tape_replay_new_scale *tape_replay_new_scale;
  driver_tape_replay_undo_scale *tape_replay_undo_scale;
  driver_tape_check_recorded_3D *tape_check_recorded_3D;
  driver_xget_windowpos *xget_windowpos;
  driver_xset_windowpos *xset_windowpos;
  driver_xget_windowdim *xget_windowdim;
  driver_xset_windowdim *xset_windowdim;
  driver_xget_popupdim *xget_popupdim;
  driver_xset_popupdim *xset_popupdim;
  driver_xget_viewport *xget_viewport;
  driver_xset_viewport *xset_viewport;
  driver_xset_curwin *xset_curwin;
  driver_xget_curwin *xget_curwin;
  driver_xset_clip *xset_clip;
  driver_xset_unclip *xset_unclip;
  driver_xset_test *xset_test;
  driver_xget_clip *xget_clip;
  driver_xset_absourel *xset_absourel;
  driver_xget_absourel *xget_absourel;
  driver_xset_alufunction1 *xset_alufunction1;
  driver_xget_alufunction *xget_alufunction;
  driver_xset_thickness *xset_thickness;
  driver_xget_thickness *xget_thickness;
  driver_xset_pattern *xset_pattern;
  driver_xget_pattern *xget_pattern;
  driver_xget_last *xget_last;
  driver_xset_dash *xset_dash;
  driver_xset_line_style *xset_line_style;
  driver_xset_dashstyle *xset_dashstyle;
  driver_xget_dash *xget_dash;
  driver_xset_usecolor *xset_usecolor;
  driver_xget_usecolor *xget_usecolor;
  driver_xset_pixmapOn *xset_pixmapOn;
  driver_xget_pixmapOn *xget_pixmapOn;
  driver_xset_wresize *xset_wresize;
  driver_xget_wresize *xget_wresize;
  driver_xset_colormap *xset_colormap;
  driver_xset_default_colormap *xset_default_colormap;
  driver_xget_colormap *xget_colormap;
  driver_xset_background *xset_background;
  driver_xget_background * xget_background;
  driver_xset_foreground *xset_foreground;
  driver_xget_foreground *xget_foreground;
  driver_xset_hidden3d *xset_hidden3d;
  driver_xget_hidden3d *xget_hidden3d;
  driver_xset_mark *xset_mark;
  driver_xget_mark *xget_mark;
  driver_xset_font *xset_font;
  driver_xget_font * xget_font;
  driver_xset_autoclear *xset_autoclear;
  driver_xset_autoclear_def *xset_autoclear_def;
  driver_xget_autoclear *xget_autoclear;
  driver_xget_fpf *xget_fpf;
  driver_xset_fpf *xset_fpf;
  driver_xset_fpf_def *xset_fpf_def;
  driver_xset_pixmapclear *xset_pixmapclear;
  driver_xset_show *xset_show;
  driver_xset_default *xset_default;
  driver_pixmap_resize *pixmap_resize;
  driver_xget_recording *xget_recording;
  driver_xset_recording *xset_recording;
  driver_xset_win_protect *xset_win_protect;
  driver_delete_window *delete_window;
  driver_force_redraw *force_redraw;
  driver_draw_pixbuf *draw_pixbuf;
  driver_draw_pixbuf_from_file *draw_pixbuf_from_file;
};



/* FIXME: A revoir */
#ifndef CoordModeOrigin 
#define CoordModeOrigin 0
#endif 
#ifndef CoordModePrevious 
#define CoordModePrevious 1
#endif 

void * graphic_initial_menu(int winid);

/** jpc_SGraph.c **/

extern int CheckClickQueue   (int *,int *x,int *y,int *ibut);
extern int ClearClickQueue  (int);
extern int PushClickQueue (int,int ,int y,int ibut,int m,int r);

/** jpc_Xloop.c **/

extern int C2F(ismenu) (void);
extern int C2F(getmen) (char *btn_cmd,int *lb,int *entry);
extern void MenuFixCurrentWin ( int ivalue);

extern BCG *GetWindowXgcNumber  (int i);

extern void nsp_initialize_gc_old( BCG *Xgc );

/* 
 * 3d bounding box 
 */

typedef struct _nsp_box_3d {
  double bbox[6];                       /* {xmin,xmax,ymin,ymax,zmin,zmax} FIXME: redondant avec Xgc->scales */
  double x_r[8],y_r[8],z_r[8];          /* coordinates of the bounding box in real space  */
  double x[8],y[8],z[8];                /* coordinates of the bounding box after rotation */
  int xind[6];                          /* indices of convex hull */
  int ix[7],iy[7];                      /* coordinates of convex hull ( point 6 == point 0 )
					 * after ratation and scaling */
  double xh[7],yh[7],zh[7];             /* coordinates of convex hull in in real space */
  int InsideU[4],InsideD[4];            /* indices of internal and external <<triedres>> */
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

void show_scales_old( BCG *Xgc);


/*
 * Shared by all graphic engines 
 * The following functions perform scale changes and eventually record of graphic data. 
 * the have the same name as their driver conterpart 
 */

typedef void driver_s_drawarc(BCG *Xgc,double arc[]);
typedef void driver_s_fillarcs(BCG *Xgc,double vects[],int fillvect[], int n);
typedef void driver_s_drawarcs(BCG *Xgc,double vects[], int style[], int n);
typedef void driver_s_fillpolyline(BCG *Xgc,double vx[], double vy[],int n,int closeflag);
typedef void driver_s_drawarrows(BCG *Xgc,double vx[],double vy[],int n,double as, int style[], int iflag);
typedef void driver_s_drawaxis(BCG *Xgc,double *alpha, int *nsteps, double *initpoint, double *size);
typedef void driver_s_cleararea(BCG *Xgc,double x, double y, double w, double h);
typedef void driver_s_xclick(BCG *Xgc,char *str,int *ibutton,int *imask, double *x, double *y, int iflag,int motion,int release,int key, int istr);
typedef void driver_s_xclick_any(BCG *Xgc,char *str, int *ibutton,int *imask, double *x1,double *yy1, int *iwin, int iflag,int motion,int release,int key,int istr);
typedef void driver_s_xgetmouse(BCG *Xgc,char *str, int *ibutton, int *imask,double *x, double *y, int iflag,int motion,int release,int key);
typedef void driver_s_fillarc(BCG *Xgc, double arc[]);
typedef void driver_s_fillrectangle(BCG *Xgc,double rect[]);
typedef void driver_s_drawpolyline(BCG *Xgc, double *vx, double *vy ,int n, int closeflag);
typedef void driver_s_drawpolyline_clip(BCG *Xgc, double *vx, double *vy ,int n,double *clip_r, int closeflag);
typedef void driver_s_fillpolylines(BCG *Xgc, double *vx, double *vy, int *fillvect, int n, int p,int v1);
typedef void driver_s_drawpolymark(BCG *Xgc,double *vx, double *vy,int n);
typedef void driver_s_displaynumbers(BCG *Xgc,double *x, double *y,int n, int flag,double *z, double *alpha);
typedef void driver_s_drawpolylines(BCG *Xgc,double *vx, double *vy, int *drawvect,int n, int p);
typedef void driver_s_drawrectangle(BCG *Xgc,double rect[]);
typedef void driver_s_drawrectangles(BCG *Xgc,double vects[],int fillvect[], int n);
typedef void driver_s_drawsegments(BCG *Xgc,double *vx, double *vy,int n, int *style, int iflag);
typedef void driver_s_displaystring(BCG *Xgc,char *string,double x, double y,int flag, double angle);
typedef void driver_s_displaystringa(BCG *Xgc,char *string, int ipos);
typedef void driver_s_boundingbox(BCG *Xgc,char *string, double x, double y, double *rect);
typedef void driver_s_xstringb(BCG *Xgc,char *str,int *fflag, double *xd, double *yd, double *wd, double *hd);
typedef void driver_s_xset_clipping_p(BCG *Xgc,double x,double y,double w,double h);
typedef void driver_s_xset_clipgrf(BCG *Xgc);
typedef void driver_s_xset_alufunction1(BCG *Xgc,int val);
typedef void driver_s_xset_background(BCG *Xgc,int val);
typedef void driver_s_xset_unclip(BCG *Xgc);
typedef void driver_s_xset_test(BCG *Xgc);
typedef void driver_s_xset_clip(BCG *Xgc,double x[]);
typedef void driver_s_xset_pattern(BCG *Xgc,int val);
typedef void driver_s_xset_colormap(BCG *Xgc,int m, double val[]);
typedef void driver_s_xset_default_colormap(BCG *Xgc);
typedef void driver_s_xset_default(BCG *Xgc) ;
typedef void driver_s_xset_font_size(BCG *Xgc,int val);
typedef void driver_s_xset_font(BCG *Xgc,int val,int val1);
typedef void driver_s_xset_foreground(BCG *Xgc,int val);
typedef void driver_s_xset_hidden3d(BCG *Xgc,int val);
typedef void driver_s_xset_absourel(BCG *Xgc,int val);
typedef void driver_s_xset_dash(BCG *Xgc,int val);
typedef void driver_s_xset_mark_size(BCG *Xgc,int val);
typedef void driver_s_xset_mark(BCG *Xgc,int val,int val1);
typedef void driver_s_xset_pixmapOn(BCG *Xgc,int val);
typedef void driver_s_xset_thickness(BCG *Xgc,int val);
typedef void driver_s_xset_usecolor(BCG *Xgc,int val);
typedef void driver_s_xset_viewport(BCG *Xgc,int val,int val1);
typedef void driver_s_xset_windowdim(BCG *Xgc,int val,int val1);
typedef void driver_s_xset_popupdim(BCG *Xgc,int val,int val1);
typedef void driver_s_xset_windowpos(BCG *Xgc,int val,int val1);
typedef void driver_s_xset_wresize(BCG *Xgc,int val);
typedef void driver_s_xset_autoclear(BCG *Xgc,int num);
typedef void driver_s_xset_autoclear_def(BCG *Xgc);
typedef void driver_s_xset_fpf(BCG *Xgc,char *fmt) ;
typedef void driver_s_xset_fpf_def(BCG *Xgc) ;
typedef void driver_s_xset_show(BCG *Xgc);
typedef void driver_s_xset_pixmapclear(BCG *Xgc);
typedef void driver_s_initialize_gc(BCG *Xgc);
typedef void driver_s_draw_pixbuf(BCG *Xgc,void *pix,int src_x,int src_y,double dest_x,
				  double dest_y,double width,double height);
typedef void driver_s_draw_pixbuf_from_file(BCG *Xgc,const char *pix,int src_x,int src_y,double dest_x,
					    double dest_y,double width,double height);


struct _nsp_gengine1 {
  driver_s_drawarc *drawarc;
  driver_s_fillarcs *fillarcs;
  driver_s_drawarcs *drawarcs;
  driver_s_fillpolyline *fillpolyline;
  driver_s_drawarrows *drawarrows;
  driver_s_drawaxis *drawaxis;
  driver_s_cleararea *cleararea;
  driver_s_xclick *xclick;
  driver_s_xclick_any *xclick_any;
  driver_s_xgetmouse *xgetmouse;
  driver_s_fillarc *fillarc;
  driver_s_fillrectangle *fillrectangle;
  driver_s_drawpolyline *drawpolyline;
  driver_s_drawpolyline_clip *drawpolyline_clip;
  driver_s_fillpolylines *fillpolylines;
  driver_s_drawpolymark *drawpolymark;
  driver_s_displaynumbers *displaynumbers;
  driver_s_drawpolylines *drawpolylines;
  driver_s_drawrectangle *drawrectangle;
  driver_s_drawrectangles *drawrectangles;
  driver_s_drawsegments *drawsegments;
  driver_s_displaystring *displaystring;
  driver_s_displaystringa *displaystringa;
  driver_s_boundingbox *boundingbox;
  driver_s_xstringb *xstringb;
  driver_s_xset_clipping_p *xset_clipping_p;
  driver_s_xset_clipgrf *xset_clipgrf;
  driver_s_xset_alufunction1 *xset_alufunction1;
  driver_s_xset_background *xset_background;
  driver_s_xset_unclip *xset_unclip;
  driver_s_xset_test *xset_test;
  driver_s_xset_clip *xset_clip;
  driver_s_xset_pattern *xset_pattern;
  driver_s_xset_colormap *xset_colormap;
  driver_s_xset_default_colormap *xset_default_colormap;
  driver_s_xset_default *xset_default;
  driver_s_xset_font_size *xset_font_size;
  driver_s_xset_font *xset_font;
  driver_s_xset_foreground *xset_foreground;
  driver_s_xset_hidden3d *xset_hidden3d;
  driver_s_xset_absourel *xset_absourel;
  driver_s_xset_dash *xset_dash;
  driver_s_xset_mark_size *xset_mark_size;
  driver_s_xset_mark *xset_mark;
  driver_s_xset_pixmapOn *xset_pixmapOn;
  driver_s_xset_thickness *xset_thickness;
  driver_s_xset_usecolor *xset_usecolor;
  driver_s_xset_viewport *xset_viewport;
  driver_s_xset_windowdim *xset_windowdim;
  driver_s_xset_popupdim *xset_popupdim;
  driver_s_xset_windowpos *xset_windowpos;
  driver_s_xset_wresize *xset_wresize;
  driver_s_xset_autoclear *xset_autoclear;
  driver_s_xset_autoclear_def *xset_autoclear_def;
  driver_s_xset_fpf *xset_fpf;
  driver_s_xset_fpf_def *xset_fpf_def;
  driver_s_xset_show *xset_show;
  driver_s_xset_pixmapclear *xset_pixmapclear;
  driver_s_initialize_gc *initialize_gc;
  driver_s_draw_pixbuf *draw_pixbuf;
  driver_s_draw_pixbuf_from_file *draw_pixbuf_from_file;
};

#ifdef WITH_GTKGLEXT 
void nsp_ogl_set_old_2dview(BCG *Xgc);
void nsp_ogl_set_old_3dview(BCG *Xgc);
void nsp_ogl_set_old_view(BCG *Xgc);
#endif



extern int nsp_enqueue_old(nsp_event_queue *q, nsp_gwin_event *ev);
extern nsp_gwin_event nsp_dequeue_old(nsp_event_queue *q);
extern nsp_gwin_event nsp_peekqueue_old(nsp_event_queue *q);
extern int nsp_queue_empty_old(nsp_event_queue *q);
extern void nsp_clear_queue_old(nsp_event_queue *q);
extern int window_list_check_queue_old(BCG *Xgc,nsp_gwin_event *ev);
extern void window_list_clear_queue_old(BCG *Xgc);
extern int window_list_search_old_from_drawing(void *win);



#endif 

