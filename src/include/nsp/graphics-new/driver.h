#ifndef NSP_DRIVER_BCG
#define NSP_DRIVER_BCG

/*
 * This Software is GPL (Copyright ENPC 1998-2019)
 * Jean-Philippe Chancelier Enpc/Cermics
 *
 * typedef for driver functions
 * and driver structures.
 */

#include <gdk/gdk.h> /* for GdkRectangle */

typedef enum { GR_STR_XLEFT ,GR_STR_XCENTER , GR_STR_XRIGHT} gr_str_posx;
typedef enum { GR_STR_YBOTTOM ,GR_STR_YCENTER , GR_STR_YBASELINE, GR_STR_YUP } gr_str_posy;
typedef enum { GR_in_box, GR_fill_box, GR_no_box} gr_str_box;

typedef void driver_boundingbox( BCG *gc,const char *string, int x, int y,double *rect);
typedef void driver_cleararea( BCG *gc,const GdkRectangle *r);
typedef void driver_clearwindow( BCG *gc);
typedef void driver_drawpolyline_clip( BCG *gc, double *vx, double *vy, int n, double *clip_box,int closeflag);
typedef void driver_displaystring( BCG *gc,const char *string, double x, double y, int flag, 
				   double angle, gr_str_posx posx, gr_str_posy posy);
typedef void driver_drawarc( BCG *gc, double arc[]);
typedef void driver_drawarrows( BCG *gc,double *vx, double *vy, int n, int as, int *style, int iflag);
typedef void driver_drawpolyline( BCG *gc, const double *vx,const double *vy, int n,int closeflag);
typedef void driver_drawpolymark( BCG *gc,  double *vx, double *vy,int n);
typedef void driver_drawrectangle( BCG *gc,const double rect[]);
typedef void driver_drawsegments( BCG *gc, double *vx, double *vy, int n, int *color, int *width);
typedef void driver_drawline(BCG *Xgc, double x1, double yy1, double x2, double y2);
typedef void driver_fillarc( BCG *gc, double arc[]);
typedef void driver_fillpolyline( BCG *gc,  const double *vx, const double *vy,int n, int closeflag,int stroke_color);
typedef void driver_fillpolylines( BCG *gc, const double *vectsx, const double *vectsy, int *fillvect, int n, int p);
typedef void driver_fillrectangle( BCG *gc,const double rect[]);
typedef void driver_window_list_get_ids(int *Num,int ids[],int flag);
typedef void *driver_initgraphic(const char *string,int *num,int *wdim,int *wpdim,double *viewport_pos,
				 int *wpos,char mode,void *data,void *Fig);
typedef void driver_loadfamily(char *name, int *j);
typedef void driver_queryfamily(char *name, int *j,int *v3);
typedef void driver_setpopupname( BCG *gc, char *x0);
typedef void driver_xclick( BCG *gc,char *str, int *ibutton,int *imask, int *x1, int *yy1, int iflag,
			    int motion,int release,int key, int istr);
typedef void driver_xclick_any(BCG *Xgc,char *str, int *ibutton,int *imask, int *x1,int *yy1,
			       int *iwin, int iflag,int motion,int release,int key,int istr);
typedef void driver_xend( BCG *gc);
typedef void driver_xgetmouse( BCG *gc,char *str, int *ibutton,int *imask, int *x1, int *yy1,
			       int iflag,int motion,int release,int key);
typedef void driver_xinfo( BCG *gc,char *message,...);
typedef void driver_xpause( int sec_time,int events);
typedef void driver_xselgraphic ( BCG *gc);
typedef void driver_sedeco( int );
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
typedef void driver_xset_clip(BCG *gc,const GdkRectangle *r);
typedef void driver_xset_unclip(BCG *gc);
typedef void driver_xget_clip(BCG *gc,int *x);
typedef void driver_xset_absourel(BCG *gc,int flag);
typedef int driver_xget_absourel(BCG *gc);
typedef int driver_xset_thickness(BCG *gc,int value);
typedef int driver_xget_thickness(BCG *gc);
typedef int driver_xset_color(BCG *gc,int num);
typedef int driver_xget_color(BCG *gc);
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
typedef int driver_xset_colormap(BCG *gc,void *a);
typedef int driver_xpush_colormap(BCG *gc,void *a);
typedef int driver_xpop_colormap(BCG *gc);
typedef int driver_xset_default_colormap(BCG *gc);
typedef void driver_xget_colormap(BCG *gc, int *num,  double *val,int color_id);
typedef void driver_xset_background(BCG *gc,int num);
typedef int driver_xget_background(BCG *gc);
typedef void driver_xset_foreground(BCG *gc,int num);
typedef int driver_xget_foreground(BCG *gc);
typedef void driver_xset_hidden3d(BCG *gc,int num);
typedef int driver_xget_hidden3d(BCG *gc);
typedef void driver_xset_mark(BCG *gc,int number, int size);
typedef void driver_xget_mark(BCG *gc,int *mark);
typedef void driver_xset_font(BCG *gc,int fontid, int fontsize,int full);
typedef void driver_xget_font(BCG *gc,int *font,int full);
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
typedef void driver_invalidate(BCG *gc,void *rect);
typedef void driver_process_updates(BCG *gc);
typedef void driver_draw_pixbuf(BCG *Xgc,void *pix,int src_x,int src_y,int dest_x,int dest_y,int width,int height);
typedef void driver_draw_pixbuf_from_file(BCG *Xgc,const char *fname,int src_x,int src_y,int dest_x,
					  int dest_y,int width,int height);
typedef void driver_xstring_pango(BCG *Xgc,char *str,int rect[],char *font,int size,int markup,int position);

/* a set of generic functions which can be used by each driver */

extern void nsp_drawpolyline_clip(BCG *Xgc, double *vx, double *vy, int n, double *clip_box , int onemore);

typedef struct _nsp_gengine_generic  nsp_gengine_generic;

struct _nsp_gengine_generic {
  driver_drawarrows *drawarrows;
  driver_drawsegments *drawsegments;
  driver_fillpolylines *fillpolylines;
  /* driver_displaynumbers *displaynumbers; */
  driver_drawarc *drawarc;
  driver_fillarc *fillarc;
  driver_draw_pixbuf *draw_pixbuf;
  driver_draw_pixbuf_from_file *draw_pixbuf_from_file;
};

/* external object containing generic functions */

extern nsp_gengine_generic nsp_peri_generic;

struct nsp_gengine {
  nsp_gengine_generic *generic; /* A set of generic functions hidden here */
  char *name;
  int id;
  Gengine1 *scale;  /* */
  driver_boundingbox *boundingbox;
  driver_cleararea *cleararea;
  driver_clearwindow *clearwindow;
  /* driver_displaynumbers *displaynumbers; */
  driver_displaystring *displaystring;
  driver_drawarc *drawarc;
  driver_drawarrows *drawarrows;
  driver_drawpolyline *drawpolyline;
  driver_drawpolyline_clip *drawpolyline_clip;
  driver_drawpolymark *drawpolymark;
  driver_drawrectangle *drawrectangle;
  driver_drawsegments *drawsegments;
  driver_drawline *drawline;
  driver_fillarc *fillarc;
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
  driver_xget_clip *xget_clip;
  driver_xset_absourel *xset_absourel;
  driver_xget_absourel *xget_absourel;
  driver_xset_thickness *xset_thickness;
  driver_xget_thickness *xget_thickness;
  driver_xset_color *xset_color;
  driver_xget_color *xget_color;
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
  driver_invalidate *invalidate;
  driver_process_updates *process_updates;
  driver_draw_pixbuf *draw_pixbuf;
  driver_draw_pixbuf_from_file *draw_pixbuf_from_file;
  driver_xpush_colormap *xpush_colormap;
  driver_xpop_colormap *xpop_colormap;
  driver_xstring_pango *xstring_pango;
};

#endif

#ifdef PERI_PRIVATE

/* forward definitions: function to be implemented 
 * by each driver 
 */

static driver_boundingbox boundingbox;
static driver_cleararea cleararea;
static driver_clearwindow clearwindow;
static driver_delete_window delete_window;
static driver_displaystring displaystring;
static driver_draw_pixbuf draw_pixbuf;
static driver_draw_pixbuf_from_file draw_pixbuf_from_file;
static driver_drawarc drawarc;
static driver_drawarrows drawarrows;
static driver_drawline drawline;
static driver_drawpolyline drawpolyline;
static driver_drawpolymark drawpolymark;
static driver_drawrectangle drawrectangle;
static driver_drawsegments drawsegments;
static driver_fillarc fillarc;
static driver_fillpolyline fillpolyline;
static driver_fillpolylines fillpolylines;
static driver_fillrectangle fillrectangle;
static driver_invalidate invalidate;
static driver_process_updates process_updates;
static driver_initgraphic initgraphic;
static driver_loadfamily loadfamily;
static driver_pixmap_resize pixmap_resize;
static driver_queryfamily queryfamily;
static driver_sedeco sedeco;
static driver_setpopupname setpopupname;
static driver_xclick xclick;
static driver_xclick_any xclick_any;
static driver_xend xend;
static driver_xget_absourel xget_absourel;
static driver_xget_autoclear  xget_autoclear;
static driver_xget_background  xget_background;
static driver_xget_clip xget_clip;
static driver_xget_colormap xget_colormap;
static driver_xpush_colormap xpush_colormap;
static driver_xpop_colormap xpop_colormap;
static driver_xget_curwin xget_curwin;
static driver_xget_dash xget_dash;
static driver_xget_font  xget_font;
static driver_xget_foreground xget_foreground;
static driver_xget_fpf xget_fpf;
static driver_xget_hidden3d xget_hidden3d;
static driver_xget_last xget_last;
static driver_xget_mark xget_mark;
static driver_xget_color xget_color;
static driver_xget_pixmapOn xget_pixmapOn;
static driver_xget_popupdim xget_popupdim;
static driver_xget_recording xget_recording;
static driver_xget_thickness xget_thickness;
static driver_xget_usecolor xget_usecolor;
static driver_xget_viewport xget_viewport;
static driver_xget_windowdim xget_windowdim;
static driver_xget_windowpos xget_windowpos;
static driver_xget_wresize xget_wresize;
static driver_xgetmouse xgetmouse;
static driver_xinfo xinfo;
static driver_xpause xpause;
static driver_xselgraphic  xselgraphic ;
static driver_xset_absourel xset_absourel;
static driver_xset_autoclear xset_autoclear;
static driver_xset_autoclear_def xset_autoclear_def;
static driver_xset_background xset_background;
static driver_xset_clip xset_clip;
static driver_xset_colormap xset_colormap;
static driver_xset_curwin xset_curwin;
static driver_xset_dash xset_dash;
static driver_xset_dashstyle xset_dashstyle;
static driver_xset_default xset_default;
static driver_xset_default_colormap xset_default_colormap;
static driver_xset_font xset_font;
static driver_xset_foreground xset_foreground;
static driver_xset_fpf xset_fpf;
static driver_xset_fpf_def xset_fpf_def;
static driver_xset_hidden3d xset_hidden3d;
static driver_xset_line_style xset_line_style;
static driver_xset_mark xset_mark;
static driver_xset_color xset_color;
static driver_xset_pixmapOn xset_pixmapOn;
static driver_xset_pixmapclear xset_pixmapclear;
static driver_xset_pixmapclear xset_pixmapclear;
static driver_xset_popupdim xset_popupdim;
static driver_xset_recording xset_recording;
static driver_xset_show xset_show;
static driver_xset_show xset_show;
static driver_xset_thickness xset_thickness;
static driver_xset_unclip xset_unclip;
static driver_xset_usecolor xset_usecolor;
static driver_xset_viewport xset_viewport;
static driver_xset_win_protect xset_win_protect;
static driver_xset_windowdim xset_windowdim;
static driver_xset_windowpos xset_windowpos;
static driver_xset_wresize xset_wresize;
static driver_xstring_pango xstring_pango;

#endif
