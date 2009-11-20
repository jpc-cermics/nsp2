#ifndef NSP_SCALE_BCG 
#define NSP_SCALE_BCG 

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 *
 * The following functions perform scale changes and then redirect 
 * to the graphics driver.
 */

typedef struct _nsp_gengine1 Gengine1; 

typedef void driver_s_boundingbox(BCG *Xgc,char *string, double x, double y, double *rect);
typedef void driver_s_cleararea(BCG *Xgc, double *r);
typedef void driver_s_displaynumbers(BCG *Xgc,double *x, double *y,int n, int flag,double *z, double *alpha);
typedef void driver_s_displaystring(BCG *Xgc,char *string,double x, double y,int flag, double angle);
typedef void driver_s_displaystringa(BCG *Xgc,char *string, int ipos);
typedef void driver_s_draw_pixbuf(BCG *Xgc,void *pix,int src_x,int src_y,double dest_x,double dest_y,double width,double height);
typedef void driver_s_draw_pixbuf_from_file(BCG *Xgc,const char *pix,int src_x,int src_y,double dest_x,double dest_y,double width,double height);
typedef void driver_s_drawarc(BCG *Xgc,double arc[]);
typedef void driver_s_drawarcs(BCG *Xgc,double vects[], int style[], int n);
typedef void driver_s_drawarrows(BCG *Xgc,double vx[],double vy[],int n,double as, int style[], int iflag);
typedef void driver_s_drawaxis(BCG *Xgc,double *alpha, int *nsteps, double *initpoint, double *size);
typedef void driver_s_drawpolyline(BCG *Xgc, double *vx, double *vy ,int n, int closeflag);
typedef void driver_s_drawpolyline_clip(BCG *Xgc, double *vx, double *vy ,int n,double *clip_r, int closeflag);
typedef void driver_s_drawpolylines(BCG *Xgc,double *vx, double *vy, int *drawvect,int n, int p);
typedef void driver_s_drawpolymark(BCG *Xgc,double *vx, double *vy,int n);
typedef void driver_s_drawrectangle(BCG *Xgc,double rect[]);
typedef void driver_s_drawrectangles(BCG *Xgc,double vects[],int fillvect[], int n);
typedef void driver_s_drawsegments(BCG *Xgc,double *vx, double *vy,int n, int *style, int iflag);
typedef void driver_s_fillarc(BCG *Xgc, double arc[]);
typedef void driver_s_fillarcs(BCG *Xgc,double vects[],int fillvect[], int n);
typedef void driver_s_fillpolyline(BCG *Xgc,double vx[], double vy[],int n,int closeflag);
typedef void driver_s_fillpolylines(BCG *Xgc, double *vx, double *vy, int *fillvect, int n, int p,int v1);
typedef void driver_s_fillrectangle(BCG *Xgc,double rect[]);
typedef void driver_s_initialize_gc(BCG *Xgc);
typedef void driver_s_xclick(BCG *Xgc,char *str,int *ibutton,int *imask, double *x, double *y, int iflag,int motion,int release,int key, int istr);
typedef void driver_s_xclick_any(BCG *Xgc,char *str, int *ibutton,int *imask, double *x1,double *yy1, int *iwin, int iflag,int motion,int release,int key,int istr);
typedef void driver_s_xgetmouse(BCG *Xgc,char *str, int *ibutton, int *imask,double *x, double *y, int iflag,int motion,int release,int key);
typedef void driver_s_xset_clip(BCG *Xgc,double x[]);
typedef void driver_s_xset_clipgrf(BCG *Xgc);
typedef void driver_s_xset_clipping_p(BCG *Xgc,double x,double y,double w,double h);
typedef void driver_s_xset_default(BCG *Xgc) ;
typedef void driver_s_xset_font_size(BCG *Xgc,int val);
typedef void driver_s_xset_mark_size(BCG *Xgc,int val);
typedef void driver_s_xstringb(BCG *Xgc,char *str,int *fflag, double *xd, double *yd, double *wd, double *hd);

struct _nsp_gengine1 {
  driver_s_boundingbox *boundingbox;
  driver_s_cleararea *cleararea;
  driver_s_displaynumbers *displaynumbers;
  driver_s_displaystring *displaystring;
  driver_s_displaystringa *displaystringa;
  driver_s_draw_pixbuf *draw_pixbuf;
  driver_s_draw_pixbuf_from_file *draw_pixbuf_from_file;
  driver_s_drawarc *drawarc;
  driver_s_drawarcs *drawarcs;
  driver_s_drawarrows *drawarrows;
  driver_s_drawaxis *drawaxis;
  driver_s_drawpolyline *drawpolyline;
  driver_s_drawpolyline_clip *drawpolyline_clip;
  driver_s_drawpolylines *drawpolylines;
  driver_s_drawpolymark *drawpolymark;
  driver_s_drawrectangle *drawrectangle;
  driver_s_drawrectangles *drawrectangles;
  driver_s_drawsegments *drawsegments;
  driver_s_fillarc *fillarc;
  driver_s_fillarcs *fillarcs;
  driver_s_fillpolyline *fillpolyline;
  driver_s_fillpolylines *zzfillpolylines; /* no more used */
  driver_s_fillrectangle *fillrectangle;
  driver_s_initialize_gc *initialize_gc;
  driver_s_xclick *xclick;
  driver_s_xclick_any *xclick_any;
  driver_s_xgetmouse *xgetmouse;
  driver_s_xset_clip *xset_clip;
  driver_s_xset_clipgrf *xset_clipgrf;
  driver_s_xset_clipping_p *xset_clipping_p;
  driver_s_xset_default *xset_default;
  driver_s_xset_font_size *xset_font_size;
  driver_s_xset_mark_size *xset_mark_size;
  driver_s_xstringb *xstringb;
};

#endif 

#ifdef PERI_SCALE_PRIVATE 

static  driver_s_boundingbox boundingbox_1;
static  driver_s_cleararea cleararea_1;
static  driver_s_displaynumbers displaynumbers_1;
static  driver_s_displaystring displaystring_1;
static  driver_s_displaystringa displaystringa_1;
static  driver_s_draw_pixbuf draw_pixbuf_1;
static  driver_s_draw_pixbuf_from_file draw_pixbuf_from_file_1;
static  driver_s_drawarc drawarc_1;
static  driver_s_drawarcs drawarcs_1;
static  driver_s_drawarrows drawarrows_1;
static  driver_s_drawaxis drawaxis_1;
static  driver_s_drawpolyline drawpolyline_1;
static  driver_s_drawpolyline_clip drawpolyline_clip_1;
static  driver_s_drawpolylines drawpolylines_1;
static  driver_s_drawpolymark drawpolymark_1;
static  driver_s_drawrectangle drawrectangle_1;
static  driver_s_drawrectangles drawrectangles_1;
static  driver_s_drawsegments drawsegments_1;
static  driver_s_fillarc fillarc_1;
static  driver_s_fillarcs fillarcs_1;
static  driver_s_fillpolyline fillpolyline_1;
static  driver_s_fillpolylines fillpolylines_1;
static  driver_s_fillrectangle fillrectangle_1;
static  driver_s_initialize_gc initialize_gc_1;
static  driver_s_xclick xclick_1;
static  driver_s_xclick_any xclick_any_1;
static  driver_s_xgetmouse xgetmouse_1;
static  driver_s_xset_clip xset_clip_1;
static  driver_s_xset_clipgrf xset_clipgrf_1;
static  driver_s_xset_clipping_p xset_clipping_p_1;
static  driver_s_xset_default xset_default_1;
static  driver_s_xset_font_size xset_font_size_1;
static  driver_s_xset_mark_size  xset_mark_size_1;
static  driver_s_xset_mark_size xset_mark_size_1;
static  driver_s_xstringb xstringb_1;

#endif 
