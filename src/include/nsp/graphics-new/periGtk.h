#ifndef __NSP_PERIGTK
#define __NSP_PERIGTK

/*
 * common to cairo, gtk and opengl
 */

#ifdef PERICAIRO
#include <cairo.h>
#define WITH_PANGO
#endif /* PERICAIRO */

#if defined(PERIGL) || defined(PERIGTK3GL)
#ifdef WIN32
#include <windows.h>
#endif

#if ! defined(PERIGTK3GL)
#include <gtk/gtkgl.h>
#endif

/* should be checked in configure */
#define HAVE_FREETYPE
#ifdef  HAVE_FREETYPE
#ifndef PANGO_DISABLE_DEPRECATED
#define PANGO_DISABLE_DEPRECATED
#endif
/* #undef PANGO_DISABLE_DEPRECATED */
#include <pango/pangoft2.h>
#endif

#ifdef WITH_EPOXY
#include <epoxy/gl.h>
#ifndef WIN32
#include <epoxy/glx.h>
#endif
#else
#ifdef __APPLE__
#   include <OpenGL/gl.h>
#   include <OpenGL/glu.h>
#else
#   include <GL/gl.h>
#   include <GL/glu.h>
#endif
#endif

#include <gtk/gtk.h>
#if ! defined(PERIGTK3GL)
#include <gtk/gtkgl.h>
#endif

#define TAILLE_CHAR 16
#define TRANSLATE_CHAR 10
#define ECHELLE_CHAR 0.6

/* 
#define bool char
#define true 1
#define false 0
*/

/*
 * Frustum (see the Red Book http://www.opengl.org/documentation/red_book_1.0)
 */

#define INIT_DISTANCE_CLIPPING_LOIN 2000.0
#define INIT_DISTANCE_CLIPPING_PROCHE 0.01

/*
 * texture TGA pour creer des fontes
 */

typedef struct
{
  GLubyte	*imageData;
  GLuint	bpp;
  GLuint	width;
  GLuint	height;
  GLuint	texID;
} TextureImage;

typedef struct
{
  float x;
  float y;
  float z;
} t_vector3d;

/*
 * utile uniquement avec  VIEW_3D
 */

typedef struct
{
  t_vector3d position ;
  t_vector3d cible ;
  t_vector3d orientation ;
  double pt_near;
  double pt_far;
  double xmin,xmax,ymin,ymax;
} t_camera;

#endif /* PERIGL  */

/*
 * private data for the gtk widget
 */

typedef struct _GTK_locator_info GTK_locator_info;

struct _GTK_locator_info {
  guint win, x,y, ok;
  int getrelease,getmotion,getmen,getkey, button, mask ;
  int sci_click_activated; /* TRUE when we are in a xclick or xclick_any function */
  guint timer;
  char *str;
  int  lstr;
};

typedef struct s_menu_entry menu_entry;

struct s_menu_entry
{
  char *name; /* name of the entry */
  char *accel;/* name of accelerator */
  int status; /* active 1, not active 0, deleted -1 */
  int nsub  ; /* if menu_entry is a sub_menu nsub gives its number else its value is one */
  menu_entry *subs; /* menu_entry has some submenus */
  int winid ; /* associated window */
  int action_type ; /* hard coded or macro */
  char *fname;/* name of function to be called*/
  char *stock_name;/* stock name to use */
  menu_entry *menu; /* point to the father for sub_menus */
  menu_entry *next; /* next one .... */
  GtkWidget *widget; /* associated widget */
  void *gc ; /* associated graphic context when graphic window */
} ;


typedef struct  _gtk_data {
  void     *a_colors;                   /* an array of colors in a NspMatrix  */
  GQueue   *q_colors;                   /* an queue for storing colors        */
#ifdef KEEP_GTK2   
  GdkColormap *colormap ;               /* used to keep track of drawing colormap */
#endif
  GtkWidget *window;			/* Graphics frame */
  GtkWidget *drawing;                   /* gtk drawing area used for drawing */
  GtkWidget *scrolled;                  /* scrolled window which contains the drawing area*/
  GtkWidget *CinfoW ;                   /* info widget */
  GtkWidget *vbox;                      /* vbox */
  GtkWidget *menubar;
  menu_entry *menu_entries;             /* */
#ifdef KEEP_GTK2   
  GdkPixmap *pixmap;                    /* backing store pixmap */
  GdkPixmap *extra_pixmap;              /* extra backing store pixmap used when pixmap mode is on  */
  GdkDrawable *drawable;                /* can be set to drawing->window, pixmap or extra_pixmap */
  GdkGC *wgc;
  GdkGC *stdgc;
#else 
#if defined(PERIGL) || defined(PERIGTK3GL)
  void *pixmap;              /* backing store surface */
  void *extra_pixmap;        /* extra backing store pixmap used when pixmap mode is on  */
  void *drawable;            /* can be set to pixmap or extra_pixmap */
#else
  cairo_surface_t *pixmap;              /* backing store surface */
  cairo_surface_t *extra_pixmap;        /* extra backing store pixmap used when pixmap mode is on  */
  cairo_surface_t *drawable;            /* can be set to pixmap or extra_pixmap */
#endif

#endif
  GdkColor gcol_bg;
  GdkColor gcol_fg;
  GdkRectangle clip;
  GdkCursor *gcursor;                   /* xclick cursor */
  GdkCursor *ccursor;                   /* standard cursor */
  GdkCursor *extra_cursor;              /* extra cursor */
#ifdef KEEP_GTK2    
  GdkFont *font;
#endif
  int resize;				/* Window resized */
  int draw_init;			/* true when first time in draw callback */
  int zzin_expose;                      /* we are in an expose_event action*/
  int draw;                             /* when true need to redraw  */
  int protect;                          /* when true window cannot be deleted */
  PangoLayout *layout;
  PangoLayout *mark_layout;
  PangoContext *context;
  PangoContext *ft2_context;
  PangoFontDescription *desc;
  PangoFontDescription *mark_desc;
#ifdef PERI_PRIVATE_CAIRO
  GtkWidget *cairo_drawing;             /* Drawable window for cairo */
  cairo_t *cairo_pixmap_cr;             /* used to draw on the private surface pixmap */
  cairo_t *cairo_extra_pixmap_cr;       /* used to draw on the private surface pixmap */
  cairo_t *cairo_drawable_cr;           /* set to one of the two previous */
#endif
#if defined(PERIGL) || defined(PERIGTK3GL)
  int gdk_only;                         /* when true only gdk draw  */
  int gl_only;                          /* when true only gl draw  */
  TextureImage  tab_textures_font[2];   /* caracters as textures */
  GLuint  fonte_encours;
  GLuint  tab_base[2];
  GLuint  base_encours;
  t_camera camera;                      /*   opengl camera */
#if defined(PERIGL)
  GdkGLContext *glcontext ;
  GdkGLDrawable *gldrawable;
#else
  void *glcontext ;
  void *gldrawable;
#endif /* defined(PERIGL) */
#endif /* defined(PERIGL) || defined(PERIGTK3GL) */
  GdkRectangle invalidated;             /* used for expose_event */
  int          configured;              /* used to wait for configure event */
  cairo_rectangle_int_t invalidated_r;  /* filled in the nsp_drawing_invalidate_handler */
  cairo_region_t *region;
} gui_private ;


extern void menu_entry_delete(menu_entry *me);

/*-----------------------------------------------------------------
 * mix with generic data
 *-----------------------------------------------------------------*/

#define  GUI_PRIVATE
#include "Graphics.h"

#ifdef PERI_PRIVATE

extern Gengine1 nsp_gengine1 ;

/*-----------------------------------------------------------------
 * private functions for drivers
 *-----------------------------------------------------------------*/

#if  defined(PERICAIRO) && !  defined(PERIGTK3GL)
#define xx__gengine Cairo_gengine
#endif

#if defined(PERIGL) || defined(PERIGTK3GL)
#define xx__gengine GL_gengine
#endif

Gengine xx__gengine = {
  &nsp_peri_generic,
  "X11",
  0,
  &nsp_gengine1,
  boundingbox,
  cleararea,
  clearwindow,
  /* displaynumbers, */
  displaystring,
  drawarc,
  drawarrows,
  drawpolyline,
  nsp_drawpolyline_clip,
  drawpolymark,
  drawrectangle,
  drawsegments,
  drawline,
  fillarc,
  fillpolyline,
  fillpolylines,
  fillrectangle,
  window_list_get_ids ,
  initgraphic,
  loadfamily,
  queryfamily,
  setpopupname,
  xclick,
  xclick_any,
  xend,
  xgetmouse,
  xinfo,
  xpause,
  xselgraphic ,
  sedeco,

  xget_windowpos,
  xset_windowpos,
  xget_windowdim,
  xset_windowdim,
  xget_popupdim,
  xset_popupdim,
  xget_viewport,
  xset_viewport,
  xset_curwin,
  xget_curwin,
  xset_clip,
  xset_unclip,
  xget_clip,
  xset_absourel,
  xget_absourel,
  xset_thickness,
  xget_thickness,
  xset_color,
  xget_color,
  xget_last,
  xset_dash,
  xset_line_style,
  xset_dashstyle,
  xget_dash,
  xset_usecolor,
  xget_usecolor,
  xset_pixmapOn,
  xget_pixmapOn,
  xset_wresize,
  xget_wresize,
  xset_colormap,
  xset_default_colormap,
  xget_colormap,
  xset_background,
  xget_background,
  xset_foreground,
  xget_foreground,
  xset_hidden3d,
  xget_hidden3d,
  xset_mark,
  xget_mark,
  xset_font,
  xget_font,
  xset_autoclear,
  xset_autoclear_def,
  xget_autoclear,
  xget_fpf,
  xset_fpf,
  xset_fpf_def,

  xset_pixmapclear,
  xset_show,
  xset_default,
  pixmap_resize,

  xget_recording,
  xset_recording,
  xset_win_protect,
  delete_window,
  invalidate,
  process_updates,

  draw_pixbuf,
  draw_pixbuf_from_file,

  xpush_colormap,
  xpop_colormap,

  xstring_pango

};

#else /* PERI_PRIVATE */

/*-----------------------------------------------------------------
 * exported Gtk graphic engine
 *-----------------------------------------------------------------*/

extern Gengine Gtk_gengine ;

#endif /* PERI_PRIVATE */

extern void nsp_ogl_set_view(BCG *Xgc);
extern void create_graphic_window_menu( BCG *dd);
extern void start_sci_gtk();
extern void change_camera(BCG *Xgc,const double *val);

#endif /* __NSP_PERIGTK */

/*------------------------------------------------------------------
 * private declarations and functions
 *------------------------------------------------------------------*/

#ifdef  PERI_PRIVATE

static gint expose_event_new(GtkWidget *widget, GdkEventExpose *event, gpointer data);
static void nsp_fonts_finalize(BCG *Xgc);
static void nsp_fonts_initialize(BCG *Xgc);
static void draw_mark(BCG *Xgc,double *x, double *y);
static void pixmap_clear_rect(BCG *Xgc,int x,int y,int w,int h);
static void pixmap_clear_rect   (BCG *Xgc,int x,int y,int w,int h);
static void nsp_event_wait(BCG *Xgc,int *ibutton,int *imask, int *x1, int *yy1,int *iwin,int iflag,
		     int getmotion, int getrelease,int getkey,char *str, int lstr);
static void scig_deconnect_handlers(BCG *winxgc);
static void gtk_nsp_graphic_window(int is_top, BCG *dd, char *dsp,GtkWidget *win,GtkWidget *box,
				   int *wdim,int *wpdim,double *viewport_pos,int *wpos);

extern void create_graphic_window_menu( BCG *dd);
extern void start_sci_gtk(void);

#if defined(PERIGL) || defined(PERIGTK3GL)
static void drawpolyline3D(BCG *Xgc, float *vertex, int n,int closeflag);
static void fillpolyline3D(BCG *Xgc, float *vertex, int back_color, int n,int closeflag);
#if defined(PERIGL) 
static int nsp_set_gldrawable(BCG *Xgc,GdkPixmap *pixmap);
#endif
#ifdef LIGHTS
static void init_gl_lights(GLfloat light0_pos[4]);
#endif /* LIGHTS */
#endif /* PERIGL */

#endif /*  PERI_PRIVATE */
