#ifndef __NSP_PERIGTK 
#define __NSP_PERIGTK 

/*-----------------------------------------------------------------
 * private data for the gtk widget 
 *-----------------------------------------------------------------*/

typedef struct s_menu_entry menu_entry;

struct s_menu_entry { 
  char *name; /* name of the entry */
  char *accel;/* name of accelerator */
  int status; /* active 1, not active 0, deleted -1 */
  int nsub  ; /* if menu_entry is a sub_menu nsub gives its number else its value is one */
  menu_entry *subs; /* menu_entry has some submenus */
  int winid ; /* associated window */
  int action_type ; /* hard coded or macro */
  char *fname;/* name of function to be called*/
  menu_entry *menu; /* point to the father for sub_menus */
  menu_entry *next; /* next one .... */
} ;


typedef struct  _gtk_data {
  guchar *Red;   /* vector of red value: between 0 and 255 */
  guchar *Green; /* vector of green value: between 0 and 255 */
  guchar *Blue;  /* vector of blue value: between 0 and 255 */
  gint bg;				/* Background */
  gint fg;                              /* Foreground */
  GtkWidget *window;			/* Graphics frame */
  GtkWidget *drawing;                   /* Drawable window */
  GtkWidget *scrolled;                  /* scrolled window */
  GtkWidget *CinfoW ;                   /* info widget */
  GtkWidget *vbox;                      /* vbox */
  GtkWidget *menubar;
  GtkItemFactory *item_factory;
  menu_entry *menu_entries;            /* */
  GdkPixmap *pixmap;                    /* Backing store */
  GdkDrawable *Cdrawable;               /* set to drawing->window or to a pixmap 
					 * when using animation mode 
					 * See CurPixmapStatus */
  GdkGC *wgc;
  GdkGC *stdgc;
  GdkColor gcol_bg; 
  GdkColor gcol_fg; 
  GdkRectangle clip;
  GdkCursor *gcursor;                   /* xclick cursor */ 
  GdkCursor *ccursor;                   /* standard cursor */
  GdkFont *font;
  int resize;				/* Window resized */
} gui_private ;


/*-----------------------------------------------------------------
 * mix with generic data 
 *-----------------------------------------------------------------*/

#define  GUI_PRIVATE
#include "Graphics.h" 

#ifdef PERI_PRIVATE

/*-----------------------------------------------------------------
 * private functions for gtk driver 
 *-----------------------------------------------------------------*/

Gengine Gtk_gengine = {
  "X11",
  0,
  fill_grid_rectangles ,
  fill_grid_rectangles1 ,
  boundingbox,
  cleararea,
  clearwindow,
  displaynumbers,
  displaystring,
  drawarc,
  drawarcs,
  drawarrows,
  drawaxis,
  drawpolyline,
  drawpolylines,
  drawpolymark,
  drawrectangle,
  drawrectangles,
  drawsegments,
  fillarc,
  fillarcs,
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

  tape_replay,
  tape_clean_plots,
  tape_replay_new_angles,
  tape_replay_new_scale,
  tape_replay_undo_scale,
  tape_check_recorded_3D,

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
  xset_alufunction1,
  xget_alufunction,
  xset_thickness,
  xget_thickness,
  xset_pattern,
  xget_pattern,
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
  pixmap_resize

};

#else 

/*-----------------------------------------------------------------
 * exported Gtk graphic engine 
 *-----------------------------------------------------------------*/

extern Gengine Gtk_gengine ;

#endif 

#endif 

