#ifndef __NSP_PERIFIG 
#define __NSP_PERIFIG 

/*-----------------------------------------------------------------
 * private data for the Fig renderer 
 *-----------------------------------------------------------------*/

typedef void gui_private ;

#if defined(THINK_C) || defined (__MWERKS__)|| defined(WIN32)
#define CoordModePrevious 0
#define CoordModeOrigin 1
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
#else
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#endif

/*-----------------------------------------------------------------
 * mix with generic data 
 *-----------------------------------------------------------------*/

#define  GUI_PRIVATE
#include "Graphics.h" 

#ifdef PERI_PRIVATE

extern Gengine1 nsp_gengine1 ;
/*-----------------------------------------------------------------
 * private functions for gtk driver 
 *-----------------------------------------------------------------*/

Gengine XFig_gengine = {
  &nsp_peri_generic, 
  "Fig",
  0,
  &nsp_gengine1,
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
  nsp_drawpolyline_clip,
  drawpolylines,
  drawpolymark,
  drawrectangle,
  drawrectangles,
  drawsegments,
  drawline,
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
  xset_test,
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
  force_redraw,

  draw_pixbuf,
  draw_pixbuf_from_file

};

#else 

/*-----------------------------------------------------------------
 * exported Gtk graphic engine 
 *-----------------------------------------------------------------*/

extern Gengine XFig_gengine ;

#endif 

#endif 

