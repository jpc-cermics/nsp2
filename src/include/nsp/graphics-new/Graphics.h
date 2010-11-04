#ifndef NSP_GRAPHICS_NEW_H
#define NSP_GRAPHICS_NEW_H

/*
 * This Software is GPL (Copyright ENPC 1998-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics     
 *
 * exported graphic_functions 
 *
 */

#include "nsp/machine.h"
#include "nsp/sciio.h"
#include "perigen.h"

/* Other functions */

/* FIXME  XXXXXX */ 
#define  sciprint Sciprintf
#define  sciprint_nd Sciprintf
#define  Scistring Sciprintf

/*  Actions.c */

typedef void (*Scig_deletegwin_handler) (int);
extern Scig_deletegwin_handler nsp_gr_set_deletegwin_handler (Scig_deletegwin_handler f);

typedef int (*Scig_handler) (BCG *Xgc,int winnum);
extern Scig_handler nsp_gr_set_handler ( Scig_handler f);
extern Scig_handler nsp_gr_handler;

extern int  nsp_gr_change (int ); 
extern void nsp_gr_2dzoom (int ); 
extern void nsp_gr_3drot (int ); 
extern void nsp_gr_delete (int ); 
extern void nsp_gr_erase (int ); 
extern void nsp_gr_export(const char *fname, int iwin, int color,const char *driver,char option,
			  int figure_export);
extern void nsp_gr_expose (int ); 
extern void nsp_gr_loadsg (int ,char *); 
extern void nsp_gr_raise (int ); 
extern void nsp_gr_replay (int ); 
extern void nsp_gr_resize (int ); 
extern void nsp_gr_resize_pixmap (int ); 
extern void nsp_gr_savesg(char *filename, int win_num);
extern void nsp_gr_sel (int ); 
extern void nsp_gr_tops (int ,int colored,char *bufname,char *driver,char option);
extern void nsp_gr_unzoom (int ); 

/* windows.c */

extern BCG *check_graphic_window(void);
extern BCG *check_graphic_window_new(void);
extern BCG * window_list_get_first();
extern BCG *window_list_search(int winnum);
extern BCG *window_list_search_new(int winnum);
extern BCG *set_graphic_window(int num) ;
extern BCG *set_graphic_window_new(int num) ;
extern int Nsetscale2d(BCG *Xgc,double WRect[4],double ARect[4],double FRect[4],
		       char *logscale);
extern int getscale2d(BCG *Xgc,double WRect[4],double FRect[4],char *logscale,
		      double ARect[4]);
extern void window_list_get_ids(int *Num, int *Ids, int flag);
extern int window_list_get_max_id(void);
extern BCG *window_list_new(void *privated);
extern BCG * window_list_win_to_front(int win);
extern void window_list_remove(int num);
extern int window_list_search_toplevel(void *win);
extern int nsp_get_win_counter(void);
extern void nsp_set_win_counter(int n);
extern void frame_clip_on(BCG *Xgc);
extern void frame_clip_off(BCG *Xgc);
extern void set_scale(nsp_gcscale *scale,const int wdim[2],const double subwin[4],
		      const double frame_values[4], const int aaint[4],const char logflag[2],
		      const double axis_values[4]);
extern int xgc_add_default_scale(BCG *Xgc);
extern int xgc_reset_scales_to_default(BCG *Xgc);
extern int move_subwindow_scale_to_front(BCG *Xgc,const double *subwin);
extern int setscale2d(BCG *Xgc,double WRect[4],double FRect[4],char *logscale);

/* periXXX */

extern void nsp_gr_set_graphic_eventhandler(int *win_num,char *name,int *ierr);
extern void nsp_set_cursor(BCG *Xgc,int id);
extern void getcolordef  (int *);
extern int nsp_new_graphics(void);
extern void clip_line  (int,int,int ,int,int *,int *,int *,int *,int *, int xleft,int xright,int ybot,int ytop); 

/* Alloc.c */

void *graphic_alloc (int indice,int n,unsigned int size);
void graphic_alloc_info (void);
void graphic_alloc_free (void);

/* Axes.c */

extern void nsp_axis_old (BCG *Xgc,char,char,double *,int *,double *,int*,
			  char *str[],int subtics,char *format,
			  int fontsize,int textcolor,int ticscolor,
			  char logflag,int seg_flag ,int grid_color);
extern void axis_draw  (BCG *Xgc, char mode, char scale, int grid_color);
extern void nsp_grformat_e  (char *fmt,double xmin,double xmax,double xpas); 
extern void nsp_grformat_e1  (char *fmt,double *x,int nx); 

/* Champ.c */

/* Contour.c */

extern int nsp_get_level_curves (double **x,double **y,int *mm,int *n);
extern int nsp_gcontour  (BCG *Xgc,double *,double *,double *,int *,int *,int *,int *,double *,double *,
			 double *,char *,int *,double *,double *,int); 
extern int nsp_contour2  (BCG *Xgc,double *,double *,double *,int *,
			  int *,int *,int *,double *,int *,char *,
			  char *,double *,int *); 
extern int nsp_contour_if  (BCG *Xgc,double *,double *,double *,int *,
			    int *,int *,int *,double *,int *); 

extern int nsp_contour2d_draw(BCG *Xgc,double *x, double *y, double *z,
			      int n1, int n2, int nz,  double *zz, int *style);


/* Math.c */

extern double Mini  (const double vect[],int);
extern double Maxi  (const double vect[],int);

/* Plo2d.c */

extern int nsp_plot2d(BCG *Xgc,double x[],double y[],int *n1,int *n2,
		      int style[],char *strflag,const char *legend,
		      int leg_pos,double brect[],int aaint[]);
extern int nsp_plot2d_1  (BCG *Xgc,char *,double *,double *,int *,int *,
			  int *,char *,const char *,int,double *,int *); 
extern int nsp_plot2d_2  (BCG *Xgc,char *,double *,double *,int *,int *,
			  int *,char *,const char *,int,double *,int *); 
extern int nsp_plot2d_3  (BCG *Xgc,char *,double *,double *,int *,int *,
			  int *,char *,const char *,int,double *,int *); 
extern int nsp_plot2d_4  (BCG *Xgc,char *,double *,double *,int *,int *,
			  int *,char *,const char *,int,double *,int *); 
extern int nsp_plot_grid  (BCG *Xgc,int *); 
extern int CheckxfParam  (char *); 

typedef enum { legend_dl, legend_dr ,legend_drm, legend_ul,legend_ur,legend_urm } legends_pos;
extern void nsp_legends(BCG *Xgc,legends_pos pos,int n1,const int *style,const char * legend,const char *sep);

/* Plo2dEch.c */

extern int graduate  (const double *,const double *,double *,double *,int *,int *,int *,
		      int *,int *); 
extern void update_frame_bounds(BCG *Xgc, int cflag, char *xf, double *x,
				double *y, int *n1, int *n2, int *aaint,
				char *strflag,double FRect[4]);
extern void ellipse2d(nsp_gcscale *,double *,int *,int *,char *); 
extern void axis2d  (nsp_gcscale *scales,double *,double *,double *,int *,double *); 
extern void rect2d_f2i(nsp_gcscale *scales,const double x[],int x1[], int n);
extern void scale_i2f(nsp_gcscale *scales, double x[], double y[],const int x1[],const int y1[],int n);
extern void scale_f2i(nsp_gcscale *scales,const double x[],const double y[],int x1[],int y1[],int n);
extern void length_scale_i2f(nsp_gcscale *scales,double *x, double *y, const int *x1, const int *y1, int n);
extern void length_scale_f2i(nsp_gcscale *scales,const double *x,const double *y, int *x1, int *y1, int n);
extern void scale_f2wrect(nsp_gcscale *scales,const double x[],double x1[]);

extern void Gr_Rescale_new  (char *,double *,int *,int *,int *,int *); 

extern void zoom  (BCG *Xgc); 
extern void unzoom  (BCG *Xgc);

/* Plo3d.c */

void GetEch3d1  ( double (*m1)[3],double *,double *,double *,double *);
void GetEch3d   (void);
extern void MaxiInd  (double *,int,int *,double); 
extern void UpNext  (int ,int *,int *); 
extern void DownNext  (int ,int *,int *); 
extern void TDAxis  (BCG *Xgc,int flag,double FPval,double LPval,int *nax,int *FPoint,int *LPoint,int *Ticsdir); 
extern void C2F(TDdrawaxis)  (BCG *Xgc,double ,double FPval,double LPval,int *nax,int *FPoint,int *LPoint,int *Ticsdir); 
extern void nsp_3d_rotation (BCG *Xgc); 
extern int DPoints1  (BCG *Xgc,int *polyx,int *polyy,int *fill,int whiteid,double zmin,double zmax,double *,double *,double *,int i,int j,int jj1,int *p,int dc,int fg); 
extern int DPoints  (BCG *Xgc,int *polyx,int *polyy,int *fill,int whiteid,double zmin,double zmax,double *,double *,double *,int i,int j,int jj1,int *p,int dc,int fg); 
extern int nsp_plot3d  (BCG *Xgc,double *,double *,double *,int *p,int *q,double *teta,double *,const char *,int *,double *); 
extern void nsp_draw_3d_obj_old( BCG *Xgc,void *Lo,double *theta,double *alpha,const char *legend,
			     int *flag,double *ebox,int with_mesh,int with_box,int box_color,int box_style);
extern int nsp_plot3d_1  (BCG *Xgc,double *,double *,double *,int *p,int *q,double *teta,double *,const char *,int *,double *); 
extern int nsp_plot_fac3d  (BCG *Xgc,double *,double *,double *,int *cvect,int *p,int *q,double *teta,double *,const char *,int *,double *); 
extern int nsp_plot_fac3d_1  (BCG *Xgc,double *,double *,double *,int *cvect,int *p,int *q,double *teta,double *,const char *,int *,double *); 
extern int nsp_plot_fac3d_2  (BCG *Xgc,double *,double *,double *,int *cvect,int *p,int *q,double *teta,double *,const char *,int *,double *);
extern int nsp_plot_fac3d_3  (BCG *Xgc,double *,double *,double *,int *cvect,int *p,int *q,double *teta,double *,const char *,int *,double *);
extern int nsp_param3d  (BCG *Xgc,double *,double *,double *,int *,double *teta,double *,const char *,int *,double *); 
extern int nsp_param3d_1  (BCG *Xgc,double *,double *,double *,int *,int *,int *,int *colors,double *teta,double *,const char *,int *,double *); 
extern int nsp_geom3d_new  (BCG *Xgc,double *,double *,double *,int *n); 
extern void SetEch3d  (BCG *Xgc,double *,double *,double *,double *,double *teta,double *); 
/* extern void SetEch3d1(BCG *Xgc, nsp_box_3d *box,const double *bbox, double Teta, double Alpha, int flag); */
extern void DrawAxis(BCG *Xgc,const nsp_box_3d *box,char flag, int style);
extern void Convex_Box(BCG *Xgc, nsp_box_3d *box,const char *legend, int flag);


/* Rec.c */ 

extern void tape_new_angles_plots(BCG *Xgc,double theta,double alpha,const int *pt);
extern int  tape_check_recorded_3D (BCG *Xgc,int winnumber); 
extern void tape_clean_plots  (BCG *Xgc,int winnumber);
extern void tape_replay_undo_scale(BCG *Xgc);
extern void tape_replay_new_scale(BCG *Xgc,int winnumber, int *flag, int *aaint,
				  double *bbox,int *ibbox);
extern void tape_replay_new_angles(BCG *Xgc);
extern void tape_replay(BCG *Xgc,const GdkRectangle *rect);
extern void tape_replay_mix(BCG *Xgc,BCG *Xgc1, int winnumber);

typedef enum {plot3d_t ,facettes_t , param3d_t} nsp_plot3d_type;

extern void nsp_remap_colors(BCG *Xgc,int remap,int *colmin,int *colmax,double *zmin,
			     double *zmax,double *coeff,
			     const int *colminmax,const double *zminmax,const double z[],int zn);

extern void nsp_set_colormap_constants(BCG *Xgc,int m);

#endif 

