/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2000 Enpc/Jean-Philippe Chancelier
 *    jpc@cereve.enpc.fr 
 --------------------------------------------------------------------------*/

#ifndef SCIG_PROTO
#define SCIG_PROTO

#include "nsp/machine.h"
#include "nsp/sciio.h"
#include "perigen.h" 

/* Other functions */

/* XXXXXX */ 

#define  sciprint Sciprintf
#define  sciprint_nd Sciprintf
#define  Scistring Sciprintf

/*Actions.c */

typedef void (*Scig_deletegwin_handler) (int);
extern Scig_deletegwin_handler set_scig_deletegwin_handler (Scig_deletegwin_handler f);

typedef int (*Scig_handler) (int);
extern Scig_handler set_scig_handler ( Scig_handler f);
extern Scig_handler scig_handler;

extern int  scig_change (int ); 
extern void scig_2dzoom (int ); 
extern void scig_3drot (int ); 
extern void scig_delete (int ); 
extern void scig_erase (int ); 
extern void scig_export(char *fname, int iwin, int color, char *driver);
extern void scig_expose (int ); 
extern void scig_loadsg (int ,char *); 
extern void scig_raise (int ); 
extern void scig_replay (int ); 
extern void scig_resize (int ); 
extern void scig_resize_pixmap (int ); 
extern void scig_savesg(char *filename, int win_num);
extern void scig_sel (int ); 
extern void scig_tops (int ,int colored,char *bufname,char *river); 
extern void scig_unzoom (int ); 

/* windows.c */

extern struct BCG *window_list_new(void *private);
extern void window_list_remove(int num);
extern BCG *window_list_search(int winnum);
extern void window_list_get_ids(int *Num, int *Ids, int flag);
extern int window_list_get_max_id(void);
struct BCG * window_list_get_first();

/* periXXX */

extern int CheckColormap  (int *m); 
extern int get_pixel  (int i); 

extern void C2F(bitmap)  (char *,int,int); 
extern void C2F(set_default_colormap)  (void); 
extern void C2F(xendgraphic)  (void); 
extern void C2F(xgetmouse_test)  (char *,int *ibutton,int *,int *,int *,int *,int *,double *,double *,double *,double *); 
extern void DeleteSGWin  (int intnum); 
extern void DeleteWindowToList  (int num); 
extern void SwitchWindow (int *intnum);
extern void clip_line  (int,int,int ,int,int *,int *,int *,int *,int *); 
extern void get_b  (int i,float *b); 
extern void get_g  (int i,float *g); 
extern void get_r  (int i,float *r); 
extern void getcolordef  (int *);
extern void set_c  (int i); 
extern void set_clip_box (int xxleft,int xxright,int yybot,int yytop); 
extern void setcolordef  (int );

/* Alloc.c */

void *graphic_alloc (int indice,int n,unsigned int size);
void graphic_alloc_info (void);
void graphic_alloc_free (void);

/* Axes.c */

extern void Sci_Axis (char,char,double *,int *,double *,int*,char *str[],int subtics,char *format,
			      int fontsize,int textcolor,int ticscolor,char logflag,int seg_flag );
extern void sci_axis (char,char,double *,int *,double *,int*,char *str[],int subtics,char *format,
			      int fontsize,int textcolor,int ticscolor,char logflag,int seg_flag);
extern void axis_draw  ( char strflag[]);
extern void ChoixFormatE  (char *fmt,double xmin,double xmax,double xpas); 
extern void ChoixFormatE1  (char *fmt,double *x,int nx); 
extern void C2F(aplot)  (int *,double *,double *,double *,double *,int *,int *,char *); 

/* Champ.c */

extern int nsp_champ  (double *,double *,double *,double *,int *,int *,char *,double *,double *,int); 
extern int nsp_champ1  (double *,double *,double *,double *,int *,int *,char *,double *,double *,int); 
	
/* Contour.c */

extern int C2F(getconts) (double **x,double **y,int *mm,int *n);
extern int C2F(contour)  (double *,double *,double *,int *,int *,int *,int *,double *,double *,double *,char *,int *,double *,double *,int); 
extern int C2F(contour2)  (double *,double *,double *,int *,int *,int *,int *,double *,int *,char *,char *,double *,int *); 
extern int C2F(contourif)  (double *,double *,double *,int *,int *,int *,int *,double *,int *); 

	/* FeC.c */

extern int C2F(fec)  (double *,double *,double *triangles,double *func,int *Nnode,int *Ntr,char *,char *,double *,int *, double *, int *); 

/* Gray.c */

extern int C2F(xgray)  (double *,double *,double *,int *,int *,char *,double *,int *,long int l1); 
extern int C2F(xgray1) (double *z,int *n1,int *n2,char *strflag,double *brect,int *aaint,long int l1);
extern int C2F(xgray2) (double *z,int *n1,int *n2,double *xrect);

/* Math.c */

extern double Mini  (double *vect,int);
extern double Maxi  (double *vect,int);

/* Plo2d.c */

extern int C2F(xgrid)  (int *); 
extern void AxisDraw  (double *,int *,int *,int *,int *,double,double,double,double,char *,char *); 
extern void FrameBounds  (char *,double *,double *,int *,int *,int *,char *,double *,double *,int *,int *); 
extern void Legends  ( int *,int *,char *); 

extern int C2F(plot2d) (double *,double *,int *,int *,int *,char *,char *,double *,int *);
extern int CheckxfParam  (char *); 
extern int C2F(plot2d1)  (char *,double *,double *,int *,int *,int *,char *,char *,double *,int *); 
extern int C2F(plot2d2)  (char *,double *,double *,int *,int *,int *,char *,char *,double *,int *); 
extern int C2F(plot2d3)  (char *,double *,double *,int *,int *,int *,char *,char *,double *,int *); 
extern int C2F(plot2d4)  (char *,double *,double *,int *,int *,int *,char *,char *,double *,int *); 

/* Plo2dEch.c */

extern void update_frame_bounds  (int cflag,char *xf,double *x,double *y,int *n1,int *n2,int *aaint,char *strflag,double *brect);

void frame_clip_on  (void);
void frame_clip_off  (void);

extern void set_scale (char flag[],double subwin[],double frame_values[],int aaint[],char logflag[],double axis[]);
extern int graduate  (double *,double *,double *,double *,int *,int *,int *,int *,int *); 
extern void current_scale2default  (void); 

extern void set_window_scale_with_default  (int i);
extern int get_window_scale  (int,double * ); 
extern void del_window_scale  (int); 

extern void Scale2D  (int,double *,int *,int *,double *,double *,double *,double *,char *);
extern int setscale2d  (double *,double *,char *); 
extern int Nsetscale2d  (double *,double *,double *,char *); 
extern int getscale2d  (double *,double *,char *,double *); 

extern void scale_i2f( double x[], double y[],const int x1[],const int y1[],int n);
extern void scale_f2i(const double x[],const double y[],int x1[],int y1[],int n);
extern void length_scale_i2f(double *x, double *y, const int *x1, const int *y1, int n);
extern void length_scale_f2i(const double *x,const double *y, int *x1, int *y1, int n);

extern void ellipse2d  (double *,int *,int *,char *); 

extern void rect2d_f2i(const double x[],int x1[], int n);
extern void rect2d_i2f(double x[],const  int x1[], int n);
extern void scale_f2wrect(const double x[],double x1[]);

extern void axis2d  (double *,double *,double *,int *,double *); 
extern void zoom  (void); 
extern void zoom_get_rectangle    (double bbox[]);
extern void unzoom  (void); 
extern void Gr_Rescale  (char *,double *,int *,int *,int *,int *); 
extern void C2F(aplot1)  (double *,int *,int *,int *,int *npx,int *npy,char *logflag,double scx,double scy,double xofset,double yofset); 

/* Plo3d.c */

int shade ( int *polyx, int *polyy, int *fill, int polysize, int flag);
void GetEch3d1  ( double (*m1)[3],double *,double *,double *,double *);
void GetEch3d   (void);
extern void AxesStrings  (int,int *,int *,int *,char *,double *); 
extern void MaxiInd  (double *,int,int *,double); 
extern void UpNext  (int ,int *,int *); 
extern void DownNext  (int ,int *,int *); 
extern void TDAxis  (int flag,double FPval,double LPval,int *nax,int *FPoint,int *LPoint,int *Ticsdir); 
extern void C2F(TDdrawaxis)  (double ,double FPval,double LPval,int *nax,int *FPoint,int *LPoint,int *Ticsdir); 
extern void BBoxToval  (double *,double *,double *,int ,double *); 
extern void I3dRotation  (void); 
extern int DPoints1  (int *polyx,int *polyy,int *fill,int whiteid,double zmin,double zmax,double *,double *,double *,int i,int j,int jj1,int *p,int dc,int fg); 
extern int DPoints  (int *polyx,int *polyy,int *fill,int whiteid,double zmin,double zmax,double *,double *,double *,int i,int j,int jj1,int *p,int dc,int fg); 
extern int C2F(plot3d)  (double *,double *,double *,int *p,int *q,double *teta,double *,char *,int *,double *); 
extern int C2F(plot3d1)  (double *,double *,double *,int *p,int *q,double *teta,double *,char *,int *,double *); 
extern int C2F(fac3d)  (double *,double *,double *,int *cvect,int *p,int *q,double *teta,double *,char *,int *,double *); 
extern int C2F(fac3d1)  (double *,double *,double *,int *cvect,int *p,int *q,double *teta,double *,char *,int *,double *); 
extern int C2F(fac3d2)  (double *,double *,double *,int *cvect,int *p,int *q,double *teta,double *,char *,int *,double *);
extern int C2F(fac3d3)  (double *,double *,double *,int *cvect,int *p,int *q,double *teta,double *,char *,int *,double *);
extern int C2F(param3d)  (double *,double *,double *,int *,double *teta,double *,char *,int *,double *); 
extern int C2F(param3d1)  (double *,double *,double *,int *,int *,int *,int *colors,double *teta,double *,char *,int *,double *); 
extern int C2F(box3d)  (double *,double *,double *); 
extern int C2F(geom3d)  (double *,double *,double *,int *n); 
extern void SetEch3d  (double *,double *,double *,double *,double *teta,double *); 
extern void SetEch3d1  (double *,double *,double *,double *,double *teta,double *,int flag); 
extern void DrawAxis  (double *,double *,int *Indices,int style); 
extern void Convex_Box  (double *,double *,int *,int *,char *,int *,double *); 

/* Rec.c */ 

extern int tape_check_recorded_3D (int winnumber); 
extern void tape_clean_plots  (int winnumber);
extern void tape_replay_undo_scale(int winnumber);
extern void tape_replay_new_scale(int winnumber, int *flag, int *aaint,  double *bbox);
extern void tape_replay_new_scale_1(int winnumber, int *flag, int *aaint, double *bbox);
extern void tape_replay_new_angles(int winnumber,int *iflag, int *flag,double *theta, double *alpha, double *bbox);
extern void tape_replay(int winnumber);

extern void UseColorFlag  (int flag); 

/* RecLoad.c  */

extern int tape_load(const char *filename);

/* RecSave.c */ 

extern int tape_save (const char *filename,int winnumber);

/* Xcall.c */

extern int C2F(inttest) (int *); 
	
/* XXXX */

extern int first_in ( int,int,int *,int*);
extern int first_out ( int,int,int *,int*);

/* Rec.c */ 

extern void store_fpf_def(void);
extern void store_fpf(char *fpf);
extern void store_clipping_p(int x,int y,int w,int h);
extern void store_clipgrf(void );
extern void store_alufunction1(int val);
extern void store_background(int val);
extern void store_unclip();
extern void store_clip(double x[]);
extern void store_pattern(int val);
extern void store_font_size(int val);
extern void store_font(int val,int val1);
extern void store_foreground(int val);
extern void store_hidden3d(int val);
extern void store_absourel(int val);
extern void store_dash(int val);
extern void store_mark_size(int val);
extern void store_mark(int val,int val1);
extern void store_pixmapOn(int val);
extern void store_thickness(int val);
extern void store_usecolor(int val);
extern void store_show(void); 
extern void store_pixmapclear(void);
extern void store_drawarc_1(double arc[]);
extern void store_fillarcs_1(double vects[],int fillvect[], int n);
extern void store_drawarcs_1(double vects[], int style[], int n);
extern void store_fillpolyline_1(double *vx, double *vy,int n,int closeflag);
extern void store_drawarrows_1(double vx[],double vy[],int n,double as, int style[], int iflag);
extern void store_drawaxis_1(double *alpha, int *nsteps,double *initpoint, double *size);
extern void store_cleararea_1(double x, double y, double w, double h);
extern void store_fillarc_1( double arc[]);
extern void store_fillrectangle_1(double rect[]);
extern void store_drawpolyline_1( double *vx, double *vy ,int n, int closeflag);
extern void store_fillpolylines_1( double *vx, double *vy, int *fillvect, int n, int p, int v1);
extern void store_drawpolymark_1(double *vx, double *vy,int n);
extern void store_displaynumbers_1(double *x, double *y,int n, int flag,double *z, double *alpha);
extern void store_drawpolylines_1(double *vx, double *vy, int *drawvect,int n, int p);
extern void store_drawrectangle_1(double rect[]);
extern void store_drawrectangles_1(double vects[],int fillvect[], int n);
extern void store_drawsegments_1(double *vx, double *vy,int n, int *style, int iflag);
extern void store_displaystring_1(char *string,double x, double y,int flag,double angle);
extern void store_displaystringa_1(char *string, int ipos);
extern void store_xstringb_1(char *str,int *fflag, double *xd, double *yd, double *wd, double *hd);
extern void store_Ech(double *WRect, double *FRect, char *logflag);
extern void store_NEch(char *flag, double *WRect, double *ARect, double *FRect, char *logflag);
extern void store_Plot_G(int code, char *xf, double *x, double *y, int *n1, int *n2, int *style, char *strflag, char *legend, double *brect, int *aint);
extern void store_Plot(char *xf, double *x, double *y, int *n1, int *n2, int *style, char *strflag, char *legend, double *brect, int *aint);
extern void store_Plot1(char *xf, double *x, double *y, int *n1, int *n2, int *style, char *strflag, char *legend, double *brect, int *aint);
extern void store_Plot2(char *xf, double *x, double *y, int *n1, int *n2, int *style, char *strflag, char *legend, double *brect, int *aint);
extern void store_Plot3(char *xf, double *x, double *y, int *n1, int *n2, int *style, char *strflag, char *legend, double *brect, int *aint);
extern void store_Plot4(char *xf, double *x, double *y, int *n1, int *n2, int *style, char *strflag, char *legend, double *brect, int *aint);
extern void store_SciAxis(char pos, char xy_type, double *x, int *nx, double *y, int *ny, char **str, int subtics, char *format, int fontsize, int textcolor, int ticscolor, char logflag, int seg_flag);
extern void store_Grid(int *style);
extern void store_Param3D(double *x, double *y, double *z, int *n, double *teta, double *alpha, char *legend, int *flag, double *bbox);
extern void store_Param3D1( double *x, double *y, double *z, int *m, int *n, int *iflag, int *colors, double *teta, double *alpha, char *legend, int *flag, double *bbox);
extern void store_Plot3D(double *x, double *y, double *z, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox);
extern void store_Plot3D1(double *x, double *y, double *z, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox);
extern void store_Fac3D(double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox);
extern void store_Fac3D1(double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox);
extern void store_Fac3D2(double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox);
extern void store_Fac3D3(double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox);
extern void store_Fec( double *x, double *y, double *triangles, double *func, int *Nnode, int *Ntr, char *strflag, char *legend, double *brect, int *aaint, double *zminmax, int *colminmax);
extern void store_Contour( double *x, double *y, double *z, int *n1, int *n2, int *flagnz, int *nz, double *zz, double *teta, double *alpha, char *legend, int *flag, double *bbox, double *zlev);
extern void store_Contour2D(double *x, double *y, double *z, int *n1, int *n2, int *flagnz, int *nz, double *zz, int *style, char *strflag, char *legend, double *brect, int *aint);
extern void store_Gray(double *x, double *y, double *z, int *n1, int *n2, char *strflag, double *brect, int *aaint);
extern void store_Gray1(double *z, int *n1, int *n2, char *strflag, double *brect, int *aaint);
extern void store_Gray2(double *z, int *n1, int *n2, double *xrect);
extern void store_Champ(double *x, double *y, double *fx, double *fy, int *n1, int *n2, char *strflag, double *vrect, double *arfact);
extern void store_Champ1(double *x, double *y, double *fx, double *fy, int *n1, int *n2, char *strflag, double *vrect, double *arfact);

#endif /** SCIG_PROTO **/
