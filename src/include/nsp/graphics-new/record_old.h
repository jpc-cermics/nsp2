#ifndef NSP_ORECORD_H 
#define NSP_ORECORD_H 

typedef void fn_store_initialize_gc(BCG *Xgc);
typedef void fn_store_clipping_p(BCG *Xgc,int x,int y,int w,int h);
typedef void fn_store_clipgrf(BCG *Xgc) ;
typedef void fn_store_alufunction1(BCG *Xgc,int val);
typedef void fn_store_background(BCG *Xgc,int val);
typedef void fn_store_unclip(BCG *Xgc);
typedef void fn_store_test(BCG *Xgc);
typedef void fn_store_clip(BCG *Xgc,double x[]);
typedef void fn_store_pattern(BCG *Xgc,int val);
typedef void fn_store_font_size(BCG *Xgc,int val);
typedef void fn_store_font(BCG *Xgc,int val,int val1);
typedef void fn_store_foreground(BCG *Xgc,int val);
typedef void fn_store_hidden3d(BCG *Xgc,int val);
typedef void fn_store_absourel(BCG *Xgc,int val);
typedef void fn_store_dash(BCG *Xgc,int val);
typedef void fn_store_mark_size(BCG *Xgc,int val);
typedef void fn_store_mark(BCG *Xgc,int val,int val1);
typedef void fn_store_pixmapOn(BCG *Xgc,int val);
typedef void fn_store_thickness(BCG *Xgc,int val);
typedef void fn_store_usecolor(BCG *Xgc,int val);
typedef void fn_store_show(BCG *Xgc);
typedef void fn_store_pixmapclear(BCG *Xgc);
typedef void fn_store_fpf_def(BCG *Xgc);
typedef void fn_store_fpf(BCG *Xgc,char *fpf);
typedef void fn_store_fillarcs_1(BCG *Xgc,double vects[],int fillvect[], int n);
typedef void fn_store_drawarcs_1(BCG *Xgc,double vects[], int style[], int n);
typedef void fn_store_fillpolyline_1(BCG *Xgc,double *vx, double *vy,int n,int closeflag);
typedef void fn_store_drawarrows_1(BCG *Xgc,double vx[],double vy[],int n,double as, int style[], int iflag);
typedef void fn_store_drawaxis_1(BCG *Xgc,double *alpha, int *nsteps,double *initpoint, double *size);
typedef void fn_store_cleararea_1(BCG *Xgc,double x, double y, double w, double h);
typedef void fn_store_fillarc_1(BCG *Xgc,double arc[]);
typedef void fn_store_fillrectangle_1(BCG *Xgc,double rect[]);
typedef void fn_store_drawpolyline_1(BCG *Xgc, double *vx, double *vy ,int n, int closeflag);
typedef void fn_store_fillpolylines_1(BCG *Xgc, double *vx, double *vy, int *fillvect, int n, int p, int v1);
typedef void fn_store_drawpolymark_1(BCG *Xgc,double *vx, double *vy,int n);
typedef void fn_store_displaynumbers_1(BCG *Xgc,double *x, double *y,int n, int flag,double *z, double *alpha);
typedef void fn_store_drawpolylines_1(BCG *Xgc,double *vx, double *vy, int *drawvect,int n, int p);
typedef void fn_store_drawrectangle_1(BCG *Xgc,double rect[]);
typedef void fn_store_drawrectangles_1(BCG *Xgc,double vects[],int fillvect[], int n);
typedef void fn_store_drawsegments_1(BCG *Xgc,double *vx, double *vy,int n, int *style, int iflag);
typedef void fn_store_displaystring_1(BCG *Xgc,char *string,double x, double y,int flag,double angle);
typedef void fn_store_displaystringa_1(BCG *Xgc,char *string, int ipos);
typedef void fn_store_xstringb_1(BCG *Xgc,char *str,int *fflag, double *xd, double *yd, double *wd, double *hd);
typedef void fn_store_Ech(BCG *Xgc,double *WRect, double *FRect, char *logflag);
typedef void fn_store_NEch(BCG *Xgc,char *flag, double *WRect, double *ARect, double *FRect, char *logflag);
typedef void fn_store_Plot(BCG *Xgc,char *xf, double *x, double *y, int *n1, int *n2, int *style, char *strflag,const char *legend,int legend_pos, double *brect, int *aint);
typedef void fn_store_Plot1(BCG *Xgc,char *xf, double *x, double *y, int *n1, int *n2, int *style, char *strflag,const char *legend,int legend_pos, double *brect, int *aint);
typedef void fn_store_Plot2(BCG *Xgc,char *xf, double *x, double *y, int *n1, int *n2, int *style, char *strflag,const char *legend,int legend_pos, double *brect, int *aint);
typedef void fn_store_Plot3(BCG *Xgc,char *xf, double *x, double *y, int *n1, int *n2, int *style, char *strflag,
		    const char *legend,int legend_pos, double *brect, int *aint);
typedef void fn_store_Plot4(BCG *Xgc,char *xf, double *x, double *y, int *n1, int *n2, int *style, char *strflag,
		    const char *legend,int legend_pos, double *brect, int *aint);
typedef void fn_store_SciAxis(BCG *Xgc,char pos, char xy_type, double *x, int *nx, double *y, int *ny, 
		      char **str, int subtics, char *format, int fontsize, int textcolor, 
		      int ticscolor, char logflag, int seg_flag);
typedef void fn_store_Grid(BCG *Xgc,int *style);
typedef void fn_store_Param3D(BCG *Xgc,double *x, double *y, double *z, int *n, double *teta, double *alpha,const char *legend, int *flag, double *bbox);
typedef void fn_store_Param3D1(BCG *Xgc, double *x, double *y, double *z, int *m, int *n, int *iflag, int *colors, double *teta, double *alpha,const char *legend, int *flag, double *bbox);
typedef void fn_store_Plot3D(BCG *Xgc,double *x, double *y, double *z, int *p, int *q, double *teta, double *alpha,const char *legend, int *flag, double *bbox);
typedef void fn_store_Plot3D1(BCG *Xgc,double *x, double *y, double *z, int *p, int *q, double *teta, double *alpha,const char *legend, int *flag, double *bbox);
typedef void fn_store_3dobj(BCG *Xgc,void *Obj,double *teta, double *alpha,const char *legend, int *flag, double *bbox,
		 int with_mesh,int with_box,int box_color,int box_style);

typedef void fn_store_pixbuf(BCG *Xgc,void *pix,double x, double y,double w,double h,int src_x,int src_y);
typedef void fn_store_pixbuf_from_file(BCG *Xgc,const char *fname,double x, double y,double w,double h,int src_x,int src_y);
typedef void fn_store_Fac3D(BCG *Xgc,double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha,const char *legend, int *flag, double *bbox);
typedef void fn_store_Fac3D1(BCG *Xgc,double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha,const char *legend, int *flag, double *bbox);
typedef void fn_store_Fac3D2(BCG *Xgc,double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha,const char *legend, int *flag, double *bbox);
typedef void fn_store_Fac3D3(BCG *Xgc,double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha,const
		  char *legend, int *flag, double *bbox);
typedef void fn_store_Fec(BCG *Xgc, double *x, double *y, double *triangles, double *func, int *Nnode, int *Ntr, char *strflag,
	       const char *legend, double *brect, int *aaint,const double *zminmax,const int *colminmax, 
	       const int *colout,int draw);
typedef void fn_store_Contour(BCG *Xgc, double *x, double *y, double *z, int *n1, int *n2, int *flagnz, int *nz, double *zz, double *teta, double *alpha, char *legend, int *flag, double *bbox, double *zlev); 
typedef void fn_store_Contour2D(BCG *Xgc,double *x, double *y, double *z, int *n1, int *n2, int *flagnz, int *nz, double *zz, int *style, char *strflag, char *legend, double *brect, int *aint);
typedef void fn_store_Gray(BCG *Xgc,double *x, double *y, double *z, int nx, int ny, char *strflag,
		double *brect, int *aaint,int remap,const int *colminmax,const double *zminmax,
		const int *colout,int shade);
typedef void fn_store_Gray2(BCG *Xgc,double *z, int nr, int nc, double *xrect,
		 int remap,const int *colminmax,const double *zminmax);
typedef void fn_store_Gray1(BCG *Xgc,double *z, int nr, int nc, char *strflag, double *brect, int *aaint,
		 int remap,const int *colminmax,const double *zminmax);
typedef void fn_store_Champ(BCG *Xgc,double *x, double *y, double *fx, double *fy, int *n1, int *n2, char *strflag, double *vrect, double *arfact);
typedef void fn_store_Champ1(BCG *Xgc,double *x, double *y, double *fx, double *fy, int *n1, int *n2, char *strflag, double *vrect, double *arfact);
typedef void fn_store_graphic_object(BCG *Xgc,void *obj);


typedef void fn_store_colormap(BCG *Xgc,int m, int n,double colors[]);
typedef void fn_store_default_colormap(BCG *Xgc);
typedef void fn_store_drawarc_1(BCG *Xgc,double arc[]);
typedef int fn_store_record(BCG *Xgc,int code ,void *plot);


typedef struct _Nsp_gengine_store Nsp_gengine_store ;

struct _Nsp_gengine_store {
 fn_store_initialize_gc *store_initialize_gc;
 fn_store_clipping_p *store_clipping_p;
 fn_store_clipgrf *store_clipgrf ;
 fn_store_alufunction1 *store_alufunction1;
 fn_store_background *store_background;
 fn_store_unclip *store_unclip;
 fn_store_test *store_test;
 fn_store_clip *store_clip;
 fn_store_pattern *store_pattern;
 fn_store_font_size *store_font_size;
 fn_store_font *store_font;
 fn_store_foreground *store_foreground;
 fn_store_hidden3d *store_hidden3d;
 fn_store_absourel *store_absourel;
 fn_store_dash *store_dash;
 fn_store_mark_size *store_mark_size;
 fn_store_mark *store_mark;
 fn_store_pixmapOn *store_pixmapOn;
 fn_store_thickness *store_thickness;
 fn_store_usecolor *store_usecolor;
 fn_store_show *store_show;
 fn_store_pixmapclear *store_pixmapclear;
 fn_store_fpf_def *store_fpf_def;
 fn_store_fpf *store_fpf;
 fn_store_fillarcs_1 *store_fillarcs_1;
 fn_store_drawarcs_1 *store_drawarcs_1;
 fn_store_fillpolyline_1 *store_fillpolyline_1;
 fn_store_drawarrows_1 *store_drawarrows_1;
 fn_store_drawaxis_1 *store_drawaxis_1;
 fn_store_cleararea_1 *store_cleararea_1;
 fn_store_fillarc_1 *store_fillarc_1;
 fn_store_fillrectangle_1 *store_fillrectangle_1;
 fn_store_drawpolyline_1 *store_drawpolyline_1;
 fn_store_fillpolylines_1 *store_fillpolylines_1;
 fn_store_drawpolymark_1 *store_drawpolymark_1;
 fn_store_displaynumbers_1 *store_displaynumbers_1;
 fn_store_drawpolylines_1 *store_drawpolylines_1;
 fn_store_drawrectangle_1 *store_drawrectangle_1;
 fn_store_drawrectangles_1 *store_drawrectangles_1;
 fn_store_drawsegments_1 *store_drawsegments_1;
 fn_store_displaystring_1 *store_displaystring_1;
 fn_store_displaystringa_1 *store_displaystringa_1;
 fn_store_xstringb_1 *store_xstringb_1;
 fn_store_Ech *store_Ech;
 fn_store_NEch *store_NEch;
 fn_store_Plot *store_Plot;
 fn_store_Plot1 *store_Plot1;
 fn_store_Plot2 *store_Plot2;
 fn_store_Plot3 *store_Plot3;
 fn_store_Plot4 *store_Plot4;
 fn_store_SciAxis *store_SciAxis;
 fn_store_Grid *store_Grid;
 fn_store_Param3D *store_Param3D;
 fn_store_Param3D1 *store_Param3D1;
 fn_store_Plot3D *store_Plot3D;
 fn_store_Plot3D1 *store_Plot3D1;
 fn_store_3dobj *store_3dobj;
 fn_store_pixbuf *store_pixbuf;
 fn_store_pixbuf_from_file *store_pixbuf_from_file;
 fn_store_Fac3D *store_Fac3D;
 fn_store_Fac3D1 *store_Fac3D1;
 fn_store_Fac3D2 *store_Fac3D2;
 fn_store_Fac3D3 *store_Fac3D3;
 fn_store_Fec *store_Fec;
 fn_store_Contour *store_Contour; 
 fn_store_Contour2D *store_Contour2D;
 fn_store_Gray *store_Gray;
 fn_store_Gray2 *store_Gray2;
 fn_store_Gray1 *store_Gray1;
 fn_store_Champ *store_Champ;
 fn_store_Champ1 *store_Champ1;
  fn_store_graphic_object *store_graphic_object;
  fn_store_colormap *store_colormap;
  fn_store_default_colormap *store_default_colormap;
  fn_store_drawarc_1 *store_drawarc_1;
  fn_store_record *store_record;
};

extern Nsp_gengine_store nsp_gengine_record_old;
#endif 


#ifdef RECORD_PRIVATE 
 static fn_store_initialize_gc ostore_initialize_gc;
 static fn_store_clipping_p ostore_clipping_p;
 static fn_store_clipgrf ostore_clipgrf ;
 static fn_store_alufunction1 ostore_alufunction1;
 static fn_store_background ostore_background;
 static fn_store_unclip ostore_unclip;
 static fn_store_test ostore_test;
 static fn_store_clip ostore_clip;
 static fn_store_pattern ostore_pattern;
 static fn_store_font_size ostore_font_size;
 static fn_store_font ostore_font;
 static fn_store_foreground ostore_foreground;
 static fn_store_hidden3d ostore_hidden3d;
 static fn_store_absourel ostore_absourel;
 static fn_store_dash ostore_dash;
 static fn_store_mark_size ostore_mark_size;
 static fn_store_mark ostore_mark;
 static fn_store_pixmapOn ostore_pixmapOn;
 static fn_store_thickness ostore_thickness;
 static fn_store_usecolor ostore_usecolor;
 static fn_store_show ostore_show;
 static fn_store_pixmapclear ostore_pixmapclear;
 static fn_store_fpf_def ostore_fpf_def;
 static fn_store_fpf ostore_fpf;
 static fn_store_fillarcs_1 ostore_fillarcs_1;
 static fn_store_drawarcs_1 ostore_drawarcs_1;
 static fn_store_fillpolyline_1 ostore_fillpolyline_1;
 static fn_store_drawarrows_1 ostore_drawarrows_1;
 static fn_store_drawaxis_1 ostore_drawaxis_1;
 static fn_store_cleararea_1 ostore_cleararea_1;
 static fn_store_fillarc_1 ostore_fillarc_1;
 static fn_store_fillrectangle_1 ostore_fillrectangle_1;
 static fn_store_drawpolyline_1 ostore_drawpolyline_1;
 static fn_store_fillpolylines_1 ostore_fillpolylines_1;
 static fn_store_drawpolymark_1 ostore_drawpolymark_1;
 static fn_store_displaynumbers_1 ostore_displaynumbers_1;
 static fn_store_drawpolylines_1 ostore_drawpolylines_1;
 static fn_store_drawrectangle_1 ostore_drawrectangle_1;
 static fn_store_drawrectangles_1 ostore_drawrectangles_1;
 static fn_store_drawsegments_1 ostore_drawsegments_1;
 static fn_store_displaystring_1 ostore_displaystring_1;
 static fn_store_displaystringa_1 ostore_displaystringa_1;
 static fn_store_xstringb_1 ostore_xstringb_1;
 static fn_store_Ech ostore_Ech;
 static fn_store_NEch ostore_NEch;
 static fn_store_Plot ostore_Plot;
 static fn_store_Plot1 ostore_Plot1;
 static fn_store_Plot2 ostore_Plot2;
 static fn_store_Plot3 ostore_Plot3;
 static fn_store_Plot4 ostore_Plot4;
 static fn_store_SciAxis ostore_SciAxis;
 static fn_store_Grid ostore_Grid;
 static fn_store_Param3D ostore_Param3D;
 static fn_store_Param3D1 ostore_Param3D1;
 static fn_store_Plot3D ostore_Plot3D;
 static fn_store_Plot3D1 ostore_Plot3D1;
 static fn_store_3dobj ostore_3dobj;
 static fn_store_pixbuf ostore_pixbuf;
 static fn_store_pixbuf_from_file ostore_pixbuf_from_file;
 static fn_store_Fac3D ostore_Fac3D;
 static fn_store_Fac3D1 ostore_Fac3D1;
 static fn_store_Fac3D2 ostore_Fac3D2;
 static fn_store_Fac3D3 ostore_Fac3D3;
 static fn_store_Fec ostore_Fec;
 static fn_store_Contour ostore_Contour; 
 static fn_store_Contour2D ostore_Contour2D;
 static fn_store_Gray ostore_Gray;
 static fn_store_Gray2 ostore_Gray2;
 static fn_store_Gray1 ostore_Gray1;
 static fn_store_Champ ostore_Champ;
 static fn_store_Champ1 ostore_Champ1;
 static fn_store_graphic_object ostore_graphic_object;
static fn_store_colormap ostore_colormap;
static fn_store_default_colormap ostore_default_colormap;
static fn_store_drawarc_1 ostore_drawarc_1;
static fn_store_record ostore_record;

#endif 






