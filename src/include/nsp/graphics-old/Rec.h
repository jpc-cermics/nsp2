/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2000 Enpc/Jean-Philippe Chancelier
 *    jpc@cereve.enpc.fr 
 * --------------------------------------------------------------------------*/

#ifndef SCI_REC_OLD 
#define SCI_REC_OLD  

#include "nsp/object.h" 

/*---------------------------------------------------------------------
 * basics 
 *---------------------------------------------------------------------------*/

struct rec_void { int code; };
struct rec_str { int code; char *str;};
struct rec_int { int code; int val; };
struct rec_int4 { int code; int vals[4]; };
struct rec_int2 { int code; int val; int val1; };
struct rec_double4 { int code; double vals[4]; };
struct rec_drawarc { int code; double arc[6];};
struct rec_fillarcs {int code;  int n; double *vects;int *fillvect;};
struct rec_fillpolyline {int code; int n, closeflag; double *vx,*vy;};
struct rec_arrows { int code; int n,iflag; double as , *vx,*vy;int *style;int def_style;};
struct rec_drawaxis { int code; double alpha; int nsteps ; double initpoint[2];  double size[3];};
struct rec_fillpolylines {int code; int n,p,v1; double *vx,*vy; int *fillvect;}; 
struct rec_displaynumbers {int code; int n; double * x,*y,*z,*alpha; int flag;};
struct rec_drawpolylines {int code; int n,p; double *vx,*vy; int *drawvect;}; 
struct rec_segment {int code; int n,iflag; double *vx,*vy;int *style;};
struct rec_displaystring {int code; double x,y,angle; char *string ; int flag;};
struct rec_displaystringa {int code;int ipos; char *string;};
struct rec_xstringb {int code; double x,y,wd,hd; int flag; char *string;};
struct rec_colormap {int code; double *colors; int m;int n;};
struct rec_object {int code; void *obj;};

/*---------------------------------------------------------------------
 * scales 
 *---------------------------------------------------------------------------*/

struct rec_scale { int code; double *Wrect,*Frect,*Frect_kp; char logflag[2]; } ;
struct rec_nscale { int code; char *flag;double *Wrect,*Frect,*Arect,*Frect_kp; char logflag[2]; } ;
  
/*---------------------------------------------------------------------
 * plot2d 
 *---------------------------------------------------------------------------*/

struct rec_plot2d {int code;
  char *xf;
  double *x,*y,*brect;
  int   n1,n2,*style,*aint;
  char *legend,*strflag;
  int legend_pos;
  char *strflag_kp;
  int *aint_kp;
  double *brect_kp;
} ;

/*---------------------------------------------------------------------
 * axis 
 *---------------------------------------------------------------------------*/

struct rec_sciaxis {
  int code;
  char pos,xy_type,logflag;
  int nx,ny,subtics,fontsize,textcolor,ticscolor,f_l,seg_flag;
  double *x,*y;  
  char **str;
  char *format; 
} ;
  
/*---------------------------------------------------------------------
 * grid 
 *---------------------------------------------------------------------------*/

struct rec_xgrid { int code;  int style ; } ;

/*---------------------------------------------------------------------
 * param3d 
 *---------------------------------------------------------------------------*/

struct rec_param3d {int code;
		    double *x,*y,*z,*bbox;
		    int   n,*flag;
		    double teta,alpha;
		    char  *legend;
		 } ;

struct rec_param3d1 {int code;
		    double *x,*y,*z,*bbox;
		    int   n,m,iflag,*colors,*flag;
		    double teta,alpha;
		    char  *legend;
		 } ;

/*---------------------------------------------------------------------
 * plot3d 
 *---------------------------------------------------------------------------*/

struct rec_plot3d {int code;
		   double *x,*y,*z,*bbox;
		   int   p,q,*flag;
		   double teta,alpha;
		   char  *legend;
		 } ;

/*---------------------------------------------------------------------
 * plot3d 
 *---------------------------------------------------------------------------*/

struct rec_3dobj {int code;
  NspList *L;
  double *bbox;
  int   *flag;
  double teta,alpha;
  char  *legend;
  int  with_mesh;
  int  with_box;
  int box_color;
  int box_style;
} ;

/*---------------------------------------------------------------------
 * fac3d 
 *---------------------------------------------------------------------------*/

struct rec_fac3d {int code;
		   double *x,*y,*z,*bbox;
		   int   p,q,*flag,*cvect;
		   double teta,alpha;
		   char  *legend;
		 } ;

/*---------------------------------------------------------------------
 * fec 
 *---------------------------------------------------------------------------*/


struct rec_fec {
  int code;
  double *x,*y,*triangles,*func;
  int   Nnode,Ntr;
  double *brect, *zminmax; 
  int  *aaint, *colminmax,*colout;
  char  *legend,*strflag;
  char *strflag_kp;
  double *brect_kp;
  int *aaint_kp;
  int draw;
} ;

/*---------------------------------------------------------------------
 * contour 
 *---------------------------------------------------------------------------*/
/** general **/
struct rec_contour {int code;
		    double *x,*y,*z,*zz,zlev;
		    int   n1,n2,nz,flagnz;
		    double *bbox;
		    double teta,alpha;
		    int *flag;
		    char  *legend;
		 } ;

/** version 2d **/

struct rec_contour2d {int code;
  double *x,*y,*z,*zz;
  int   n1,n2,nz,flagnz;
  double *brect;
  int *style,*aint;
  char *legend,*strflag;
  char *strflag_kp;
  int *aint_kp;
  double *brect_kp;
} ;

/*---------------------------------------------------------------------
 * xgray 
 *---------------------------------------------------------------------------*/

struct rec_gray {int code;
  char *strflag;
  double *x,*y,*z,*brect;
  int   n1,n2,*aaint;
  int *aaint_kp;
  double *brect_kp;
  char *strflag_kp;
  int remap;
  double *zminmax;
  int *colminmax;
  int *colout;
  int shade;
} ;

struct rec_gray1 {int code;
  char *strflag;
  double *x,*y,*z,*brect;
  int   n1,n2,*aaint;
  int *aaint_kp;
  double *brect_kp;
  char *strflag_kp;
  int remap;
  double *zminmax;
  int *colminmax;
} ;


struct rec_gray_2 {int code;
  double *z,*xrect;
  int   n1,n2;
  int remap;
  double *zminmax;
  int *colminmax;
} ;

/*---------------------------------------------------------------------
 * xchamp  
 *---------------------------------------------------------------------------*/

struct rec_champ {int code;
		  double *x,*y,*fx,*fy,*vrect,arfact;
		  int   n1,n2;
		  char *strflag;
		  char *strflag_kp;
		  double *vrect_kp;
} ;

/* A GdkPixbuf 
 *
 */ 

struct rec_pixbuf {
  int code;
  double x,y,w,h;
  int src_x,src_y;
  NspObject *nsp_pixbuf;
};

/* A file with a pixbuf */
struct rec_pixbuf_file {
  int code;
  double x,y,w,h;
  int src_x,src_y;
  char *pixbuf_file;
};



/*---------------------------------------------------------------------
 * doubly linked list for graphic storage 
 *---------------------------------------------------------------------------*/


typedef void  (*record_action) (void*);  /*action */
typedef void  (*record_scale_change)(void *plot, int *flag, double *bbox, int *aaint, int undo, int *bbox1, double *subwin, int win_num);
typedef void  (*record_new_angles)(void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox);


typedef  struct  {
  int  code ;        /* Indice in the table */
  void  (*replay) (BCG *,void*);  /*action */
  void  (*clean) (void*);  /*action */
  void  (*unscale) (void*);  /*action */
  void  (*scale_change)(BCG *,void *plot, int *flag, double *bbox, int *aaint,char *strf, int undo, int *bbox1, double *subwin, int win_num);
  void  (*new_angles)(BCG *,void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox, int *pt);

} Record_table ;


#define CODEclipping_p           0
#define CODEclipgrf		 1     
#define CODEalufunction1	 2     
#define CODEbackground	    	 3  
#define CODEunclip		 4     
#define CODEclip		 5     
#define CODEpattern		 6     
#define CODEfont_size	    	 7  
#define CODEfont		 8     
#define CODEforeground	    	 9  
#define CODEhidden3d		 10    
#define CODEabsourel		 11    
#define CODEdash		 12    
#define CODEmark_size	    	 13 
#define CODEmark		 14    
#define CODEpixmapOn		 15    
#define CODEthickness      	 16    
#define CODEusecolor		 17    
#define CODEshow		 18    
#define CODEpixmapclear	    	 19 
#define CODEdrawarc_1       	 20    
#define CODEfillarcs_1      	 21    
#define CODEdrawarcs_1      	 22    
#define CODEfillpolyline_1	 23    
#define CODEdrawarrows_1	 24    
#define CODEdrawaxis_1      	 25    
#define CODEcleararea_1	    	 26 
#define CODEfillarc_1       	 27    
#define CODEfillrectangle_1	 28    
#define CODEdrawpolyline_1	 29    
#define CODEfillpolylines_1	 30    
#define CODEdrawpolymark_1	 31    
#define CODEdisplaynumbers_1	 32    
#define CODEdrawpolylines_1	 33    
#define CODEdrawrectangle_1	 34    
#define CODEdrawrectangles_1	 35    
#define CODEdrawsegments_1	 36    
#define CODEdisplaystring_1	 37    
#define CODEdisplaystringa_1	 38    
#define CODExstringb_1      	 39    
#define CODEEch		    	 40 
#define CODENEch		 41    
#define CODEPlot		 42    
#define CODEPlot1		 43    
#define CODEPlot2		 44    
#define CODEPlot3		 45    
#define CODEPlot4		 46    
#define CODESciAxis		 47    
#define CODEGrid		 48    
#define CODEParam3D		 49    
#define CODEParam3D1		 50    
#define CODEPlot3D		 51    
#define CODEPlot3D1		 52    
#define CODEFac3D		 53    
#define CODEFac3D1		 54    
#define CODEFac3D2		 55    
#define CODEFac3D3		 56    
#define CODEFec		    	 57 
#define CODEFecN		 58    
#define CODEContour		 59    
#define CODEContour2D       	 60 
#define CODEGray		 61 
#define CODEGray1		 62 
#define CODEGray2		 63 
#define CODEChamp		 64 
#define CODEChamp1		 65 
#define CODEfpf_def              66
#define CODEfpf                  67
#define CODEinitialize_gc        68
#define CODEColormap             69    
#define CODEdefault_colormap     70
#define CODE3dobj                71
#define CODEpixbuf               72
#define CODEpixbuf_file          73
#define CODEtest                 74
#define CODEobject               75
/* take care : this one must be the last */
#define CODEendplots             76

#if defined(REC_PRIVATE) && !defined(IGNORE_REC) 

static void replay_clipping_p(BCG *Xgc,void *theplot);
static void replay_clipgrf(BCG *Xgc, void *theplot);
static void replay_alufunction1(BCG *Xgc,void *theplot);
static void replay_background(BCG *Xgc,void *theplot);
static void replay_unclip(BCG *Xgc, void *theplot);
static void replay_test(BCG *Xgc, void *theplot);
static void replay_clip(BCG *Xgc,void *theplot);
static void replay_pattern(BCG *Xgc,void *theplot);
static void replay_font_size(BCG *Xgc,void *theplot);
static void replay_font(BCG *Xgc,void *theplot);
static void replay_foreground(BCG *Xgc,void *theplot);
static void replay_hidden3d(BCG *Xgc,void *theplot);
static void replay_absourel(BCG *Xgc,void *theplot);
static void replay_dash(BCG *Xgc,void *theplot);
static void replay_mark_size(BCG *Xgc,void *theplot);
static void replay_mark(BCG *Xgc,void *theplot);
static void replay_pixmapOn(BCG *Xgc,void *theplot);
static void replay_thickness(BCG *Xgc,void *theplot);
static void replay_usecolor(BCG *Xgc,void *theplot);
static void replay_show(BCG *Xgc, void *theplot);
static void replay_pixmapclear(BCG *Xgc, void *theplot);
static void replay_fpf(BCG *Xgc,void *theplot);
static void replay_fpf_def(BCG *Xgc,void *theplot);
static void replay_drawarc_1(BCG *Xgc,void  *theplot);
static void replay_fillarcs_1(BCG *Xgc,void  *theplot);
static void replay_drawarcs_1(BCG *Xgc,void  *theplot);
static void replay_fillpolyline_1(BCG *Xgc,void  *theplot);
static void replay_drawarrows_1(BCG *Xgc,void  *theplot);
static void replay_drawaxis_1(BCG *Xgc,void  *theplot);
static void replay_cleararea_1(BCG *Xgc,void  *theplot);
static void replay_fillarc_1(BCG *Xgc,void  *theplot);
static void replay_fillrectangle_1(BCG *Xgc,void  *theplot);
static void replay_drawpolyline_1(BCG *Xgc,void  *theplot);
static void replay_fillpolylines_1(BCG *Xgc,void  *theplot);
static void replay_drawpolymark_1(BCG *Xgc,void  *theplot);
static void replay_displaynumbers_1(BCG *Xgc,void  *theplot);
static void replay_drawpolylines_1(BCG *Xgc,void  *theplot);
static void replay_drawrectangle_1(BCG *Xgc,void  *theplot);
static void replay_drawrectangles_1(BCG *Xgc,void  *theplot);
static void replay_drawsegments_1(BCG *Xgc,void  *theplot);
static void replay_displaystring_1(BCG *Xgc,void  *theplot);
static void replay_displaystringa_1(BCG *Xgc,void  *theplot);
static void replay_xstringb_1(BCG *Xgc,void  *theplot);
static void replay_Ech(BCG *Xgc,void *theplot);
static void replay_NEch(BCG *Xgc,void *theplot);
static void replay_Plot(BCG *Xgc,void * theplot);
static void replay_Plot1(BCG *Xgc,void * theplot);
static void replay_Plot2(BCG *Xgc,void * theplot);
static void replay_Plot3(BCG *Xgc,void * theplot);
static void replay_Plot4(BCG *Xgc,void * theplot);
static void replay_SciAxis(BCG *Xgc,void *theplot);
static void replay_Grid(BCG *Xgc,void *theplot);
static void replay_Param3D(BCG *Xgc,void *theplot);
static void replay_Param3D1(BCG *Xgc,void *theplot);
static void replay_3D(BCG *Xgc,void *theplot);
static void replay_3D1(BCG *Xgc,void *theplot);
static void replay_3dobj(BCG *Xgc,void *theplot);
static void replay_Fac3D(BCG *Xgc,void *theplot);
static void replay_Fac3D1(BCG *Xgc,void *theplot);
static void replay_Fac3D2(BCG *Xgc,void *theplot);
static void replay_Fac3D3(BCG *Xgc,void *theplot);
static void replay_Fec(BCG *Xgc,void *theplot);
static void replay_Contour(BCG *Xgc,void *theplot);
static void replay_Contour2D(BCG *Xgc,void *theplot);
static void replay_Gray(BCG *Xgc,void *theplot);
static void replay_Gray1(BCG *Xgc,void *theplot);
static void replay_Gray2(BCG *Xgc,void *theplot);
static void replay_Champ(BCG *Xgc,void *theplot);
static void replay_Champ1(BCG *Xgc,void *theplot);
static void replay_initialize_gc(BCG *Xgc,void * theplot );
static void replay_colormap(BCG *Xgc,void  *theplot);
static void replay_default_colormap(BCG *Xgc,void  *theplot);
static void replay_pixbuf(BCG *Xgc,void  *theplot);
static void replay_pixbuf_from_file(BCG *Xgc,void  *theplot);
static void replay_graphic_object(BCG *Xgc,void  *theplot);

static void clean_fpf(void *theplot);
static void clean_drawarc_1(void  *theplot);
static void clean_fillarcs_1(void  *theplot);
static void clean_drawarcs_1(void  *theplot);
static void clean_fillpolyline_1(void  *theplot);
static void clean_drawarrows_1(void  *theplot);
static void clean_drawaxis_1(void  *theplot);
static void clean_cleararea_1(void  *theplot);
static void clean_fillarc_1(void  *theplot);
static void clean_fillrectangle_1(void  *theplot);
static void clean_drawpolyline_1(void  *theplot);
static void clean_fillpolylines_1(void  *theplot);
static void clean_drawpolymark_1(void  *theplot);
static void clean_displaynumbers_1(void  *theplot);
static void clean_drawpolylines_1(void  *theplot);
static void clean_drawrectangle_1(void  *theplot);
static void clean_drawrectangles_1(void  *theplot);
static void clean_drawsegments_1(void  *theplot);
static void clean_displaystring_1(void  *theplot);
static void clean_displaystringa_1(void  *theplot);
static void clean_xstringb_1(void  *theplot);
static void clean_Ech(void *theplot);
static void clean_NEch(void *theplot);
static void clean_Plot(void * theplot);
static void clean_SciAxis(void *theplot);
static void clean_Grid(void *theplot);
static void clean_Param3D(void *theplot);
static void clean_Param3D1(void *theplot);
static void clean_3D(void *theplot);
static void clean_3D1(void *theplot);
static void clean_3dobj(void *theplot);
static void clean_Fac3D(void *theplot);
static void clean_Fec(void *theplot);
static void clean_Contour(void *theplot);
static void clean_Contour2D(void *theplot);
static void clean_Gray(void *theplot);
static void clean_Gray1(void *theplot);
static void clean_Gray2(void *theplot);
static void clean_Champ(void *theplot);
static void clean_colormap(void *theplot);
static void clean_pixbuf(void *theplot);
static void clean_pixbuf_from_file(void *theplot);
static void clean_graphic_object(void *plot);

static void scale_change_Plot(BCG *Xgc,void *plot, int *flag, double *bbox, int *aaint, char *strflag,int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Contour2D(BCG *Xgc,void *plot, int *flag, double *bbox, int *aaint, char *strflag,int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Gray(BCG *Xgc,void *plot, int *flag, double *bbox, int *aaint, char *strflag,int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Champ(BCG *Xgc,void *plot, int *flag, double *bbox, int *aaint, char *strflag,int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Fec(BCG *Xgc,void *plot, int *flag, double *bbox, int *aaint, char *strflag,int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Ech(BCG *Xgc,void *plot, int *flag, double *bbox, int *aaint, char *strflag,int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_NEch(BCG *Xgc,void *plot, int *flag, double *bbox, int *aaint, char *strflag,int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Contour(BCG *Xgc,void *plot, int *flag, double *b1, int *aaint, char *strflag,int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Fac3D(BCG *Xgc,void *plot, int *flag, double *b1, int *aaint, char *strflag,int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Param3D(BCG *Xgc,void *plot, int *flag, double *b1, int *aaint, char *strflag,int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Param3D1(BCG *Xgc,void *plot, int *flag, double *b1, int *aaint,  char *strflag,int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Plot3D(BCG *Xgc,void *plot, int *flag, double *b1, int *aaint,  char *strflag,int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_3dobj(BCG *Xgc,void *plot, int *flag, double *b1, int *aaint,char *strflag, 
			       int undo, int *bbox1, double *subwin, int win_num);

static void scale_change_graphic_object(BCG *Xgc,void *plot, int *flag, double *b1, int *aaint,char *strflag, 
					int undo, int *bbox1, double *subwin, int win_num);

static void unscale_Plot(void *plot);
static void unscale_Contour2D(void *plot);
static void unscale_Gray(void *plot);
static void unscale_Champ(void *plot);
static void unscale_Fec(void *plot);
static void unscale_Ech(void *plot);
static void unscale_NEch(void *plot);
static void unscale_Contour(void *plot);
static void unscale_Fac3D(void *plot);
static void unscale_Param3D(void *plot);
static void unscale_Param3D1(void *plot);
static void unscale_Plot3D(void *plot);
static void unscale_3dobj(void *plot);
static void unscale_graphic_object(void *plot);

static void new_angles_Plot3D(BCG *,void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox,int *pt);
static void new_angles_Fac3D(BCG *,void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox,int *pt);
static void new_angles_Contour(BCG *,void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox,int *pt);
static void new_angles_Param3D(BCG *,void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox,int *pt);
static void new_angles_Param3D1(BCG *,void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox,int *pt);
static void new_angles_3dobj(BCG *,void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox,int *pt);
static void new_angles_graphic_object(BCG *,void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox,int *pt);

Record_table record_table [] ={
  {CODEclipping_p            , replay_clipping_p       	,NULL,NULL,NULL,NULL},
  {CODEclipgrf		     , replay_clipgrf		,NULL,NULL,NULL,NULL},   	     
  {CODEalufunction1	     , replay_alufunction1     	,NULL,NULL,NULL,NULL},  	     
  {CODEbackground	     , replay_background       	,NULL,NULL,NULL,NULL},  	     
  {CODEunclip		     , replay_unclip	       	,NULL,NULL,NULL,NULL},  	     
  {CODEclip		     , replay_clip	       	,NULL,NULL,NULL,NULL},  	     
  {CODEpattern		     , replay_pattern		,NULL,NULL,NULL,NULL},   	     
  {CODEfont_size	     , replay_font_size		,NULL,NULL,NULL,NULL},          	     
  {CODEfont		     , replay_font	       	,NULL,NULL,NULL,NULL},  	     
  {CODEforeground	     , replay_foreground       	,NULL,NULL,NULL,NULL},  	     
  {CODEhidden3d		     , replay_hidden3d		,NULL,NULL,NULL,NULL},   	     
  {CODEabsourel		     , replay_absourel		,NULL,NULL,NULL,NULL},   	     
  {CODEdash		     , replay_dash	       	,NULL,NULL,NULL,NULL},  	     
  {CODEmark_size	     , replay_mark_size		,NULL,NULL,NULL,NULL},          	     
  {CODEmark		     , replay_mark	       	,NULL,NULL,NULL,NULL},  	     
  {CODEpixmapOn		     , replay_pixmapOn		,NULL,NULL,NULL,NULL},   	     
  {CODEthickness      	     , replay_thickness		,NULL,NULL,NULL,NULL},          	     
  {CODEusecolor		     , replay_usecolor		,NULL,NULL,NULL,NULL},   	     
  {CODEshow		     , replay_show	       	,NULL,NULL,NULL,NULL},  	     
  {CODEpixmapclear	     , replay_pixmapclear      	,NULL,NULL,NULL,NULL},  	     
  {CODEdrawarc_1       	     , replay_drawarc_1		,clean_drawarc_1  	  ,NULL,NULL,NULL},          	     
  {CODEfillarcs_1      	     , replay_fillarcs_1	,clean_fillarcs_1	  ,NULL,NULL,NULL},  	     
  {CODEdrawarcs_1      	     , replay_drawarcs_1       	,clean_drawarcs_1	  ,NULL,NULL,NULL},  	     
  {CODEfillpolyline_1	     , replay_fillpolyline_1   	,clean_fillpolyline_1	  ,NULL,NULL,NULL},  	     
  {CODEdrawarrows_1	     , replay_drawarrows_1     	,clean_drawarrows_1	  ,NULL,NULL,NULL},  	     
  {CODEdrawaxis_1      	     , replay_drawaxis_1       	,clean_drawaxis_1	  ,NULL,NULL,NULL},  	     
  {CODEcleararea_1	     , replay_cleararea_1      	,clean_cleararea_1	  ,NULL,NULL,NULL},  	     
  {CODEfillarc_1       	     , replay_fillarc_1		,clean_fillarc_1	  ,NULL,NULL,NULL},          	     
  {CODEfillrectangle_1	     , replay_fillrectangle_1	,clean_fillrectangle_1	  ,NULL,NULL,NULL},   	     
  {CODEdrawpolyline_1	     , replay_drawpolyline_1   	,clean_drawpolyline_1	  ,NULL,NULL,NULL},  	     
  {CODEfillpolylines_1	     , replay_fillpolylines_1	,clean_fillpolylines_1	  ,NULL,NULL,NULL},   	     
  {CODEdrawpolymark_1	     , replay_drawpolymark_1   	,clean_drawpolymark_1	  ,NULL,NULL,NULL},  	     
  {CODEdisplaynumbers_1	     , replay_displaynumbers_1	,clean_displaynumbers_1	  ,NULL,NULL,NULL},   	     
  {CODEdrawpolylines_1	     , replay_drawpolylines_1	,clean_drawpolylines_1	  ,NULL,NULL,NULL},   	     
  {CODEdrawrectangle_1	     , replay_drawrectangle_1	,clean_drawrectangle_1	  ,NULL,NULL,NULL},   	     
  {CODEdrawrectangles_1	     , replay_drawrectangles_1	,clean_drawrectangles_1	  ,NULL,NULL,NULL},   	     
  {CODEdrawsegments_1	     , replay_drawsegments_1   	,clean_drawsegments_1	  ,NULL,NULL,NULL},  	     
  {CODEdisplaystring_1	     , replay_displaystring_1	,clean_displaystring_1	  ,NULL,NULL,NULL},   	     
  {CODEdisplaystringa_1	     , replay_displaystringa_1	,clean_displaystringa_1	  ,NULL,NULL,NULL},   	     
  {CODExstringb_1      	     , replay_xstringb_1       	,clean_xstringb_1	  ,NULL,NULL,NULL},  	     
  {CODEEch		     , replay_Ech	       	,clean_Ech		  ,unscale_Ech		   ,scale_change_Ech		  ,NULL},  	     
  {CODENEch		     , replay_NEch	       	,clean_NEch		  ,unscale_NEch		   ,scale_change_NEch		  ,NULL},  	     
  {CODEPlot		     , replay_Plot	       	,clean_Plot		  ,unscale_Plot		   ,scale_change_Plot		  ,NULL},  	     
  {CODEPlot1		     , replay_Plot1	       	,clean_Plot		  ,unscale_Plot		   ,scale_change_Plot		  ,NULL},  	     
  {CODEPlot2		     , replay_Plot2    		,clean_Plot		  ,unscale_Plot		   ,scale_change_Plot		  ,NULL},  	     
  {CODEPlot3		     , replay_Plot3    		,clean_Plot		  ,unscale_Plot		   ,scale_change_Plot		  ,NULL},  	     
  {CODEPlot4		     , replay_Plot4    		,clean_Plot		  ,unscale_Plot		   ,scale_change_Plot		  ,NULL},  	     
  {CODESciAxis		     , replay_SciAxis		,clean_SciAxis		  ,NULL                    ,NULL                          ,NULL},  	     
  {CODEGrid		     , replay_Grid     		,clean_Grid		  ,NULL                    ,NULL                          ,NULL},  	     
  {CODEParam3D		     , replay_Param3D		,clean_Param3D		  ,unscale_Param3D	   ,scale_change_Param3D	  ,new_angles_Param3D},     
  {CODEParam3D1		     , replay_Param3D1		,clean_Param3D1		  ,unscale_Param3D1	   ,scale_change_Param3D1	  ,new_angles_Param3D1},
  {CODEPlot3D		     , replay_3D       		,clean_3D		  ,unscale_Plot3D      	   ,scale_change_Plot3D		  ,new_angles_Plot3D},      
  {CODEPlot3D1		     , replay_3D1      		,clean_3D1		  ,unscale_Plot3D	   ,scale_change_Plot3D  	  ,new_angles_Plot3D},     
  {CODEFac3D		     , replay_Fac3D    		,clean_Fac3D		  ,unscale_Fac3D	   ,scale_change_Fac3D		  ,new_angles_Fac3D},      
  {CODEFac3D1		     , replay_Fac3D1   		,clean_Fac3D		  ,unscale_Fac3D	   ,scale_change_Fac3D		  ,new_angles_Fac3D},      
  {CODEFac3D2		     , replay_Fac3D2   		,clean_Fac3D		  ,unscale_Fac3D	   ,scale_change_Fac3D		  ,new_angles_Fac3D},      
  {CODEFac3D3		     , replay_Fac3D3   		,clean_Fac3D		  ,unscale_Fac3D	   ,scale_change_Fac3D		  ,new_angles_Fac3D},      
  {CODEFec		     , replay_Fec      		,clean_Fec		  ,unscale_Fec		   ,scale_change_Fec		  ,NULL},  	     
  {CODEFecN		     , replay_Fec      		,clean_Fec		  ,unscale_Fec		   ,scale_change_Fec		  ,NULL},  	
  {CODEContour		     , replay_Contour		,clean_Contour		  ,unscale_Contour	   ,scale_change_Contour	  ,new_angles_Contour},
  {CODEContour2D       	     , replay_Contour2D		,clean_Contour2D	  ,unscale_Contour2D	   ,scale_change_Contour2D	  ,NULL},          	
  {CODEGray		     , replay_Gray     		,clean_Gray		  ,unscale_Gray		   ,scale_change_Gray		  ,NULL},        
  {CODEGray1		     , replay_Gray1    		,clean_Gray1		  ,unscale_Gray		   ,scale_change_Gray		  ,NULL},
  {CODEGray2		     , replay_Gray2    		,clean_Gray2		  ,NULL  	           ,NULL        		  ,NULL},
  {CODEChamp		     , replay_Champ    		,clean_Champ		  ,unscale_Champ	   ,scale_change_Champ		  ,NULL},     
  {CODEChamp1		     , replay_Champ1            ,clean_Champ              ,unscale_Champ           ,scale_change_Champ            ,NULL},
  {CODEfpf_def   	     , replay_fpf_def           ,NULL                     ,NULL                    ,NULL                          ,NULL},
  {CODEfpf                   , replay_fpf               ,clean_fpf                ,NULL                    ,NULL                          ,NULL},
  {CODEinitialize_gc         , replay_initialize_gc     ,NULL                     ,NULL                    ,NULL                          ,NULL}, 
  {CODEColormap              , replay_colormap          ,clean_colormap           ,NULL                    ,NULL                          ,NULL}, 
  {CODEdefault_colormap      , replay_default_colormap  ,NULL                     ,NULL                    ,NULL                          ,NULL}, 
  {CODE3dobj		     , replay_3dobj    		,clean_3dobj	          ,unscale_3dobj      	   ,scale_change_3dobj		  ,new_angles_3dobj}, 
  {CODEpixbuf		     , replay_pixbuf    	,clean_pixbuf	          ,NULL                    ,NULL                          ,NULL},
  {CODEpixbuf_file	     , replay_pixbuf_from_file 	,clean_pixbuf_from_file	  ,NULL                    ,NULL                          ,NULL},
  {CODEtest	             , replay_test 	        ,NULL                     ,NULL                    ,NULL                          ,NULL},
  {CODEobject       	     , replay_graphic_object	,clean_graphic_object  	  ,unscale_graphic_object ,scale_change_graphic_object   ,new_angles_graphic_object}, 
};     	

#endif /*  REC_PRIVATE */

#ifndef REC_PRIVATE 
extern  Record_table record_table [];
#endif 
							                                     
#endif 
