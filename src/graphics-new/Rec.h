/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2000 Enpc/Jean-Philippe Chancelier
 *    jpc@cereve.enpc.fr 
 * --------------------------------------------------------------------------*/

#ifndef SCI_REC
#define SCI_REC 

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
  integer   n1,n2,*style,*aint;
  char *legend,*strflag;
  char *strflag_kp;
  integer *aint_kp;
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

struct rec_xgrid { int code;  integer style ; } ;

/*---------------------------------------------------------------------
 * param3d 
 *---------------------------------------------------------------------------*/

struct rec_param3d {int code;
		    double *x,*y,*z,*bbox;
		    integer   n,*flag;
		    double teta,alpha;
		    char  *legend;
		 } ;

struct rec_param3d1 {int code;
		    double *x,*y,*z,*bbox;
		    integer   n,m,iflag,*colors,*flag;
		    double teta,alpha;
		    char  *legend;
		 } ;

/*---------------------------------------------------------------------
 * plot3d 
 *---------------------------------------------------------------------------*/

struct rec_plot3d {int code;
		   double *x,*y,*z,*bbox;
		   integer   p,q,*flag;
		   double teta,alpha;
		   char  *legend;
		 } ;

/*---------------------------------------------------------------------
 * fac3d 
 *---------------------------------------------------------------------------*/

struct rec_fac3d {int code;
		   double *x,*y,*z,*bbox;
		   integer   p,q,*flag,*cvect;
		   double teta,alpha;
		   char  *legend;
		 } ;

/*---------------------------------------------------------------------
 * fec 
 *---------------------------------------------------------------------------*/


struct rec_fec {int code;
		double *x,*y,*triangles,*func;
		integer   Nnode,Ntr;
                double *brect, *zminmax;      /* zminmax added by bruno */
                integer  *aaint, *colminmax;  /* colminmax added by bruno */
		char  *legend,*strflag;
		char *strflag_kp;
		double *brect_kp;
		integer *aaint_kp;
	      } ;

/*---------------------------------------------------------------------
 * contour 
 *---------------------------------------------------------------------------*/
/** general **/
struct rec_contour {int code;
		    double *x,*y,*z,*zz,zlev;
		    integer   n1,n2,nz,flagnz;
		    double *bbox;
		    double teta,alpha;
		    integer *flag;
		    char  *legend;
		 } ;

/** version 2d **/

struct rec_contour2d {int code;
  double *x,*y,*z,*zz;
  integer   n1,n2,nz,flagnz;
  double *brect;
  integer *style,*aint;
  char *legend,*strflag;
  char *strflag_kp;
  integer *aint_kp;
  double *brect_kp;
} ;

/*---------------------------------------------------------------------
 * xgray 
 *---------------------------------------------------------------------------*/

struct rec_gray {int code;
  char *strflag;
  double *x,*y,*z,*brect;
  integer   n1,n2,*aaint;
  integer *aaint_kp;
  double *brect_kp;
  char *strflag_kp;
} ;

struct rec_gray_2 {int code;
		 double *z,*xrect;
		 integer   n1,n2;
		 } ;

/*---------------------------------------------------------------------
 * xchamp  
 *---------------------------------------------------------------------------*/

struct rec_champ {int code;
		  double *x,*y,*fx,*fy,*vrect,arfact;
		  integer   n1,n2;
		  char *strflag;
		  char *strflag_kp;
		  double *vrect_kp;
} ;

/*---------------------------------------------------------------------
 * doubly linked list for graphic storage 
 *---------------------------------------------------------------------------*/

/* all the plot records can be cast to a plot code */

typedef struct _plot_code {
  int code; 
} plot_code ; 

typedef struct _listplot {
  int  window;
  void *theplot; 
  struct _listplot   *next;
  struct _listplot   *previous;
} list_plot ;


extern list_plot *ListPFirst;


typedef void  (*record_action) (void*);  /*action */
typedef void  (*record_scale_change)(void *plot, int *flag, double *bbox, int *aaint, int undo, int *bbox1, double *subwin, int win_num);
typedef void  (*record_new_angles)(void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox);


typedef  struct  {
  int  code ;        /* Indice in the table */
  void  (*replay) (void*);  /*action */
  void  (*clean) (void*);  /*action */
  void  (*unscale) (void*);  /*action */
  void  (*scale_change)(void *plot, int *flag, double *bbox, int *aaint, int undo, int *bbox1, double *subwin, int win_num);
  void  (*new_angles)(void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox);

} Record_table ;


int store_record(int code ,void *plot);
void store_clipping_p(int x,int y,int w,int h);
void store_clipgrf(void);
void store_alufunction1(int val);
void store_background(int val);
void store_unclip(void);
void store_clip(double x[]);
void store_pattern(int val);
void store_font_size(int val);
void store_font(int val,int val1);
void store_foreground(int val);
void store_hidden3d(int val);
void store_absourel(int val);
void store_dash(int val);
void store_mark_size(int val);
void store_mark(int val,int val1);
void store_pixmapOn(int val);
void store_thickness(int val);
void store_usecolor(int val);
void store_show(void);
void store_pixmapclear(void);
void store_drawarc_1(double arc[]);
void store_fillarcs_1(double vects[],int fillvect[], int n);
void store_drawarcs_1(double vects[], int style[], int n);
void store_fillpolyline_1(double *vx, double *vy,int n,int closeflag);
void store_drawarrows_1(double vx[],double vy[],int n,double as, int style[], int iflag);
void store_drawaxis_1(double *alpha, int *nsteps,double *initpoint, double *size);
void store_cleararea_1(double x, double y, double w, double h);
void store_fillarc_1( double arc[]);
void store_fillrectangle_1(double rect[]);
void store_drawpolyline_1( double *vx, double *vy ,int n, int closeflag);
void store_fillpolylines_1( double *vx, double *vy, int *fillvect, int n, int p, int v1);
void store_drawpolymark_1(double *vx, double *vy,int n);
void store_displaynumbers_1(double *x, double *y,int n, int flag,double *z, double *alpha);
void store_drawpolylines_1(double *vx, double *vy, int *drawvect,int n, int p);
void store_drawrectangle_1(double rect[]);
void store_drawrectangles_1(double vects[],int fillvect[], int n);
void store_drawsegments_1(double *vx, double *vy,int n, int *style, int iflag);
void store_displaystring_1(char *string,double x, double y,int flag,double angle);
void store_displaystringa_1(char *string, int ipos);
void store_xstringb_1(char *str,int *fflag, double *xd, double *yd, double *wd, double *hd);

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
#define CODEColormap             68     
#define CODEendplots             69 

#ifdef  REC_PRIVATE      

static void replay_clipping_p(void *theplot);
static void replay_clipgrf( void *theplot);
static void replay_alufunction1(void *theplot);
static void replay_background(void *theplot);
static void replay_unclip( void *theplot);
static void replay_clip(void *theplot);
static void replay_pattern(void *theplot);
static void replay_font_size(void *theplot);
static void replay_font(void *theplot);
static void replay_foreground(void *theplot);
static void replay_hidden3d(void *theplot);
static void replay_absourel(void *theplot);
static void replay_dash(void *theplot);
static void replay_mark_size(void *theplot);
static void replay_mark(void *theplot);
static void replay_pixmapOn(void *theplot);
static void replay_thickness(void *theplot);
static void replay_usecolor(void *theplot);
static void replay_show( void *theplot);
static void replay_pixmapclear( void *theplot);
static void replay_fpf(void *theplot);
static void replay_fpf_def(void *theplot);
static void replay_drawarc_1(void  *theplot);
static void replay_fillarcs_1(void  *theplot);
static void replay_drawarcs_1(void  *theplot);
static void replay_fillpolyline_1(void  *theplot);
static void replay_drawarrows_1(void  *theplot);
static void replay_drawaxis_1(void  *theplot);
static void replay_cleararea_1(void  *theplot);
static void replay_fillarc_1(void  *theplot);
static void replay_fillrectangle_1(void  *theplot);
static void replay_drawpolyline_1(void  *theplot);
static void replay_fillpolylines_1(void  *theplot);
static void replay_drawpolymark_1(void  *theplot);
static void replay_displaynumbers_1(void  *theplot);
static void replay_drawpolylines_1(void  *theplot);
static void replay_drawrectangle_1(void  *theplot);
static void replay_drawrectangles_1(void  *theplot);
static void replay_drawsegments_1(void  *theplot);
static void replay_displaystring_1(void  *theplot);
static void replay_displaystringa_1(void  *theplot);
static void replay_xstringb_1(void  *theplot);
static void replay_Ech(void *theplot);
static void replay_NEch(void *theplot);
static void replay_Plot(void * theplot);
static void replay_Plot1(void * theplot);
static void replay_Plot2(void * theplot);
static void replay_Plot3(void * theplot);
static void replay_Plot4(void * theplot);
static void replay_SciAxis(void *theplot);
static void replay_Grid(void *theplot);
static void replay_Param3D(void *theplot);
static void replay_Param3D1(void *theplot);
static void replay_3D(void *theplot);
static void replay_3D1(void *theplot);
static void replay_Fac3D(void *theplot);
static void replay_Fac3D1(void *theplot);
static void replay_Fac3D2(void *theplot);
static void replay_Fac3D3(void *theplot);
static void replay_Fec(void *theplot);
static void replay_Contour(void *theplot);
static void replay_Contour2D(void *theplot);
static void replay_Gray(void *theplot);
static void replay_Gray1(void *theplot);
static void replay_Gray2(void *theplot);
static void replay_Champ(void *theplot);
static void replay_Champ1(void *theplot);

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
static void clean_Fac3D(void *theplot);
static void clean_Fec(void *theplot);
static void clean_Contour(void *theplot);
static void clean_Contour2D(void *theplot);
static void clean_Gray(void *theplot);
static void clean_Gray2(void *theplot);
static void clean_Champ(void *theplot);

static void scale_change_Plot(void *plot, int *flag, double *bbox, int *aaint, int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Contour2D(void *plot, int *flag, double *bbox, int *aaint, int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Gray(void *plot, int *flag, double *bbox, int *aaint, int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Champ(void *plot, int *flag, double *bbox, int *aaint, int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Fec(void *plot, int *flag, double *bbox, int *aaint, int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Ech(void *plot, int *flag, double *bbox, int *aaint, int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_NEch(void *plot, int *flag, double *bbox, int *aaint, int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Contour(void *plot, int *flag, double *b1, int *aaint, int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Fac3D(void *plot, int *flag, double *b1, int *aaint, int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Param3D(void *plot, int *flag, double *b1, int *aaint, int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Param3D1(void *plot, int *flag, double *b1, int *aaint,  int undo, int *bbox1, double *subwin, int win_num);
static void scale_change_Plot3D(void *plot, int *flag, double *b1, int *aaint,  int undo, int *bbox1, double *subwin, int win_num);

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

static void new_angles_Plot3D(void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox);
static void new_angles_Fac3D(void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox);
static void new_angles_Contour(void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox);
static void new_angles_Param3D(void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox);
static void new_angles_Param3D1(void *plot, double *theta, double *alpha, int *iflag, int *flag, double *bbox);


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
  {CODESciAxis		     , replay_SciAxis		,clean_SciAxis		  ,NULL,NULL,NULL},  	     
  {CODEGrid		     , replay_Grid     		,clean_Grid		  ,NULL,NULL,NULL},  	     
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
  {CODEGray1		     , replay_Gray1    		,clean_Gray		  ,unscale_Gray		   ,scale_change_Gray		  ,NULL},
  {CODEGray2		     , replay_Gray2    		,clean_Gray2		  ,unscale_Gray	           ,scale_change_Gray		  ,NULL},
  {CODEChamp		     , replay_Champ    		,clean_Champ		  ,unscale_Champ	   ,scale_change_Champ		  ,NULL},     
  {CODEChamp1		     , replay_Champ1            ,clean_Champ              ,unscale_Champ           ,scale_change_Champ            ,NULL},
  {CODEfpf_def   	     , replay_fpf_def           ,NULL                     ,NULL                    ,NULL                          ,NULL},
  {CODEfpf                   , replay_fpf               ,clean_fpf                ,NULL                    ,NULL                          ,NULL} 
};     	

#endif /*  REC_PRIVATE */

#ifndef REC_PRIVATE 
extern  Record_table record_table [];
#endif 
							                                     
#endif 
