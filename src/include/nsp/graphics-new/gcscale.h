#ifndef NSP_GCSCALE_BCG 
#define NSP_GCSCALE_BCG 

/*
 * This Software is GPL (Copyright ENPC 2009-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 *
 * The following functions perform scale changes and then redirect 
 * to the graphics driver.
 *
 * structure for storing scale informations associated to an 
 * axes. This should be similar to the same structure in Xgc
 * which will be removed in the future.
 */

typedef struct wcscalelist window_scale_list;
typedef struct wcscalelist nsp_gcscale;

struct wcscalelist 
{
  int    scale_flag ;                   /* zero when this is a default scale */
  int    scale_flag3d ;                 /* set to != 0 when 3d scales are active */
  int  scale_3drot_flag;                /* test */
  int    wdim[2];                       /* currend windo dim in pixels */
  double subwin_rect[4];                /* subwindow specification */
  double frect[4];                      /* bounds in the <<real>> space: xmin,ymin,xmax,ymax */
  double zfrect[2];                     /* zmin,zmax after transformation for plot3d */
  double axis[4];                       /* position of the axis rectangle */
                                        /* = [mfact_xl, mfact_xr,mfact_yu,mfact_yd]; */
  double xtics[4],ytics[4];             /* [xmin,ymin,nint] or [kmin,kmax,ar,nint]           */
  char   strflag;                       /* the default mode used by stored graphics: 
					 * supposed to be 1 or 3 or 5 
					 */
  double Wxofset1,Wyofset1,Wscx1,Wscy1; /* ofsets and scale factor for pixel<->double transf.*/
  char logflag[2];                      /* are we using logscale */
  GdkRectangle Irect;                   /* WIRect1[4]; frame bounds in pixel */
  int Waaint1[4];                       /* tics and subtics numbers: [xint,xsubint,yint,ysubint] */
  double m[3][3];                       /* 3d geometric transformation */
  double bbox1[6];                      /* 3d bounds */
  double c[3] ;                          /* center of 3d box */
  double alpha,theta;                   /* polar coordinates of visualization point */
  int metric3d;                         /* added by es - metric mode  for 3d -> 2d */
  double cosa,sina;                     /* test ! */
  window_scale_list *next;             /* points to next one */
  window_scale_list *prev;             /* points to previous one */
};

extern void nsp_scale_copy(nsp_gcscale *scale1,nsp_gcscale *scale2);
extern void nsp_scale_default(nsp_gcscale *scale1);

#endif 
