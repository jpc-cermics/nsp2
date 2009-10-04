
/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2000 Enpc/Jean-Philippe Chancelier
 *    jpc@cereve.enpc.fr 
 --------------------------------------------------------------------------*/
/*------------------------------------------------------------------------
 *    SCILAB GIF Output based on GD Library from: http://www.boutell.com/gd
 *   Modelled after the original PostScript Driver in periPos.c
 *   Copyright (C) 1999, Tom Leitner, tom@finwds01.tu-graz.ac.at
 *--------------------------------------------------------------------------*/

 extern void  C2F(drawarcGif)(char *str, int *x, int *y, int *width, int *height, int *angle1, int *angle2, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(fillarcsGif)(char *str, int *vects, int *fillvect, int *n, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(drawarcsGif)(char *str, int *vects, int *style, int *n, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(fillpolylineGif)(char *str, int *n, int *vx, int *vy, int *closeareaflag, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(drawaxisGif)(char *str, int *alpha, int *nsteps, int *v2, int *initpoint, int *v6, int *v7, double *size, double *dx2, double *dx3, double *dx4);
 extern void  C2F(clearareaGif)(char *str, int *x, int *y, int *w, int *h, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(clearwindowGif)(char *v1, int *v2, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(xclickGif)(char *str, int *ibutton, int *xx1, int *yy1, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(xclick_anyGif)(char *str, int *ibutton, int *xx1, int *yy1, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(xgetmouseGif)(char *str, int *ibutton, int *xx1, int *yy1, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(xendGif)(char *v1, int *v2, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(fillarcGif)(char *str, int *x, int *y, int *width, int *height, int *angle1, int *angle2, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(fillrectangleGif)(char *str, int *x, int *y, int *width, int *height, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(scilabgcgetGif)(char *str, int *verbose, int *x1, int *x2, int *x3, int *x4, int *x5, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(initgraphicGif)(char *string, int *v2, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(drawpolylineGif)(char *str, int *n, int *vx, int *vy, int *closeflag, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(fillpolylinesGif)(char *str, int *vectsx, int *vectsy, int *fillvect, int *n, int *p, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(drawpolylinesGif)(char *str, int *vectsx, int *vectsy, int *drawvect, int *n, int *p, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(drawpolymarkGif)(char *str, int *n, int *vx, int *vy, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(displaynumbersGif)(char *str, int *x, int *y, int *v1, int *v2, int *n, int *flag, double *z, double *alpha, double *dx3, double *dx4);
 extern void  C2F(xpauseGif)(char *str, int *sec_time, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(drawrectangleGif)(char *str, int *x, int *y, int *width, int *height, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(drawrectanglesGif)(char *str, int *vects, int *fillvect, int *n, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(drawsegmentsGif)(char *str, int *vx, int *vy, int *n, int *style, int *iflag, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(xselgraphicGif)(char *v1, int *v2, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(scilabgcsetGif)(char *str, int *x1, int *x2, int *x3, int *x4, int *x5, int *x6, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(set_driverGif)();
 extern void  C2F(displaystringGif)(char *string, int *x, int *y, int *v1, int *flag, int *v6, int *v7, double *angle, double *dv2, double *dv3, double *dv4);
 extern void  C2F(boundingboxGif)(char *string, int *x, int *y, int *rect, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(drawarrowsGif)(char *str, int *vx, int *vy, int *n, int *as, int *style, int *iflag, double *dv1, double *dv2, double *dv3, double *dv4); 
 extern void  C2F(loadfamilyGif)(char *name, int *j, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void  C2F(queryfamilyGif)(char *name, int *j, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);

