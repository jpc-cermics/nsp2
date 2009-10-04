/*-------------BEGIN--------------------------------------------------------
---------------------------------------------------------------------------*/

 extern void C2F(drawarc)(char *str, int *x, int *y, int *width, int *height, int *angle1, int *angle2, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(fillarcs)(char *str, int *vects, int *fillvect, int *n, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(drawarcs)(char *str, int *vects, int *style, int *n, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(fillpolyline)(char *str, int *n, int *vx, int *vy, int *closeflag, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(drawaxis)(char *str, int *alpha, int *nsteps, int *v2, int *initpoint, int *v6, int *v7, double *size, double *dx2, double *dx3, double *dx4);
 extern void C2F(cleararea)(char *str, int *x, int *y, int *w, int *h, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(clearwindow)(char *v1, int *v2, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(xclick)(char *str, int *ibutton, int *x1, int *yy1, int *iflag, int *istr, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(xclick_any)(char *str, int *ibutton, int *x1, int *yy1, int *iwin, int *iflag, int *istr, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(xgetmouse)(char *str, int *ibutton, int *x1, int *yy1, int *iflag, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(xend)(char *v1, int *v2, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(fillarc)(char *str, int *x, int *y, int *width, int *height, int *angle1, int *angle2, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(viderbuff)();
 extern void C2F(fillrectangle)(char *str, int *x, int *y, int *width, int *height, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(MissileGCget)(char *str, int *verbose, int *x1, int *x2, int *x3, int *x4, int *x5, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(initgraphic)(char *string, int *v2, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(drawpolyline)(char *str, int *n, int *vx, int *vy, int *closeflag, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(fillpolylines)(char *str, int *vectsx, int *vectsy, int *fillvect, int *n, int *p, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(drawpolylines)(char *str, int *vectsx, int *vectsy, int *drawvect, int *n, int *p, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(drawpolymark)(char *str, int *n, int *vx, int *vy, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(displaynumbers)(char *str, int *x, int *y, int *v1, int *v2, int *n, int *flag, double *z, double *alpha, double *dx3, double *dx4);
 extern void C2F(xpause)(char *str, int *sec_time, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(drawrectangle)(char *str, int *x, int *y, int *width, int *height, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(drawrectangles)(char *str, int *vects, int *fillvect, int *n, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(drawsegments)(char *str, int *vx, int *vy, int *n, int *style, int *iflag, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(xselgraphic)(char *v1, int *v2, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(MissileGCset)(char *str, int *x1, int *x2, int *x3, int *x4, int *x5, int *x6, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(displaystring)(char *string, int *x, int *y, int *v1, int *flag, int *v6, int *v7, double *angle, double *dv2, double *dv3, double *dv4);
 extern void C2F(boundingbox)(char *string, int *x, int *y, int *rect, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(drawarrows)(char *str, int *vx, int *vy, int *n, int *as, int *style, int *iflag, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(loadfamily)(char *name, int *j, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(queryfamily)(char *name, int *j, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(xinfo)(char *message, int *v2, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4);
 extern void C2F(setpopupname)(char *x0, int *v2, int *v3, int *v4, int *v5, int *v6, int *v7, double *dv1, double *dv2, double *dv3, double *dv4); 

/*---------------------END------------------------------*/


