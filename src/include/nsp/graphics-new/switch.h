#ifndef NSP_SWITCH_NEW_H
#define NSP_SWITCH_NEW_H

/*
 * This Software is GPL (Copyright ENPC 2009-2019)
 * Jean-Philippe Chancelier Enpc/Cermics
 *
 * The following functions perform scale changes and then redirect
 * to the graphics driver.
 */

extern int use_new_graphics;
extern void nsp_gr_new_raise(int win_num);
extern int nsp_gr_new_change(int win_num);
extern int nsp_graphic_new_new(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,double *viewport_pos,int *wpos);
extern int nsp_graphic_new_cairo_new(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,double *viewport_pos,int *wpos);
#ifdef WITH_OPENGL
extern int nsp_graphic_new_gl_new(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim, double *viewport_pos,int *wpos);
#endif
#endif
