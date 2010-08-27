/* -*- Mode: C -*- */
#ifndef NSP_INC_GRCommon
#define NSP_INC_GRCommon

/*
 * Copyright (C) 2009-2010 Jean-Philippe Chancelier Enpc/Cermics
 *                         Bruno Pincon Esial/Iecn
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/* Figure */
#include "../graphics-new/Plo3dObj.h"

extern BCG *nsp_check_graphic_context(void);
extern void store_graphic_object(BCG *Xgc,NspObject *obj);
extern void fillpolylines3D(BCG *Xgc,double *vectsx, double *vectsy, double *vectsz, int *fillvect,int n, int p); 
/* extern  int nsp_obj3d_orientation(int x[], int y[], int n); */
extern void nsp_figure_force_redraw(nsp_figure *F,void *rect);
extern void apply_transforms(BCG *Xgc,double Coord[],const double *M, VisionPos pos[],const double lim[], int ncoord);
extern void apply_transforms_new(BCG *Xgc,double Coord[],const double *M, VisionPos pos[],const double lim[], int ncoord);
#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 
extern NspPolyhedron *nsp_polyhedron_create_from_triplet(char *name,double *x,double *y,double *z,int m,int n);
extern NspSPolyhedron *nsp_spolyhedron_create_from_triplet(char *name,double *x,double *y,double *z,int m,int n,double *col, int ncol);
extern NspMatrix *nsp_surf_to_faces(const char *name,double *x,int xmn,double *y,int ymn)  ;
extern NspMatrix *nsp_surf_to_coords(const char *name,double *x,double *y,double *z,int m,int n);
extern int nsp_facets_to_faces(double *x,double *y,double *z,int *colors,int ncol,int m,int n,
			       NspMatrix **Cr,NspMatrix **Fr,NspMatrix **Colr );



extern void apply_transforms_new(BCG *Xgc,double Coord[],const double *M, VisionPos pos[],const double lim[], int ncoord);
extern void apply_transforms_new1(BCG *Xgc,double Coord[],const double *M, VisionPos pos[],const double lim[], int ncoord);

extern void fillpolylines3D_shade(BCG *Xgc,double *vectsx, double *vectsy, double *vectsz, int *fillvect,int n, int p);

extern int nsp_obj3d_orientation(int x[], int y[], int n);


#endif 
