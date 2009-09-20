#ifndef NSP_PLOT3DOBJ 
#define  NSP_PLOT3DOBJ 

/* plotting a set of 3d objects routine for Nsp 
 *
 * Copyright (C) 2005  Bruno Pincon
 *
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
 *
 * Author: Bruno Pincon 
 * Adapted to nsp internals and _ogl added 
 *   Jean-Philippe Chancelier Mars 2005 
 */

#define DEFAULT_FONT 2
#define DEFAULT_FONT_SIZE 2

#define LEFT -1
#define CENTER 0
#define RIGHT 1
#define DOWN -1
#define UP 1

#define FLAT 0
#define INTERP 1

enum BooleanEnum {BTRUE=1, BFALSE=0};
typedef enum BooleanEnum GBoolean;

enum VisionPosEnum {VIN=1, OUT_XY=0, OUT_Z=-1};
typedef enum VisionPosEnum VisionPos;

enum BoxStyleEnum {MATLAB=0, SCILAB=1, OTHER=2};
typedef enum BoxStyleEnum BoxStyle;

enum ObjTypeEnum {POLYHEDRON=0, POLYLINE=1, OBJPOINTS=2, STRING3D=3, SPOLYHEDRON=4};
typedef enum ObjTypeEnum ObjType;

typedef struct _hfstruct HFstruct ;

/* func_3dobj is shared by all 3d Object 
 * a 3d Object can be casted to func_3dobj with OBJ3D
 */

typedef void obj3d_draw_partial(BCG *Xgc,void *Obj,int j);
typedef void obj3d_draw_ogl(BCG *Xgc,void *Obj);
typedef void obj3d_free(void *Obj);
typedef void obj3d_zmean(void *Q,double *z, HFstruct *HF, int *n, int k);

typedef struct _generic_3dobj func_3dobj;

struct _generic_3dobj
{
  obj3d_draw_partial *draw_partial;
  obj3d_draw_ogl  *draw_ogl;
  obj3d_free *free;
  obj3d_zmean *zmean;
  int nb_coords;
  double *coord;
  VisionPos *pos;    /* precise la position dans le champs de visu */
} ;

#define OBJ3D(x) ((func_3dobj *) x)

typedef struct
{
  obj3d_draw_partial *draw_partial;
  obj3d_draw_ogl  *draw_ogl;
  obj3d_free *free;
  obj3d_zmean *zmean;
  int nb_coords;
  double *coord;
  VisionPos *pos;    /* precise la position dans le champs de visu */

  /* */
  int nb_faces;
  int nb_vertices_per_face;
  int *face;
  int nb_colors;  /* actuellement une seule couleur par face */
  int *color;
  int nb_back_colors;
  int *back_color;
  GBoolean with_mesh;
  /* keep some matrices for freeing allocated space */
  NspMatrix *Mcoord; /* les coordonnees */
  NspMatrix *Mface; 
  NspMatrix *Mcolor;
  NspMatrix *Mback_color;
} Polyhedron;

typedef struct
{
  obj3d_draw_partial *draw_partial;
  obj3d_draw_ogl  *draw_ogl;
  obj3d_free *free;
  obj3d_zmean *zmean;
  int nb_coords;
  double *coord;
  VisionPos *pos;    /* precise la position dans le champs de visu */
  /* */
  int nb_faces;
  int nb_vertices_per_face;
  int *face;
  double *val;  /* func values associated to each vertex */
  int nb_levels;
  double vmin;
  double vmax;
  double *vlevel;
  int *fill; /* fill[0] = colout_min, fill[1] = col_min, ..., fill[nb_levels] = col_max, fill[nb_levels+1] = colout_max */
  int back_color;
  GBoolean with_mesh;
  /* keep some matrices for freeing allocated space */
  NspMatrix *Mcoord,*Mface,*Mval,*Mvalminmax,*Mcolminmax,*Mcolout;
} SPolyhedron;


typedef struct 
{
  obj3d_draw_partial *draw_partial;
  obj3d_draw_ogl  *draw_ogl;
  obj3d_free *free;
  obj3d_zmean *zmean;
  int nb_coords;
  double *coord;
  VisionPos *pos;
  /* */
  int nb_colors;
  int *color;
  NspMatrix *Mcoord,*Mcolor;
} PolyLine;         /* ajouter éventuellement une épaisseur pour la ligne */



typedef struct 
{
  obj3d_draw_partial *draw_partial;
  obj3d_draw_ogl  *draw_ogl;
  obj3d_free *free;
  obj3d_zmean *zmean;
  int nb_coords;
  double *coord;
  VisionPos *pos;
  /* */
  int color;
  int mark_type;   /* un entier de [0,9] */
  NspMatrix *Mcoord;
} Points;

typedef struct 
{
  obj3d_draw_partial *draw_partial;
  obj3d_draw_ogl  *draw_ogl;
  obj3d_free *free;
  obj3d_zmean *zmean;
  int nb_coords;
  double *coord;
  VisionPos *pos;
  /* */
  char *str;
  int font_type;
  int font_size;
  NspMatrix *Mcoord;
} String3d;


/* this one is treated separately and do not share 
 * func_3dobj
 */

typedef struct
{
  obj3d_draw_partial *draw_partial;
  obj3d_draw_ogl  *draw_ogl;
  obj3d_free *free;
  double coord[24];
  const int *face;
  const int *segment;
  VisionPos pos[8];
  int color;
  int inear;
  GBoolean with_ticks;
  BoxStyle box_style;
  double *others_coord;
  VisionPos *others_pos;
  double *ticks_coord;
  VisionPos *ticks_pos;
  int nb_xticks;
  double *xticks;
  int nb_yticks;
  double *yticks;
  int nb_zticks;
  double *zticks;
  int nb_xyz_ticks;
} Plot3dBox;


typedef struct 
{
  ObjType obj_type;
  void *obj;
} Obj3d;


struct _hfstruct
{
  int num_obj;
  int num_in_obj;
};

extern void **obj3d_from_list_old(Stack stack,NspList *L,int alloc_objs,int *err,int *nf,int *nbObj) ;


#endif
