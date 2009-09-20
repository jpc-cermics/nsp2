/* plotting a set of 3d objects routine for Nsp 
 *
 * Copyright (C) 2008-2009  Bruno Pincon
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

#include <string.h> /* in case of dbmalloc use */
#include <math.h>
#include <stdio.h>

#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"
#include "nsp/interf.h"
#include "Plo3dObj.h"

extern Stack SciStack; 

#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 

/* This is to be fixed 3D primitives are to be added 
 * in the graphic_engines.
 */
extern void nsp_obj3d_draw_near_box_segments_old(BCG *Xgc,Plot3dBox *B);
extern void fillpolylines3D(BCG *Xgc,double *vectsx, double *vectsy, double *vectsz, int *fillvect,int n, int p); 
extern void fillpolylines3D_shade(BCG *Xgc,double *vectsx, double *vectsy, double *vectsz, int *fillvect,int n, int p);
extern void drawpolylines3D(BCG *Xgc,double *vectsx, double *vectsy, double *vectsz, int *drawvect,int n, int p);
extern void drawsegments3D(BCG *Xgc,double *x,double *y,double *z, int n, int *style, int iflag);
extern int gr_compute_ticks(double *xminv, double *xmaxv, double *grads, int *ngrads);

static  int nsp_obj3d_orientation_old(int x[], int y[], int n);
static Plot3dBox* make_box_old(BCG *Xgc,double Box[], GBoolean with_ticks,
			       BoxStyle box_style,int box_color, double lim[]);
static void apply_transforms_old(BCG *Xgc,double Coord[],const double *M,
				 VisionPos pos[],const double lim[],int ncoord);
static void nsp_obj3d_draw_box(BCG *Xgc,Plot3dBox *B);
static void nsp_obj3d_dsortc(double x[], int *n, int p[]);
static  void nsp_obj3d_free_box(Plot3dBox *B);
static int select_box_vertex(const double coord[]);
static void draw_segment(BCG *Xgc,double coord[], int ia, int ib, int color);
static void draw_segment_bis(BCG *Xgc,double coord[], int ns, int color);
static void draw_justified_string(BCG *Xgc,char *str, double xx, double yy, int xj, int yj);
static void draw_far_box_segments(BCG *Xgc,Plot3dBox *B);
static void draw_box_face(BCG *Xgc,Plot3dBox *B, int j);
static void draw_tick(BCG *Xgc,Plot3dBox *B,double val,const double coord[]);

static void compute_ticks(double *vmin, double *vmax, double **Ticks, int *Nb_ticks);
static int build_ticks_segment(Plot3dBox *B, double xmin, double xmax, 
			       double ymin, double ymax, double zmin, double zmax);
static void build_xtick_seg(double *coord, int *num_sg, double x, double axe[], double sens[]);
static void build_ytick_seg(double *coord, int *num_sg, double y, double axe[], double sens[]);
static void build_ztick_seg(double *coord, int *num_sg, double z, double axe[], double sens[]);
static int  build_box_others_segment(Plot3dBox *B, double xmin, double xmax, double ymin, double ymax, double zmin, double zmax);
static void build_x_seg(double *coord, int *num_sg, double xmin, double xmax, double y, double z);
static void build_y_seg(double *coord, int *num_sg, double x, double ymin, double ymax, double z);
static void build_z_seg(double *coord, int *num_sg, double x, double y, double zmin, double zmax);
static void permut_of_sort(int *tab, int *perm);
static void find_intersection(int *sx, int *sy, double *fxy, double z, 
			      int inda, int indb, int *xint, int *yint);
static int zone(double val, double valmin, double valmax, int nv);
static void interp_color_triangle(BCG *Xgc,int *x, int *y, double *v, int *z, double *zlevel, int *fill);

static void init_Obj3d(Obj3d Obj[], int nbObj);
static void free_Obj3d(Obj3d Obj[], int nbObj);
static int get_polyhedron(Stack *stack,int k,NspHash *H,Polyhedron *Q,int *nf);
static obj3d_free  free_polyhedron;
static obj3d_draw_partial draw_polyhedron_face;
static obj3d_draw_ogl draw_polyhedron_ogl;
static obj3d_free free_polyhedron;
static obj3d_zmean zmean_faces_for_Polyhedron;

static int get_spolyhedron(Stack *stack,int k,NspHash *H,SPolyhedron *Q,int *nf);
static obj3d_free  free_spolyhedron;
static obj3d_draw_partial draw_spolyhedron_face;
static obj3d_draw_ogl draw_spolyhedron_ogl;
static obj3d_free free_spolyhedron;
static obj3d_zmean zmean_faces_for_SPolyhedron;

static int get_polyline(Stack *stack,int k,NspHash *H,PolyLine *L,int *nf);
static obj3d_free  free_polyline;
static obj3d_draw_partial draw_polyline_segment;
static obj3d_draw_ogl draw_polyline_ogl;
static obj3d_free free_polyline;
static obj3d_zmean zmean_segments_for_polyline;

static int get_points(Stack *stack,int k,NspHash *H,Points *P,int *nf);
static obj3d_free  free_points;
static obj3d_draw_partial draw_point;
static obj3d_draw_ogl draw_points_ogl;
static obj3d_free free_points;
static obj3d_zmean zmean_for_Points;

static int get_string3d(Stack *stack,int k,NspHash *H,String3d *S,int *nf);
static obj3d_free  free_string3d;
static obj3d_draw_partial draw_string3d;
static obj3d_draw_ogl draw_string3d_ogl;
static obj3d_free free_string3d;
static obj3d_zmean zmean_for_string3d;

#ifdef  WITH_GTKGLEXT 
static void nsp_draw_3d_obj_ogl( BCG *Xgc,void *Lo,double *theta,double *alpha,const char *legend,
				 int *flag,double *ebox,int with_mesh1,int with_box,int box_color,int box_style);
#endif 

static void draw_justified_string3d_ogl(BCG *Xgc,String3d *S, int xj, int yj);
static void draw_justified_string3d(BCG *Xgc,String3d *S, int xj, int yj);

/* des  variables globales... */
GBoolean with_mesh;  /*  actuellement soit on dessine tous les polyedres
		      *  avec ou sans le maillage mais il serait possible
		      *  d'en avoir certains avec le maillage et d'autres sans
		      *  cf variable with_mesh des objets polyedres a ce niveau
		      */

int display_mode = INTERP;
int ticks_font_type = 2;
int ticks_font_size = 1;
int current_color = 1;
int current_mark = 1;
int foreground_color = 1;
int background_color = 1;

/*                      s0    s1    s2    s3    s4    s5    s6    s7    s8    s9    s10   s11  */
static int box_segments[24] = {0,2,  0,5,  0,6,  1,3,  1,4,  1,7,  2,4,  2,7,  3,5,  3,6,  4,6,  5,7};
static int box_faces[24] = {0,5,3,6,  0,6,4,2,  0,2,7,5,  1,7,2,4,  1,3,5,7,  1,4,6,3};


/* Walk through L and store elements in an array 
 * or just check that the given list is correct 
 *
 */

void **obj3d_from_list_old(Stack stack,NspList *L,int alloc_objs,int *err,int *nf,int *nbObj) 
{
  Obj3d *Obj= NULL;
  int num=0;
  *err= FALSE;
  Cell *C= L->first;
  if ( alloc_objs  == TRUE ) 
    {
      int nbObj =nsp_list_length(L);
      if ( nbObj <=  0)  return NULL;
      Obj = malloc( nbObj*sizeof(Obj3d) );
      if (! Obj)
	{ 
	  Scierror("%s: malloc failure \n",NspFname(stack));
	  return NULL;
	}
      init_Obj3d((Obj3d *) Obj, nbObj);
    }
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ && IsHash(C->O) )
	{
	  NspHash *H= (NspHash *) C->O;
	  NspObject *Ob;
	  if (nsp_hash_find(H,"type",&Ob) == FAIL) {*err= TRUE; return NULL ;}
	  if ( IsString(Ob) == FALSE) {*err= TRUE; return NULL ;}
	  if ( strcmp(((NspSMatrix *) Ob)->S[0],"polyhedron")==0) 
	    {
	      if ( alloc_objs == TRUE) 
		{
		  Obj[num].obj_type = POLYHEDRON;
		  Obj[num].obj = malloc( sizeof(Polyhedron) );
		  if ( get_polyhedron(&stack,num,H, (Polyhedron *) Obj[num].obj,nf) == FAIL)
		    return NULL;
		}
	    }
	  else if ( strcmp(((NspSMatrix *) Ob)->S[0],"spolyhedron")==0)
	    {
	      if ( alloc_objs == TRUE) 
		{
		  Obj[num].obj_type = SPOLYHEDRON;
		  Obj[num].obj = malloc( sizeof(SPolyhedron) );
		  if ( get_spolyhedron(&stack,num,H, (SPolyhedron *) Obj[num].obj,nf) == FAIL)
		    return NULL;
		}
	    }
	  else if ( strcmp(((NspSMatrix *) Ob)->S[0],"polyline")==0)
	    {
	      if ( alloc_objs == TRUE) 
		{
		  Obj[num].obj_type = POLYLINE;
		  Obj[num].obj = malloc( sizeof(PolyLine) );
		  if ( get_polyline(&stack,num,H, (PolyLine *) Obj[num].obj,nf) == FAIL)
		    return NULL;
		}
	    }
	  else if ( strcmp(((NspSMatrix *) Ob)->S[0],"points")==0)
	    {
	      if ( alloc_objs == TRUE) 
		{
		  Obj[num].obj_type = OBJPOINTS;
		  Obj[num].obj = malloc( sizeof(Points) );
		  if ( get_points(&stack,num,H, (Points *) Obj[num].obj,nf) == FAIL)
		    return NULL;
		}
	    }
	  else if ( strcmp(((NspSMatrix *) Ob)->S[0],"string3d")==0)
	    {
	      if ( alloc_objs == TRUE) 
		{
		  Obj[num].obj_type = STRING3D;
		  Obj[num].obj = malloc( sizeof(String3d) );
		  if ( get_string3d(&stack,num,H, (String3d *) Obj[num].obj,nf) == FAIL)
		    return NULL;
		}
	    }
	  else 
	    {
	      *err=TRUE;
	    }
	}
      C = C->next ;
      num++;
    }
  *nbObj = num;
  return (void **) Obj;
} 


/*   nsp_draw_3d_obj_old(Xgc,L,&theta,&alpha,leg1,iflag,ebox);*/


extern void nsp_draw_3d_obj_old( BCG *Xgc,void *Lo,double *theta,double *alpha,const char *legend,
			     int *flag,double *ebox,int with_mesh1,int with_box,int box_color,int box_style)
{
  NspList *Lobj = Lo;
  /* Stack stack;*/ /* just used for messages i.e NspFname(stack) */
  nsp_box_3d box;
  double x[2],y[2],zz[2],zzmin,zzmax;
  int two=2;
  double Box[6]={ebox[0],ebox[2],ebox[4],ebox[1],ebox[3],ebox[5]};
  int nf=0,err=0,nbObj;
  Obj3d *Obj;
  int i, j, k, n, *p;
  HFstruct *HF;
  double lim[3], *z;
  Plot3dBox *B=NULL;
  int flagx;

#ifdef WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      nsp_draw_3d_obj_ogl(Xgc,Lo,theta,alpha,legend,flag,ebox,with_mesh1,with_box,box_color,box_style);
      nsp_ogl_set_2dview(Xgc);
      return; 
    }
#endif

  /* NspFname(stack) ="drawobj"; */
  flagx = Xgc->graphic_engine->xget_last(Xgc);
  foreground_color = flagx+1;
  background_color = flagx+2;
  with_mesh = with_mesh1; 

  /* allocate a structure for drawing purpose 
   * The unchanged values are kept in Lobj
   */

  Obj = (Obj3d *)obj3d_from_list_old(SciStack,Lobj,TRUE,&err,&nf,&nbObj) ;

  if ( Obj == NULL ) return;

  x[0]= Box[0];y[0]= Box[1];zz[0]= Box[2];
  x[1]= Box[3];y[1]= Box[4];zz[1]= Box[5];

  nsp_plot3d_update_bounds(Xgc,"box",x,y,zz,&two,&two,theta,alpha,legend,&flag[1],
			   ebox,&zzmin,&zzmax,param3d_t);
  SetEch3d1(Xgc,&box,ebox,*theta,*alpha,(long)(flag[1]+1)/2);

#ifdef WITH_GTKGLEXT 
  /* transmit info to opengl pretending we are doing 2d !!! */
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      nsp_ogl_set_2dview(Xgc);
    }
#endif

  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    {
      nsp_gengine_record_old.store_3dobj(Xgc,Lobj,theta,alpha,legend,flag,ebox,with_mesh1,with_box,box_color,box_style);
    }
  
  lim[0] = 1.e+10;
  lim[1] = 1.e+10;
  lim[2] = - 1.e+10;

  /* fabrication de la boite et calcul de ses coordonnees ds le repere local */
  if ( with_box == TRUE ) B = make_box_old(Xgc,Box, BTRUE, box_style,box_color, lim);

  HF= malloc( nf * sizeof(HFstruct) );
  z = malloc( nf * sizeof(double) );
  p = malloc( nf * sizeof(int) );

  /* step 1 : for each object :
   *            a/ get the coordinates in the local repair
   *               and determines the pos of each point within the pyramidal
   *               visible region (IN, OUT_XY, OUT_Z)
   *            b/ then add the visible parts (faces, segments, points) in the z 
   *               and HF arrays for the hidden face algorithm (only partial visible 
   *               parts without any OUT_Z point are included)
   */
  n = 0;
  for ( k = 0 ; k < nbObj ; k++ )
    {
      func_3dobj *Q = OBJ3D(Obj[k].obj);
      apply_transforms_old(Xgc,Q->coord,Q->coord, Q->pos, lim, Q->nb_coords);
      Q->zmean(Q, z, HF, &n, k);
    }
 
  /*  step 3 : sort of all the a priori visible "faces" (faces, segments, points) */
  nsp_obj3d_dsortc(z, &n, p);

  /* step 4 : drawing of each faces */
  if ( with_box == TRUE  ) nsp_obj3d_draw_box(Xgc,B);

  for (i = n -1 ; i >= 0 ; i--)
    {
      k = HF[p[i]].num_obj;  /* numero de l'objet correspondant a cette "face" */
      j = HF[p[i]].num_in_obj; /* son numéro de face dans l'objet en question */
      /* dessin partiel de l'objet en utilisant la face j */
      OBJ3D(Obj[k].obj)->draw_partial(Xgc,Obj[k].obj,j);
    }
  if ( with_box == TRUE  &&  B->box_style == SCILAB )  nsp_obj3d_draw_near_box_segments_old(Xgc,B);
  if ( with_box == TRUE ) nsp_obj3d_free_box(B);
  free_Obj3d(Obj,nbObj);
  free(HF);
  free(z);
  free(p);
}

#ifdef  WITH_GTKGLEXT 

static void nsp_draw_3d_obj_ogl( BCG *Xgc,void *Lo,double *theta,double *alpha,const char *legend,
				 int *flag,double *ebox,int with_mesh1,int with_box,int box_color,int box_style)
{
  NspList *Lobj = Lo;
  /* Stack stack;*/ /* just used for messages i.e NspFname(stack) */
  nsp_box_3d box;
  double x[2],y[2],zz[2],zzmin,zzmax;
  double Box[6]={ebox[0],ebox[2],ebox[4],ebox[1],ebox[3],ebox[5]};
  Obj3d *Obj;
  int k, flagx, nf=0,err=0,nbObj, two=2;
  double lim[3];
  Plot3dBox *B;
  /* NspFname(stack) ="drawobj"; */
  flagx = Xgc->graphic_engine->xget_last(Xgc);
  foreground_color = flagx+1;
  background_color = flagx+2;
  with_mesh = with_mesh1; 

  /* allocate a structure for drawing purpose 
   * The unchanged values are kept in Lobj
   */

  Obj = (Obj3d *)obj3d_from_list_old(SciStack,Lobj,TRUE,&err,&nf,&nbObj) ;

  if ( Obj == NULL ) return;

  x[0]= Box[0];y[0]= Box[1];zz[0]= Box[2];
  x[1]= Box[3];y[1]= Box[4];zz[1]= Box[5];

  nsp_plot3d_update_bounds(Xgc,"box",x,y,zz,&two,&two,theta,alpha,legend,&flag[1],
			   ebox,&zzmin,&zzmax,param3d_t);
  SetEch3d1(Xgc,&box,ebox,*theta,*alpha,(long)(flag[1]+1)/2);

  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    {
      nsp_gengine_record_old.store_3dobj(Xgc,Lobj,theta,alpha,legend,flag,ebox,with_mesh1,with_box,box_color,box_style);
    }
  
  lim[0] = 1.e+10;
  lim[1] = 1.e+10;
  lim[2] = - 1.e+10;

  /* fabrication de la boite et calcul de ses coordonnees ds le repere local */
  if ( with_box == TRUE  )
    {
      B = make_box_old(Xgc,Box, BTRUE, box_style,box_color,lim);
      nsp_obj3d_draw_box(Xgc,B);
      if (B->box_style == SCILAB ) nsp_obj3d_draw_near_box_segments_old(Xgc,B);
      nsp_obj3d_free_box(B);
    }

  for (k=0; k < nbObj; k++)  OBJ3D(Obj[k].obj)->draw_ogl(Xgc,Obj[k].obj);
  free_Obj3d(Obj,nbObj);
}
#endif 


static Plot3dBox* make_box_old(BCG *Xgc,double Box[], GBoolean with_ticks, BoxStyle box_style,int box_color, double lim[])
{
#ifdef WITH_GTKGLEXT 
  double coord[24];
#endif
  Plot3dBox *B;
  double xmin, ymin, zmin, xmax, ymax, zmax;

  B = malloc(sizeof(Plot3dBox));

  xmin = Box[0]; ymin = Box[1]; zmin = Box[2];
  xmax = Box[3]; ymax = Box[4]; zmax = Box[5];
  
  if ( with_ticks )
    {
      B->with_ticks = BTRUE;
      compute_ticks(&xmin, &xmax, &(B->xticks), &(B->nb_xticks));
      compute_ticks(&ymin, &ymax, &(B->yticks), &(B->nb_yticks));
      if ( 1) /* -0.95 <= P[8]  &&  P[8] <= 0.95 ) */
	compute_ticks(&zmin, &zmax, &(B->zticks), &(B->nb_zticks));
      else
	{ B->nb_zticks = 0; B->zticks = NULL; }
      B->nb_xyz_ticks = B->nb_xticks + B->nb_yticks + B->nb_zticks;
    }
  else
    B->with_ticks = BFALSE;
    
  B->box_style = box_style;
  B->segment = box_segments;
  B->face = box_faces;

  if ( box_color == -1 )
    {

      /* last +3 is a light gray */
      box_color = Xgc->graphic_engine->xget_last(Xgc)+3;
    }

  B->color = box_color;
  
  B->coord[0]  = xmin; B->coord[1]  = ymin; B->coord[2]  = zmin; //1
  B->coord[3]  = xmax; B->coord[4]  = ymax; B->coord[5]  = zmax; //2
  B->coord[6]  = xmin; B->coord[7]  = ymin; B->coord[8]  = zmax; //3
  B->coord[9]  = xmax; B->coord[10] = ymax; B->coord[11] = zmin; //4
  B->coord[12] = xmax; B->coord[13] = ymin; B->coord[14] = zmax; //5
  B->coord[15] = xmin; B->coord[16] = ymax; B->coord[17] = zmin; //6
  B->coord[18] = xmax; B->coord[19] = ymin; B->coord[20] = zmin; //7
  B->coord[21] = xmin; B->coord[22] = ymax; B->coord[23] = zmax; //8

#ifdef WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      /* in open_gl we do not want to change coordinates */
      apply_transforms_old(Xgc,coord,B->coord, B->pos, lim, 8);
      B->inear = select_box_vertex(coord);       
    }
  else 
#endif
    {
      apply_transforms_old(Xgc,B->coord,B->coord, B->pos, lim, 8);
      B->inear = select_box_vertex(B->coord);      
    }
  
  if ( B->with_ticks )
    {
      build_ticks_segment(B, xmin, xmax, ymin, ymax, zmin, zmax);
      B->ticks_pos = malloc(2*(B->nb_xyz_ticks)*sizeof(VisionPos));
#ifdef WITH_GTKGLEXT 
      if ( Xgc->graphic_engine != &GL_gengine ) 
#endif	
	apply_transforms_old(Xgc, B->ticks_coord,B->ticks_coord, B->ticks_pos, lim, 2*(B->nb_xyz_ticks)); 
    }
  if ( B->with_ticks  &&  B->box_style == MATLAB )
    {
      build_box_others_segment(B, xmin, xmax, ymin, ymax, zmin, zmax);
      B->others_pos = malloc(4*(B->nb_xyz_ticks)*sizeof(VisionPos));
#ifdef WITH_GTKGLEXT 
      if ( Xgc->graphic_engine != &GL_gengine ) 
#endif	
	apply_transforms_old(Xgc, B->others_coord,B->others_coord, B->others_pos, lim, 4*(B->nb_xyz_ticks));
    }      
  return ( B );
}

static int  build_ticks_segment(Plot3dBox *B, double xmin, double xmax, 
				double ymin, double ymax, double zmin, double zmax)
{
  int i, sg=0;
  double d, e, axe_x[2], axe_y[2], axe_z[2], sens_x[2], sens_y[2], sens_z[2];

  d = (xmax-xmin + ymax-ymin + zmax-zmin)/100; /* a voir */
  e = d/sqrt(2.0);
  
  B->ticks_coord = malloc( 6*(B->nb_xyz_ticks)*sizeof(double) );
  if ( B->ticks_coord == NULL) return FAIL;
  switch(B->inear)
    {
    default:
    case(0):
      axe_x[0] = ymin; axe_x[1] = zmax; sens_x[0] =-d; sens_x[1] = 0;
      axe_y[0] = xmin; axe_y[1] = zmax; sens_y[0] =-d; sens_y[1] = 0;
      axe_z[0] = xmax; axe_z[1] = ymin; sens_z[0] = d; sens_z[1] = 0;
      sens_z[0] = e; sens_z[1] =-e;
      break;
    case(1):
      axe_x[0] = ymax; axe_x[1] = zmin; sens_x[0] = d; sens_x[1] = 0;
      axe_y[0] = xmax; axe_y[1] = zmin; sens_y[0] = d; sens_y[1] = 0;
      axe_z[0] = xmax; axe_z[1] = ymin; sens_z[0] = 0; sens_z[1] =-d;
      sens_z[0] = e; sens_z[1] =-e;
      break;
    case(2):
      axe_x[0] = ymin; axe_x[1] = zmin; sens_x[0] =-d; sens_x[1] = 0;
      axe_y[0] = xmin; axe_y[1] = zmin; sens_y[0] =-d; sens_y[1] = 0;
      axe_z[0] = xmin; axe_z[1] = ymax; sens_z[0] = 0; sens_z[1] = d;
      sens_z[0] =-e; sens_z[1] = e;
      break;
    case(3):
      axe_x[0] = ymax; axe_x[1] = zmax; sens_x[0] = d; sens_x[1] = 0;
      axe_y[0] = xmax; axe_y[1] = zmax; sens_y[0] = d; sens_y[1] = 0;
      axe_z[0] = xmin; axe_z[1] = ymax; sens_z[0] =-d; sens_z[1] = 0;
      sens_z[0] =-e; sens_z[1] = e;
      break;
    case(4):
      axe_x[0] = ymin; axe_x[1] = zmin; sens_x[0] =-d; sens_x[1] = 0;
      axe_y[0] = xmax; axe_y[1] = zmin; sens_y[0] = d; sens_y[1] = 0;
      axe_z[0] = xmin; axe_z[1] = ymin; sens_z[0] =-d; sens_z[1] = 0;
      sens_z[0] =-e; sens_z[1] =-e;
      break;
    case(5):
      axe_x[0] = ymax; axe_x[1] = zmax; sens_x[0] = d; sens_x[1] = 0;
      axe_y[0] = xmin; axe_y[1] = zmax; sens_y[0] =-d; sens_y[1] = 0;
      axe_z[0] = xmin; axe_z[1] = ymin; sens_z[0] = 0; sens_z[1] =-d;
      sens_z[0] =-e; sens_z[1] =-e;
      break;
    case(6):
      axe_x[0] = ymin; axe_x[1] = zmax; sens_x[0] =-d; sens_x[1] = 0;
      axe_y[0] = xmax; axe_y[1] = zmax; sens_y[0] = d; sens_y[1] = 0;
      axe_z[0] = xmax; axe_z[1] = ymax; sens_z[0] = 0; sens_z[1] = d;
      sens_z[0] = e; sens_z[1] = e;
      break;
    case(7):
      axe_x[0] = ymax; axe_x[1] = zmin; sens_x[0] = d; sens_x[1] = 0;
      axe_y[0] = xmin; axe_y[1] = zmin; sens_y[0] =-d; sens_y[1] = 0;
      axe_z[0] = xmax; axe_z[1] = ymax; sens_z[0] = d; sens_z[1] = 0;
      sens_z[0] = e; sens_z[1] = e;
      break;
    }

  for ( i = 0 ; i < B->nb_xticks ; i++)
    build_xtick_seg(B->ticks_coord, &sg, B->xticks[i], axe_x, sens_x);
  for ( i = 0 ; i < B->nb_yticks ; i++)
    build_ytick_seg(B->ticks_coord, &sg, B->yticks[i], axe_y, sens_y);
  for ( i = 0 ; i < B->nb_zticks ; i++)
    build_ztick_seg(B->ticks_coord, &sg, B->zticks[i], axe_z, sens_z);
  return OK;
}

static void build_xtick_seg(double *coord, int *num_sg, double x, double axe[], double sens[])
{
  int sg = *num_sg;
  coord[6*sg]   = x; coord[6*sg+1] = axe[0];         coord[6*sg+2] = axe[1];
  coord[6*sg+3] = x; coord[6*sg+4] = axe[0]+sens[0]; coord[6*sg+5] = axe[1]+sens[1];
  (*num_sg)++;
}

static void build_ytick_seg(double *coord, int *num_sg, double y, double axe[], double sens[])
{
  int sg = *num_sg;
  coord[6*sg]   = axe[0];         coord[6*sg+1] = y; coord[6*sg+2] = axe[1];
  coord[6*sg+3] = axe[0]+sens[0]; coord[6*sg+4] = y; coord[6*sg+5] = axe[1]+sens[1];
  (*num_sg)++;
}

static void build_ztick_seg(double *coord, int *num_sg, double z, double axe[], double sens[])
{
  int sg = *num_sg;
  coord[6*sg]   = axe[0];         coord[6*sg+1] = axe[1];         coord[6*sg+2] = z;
  coord[6*sg+3] = axe[0]+sens[0]; coord[6*sg+4] = axe[1]+sens[1]; coord[6*sg+5] = z;
  (*num_sg)++;
}

static int build_box_others_segment(Plot3dBox *B, double xmin, double xmax, 
				    double ymin, double ymax, double zmin, double zmax)
{
  /* compute the 3 faces where these segments are drawn */
  int f[3], i, j, k, nf=0, sg=0;
  double x, y, z;
  GBoolean GOK;

  for ( k = 0 ; k < 6 ; k++ )
    {
      GOK = BTRUE;
      for ( j = 0; j < 4 && GOK ; j++ )
	GOK = B->face[4*k+j] != B->inear;
      if ( GOK ) { f[nf] = k; nf++; };
    }

  /* face:   0 <-> z=zmin, 1 <-> y=ymin, 2 <-> x=xmin
   *	     3 <-> z=zmax, 4 <-> y=ymax, 3 <-> x=xmax
   */
  B->others_coord = malloc( 12*(B->nb_xyz_ticks)*sizeof(double) );
  if ( B->others_coord == NULL) return FAIL;
  for ( nf = 0 ; nf < 3 ; nf++ )
    {
      if ( f[nf] == 0 || f[nf] == 3 )     /* z = cte */
	{
	  if ( f[nf] == 0 ) z = zmin; else z = zmax;
	  for ( i = 0 ; i < B->nb_xticks ; i++)
	    build_y_seg(B->others_coord, &sg, B->xticks[i], ymin, ymax, z);
 	  for ( i = 0 ; i < B->nb_yticks ; i++)
	    build_x_seg(B->others_coord, &sg, xmin, xmax, B->yticks[i], z);
	}
      else if ( f[nf] == 1 || f[nf] == 4 )  /* y = cte */
	{
	  if ( f[nf] == 1 ) y = ymin; else y = ymax;
	  for ( i = 0 ; i < B->nb_xticks ; i++)
	    build_z_seg(B->others_coord, &sg, B->xticks[i], y, zmin, zmax);
	  for ( i = 0 ; i < B->nb_zticks ; i++)
	    build_x_seg(B->others_coord, &sg, xmin, xmax, y, B->zticks[i]);
	}
      else if ( f[nf] == 2 || f[nf] == 5 ) /* x = cte */
	{
	  if ( f[nf] == 2 ) x = xmin; else x = xmax;
	  for ( i = 0 ; i < B->nb_yticks ; i++)
	    build_z_seg(B->others_coord, &sg, x, B->yticks[i], zmin, zmax);
	  for ( i = 0 ; i < B->nb_zticks ; i++)
	    build_y_seg(B->others_coord, &sg, x, ymin, ymax, B->zticks[i]);
	}
    }
  return OK;
}

static void build_x_seg(double *coord, int *num_sg, double xmin, double xmax, double y, double z)
{
  int sg = *num_sg;
  coord[6*sg]   = xmin; coord[6*sg+1] = y; coord[6*sg+2] = z;
  coord[6*sg+3] = xmax; coord[6*sg+4] = y; coord[6*sg+5] = z;
  (*num_sg)++;
}

static void build_y_seg(double *coord, int *num_sg, double x, double ymin, double ymax, double z)
{
  int sg = *num_sg;
  coord[6*sg]   = x; coord[6*sg+1] = ymin; coord[6*sg+2] = z;
  coord[6*sg+3] = x; coord[6*sg+4] = ymax; coord[6*sg+5] = z;
  (*num_sg)++;
}

static void build_z_seg(double *coord, int *num_sg, double x, double y, double zmin, double zmax)
{
  int sg = *num_sg;
  coord[6*sg]   = x; coord[6*sg+1] = y; coord[6*sg+2] = zmin;
  coord[6*sg+3] = x; coord[6*sg+4] = y; coord[6*sg+5] = zmax;
  (*num_sg)++;
}


static void compute_ticks(double *vmin, double *vmax, double **Ticks, int *Nb_ticks)
{ 
  int i, j, first, last, inc=1, nb_grad, nb_ticks;
  double work[20], *ticks;
  
  gr_compute_ticks(vmin, vmax, work, &nb_grad);
  if ( nb_grad <= 2 )
    {
      nb_ticks = 2; work[0] = *vmin; work[1] = *vmax;
      first = 0;
    }
  else
    {
      if ( work[0] < *vmin ) 
	first = 1; 
      else 
	first = 0;
      if ( work[nb_grad-1] > *vmax ) 
	last = nb_grad-2; 
      else 
	last = nb_grad-1;
      
      nb_ticks = last - first + 1;
      
      if ( nb_ticks < 2 )
	{
	  nb_ticks = 2; work[0] = *vmin; work[1] = *vmax;
	  first = 0;
	}
      else if ( nb_ticks > 8 ) 
	{
	  nb_ticks = (nb_ticks+1)/2; inc = 2; 
	}
    }
  
  ticks = malloc(nb_ticks*sizeof(double));
  for ( i = 0, j = first ; i < nb_ticks ; i++, j+= inc )
    ticks[i] = work[j];

  *Nb_ticks = nb_ticks;
  *Ticks = ticks;

  return;
}

static int select_box_vertex(const double coord[])
{
  /* selectionne le sommet le plus proche de la camera */
  int k, imax = 0 ;
  double zmax = coord[2];

  for ( k = 1 ; k < 8 ; k++ )
    if ( coord[3*k+2] > zmax )
      {
	imax = k;
	zmax = coord[3*k+2];
      };
  return ( imax );
}

static void nsp_obj3d_draw_box(BCG *Xgc,Plot3dBox *B)
{
  int k, j, b0;
  GBoolean GOK;
  if ( B->box_style == SCILAB )
    draw_far_box_segments(Xgc,B);
  else
    for ( k = 0 ; k < 6 ; k++ )
      {
	GOK = BTRUE;
	for ( j = 0; j < 4 && GOK ; j++ )
	  {
	    int face =  B->face[4*k+j];
	    GOK = face != B->inear  &&  B->pos[face] != OUT_Z;
	  }
	if ( GOK )
	  draw_box_face(Xgc,B, k);
      }

  if ( B->with_ticks )
    {
      for ( j = 0 ; j < B->nb_xyz_ticks ; j++)
	draw_segment_bis(Xgc,B->ticks_coord, j, foreground_color);

      Xgc->graphic_engine->xset_font(Xgc,(ticks_font_type),(ticks_font_size));
      for ( j = 0 ; j < B->nb_xticks ; j++)
	draw_tick(Xgc,B,B->xticks[j], &(B->ticks_coord[6*j]));
      b0 = 6*B->nb_xticks;
      for ( j = 0 ; j < B->nb_yticks ; j++)
	draw_tick(Xgc,B,B->yticks[j], &(B->ticks_coord[6*j+b0]));
      b0 += 6*B->nb_yticks;
      for ( j = 0 ; j < B->nb_zticks ; j++)
	draw_tick(Xgc,B,B->zticks[j], &(B->ticks_coord[6*j+b0]));

      if ( B->box_style == MATLAB )
	{
	  Xgc->graphic_engine->xset_line_style(Xgc,2);
	  for ( j = 0 ; j < 2*(B->nb_xyz_ticks) ; j++)
	    draw_segment_bis(Xgc,B->others_coord, j, foreground_color);
	  Xgc->graphic_engine->xset_line_style(Xgc,1);
	}
    }
}

static void draw_tick(BCG *Xgc,Plot3dBox *B,double val,const double coord[])
{
  double xt, yt, vxt, vyt, normv, lim = 0.7071068;
  int xj, yj;
  char buf[60];

#ifdef WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      const double lim[] ={ 1.e+10,  1.e+10, - 1.e+10};
      /* we move to 2d scale */
      double Tcoord[6];
      apply_transforms_old(Xgc,Tcoord,coord, B->pos,lim,2); 
      vxt = Tcoord[3] - Tcoord[0];
      vyt = Tcoord[4] - Tcoord[1];
      xt = Tcoord[3] + 0.6*vxt;
      yt = Tcoord[4] + 0.6*vyt;
    } 
  else 
#endif 
    {
      vxt = coord[3] - coord[0];
      vyt = coord[4] - coord[1];
      xt = coord[3] + 0.6*vxt;
      yt = coord[4] + 0.6*vyt;
    }
  normv = sqrt( vxt*vxt + vyt*vyt );
  vxt = vxt/normv;
  vyt = vyt/normv;
  if ( vxt >= lim )
    { xj = LEFT; yj = CENTER; }
  else if ( vxt <= -lim )
    { xj = RIGHT; yj = CENTER; }
  else if ( vyt >= lim )
    { xj = CENTER; yj = UP; }
  else
    { xj = CENTER; yj = DOWN; }
  sprintf(buf, "%g", val);
  xt = XScale(xt);
  yt = YScale(yt);
#ifdef WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      nsp_ogl_set_2dview(Xgc);
      draw_justified_string(Xgc,buf, xt, yt, xj, yj);
      nsp_ogl_set_3dview(Xgc);
      return ;
    }
#endif 
  draw_justified_string(Xgc,buf, xt, yt, xj, yj);
}

static void draw_far_box_segments(BCG *Xgc,Plot3dBox *B)
{
  /* dessine les segments n'ayant pas le point inear comme sommet */
  int k, ia, ib;

  for ( k = 0 ; k < 12 ; k++ )
    {
      ia = B->segment[2*k]; ib = B->segment[2*k+1];
      if ( ia != B->inear  &&  ib != B->inear )
	draw_segment(Xgc,B->coord, ia, ib, foreground_color);
    };
}

void nsp_obj3d_draw_near_box_segments_old(BCG *Xgc,Plot3dBox *B)
{
  /* dessine les segments ayant le point inear comme sommet */
  int k, ia, ib;
  if (B->pos[B->inear] == OUT_Z )
    return;
  Xgc->graphic_engine->xset_line_style(Xgc,2); 
  for ( k = 0 ; k < 12 ; k++ )
    {
      ia = B->segment[2*k]; ib = B->segment[2*k+1];
      if ( ia == B->inear ||  ib == B->inear )
	draw_segment(Xgc,B->coord, ia, ib, foreground_color);
    };
  Xgc->graphic_engine->xset_line_style(Xgc,1); 
}


static void permut_of_sort(int *tab, int *perm)
{
  /* 
   *   get the permutation perm(0:2) which sort the array tab(0:2) in increasing order 
   */
  perm[0]=0; perm[1] = 1; perm[2] = 2;
  if ( tab[1] < tab[0] ) 
    {
      perm[1]=0 ; perm[0] = 1;
    }
  if ( tab[2] < tab[perm[1]] ) 
    {   /* sort not finish */
      if ( tab[2] < tab[perm[0]] ) 
	{
	  perm[2] = perm[1]; perm[1] = perm[0]; perm[0] = 2; 
	}
      else 
	{
	  perm[2] = perm[1] ; perm[1] = 2;
	}
    }
}

static void find_intersection(int *sx, int *sy, double *fxy, double z, 
			      int inda, int indb, int *xint, int *yint)
{ 
  double alpha;
  alpha = (z - fxy[inda])/(fxy[indb] - fxy[inda]);
  *xint = (int) ((1.0 - alpha)*sx[inda] + alpha*sx[indb]);
  *yint = (int) ((1.0 - alpha)*sy[inda] + alpha*sy[indb]);
} 

static int zone(double val, double valmin, double valmax, int nv)
{
  int z;
  if ( val > valmax )
    return (nv+1);
  else if ( val < valmin )
    return (0);
  else
    {
      z = 1 + (int) floor( nv*((val - valmin)/(valmax-valmin)) );
      if ( z > nv ) z = nv;
      return (z);
    }
}

static void interp_color_triangle(BCG *Xgc,int *x, int *y, double *v, int *z, double *zlevel, int *fill)
{
  int sx[3], sy[3], zxy[3], perm[3];
  double fxy[3];

  int i, nb0, edge, izone, color;
  int nr, resx[5],resy[5];
  int xEdge2, yEdge2, xEdge, yEdge; 

  permut_of_sort(z, perm);
  for ( i = 0 ; i < 3 ; i++)
    {
      sx[i] = x[perm[i]];
      sy[i] = y[perm[i]];
      zxy[i] = z[perm[i]];
      fxy[i] = v[perm[i]];
    }

  if ( zxy[0] == zxy[2] )   /*  case of only one color for the triangle : */
    {
      resx[0] = sx[0]; resx[1] = sx[1]; resx[2] = sx[2];
      resy[0] = sy[0]; resy[1] = sy[1]; resy[2] = sy[2];
      color = - Abs(fill[zxy[0]]); nr = 3;
      if ( color != 0 )
	Xgc->graphic_engine->fillpolylines(Xgc,resx,resy,&color,1,nr); 
      return; 
    }

  /* 
     at least 2 colors for painting the triangle : it is divided in elementary
     polygons. The number of polygons is npolys = zxy[2]-zxy[0]+1.

     P2           as zxy[0] <= zxy[1] <  zxy[2] or 
     Notations/Hints :       /\              zxy[0] <  zxy[1] <= zxy[2]
     edge2  /  \ edge1    from a previus sort. All the polygons
     /    \         have 2 points on edge2, the others points
     /______\        are on edge0 and/or edge1. I name the 2 ends
     P0        P1      points on each poly PEdge2 and Pedge, they are 
     edge0         the 2 first points of the next poly. I start
     from P0 to form the first poly (a triangle or
     a 4 sides depending if zxy[0]=zxy[1]), then the 2, 3, .., npolys - 1 (if they exist)
     and finally the last one which comprise the P2 vertex.  In some special cases
     we can have a degenerate poly but it doesn't matter ! 				  
  */
  
  nb0 = zxy[1]-zxy[0]; /* number of intersection points on edge 0 */

  /*----------------------------+
    |   compute the first poly    |
    +----------------------------*/
  
  resx[0] = sx[0]; resy[0] = sy[0]; nr = 1; edge = 0;
  if ( nb0 == 0 )  /* the intersection point is on Edge1 but */
    {              /* the next point of the poly is P1 */  
      resx[1] = sx[1]; resy[1] = sy[1]; nr++;
      edge = 1;    /* the next intersection points will be on edge1 */
    } 
  else 
    nb0--;
  /* the intersection point on edge (0 or 1) : */
  find_intersection(sx, sy, fxy, zlevel[zxy[0]], edge, edge+1, &xEdge, &yEdge);
  resx[nr] = xEdge; resy[nr] = yEdge; nr++;
  /* the last point of the first poly (edge 2) : */
  find_intersection(sx, sy, fxy, zlevel[zxy[0]], 0, 2, &xEdge2, &yEdge2);
  resx[nr] = xEdge2; resy[nr] = yEdge2; nr++;
  color = - Abs(fill[zxy[0]]);
  if ( color != 0 )
    Xgc->graphic_engine->fillpolylines(Xgc,resx,resy,&color,1,nr);

  /*------------------------------------+ 
    | compute the intermediary polygon(s) |
    +------------------------------------*/

  for ( izone = zxy[0]+1 ; izone < zxy[2] ; izone++ ) 
    {
      resx[0] = xEdge2; resy[0] = yEdge2;          /* the 2 first points are known */
      resx[1] = xEdge;  resy[1] = yEdge; nr = 2;
      if ( edge == 0 )   /* the intersection point is perhaps on edge 0 */
	{
	  if (nb0 == 0 )  /* no it is on edge 1 but the next point of the poly is P1 */
	    {
	      resx[2] = sx[1]; resy[2] = sy[1]; nr++;
	      edge = 1;          /* the next intersection points will be on edge1 */
	    } 
	  else 
	    nb0--;
	};
      /* the intersection point on edge (0 or 1) : */
      find_intersection(sx, sy, fxy, zlevel[izone], edge, edge+1, &xEdge, &yEdge);
      resx[nr] = xEdge; resy[nr] = yEdge; nr++;
      /* the last point of the first poly (edge 2) : */
      find_intersection(sx, sy, fxy, zlevel[izone], 0, 2, &xEdge2, &yEdge2);
      resx[nr] = xEdge2; resy[nr] = yEdge2; nr++;
      color = - Abs(fill[izone]);
      if ( color != 0 )
	Xgc->graphic_engine->fillpolylines(Xgc,resx,resy,&color,1,nr);
    };

  /*-----------------------+ 
    | compute the last poly  |
    +-----------------------*/
  resx[0] = xEdge2; resy[0] = yEdge2;         /* the 2 first points are known */
  resx[1] = xEdge;  resy[1] = yEdge; nr = 2;
  if ( edge == 0 )  /* the next point of the poly is P1 */
    {                         
      resx[2] = sx[1]; resy[2] = sy[1]; nr++;
    };
  /* the last point is P2 */
  resx[nr] = sx[2]; resy[nr] = sy[2]; nr++;
  color = - Abs(fill[zxy[2]]);
  if ( color != 0 )
    Xgc->graphic_engine->fillpolylines(Xgc,resx,resy,&color,1,nr);
}


static void zmean_faces_for_Polyhedron(void *Ob, double z[], HFstruct HF[], int *n, int k)
{
  Polyhedron *Q=Ob;
  int m, i, j, *current_vertex;
  VisionPos pos_face, pos_vertex;
  double coef, zmean;

  m = Q->nb_vertices_per_face; 
  coef = 1.0/m;
  current_vertex = Q->face;
  for ( j = 0 ; j < Q->nb_faces ; j++ )
    {
      zmean = 0.0; pos_face = OUT_XY;
      /* Une face rentre dans le calcul des faces cachées si :
       *     1/ aucun point n'est en position OUT_Z
       *     2/ au moins un point est IN (les autres étant alors soit
       *        IN soit OUT_XY)
       * On pourra par la suite détailler un peu plus car si tous les
       * sommets de la face sont IN aucun clippling n'est à effectuer.
       * Faire ce clipping moi-même ?
       */
      for ( i = 0 ; i < m ; i++ )
	{
	  zmean += Q->coord[3*(*current_vertex)+2];
	  pos_vertex = Q->pos[*current_vertex];
	  if (pos_vertex == OUT_Z)
	    pos_face = OUT_Z;
	  else if (pos_vertex == VIN && pos_face != OUT_Z)
	    pos_face = VIN;
	  current_vertex++;
	}
      if (pos_face == VIN) 
	{
	  z[*n] = coef*zmean;
	  HF[*n].num_obj = k;
	  HF[*n].num_in_obj = j;
	  (*n)++; 
	}
    }
}

static void zmean_faces_for_SPolyhedron(void *Ob, double z[], HFstruct HF[], int *n, int k)
{
  SPolyhedron *Q=Ob;
  int m, i, j, *current_vertex;
  VisionPos pos_face, pos_vertex;
  double coef, zmean;

  m = Q->nb_vertices_per_face; 
  coef = 1.0/m;
  current_vertex = Q->face;
  for ( j = 0 ; j < Q->nb_faces ; j++ )
    {
      zmean = 0.0; pos_face = OUT_XY;
      /* Une face rentre dans le calcul des faces cachées si :
       *     1/ aucun point n'est en position OUT_Z
       *     2/ au moins un point est IN (les autres étant alors soit
       *        IN soit OUT_XY)
       * On pourra par la suite détailler un peu plus car si tous les
       * sommets de la face sont IN aucun clippling n'est à effectuer.
       * Faire ce clipping moi-même ?
       */
      for ( i = 0 ; i < m ; i++ )
	{
	  zmean += Q->coord[3*(*current_vertex)+2];
	  pos_vertex = Q->pos[*current_vertex];
	  if (pos_vertex == OUT_Z)
	    pos_face = OUT_Z;
	  else if (pos_vertex == VIN && pos_face != OUT_Z)
	    pos_face = VIN;
	  current_vertex++;
	}
      if (pos_face == VIN) 
	{
	  z[*n] = coef*zmean;
	  HF[*n].num_obj = k;
	  HF[*n].num_in_obj = j;
	  (*n)++; 
	}
    }
}

static void zmean_segments_for_polyline(void *Ob, double z[], HFstruct HF[], int *n, int k)
{
  PolyLine *L=Ob;
  int j;
  double zmean;

  for ( j = 0 ; j < L->nb_coords-1 ; j++ )
    {
      zmean = 0.5 * (L->coord[3*j+2] + L->coord[3*j+5]);
      if (L->pos[j] != OUT_Z && L->pos[j+1] != OUT_Z)
	if (L->pos[j] == VIN || L->pos[j+1] == VIN)
	  { 
	    /* le segment rentre dans les "facettes" à traiter */
	    z[*n] = zmean;
	    HF[*n].num_obj = k;
	    HF[*n].num_in_obj = j;
	    (*n)++; 
	  }
    }
}

static void draw_spolyhedron_face(BCG *Xgc,void *Ob, int j)
{
  SPolyhedron *Q = (SPolyhedron *) Ob;
  int i, k, np=1, m, zero=0;
  int x[4], y[4];
  int nbtri, triangle[2][3] = {{0,1,2},{0,2,3}};
  int zxy[3], sx[3], sy[3];
  int numpt, *current_vertex, color, orient;
  double v[3], val_mean=0.0;

  m = Q->nb_vertices_per_face;
  current_vertex = &(Q->face[m*j]);
  for (i = 0 ; i < m ; i++)
    {
      numpt = current_vertex[i];
      x[i] = XScale(Q->coord[3*numpt]);
      y[i] = YScale(Q->coord[3*numpt+1]);
      val_mean += Q->val[numpt];
    }
  val_mean = val_mean / m;

  orient = nsp_obj3d_orientation_old(x, y, m);

  /*   if ( orient == 1 ) */
  /*     return; */

  Xgc->graphic_engine->xset_pattern(Xgc,foreground_color);
  if ( display_mode == FLAT  || ( orient == 1 && Q->back_color >= 0 ) ) 
    {
      if ( orient == 1  && Q->back_color >= 0 )
	color = -Q->back_color;
      else
	color = -Q->fill[zone(val_mean, Q->vmin, Q->vmax, Q->nb_levels)];
      if ( Q->with_mesh ) color= Abs(color);
      Xgc->graphic_engine->fillpolylines(Xgc, x, y, &color, np, m);
    }
  else
    {
      nbtri = m - 2;
      for ( k = 0 ; k < nbtri ; k++ )
	{
	  for ( j = 0 ; j < 3 ; j++ )
	    {
	      i = triangle[k][j];
	      v[j] = Q->val[current_vertex[i]];
	      zxy[j] = zone(v[j], Q->vmin, Q->vmax, Q->nb_levels);
	      sx[j] = x[i]; sy[j] = y[i];
	    }
	  interp_color_triangle (Xgc,sx, sy, v, zxy, Q->vlevel, Q->fill);
	}
      if ( Q->with_mesh  ) 
	Xgc->graphic_engine->fillpolylines(Xgc, x, y, &zero, np, m);
    }
}

static void draw_spolyhedron_ogl(BCG *Xgc,void *Ob)
{
#ifdef  WITH_GTKGLEXT 
  SPolyhedron *Q = (SPolyhedron *) Ob;
  int i,j, np=1, m, zero=0,colors[4];
  int numpt, *current_vertex, color;
  double val_mean=0.0;
  double x[4], y[4], z[4], v[4];
  for ( j = 0 ; j < Q->nb_faces ; j++) 
    {
      m = Q->nb_vertices_per_face;
      current_vertex = &(Q->face[m*j]);
      for (i = 0 ; i < m ; i++)
	{
	  numpt = current_vertex[i];
	  x[i] = Q->coord[3*numpt];
	  y[i] = Q->coord[3*numpt+1];
	  z[i] = Q->coord[3*numpt+2];
	  val_mean += (v[i]=Q->val[numpt]);
	}
      val_mean = val_mean / m;
      Xgc->graphic_engine->xset_pattern(Xgc,foreground_color);
      if ( display_mode == FLAT  )
	{
	  color = -Q->fill[zone(val_mean, Q->vmin, Q->vmax, Q->nb_levels)];
	  fillpolylines3D(Xgc, x, y,z, &color, np, m);
	  if ( Q->with_mesh )
	    fillpolylines3D(Xgc, x, y,z, &zero, np, m);	  
	}
      else
	{
	  for (i = 0 ; i < m ; i++) 
	    colors[i] = -Q->fill[zone(v[i], Q->vmin, Q->vmax, Q->nb_levels)];
	  /* colors are given by cvect of size (*p) times (*q) */
	  fillpolylines3D_shade(Xgc,x,y,z,colors, np,m);
	  if ( Q->with_mesh )
	    fillpolylines3D(Xgc, x, y,z, &zero, np, m);	  
	}
    }
#endif
}

static void draw_polyhedron_face(BCG *Xgc,void *Ob, int j)
{
  Polyhedron *Q = (Polyhedron *) Ob;
  int i, np=1, m;
  int x[6], y[6];   /* a changer */
  int numpt, *current_vertex, color;
  
  m = Q->nb_vertices_per_face;
  current_vertex = &(Q->face[m*j]);
  for (i = 0 ; i < m ; i++)
    {
      numpt = current_vertex[i];
      x[i] = XScale(Q->coord[3*numpt]);
      y[i] = YScale(Q->coord[3*numpt+1]);
    }

  if ( nsp_obj3d_orientation_old(x, y, m) == -1 )  /* le repère de la caméra est indirect ! */
    if ( Q->nb_colors == 1 )
      color = Q->color[0];
    else
      color = Q->color[j];
  else       /* orientation < 0 =>  back color is used */
    if ( Q->nb_back_colors == 1 )
      color = Q->back_color[0];
    else
      color = Q->back_color[j];
	    
  if ( ! Q->with_mesh )  /* le contour du polygone ne doit pas apparaitre */
    color = -color; 

  /* color = 0;  permet de voir uniquement le maillage */
  /* 
   *  x, y : polygone(s) coordinates, nr : number of sides
   *  np : number of polygone(s) =1 here
   */
  Xgc->graphic_engine->xset_pattern(Xgc,foreground_color);
  Xgc->graphic_engine->fillpolylines(Xgc, x, y, &color, np, m);
}

static void draw_polyhedron_ogl(BCG *Xgc,void *Ob)
{
#ifdef  WITH_GTKGLEXT 
  Polyhedron *Q = (Polyhedron *) Ob;
  int i,j, np=1, m;
  double x[6], y[6], z[6];   /* a changer */
  int numpt, *current_vertex, color;
  
  m = Q->nb_vertices_per_face;

  for ( j = 0 ; j < Q->nb_faces ; j++ )
    {
      current_vertex = &(Q->face[m*j]);
      for (i = 0 ; i < m ; i++)
	{
	  numpt = current_vertex[i];
	  x[i] = Q->coord[3*numpt];
	  y[i] = Q->coord[3*numpt+1];
	  z[i] = Q->coord[3*numpt+2];
	}
      
      if ( Q->nb_colors == 1 )
	color = Q->color[0];
      else
	color = Q->color[j];
      
      if ( ! Q->with_mesh )  /* le contour du polygone ne doit pas apparaitre */
	color = -color; 

      /* color = 0;  permet de voir uniquement le maillage */
      /* 
       *  x, y : polygone(s) coordinates, nr : number of sides
       *  np : number of polygone(s) =1 here
       */
      Xgc->graphic_engine->xset_pattern(Xgc,foreground_color);
      fillpolylines3D(Xgc, x, y, z, &color, np, m);
    }
#endif
}


static void draw_box_face(BCG *Xgc,Plot3dBox *B, int j)
{
  int x[4], y[4], i, numpt,np=1, m=4;
  const int *current_vertex;
#ifdef WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      double xd[4],yd[4],zd[4];
      current_vertex = &(B->face[4*j]);
      for (i = 0 ; i < 4 ; i++)
	{
	  numpt = current_vertex[i];
	  xd[i] = B->coord[3*numpt];
	  yd[i] = B->coord[3*numpt+1];
	  zd[i] = B->coord[3*numpt+2];
	}
      Xgc->graphic_engine->xset_pattern(Xgc,foreground_color);
      fillpolylines3D(Xgc, xd, yd,zd, &B->color, np, m);
      return;
    }
#endif
  current_vertex = &(B->face[4*j]);
  for (i = 0 ; i < 4 ; i++)
    {
      numpt = current_vertex[i];
      x[i] = XScale(B->coord[3*numpt]);
      y[i] = YScale(B->coord[3*numpt+1]);
    }
  Xgc->graphic_engine->xset_pattern(Xgc,foreground_color);
  Xgc->graphic_engine->fillpolylines(Xgc, x, y,&B->color, np, m);
}

static void draw_polyline_segment(BCG *Xgc,void *Ob, int j)
{
  PolyLine *L = (PolyLine *) Ob;
  int x[2], y[2], color, n=2, flag=0;
  x[0] = XScale(L->coord[3*j]);
  y[0] = YScale(L->coord[3*j+1]);
  x[1] = XScale(L->coord[3*j+3]);
  y[1] = YScale(L->coord[3*j+4]);
  color = ( L->nb_colors == 1 ) ? L->color[0] : L->color[j];
  Xgc->graphic_engine->drawsegments(Xgc, x, y , n, &color, flag);
}

static void draw_polyline_ogl(BCG *Xgc,void *Ob)
{
#ifdef  WITH_GTKGLEXT 
  PolyLine *L = (PolyLine *) Ob;
  int j,color;
  double x[2], y[2],z[2];
  int  n=2, flag=0;
  for ( j = 0 ; j < L->nb_coords-1 ; j++ )
    {
      color = ( L->nb_colors == 1 ) ? L->color[0] : L->color[j];
      x[0] = L->coord[3*j];
      y[0] = L->coord[3*j+1];
      z[0] = L->coord[3*j+2];
      x[1] = L->coord[3*j+3];
      y[1] = L->coord[3*j+4];
      z[1] = L->coord[3*j+5];
      drawsegments3D(Xgc, x, y ,z, n, &color, flag);
    }
#endif
}

static void draw_segment(BCG *Xgc,double coord[], int ia, int ib, int color)
{
  int x[2], y[2], n=2, flag=0;
#ifdef WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      double xd[2], yd[2],zd[2];
      int n=2, flag=0;
      xd[0] = coord[3*ia];
      yd[0] = coord[3*ia+1];
      zd[0] = coord[3*ia+2];
      xd[1] = coord[3*ib];
      yd[1] = coord[3*ib+1];
      zd[1] = coord[3*ib+2];
      drawsegments3D(Xgc, xd, yd ,zd, n, &color, flag);
      return; 
    }
#endif
  x[0] = XScale(coord[3*ia]);
  y[0] = YScale(coord[3*ia+1]);
  x[1] = XScale(coord[3*ib]);
  y[1] = YScale(coord[3*ib+1]);
  Xgc->graphic_engine->drawsegments(Xgc, x, y , n, &color, flag);
}

static void draw_segment_bis(BCG *Xgc,double coord[], int ns, int color)
{
  int x[2], y[2], n=2, flag=0;
#ifdef WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      double x[2], y[2], z[2]; 
      int n=2, flag=0;
      x[0] = coord[6*ns];
      y[0] = coord[6*ns+1];
      z[0] = coord[6*ns+2];
      x[1] = coord[6*ns+3];
      y[1] = coord[6*ns+4];
      z[1] = coord[6*ns+5];
      drawsegments3D(Xgc, x, y ,z, n, &color, flag);
      return;
    }
#endif
  x[0] = XScale(coord[6*ns]);
  y[0] = YScale(coord[6*ns+1]);
  x[1] = XScale(coord[6*ns+3]);
  y[1] = YScale(coord[6*ns+4]);
  Xgc->graphic_engine->drawsegments(Xgc, x, y , n, &color, flag);
}

static void draw_point(BCG *Xgc,void *Ob, int j)
{
  Points *V = (Points *) Ob;
  int x, y, color, mark, n=1;

  x = XScale(V->coord[3*j]);
  y = YScale(V->coord[3*j+1]);
  color = V->color;
  mark = V->mark_type;

  /* il faudrait sauvegarder le pattern et la marque actuels */
  Xgc->graphic_engine->xset_pattern(Xgc, color);
  /* ici &n doit concerner la taille du symbole (a voir) */
  Xgc->graphic_engine->xset_mark(Xgc, mark,n);
  Xgc->graphic_engine->drawpolymark(Xgc,&x,&y,n);
}

static void draw_points_ogl(BCG *Xgc,void *Ob)
{
  Points *V = (Points *) Ob;
  double x, y,z;
  int color, mark, n=1,j;
  for ( j = 0 ; j < V->nb_coords ; j++) 
    {
      x = V->coord[3*j];
      y = V->coord[3*j+1];
      z = V->coord[3*j+2];
      color = V->color;
      mark = V->mark_type;
      /* il faudrait sauvegarder le pattern et la marque actuels */
      Xgc->graphic_engine->xset_pattern(Xgc, color);
      /* ici &n doit concerner la taille du symbole (a voir) */
      Xgc->graphic_engine->xset_mark(Xgc, mark,n);
      /* Xgc->graphic_engine->drawpolymark(Xgc,&x,&y,&z,n); */
    }
}

static void draw_string3d(BCG *Xgc,void *Ob, int j)
{
  draw_justified_string3d(Xgc,Ob,CENTER,CENTER);
}

static void draw_justified_string3d(BCG *Xgc,String3d *V, int xj, int yj)
{
  double x,y;
  Xgc->graphic_engine->xset_font(Xgc,V->font_type,V->font_size);
  x = XScale(V->coord[0]);
  y = YScale(V->coord[1]);
  draw_justified_string(Xgc,V->str,x,y, xj, yj);
}

static void draw_string3d_ogl(BCG *Xgc,void *Ob)
{
  draw_justified_string3d_ogl(Xgc,Ob,CENTER,CENTER);
}

static void draw_justified_string3d_ogl(BCG *Xgc,String3d *V, int xj, int yj)
{
#ifdef  WITH_GTKGLEXT 
  const double lim[] ={ 1.e+10,  1.e+10, - 1.e+10};
  /* we move to 2d scale */
  double Tcoord[3];
  apply_transforms_old(Xgc,Tcoord,V->coord,V->pos,lim,1); 
  Tcoord[0] = XScale(Tcoord[0]);
  Tcoord[1] = YScale(Tcoord[1]);
  nsp_ogl_set_2dview(Xgc);
  Xgc->graphic_engine->xset_font(Xgc,V->font_type,V->font_size);
  draw_justified_string(Xgc,V->str,Tcoord[0],Tcoord[1], xj, yj);
  nsp_ogl_set_3dview(Xgc);
#endif 
}


static void draw_justified_string(BCG *Xgc,char *str, double x, double y, int xj, int yj)
{
  int flag=0, rect[4], w, h;
  double angle=0.0; 
  Xgc->graphic_engine->boundingbox(Xgc,str,x,y, rect);
  w = rect[2]; h = rect[3];
  if ( xj == CENTER ) 
    x -= w/2;
  else if ( xj == RIGHT )
    x -= w;
  if ( yj == CENTER )
    y += h/2;
  else if ( yj == DOWN )
    y += h;
  Xgc->graphic_engine->displaystring(Xgc,str,x,y, flag,angle);
}


static int nsp_obj3d_orientation_old(int x[], int y[], int n)
{
  /* calcule l'orientation avec les 3 premiers points du polygone ... */
  int a, b, c, d ;
  a = x[1] - x[0]; c = x[2] - x[0]; 
  b = y[1] - y[0]; d = y[2] - y[0];
  if ( a*d - b*c >= 0)
    return ( 1 );
  else
    return ( -1 );
}

static void init_Obj3d(Obj3d Obj[], int nbObj)
{
  int k;
  for ( k = 0 ; k < nbObj ; k++ )
    Obj[k].obj = NULL;
}

static void free_Obj3d(Obj3d Obj[], int nbObj)
{
  int k;
  for ( k = 0 ; k < nbObj ; k++ )
    {
      OBJ3D(Obj[k].obj)->free(Obj[k].obj);
    }
  free(Obj);
}

void nsp_obj3d_free_box(Plot3dBox *B)
{
  if ( B->with_ticks )
    {
      free(B->xticks);
      free(B->yticks);
      free(B->zticks);
      free(B->ticks_coord);
      free(B->ticks_pos);
      if ( B->box_style == MATLAB )
	{
	  free(B->others_coord);
	  free(B->others_pos);
	}
    }
  free(B);
}


/* 
 *  Compute for each point the local coordinate (in the visualisation repair)
 *  then applies the perpective transform and test if the point is inside the
 *  troncated vision pyramide :  | loc_x | <= lim[0]
 *                               | loc_y | <= lim[1]
 *                               | loc_z | >= lim[2] 
 */


static void apply_transforms_old(BCG *Xgc,double Coord[],const double *M, VisionPos pos[],const double lim[], int ncoord)
{
  int i, k=0;
  double facteur;
  for (i = 0; i < 3*ncoord ; i += 3)
    {
      /* take care that Coord and M can point to the same location 
       * thus we have to copy
       */
      double v[3];
      v[0] = M[i];v[1] = M[i+1]; v[2] = M[i+2]; 
      Coord[i]   = TRX(v[0],v[1],v[2]);
      Coord[i+1] = TRY(v[0],v[1],v[2]);
      Coord[i+2] = TRZ(v[0],v[1],v[2]);
      if ( Coord[i+2] < lim[2] )  
	{
	  pos[k] = OUT_Z; /* dans ce cas on applique pas la perspective */
	}
      else
	{
	  /* on applique la perspective */
	  facteur = 1.0/Coord[i+2];
	  facteur = 1.0;
	  Coord[i]   = facteur*Coord[i];
	  Coord[i+1] = facteur*Coord[i+1];
	  /* le point est-il dans le rectangle de visu ? */
	  if ( fabs(Coord[i]) > lim[0] || fabs(Coord[i+1]) > lim[1] ) 
	    pos[k] = OUT_XY;
	  else
	    pos[k] = VIN;
	}
      k++;
    }
}



/* 
 * polyhedron 
 * 
 */

static int get_polyhedron(Stack *stack,int k,NspHash *H,Polyhedron *Q,int *nf)
{
  int i;
  nsp_option opts[] ={{"coord",realmatcopy,NULLOBJ,-1},
		      {"face",realmatcopy,NULLOBJ,-1},
		      {"color",realmatcopy,NULLOBJ,-1},
		      {"back_color",realmatcopy,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( get_args_from_hash(*stack,H,opts,&Q->Mcoord,&Q->Mface,&Q->Mcolor,&Q->Mback_color)==FAIL) 
    {
      return FAIL;
    }
  if ( Q->Mcoord->m != 3 ) 
    {
      Scierror("%s: bad coord, first dimension should be 3\n", NspFnameH(stack));
      return FAIL;
    }
  Q->nb_coords = Q->Mcoord->n;
  Q->coord = Q->Mcoord->R;

  if ( Q->Mface->m < 3 ) 
    {
      Scierror("%s : bad face, first dimension should be < 3 %d\n",NspFnameH(stack));
      return FAIL;
    }
  Q->nb_vertices_per_face = Q->Mface->m;
  Q->nb_faces = Q->Mface->n;
  *nf += Q->nb_faces;
  /* switch to int XXX */
  Q->Mface = Mat2int(Q->Mface);
  Q->face = (int *) Q->Mface->R;
  for ( i = 0 ; i < Q->Mface->mn ; i++) Q->face[i]--;  
  
  if ( Q->Mcolor->mn !=  Q->nb_faces   && Q->Mcolor->mn != 1 ) 
    {
      Scierror("%s : bad color size, expecting 1 or %d\n", Q->nb_faces);
      return FAIL;
    }
  Q->nb_colors = Q->Mcolor->mn;
  /* switch to int XXX */
  Q->Mcolor = Mat2int(Q->Mcolor);
  Q->color =  (int *) Q->Mcolor->R;

  if ( Q->Mback_color->mn  !=  Q->nb_faces  && Q->Mback_color->mn != 1 ) 
    {
      Scierror("%s : bad back_color size, expecting 1 or %d\n", Q->nb_faces);
      return FAIL;
    }
  Q->nb_back_colors = Q->Mback_color->mn ;
  /* switch to int XXX */
  Q->Mback_color = Mat2int(Q->Mback_color);

  Q->back_color =  (int *) Q->Mback_color->R;
 
  /* create a matrix FIXME */
  Q->pos = malloc( Q->nb_coords * sizeof(VisionPos));
  Q->with_mesh = with_mesh;

  Q->free = free_polyhedron;
  Q->draw_partial = draw_polyhedron_face; 
  Q->draw_ogl = draw_polyhedron_ogl; 
  Q->zmean = zmean_faces_for_SPolyhedron;
  return OK;
}


static void free_polyhedron(void *Ob)
{
  Polyhedron *Q = (  Polyhedron *) Ob;
  nsp_matrix_destroy(Q->Mcoord);
  nsp_matrix_destroy(Q->Mface);
  nsp_matrix_destroy(Q->Mcolor);
  nsp_matrix_destroy(Q->Mback_color);
  free(Q->pos);
  free(Q);
}




static int get_spolyhedron(Stack *stack,int k,NspHash *H,SPolyhedron *Q,int *nf)
{
  double dv;
  int i,back_color=-1;
  nsp_option opts[] ={{"coord",realmatcopy,NULLOBJ,-1},
		      {"face",realmatcopy,NULLOBJ,-1},
		      {"val",realmatcopy,NULLOBJ,-1},
		      {"valminmax",realmatcopy,NULLOBJ,-1},
		      {"colminmax",realmatcopy,NULLOBJ,-1},
		      {"colout",realmatcopy,NULLOBJ,-1},
		      {"back_color",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( get_args_from_hash(*stack,H,opts,&Q->Mcoord,&Q->Mface,&Q->Mval,&Q->Mvalminmax,
			  &Q->Mcolminmax,&Q->Mcolout,&back_color)==FAIL) 
    {
      return FAIL;
    }
  if ( Q->Mcoord->m != 3 ) 
    {
      Scierror("%s: bad coord, first dimension should be 3\n", NspFnameH(stack));
      return FAIL;
    }
  Q->nb_coords = Q->Mcoord->n;
  Q->coord = Q->Mcoord->R;

  if ( Q->Mface->m < 3 ) 
    {
      Scierror("%s : bad face, first dimension should be < 3 %d\n", NspFnameH(stack));
      return FAIL;
    }
  Q->nb_vertices_per_face = Q->Mface->m;
  Q->nb_faces = Q->Mface->n;
  *nf += Q->nb_faces;
  /* switch to int XXX */
  Q->Mface = Mat2int(Q->Mface);

  Q->face = (int *) Q->Mface->R;
  for ( i = 0 ; i < Q->Mface->mn ; i++) Q->face[i]--;  

  /* val  */
  
  if ( Q->Mval->mn != Q->nb_coords ) 
    {
      Scierror("%s : bad dimensions for val, mxn should be equal to %d\n", NspFnameH(stack),Q->nb_coords);
      return FAIL;
    }
  Q->val = Q->Mval->R;
  
  /* */

  if ( Q->Mvalminmax->mn != 2 )
    {
      Scierror("%s : bad dimensions for valminmax, should be of size 2\n", NspFnameH(stack));
      return FAIL;
    }
  Q->vmin = Q->Mvalminmax->R[0]; Q->vmax = Q->Mvalminmax->R[1];

  /* */

  if ( Q->Mcolminmax->mn != 2 )
    {
      Scierror("%s : bad dimensions for colminmax, should be of size 2\n", NspFnameH(stack));
      return FAIL;
    }

  /* */

  if ( Q->Mcolout->mn != 2 )
    {
      Scierror("%s : bad dimensions for colout, should be of size 2\n", NspFnameH(stack));
      return FAIL;
    }

  /* */

  Q->nb_levels = Q->Mcolminmax->R[1] - Q->Mcolminmax->R[0] + 1;

  if ( Q->nb_levels < 1 )
    {
      Scierror("%s : bad colout field for object number %d \r\n", NspFnameH(stack), k-7);
    }

  Q->back_color = back_color;

  Q->vlevel = malloc( (1 + Q->nb_levels) * sizeof(double));
  Q->fill = malloc( (2 + Q->nb_levels) * sizeof(int));

  dv = (Q->vmax - Q->vmin)/Q->nb_levels;
  Q->vlevel[0] = Q->vmin;
  for ( i = 1 ; i < Q->nb_levels ; i++ )
    Q->vlevel[i] = Q->vmin + i*dv;
  Q->vlevel[Q->nb_levels] = Q->vmax;

  Q->fill[0] = Q->Mcolout->R[0];
  Q->fill[1] = Q->Mcolminmax->R[0];
  for ( i = 2 ; i <= Q->nb_levels ; i++ )
    Q->fill[i] = Q->fill[i-1] + 1;
  Q->fill[Q->nb_levels+1] = Q->Mcolout->R[1];

  Q->pos = malloc( Q->nb_coords * sizeof(VisionPos));

  Q->with_mesh = with_mesh;

  Q->free = free_spolyhedron;
  Q->draw_partial = draw_spolyhedron_face; 
  Q->draw_ogl = draw_spolyhedron_ogl; 
  Q->zmean = zmean_faces_for_Polyhedron;
  return OK;
}



static void free_spolyhedron(void *Ob)
{
  SPolyhedron *Q = (SPolyhedron *) Ob;
  nsp_matrix_destroy(Q->Mcoord);
  nsp_matrix_destroy(Q->Mface);
  nsp_matrix_destroy(Q->Mval);
  nsp_matrix_destroy(Q->Mvalminmax);
  nsp_matrix_destroy(Q->Mcolminmax);
  nsp_matrix_destroy(Q->Mcolout);
  free(Q->vlevel);
  free(Q->fill);
  free(Q->pos);
  free(Q);
}


static int get_polyline(Stack *stack,int k,NspHash *H,PolyLine *L,int *nf)
{
  nsp_option opts[] ={{"coord",realmatcopy,NULLOBJ,-1},
		      {"color",realmatcopy,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( get_args_from_hash(*stack,H,opts,&L->Mcoord,&L->Mcolor)==FAIL) 
    {
      return FAIL;
    }

  if ( L->Mcoord->m != 3 ) 
    {
      Scierror("%s: bad coord, first dimension should be 3\n", NspFnameH(stack));
      return FAIL;
    }
  L->nb_coords = L->Mcoord->n;
  L->coord = L->Mcoord->R;
  *nf += L->Mcoord->n -1;

  if ( L->Mcolor->mn != L->Mcoord->n -1 && L->Mcolor->mn != 1 ) 
    {
      Scierror("%s : bad color for object number %d \r\n", NspFnameH(stack), k-7);
    }
  L->nb_colors = L->Mcolor->mn ;
  L->Mcolor = Mat2int(L->Mcolor);
  L->color = (int *) L->Mcolor->R;
  L->pos = malloc( L->nb_coords * sizeof(VisionPos));

  L->free = free_polyline;
  L->draw_partial = draw_polyline_segment; 
  L->draw_ogl = draw_polyline_ogl; 
  L->zmean = zmean_segments_for_polyline;

  return 0;
}

static void free_polyline(void *Ob)
{
  PolyLine *Q = Ob;
  nsp_matrix_destroy(Q->Mcoord);
  nsp_matrix_destroy(Q->Mcolor);
  free(Q->pos);
  free(Q);
}


static int get_points(Stack *stack,int k,NspHash *H,Points *P,int *nf)
{
  int mark_type=0,color=0;
  nsp_option opts[] ={{"coord",realmatcopy,NULLOBJ,-1},
		      {"color",s_int,NULLOBJ,-1},
		      /* {"mark_type",s_int,NULLOBJ,-1}, */
		      { NULL,t_end,NULLOBJ,-1}};

  if ( get_args_from_hash(*stack,H,opts,&P->Mcoord,&color,&mark_type)==FAIL) 
    {
      return FAIL;
    }

  if ( P->Mcoord->m != 3 ) 
    {
      Scierror("%s: bad coord, first dimension should be 3\n", NspFnameH(stack));
      return FAIL;
    }
  P->nb_coords = P->Mcoord->n;
  P->coord = P->Mcoord->R;
  *nf += P->Mcoord->n;
  P->color = color;
  P->mark_type = mark_type;
  P->pos = malloc( P->nb_coords * sizeof(VisionPos));
  P->free = free_points;
  P->draw_partial = draw_point; 
  P->draw_ogl = draw_points_ogl; 
  P->zmean = zmean_for_Points;

  return 0;
}

static void zmean_for_Points(void *Ob, double z[], HFstruct HF[], int *n, int k)
{
  int j;
  Points *V = Ob;
  for ( j = 0 ; j < V->nb_coords ; j++)
    if (V->pos[j] == VIN)
      {
	z[*n] = V->coord[3*j+2]; HF[*n].num_obj = k; HF[*n].num_in_obj = j;
	(*n)++; 
      }
}

static void free_points(void *Ob)
{
  Points *Q = Ob;
  nsp_matrix_destroy(Q->Mcoord);
  free(Q->pos);
  free(Q);
}

static int get_string3d(Stack *stack,int k,NspHash *H,String3d *S,int *nf)
{
  char *str;
  int font_type =  DEFAULT_FONT, font_size = DEFAULT_FONT_SIZE;

  nsp_option opts[] ={{"coord",realmatcopy,NULLOBJ,-1},
		      {"str",stringcopy,NULLOBJ,-1},
		      /* XXX {"font_type",s_int,NULLOBJ,-1},
			 {"font_size",s_int,NULLOBJ,-1},
		      */

		      { NULL,t_end,NULLOBJ,-1}};

  if ( get_args_from_hash(*stack,H,opts,&S->Mcoord,&str,&font_type,&font_size)==FAIL) 
    {
      return FAIL;
    }
  if ( S->Mcoord->m != 3 || S->Mcoord->n != 1 ) 
    {
      Scierror("%s: bad coord for object %d \n", NspFnameH(stack) ,k-7);
      return FAIL;
    }

  S->nb_coords = S->Mcoord->n;
  S->coord = S->Mcoord->R;
  *nf += S->Mcoord->n;
  S->str = str;
  S->font_type = font_type ;
  S->font_size = font_size ;
  S->pos = malloc( S->nb_coords * sizeof(VisionPos));

  S->free = free_string3d;
  S->draw_partial = draw_string3d; 
  S->draw_ogl = draw_string3d_ogl; 
  S->zmean = zmean_for_string3d;

  return 0;
}

static void free_string3d(void *Ob)
{
  String3d *Q= Ob;
  nsp_matrix_destroy(Q->Mcoord);
  free(Q->str);
  free(Q->pos);
  free(Q);
}


static void zmean_for_string3d(void *Ob, double z[], HFstruct HF[], int *n, int k)
{
  int j;
  String3d *S= Ob;
  for ( j = 0 ; j < S->nb_coords ; j++)
    if (S->pos[j] == VIN)
      {
	z[*n] = S->coord[3*j+2]; HF[*n].num_obj = k; HF[*n].num_in_obj = j;
	(*n)++; 
      }
}

#define SWAP(i,j)  temp = x[i]; x[i] = x[j]; x[j] = temp; \
                   itemp = p[i]; p[i] = p[j]; p[j] = itemp

#define SWITCH_VALUE 20

#define POP_segment(ia,ib) la--; if ( la >= 0 ) { ia = ileft[la]; ib = iright[la]; }
#define PUSH_segment(ia,ib) ileft[la] = (ia); iright[la] = (ib); la++


void nsp_obj3d_dsortc(double x[], int *n, int p[])
{
  /*
   *     PURPOSE
   *        sort the double precision array x(1..n) in decreasing order
   *        and computes (if perm == 1) the permutation p of the sort :
   *
   *               x_sorted(i) = x(p(i))  1<=i<=n
   *
   *     AUTHOR
   *        B. Pincon (trying to accelerate the initial scilab dsort.f)
   *
   *     NOTES
   *        (i) n must be less than 2**(25) ! due to lengh of work space (ileft, iright)
   *        (ii) quicksort is used with Sedgewick tricks
   */

  int ileft[25], iright[25]; /* to store parts (segments) of the array which stay to sort */
  int i, ia, ib, im, la, j, itemp;
  double temp, pivot;

  for ( i = 0; i < *n ; i++) p[i] = i;

  if ( *n == 1 ) return;

  ia = 0; ib = *n-1;  /* ia..ib is the current part (segment) of the array to sort */
  la = 0;

  while (la >= 0)   /* la >= 0  <=> stay one or some segments to sort */
    {
      if ( ib-ia < SWITCH_VALUE ) /* segment is short enough => insertion sort */
	{
	  for ( i = ia+1 ; i <= ib ; i++ )
	    {
	      j = i;
	      while ( j > ia  &&  x[j] > x[j-1] )
		{
		  SWAP(j,j-1);
		  j--;
		}
	    };
	  POP_segment(ia,ib);  /* get the next segment to sort if any */
	}
      else    /* quicksort */
	{
	  im = (ia+ib)/2;
	  SWAP(ia, im);
	  i = ia+1; j = ib;
	  if (x[i] < x[j])  { SWAP(i, j); }
	  if (x[ia] < x[j]) { SWAP(ia, j); }
	  else if (x[i] < x[ia]) { SWAP(ia, i); }
	  pivot = x[ia];
          /* at this point we have  x[i=ia+1] >= pivot (=x[ia]) >= x[j=ib]  */
	  while (1)
	    {
	      do i++;  while ( x[i] > pivot );
	      do j--;  while ( x[j] < pivot );
	      if (i >= j) break;
	      SWAP(i, j);
	    }
	  SWAP(ia, j);

	  /*  store the longer subdivision in workspace and    */
          /*  update the current segment to be sorted [ia..ib] */
	  if ( j-ia > ib-j )
	    { PUSH_segment(ia,j-1); ia = j+1; }
	  else
	    { PUSH_segment(j+1,ib); ib = j-1; }
	  if ( ib-ia <= 0)
	    { POP_segment(ia,ib); }
	}
    }
}
