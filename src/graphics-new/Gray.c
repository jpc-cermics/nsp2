/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 *
 *    nsp_fec 
 *    modified by Bruno Pincon 01/02/2001 for gain in speed and added 
 *    possibilities to set zmin, zmax by the user and also to set the 
 *    first and last color of the colormap (Bruno.Pincon@iecn.u-nancy.fr)
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"

/* functions used by the modified version : */

static void PaintTriangle (BCG *Xgc,const double *sx,const double *sy,const  double *fxy, 
			   const int *zxy, const double *zlevel,const int *fill);
static void PermutOfSort (const int tab[], int perm[]);
static void FindIntersection(const double *sx,const double *sy,const double *fxy,double z,int inda, int indb,
			     int *xint, int *yint);

/**
 * nsp_draw_matrix:
 * @Xgc: 
 * @x: 
 * @y: 
 * @z: 
 * @n1: 
 * @n2: 
 * @strflag: 
 * @brect: 
 * @aaint: 
 * @l1: 
 * 
 * - z is a (n1,n2) matrix 
 * - x is a (1,n1) matrix 
 * - y is a (1,n2) matrix 
 * - x,y,z are stored as one dimensionnal array in C 
 *
 *  z : is the value of a function on the grid defined by x,y 
 *  on each rectangle the average value of z is computed 
 *  and [zmin,zmax] is linearly remapped to the [colormin,colormap]
 *  values of colors in the current colormap 
 *  the color associated to zmoy is used for filling a specific rectangle 
 * 
 * 
 * Return value: 
 **/

int nsp_draw_matrix_old(BCG *Xgc,double *x, double *y, double *z, int nx, int ny, char *strflag,
		    double *brect, int *aaint, int remap,const int *colminmax,const double *zminmax,
		    const int *colout)
{
  int N = Max((nx),(ny));
  double xx[2],yy[2];
  int *xm,*ym, j, nn1=1,nn2=2;
  /** If Record is on **/
  xx[0]=Mini(x,nx);xx[1]=Maxi(x,nx);
  yy[0]=Mini(y,ny);yy[1]=Maxi(y,ny);
  /** Boundaries of the frame **/
  update_frame_bounds(Xgc,0,"gnn",xx,yy,&nn1,&nn2,aaint,strflag,brect);
  /** Allocation **/

  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    store_Gray(Xgc,x,y,z,nx,ny,strflag,brect,aaint,
	       remap,colminmax,zminmax,colout);

  xm = graphic_alloc(0,N,sizeof(int));
  ym = graphic_alloc(1,N,sizeof(int));
  if ( xm == 0 || ym == 0) 
    {
      sciprint("Running out of memory \n");
      return 0;
    }      

  /** Draw Axis or only rectangle **/
  axis_draw(Xgc,strflag);
  /** Drawing the curves **/
  frame_clip_on(Xgc);
  for ( j =0 ; j < (nx) ; j++)	 xm[j]= XScale(x[j]);
  for ( j =0 ; j < (ny) ; j++)	 ym[j]= YScale(y[j]); 
  Xgc->graphic_engine->fill_grid_rectangles(Xgc,xm,ym,z,nx,ny,remap,colminmax,zminmax);
  frame_clip_off(Xgc);
  Xgc->graphic_engine->drawrectangle(Xgc,Xgc->scales->WIRect1);
  return(0);
}

/**
 * nsp_draw_matrix_shade:
 * @Xgc: 
 * @x: 
 * @y: 
 * @z: 
 * @n1: 
 * @n2: 
 * @strflag: 
 * @brect: 
 * @aaint: 
 * @l1: 
 * 
 * similar to @nsp_draw_matrix but the we assume that the 
 * function is piecewise linear on triangles and we use 
 * code similar to code used in nsp_fec to linearly interpolate 
 * colors in triangles. 
 * The x,y grid is decomposed as follows 
 *  ______ 
 *  | /| /|
 *  |/_|/_|  
 *  | /| /|
 *  |/_|/_|
 * 
 */

#if 0 
/* FIXME */
extern void fillpolyline2D_shade(BCG *Xgc,int *vx, int *vy, int *colors, int n);
extern Gengine GL_gengine;
#endif 

int nsp_draw_matrix(BCG *Xgc,double *x, double *y, double *func, int nx, int ny, char *strflag,
		    double *brect, int *aaint, int remap,const int *colminmax,const double *zminmax,
		    const int *colout)
{
  double xx[2],yy[2];
  int i,*xm,*ym,j,k, Nnode= nx*ny, nn1=1,nn2=2;
  xx[0]=Mini(x,nx);xx[1]=Maxi(x,nx);
  yy[0]=Mini(y,ny);yy[1]=Maxi(y,ny);

  /** Boundaries of the frame **/
  update_frame_bounds(Xgc,0,"gnn",xx,yy,&nn1,&nn2,aaint,strflag,brect);

  /* Storing values if using the Record driver */
  /* FIXME: need one more flag */
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    store_Gray(Xgc,x,y,func,nx,ny,strflag,brect,aaint, remap,colminmax,zminmax,colout);

  /** Allocation **/
  xm = graphic_alloc(0,Nnode,sizeof(int));
  ym = graphic_alloc(1,Nnode,sizeof(int));
  if ( xm == 0 || ym == 0) 
    {
      sciprint("Running out of memory \n");
      return 0;
    }      
  
  for ( i = 0 ; i < nx  ; i++ )
    for ( j = 0 ; j < ny  ; j++) 
      {
	double xp=x[i],yp=y[j];
	scale_f2i(Xgc,&xp,&yp,xm+i+nx*j,ym+i+nx*j,1);
      }

  /* Fec code */
  frame_clip_on(Xgc);
  {
    int nz;
    int whiteid;
    
    double *zlevel, dz, zmin, zmax, fxy[3], sx[3], sy[3];
    int *zone, *fill, kp, perm[3], zxy[3], color_min;

    /* choice between zmin and zmax given by the user or computed
     * with the min and max z values.
     */

    if ( zminmax == NULL  ) { 
      zmin=(double) Mini(func,Nnode);
      zmax=(double) Maxi(func,Nnode);
    } 
    else {
      zmin = Min( zminmax[0] , zminmax[1] );
      zmax = Max( zminmax[0] , zminmax[1] );
    };
      
    whiteid= Xgc->graphic_engine->xget_last(Xgc);
    nz=whiteid;
    
    /* choice for the colormap (in case of a user 's choice 
     * verify the parameter). For the automatic choice I have
     * put colminmax[0]=colominmax[1]=1 in matdes.c  
     */

    if ( colminmax == NULL  )  /* automatic choice (see matdes.c) */
      color_min=1; 
    else if ( colminmax[0] < 1 || colminmax[1] > nz || colminmax[0] > colminmax[1] ) {
      /* ici on pourrait plutot forcer les choses en imposant 1<= colmin < colmax <= nz */
      sciprint("\n\r fec : colminmax badly choosen ! ");
      return ( 0 );
    }
    else {
      color_min = colminmax[0];
      nz = colminmax[1] - colminmax[0] + 1;
    };
      
    /* 
     *  1/ the purpose of the first part is to to compute the "zone" of each point :
     *    
     *    - the array zlevel are the boundaries between the differents zones :
     *        zlevel[0] = zmin, zlevel[nz] = zmax 
     *     and zlevel[i] = zmin + i*(zmax-zmin)/nz
     *  
     *     - if  zlevel[j-1] <= func[i] < zlevel[j]  then zone[i] = j
     *       if func[i] > zmax  then zone[i] = nz+1
     *       if func[i] < zmin  then zone[i] = 0
     *     - the zone j is filled with color fill[j] with
     *       fill[j] = -(j-1 + color_min) if 1 <= j <= nz
     *     - if colout == NULL
     *        fill[0] = color attributed for fill[1]     ---> this behavior may be changed ...
     *        fill[nz+1] = color attributed for fill[nz] --/
     *       else 
     *        fill[0]=- colout[0];
     *        fill[1]=- colout[1];
     */
 
    /* allocations for some arrays ... */

    zone = graphic_alloc(2,Nnode,sizeof(int));
    zlevel = graphic_alloc(3,nz+1,sizeof(double));
    fill  = graphic_alloc(4,nz+2,sizeof(int));
    if ( (zone == NULL) || (zlevel == NULL) || (fill  == NULL)) 
      {
	Scistring("fec: malloc No more Place\n");
	return 0;
      }
    /* compute the fill array (fill = - num color) */
    fill[1] = - color_min;
    for ( i = 2 ; i <= nz ; i++ ) fill[i] = fill[i-1] - 1;
    if ( colout == NULL) 
      {
	fill[0] =  fill[1] ; fill[nz+1] = fill[nz];
      }
    else 
      {
	fill[0] = - colout[0] ; fill[nz+1] = - colout[1];
      }

    /* compute the zlevels */
    dz = (zmax - zmin)/nz;
    for (i = 0 ; i < nz ; i++) zlevel[i] = zmin + i*dz;
    zlevel[nz] = zmax;

    /* finaly compute the zone of each point */

    for ( i = 0 ; i < Nnode ; i++ ) {
      if ( func[i] > zmax )
	zone[i] = nz+1;
      else if ( func[i] < zmin )
	zone[i] = 0;
      else
	zone[i] = floor( (func[i] - zmin)/dz ) + 1;
    };

    /* 
     * 2/ loop on the triangles : each triangle is finally decomposed 
     *    into its differents zones (polygons) by the function PaintTriangle   
     */

#if 0     
    if (  Xgc->graphic_engine == &GL_gengine ) 
      {
	for ( i = 0 ; i < nx -1 ; i++ )
	  for ( j = 0 ; j < ny -1 ; j++) 
	    {
	      int pos[4],colors[4],xp[4],yp[4];
	      pos[0]=i+nx*j; pos[1]=pos[0]+nx;  pos[2]=pos[0]+nx+1, pos[3]=pos[0]+1;
	      for ( k = 0 ; k < 4 ; k++ ) 
		{
		  colors[k]= fill[zone[pos[k]]];
		  xp[k]= xm[pos[k]];
		  yp[k]= ym[pos[k]];
		}
	      fillpolyline2D_shade(Xgc,xp,yp,colors,4);
	    }
      }
    else 
#endif
      {
	for ( i = 0 ; i < nx -1 ; i++ )
	  for ( j = 0 ; j < ny -1 ; j++) 
	    {
	      int pos[3],tr;
	      for ( tr = 0 ; tr < 2 ; tr ++ )
		{
		  if ( tr == 0 ) 
		    { 
		      /* upper triangle 
		       *  ___
		       *  | /
		       *  |/
		       */
		      pos[0]=i+nx*j; pos[1]=pos[0]+nx;  pos[2]=pos[0]+nx+1;
		    }
		  else 
		    {
		      /* lower triangle 
		       * 
		       *   /|
		       *  /_|
		       */
		      pos[0]=i+nx*j;  pos[1]=pos[0]+1;   pos[2]=pos[0]+nx+1;
		    }
		  /* retrieve node numbers and functions values */
		  for ( k = 0 ; k < 3 ; k++ ) zxy[k]= zone[pos[k]];
		  /* get the permutation perm so as zxy[perm] is sorted */
		  PermutOfSort(zxy, perm); 
		  /* apply the permutation to get the triangle 's vertices
		   * in increasing zone (zxy[0] <= zxy[1] <= zxy[2]) 
		   */
		  for ( k = 0 ; k < 3 ; k++ ) {
		    kp = perm[k];
		    zxy[k]= zone[pos[kp]];
		    fxy[k]= func[pos[kp]];
		    sx[k] = xm[pos[kp]];
		    sy[k] = ym[pos[kp]];
		  }
		  /* call the "painting" function */
		  PaintTriangle(Xgc,sx, sy, fxy, zxy, zlevel, fill);
		}
	    }
      }
  }
  frame_clip_off(Xgc);

  /** Draw Axis or only rectangle **/
  axis_draw(Xgc,strflag);

  return(0);
}


/**
 * nsp_draw_matrix_1:
 * @Xgc: 
 * @z: 
 * @n1: 
 * @n2: 
 * @strflag: 
 * @brect: 
 * @aaint: 
 * @l1: 
 * 
 *  z : of size n1*n2 
 *  the z value is interpreted as a color number inside the current colormap
 *  z[i,j] is used as the color of a square [i-0.5,i+0.5] [j-0.5,j+0.5]
 * 
 * Return value: 
 **/

int nsp_draw_matrix_1(BCG *Xgc,double *z, int nr, int nc, char *strflag, double *brect, int *aaint, 
		      int remap,const int *colminmax,const double *zminmax)
{
  double xx[]={0.5, nc+0.5} ;
  double yy[]={0.5, nr+0.5} ;
  int nn1=1,nn2=2;
  int *xm,*ym,j;

  /** Boundaries of the frame **/
  update_frame_bounds(Xgc,0,"gnn",xx,yy,&nn1,&nn2,aaint,strflag,brect);

  /** If Record is on **/
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    store_Gray1(Xgc,z,nr,nc,strflag,brect,aaint,remap,colminmax,zminmax);

  /** Allocation **/
  xm = graphic_alloc(0,nc+1,sizeof(int));
  ym = graphic_alloc(1,nr+1,sizeof(int));
  if ( xm == 0 || ym == 0) 
    {
      sciprint("Running out of memory \n");
      return 0;
    }      

  /** Draw Axis or only rectangle **/
  axis_draw(Xgc,strflag);
  /** Drawing the curves **/
  frame_clip_on(Xgc);
  for ( j =0 ; j < (nc+1) ; j++) xm[j]= XScale(j+0.5);
  for ( j =0 ; j < (nr+1) ; j++) ym[j]= YScale((nr -j+0.5));
  Xgc->graphic_engine->fill_grid_rectangles1(Xgc,xm,ym,z,nr,nc,remap,colminmax,zminmax);
  frame_clip_off(Xgc);
  Xgc->graphic_engine->drawrectangle(Xgc,Xgc->scales->WIRect1);
  return(0);
}

/**
 * nsp_draw_matrix_2:
 * @Xgc: 
 * @z: 
 * @n1: 
 * @n2: 
 * @xrect: 
 * 
 * like nsp_draw_matrix_1
 * but xrect here give the rectangle in which the 
 * grayplot is to be drawn using the current scale
 * 
 * Return value: 
 **/

int nsp_draw_matrix_2(BCG *Xgc,double *z,int nr, int nc, double *xrect, 
		      int remap,const int *colminmax,const double *zminmax)
{
  double xx[]={ xrect[0],xrect[2]};
  double yy[]={ xrect[1],xrect[3]};
  int xx1[2],yy1[2];
  int *xm,*ym,  j;
  /** If Record is on **/
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    store_Gray2(Xgc,z,nr,nc,xrect,remap,colminmax,zminmax);

  /** Boundaries of the frame **/
  scale_f2i(Xgc,xx,yy,xx1,yy1,2);
  xm = graphic_alloc(0,nc+1,sizeof(int));
  ym = graphic_alloc(1,nr+1,sizeof(int));
  if ( xm == 0 || ym == 0 )
    {
      Scistring("Xgray: running out of memory\n");
      return 0;
    }
  for ( j =0 ; j < (nc+1) ; j++)	 
    xm[j]= (int) (( xx1[1]*j + xx1[0]*(nc-j) )/((double) nc));
  for ( j =0 ; j < (nr+1) ; j++)	 
    ym[j]= (int) (( yy1[0]*j + yy1[1]*(nr-j) )/((double) nr));
  frame_clip_on(Xgc);
  Xgc->graphic_engine->fill_grid_rectangles1(Xgc,xm,ym,z,nr,nc,remap,colminmax,zminmax);
  frame_clip_off(Xgc);
  return(0);
}


/*
 *  Iso contour with grey level or colors 
 *  for a function defined by finite elements 
 *  ( f is linear on triangles )
 *  we give two versions of the function : 
 *     - a quick version wich only fill triangles according to the average 
 *     value of f on a triangle (no more such version now ?)
 *     - and a slow version but more sexy which use the fact that f is linear
 *     on each triangle.
 *  Nodes (x[no],y[no])
 *  Triangles (Matrix: [ numero, no1,no2,no3,iflag;...]
 *  func[no] : Function value on Nodes.
 *  Nnode : number of nodes 
 *  Ntr   : number of triangles 
 *  strflag,legend,brect,aint : see plot2d
 *  zminmax   : to set (optionnaly) the min and max level
 *  colminmax : to set (optionnaly) the first and last color to use
 *
 *  modified by Bruno Pincon 01/02/2001 for gain in speed and added 
 *  possibilities to set zmin, zmax by the user and also to set the 
 *  first and last color of the colormap (Bruno.Pincon@iecn.u-nancy.fr)
 */

int nsp_fec(BCG *Xgc,double *x, double *y, double *triangles, double *func, int *Nnode, int *Ntr, 
	    char *strflag,const char *legend, double *brect, int *aaint,const double *zminmax,
	    const int *colminmax, const int *colout)
{
  int i,*xm,*ym,j,k, n1=1;

  /** Boundaries of the frame **/
  update_frame_bounds(Xgc,0,"gnn",x,y,&n1,Nnode,aaint,strflag,brect);

  /* Storing values if using the Record driver */
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    /* added zminmax and colminmax (bruno) */
    store_Fec(Xgc,x,y,triangles,func,Nnode,Ntr,strflag,legend,brect,aaint,zminmax,colminmax,colout);


  /** Allocation **/
  xm = graphic_alloc(0,*Nnode,sizeof(int));
  ym = graphic_alloc(1,*Nnode,sizeof(int));
  if ( xm == 0 || ym == 0) 
    {
      sciprint("Running out of memory \n");
      return 0;
    }      
  
  scale_f2i(Xgc,x,y,xm,ym,*Nnode);

  /* Fec code */
  frame_clip_on(Xgc);
  {
    /********************************************************************
     *	 beginning of the code modified by Bruno 01/02/2001  
     ********************************************************************/
    
    int nz;
    int whiteid;
    
    double *zlevel, dz, zmin, zmax, fxy[3], sx[3], sy[3];
    int *zone, *fill, kp, perm[3], zxy[3], color_min;
    int ii[3];

    /* choice between zmin and zmax given by the user or computed
     *   with the min and max z values. In matdes.c I have put 
     * zminmax[0]= zminmax[1]=0 if the user don't give this argument 
     */

    if ( zminmax[0]==zminmax[1] ) { 
      zmin=(double) Mini(func,*Nnode); 
      zmax=(double) Maxi(func,*Nnode);
    } 
    else {
      zmin = Min( zminmax[0] , zminmax[1] );
      zmax = Max( zminmax[0] , zminmax[1] );
    };
      
    whiteid= Xgc->graphic_engine->xget_last(Xgc);
    nz=whiteid;
    
    /* choice for the colormap (in case of a user 's choice 
     *   verify the parameter). For the automatic choice I have
     * put colminmax[0]=colominmax[1]=1 in matdes.c  
     */

    if ( colminmax[0] == colminmax[1] )  /* automatic choice (see matdes.c) */
      color_min=1; 
    else if ( colminmax[0] < 1 || colminmax[1] > nz || colminmax[0] > colminmax[1] ) {
      /* ici on pourrait plutot forcer les choses en imposant 1<= colmin < colmax <= nz */
      sciprint("\n\r fec : colminmax badly choosen ! ");
      return ( 0 );
    } 
    else {
      color_min = colminmax[0];
      nz = colminmax[1] - colminmax[0] + 1;
    };
      
    /* 
     *  1/ the purpose of the first part is to to compute the "zone" of each point :
     *    
     *    - the array zlevel are the boundaries between the differents zones :
     *
     *        zlevel[0] = zmin, zlevel[nz] = zmax 
     *     and zlevel[i] = zmin + i*(zmax-zmin)/nz
     *  
     *     - if  zlevel[j-1] <= func[i] < zlevel[j]  then zone[i] = j
     *       if func[i] > zmax  then zone[i] = nz+1
     *       if func[i] < zmin  then zone[i] = 0
     *     - the zone j is filled with color fill[j] with
     *       fill[j] = -(j-1 + color_min) if 1 <= j <= nz
     *     - if colout == NULL
     *        fill[0] = color attributed for fill[1]     ---> this behavior may be changed ...
     *        fill[nz+1] = color attributed for fill[nz] --/
     *       else 
     *        fill[0]=- colout[0];
     *        fill[1]=- colout[1];
     */
 
    /* allocations for some arrays ... */

    zone = graphic_alloc(2,(*Nnode),sizeof(int));
    zlevel = graphic_alloc(3,nz+1,sizeof(double));
    fill  = graphic_alloc(4,nz+2,sizeof(int));
    if ( (zone == NULL) || (zlevel == NULL) || (fill  == NULL)) 
      {
	Scistring("fec: malloc No more Place\n");
	return 0;
      }
    /* compute the fill array (fill = - num color) */
    fill[1] = - color_min;
    for ( i = 2 ; i <= nz ; i++ ) fill[i] = fill[i-1] - 1;
    if ( colout == NULL) 
      {
	fill[0] =  fill[1] ; fill[nz+1] = fill[nz];
      }
    else 
      {
	fill[0] = - colout[0] ; fill[nz+1] = - colout[1];
      }

    /* compute the zlevels */
    dz = (zmax - zmin)/nz;
    for (i = 0 ; i < nz ; i++) zlevel[i] = zmin + i*dz;
    zlevel[nz] = zmax;

    /* finaly compute the zone of each point */
    for ( i = 0 ; i < (*Nnode) ; i++ ) {
      if ( func[i] > zmax )
	zone[i] = nz+1;
      else if ( func[i] < zmin )
	zone[i] = 0;
      else
	zone[i] = floor( (func[i] - zmin)/dz ) + 1;
    };
    /* 
       2/ loop of the triangles : each triangle is finally decomposed 
          into its differents zones (polygons) by the function PaintTriangle   
    */
    for ( j = 0 ; j < *Ntr ; j++) {

      /* retrieve node numbers and functions values */
      for ( k = 0 ; k < 3 ; k++ ) {
	ii[k] = (int) triangles[j+(*Ntr)*(k+1)] - 1;
	zxy[k] = zone[ii[k]];
      }

      /* get the permutation perm so as zxy[perm] is sorted */
      PermutOfSort(zxy, perm); 

      /* apply the permutation to get the triangle 's vertices
         in increasing zone (zxy[0] <= zxy[1] <= zxy[2]) */
      for ( k = 0 ; k < 3 ; k++ ) {
	kp = perm[k];
	sx[k]  = xm[ii[kp]];   sy[k]  = ym[ii[kp]];
	fxy[k] = func[ii[kp]]; zxy[k] = zone[ii[kp]];
      };

      /* call the "painting" function */
      PaintTriangle(Xgc,sx, sy, fxy, zxy, zlevel, fill);

    };
  }

  /********************************************************************
   *                     end of the modified code
   ********************************************************************/

  frame_clip_off(Xgc);

  /** Draw Axis or only rectangle **/
  axis_draw(Xgc,strflag);

  /** Drawing the Legends **/
  if ((int)strlen(strflag) >=1  && strflag[0] == '1')
    {
      int style = -1;
      nsp_legends(Xgc,legend_ur,1,&style,legend,"@"); 
    }
  return(0);
}


/********************************************************************
 * functions used by the modified code (Bruno 01/02/2001)
 ********************************************************************/

static void PermutOfSort (const int *tab, int *perm)
{
  /* 
   * get the permutation perm[3] which sort the array tab[3] in increasing order 
   */
  perm[0]=0; perm[1] = 1; perm[2] = 2;
  if ( tab[1] < tab[0] ) {
    perm[1]=0 ; perm[0] = 1;
  };
  if ( tab[2] < tab[perm[1]] ) {   /* sort not finish */
    if ( tab[2] < tab[perm[0]] ) {
      perm[2] = perm[1]; perm[1] = perm[0]; perm[0] = 2; 
    }
    else {
      perm[2] = perm[1] ; perm[1] = 2;
    };
  };
}


static void PaintTriangle (BCG *Xgc,const double *sx,const double *sy,const  double *fxy, 
			   const int *zxy, const double *zlevel,const int *fill)
{
  /* 
     arguments :
     ---------
     sx, sy : vertices coordinates of a triangle (Pi=(sx[i],sy[i]) i=0,1,2)
     fxy    : fxy[i], (i=0,1,2) value of an affine function on the vertex Pi
     zxy    : zone of Pi : zxy[i]=j if  zlevel[j-1] <= fxy[i] < zlevel[j]
     zlevel : a (0..nz) vector given the boundaries for color filling
     fill   : fill[j] is the color pattern associated with zone[j] 
              when fill[j]=0 the zone is not painted 
     purpose : this function decompose the triangle into its different
     -------   zones (which gives polygones) and send them to the
               graphic driver. This is something like the shade function
               (see Plo3d.c) but a little different as in shade
               a color is directly associated with each vertex.
  */

  int nb0, edge, izone, color;
  int nr, resx[5],resy[5];
  int xEdge2, yEdge2, xEdge, yEdge; 

  /* 
   * case of only one color for the triangle : 
   */

  if ( zxy[0] == zxy[2] ) {
    resx[0]=inint(sx[0]); resx[1]=inint(sx[1]);  resx[2]=inint(sx[2]);
    resy[0]=inint(sy[0]); resy[1]=inint(sy[1]);  resy[2]=inint(sy[2]);
    color = fill[zxy[0]]; nr = 3;
    if ( color != 0 ) Xgc->graphic_engine->fillpolylines(Xgc,resx,resy,&color,1,nr);
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
  
  resx[0]=inint(sx[0]); resy[0]=inint(sy[0]); nr = 1; edge = 0;
  if ( nb0 == 0 ) {    /* the intersection point is on Edge1 but the next point
                          of the poly is P1 */
    resx[1]=inint(sx[1]); resy[1]=inint(sy[1]); nr++;
    edge = 1;          /* the next intersection points will be on edge1 */
  } else nb0--;
  /* the intersection point on edge (0 or 1) : */
  FindIntersection(sx, sy, fxy, zlevel[zxy[0]], edge, edge+1, &xEdge, &yEdge);
  resx[nr]=xEdge; resy[nr]=yEdge; nr++;
  /* the last point of the first poly (edge 2) : */
  FindIntersection(sx, sy, fxy, zlevel[zxy[0]], 0, 2, &xEdge2, &yEdge2);
  resx[nr]=xEdge2; resy[nr]=yEdge2; nr++;
  color = fill[zxy[0]];
  if ( color != 0 )   Xgc->graphic_engine->fillpolylines(Xgc,resx,resy,&color,1,nr);

  /*------------------------------------+ 
  | compute the intermediary polygon(s) |
  +------------------------------------*/

  for ( izone = zxy[0]+1 ; izone < zxy[2] ; izone++ ) {
    resx[0] = xEdge2; resy[0] = yEdge2;          /* the 2 first points are known */
    resx[1] = xEdge;  resy[1] = yEdge; nr = 2;
    if ( edge == 0 ) {  /* the intersection point is perhaps on edge 0 */
      if (nb0 == 0 ) {  /* no it is on edge 1 but the next point of the poly is P1 */
	resx[2]=inint(sx[1]); resy[2]=inint(sy[1]); nr++;
	edge = 1;          /* the next intersection points will be on edge1 */
      } else nb0--;
    };
    /* the intersection point on edge (0 or 1) : */
    FindIntersection(sx, sy, fxy, zlevel[izone], edge, edge+1, &xEdge, &yEdge);
    resx[nr]=xEdge; resy[nr]=yEdge; nr++;
    /* the last point of the first poly (edge 2) : */
    FindIntersection(sx, sy, fxy, zlevel[izone], 0, 2, &xEdge2, &yEdge2);
    resx[nr]=xEdge2; resy[nr]=yEdge2; nr++;
    color = fill[izone];
    if ( color != 0 )    Xgc->graphic_engine->fillpolylines(Xgc,resx,resy,&color,1,nr);
  };

  /*-----------------------+ 
  | compute the last poly  |
  +-----------------------*/

  resx[0] = xEdge2; resy[0] = yEdge2;         /* the 2 first points are known */
  resx[1] = xEdge;  resy[1] = yEdge; nr = 2;
  if ( edge == 0 ) {                          /* the next point of the poly is P1 */
    resx[2]=inint(sx[1]); resy[2]=inint(sy[1]); nr++;
  };
  /* the last point is P2 */
  resx[nr] = inint(sx[2]); resy[nr] = inint(sy[2]); nr++;
  color = fill[zxy[2]];
  if ( color != 0 )  Xgc->graphic_engine->fillpolylines(Xgc,resx,resy,&color,1,nr);
}

static void FindIntersection(const double *sx,const double *sy,const double *fxy,double z,int inda, int indb,
			     int *xint, int *yint)
{
  double alpha =  (z - fxy[inda])/(fxy[indb] - fxy[inda]);
  *xint = inint((1 - alpha)*sx[inda] + alpha*sx[indb]);
  *yint = inint((1 - alpha)*sy[inda] + alpha*sy[indb]);
}

/*
 * FIXME: 
 *   set of generic functions which are to be moved on 
 *   perigen.h 
 *   they can be accelerated for each periXXXX
 */

void nsp_remap_colors(BCG *Xgc,int remap,int *colmin,int *colmax,double *zmin, double *zmax,double *coeff,
		      const int *colminmax,const double *zminmax,const double z[],int zn)
{
  *colmin=1;
  *colmax=Xgc->graphic_engine->xget_last(Xgc);
  if ( remap == TRUE) 
    {
      if ( colminmax != NULL) 
	{
	  *colmax = Min(*colmax,colminmax[1]);
	  *colmin = Max(*colmin,colminmax[0]);
	}
      if ( zminmax == NULL) 
	{
	  *zmin = Mini(z,zn);
	  *zmax = Maxi(z,zn);
	  *coeff = ( *zmin == *zmax ) ? 1.0: 1.0/(*zmax-*zmin);
	}
      else 
	{
	  *zmin = zminmax[0];
	  *zmax = zminmax[1];
	  *coeff = ( *zmin == *zmax ) ? 1.0: 1.0/(*zmax-*zmin);
	}
    }
}


/**
 * fill_grid_rectangles1_gen:
 * @Xgc: 
 * @x: array of int of size nc+1
 * @y: array of int of size nr+1
 * @z: array of double of size nr*nc
 * 
 * A generic function for drawing a set of rectangles 
 * which is accelerated on Gtk driver (see periGtk.c) 
 * 
 *  x : of size nc+1 gives the x-values of the grid 
 *  y : of size nr+1 gives the y-values of the grid 
 *  z : of size nr*nc  gives the color to be used 
 *      on the rectangle defined by ( x[i],y[j], x[i+1],y[j+1])
 *  if zremap = %f then z values are considered as color id 
 *  if zremap = %t zvalues are remapped to colors 
 *      if colminmax== NULL then zmin,zmax are remapped to the min and max 
 *         values of current colormap
 *      else  the zminmax range is remapped to the colmimax range 
 *         and rectangles outside the range are not drawn 
 * 
 **/

void fill_grid_rectangles1_gen(BCG *Xgc,const int x[],const int y[],const double z[], int nr, int nc,
			       int remap,const int *colminmax,const double *zminmax)
{
  int colmin,colmax;
  double zmin,zmax,coeff;
  int i,j,fill[1],cpat,xz[2];
  cpat = Xgc->graphic_engine->xget_pattern(Xgc);
  Xgc->graphic_engine->xget_windowdim(Xgc,xz,xz+1);
  
  nsp_remap_colors(Xgc,remap,&colmin,&colmax,&zmin,&zmax,&coeff,colminmax,zminmax,z,nr*nc);

  for (i = 0 ; i < nr ; i++)
    for (j = 0 ; j < nc ; j++)
      {
	int w,h;
	fill[0]= (remap == FALSE) ? rint(z[i+nr*j]) : rint((colmax-colmin)*(z[i+nr*j] - zmin)*coeff + colmin);
	/* do not draw rectangles which are outside the colormap range */
	if ( fill[0] < colmin || fill[0] > colmax ) continue ;
	Xgc->graphic_engine->xset_pattern(Xgc,fill[0]);
	w=Abs(x[j+1]-x[j]);
	h=Abs(y[i+1]-y[i]);
	/* We don't trace rectangle which are totally out **/
	if ( w != 0 && h != 0 && x[j] < xz[0] && y[i] < xz[1] && x[j]+w > 0 && y[i]+h > 0 )
	  if ( Abs(x[j]) < int16max && Abs(y[i+1]) < int16max && w < uns16max && h < uns16max)
	    {
	      int rect[]={x[j],y[i],w,h};
	      Xgc->graphic_engine->fillrectangle(Xgc,rect);
	    }
      }
  Xgc->graphic_engine->xset_pattern(Xgc,cpat);
}



/**
 * fill_grid_rectangles_gen:
 * @Xgc: 
 * @x: 
 * @y: 
 * @z: 
 * @nx: 
 * @ny: 
 * 
 * A generic function for drawing a set of rectangles 
 * which is accelerated on Gtk driver (see periGtk.c) 
 * 
 *  x : of size nx gives the x-values of the grid 
 *  y : of size ny gives the y-values of the grid 
 *  z : of size (nx)*(ny). 
 *  the rectangle ( x[i],y[j], x[i+1],y[j+1]) 
 *  the average value of z is computed on each corner of the rectangle 
 *  defined by ( x[i],y[j], x[i+1],y[j+1]). Then this value is 
 *  converted to a colorvalue using the current colormap.
 * 
 **/

void fill_grid_rectangles_gen(BCG *Xgc,const int x[],const int y[],const double z[], int nx, int ny,
			      int remap,const int *colminmax,const double *zminmax)
{
  int colmin,colmax;
  double zmax,zmin,coeff,zmoy;
  int i,j,color,cpat,xz[2];
  
  nsp_remap_colors(Xgc,remap,&colmin,&colmax,&zmin,&zmax,&coeff,colminmax,zminmax,z,nx*ny);
  cpat = Xgc->graphic_engine->xget_pattern(Xgc);
  Xgc->graphic_engine->xget_windowdim(Xgc,xz,xz+1);
  for (i = 0 ; i < (nx)-1 ; i++)
    for (j = 0 ; j < (ny)-1 ; j++)
      {
	int w,h;
	zmoy=1/4.0*(z[i+nx*j]+z[i+nx*(j+1)]+z[i+1+nx*j]+z[i+1+nx*(j+1)]);
	color = (remap == FALSE) ? rint(zmoy) : rint((colmax-colmin)*(zmoy - zmin)*coeff + colmin);
	if (color < colmin || color > colmax ) continue ;
	Xgc->graphic_engine->xset_pattern(Xgc,color);
        w=Abs(x[i+1]-x[i]);h=Abs(y[j+1]-y[j]);
	/* We don't trace rectangle which are totally out **/
	if ( w != 0 && h != 0 && x[i] < xz[0] && y[j+1] < xz[1] && x[i]+w > 0 && y[j+1]+h > 0 )
	  {
	    if ( Abs(x[i]) < int16max && Abs(y[j+1]) < int16max && w < uns16max && h < uns16max)
	      {
		int rect[]={x[i],y[j+1],w,h};
		Xgc->graphic_engine->fillrectangle(Xgc,rect);
	      }
	    else 
	      {
		/* fprintf(stderr,"Rectangle too large \n"); */
	      }
	  }
      }
  Xgc->graphic_engine->xset_pattern(Xgc,cpat);
}

