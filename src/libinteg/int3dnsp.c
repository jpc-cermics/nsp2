/* 
 * 3d integration routine for Nsp
 * Copyright (C) 2009  Bruno Pincon
 * 
 * The code uses the method described in:
 * Algorithm 720, TOMS, ACM
 * An Algorithm for Adaptive Cubature Over a Collection of 3-Dimensional Simplices
 * Jarle Berntsen, Ronald Cools, Terje O. Espelid
 *
 * The code is quite different from decutet.f (the fortran routines associated to
 * algorithm 720), a few points:
 *   - the integrand function should be real valued (while it can be vector valued
 *     in decutet.f)
 *   - the integrand function should be of the form "f(x,y,z)", see the int3d_f type
 *     here after (it is of the form "f(V)" for decutet.f with V a 3d vector) 
 *   - it is possible to call the integrand function on a collection of integration points. 
 *     This way the routine is more efficient when the function is written in nsp
 *     language with vector evaluation (the nsp interpretor works less).
 *   - integration points are computed by matrix multiplication.
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
 */

#include <string.h>      /* for memcpy */
#include "nsp/math.h"    /* this include also <math.h>, <stdlib.h> (malloc) , machine.h (C2F) and also <float.h> */


typedef int (*int3d_f)(const double *x, const double *y, const double *z, double *values, int *n, void *params);


/* a simple Heap datatype and the associated routines and macros */
typedef struct
{
  double key;
  int loc;
} Heap_elt;

typedef struct
{
  int n;
  int nmax;
  Heap_elt *elts;
} Heap;

#define father(j) ((j)-1)/2
#define left_child(i) (2*(i)+1)
#define right_child(i) (2*(i)+2)

static Heap *init_heap(int kmax)
{
  Heap *H=NULL;
  Heap_elt *Tab=NULL;

  if ( ( H = malloc(sizeof(Heap)) ) == NULL )
    return NULL;

  if ( (Tab = malloc(kmax*sizeof(Heap_elt))) == NULL )
    {
      free(H); return NULL;
    }

  H->nmax = kmax;
  H->n = 0;
  H->elts = Tab;

  return H;
}

static void destroy_heap(Heap **H)
{
  if ( *H != NULL )
    {
      free((*H)->elts);
      free(*H);
      *H = NULL;
    }
}

static int insert_elt(Heap *H, double key, int loc)
{
  int i, j;
  Heap_elt temp;

  if ( H->n >= H->nmax )
    return 0;

  j = H->n;
  H->elts[j].key = key;
  H->elts[j].loc = loc;

  while ( j > 0 )
    {
      i = father(j);
      if ( H->elts[i].key < key )
	{
	  /* swap i and j */
	  temp = H->elts[i]; H->elts[i] = H->elts[j]; H->elts[j] = temp;
	  j = i;
	}
      else
	break;
    }

  H->n++;
  return 1;
}

static int extract_head(Heap *H, int *loc)
{
  int ileft, iright, i, j;
  Heap_elt temp;

  if ( H->n == 0 ) return 0;

  *loc = H->elts[0].loc;

  H->n--;
  H->elts[0] = H->elts[H->n];

  j = 0;

  while ( 1 )
    {
      ileft = left_child(j);
      if ( ileft >= H->n ) break;

      iright = right_child(j);
      if ( iright < H->n )
	i =  H->elts[ileft].key >= H->elts[iright].key ? ileft : iright;
      else
	i = ileft;

      if ( H->elts[j].key < H->elts[i].key )
	{
	  /* swap i and j */
	  temp = H->elts[i]; H->elts[i] = H->elts[j]; H->elts[j] = temp;
	  j = i;
	}
      else
	break;
    }

  return 1;
}
/* end of the Heap stuff */


/* The code uses a 43x4 matrix (Lambda) with the barycentric coordinates of all integration
 * points (there are 43 integration points for a tetrahedron); this matrix should be 
 * initialized at the first call to nsp_int3d. is_init is the global variable which
 * tells if this initialization has been done or not.
 */
static int is_init = 0;

static double Lambda[43*4];

/* this routine initializes the Lambda matrix */
static void init_int3d()
{

  /* datas */

  /* orbit of type 0: */
  /*   (z1, z1, z1; z1)   only one such orbit with one point: (1/4, 1/4, 1/4, 1/4) */

  /* orbit of type 1: */
  /*   (z1, z2, z2 ; z2) => z2 = (1-z1)/3 only z1 is provided */
  /*   one orbit comprises 4 points, 3 such orbits are used so 3 values for z1 */
  double z1_orbit1[3] = {0.379510205167980387748057300876,
			 0.753689235068359830728182577696,
			 0.982654148484406008240470085259};

  /* orbit of type 2: */
  /*   (z1, z1, z2 ; z2) => z2 = (1-2z1)/2 only z1 is provided */
  /*   one orbit comprises 6 points, 1 such orbit is used so 1 value for z1 */
  double z1_orbit2 = 0.449467259981105775574375471447;

  /* the following array lets to compute all points for orbit of type 2 (and 3) */
  int ind[6][2] = { {0,1}, {0,2}, {0,3}, {1,2}, {1,3}, {2,3} }; 

  /* orbit of type 3: */
  /*   (z1, z2, z3 ; z3) => z3 = (1-z1-z2)/2, z1 and z2 are provided */
  /*   one orbit comprises 12 points, 2 such orbit are used */
  double z1_orbit3[2] = {0.506227344977843677082264893876, 0.736298458958971696943019005441};
  double z2_orbit3[2] = {0.0356395827885340437169173969841, 0.190486041934633455699433285302};

  int i, irow, j, k;
  double z1, z2, z3;

  /* computes Lambda */
  for ( k = 0 ; k < 43*4 ; k++ ) Lambda[k] = 0.0;

  /* orbit of type 0 (only one point) */
  i = 0;
  for ( j = 0 ; j < 4 ; j++ )
    Lambda[ i + 43*j ] = 0.25;
 
  irow = 1;
  /* orbit of type 1 (4 points, 3 orbits) */
  for ( k = 0 ; k < 3 ; k++ )
    {
      z1 = z1_orbit1[k];
      z2 = (1.0 - z1)/3.0;
      for ( i = 0 ; i < 4 ; i++, irow++ )
	{
	  for ( j = 0 ; j < 4 ; j++ )
	    Lambda[ irow + 43*j ] = z2;
	  Lambda[irow + 43*i] = z1;
	}
    }

  /* orbit of type 2 (6 points, 1 orbit) */
  z1 = z1_orbit2;
  z2 = (1.0 - 2.0*z1)/2.0;
  for ( i = 0 ; i < 6 ; i++, irow++ )
    {
      Lambda[irow + 43*ind[i][0]] = z1; 
      Lambda[irow + 43*ind[i][1]] = z1; 
      Lambda[irow + 43*ind[5-i][0]] = z2; 
      Lambda[irow + 43*ind[5-i][1]] = z2;
    } 

  /* orbit of type 3 (12 points, 2 orbits) */
  for ( k = 0 ; k < 2 ; k++ )
    {
      z1 = z1_orbit3[k];
      z2 = z2_orbit3[k];
      z3 = (1.0 - z1 - z2)/2.0;
      for ( i = 0 ; i < 6 ; i++, irow+=2 )
	{
	  Lambda[irow + 43*ind[i][0]] = z3; 
	  Lambda[irow + 43*ind[i][1]] = z3; 
	  Lambda[irow + 43*ind[5-i][0]] = z1; 
	  Lambda[irow + 43*ind[5-i][1]] = z2;
	  Lambda[irow + 1 + 43*ind[i][0]] = z3; 
	  Lambda[irow + 1 + 43*ind[i][1]] = z3; 
	  Lambda[irow + 1 + 43*ind[5-i][0]] = z2; 
	  Lambda[irow + 1 + 43*ind[5-i][1]] = z1;
	} 
    }

  is_init = 1;

}


/*  w: weights associated to the major formula
 *  w5, w4, w31, w32, w2, w1: weights associated to the error estimation
 */
static double w[7] = {-0.123001131951839495043519102752,
		       0.855018349372014074906384482699e-01,
		       0.118021998788034059253768205083e-01,
		       0.101900465455732427902646736855e-02,
		       0.274781029468036908044610867719e-01,
		       0.342269148520915110408153517904e-01,
		      0.128431148469725555789001180031e-01};

static double w5[7] = { 0.211921237628032658308230999090,
		       -0.660207516445726284649283745987e-01,
			0.225058824086711710443385047042e-01,
		       -0.375962972067425589765730699401e-03,
			0.710066020561055159657284834784e-02,
			0.156515256061747694921427149028e-02,
		       -0.814530839643584660306807872526e-02};

static double w4[7] = {-0.508105488137100551376844924797e-01,
		        0.104596681151665328209751420525e-01,
		        0.927471438532788763594989973184e-01,
		        0.210489990008917994323967321174e-02,
		        0.379184172251962722213408547663e-01,
		       -0.111747242913563605790923001557e-01,
		       -0.386541758762774673113423570465e-01};

static double w31[7]= {-0.775992773232808462404390159802e-01,
		       -0.527453289659022924847298408064e-01,
		        0.145876238555932704488677626554e-01,
		        0.739374873393616192857532718429e-02,
		       -0.374618791364332892611678523428e-01,
		        0.538502846550653076078817013885e-01,
		       -0.183980865177843057548322735665e-01};

static double w32[7] = {0.181767621501470154602720474731e-01,
			0.179938831310058580533178529022e-01,
			0.713210362750414891598257378898e-01,
		       -0.443935688958258805893448212636e-01,
		       -0.657639036547720234169662790056e-01,
		       -0.101551807522541414699808460583e-01,
			0.265486188970540796821750584204e-01};

static double w2[7] = {-0.867629853722843888927184699428e-01,
		       -0.715881271235661902772072127812e-01,
		        0.886720767790426261677273459523e-02,
		       -0.577885573028655167063092577589e-01,
		        0.430310167581202031805055255554e-01,
		       -0.606467834856775537069463817445e-02,
		        0.319492443333738343104163265406e-01};

static double w1[7] = { 0.510374015624925451319499382594e-01,
		        0.463998830432033721597269299429e-01,
		       -0.191086148397852799983451475821e-01,
		       -0.973768821003670776204287367278e-01,
		        0.180352562073914141268335496511e-01,
		        0.277129527093489643801598303110e-01,
		       -0.176218263109360550515567818653e-01};
 
static double *Weight[7] = {w, w5, w4, w31, w32, w2, w1};

/* this routine computes the weigthed sum of the function values on
 *  the integration points 
 */  
static double weightsum(double *f, double *w)
{
  double s;
  s =  w[0]*f[0]    /* orbit 0 */
    + w[1]*(f[1] + f[2] + f[3] + f[4])    /* 1th orbit of type 1  (4 points)*/ 
    + w[2]*(f[5] + f[6] + f[7] + f[8])    /* 2d  orbit of type 1  (4 points)*/ 
    + w[3]*(f[9] + f[10] + f[11] + f[12]) /* 3d  orbit of type 1  (4 points) */ 
    + w[4]*(f[13] + f[14] + f[15] + f[16] + f[17] + f[18])   /* orbit of type 2 (6 points) */
    + w[5]*(  f[19] + f[20] + f[21] + f[22] + f[23] + f[24]  /* 1th orbit of type 3 (12 points)*/ 
            + f[25] + f[26] + f[27] + f[28] + f[29] + f[30]) 
    + w[6]*(  f[31] + f[32] + f[33] + f[34] + f[35] + f[36]  /* 2d  orbit of type 3 (12 points)*/ 
	    + f[37] + f[38] + f[39] + f[40] + f[41] + f[42]);
  return s;
}



static void split_tetrahedron(const double *X, const double *Y, const double *Z, 
			     double *Xn, double *Yn, double *Zn)
{
  /* split the tetrahedron given X, Y, Z in 8 tetrahedra (stored in Xn, Yn, Zn) */
  /* Xn, Yn, Zn should be well allocated with 32=4*8 */

  double mX[6], mY[6], mZ[6];

  /* mid points edges */
  mX[0] = 0.5*(X[0] + X[1]); mY[0] = 0.5*(Y[0] + Y[1]); mZ[0] = 0.5*(Z[0] + Z[1]);
  mX[1] = 0.5*(X[1] + X[2]); mY[1] = 0.5*(Y[1] + Y[2]); mZ[1] = 0.5*(Z[1] + Z[2]);
  mX[2] = 0.5*(X[0] + X[2]); mY[2] = 0.5*(Y[0] + Y[2]); mZ[2] = 0.5*(Z[0] + Z[2]);
  mX[3] = 0.5*(X[0] + X[3]); mY[3] = 0.5*(Y[0] + Y[3]); mZ[3] = 0.5*(Z[0] + Z[3]);
  mX[4] = 0.5*(X[1] + X[3]); mY[4] = 0.5*(Y[1] + Y[3]); mZ[4] = 0.5*(Z[1] + Z[3]);
  mX[5] = 0.5*(X[2] + X[3]); mY[5] = 0.5*(Y[2] + Y[3]); mZ[5] = 0.5*(Z[2] + Z[3]);

  /* build the new 8 tetrahedra */

  /* 1th tetrahedron */
  Xn[0] = X[0];  Yn[0] = Y[0];  Zn[0] = Z[0];
  Xn[1] = mX[0]; Yn[1] = mY[0]; Zn[1] = mZ[0];
  Xn[2] = mX[2]; Yn[2] = mY[2]; Zn[2] = mZ[2];
  Xn[3] = mX[3]; Yn[3] = mY[3]; Zn[3] = mZ[3];

  /* 2d tetrahedron */
  Xn[4] = X[1];  Yn[4] = Y[1];  Zn[4] = Z[1];
  Xn[5] = mX[0]; Yn[5] = mY[0]; Zn[5] = mZ[0];
  Xn[6] = mX[1]; Yn[6] = mY[1]; Zn[6] = mZ[1];
  Xn[7] = mX[4]; Yn[7] = mY[4]; Zn[7] = mZ[4];

  /* 3d tetrahedron */
  Xn[8]  = X[2];   Yn[8] = Y[2];   Zn[8] = Z[2];
  Xn[9]  = mX[1];  Yn[9] = mY[1];  Zn[9] = mZ[1];
  Xn[10] = mX[2]; Yn[10] = mY[2]; Zn[10] = mZ[2];
  Xn[11] = mX[5]; Yn[11] = mY[5]; Zn[11] = mZ[5];

  /* 4th tetrahedron */
  Xn[12] = X[3];  Yn[12] = Y[3];  Zn[12] = Z[3];
  Xn[13] = mX[3]; Yn[13] = mY[3]; Zn[13] = mZ[3];
  Xn[14] = mX[4]; Yn[14] = mY[4]; Zn[14] = mZ[4];
  Xn[15] = mX[5]; Yn[15] = mY[5]; Zn[15] = mZ[5];

  /* 5th tetrahedron */
  Xn[16] = mX[0]; Yn[16] = mY[0]; Zn[16] = mZ[0];
  Xn[17] = mX[2]; Yn[17] = mY[2]; Zn[17] = mZ[2];
  Xn[18] = mX[3]; Yn[18] = mY[3]; Zn[18] = mZ[3];
  Xn[19] = mX[4]; Yn[19] = mY[4]; Zn[19] = mZ[4];

  /* 6th tetrahedron */
  Xn[20] = mX[0]; Yn[20] = mY[0]; Zn[20] = mZ[0];
  Xn[21] = mX[1]; Yn[21] = mY[1]; Zn[21] = mZ[1];
  Xn[22] = mX[2]; Yn[22] = mY[2]; Zn[22] = mZ[2];
  Xn[23] = mX[4]; Yn[23] = mY[4]; Zn[23] = mZ[4];

  /* 7th tetrahedron */
  Xn[24] = mX[1]; Yn[24] = mY[1]; Zn[24] = mZ[1];
  Xn[25] = mX[2]; Yn[25] = mY[2]; Zn[25] = mZ[2];
  Xn[26] = mX[4]; Yn[26] = mY[4]; Zn[26] = mZ[4];
  Xn[27] = mX[5]; Yn[27] = mY[5]; Zn[27] = mZ[5];

  /* 8th tetrahedron */
  Xn[28] = mX[2]; Yn[28] = mY[2]; Zn[28] = mZ[2];
  Xn[29] = mX[3]; Yn[29] = mY[3]; Zn[29] = mZ[3];
  Xn[30] = mX[4]; Yn[30] = mY[4]; Zn[30] = mZ[4];
  Xn[31] = mX[5]; Yn[31] = mY[5]; Zn[31] = mZ[5];

}

static void prodmat(double *A, double *B, double *C, int n)
{
  /* C = A*B, A is a 43 x 4 matrix, B is a 4 x n matrix */
  int i, j, k, stride;
  for ( j = 0, k = 0, stride = 0 ; j < n ; j++, stride += 4 )
    {
      /* compute column j of C */
      C[k] = A[0]*B[stride] +  A[43]*B[stride+1] + A[86]*B[stride+2] +  A[129]*B[stride+3];
      k++;
      for ( i = 1 ; i < 43 ; i+=6, k+=6 )
	{
	  C[k]   = A[i]*B[stride]   +  A[i+43]*B[stride+1] + A[i+86]*B[stride+2] +  A[i+129]*B[stride+3];
	  C[k+1] = A[i+1]*B[stride] +  A[i+44]*B[stride+1] + A[i+87]*B[stride+2] +  A[i+130]*B[stride+3];
	  C[k+2] = A[i+2]*B[stride] +  A[i+45]*B[stride+1] + A[i+88]*B[stride+2] +  A[i+131]*B[stride+3];
	  C[k+3] = A[i+3]*B[stride] +  A[i+46]*B[stride+1] + A[i+89]*B[stride+2] +  A[i+132]*B[stride+3];
	  C[k+4] = A[i+4]*B[stride] +  A[i+47]*B[stride+1] + A[i+90]*B[stride+2] +  A[i+133]*B[stride+3];
	  C[k+5] = A[i+5]*B[stride] +  A[i+48]*B[stride+1] + A[i+91]*B[stride+2] +  A[i+134]*B[stride+3];
	}
    }
}


static void build_integration_points_coordinates(double *Xn, double *Yn, double *Zn, int nt, 
						 double *xx, double *yy, double *zz)
{
  /* Xn, Yn, Zn are 4 x nt matrices with the coordinates of the nt tetrahedra */
  /* we compute xx, yy and zz which are the integration points (should be allocated with size  43*nt) */
  /* simply we have  xx = Lambda * Xn, yy = Lambda * Yn, zz = Lambda * Zn */
  /* Lambda is a 43 x 4 matrix */

/*   int m = 4, npi = 43; */
/*   double alpha = 1.0, beta = 0.0; */

/*   C2F(dgemm)("N","N", &npi, &nt, &m, &alpha, Lambda, &npi, Xn, &m, */
/* 	     &beta, xx, &npi, 1, 1); */
 
/*   C2F(dgemm)("N","N", &npi, &nt, &m, &alpha, Lambda, &npi, Yn, &m, */
/* 	     &beta, yy, &npi, 1, 1); */

/*   C2F(dgemm)("N","N", &npi, &nt, &m, &alpha, Lambda, &npi, Zn, &m, */
/* 	     &beta, zz, &npi, 1, 1); */

  prodmat(Lambda, Xn, xx, nt);
  prodmat(Lambda, Yn, yy, nt);
  prodmat(Lambda, Zn, zz, nt);

}

/* compute the volume of a tetrahedron using a QR factorization.
 * A kind of luxury... The volume of a tetrahedron is the absolute
 * value (divided by 6) of the following determinant :
 *
 *       | x1 y1 z1 1 |
 *       | x2 y2 z2 1 | 
 *   A = | x3 y3 z3 1 | 
 *       | x4 y4 z4 1 | 
 *
 * So we proceed to a QR factorization of A (A = QR) and computes the
 * determinant of the triangular matrix R. 
 */
static double compute_tetrahedron_volume(double *Xn, double *Yn, double *Zn)
{
  double Mat[4][4], alpha, beta, gamma, temp;
  int i, j, k;

  for ( i = 0 ; i < 4 ; i++ )
    {
      Mat[i][0] = Xn[i];
      Mat[i][1] = Yn[i];
      Mat[i][2] = Zn[i];
      Mat[i][3] = 1.0;
    }
  
  for ( k = 0 ; k < 3 ; k++ )
    {
      alpha = 0.0;
      for ( i = k ; i < 4 ; i++ )
	alpha += Mat[i][k]*Mat[i][k];
      alpha = sqrt(alpha);
      if ( alpha != 0.0 )
	{
	  gamma = 2.0*alpha*(alpha + fabs(Mat[k][k]));
	  alpha = Mat[k][k] >= 0.0 ? -alpha : alpha;
	  Mat[k][k] -= alpha;
	  for ( j = k+1 ; j < 4 ; j++ )
	    {
	      beta = 0.0;
	      for ( i = k ; i < 4 ; i++ )
		beta += Mat[i][k]*Mat[i][j];
	      beta /= gamma;
	      for ( i = k ; i < 4 ; i++ )
		Mat[i][j] -= 2*beta*Mat[i][k];
	    }
	  Mat[k][k] = alpha;
	}
    }

  temp = 1.0;
  for ( k = 0 ; k < 4 ; k++ )
    temp *= Mat[k][k];

  return fabs(temp)/6.0;
}


/* compute for each of the nt tetrahedra defined by the arrays Xt, Yt, Zt (4 x nt arrays) 
 *  the basic formula with its error estimation 
 */
static int basic_integration(double *Xt, double *Yt, double *Zt, int nt, 
			     int3d_f func, void *params, double *It, double *et,
			     double *xx, double *yy, double *zz, double *val, int vecteval)
{
  int k, i, np=43*nt, one=1;;
  double I, e, vol, noise, deg4, deg3, deg1, r, r1, r2, Null[6], temp;

  /* currently these parameters are constants: */
  const double crival = 0.5, facmed = 5, facopt = 10; /*  facopt = facmed/crival */


  /* step 1: compute integration points for all tetrahedra */
  build_integration_points_coordinates(Xt, Yt, Zt, nt, xx, yy, zz);

  /* step 2: compute the function values onto the integration points */
  if ( vecteval )
    {
      if ( ! func(xx, yy, zz, val, &np, params) )   /* if return value is 0 something bad had occured */
	return 0;
    }
  else  /* "scalar" evaluation */
    {
      for ( k = 0 ; k < np ; k++ )
	if ( ! func(xx+k, yy+k, zz+k, val+k, &one, params) )   /* if return value is 0 something bad had occured */
	  return 0;
    }

  /* step 3: for each tetrahedron computes the basic formula with its error estimation */
  for ( k = 0 ; k < nt ; k++ )
    {
      vol = compute_tetrahedron_volume(&(Xt[4*k]),&(Yt[4*k]),&(Zt[4*k]));
      I = weightsum(&(val[43*k]), Weight[0]);
      for ( i = 0 ; i < 6 ; i++ )
	Null[i] = weightsum(&(val[43*k]), Weight[i+1]);

      noise = fabs(I) * 50.0 * DBL_EPSILON;
      deg4 = sqrt( Null[0]*Null[0] + Null[1]*Null[1] );
      deg3 = sqrt( Null[2]*Null[2] + Null[3]*Null[3] );
      deg1 = sqrt( Null[4]*Null[4] + Null[5]*Null[5] );
	  
      if ( deg4 > noise )
	{
	  if ( deg3 != 0.0 )
	    {
	      temp = deg4 / deg3; r1 = temp*temp;
	    }
	  else
	    r1 = 1.0;

	  if ( deg1 != 0.0 )
	    r2 = deg3 / deg1;
	  else
	    r2 = 1.0;

	  r = r1 >= r2 ? r1 : r2;

	  if ( r >= crival )
	    e = facmed * r * deg4;
	  else
	    e = facopt * (r*r) * deg4;
	  
	  e = e >= noise ? e : noise;
	}
      else
	e = noise;

      It[k] = I*vol;
      et[k] = e*vol;
    }

  return 1;
}

/* Compute an approximation I of  int_T func(x) dx  where
 *  T is a collection of nt tetrahedra defined by the arrays
 *  X, Y, Z. The approximation is build such that:
 *
 *     | Iexact - I | <=  max( rtol | I |, atol )
 *
 *  ntmax (which should be >= nt) is the max number of tetrahedra
 *  that could be used in the computation. 
 *  e is the estimated absolute error  | Iexact - I |
 *
 *  return an int which has the following meaning:
 *     -2: ntmax < nt (no computation have been done)
 *     -1: malloc fails (ntmax is certainly too large)
 *      0: evaluation of f fails at the beginning
 *      1: evaluation of f fails at some time but a first approximation of I
 *         and a first error estimate have been computed
 *      2: the tolerance on the error have been not reached
 *         (ntmax may be too small or tolerance too stringent or 
 *         the function is not enough smooth). But an approximation
 *         of I and an error estimate have been computed 
 *      3: successful output
 *
 *  vecteval: should be 1 if the function could be evaluated on a vector
 *            otherwise vecteval is 0
 *   
 */
int nsp_int3d(double *X, double *Y, double *Z, int nt, 
	      int3d_f func, void *params, double *I, double *e, 
	      int ntmax, double atol, double rtol, int vecteval,
	      int *ntused, int *nbcalls)
{
  double *Xt=NULL, *Yt=NULL, *Zt=NULL, *It=NULL, *et=NULL;
  double *xx=NULL, *yy=NULL, *zz=NULL, *vf=NULL;
  double I_loc_old, e_loc_old;
  long double II, ee;
  Heap *H=NULL;
  int k, rep, stat, loc=0;

  if ( ntmax < nt )
    return -2;

  *nbcalls = 0;

  if ( ! is_init )
    init_int3d();

  H = init_heap(ntmax);
  Xt = malloc(4*ntmax*sizeof(double));
  Yt = malloc(4*ntmax*sizeof(double));
  Zt = malloc(4*ntmax*sizeof(double));
  It = malloc(ntmax*sizeof(double));
  et = malloc(ntmax*sizeof(double));

  xx = malloc(8*43*sizeof(double));
  yy = malloc(8*43*sizeof(double));
  zz = malloc(8*43*sizeof(double));
  vf = malloc(8*43*sizeof(double));

  if ( ! ( H && Xt && Yt && Zt && It && et && xx && yy && zz && vf ) )
    {
      stat = -1; goto end;
    }

  memcpy(Xt, X, 4*nt*sizeof(double) );
  memcpy(Yt, Y, 4*nt*sizeof(double) );
  memcpy(Zt, Z, 4*nt*sizeof(double) );

  /* first pass onto all the nt tetrahedra */
  rep = nt/8;
  for ( k = 0 ; k < rep; k++ )
    {
      if ( ! basic_integration(&(Xt[8*4*k]), &(Yt[8*4*k]), &(Zt[8*4*k]), 8, func, params, 
			       &(It[8*k]), &(et[8*k]), xx, yy, zz, vf, vecteval) )
	{
	  stat = 0; goto end;
	}
      
    }
  if ( nt > 8*rep )
    {
      if ( ! basic_integration(&(Xt[8*4*rep]), &(Yt[8*4*rep]), &(Zt[8*4*rep]), nt-8*rep, func, params, 
			       &(It[8*rep]), &(et[8*rep]), xx, yy, zz, vf, vecteval) )
	{
	  stat = 0; goto end;
	}

    } 
  *nbcalls = 43*nt;

  /* enter the results inside the heap */
  for ( k = 0 ; k < nt ; k++ )
    insert_elt(H, et[k], k);      

  /* compute current integral approximation and current error estimate */
  II = 0.0; ee = 0.0;
  for ( k = 0 ; k < nt ; k++ )
    {
      II += (long double) It[k]; ee += (long double) et[k];
    }

  /* test and loop */
  stat = 3;

  while ( ee > Max( rtol*fabs(II) , atol ) )
    {
      /* is there still enough place ? */
      if ( ntmax - nt < 8 )
	{
	  stat = 2; break;
	}

      /* split the tetrahedron which has the max error (into 8 tetrahedra) */
      extract_head(H, &loc);
      split_tetrahedron(&(Xt[4*loc]), &(Yt[4*loc]), &(Zt[4*loc]), &(Xt[4*nt]),  &(Yt[4*nt]),  &(Zt[4*nt]));

      /* compute the integral over the new 8 tetrahedra */
      if ( ! basic_integration(&(Xt[4*nt]), &(Yt[4*nt]), &(Zt[4*nt]), 8, func, params, 
			       &(It[nt]), &(et[nt]), xx, yy, zz, vf, vecteval) )
	{
	  stat = 1; break;
	}

      *nbcalls +=344; /* 8*43 */
      I_loc_old = It[loc]; e_loc_old = et[loc];

      /* remove the tetrahedron number loc with the last new one */
      memcpy(&(Xt[4*loc]), &(Xt[4*(nt+7)]), 4*sizeof(double));
      memcpy(&(Yt[4*loc]), &(Yt[4*(nt+7)]), 4*sizeof(double));
      memcpy(&(Zt[4*loc]), &(Zt[4*(nt+7)]), 4*sizeof(double));
      It[loc] = It[nt+7]; et[loc] = et[nt+7];

      /* insert new results in the heap */
      insert_elt(H, et[loc], loc);
      for ( k = nt ; k < nt+7 ; k++ )
	insert_elt(H, et[k], k);

      /* compute new integral approximation and new error estimate */
      II += (  (  (long double) It[loc]  + (long double) It[nt]   + (long double) It[nt+1] + (long double) It[nt+2] 
	        + (long double) It[nt+3] + (long double) It[nt+4] + (long double) It[nt+5] + (long double) It[nt+6] )
	      - (long double) I_loc_old );

      ee += (  (  (long double) et[loc]  + (long double) et[nt]   + (long double) et[nt+1] + (long double) et[nt+2] 
                + (long double) et[nt+3] + (long double) et[nt+4] + (long double) et[nt+5] + (long double) et[nt+6] ) 
	      -  (long double) e_loc_old ); 

      nt += 7;
    }

   /* recompute the integral approximation and error estimate */
  II = 0.0; ee = 0.0;
  for ( k = 0 ; k < nt ; k++ )
    {
      II += (long double) It[k]; ee += (long double) et[k];
    }
  *I = (double) II;
  *e = (double) ee;

 end:
  *ntused = nt;

  destroy_heap(&H);
  free(Xt); free(Yt); free(Zt); free(It); free(et);
  free(xx); free(yy); free(zz); free(vf);

  return stat;
}

/* An utility routine to split a parallelepiped into 5 tetrahedra */
void para2tetrahedra(double a, double b, double c, double d, double e, double f,
		     double *Xt, double *Yt, double *Zt)
{
  Xt[0] = b; Xt[4] = a; Xt[8] = b; Xt[12]= b; Xt[16]= a;
  Xt[1] = a; Xt[5] = a; Xt[9] = b; Xt[13]= b; Xt[17]= a;
  Xt[2] = a; Xt[6] = b; Xt[10]= b; Xt[14]= b; Xt[18]= b;
  Xt[3] = b; Xt[7] = a; Xt[11]= a; Xt[15]= a; Xt[19]= a;

  Yt[0] = c; Yt[4] = c; Yt[8] = d; Yt[12]= c; Yt[16]= d;
  Yt[1] = d; Yt[5] = c; Yt[9] = d; Yt[13]= c; Yt[17]= d;
  Yt[2] = c; Yt[6] = c; Yt[10]= c; Yt[14]= d; Yt[18]= d;
  Yt[3] = d; Yt[7] = d; Yt[11]= d; Yt[15]= c; Yt[19]= c;

  Zt[0] = e; Zt[4] = e; Zt[8] = e; Zt[12]= f; Zt[16]= f;
  Zt[1] = e; Zt[5] = f; Zt[9] = f; Zt[13]= e; Zt[17]= e;
  Zt[2] = f; Zt[6] = e; Zt[10]= e; Zt[14]= f; Zt[18]= f;
  Zt[3] = f; Zt[7] = e; Zt[11]= e; Zt[15]= f; Zt[19]= f;
}
