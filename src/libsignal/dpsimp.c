#include "signal.h"

static int c__1 = 1;
static double c_b11 = 0.;

static int signal_bezstp (double *p1, int *n1, double *p2, int *n2, double *a, int *na,
			  double *u, int *nu, int *l, double *x, double *v, double *w,
			  double *best, int *ipb, double *errr);
static int signal_dpmul1 (double *p1, int *d1, double *p2, int *d2, double *p3);
static int signal_dpmul (double *p1, int *d1, double *p2, int *d2, double *p3, int *d3);
  
/*
 *    Etant donnes une fraction rationnelle donnee par ses polynomes 
 *    numerateur et denominateurs, ce sous programme retourne le numerateur 
 *    et le denominateur de sa representation simplifiee. 
 *
 *    subroutine dpsimp(a,na,b,nb,as,nas,bs,nbs,w,ierr) 
 * 
 *    double precision a(na+1),b(nb+1),as(*),bs(*),w,er 
 *    int na,nb,nas,nbs,ierr 
 * 
 *    a    :  tableau contenant les coefficients du numerateur range 
 *            par puissance croissante.(entree) 
 *    na   :  degre du numerateur (entree) 
 *    b    :  tableau contenant les coefficients du denominateur range 
 *            par puissance croissante. (entree) 
 *    nb   :  degre du denominateur (entree) 
 *    a1   :  tableau contenant les coefficients du numerateur range 
 *            par puissance croissante.(sortie) 
 *    na1  :  degre+1 du numerateur (sortie) 
 *    b1   :  tableau contenant les coefficients du denominateur range 
 *            par puissance croissante. (sortie) 
 *    nb1  :  degre+1 du denominateur (sortie) 
 *    les implantations de a et a1, b et b1 peuvent etre confondues. 
 *    Dans les cas ou les zones memoires de a (resp b) et a1 (resp b1) se 
 *    chevauchent, l'adresse de a1 (resp b1) doit etre au moins egale a 
 *    l'adresse de a  (resp b) 
 *    w    :  tableau de travail de taille: 
 *            2*(na+nb)+min(na,nb)+10*max(na,nb)+3*max(na,nb)**2+4 
 *    ierr : 
 *            en entree ierr specifie l'espace memoire disponible dans w 
 *            en sortie: 
 *    ierr=0 : ok 
 *    ierr=1 : denominateur nul 
 *    ierr=2 : espace memoire insuffisant on retourne les polynomes 
 *
 *   Copyright S. Steer INRIA 1990 
 */

int signal_dpsimp (double *a,const int *na,double *b,const int *nb,
		   double *a1, int *na1, double *b1, int *nb1, double *w, int *ierr)
{
  double d__1, t, t1, t2, er;
  int i1, nden, maxw, nnum, lfree, n0, lw, nz, la0, lb0, ipb[6]={0}, nna, nnb;

  --a;
  --b;
  --a1;
  --b1;
  --w;

  lw = ((*na + *nb) << 1) + 1 + Min (*na, *nb) + 3;
  maxw = *ierr;
  *ierr = 0;
  /* 
   * degre reel des polynomes 
   */
  nnb = *nb + 1;
 L8:
  --nnb;
  if (nnb < 0)
    {
      *ierr = 1;
      return 0;
    }
  if (b[nnb + 1] == 0.)
    {
      goto L8;
    }
  nna = *na + 1;
 L9:
  --nna;
  if (nna < 0)
    {
      goto L20;
    }
  if (a[nna + 1] == 0.)
    {
      goto L9;
    }
  /* 
   *    elimination des racines en zero 
   */
  la0 = 0;
 L10:
  ++la0;
  if (a[la0] == 0.)
    {
      goto L10;
    }
  *na1 = nna - (la0 - 1);
  nz = la0 - 1;
  /* 
   */
  lb0 = 0;
 L11:
  ++lb0;
  if (b[lb0] == 0.)
    {
      goto L11;
    }
  *nb1 = nnb - (lb0 - 1);
  nz -= lb0 - 1;
  /* 
   */
  n0 = Max (*na1, *nb1) + 1;
  lfree = lw + n0 * 10 + n0 * 3 * n0;
  if (lfree >= maxw && *na1 > 0 && *nb1 > 0)
    {
      *ierr = 2;
    }
  if (lfree >= maxw || *na1 == 0 || *nb1 == 0)
    {
      if (nz == 0)
	{
	  i1 = *na1 + 1;
	  C2F(dcopy) (&i1, &a[la0], &c__1, &a1[1], &c__1);
	  i1 = *nb1 + 1;
	  C2F(dcopy) (&i1, &b[lb0], &c__1, &b1[1], &c__1);
	}
      else if (nz > 0)
	{
	  nsp_dset (&nz, &c_b11, &a1[1], &c__1);
	  i1 = *na1 + 1;
	  C2F(dcopy) (&i1, &a[la0], &c__1, &a1[nz + 1], &c__1);
	  i1 = *nb1 + 1;
	  C2F(dcopy) (&i1, &b[lb0], &c__1, &b1[1], &c__1);
	  *na1 += nz;
	}
      else
	{
	  i1 = *na1 + 1;
	  C2F(dcopy) (&i1, &a[la0], &c__1, &a1[1], &c__1);
	  i1 = -nz;
	  nsp_dset (&i1, &c_b11, &b1[1], &c__1);
	  i1 = *nb1 + 1;
	  C2F(dcopy) (&i1, &b[lb0], &c__1, &b1[-nz + 1], &c__1);
	  *nb1 -= nz;
	}
      ++(*na1);
      ++(*nb1);
      return 0;
    }
  /*    normalize highest degree coefficients of num and den 
   */
  t1 = a[nna + 1];
  t2 = b[nnb + 1];
  i1 = *na1 + 1;
  d__1 = 1. / t1;
  C2F(dscal) (&i1, &d__1, &a[la0], &c__1);
  i1 = *nb1 + 1;
  d__1 = 1. / t2;
  C2F(dscal) (&i1, &d__1, &b[lb0], &c__1);
  /* 
   */
  signal_recbez (&a[la0], na1, &b[lb0], nb1, &w[1], ipb, &w[lw], &er);
  if (er > .001)
    {
      goto L30;
    }
  nden = ipb[4] - ipb[3];
  nnum = ipb[5] - ipb[4];
  if (*na1 != nnum - 1)
    {
      t = w[ipb[4] - 1];
      t = 1. / t;
      if (nz == 0)
	{
	  C2F(dcopy) (&nnum, &w[ipb[4]], &c__1, &a1[1], &c__1);
	  C2F(dcopy) (&nden, &w[ipb[3]], &c__1, &b1[1], &c__1);
	  C2F(dscal) (&nden, &t, &b1[1], &c__1);
	}
      else if (nz > 0)
	{
	  C2F(dcopy) (&nnum, &w[ipb[4]], &c__1, &a1[nz + 1], &c__1);
	  nsp_dset (&nz, &c_b11, &a1[1], &c__1);
	  nnum += nz;
	  C2F(dcopy) (&nden, &w[ipb[3]], &c__1, &b1[1], &c__1);
	  C2F(dscal) (&nden, &t, &b1[1], &c__1);
	}
      else if (nz < 0)
	{
	  nz = -nz;
	  C2F(dcopy) (&nnum, &w[ipb[4]], &c__1, &a1[1], &c__1);
	  C2F(dcopy) (&nden, &w[ipb[3]], &c__1, &b1[nz + 1], &c__1);
	  nsp_dset (&nz, &c_b11, &b1[1], &c__1);
	  C2F(dscal) (&nden, &t, &b1[nz + 1], &c__1);
	  nden += nz;
	}
      d__1 = -t * t1 / t2;
      C2F(dscal) (&nnum, &d__1, &a1[1], &c__1);
    }
  else
    {
      if (nz == 0)
	{
	  C2F(dcopy) (&nnum, &a[la0], &c__1, &a1[1], &c__1);
	  C2F(dcopy) (&nden, &b[lb0], &c__1, &b1[1], &c__1);
	}
      else if (nz > 0)
	{
	  C2F(dcopy) (&nnum, &a[la0], &c__1, &a1[nz + 1], &c__1);
	  nsp_dset (&nz, &c_b11, &a1[1], &c__1);
	  nnum += nz;
	  C2F(dcopy) (&nden, &b[lb0], &c__1, &b1[1], &c__1);
	}
      else
	{
	  nz = -nz;
	  C2F(dcopy) (&nnum, &a[la0], &c__1, &a1[1], &c__1);
	  C2F(dcopy) (&nden, &b[lb0], &c__1, &b1[nz + 1], &c__1);
	  nsp_dset (&nz, &c_b11, &b1[1], &c__1);
	  nden += nz;
	}
      C2F(dscal) (&nnum, &t1, &a1[1], &c__1);
      C2F(dscal) (&nden, &t2, &b1[1], &c__1);
    }
  *na1 = nnum;
  *nb1 = nden;
  return 0;
 L20:
  a1[1] = 0.;
  b1[1] = 1.;
  *na1 = 1;
  *nb1 = 1;
  return 0;
 L30:
  if (nz == 0)
    {
      i1 = *na1 + 1;
      C2F(dcopy) (&i1, &a[la0], &c__1, &a1[1], &c__1);
      i1 = *nb1 + 1;
      C2F(dcopy) (&i1, &b[lb0], &c__1, &b1[1], &c__1);
    }
  else if (nz > 0)
    {
      nsp_dset (&nz, &c_b11, &a1[1], &c__1);
      i1 = *na1 + 1;
      C2F(dcopy) (&i1, &a[la0], &c__1, &a1[nz + 1], &c__1);
      i1 = *nb1 + 1;
      C2F(dcopy) (&i1, &b[lb0], &c__1, &b1[1], &c__1);
      *na1 += nz;
    }
  else
    {
      i1 = *na1 + 1;
      C2F(dcopy) (&i1, &a[la0], &c__1, &a1[1], &c__1);
      i1 = -nz;
      nsp_dset (&i1, &c_b11, &b1[1], &c__1);
      i1 = *nb1 + 1;
      C2F(dcopy) (&i1, &b[lb0], &c__1, &b1[-nz + 1], &c__1);
      *nb1 -= nz;
    }
  ++(*na1);
  ++(*nb1);
  C2F(dscal) (na1, &t1, &a1[1], &c__1);
  C2F(dscal) (nb1, &t2, &b1[1], &c__1);
  return 0;
}

static double c_b6 = 0.;
static int c__2 = 2;
static double c_b13 = 1.;

/* 
 * calcule la factorisation de bezout de deux polynomes p1,p2 c'est a dire 
 * les polynomes x,y,u, v et pgcd de degre minimal et tels que : 
 *                     [x  u] 
 *           [p1 p2] * [    ] = [0 Pgcd] 
 *                     [y  v] 
 *!liste d'appel 
 *    subroutine recbez(p1,n1,p2,n2,best,ipb,w,err) 
 *    double precision  p1(n1+1),p2(n2+1),w(*),best(*),err 
 * 
 *    p1 : vecteur des coefficients du polynome p1 ranges par puissances 
 *         croissantes 
 *    n1 : degre du polynome p1 
 *    p2 : vecteur des coefficients du polynome p2 ranges par puissances 
 *         croissantes 
 *    n2 : degre du polynome p2 
 * 
 *    best : tableau resultat doit etre de taille au moins : 
 *           (2*(n1+n2)+min(n1,n2)+3)  [x y u v pgcd] 
 *           x=best(1:ipb(2)-1),y=best(ipb(2):ipb(3)-1) 
 *           u=best(ipb(3):ipb(4)-1) v=best(ipb(4):ipb(5)-1) 
 *           pgcd=best(ipb(5):ipb(6)-1) 
 *    ipb :vecteur des pointeurs sur les adresses de debut de x,y,u,v,pgcd 
 *         dans best ( voir ci-dessus) 
 *    w: tableau de travail  w(7*n0+3*n0*n0) ou n0=max(n1,n2)+1 
 *    err : estimee de l'erreur d'equation 
 *
 *S Steer INRIA 1989 
 */

int signal_recbez (double *p1, int *n1, double *p2, int *n2, double *best,
		   int *ipb, double *w, double *err)
{
  int i1;

  double cres;
  int l;
  int n0, ia, la, na, n02;
  int iu, iw, lu, nu;
  int nn1, nn2;

  /* Parameter adjustments */
  --p1;
  --p2;
  --best;
  --ipb;
  --w;

  /* Function Body */
  *err = C2F(dlamch) ("o", 1L);
  ia = 1;
  nn1 = *n1;
  nn2 = *n2;
  /* 
   *degre reel des polynomes 
   * 
   */
  ++nn1;
 L1:
  --nn1;
  if (nn1 < 0)
    {
      goto L30;
    }
  if (p1[nn1 + 1] == 0.)
    {
      goto L1;
    }
  /* 
   */
  ++nn2;
 L2:
  --nn2;
  if (nn2 < 0)
    {
      goto L30;
    }
  if (p2[nn2 + 1] == 0.)
    {
      goto L2;
    }
  /* 
   */
  n0 = Max (nn1, nn2) + 1;
  n02 = n0 * (n0 + 1);
  na = n0 + 1;
  nsp_dset (&n02, &c_b6, &w[ia], &c__1);
  iu = ia + n02;
  nu = n0 + 1;
  i1 = n02 << 1;
  nsp_dset (&i1, &c_b6, &w[iu], &c__1);
  iw = iu + (n02 << 1);

  /* 
   */
  la = ia + na - 1;
  lu = iu + nu - 1 + (n0 << 1) * nu;
  /* 
   */
  i1 = nn1 + 1;
  C2F(dcopy) (&i1, &p1[1], &c__1, &w[la - 1], &na);
  i1 = nn2 + 1;
  C2F(dcopy) (&i1, &p2[1], &c__1, &w[la], &na);
  i1 = nu + 1;
  nsp_dset (&c__2, &c_b13, &w[lu - 1 - (nu << 1)], &i1);
  /* 
   */
  i1 = n0;
  for (l = 1; l <= i1; ++l)
    {
      --la;
      lu = lu - 1 - (nu << 1);
      /*    la :  pointeur sur le coin en haut a gauche de la matrice a courante 
       *    lu : pointeur sur le coin en haut a gauche de la matrice u courante 
       */
      signal_bezstp (&p1[1], &nn1, &p2[1], &nn2, &w[la], &na, &w[lu], &nu, &l,
		     &w[la - 1 + na], &w[lu - 1 - (nu << 1)], &w[iw],
		     &best[1], &ipb[1], err);
      /* L20: */
    }
  return 0;
 L30:
  *err = 0.;
  ipb[1] = 1;
  if (Min (nn1, nn2) == 0)
    {
      goto L70;
    }
  cres = (double) (nn1 - nn2);
  if (cres < 0.)
    {
      goto L40;
    }
  else if (cres == 0.)
    {
      goto L50;
    }
  else
    {
      goto L60;
    }
 L40:
  /*    p1=0 
   */
  
  i1 = nn2 + 1;
  C2F(dcopy) (&i1, &p2[1], &c__1, &best[1], &c__1);
  ipb[2] = ipb[1] + nn2 + 1;
  best[ipb[2]] = 0.;
  ipb[3] = ipb[2] + 1;
  best[ipb[3]] = 1.;
  ipb[4] = ipb[3] + 1;
  best[ipb[4]] = 1.;
  ipb[5] = ipb[4] + 1;
  best[ipb[5]] = 0.;
  ipb[6] = ipb[5] + 1;
  return 0;
 L50:
  /*    p1=0,p2=0  la decomposition n'est pas definie 
   */
  best[1] = 0.;
  ipb[2] = ipb[1] + 1;
  best[ipb[2]] = 1.;
  ipb[3] = ipb[2] + 1;
  best[ipb[3]] = 0.;
  ipb[4] = ipb[3] + 1;
  best[ipb[4]] = 0.;
  ipb[5] = ipb[4] + 1;
  best[ipb[5]] = 1.;
  ipb[6] = ipb[5] + 1;
  return 0;
 L60:
  /*    p2=0 
   */

  i1 = nn1 + 1;
  C2F(dcopy) (&i1, &p1[1], &c__1, &best[1], &c__1);
  ipb[2] = ipb[1] + nn1 + 1;
  best[ipb[2]] = 1.;
  ipb[3] = ipb[2] + 1;
  best[ipb[3]] = 0.;
  ipb[4] = ipb[3] + 1;
  best[ipb[4]] = 0.;
  ipb[5] = ipb[4] + 1;
  best[ipb[5]] = 1.;
  ipb[6] = ipb[5] + 1;
  return 0;
  /* 
   */
 L70:
  best[1] = 1.;
  ipb[2] = 2;
  cres = (double) (nn1 - nn2);
  if (cres < 0.)
    {
      goto L90;
    }
  else if (cres == 0.)
    {
      goto L95;
    }
  else
    {
      goto L100;
    }
 L90:
  /*    p1=cte 
   */
  best[ipb[2]] = 1. / p1[1];
  ipb[3] = ipb[2] + 1;
  best[ipb[3]] = 0.;
  ipb[4] = ipb[3] + 1;
  i1 = nn2 + 1;
  C2F(dcopy) (&i1, &p2[1], &c__1, &best[ipb[4]], &c__1);
  ipb[5] = ipb[4] + nn2 + 1;
  best[ipb[5]] = -p1[1];
  ipb[6] = ipb[5] + 1;
  return 0;
 L95:
  /*    p1=cte,p2=cte 
   */
  if (Abs (p1[1]) > Abs (p2[1]))
    {
      goto L90;
    }
  /* 
   */
 L100:
  /*    p2=cte 
   */
  best[ipb[2]] = 0.;
  ipb[3] = ipb[2] + 1;
  best[ipb[3]] = 1. / p2[1];
  ipb[4] = ipb[3] + 1;
  best[ipb[4]] = -p2[1];
  ipb[5] = ipb[4] + 1;
  i1 = nn1 + 1;
  C2F(dcopy) (&i1, &p1[1], &c__1, &best[ipb[5]], &c__1);
  ipb[6] = ipb[5] + nn1 + 1;
  return 0;
  /* 
   */
}				/* recbez_ */


static int c_n1 = -1;
static int c__0 = 0;

int signal_bezstp (double *p1, int *n1, double *p2, int *n2, double *a, int *na,
		   double *u, int *nu, int *l, double *x, double *v, double *w,
		   double *best, int *ipb, double *errr)
{

  int a_dim1, a_offset, u_dim1, u_offset, x_dim1, x_offset, v_dim1, v_offset,
    i1, i2;
  double d__1, d__2;
  double fact;
  double errd, erri;
  double c__;
  int k;
  double s;
  double z__;
  int n0, m1, m2;
  int nb;
  int ll;
  double mm;
  int nn, np, iw, nw;
  double dt0;
  int iw1;

  int iuv, ixy;

  --p1;
  --p2;
  x_dim1 = *na;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  v_dim1 = *nu;
  v_offset = v_dim1 + 1;
  v -= v_offset;
  u_dim1 = *nu;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  --w;
  --best;
  --ipb;

  /* Function Body */
  /* eps = C2F(dlamch) ("p", 1L); */
  n0 = Max (*n1, *n2) + 1;
  /*Computing MAX 
   */
  i1 = *n1 - *n2;
  m1 = Max (i1, 0);
  /*Computing MAX 
   */
  i1 = *n2 - *n1;
  m2 = Max (i1, 0);
  ll = *l << 1;
  iuv = 1;
  ixy = iuv + ll;
  iw1 = ixy + ll;
  iw = iw1 + n0;

  /* 
   */
  i1 = *l;
  for (k = 1; k <= i1; ++k)
    {
      nsp_ctrlpack_giv (&a[k + (n0 + 1 - k) * a_dim1],
			&a[k + 1 + (n0 + 1 - k) * a_dim1], &c__, &s);
      C2F(drot) (&n0, &a[k + a_dim1], na, &a[k + 1 + a_dim1], na, &c__, &s);
      a[k + 1 + (n0 + 1 - k) * a_dim1] = 0.;
      C2F(drot) (&ll, &u[k + u_dim1], nu, &u[k + 1 + u_dim1], nu, &c__, &s);
      if (k == 1 && *l < n0)
	{
	  i2 = n0 - 1;
	  C2F(dcopy) (&i2, &a[a_dim1 + 2], na, &x[x_offset], na);
	  C2F(dcopy) (&ll, &u[u_dim1 + 2], nu, &v[v_offset], nu);
	}
      /* L10: */
    }
  /* 
   */
  C2F(dcopy) (&ll, &u[*l + u_dim1], nu, &w[iuv], &c__1);
  C2F(dcopy) (&ll, &u[*l + 1 + u_dim1], nu, &w[ixy], &c__1);
  /* 
   */
  if (*l <= (i1 = *n1 - *n2, Abs (i1)))
    {
      goto L99;
    }
  fact = a[*l + (n0 - *l + 1) * a_dim1];
  if (*l > 1)
    {
      /*Computing 2nd power 
       */
      d__1 = w[ixy + (m1 << 1)];
      /*Computing 2nd power 
       */
      d__2 = w[ixy + 1 + (m2 << 1)];
      mm = d__1 * d__1 + d__2 * d__2;
      z__ =
	w[iuv + (m1 << 1)] * w[ixy + (m1 << 1)] + w[iuv + 1 +
						    (m2 << 1)] * w[ixy + 1 +
								   (m2 << 1)];
    }
  else
    {
      /*Computing 2nd power 
       */
      d__1 = w[ixy + (m1 << 1)];
      mm = d__1 * d__1;
      z__ = w[iuv + (m1 << 1)] * w[ixy + (m1 << 1)];
    }
  if (mm != 0.)
    {
      /*    on abaisse le degre de [u,v] 
       */
      z__ = -z__ / mm;
      C2F(daxpy) (&ll, &z__, &w[ixy], &c__1, &w[iuv], &c__1);
    }
  /* 
   *    normalisation pour que le terme de plus haut degre du pgcd soit 1 et 
   *      que le determinant soit 1 
   * 
   */
  if (fact == 0.)
    {
      goto L99;
    }
  d__1 = 1. / fact;
  C2F(dscal) (&ll, &d__1, &w[iuv], &c__1);
  dt0 =
    w[ixy + ((*l - 1) << 1)] * w[iuv + (*l << 1) - 1]
    - w[ixy + (*l << 1) - 1] * w[iuv + ((*l - 1) << 1)];
  if (dt0 == 0.)
    {
      goto L99;
    }
  d__1 = 1. / dt0;
  C2F(dscal) (&ll, &d__1, &w[ixy], &c__1);
  dt0 = 1.;
  /* 
   *    estimation de l'erreur directe 
   * 
   *    p1*x 
   */
  i1 = *l - m1;
  C2F(dcopy) (&i1, &w[ixy + (m1 << 1)], &c__2, &w[iw1], &c_n1);
  i1 = *l - 1 - m1;
  signal_dpmul1 (&p1[1], n1, &w[iw1], &i1, &w[iw]);
  nw = *n1 + *l - 1 - m1;
  /*    p1*x+p2*y 
   */
  i1 = *l - m2;
  C2F(dcopy) (&i1, &w[ixy + 1 + (m2 << 1)], &c__2, &w[iw1], &c_n1);
  i1 = *l - 1 - m2;
  signal_dpmul (&p2[1], n2, &w[iw1], &i1, &w[iw], &nw);
  i1 = nw + 1;
  errd = C2F(ddot) (&i1, &w[iw], &c__1, &w[iw], &c__1);
  /*    p1*u 
   */
  if (*l - 1 - m1 > 0)
    {
      i1 = *l - 1 - m1;
      C2F(dcopy) (&i1, &w[iuv + 2 + (m1 << 1)], &c__2, &w[iw1], &c_n1);
      i1 = *l - 2 - m1;
      signal_dpmul1 (&p1[1], n1, &w[iw1], &i1, &w[iw]);
      nw = *n1 + *l - 2 - m1;
    }
  else
    {
      signal_dpmul1 (&p1[1], n1, &w[iuv + (m1 << 1)], &c__0, &w[iw]);
      nw = *n1;
    }
  /*    p1*u+p2*v 
   */
  if (*l - 1 - m2 > 0)
    {
      i1 = *l - 1 - m2;
      C2F(dcopy) (&i1, &w[iuv + 3 + (m2 << 1)], &c__2, &w[iw1], &c_n1);
      i1 = *l - 2 - m2;
      signal_dpmul (&p2[1], n2, &w[iw1], &i1, &w[iw], &nw);
    }
  else
    {
      signal_dpmul (&p2[1], n2, &w[iuv + 1 + (m2 << 1)], &c__0, &w[iw], &nw);
    }
  /*    p 
   */
  np = n0 - *l;
  i1 = np + 1;
  C2F(dcopy) (&i1, &a[*l + a_dim1], na, &w[iw1], &c__1);
  C2F(daxpy) (&np, &z__, &a[*l + 1 + a_dim1], na, &w[iw1], &c__1);
  i1 = np + 1;
  d__1 = 1. / fact;
  C2F(dscal) (&i1, &d__1, &w[iw1], &c__1);
  /*    p1*u+p2*v-p 
   */
  i1 = np + 1;
  nsp_calpack_ddif (&i1, &w[iw1], &c__1, &w[iw], &c__1);
  i1 = nw + 1;
  errd += C2F(ddot) (&i1, &w[iw], &c__1, &w[iw], &c__1);
  /* 
   *    estimation de l'erreur inverse 
   *    ------------------------------ 
   *    y 
   */
  i1 = *n1 - np + 1;
  C2F(dcopy) (&i1, &w[ixy + 1 + (m2 << 1)], &c__2, &w[iw], &c_n1);
  /*    p*y+p1 
   */
  i1 = *n1 - np;
  signal_dpmul1 (&w[iw1], &np, &w[iw], &i1, &w[iw]);
  i1 = *n1 + 1;
  nsp_dadd (i1, &p1[1], c__1, &w[iw], c__1);
  i1 = *n1 + 1;
  erri = C2F(ddot) (&i1, &w[iw], &c__1, &w[iw], &c__1);
  /*    x 
   */
  i1 = *n2 - np + 1;
  C2F(dcopy) (&i1, &w[ixy + (m1 << 1)], &c__2, &w[iw], &c_n1);
  /*    p*x 
   */
  i1 = *n2 - np;
  signal_dpmul1 (&w[iw1], &np, &w[iw], &i1, &w[iw]);
  /*    p*x-p2 
   */
  i1 = *n2 + 1;
  nsp_calpack_ddif (&i1, &p2[1], &c__1, &w[iw], &c__1);
  i1 = *n2 + 1;
  erri += C2F(ddot) (&i1, &w[iw], &c__1, &w[iw], &c__1);
  /*      write(6,*) np,errd,erri 
   * 
   */
  if (Max (erri, errd) < *errr)
    {
      *errr = Max (erri, errd);
      /*Computing MAX 
       */
      i1 = 0, i2 = n0 - *l;
      nb = Max (i1, i2);
      ipb[1] = 1;
      /*    pgcd 
       */
      i1 = nb + 1;
      C2F(dcopy) (&i1, &a[*l + a_dim1], na, &best[ipb[1]], &c__1);
      if (*l > 1)
	{
	  i1 = nb + 1;
	  C2F(daxpy) (&i1, &z__, &a[*l + 1 + a_dim1], na, &best[ipb[1]],
		      &c__1);
	}
      i1 = nb + 1;
      d__1 = 1. / fact;
      C2F(dscal) (&i1, &d__1, &best[ipb[1]], &c__1);
      ipb[2] = ipb[1] + nb + 1;
      if (*l > 1)
	{
	  /*Computing MAX 
	   */
	  i1 = *n2 - nb;
	  nn = Max (i1, 1);
	  C2F(dcopy) (&nn, &w[iuv + ((*l - nn) << 1)], &c__2, &best[ipb[2]],
		      &c_n1);
	  ipb[3] = ipb[2] + nn;
	  /*Computing MAX 
	   */
	  i1 = *n1 - nb;
	  nn = Max (i1, 1);
	  C2F(dcopy) (&nn, &w[iuv + 1 + ((*l - nn) << 1)], &c__2,
		      &best[ipb[3]], &c_n1);
	  ipb[4] = ipb[3] + nn;
	}
      else
	{
	  best[ipb[2]] = w[iuv];
	  ipb[3] = ipb[2] + 1;
	  best[ipb[3]] = w[iuv + 1];
	  ipb[4] = ipb[3] + 1;
	}
      nn = *n2 + 1 - nb;
      C2F(dcopy) (&nn, &w[ixy + ((*l - nn) << 1)], &c__2, &best[ipb[4]],
		  &c_n1);
      ipb[5] = ipb[4] + nn;
      nn = *n1 + 1 - nb;
      C2F(dcopy) (&nn, &w[ixy + 1 + ((*l - nn) << 1)], &c__2, &best[ipb[5]],
		  &c_n1);
      ipb[6] = ipb[5] + nn;
    }
  /* 
   */
 L99:
  return 0;
}				/* bezstp_ */


/*
 * ce sous programme effectue le produit polynomial: 
 * 
 *               p3(x) = p1(x) * p2(x) 
 * 
 *     subroutine dpmul1(p1,d1,p2,d2,p3) 
 *    double precision p1(d1+1),p2(d2+1),p3(d1+d2+1) 
 *    int d1,d2,d3 
 * 
 *    p1 : contient les coefficient du premier polynome ranges 
 *         suivant les puissances croissantes 
 *    p2 : contient les coefficients du second polynome ranges 
 *         suivant les puissances croissantes 
 *    p3 :contient les coefficient du resultats. 
 *        p3 peut designer la meme adresse que p1 ou p2 
 *    d1,d2 : degre respectifs des  polynomesp1 et p2 
 */

int signal_dpmul1 (double *p1, int *d1, double *p2, int *d2, double *p3)
{
  int i1;
  int k, l, d3, l1, l2, l3, m3;

  /* Parameter adjustments */
  --p3;
  --p2;
  --p1;

  /* Function Body */
  l = 1;
  l1 = *d1 + 1;
  l2 = *d2 + 1;
  d3 = *d1 + *d2;
  l3 = d3 + 1;
  /* 
   */
  m3 = Min (l1, l2);
  i1 = m3;
  for (k = 1; k <= i1; ++k)
    {
      p3[l3] = C2F(ddot) (&l, &p1[l1], &c__1, &p2[l2], &c_n1);
      ++l;
      --l3;
      --l1;
      --l2;
      /* L10: */
    }
  --l;
  /* 
   */
  if (l1 == 0)
    {
      goto L30;
    }
  m3 = l1;
  i1 = m3;
  for (k = 1; k <= i1; ++k)
    {
      p3[l3] = C2F(ddot) (&l, &p1[l1], &c__1, &p2[1], &c_n1);
      --l1;
      --l3;
      /* L20: */
    }
  goto L40;
 L30:
  if (l2 == 0)
    {
      goto L40;
    }
  m3 = l2;
  i1 = m3;
  for (k = 1; k <= i1; ++k)
    {
      p3[l3] = C2F(ddot) (&l, &p1[1], &c__1, &p2[l2], &c_n1);
      --l2;
      --l3;
      /* L31: */
    }
  /* 
   */
 L40:
  if (l3 == 0)
    {
      return 0;
    }
  m3 = l3;
  i1 = m3;
  for (k = 1; k <= i1; ++k)
    {
      --l;
      p3[l3] = C2F(ddot) (&l, &p1[1], &c__1, &p2[1], &c_n1);
      --l3;
      /* L41: */
    }
  return 0;
}				/* dpmul1_ */

int
signal_dpmul (double *p1, int *d1, double *p2, int *d2, double *p3, int *d3)
{
  int i1, i2;
  double d__1, d__2, d__3;

  /* Local variables */
  int dmin__, dmax__;
  int dsum, i3, j, k, l;
  double w;
  int e1, e2;
  double w1;
  double eps;

  /*!purpose 
   * ce sous programme effectue le calcul: 
   * 
   *               p3(x) = p3(x) + (p1(x) * p2(x)) 
   * 
   *ou  p1,  p2  et p3 sont des polynomes de degres d1, d2 et d3, 
   *respectivement. Ils sont classes en ordre croissant. 
   *Tous  les  parametres  sont  d'entree, sauf p3 et d3 qui sont 
   *d'entree-sortie. 
   *! 
   *auteur: c. klimann. 
   *date: 22 fevrier 1985. 
   *&&var 
   */
  /* Parameter adjustments */
  --p3;
  --p2;
  --p1;

  /* Function Body */
  eps = C2F(dlamch) ("p", 1L);
  /*&&ker 
   */
  dsum = *d1 + *d2;
  /*fixation de dmax et dmin 
   */
  dmax__ = *d1;
  if (*d2 > *d1)
    {
      dmax__ = *d2;
    }
  dmin__ = dsum - dmax__;
  /*fixation de d3 
   */
  if (*d3 >= dsum)
    {
      goto L1;
    }
  e1 = *d3 + 2;
  e2 = dsum + 1;
  i1 = e2;
  for (i3 = e1; i3 <= i1; ++i3)
    {
      p3[i3] = 0.;
      /* L2: */
    }
  *d3 = dsum;
 L1:
  /*cas des degres egaux a zero 
   */
  if (*d1 == 0 || *d2 == 0)
    {
      goto L53;
    }
  /*produit et addition 
   */
  e1 = 1;
  e2 = dmin__ + 1;
  i1 = e2;
  for (i3 = e1; i3 <= i1; ++i3)
    {
      w = C2F(ddot) (&i3, &p1[1], &c__1, &p2[1], &c_n1);
      w1 = p3[i3] + w;
      /*Computing MAX 
       */
      d__2 = (d__1 = p3[i3], Abs (d__1)), d__3 = Abs (w);
      if (Abs (w1) > eps * Max (d__2, d__3))
	{
	  p3[i3] = w1;
	}
      else
	{
	  p3[i3] = 0.;
	}
      /* L10: */
    }
  k = 1;
  if (*d1 == *d2)
    {
      goto L21;
    }
  e1 = dmin__ + 2;
  e2 = dmax__ + 1;
  /*si d1 > d2 
   */
  if (*d1 < *d2)
    {
      goto L25;
    }
  i1 = e2;
  for (i3 = e1; i3 <= i1; ++i3)
    {
      ++k;
      i2 = dmin__ + 1;
      w = C2F(ddot) (&i2, &p1[k], &c__1, &p2[1], &c_n1);
      w1 = p3[i3] + w;
      /*Computing MAX 
       */
      d__2 = (d__1 = p3[i3], Abs (d__1)), d__3 = Abs (w);
      if (Abs (w1) > eps * Max (d__2, d__3))
	{
	  p3[i3] = w1;
	}
      else
	{
	  p3[i3] = 0.;
	}
      /* L20: */
    }
 L21:
  e1 = dmax__ + 2;
  e2 = dsum + 1;
  l = 1;
  j = dmin__ + 1;
  i1 = e2;
  for (i3 = e1; i3 <= i1; ++i3)
    {
      --j;
      ++k;
      ++l;
      w = C2F(ddot) (&j, &p1[k], &c__1, &p2[l], &c_n1);
      w1 = p3[i3] + w;
      /*Computing MAX 
       */
      d__2 = (d__1 = p3[i3], Abs (d__1)), d__3 = Abs (w);
      if (Abs (w1) > eps * Max (d__2, d__3))
	{
	  p3[i3] = w1;
	}
      else
	{
	  p3[i3] = 0.;
	}
      /* L40: */
    }
  return 0;
  /*si d2 > d1 
   */
 L25:
  i1 = e2;
  for (i3 = e1; i3 <= i1; ++i3)
    {
      ++k;
      i2 = dmin__ + 1;
      w = C2F(ddot) (&i2, &p2[k], &c_n1, &p1[1], &c__1);
      w1 = p3[i3] + w;
      /*Computing MAX 
       */
      d__2 = (d__1 = p3[i3], Abs (d__1)), d__3 = Abs (w);
      if (Abs (w1) > eps * Max (d__2, d__3))
	{
	  p3[i3] = w1;
	}
      else
	{
	  p3[i3] = 0.;
	}
      /* L30: */
    }
  e1 = dmax__ + 2;
  e2 = dsum + 1;
  l = 1;
  j = dmin__ + 1;
  i1 = e2;
  for (i3 = e1; i3 <= i1; ++i3)
    {
      --j;
      ++k;
      ++l;
      w = C2F(ddot) (&j, &p1[l], &c__1, &p2[k], &c_n1);
      w1 = p3[i3] + w;
      /*Computing MAX 
       */
      d__2 = (d__1 = p3[i3], Abs (d__1)), d__3 = Abs (w);
      if (Abs (w1) > eps * Max (d__2, d__3))
	{
	  p3[i3] = w1;
	}
      else
	{
	  p3[i3] = 0.;
	}
      /* L50: */
    }
  return 0;
  /*cas des degres egaux a zero 
   */
 L53:
  if (*d1 == 0 && *d2 == 0)
    {
      goto L100;
    }
  e1 = 1;
  if (*d1 == 0)
    {
      goto L60;
    }
  e2 = *d1 + 1;
  i1 = e2;
  for (i3 = e1; i3 <= i1; ++i3)
    {
      w = p1[i3] * p2[1];
      w1 = p3[i3] + w;
      /*Computing MAX 
       */
      d__2 = (d__1 = p3[i3], Abs (d__1)), d__3 = Abs (w);
      if (Abs (w1) > eps * Max (d__2, d__3))
	{
	  p3[i3] = w1;
	}
      else
	{
	  p3[i3] = 0.;
	}
      /* L55: */
    }
  return 0;
 L60:
  e2 = *d2 + 1;
  i1 = e2;
  for (i3 = e1; i3 <= i1; ++i3)
    {
      w = p2[i3] * p1[1];
      w1 = p3[i3] + w;
      /*Computing MAX 
       */
      d__2 = (d__1 = p3[i3], Abs (d__1)), d__3 = Abs (w);
      if (Abs (w1) > eps * Max (d__2, d__3))
	{
	  p3[i3] = w1;
	}
      else
	{
	  p3[i3] = 0.;
	}
      /* L65: */
    }
  return 0;
 L100:
  p3[1] += p1[1] * p2[1];
  return 0;
}	
