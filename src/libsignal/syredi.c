#include "signal.h"

/*!purpose 
 *    computes butterworth, chebyshev, and elliptic approximation; 
 *!calling sequence 
 *     subroutine syredi(maxdeg,ityp,iapro,om,adelp,adels,ndeg, 
 *   *         nb,fact,b2,b1,b0,c1,c0,zzr,zzi,zpr,zpi,ierr, 
 *   *          spr,spi,pren,pimn,zm,sm,rom,nzero,nze) 
 * 
 *    variables d entree 
 *       int maxdeg,ityp,iapro 
 *       double precision om(4),adelp,adels 
 *    variables de sortie 
 *       int ndeg,ierr 
 *       double precision bo(nb),b1(nb),b2(nb),c0(nb),c1(nb),fact 
 *       double precision zzr(maxdeg),zzi(maxdeg),spr(maxdeg),spi(maxdeg) 
 *       nb=(maxdeg+1)/2 
 *! 
 *    variables de travail 
 *        pren,primn,nzero,zm,sm,rom,nzm,nze 
 * 
 */


int
signal_syredi (int *maxdeg, int *ityp, int *iapro, double *om, double *adelp,
	       double *adels, int *ndeg, int *nb, double *fact, double *b2,
	       double *b1, double *b0, double *c1, double *c0, double *zzr,
	       double *zzi, double *zpr, double *zpi, int *ierr, double *spr,
	       double *spi, double *pren, double *pimn, double *zm,
	       double *sm, double *rom, int *nzero, int *nze)
{
  int zm_dim1, zm_offset, sm_dim1, sm_offset, i__1;

  /* Local variables */
  double adeg, edeg, vsnn, a;
  int i__, j;
  double dcap02, dcap04;
  int ndegn, nmaxi, norma;
  double dk;
  int nh;
  double adelta, vd, ack, ogc, acx;
  int nbn;
  double ugc, dks;
  int nzm[4];
  double vsn;

  /* Parameter adjustments */
  sm_dim1 = *maxdeg;
  sm_offset = sm_dim1 + 1;
  sm -= sm_offset;
  zm_dim1 = *maxdeg;
  zm_offset = zm_dim1 + 1;
  zm -= zm_offset;
  --om;
  --b2;
  --b1;
  --b0;
  --c1;
  --c0;
  --zzr;
  --zzi;
  --zpr;
  --zpi;
  --spr;
  --spi;
  --pren;
  --pimn;
  --rom;
  --nzero;
  --nze;

  /* Function Body */
  nmaxi = (*maxdeg + 5) / 2;
  *nb = (*maxdeg + 1) / 2;
  /* 
   */
  norma = 0;
  *ndeg = 0;
  edeg = 0.;
  acx = 0.;
  /* 
   */
  adeg = 0.;
  adelta = 0.;
  a = 0.;
  /* 
   */
  for (i__ = 1; i__ <= 4; ++i__)
    {
      rom[i__] = 0.;
      /* L10: */
    }
  i__1 = *maxdeg;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      zzr[i__] = 0.;
      zzi[i__] = 0.;
      zpr[i__] = 0.;
      zpi[i__] = 0.;
      spr[i__] = 0.;
      spi[i__] = 0.;
      pren[i__] = 0.;
      pimn[i__] = 0.;
      nzero[i__] = 0;
      /* L15: */
    }
  i__1 = *nb;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      b2[i__] = 0.;
      b1[i__] = 0.;
      b0[i__] = 0.;
      c1[i__] = 0.;
      c0[i__] = 0.;
      /* L20: */
    }
  i__1 = *maxdeg;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      for (j = 1; j <= 4; ++j)
	{
	  sm[i__ + j * sm_dim1] = 0.;
	  zm[i__ + j * zm_dim1] = 0.;
	  /* L21: */
	}
    }
  /* 
   */
  *ierr = 0;
  signal_desia (&nmaxi, maxdeg, ityp, iapro, &om[1], &norma, &edeg, ndeg,
		adelp, adels, &nbn, &nzero[1], nzm, &vsn, &a, &adelta, &adeg,
		&sm[sm_offset], &pren[1], &pimn[1], &ugc, &ogc, &ack,
		&zm[zm_offset], &zzr[1], &zzi[1], &rom[1], &b2[1], &b1[1],
		&b0[1], &dk, &dks, &dcap02, &dcap04, &vsnn, &ndegn, &nh, &vd,
		&nze[1], ierr);
  if (*ndeg << 1 > *maxdeg)
    {
      *ierr = -9;
      return 0;
    }
  if (*ndeg <= 0)
    {
      *ierr = -7;
      return 0;
    }
  signal_desib (&nmaxi, maxdeg, &vsnn, &ndegn, &nbn, ityp, iapro, &om[1], &nh,
		adelp, adels, &vd, &a, &adelta, &pren[1], &pimn[1], &ugc,
		&ogc, &ack, &dk, &dks, &dcap02, &dcap04, &acx, &spr[1],
		&spi[1], &zpr[1], &zpi[1], nb, fact, &c1[1], &c0[1],
		&sm[sm_offset], ierr, ndeg);
  /* 
   */
  return 0;
}				/* syredi_ */
