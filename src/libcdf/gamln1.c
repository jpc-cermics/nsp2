#include "cdf.h"



/**
 * cdf_gamln1:
 * @a: 
 * 
 * computes LN(GAMMA(1 + A)) for -0.2 <= A <= 1.25 
 * 
 * Returns: a double 
 **/

double cdf_gamln1 (double a)
{
  /* a= -0.2:0.01:1.25; norm(log(gamma(1+a))- cdf_gamln1(a)) */
  const double p0 = .577215664901533;
  const double q3 = 1.56875193295039;
  const double q4 = .361951990101499;
  const double q5 = .0325038868253937;
  const double q6 = 6.67465618796164e-4;
  const double r0 = .422784335098467;
  const double r1 = .848044614534529;
  const double r2 = .565221050691933;
  const double r3 = .156513060486551;
  const double r4 = .017050248402265;
  const double r5 = 4.97958207639485e-4;
  const double p1 = .844203922187225;
  const double s1 = 1.24313399877507;
  const double s2 = .548042109832463;
  const double s3 = .10155218743983;
  const double s4 = .00713309612391;
  const double s5 = 1.16165475989616e-4;
  const double p2 = -.168860593646662;
  const double p3 = -.780427615533591;
  const double p4 = -.402055799310489;
  const double p5 = -.0673562214325671;
  const double p6 = -.00271935708322958;
  const double q1 = 2.88743195473681;
  const double q2 = 3.12755088914843;
  double w, x;
  if (a >= .6)
    {
      x = (a) - .5 - .5;
      w =  (((((r5*x + r4)*x + r3)*x + r2)*x + r1)*x + r0) 
	/ (((((s5*x + s4)*x + s3)*x + s2)*x + s1)*x + 1.);
      return  x*w;
    }
  else 
    {
      w = ((((((p6*(a) + p5)*(a) + p4)*(a) + p3)*(a) + p2)*(a) + p1)*(a) + p0) 
	/ ((((((q6*(a) + q5)*(a) + q4)*(a) + q3)*(a) + q2)*(a) +  q1)*(a) + 1.);
      return  -((a)) * w;
    }
}

