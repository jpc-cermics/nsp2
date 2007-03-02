/* Nsp
 * Copyright (C) 2006 Bruno Pincon Esial/Iecn
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

/*    rejection ziggurat method for the normal distribution
 *    as explained in "Marsaglia G and WW Tsang,The Ziggurat 
 *    Method for Generating Random Variables
 *    Journal of Statistical Software, vol. 5 (2000), no. 8"
 *    (this paper together with a C implementation is available 
 *    at http://www.jstatsoft.org/v05/i08/)
 *
 *    Our implementation uses 128 rectangles and the current "base"
 *    generator of Nsp (which can be mt, kiss, clcg4, clcg2, fsultra
 *    and urand). The constants (x_k and y_k values) have been 
 *    calculated with the pari-gp software.
 */

#include "grand.h"
#include <math.h>

/* double rej_nor_A=1.01236236849665921; */
double rej_nor_coef=0.927158602609581818;
double rej_nor_x[128]={
    0.0                 ,0.272320864704663850,0.362871431028418304,0.426547986303305124,
    0.477437837253787876,0.520656038725144917,0.558692178375517971,0.592962942441977979,
    0.624358597309088221,0.653478638715042387,0.680747918645904216,0.706479611313608034,
    0.730911910621881281,0.754230664434510071,0.776583987876148386,0.798092060626274804,
    0.818853906683317723,0.838952214281207455,0.858456843178050863,0.877427429097715691,
    0.895915352566238529,0.913965251008801776,0.931616196601353810,0.948902625497911954,
    0.965855079388130595,0.982500803502760385,0.998864233480643513,1.01496739523929947,
    1.03083023605645559,1.04647090075258026,1.06190596368361940,1.07715062488193765,
    1.09221887689655376,1.10712364752353539,1.12187692257225406,1.13648985200307553,
    1.15097284213897606,1.16533563615504690,1.17958738465446070,1.19373670782377219,
    1.20779175040675760,1.22176023053096256,1.23564948325448117,1.24946649956433374,
    1.26321796144602823,1.27691027355169971,1.29054959191787315,1.30414185012042153,
    1.31769278320134299,1.33120794965767650,1.34469275174571304,1.35815245432242287,
    1.37159220241973227,1.38501703772514864,1.39843191412360635,1.41184171243976025,
    1.42525125450686157,1.43866531667746131,1.45208864288221649,1.46552595733579462,
    1.47898197698309785,1.49246142377461540,1.50596903685655026,1.51950958475937078,
    1.53308787766755607,1.54670877985350346,1.56037722235984068,1.57409821601674972,
    1.58787686488440070,1.60171838021527705,1.61562809503713296,1.62961147946467839,
    1.64367415685698264,1.65782192094820754,1.67206075409185220,1.68639684677348632,
    1.70083661856430094,1.71538674070811654,1.73005416055824353,1.74484612810837650,
    1.75977022489423187,1.77483439558076924,1.79004698259461898,1.80541676421404874,
    1.82095299659100507,1.83666546025338404,1.85256451172308706,1.86866114098954200,
    1.88496703570286923,1.90149465310031761,1.91825730085973203,1.93526922829190020,
    1.95254572954888890,1.97010326084971322,1.98795957412306072,2.00613386995896684,
    2.02464697337293387,2.04352153665066949,2.06278227450396335,2.08245623798772464,
    2.10257313518499888,2.12316570866978996,2.14427018235626135,2.16592679374484073,
    2.18818043207202060,2.21108140887472781,2.23468639558705698,2.25905957386532952,
    2.28427405967365680,2.31041368369500215,2.33757524133553073,2.36587137011398754,
    2.39543427800746734,2.42642064553021159,2.45901817740835009,2.49345452209195076,
    2.53000967238546661,2.56903362592163913,2.61097224842861320,2.65640641125819250,
    2.70611357311872233,2.76116937238415385,2.82312535054596643,2.89434400701867062,
    2.97869625264501696,3.08322885821421370,3.22308498457861854,3.44261985589665212
};
double rej_nor_y[128]={
    0.398942280401432678,0.384420658981682726,0.373522748141106316,0.364251715035986642,
    0.355968876531425490,0.348373573283578819,0.341295363394479423,0.334626244265964860,
    0.328292479472809649,0.322240957253635010,0.316431845843046832,0.310834316291433707,
    0.305423896178597626,0.300180751490354248,0.295088526356077135,0.290133533446065115,
    0.285304173202847215,0.280590507234007472,0.275983938443315368,0.271476966845668090,
    0.267063000179495015,0.262736204934735768,0.258491387684491830,0.254323899476874075,
    0.250229558010610547,0.246204583692175839,0.242245546648512680,0.238349322473737380,
    0.234513055003481010,0.230734124792334072,0.227010122256156323,0.223338824658038588,
    0.219718176282891875,0.216146271274101058,0.212621338705853685,0.209141729543510392,
    0.205705905206777190,0.202312427500227481,0.198959949715719394,0.195647208743592170,
    0.192373018055828097,0.189136261445882234,0.185935887427576882,0.182770904210084495,
    0.179640375178172028,0.176543414817017910,0.173479185029409926,0.170446891800283875,
    0.167445782169606159,0.164475141479729723,0.161534290867717281,0.158622584976854697,
    0.155739409864774326,0.152884181088357758,0.150056341947960011,0.147255361875550651,
    0.144480734953149976,0.141731978549490419,0.139008632064188530,0.136310255769899547,
    0.133636429743968693,0.130986752882010972,0.128360841986661826,0.125758330925458934,
    0.123178869852453437,0.120622124488717524,0.118087775457424043,0.115575517669630454,
    0.113085059757311182,0.110616123550555179,0.108168443596184749,0.105741766715362132,
    0.103335851598036258,0.100950468432347246,0.0985853985673542673,0.0962404342076864141,
    0.0939153781389394019,0.0916100434828561740,0.0893242534815396559,0.0870578413101539340,
    0.0848106499177789685,0.0825825318962967149,0.0803733493774065748,0.0781829739580991091,
    0.0760112866551630387,0.0738581778895664387,0.0717235475018441125,0.0696073047999458090,
    0.0675093686413617411,0.0654296675517518697,0.0633681398827745569,0.0613247340123518580,
    0.0592994085912393283,0.0572921328405081375,0.0553028869054218956,0.0533316622722318082,
    0.0513784622556619760,0.0494433025663632938,0.0475262119694455836,0.0456272330474388750,
    0.0437464230837977638,0.0418838550864944908,0.0400396189755420114,0.0382138229637097002,
    0.0364065951665963416,0.0346180854870940228,0.0328484678307869468,0.0310979427239285567,
    0.0293667404256784767,0.0276551246532096905,0.0259633970749860604,0.0242919027782570097,
    0.0226410369881801143,0.0210112534182023107,0.0194030747807804796,0.0178171062111101946,
    0.0162540526998516929,0.0147147421738863560,0.0132001567529497395,0.0117114762238032116,
    0.0102501404760272694,0.00881794274795907225,0.00741717586342264762,0.00605087645590561801,
    0.00472326861457955805,0.00344067147898109344,0.00221372880732917423,0.00106502791455805240
};

double rand_nor_core()
{
  int k, positif;
  double u, v, y;
  while (1)
    {
      k = 0xff & rand_lgi();  /* a random integer in [0,255] k = b_7 2^7 + ... b_0^2  */    
      positif = k & 0x80;     /* take b_7 as the sign of the future output number */
      k &= 0x7f;              /* make b_7 = 0 for k : k is a random integer in [0,127] */

      if ( k < 127 )
	{
	  u = rej_nor_x[k+1]*rand_ranf();
	  if ( u <= rej_nor_x[k] )
	    break;
	  else
	    {
	      y = (rej_nor_y[k]-rej_nor_y[k+1])*rand_ranf() + rej_nor_y[k+1];
	      if ( y <= rej_nor_y[0]*exp(-0.5*u*u) )
		break;
	    }
	}
      else  /* k = 127 ie the bottom area  (one rectangle plus a gaussian tail) */
	{
	  double r = rej_nor_x[127]; 
	  v = rand_ranf();
	  if ( v <= rej_nor_coef )  /* generate in the rectangle */
	    {
	      u = r*rand_ranf();
	      break;
	    }
	  else                      /* generate in the tail (see Knuth the Art of CP tome 2      *
                                     * 2 edition, p 123 steps M7 and M8 of alg M and exercise 11) */
	    {
	      do
		{
		  v = rand_ranf(); 
                  /* use 1-rand_ranf() because rand_ranf() could output 0 but never 1 */
		  u = sqrt( r*r - 2*log(1.0-rand_ranf()) );
		}
	      while ( u*v > r );
	      break;
	    }
	}
    }

  if ( positif )
    return u;
  else
    return -u;
}

double rand_nor(double mu, double sigma)
{
  return mu + sigma*rand_nor_core();
}
