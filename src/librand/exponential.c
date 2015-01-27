/* Nsp
 * Copyright (C) 2006-2015 Bruno Pincon Esial/Iecn
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

#include "grand.h"
#include <math.h>

const double rej_exp_coef=0.873390718750090708;
const double rej_exp_x[128]={
  0.0                 ,0.0913395181029036071,0.150952685936872430,0.198733655529202445,
  0.240088052723822281,0.277325069275912096,0.311666667258660008,0.343847188566281822,
  0.374346987428728805,0.403499951515524141,0.431549022036835207,0.458677399489966205,
  0.485027265694870614,0.510711613728867582,0.535822045250432114,0.560434093304459448,
  0.584610965138341206,0.608406241612121715,0.631865867316897729,0.655029646251399642,
  0.677932385146246501,0.700604780753902613,0.723074117839883242,0.745364825026504550,
  0.767498922393608281,0.789496385605450544,0.811375444921901830,0.833152832880709453,
  0.854843991130223948,0.876463244467077211,0.898023948333463225,0.919538614677599724,
  0.941019020056089786,0.962476299071922503,0.983921025635170593,1.00536328406062329,
  1.02681273164522130,1.04827865407435316,1.06977001477206406,1.09129549912268653,
  1.11286355434023982,1.13448242563946729,1.15616018926267901,1.17790478283506834,
  1.19972403345431924,1.22162568386532860,1.24361741702554262,1.26570687932901418,
  1.28790170272645015,1.31020952595313799,1.33263801505584101,1.35519488339283119,
  1.37788791126762625,1.40072496534626713,1.42371401799975957,1.44686316670733054,
  1.47018065365220569,1.49367488563953696,1.51735445446579093,1.54122815787027719,
  1.56530502120252308,1.58959431994388958,1.61410560322821445,1.63884871851443631,
  1.66383383757420835,1.68907148396960046,1.71457256221030420,1.74034838879653090,
  1.76641072537331659,1.79277181424456505,1.81944441652128624,1.84644185320861772,
  1.87377804957094017,1.90146758315441729,1.92952573589244104,1.95796855077274116,
  1.98681289360651365,2.01607652051125982,2.04577815180181464,2.07593755308033479,
  2.10657562442829500,2.13771449873480881,2.16937765034949677,2.20159001542912381,
  2.23437812556076815,2.26777025649706999,2.30179659413941382,2.33648942026299161,
  2.37188332090636754,2.40801542086344608,2.44492564833780920,2.48265703457377848,
  2.52125605419779407,2.56077301312943841,2.60126249230735978,2.64278385719113861,
  2.68540184513682578,2.72918724542131485,2.77421769006790018,2.82057857791505357,
  2.86836415985974299,2.91767882028667449,2.96863859890389152,3.02137300929172274,
  3.07602722648897780,3.13276473738663276,3.19177057673762192,3.25325531138591809,
  3.31745999056962327,3.38466235796156261,3.45518473239647667,3.52940412612390563,
  3.60776540937241985,3.69079869307661819,3.77914266375610148,3.87357649759723393,
  3.97506444137807530,4.08481961680575458,4.20439793932874775,4.33584099408585739,
  4.48190207226268505,4.64642113286248252,4.83498436732483978,5.05617414879851055,
  5.32418237313749938,5.66507246608573970,6.13519284450123847,6.89831511661564260
};
const double rej_exp_y[128]={
  1.00000000000000000,0.912707777475125220,0.859888382509951496,0.819768204987318030,
  0.786558599390136387,0.757808116931513491,0.732225562307741979,0.709037268773050106,
  0.687738233900834131,0.667978059185554816,0.649502221835903891,0.632119133965388683,
  0.615680409581243590,0.600068409921908627,0.585188041317763257,0.570961159629425318,
  0.557322637577984288,0.544217529519877486,0.531598981894555764,0.519426663305339768,
  0.507665564832616016,0.496285069354161380,0.485258219764560839,0.474561136575439366,
  0.464172549299398272,0.454073415617565183,0.444246609064008530,0.434676660760543907,
  0.425349544207953113,0.416252494685469697,0.407373856699979369,0.398702954344944512,
  0.390229980505289026,0.381945901669018712,0.373842375743849699,0.365911680774216508,
  0.358146652844753362,0.350540631765755069,0.343087413382874571,0.335781207551398749,
  0.328616600975455652,0.321588524242526744,0.314692222489879234,0.307923229226805006,
  0.301277342908601913,0.294750605917999761,0.288339285659536601,0.282039857514071897,
  0.275848989435650697,0.269763528002485489,0.263780485758850116,0.257897029705952457,
  0.252110470817995964,0.246418254475166333,0.240817951718608916,0.235307251243941083,
  0.229883952059758183,0.224545956746179857,0.219291265255943587,0.214117969207050231,
  0.209024246621641177,0.204008357070755682,0.199068637188979041,0.194203496526831185,
  0.189411413712132522,0.184690932894579989,0.180040660450423427,0.175459261926495088,
  0.170945459204951709,0.166498027871972372,0.162115794775344983,0.157797635757394773,
  0.153542473551081593,0.149349275828338198,0.145217053390856313,0.141144858494566106,
  0.137131783300011535,0.133176958441711242,0.129279551710423646,0.125438766843016345,
  0.121653842415384012,0.117924050834575448,0.114248697426989191,0.110627119620187871,
  0.107058686216574762,0.103542796757882679,0.100078880980157546,0.0966663983596901123,
  0.0933048377511747320,0.0899937171202717472,0.0867325833737407979,0.0835210122914215960,
  0.0803586085655967753,0.0772450059547157498,0.0741798675601352348,0.0711628862364988129,
  0.0681937851487071493,0.0652723184912141664,0.0623982723887403761,0.0595714660015745097,
  0.0567917528636362068,0.0540590224876553146,0.0513732022795311461,0.0487342598136280059,
  0.0461422055330635747,0.0435970959548049800,0.0410990374797804057,0.0386481909348909370,
  0.0362447770091107953,0.0338890827931726876,0.0315814696966007573,0.0293223831044654659,
  0.0271123642604352345,0.0249520650399683837,0.0228422665356618877,0.0207839027613826766,
  0.0187780913696082754,0.0168261742012833758,0.0149297719923082533,0.0130908601063721507,
  0.0113118766723302260,0.00959588294047461015,0.00794681255393166100,0.00636988317909037829,
  0.00487233313651831522,0.00346489667612250272,0.00216530760995327540,0.00100948486124341257
};

/**
 * nsp_rand_exp_core:
 *
 * generates a random number from E(1), the exponential distribution
 * of parameter 1.
 *
 * Method: rejection ziggurat method for the exponential distribution
 * as explained in "Marsaglia G and WW Tsang,The Ziggurat 
 * Method for Generating Random Variables
 * Journal of Statistical Software, vol. 5 (2000), no. 8"
 * (this paper together with a C implementation is available 
 *  at http://www.jstatsoft.org/v05/i08/)
 *
 * Our implementation uses 128 rectangles and the current "base"
 * generator of Nsp (mt, kiss, clcg4, clcg2, fsultra, ...)
 * The constants (x_k and y_k values) have been calculated with 
 * the pari-gp software.
 *
 * Returns: a double
 */
double nsp_rand_exp_core()
{
  int k;
  double u, y;
  while (1)
    {
      k = 0x7f & rand_lgi();
      if ( k < 127 )
	{
	  u = rej_exp_x[k+1]*rand_ranf();
	  if ( u <= rej_exp_x[k] )
	    return u;
	  else
	    {
	      y = (rej_exp_y[k]-rej_exp_y[k+1])*rand_ranf() + rej_exp_y[k+1];
	      if ( y <= exp(-u) )
		return u;
	    }
	}
      else  /* k = 127 ie the bottom area (one rectangle plus an exponential tail) */
	{
	  u = rand_ranf();
	  if ( u <= rej_exp_coef )  /* generate in the rectangle */
	    return rej_exp_x[127]*rand_ranf(); 
	  else                      /* generate in the tail */
	    return rej_exp_x[127] - log(1.0-rand_ranf());  /* using 1-rand_ranf() because rand_ranf() 
                                                              could output 0 but never 1 */

	}
    }
}

/**
 * nsp_rand_exp:
 * @tau: parameter of the exponential distribution.
 *
 * generates a random number from E(tau), the exponential distribution
 * of parameter tau.
 *
 * Returns: a double
 */
double nsp_rand_exp(double tau)
{
  return tau*nsp_rand_exp_core();
}
