#include <math.h>
#include "scicos/scicos.h"

#if WIN32
/*
 * arcsinh z = log (z+sqrt(1+z2))
 */
double asinh(double x)
{
  return log(x+sqrt(x*x+1));
}

double acosh(double x)
{
  return log(x+sqrt(x*x-1));
}

/* atanh(x)
 *      For x>=0.5
 *                  1              2x                          x
 *	atanh(x) = --- * log(1 + -------) = 0.5 * log1p(2 * --------)
 *                  2             1 - x                      1 - x
 *	
 * 	For x<0.5
 *	atanh(x) = 0.5*log1p(2x+2x*x/(1-x))
 *
 */

double atanh(double x)
{
  if (x >=0.5) 
    {
      return 0.5*log((1+x)/(1-x));
    }
  else
    {
      return 0.5*log((2*x)+(2*x)*x/(1-x));
    }
  
}
#endif


#if WIN32
#define CHECK_VALUE(x) !_finite(x) || _isnan(x)
#else
#define CHECK_VALUE(x) isinf(x) || isnan(x)
#endif

void scicos_evaluate_expr_block(scicos_block *block,int flag)
{
  static double stack [1000];
  static int count,bottom,nzcr,i,phase; 
  int j;  
  if (flag==1||flag==9){
    phase=get_phase_simulation();
    bottom=-1;
    count=-1;
    nzcr=-1;
    while (count<block->nipar-1){
      count=count+1;
      switch (block->ipar[count]) {
      case 2:
	count=count+1;
	bottom=bottom+1;
	if(bottom>999){
	  set_block_error(-16);
	  return;
	}
	if (block->nin>1){
	  stack[bottom]=block->inptr[block->ipar[count]-1][0];
	}else{
	  j=block->ipar[count]-1;
	  if (j<block->insz[0]){
	    stack[bottom]=block->inptr[0][block->ipar[count]-1];
	  }else{
	    stack[bottom]=0.;
	  }
	}
	break;
      case 6:
	count=count+1;
	bottom=bottom+1;
	if(bottom>999){
	  set_block_error(-16);
	  return;
	}
	stack[bottom]=block->rpar[block->ipar[count]-1];
	break;
      case 5:
	count=count+1;
	switch (block->ipar[count]) {
	case 1:
	  stack[bottom-1]=stack[bottom-1]+stack[bottom];
	  bottom=bottom-1;
	  break;
	case 2:
	  stack[bottom-1]=stack[bottom-1]-stack[bottom];
	  bottom=bottom-1;
	  break;
	case 3:
	  stack[bottom-1]=stack[bottom-1]*stack[bottom];
	  bottom=bottom-1;
	  break;
	case 7:
	  stack[bottom-1]=stack[bottom-1]/stack[bottom];
	  bottom=bottom-1;
	  break;
	case 15:
	  stack[bottom-1]=pow(stack[bottom-1],stack[bottom]);
	  bottom=bottom-1;
	  break;
	case 16: /* case == */
	  if(block->ng>0) nzcr=nzcr+1;
	  if (flag==9) {
	    block->g[nzcr]=stack[bottom-1]-stack[bottom];
	    if(phase==1) {
	      block->mode[nzcr]=(stack[bottom-1]==stack[bottom]);
	    }
	  }
	  if(phase==1||block->ng==0){
	    i=(stack[bottom-1]==stack[bottom]);
	  } else{
	    i=block->mode[nzcr];
	  }
	  stack[bottom-1]=(double)i;
	  bottom=bottom-1;
	  break;

	case 17:
	  if(block->ng>0) nzcr=nzcr+1;
	  if (flag==9) {
	    block->g[nzcr]=stack[bottom-1]-stack[bottom];
	    if(phase==1) {
	      block->mode[nzcr]=(stack[bottom-1]<stack[bottom]);
	    }
	  }
	  if(phase==1||block->ng==0){
	    i=(stack[bottom-1]<stack[bottom]);
	  } else{
	    i=block->mode[nzcr];
	  }
	  stack[bottom-1]=(double)i;
	  bottom=bottom-1;
	  break;
	case 18:
	  if(block->ng>0) nzcr=nzcr+1;
	  if (flag==9) {
	    block->g[nzcr]=stack[bottom-1]-stack[bottom];
	    if(phase==1) {
	      block->mode[nzcr]=(stack[bottom-1]>stack[bottom]);
	    }
	  }
	  if(phase==1||block->ng==0){
	    i=(stack[bottom-1]>stack[bottom]);
	  } else{
	    i=block->mode[nzcr];
	  }
	  stack[bottom-1]=(double)i;
	  bottom=bottom-1;
	  break;
	case 19:
	  if(block->ng>0) nzcr=nzcr+1;
	  if (flag==9) {
	    block->g[nzcr]=stack[bottom-1]-stack[bottom];
	    if(phase==1) {
	      block->mode[nzcr]=(stack[bottom-1]<=stack[bottom]);
	    }
	  }
	  if(phase==1||block->ng==0){
	    i=(stack[bottom-1]<=stack[bottom]);
	  } else{
	    i=block->mode[nzcr];
	  }
	  stack[bottom-1]=(double)i;
	  bottom=bottom-1;
	  break;
	case 20:
	  if(block->ng>0) nzcr=nzcr+1;
	  if (flag==9) {
	    block->g[nzcr]=stack[bottom-1]-stack[bottom];
	    if(phase==1) {
	      block->mode[nzcr]=(stack[bottom-1]>=stack[bottom]);
	    }
	  }
	  if(phase==1||block->ng==0){
	    i=(stack[bottom-1]>=stack[bottom]);
	  } else{
	    i=block->mode[nzcr];
	  }
	  stack[bottom-1]=(double)i;
	  bottom=bottom-1;
	  break;
	case 21:
	  if(block->ng>0) nzcr=nzcr+1;
	  if (flag==9) {
	    block->g[nzcr]=stack[bottom-1]-stack[bottom];
	    if(phase==1) {
	      block->mode[nzcr]=(stack[bottom-1]!=stack[bottom]);
	    }
	  }
	  if(phase==1||block->ng==0){
	    i=(stack[bottom-1]!=stack[bottom]);
	  } else{
	    i=block->mode[nzcr];
	  }
	  stack[bottom-1]=(double)i;
	  bottom=bottom-1;
	  break;
	case 28:
	  if(block->ng>0) nzcr=nzcr+1;
	  if (flag==9) {
	    block->g[nzcr]=stack[bottom-1]-stack[bottom];
	    if(phase==1) {
	      block->mode[nzcr]=((int)stack[bottom-1]||(int)stack[bottom]);
	    }
	  }
	  if(phase==1||block->ng==0){
	    i=((int)stack[bottom-1]||(int)stack[bottom]);
	  } else{
	    i=block->mode[nzcr];
	  }
	  stack[bottom-1]=(double)i;
	  bottom=bottom-1;
	  break;
	case 29:
	  if(block->ng>0) nzcr=nzcr+1;
	  if (flag==9) {
	    block->g[nzcr]=stack[bottom-1]-stack[bottom];
	    if(phase==1) {
	      block->mode[nzcr]=((int)stack[bottom-1]&&(int)stack[bottom]);
	    }
	  }
	  if(phase==1||block->ng==0){
	    i=((int)stack[bottom-1]&&(int)stack[bottom]);
	  } else{
	    i=block->mode[nzcr];
	  }
	  stack[bottom-1]=(double)i;
	  bottom=bottom-1;
	  break;

	case 30:
	  if (flag==9) {
	    block->g[nzcr]=stack[bottom];
	    if(phase==1) {
	      block->mode[nzcr]=(0.0==stack[bottom]);
	    }
	  }
	  if(block->ng>0) nzcr=nzcr+1;
	  if(phase==1||block->ng==0){
	    i=(stack[bottom]==0.0);
	  }else{
	    i=block->mode[nzcr];
	  }
	  if (i){
	    stack[bottom]=1.0;
	  }else{
	    stack[bottom]=0.0;   
	  }
	  break;
	case 99:
	  stack[bottom]=-stack[bottom];
	  break;
	case 101:
	  stack[bottom]=sin(stack[bottom]);
	  break;
	case 102:
	  stack[bottom]=cos(stack[bottom]);
	  break;
	case 103:
	  stack[bottom]=tan(stack[bottom]);
	  break;
	case 104:
	  stack[bottom]=exp(stack[bottom]);
	  break;
	case 105:
	  stack[bottom]=log(stack[bottom]);
	  break;
	case 106:
	  stack[bottom]=sinh(stack[bottom]);
	  break;
	case 107:
	  stack[bottom]=cosh(stack[bottom]);
	  break;
	case 108:
	  stack[bottom]=tanh(stack[bottom]);
	  break;
	case 109:
	  if(block->ng>0) nzcr=nzcr+1;
	  if (flag==9) {
	    if (stack[bottom]>0) {
	      i=(int)floor(stack[bottom]);
	    }else{
	      i=(int)ceil(stack[bottom]);
	    }
	    if(i==0)  {
	      block->g[nzcr]=(stack[bottom]-1)*(stack[bottom]+1);
	    }else if(i>0){
	      block->g[nzcr]=(stack[bottom]-i-1.)*(stack[bottom]-i);
	    }else{
	      block->g[nzcr]=(stack[bottom]-i)*(stack[bottom]-i+1);
	    }
	    if(i%2)  block->g[nzcr]=-block->g[nzcr];
	    if(phase==1) block->mode[nzcr]=i;
	  }
	  if(phase==1||block->ng==0){
	    if (stack[bottom]>0) {
	      stack[bottom]=floor(stack[bottom]);
	    }else{
	      stack[bottom]=ceil(stack[bottom]);
	    }
	  }else{
	    stack[bottom]=(double) block->mode[nzcr];
	  }
	  break;
	case 110:
	  if(block->ng>0) nzcr=nzcr+1;
	  if (flag==9) {
	    if (stack[bottom]>0) {
	      i=(int)floor(stack[bottom]+.5);
	    }else{
	      i=(int)ceil(stack[bottom]-.5);
	    }
	    block->g[nzcr]=(stack[bottom]-i-.5)*(stack[bottom]-i+.5);
	    if(i%2)  block->g[nzcr]=-block->g[nzcr];
	    if(phase==1) block->mode[nzcr]=i;
	  }
	  if(phase==1||block->ng==0){
	    if (stack[bottom]>0) {
	      stack[bottom]=floor(stack[bottom]+.5);
	    }else{
	      stack[bottom]=ceil(stack[bottom]-.5);
	    }
	  }else{
	    stack[bottom]=(double) block->mode[nzcr];
	  }
	  break;
	case 111:
	  if(block->ng>0) nzcr=nzcr+1;
	  if (flag==9) {
	    i=(int)ceil(stack[bottom]);
	    block->g[nzcr]=(stack[bottom]-i)*(stack[bottom]-i+1);
	    if(i%2)  block->g[nzcr]=-block->g[nzcr];
	    if(phase==1) block->mode[nzcr]=i;
	  }
	  if(phase==1||block->ng==0){
	    stack[bottom]=ceil(stack[bottom]);
	  }else{
	    stack[bottom]=(double) block->mode[nzcr];
	  }
	  break;
	case 112:
	  if(block->ng>0) nzcr=nzcr+1;
	  if (flag==9) {
	    i=(int)floor(stack[bottom]);
	    block->g[nzcr]=(stack[bottom]-i-1)*(stack[bottom]-i);
	    if(i%2)  block->g[nzcr]=-block->g[nzcr];
	    if(phase==1) block->mode[nzcr]=i;
	  }
	  if(phase==1||block->ng==0){
	    stack[bottom]=floor(stack[bottom]);
	  }else{
	    stack[bottom]=(double) block->mode[nzcr];
	  }
	  break;
	case 113:
	  if(block->ng>0) nzcr=nzcr+1;
	  if (flag==9) 
	    {
	      i = (stack[bottom]>0) ? 1 : ( (stack[bottom]<0) ? -1 :0);
	      block->g[nzcr]=stack[bottom];
	      if(phase==1) block->mode[nzcr]=i;
	    }
	  if(phase==1||block->ng==0)
	    {
	      stack[bottom] = (stack[bottom]>0) ? 1.0 : ( (stack[bottom]<0) ? -1 :0);
	    }
	  else
	    {
	      stack[bottom]=(double) block->mode[nzcr];
	    }
	  break;
	case 114:  /* abs */
	  if(block->ng>0) nzcr=nzcr+1;
	  if (flag==9) 
	    {
	      i = (stack[bottom]>0) ? 1 : ( (stack[bottom]<0) ? -1 :0);
	      block->g[nzcr]=stack[bottom];
	      if(phase==1) block->mode[nzcr]=i;
	    }
	  if(phase==1||block->ng==0){
	    if (stack[bottom]>0) {
	      stack[bottom]=stack[bottom];
	    }else {
	      stack[bottom]=-stack[bottom];
	    }
	  }else{
	    stack[bottom]=stack[bottom]*(block->mode[nzcr]);
	  }
	  break;
	  /* if (stack[bottom]>0) {
	     stack[bottom]=stack[bottom];
	     }else {
	     stack[bottom]=-stack[bottom];
	     }*/
	case 115:
	  if(block->ng>0) nzcr=nzcr+1;
	  if (flag==9) {
	    if (stack[bottom]>stack[bottom-1]) {
	      i=0;
	    }else {
	      i=1;
	    }
	    block->g[nzcr]=stack[bottom]-stack[bottom-1];
	    if(phase==1) block->mode[nzcr]=i;
	  }
	  if(phase==1||block->ng==0){
	    stack[bottom-1]=Max(stack[bottom-1],stack[bottom]);
	  }else{
	    stack[bottom-1]=stack[bottom-block->mode[nzcr]];
	  }
	  bottom=bottom-1;
	  break;
	case 116:
	  if(block->ng>0) nzcr=nzcr+1;
	  if (flag==9) {
	    if (stack[bottom]<stack[bottom-1]) {
	      i=0;
	    }else {
	      i=1;
	    }
	    block->g[nzcr]=stack[bottom]-stack[bottom-1];
	    if(phase==1) block->mode[nzcr]=i;
	  }
	  if(phase==1||block->ng==0){
	    stack[bottom-1]=Min(stack[bottom-1],stack[bottom]);
	  }else{
	    stack[bottom-1]=stack[bottom-block->mode[nzcr]];
	  }
	  bottom=bottom-1;
	  break;
	case 117:
	  stack[bottom]=asin(stack[bottom]);
	  break;
	case 118:
	  stack[bottom]=acos(stack[bottom]);
	  break;
	case 119:
	  stack[bottom]=atan(stack[bottom]);
	  break;
	case 120:
	  stack[bottom]=asinh(stack[bottom]);
	  break;
	case 121:
	  stack[bottom]=acosh(stack[bottom]);
	  break;
	case 122:
	  stack[bottom]=atanh(stack[bottom]);
	  break;
	case 123:
	  stack[bottom-1]=atan2(stack[bottom-1],stack[bottom]);
	  bottom=bottom-1;
	  break;

	case 124:
	  stack[bottom]=log10(stack[bottom]);
	  break;
	}
      }
    }
    if ( CHECK_VALUE(stack[bottom])) 
      {
	set_block_error(-2);
	return;
      } 
    else
      {
	block->outptr[0][0]=stack[bottom];
      }
  }
}


#define EVAL(OP) stack[bottom-1]=stack[bottom-1] OP stack[bottom];bottom=bottom-1
#define UEVAL(OP) stack[bottom]=OP/**/(stack[bottom-1]);

int scicos_evaluate_expr(const int *ipar,int nipar,const double *rpar,double *res)
{
  double stack [1000];
  int count,bottom;
  bottom=-1; /* position in stack */
  count=-1;  /* position of arguments in ipar */
  /* loop on ipar which contains bycode to be executed */
  while ( count < nipar) 
    {
      count=count+1;
      switch (ipar[count]) 
	{
	case 6:
	  /* [6,xx] : push rpar[ipar[xx]-1] on the stack */
	  count=count+1;
	  bottom=bottom+1;
	  if( bottom > 999 ) {
	    Scierror("Error: stack overflow in evaluate_expr\n");
	    return FAIL;
	  }
	  stack[bottom]=rpar[ipar[count]-1];
	  break;
	case 5:
	  /* [5,op] : execute op */
	  count=count+1;
	  switch (ipar[count]) 
	    {
	    case 1: /* + */  EVAL(+);break;
	    case 2: /* - */  EVAL(-);break;
	    case 3: /* mult */ EVAL(*);break;
	    case 7: /* div */ EVAL(/);break;
	    case 15: /* ^ */ 
	      stack[bottom-1]=pow(stack[bottom-1],stack[bottom]);
	      bottom=bottom-1;
	      break;
	    case 16: /* == */ EVAL(==);break;
	    case 17: /* < */  EVAL(<);break;
	    case 18: /* > */  EVAL(>);break;
	    case 19: /* <= */ EVAL(<=);break;
	    case 20: /* >= */ EVAL(>=);break;
	    case 21: /* != */ EVAL(!=);break;
	    case 28: /* || */ EVAL(||);break;
	    case 29: /* && */ EVAL(&&);break;
	    case 30: /* == 0.0 */
	      stack[bottom]= (stack[bottom]==0.0) ? 0.0 : 1.0;
	      break;
	    case 99: /* -x */
	      stack[bottom]=-stack[bottom];
	      break;
	    case 101: /* sin */ UEVAL(sin);break;
	    case 102: /* cos */ UEVAL(cos);break;
	    case 103: /* tan */ UEVAL(tan);break;
	    case 104: /* exp */ UEVAL(exp);break;
	    case 105: /* log */ UEVAL(log);break;
	    case 106: /* sinh */UEVAL(sinh);break;
	    case 107: /* cosh */UEVAL(cosh);break;
	    case 108: /* tanh */UEVAL(tanh);break;
	    case 109: /* round toward 0 */
	      stack[bottom]=(stack[bottom]>0) ? floor(stack[bottom]) : ceil(stack[bottom]);
		break;
	    case 110: /* ? */
	      stack[bottom]=(stack[bottom]>0) ? floor(stack[bottom]+.5)
		: ceil(stack[bottom]-.5);
		break;
	    case 111: /* ceil */ UEVAL(ceil);break;
	    case 112: /* floor */ UEVAL(floor);break;
	    case 113: /* sign */ 
	      stack[bottom]= (stack[bottom]>0) ? 1.0 : ((stack[bottom]<0) ? -1.0 : 0.0);
	      break;
	    case 114:  /* abs */ 
	      stack[bottom]= Abs(stack[bottom]);
	      break;
	    case 115: /* Max */
	      stack[bottom-1]=Max(stack[bottom-1],stack[bottom]);
	      break;
	    case 116: /* Min */
	      stack[bottom-1]=Min(stack[bottom-1],stack[bottom]);
	      break;
	    case 117: /* asin */ UEVAL(asin);break;
	    case 118: /* acos */ UEVAL(acos);break;
	    case 119: /* */ UEVAL(atan);break;
	    case 120: /* */ UEVAL(asinh);break;
	    case 121: /* */ UEVAL(acosh);break;
	    case 122: /* */ UEVAL(atanh);break;
	    case 123: /* */ stack[bottom-1]=atan2(stack[bottom-1],stack[bottom]);
	      bottom=bottom-1;
	      break;
	    case 124: stack[bottom]=log10(stack[bottom]); break;
	    }
	}
    }
  *res = stack[bottom];
  return OK;
}

