#include <math.h>
#include "scicos/scicos.h"

extern int nsp_scalarexp_byte_eval(const int *code,int lcode,const double *constv,const double *vars, double *res);


#if WIN32
double asinh(double x) { return log(x+sqrt(x*x+1));}
double acosh(double x) { return log(x+sqrt(x*x-1));}
double atanh(double x) { 
  return  (x >=0.5) ? 0.5*log((1+x)/(1-x)) : 0.5*log((2*x)+(2*x)*x/(1-x));
}
#endif

#if WIN32
#define CHECK_VALUE(x) !_finite(x) || _isnan(x)
#else
#define CHECK_VALUE(x) isinf(x) || isnan(x)
#endif


void scicos_evaluate_expr_block_old(scicos_block *block,int flag)
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

static int nsp_scalarexp_byte_eval_scicos(const int *code,int lcode,const double *constv,const double *vars,
					  int phase,int flag, int block_ng, double *block_g, int *block_mode,
					  double *res);

void scicos_evaluate_expr_block(scicos_block *block,int flag)
{
  int i,phase;
  double *constv = block->rpar, vars[8],res;
  phase=get_phase_simulation();  
  if ( block->nin>1 ) 
    for ( i = 0 ; i < block->nin ; i++) vars[i]=block->inptr[i][0];
  else 
    for ( i = 0 ; i < block->insz[0] ; i++) vars[i]=block->inptr[0][i];
  nsp_scalarexp_byte_eval_scicos(block->ipar,block->nipar,constv,vars,phase,flag,block->ng,
				 block->g,block->mode,&res);
  block->outptr[0][0]=res;
}


typedef enum { 
  f_sin, f_cos, f_tan, f_exp, f_log, f_sinh, f_cosh, f_tanh,
  f_int, f_round, f_ceil, f_floor, f_sign, f_abs, f_max, f_min,
  f_asin, f_acos, f_atan, f_asinh, f_acosh, f_atanh,
  f_atan2, f_log10, f_gamma
} f_enum;


typedef struct _expr_func expr_func;

struct _expr_func {
  const char *name;
  f_enum id;
  double (*f1)(double);
  double (*f2)(double,double);
};



#if WIN32
double asinh(double x) { return log(x+sqrt(x*x+1));}
double acosh(double x) { return log(x+sqrt(x*x-1));}
double atanh(double x) { 
  return  (x >=0.5) ? 0.5*log((1+x)/(1-x)) : 0.5*log((2*x)+(2*x)*x/(1-x));
}
#endif

extern double round(double);
static double sign(double x) { return (x>0) ? 1: ((x==0) ? 0:-1);}
static double dmax(double x,double y) { return Max(x,y);}
static double dmin(double x,double y) { return Min(x,y);}
static double dabs(double x) { return Abs(x);}
static double dgamma(double x) {
#ifdef HAVE_TGAMMA
  return tgamma(x);
#else 
  return cdf_gamma(&x);
#endif 
}


static expr_func expr_functions[] = 
  {
    {"sin",f_sin,sin,NULL},
    {"cos",f_cos,cos,NULL},
    {"tan",f_tan,tan,NULL},
    {"exp",f_exp,exp,NULL},
    {"log",f_log,log,NULL},
    {"sinh",f_sinh,sinh,NULL},
    {"cosh",f_cosh,cosh,NULL},
    {"tanh",f_tanh,tanh,NULL},
    {"int",f_int,rint,NULL},
    {"round",f_round,round,NULL},
    {"ceil",f_ceil,ceil,NULL},
    {"floor",f_floor,floor,NULL},
    {"sign",f_sign,sign,NULL},
    {"abs",f_abs,dabs,NULL},
    {"max",f_max,NULL,dmax},
    {"min",f_min,NULL,dmin},
    {"asin",f_asin,asin,NULL},
    {"acos",f_acos,acos,NULL},
    {"atan",f_atan,atan,NULL},
    {"asinh",f_asinh,asinh,NULL},
    {"acosh",f_acosh,acosh,NULL},
    {"atanh",f_atanh,atanh,NULL},
    {"atan2",f_atan2,NULL,atan2},
    {"log10",f_log10,log10,NULL},
    {"gamma",f_gamma,dgamma,NULL},
    {NULL,0}
  };


static int nsp_scalarexp_byte_eval_scicos(const int *code,int lcode,const double *constv,const double *vars,
					  int phase,int flag, int block_ng, double *block_g, int *block_mode,
					  double *res)
{
  unsigned int type;
  int i,s_pos=0,n, nzcr=-1;
  double stack[512];
  for ( i = 0 ; i < lcode ; i++)
    {
      unsigned int bcode = *code;
      code++;
      type = (bcode & 0xefff0000 ) >> 16;
      switch (type )
	{
	case 1:
	  n = bcode & 0xffff;
	  /* Sciprintf("Need  an operator %d\n",n);*/
	  switch (n) 
	    {
	    case TILDE_OP: 
	      if (flag==9) {
		block_g[nzcr]=stack[s_pos-1];
		if(phase==1) {
		  block_mode[nzcr]=(0.0==stack[s_pos-1]);
		}
	      }
	      if(block_ng>0) nzcr=nzcr+1;
	      if(phase==1||block_ng==0){
		i=(stack[s_pos-1]==0.0);
	      }else{
		i=block_mode[nzcr];
	      }
	      if (i){
		stack[s_pos-1]=1.0;
	      }else{
		stack[s_pos-1]=0.0;   
	      }
	      break;
	    case DOTPRIM :
	    case QUOTE_OP : break;
	    case DOTSTARDOT:
	    case DOTSTAR :
	    case STAR_OP :  stack[s_pos-2] *= stack[s_pos-1];s_pos--; break;			      
	    case DOTPLUS: 
	    case PLUS_OP :  stack[s_pos-2] += stack[s_pos-1];s_pos--; break;			      
	    case HAT_OP :   stack[s_pos-2]= pow(stack[s_pos-2],stack[s_pos-1]);s_pos--; break;			      
	    case SEQOR : 	
	    case OR_OP :    
	      if(block_ng>0) nzcr=nzcr+1;
	      if (flag==9) {
		block_g[nzcr]=stack[s_pos-2]-stack[s_pos-1];
		if(phase==1) {
		  block_mode[nzcr]=((int)stack[s_pos-2]||(int)stack[s_pos-1]);
		}
	      }
	      if(phase==1||block_ng==0){
		i=((int)stack[s_pos-2]||(int)stack[s_pos-1]);
	      } else{
		i=block_mode[nzcr];
	      }
	      stack[s_pos-2]=(double)i;
	      s_pos--;
	      break;
	    case SEQAND   : 
	    case AND_OP : 
	      if(block_ng>0) nzcr=nzcr+1;
	      if (flag==9) {
		block_g[nzcr]=stack[s_pos-2]-stack[s_pos-1];
		if(phase==1) {
		  block_mode[nzcr]=((int)stack[s_pos-2]&&(int)stack[s_pos-1]);
		}
	      }
	      if(phase==1||block_ng==0){
		i=((int)stack[s_pos-2]&&(int)stack[s_pos-1]);
	      } else{
		i=block_mode[nzcr];
	      }
	      stack[s_pos-2]=(double)i;
	      s_pos--;
	      break;
	    case COMMA_OP : break;
	    case SEMICOLON_OP :break;
	    case RETURN_OP : break;
	    case MINUS_OP :  stack[s_pos-2] -= stack[s_pos-1];s_pos--;break; /* binary */
	    case DOTSLASH:
	    case DOTSLASHDOT:
	    case SLASH_OP : stack[s_pos-2] /= stack[s_pos-1];s_pos--;break;
	    case DOTBSLASH :
	    case DOTBSLASHDOT: 
	    case BACKSLASH_OP: stack[s_pos-2] = stack[s_pos-1]/stack[s_pos-2];s_pos--;break;
	    case DOTHAT : 	stack[s_pos-2]= pow(stack[s_pos-2],stack[s_pos-1]);s_pos--;break;
	    case DOTEQ :
	    case EQ     : 
	      if(block_ng>0) nzcr++;
	      if (flag==9) {
		block_g[nzcr]=stack[s_pos-2]-stack[s_pos-1];
		if(phase==1) {
		  block_mode[nzcr]=(stack[s_pos-2]== stack[s_pos-1]);
		}
	      }
	      if(phase==1||block_ng==0){
		i=(stack[s_pos-2]== stack[s_pos-1]);
	      } else{
		i=block_mode[nzcr];
	      }
	      stack[s_pos-2]=(double) i;
	      s_pos--;
	      break;
	    case DOTLEQ:
	    case LEQ    :
	      if(block_ng>0) nzcr=nzcr+1;
	      if (flag==9) {
		block_g[nzcr]=stack[s_pos-2]-stack[s_pos-1];
		if(phase==1) {
		  block_mode[nzcr]=(stack[s_pos-2]<=stack[s_pos-1]);
		}
	      }
	      if(phase==1||block_ng==0){
		i=(stack[s_pos-2]<=stack[s_pos-1]);
	      } else{
		i=block_mode[nzcr];
	      }
	      stack[s_pos-2]=(double)i;
	      s_pos--;
	      break;
	    case DOTGEQ :
	    case GEQ    : 
	      if(block_ng>0) nzcr=nzcr+1;
	      if (flag==9) {
		block_g[nzcr]=stack[s_pos-2]-stack[s_pos-1];
		if(phase==1) {
		  block_mode[nzcr]=(stack[s_pos-2]>=stack[s_pos-1]);
		}
	      }
	      if(phase==1||block_ng==0){
		i=(stack[s_pos-2]>=stack[s_pos-1]);
	      } else{
		i=block_mode[nzcr];
	      }
	      stack[s_pos-2]=(double)i;
	      s_pos--;
	      break;
	    case DOTNEQ :
	    case NEQ    :  
	      if(block_ng>0) nzcr=nzcr+1;
	      if (flag==9) {
		block_g[nzcr]=stack[s_pos-2]-stack[s_pos-1];
		if(phase==1) {
		  block_mode[nzcr]=(stack[s_pos-2]!=stack[s_pos-1]);
		}
	      }
	      if(phase==1||block_ng==0){
		i=(stack[s_pos-2]!=stack[s_pos-1]);
	      } else{
		i=block_mode[nzcr];
	      }
	      stack[s_pos-2]=(double)i;
	      s_pos--;
	      break;
	    case MOINS   : 	stack[s_pos-1] =-stack[s_pos-1] ;break;   /* unary minus */	      
	    case DOTLT :
	    case LT_OP:
	      if(block_ng>0) nzcr=nzcr+1;
	      if (flag==9) {
		block_g[nzcr]=stack[s_pos-2]-stack[s_pos-1];
		if(phase==1) {
		  block_mode[nzcr]=(stack[s_pos-2]<stack[s_pos-1]);
		}
	      }
	      if(phase==1||block_ng==0){
		i=(stack[s_pos-2]<stack[s_pos-1]);
	      } else{
		i=block_mode[nzcr];
	      }
	      stack[s_pos-2]=(double)i;
	      s_pos--;
	      break;
	    case DOTGT:
	    case GT_OP: 
	      if(block_ng>0) nzcr=nzcr+1;
	      if (flag==9) {
		block_g[nzcr]=stack[s_pos-2]-stack[s_pos-1];
		if(phase==1) {
		  block_mode[nzcr]=(stack[s_pos-2]>stack[s_pos-1]);
		}
	      }
	      if(phase==1||block_ng==0){
		i=(stack[s_pos-2]>stack[s_pos-1]);
	      } else{
		i=block_mode[nzcr];
	      }
	      stack[s_pos-2]=(double)i;
	      s_pos--;
	      break;
	  }
	  break;
	case 2:
	  n = bcode & 0xffff;
	  /* Sciprintf("Need  a function %d\n", n); */
	  if ( expr_functions[n].f1 != NULL) 
	    {
	      stack[s_pos-1] = (expr_functions[n].f1)(stack[s_pos-1]);
	    }
	  else 
	    {
	      stack[s_pos-2] = (expr_functions[n].f2)(stack[s_pos-2],stack[s_pos-1]);
	      s_pos--;
	    }
	  break;
	case 3:
	  /* Sciprintf("Need  a name %d\n", bcode & 0xffff); */
	  stack[s_pos]=vars[  bcode & 0xffff];
	  s_pos++;
	  break;
	case 4:
	  /* Sciprintf("A number %f\n",constv[ bcode & 0xffff]); */
	  stack[s_pos]=constv[ bcode & 0xffff]; 
	  s_pos++;
	  break;
	}
    }
  *res = stack[0];
  return OK;
}



