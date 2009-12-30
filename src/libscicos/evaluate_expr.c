/* Nsp
 * Copyright (C) 2007-2009 Ramine Nikoukhah (Inria) 
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
 * Scicos blocks copyrighted GPL in this version by Ramine Nikoukhah
 * adapted to nsp by Jean-Philippe Chancelier 2006-2007
 * 
 *--------------------------------------------------------------------------*/



#include <math.h>
#include "scicos/scicos.h"
#include "scicos/blocks.h"

static int nsp_scalarexp_byte_eval_scicos(const int *code,int lcode,const double *constv,const double *vars,
					  int phase,int flag, int block_ng, double *block_g, int *block_mode,
					  double *res);

/* evaluate the byte code of a scicos EXPRESSION block 
 * the treatment is special in scicos since we have to 
 * deal with zero crossing. 
 * Counting the zero crossing contained in an expression 
 * is done in the byte compiler in scalexp.c 
 */

void scicos_evaluate_expr_block(scicos_block *block,int flag)
{
  int i,phase;
  double *constv = block->rpar, vars[8]={0},res=0.0;
  if ( flag==1 || flag==9) 
    {
      phase=scicos_get_phase_simulation();  
      if ( block->nin>1 ) 
	for ( i = 0 ; i < block->nin ; i++) vars[i]=block->inptr[i][0];
      else 
	for ( i = 0 ; i < block->insz[0] ; i++) vars[i]=block->inptr[0][i];
      nsp_scalarexp_byte_eval_scicos(block->ipar,block->nipar,constv,vars,phase,flag,block->ng,
				     block->g,block->mode,&res);
      
      if ( isinf(res) || ISNAN(res) ) 
	{
	  scicos_set_block_error(-2);
	  return;
	} 
      else
	{
	  block->outptr[0][0]=res; 
	}
    }
}

#include "nsp/scalexp.h" 

#define SCICOS_OP_EVAL_BINARY(exp)					\
  if(block_ng>0) nzcr=nzcr+1;						\
  if (flag==9) {							\
    block_g[nzcr]=stack[s_pos-2]-stack[s_pos-1];			\
    if(phase==1) {							\
      block_mode[nzcr]=(exp);						\
    }									\
  }									\
  stack[s_pos-2]=(double)((phase==1||block_ng==0) ? (exp) : block_mode[nzcr]); \
  s_pos--;



#define BEVAL_STACK_SIZE 512

static int nsp_scalarexp_byte_eval_scicos(const int *code,int lcode,const double *constv,const double *vars,
					  int phase,int flag, int block_ng, double *block_g, int *block_mode,
					  double *res)
{
  unsigned int type;
  int i,s_pos=0,n, nzcr=-1,ok;
  double stack[STACK_SIZE];
  for ( i = 0 ; i < lcode ; i++)
    {
      unsigned int bcode = *code;
      code++;
      type = (bcode & 0xefff0000 ) >> 16;
      switch (type )
	{
	case 1:
	  /*  we must evaluate an operator */
	  n = bcode & 0xffff;
	  /* Sciprintf("Need  an operator %d\n",n);*/
	  switch (n) 
	    {
	    case TILDE_OP: 
	      if(block_ng>0) nzcr=nzcr+1; /* XXX a confirmer par ramine  */
	      if (flag==9) {
		block_g[nzcr]=stack[s_pos-1];
		if(phase==1) {
		  block_mode[nzcr]=(0.0==stack[s_pos-1]);
		}
	      }
	      stack[s_pos-1]=((phase==1||block_ng==0) ? (stack[s_pos-1]==0.0) : block_mode[nzcr]);
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
	    case OR_OP :   SCICOS_OP_EVAL_BINARY(((int)stack[s_pos-2]||(int)stack[s_pos-1])); break;
	    case SEQAND   : 
	    case AND_OP : SCICOS_OP_EVAL_BINARY(((int)stack[s_pos-2] && (int)stack[s_pos-1])); break;
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
	    case EQ     :  SCICOS_OP_EVAL_BINARY((stack[s_pos-2] == stack[s_pos-1])); break;
	    case DOTLEQ:
	    case LEQ    : SCICOS_OP_EVAL_BINARY((stack[s_pos-2] <= stack[s_pos-1])); break;
	    case DOTGEQ :
	    case GEQ    :  SCICOS_OP_EVAL_BINARY((stack[s_pos-2] >= stack[s_pos-1])); break;
	    case DOTNEQ :
	    case NEQ    :  SCICOS_OP_EVAL_BINARY((stack[s_pos-2] != stack[s_pos-1])); break;
	    case MOINS   : 	stack[s_pos-1] =-stack[s_pos-1] ;break;   /* unary minus */	      
	    case DOTLT :
	    case LT_OP:   SCICOS_OP_EVAL_BINARY((stack[s_pos-2] < stack[s_pos-1])); break;
	    case DOTGT:
	    case GT_OP:  SCICOS_OP_EVAL_BINARY((stack[s_pos-2] > stack[s_pos-1])); break;
	    }
	  break;
	case 2:  /*  we must evaluate a function */
	  n = bcode & 0xffff;
	  ok = TRUE;
	  /* first check the special cases */
	  switch ( expr_functions[n].id ) 
	    {
	    case f_int :  /* int */
	      if(block_ng>0) nzcr=nzcr+1;
	      if (flag==9) {
		if (stack[s_pos-1]>0) {
		  i=(int)floor(stack[s_pos-1]);
		}else{
		  i=(int)ceil(stack[s_pos-1]);
		}
		if(i==0)  {
		  block_g[nzcr]=(stack[s_pos-1]-1)*(stack[s_pos-1]+1);
		}else if(i>0){
		  block_g[nzcr]=(stack[s_pos-1]-i-1.)*(stack[s_pos-1]-i);
		}else{
		  block_g[nzcr]=(stack[s_pos-1]-i)*(stack[s_pos-1]-i+1);
		}
		if(i%2)  block_g[nzcr]=-block_g[nzcr];
		if(phase==1) block_mode[nzcr]=i;
	      }
	      if(phase==1||block_ng==0){
		if (stack[s_pos-1]>0) {
		  stack[s_pos-1]=floor(stack[s_pos-1]);
		}else{
		  stack[s_pos-1]=ceil(stack[s_pos-1]);
		}
	      }else{
		stack[s_pos-1]=(double) block_mode[nzcr];
	      }
	      break;
	    case f_round:
	      if(block_ng>0) nzcr=nzcr+1;
	      if (flag==9) {
		if (stack[s_pos-1]>0) {
		  i=(int)floor(stack[s_pos-1]+.5);
		}else{
		  i=(int)ceil(stack[s_pos-1]-.5);
		}
		block_g[nzcr]=(stack[s_pos-1]-i-.5)*(stack[s_pos-1]-i+.5);
		if(i%2)  block_g[nzcr]=-block_g[nzcr];
		if(phase==1) block_mode[nzcr]=i;
	      }
	      if(phase==1||block_ng==0){
		if (stack[s_pos-1]>0) {
		  stack[s_pos-1]=floor(stack[s_pos-1]+.5);
		}else{
		  stack[s_pos-1]=ceil(stack[s_pos-1]-.5);
		}
	      }else{
		stack[s_pos-1]=(double) block_mode[nzcr];
	      }
	      break;
	    case f_ceil:
	      if(block_ng>0) nzcr=nzcr+1;
	      if (flag==9) {
		i=(int)ceil(stack[s_pos-1]);
		block_g[nzcr]=(stack[s_pos-1]-i)*(stack[s_pos-1]-i+1);
		if(i%2)  block_g[nzcr]=-block_g[nzcr];
		if(phase==1) block_mode[nzcr]=i;
	      }
	      if(phase==1||block_ng==0){
		stack[s_pos-1]=ceil(stack[s_pos-1]);
	      }else{
		stack[s_pos-1]=(double) block_mode[nzcr];
	      }
	      break;
	    case f_floor:
	      if(block_ng>0) nzcr=nzcr+1;
	      if (flag==9) {
		i=(int)floor(stack[s_pos-1]);
		block_g[nzcr]=(stack[s_pos-1]-i-1)*(stack[s_pos-1]-i);
		if(i%2)  block_g[nzcr]=-block_g[nzcr];
		if(phase==1) block_mode[nzcr]=i;
	      }
	      if(phase==1||block_ng==0){
		stack[s_pos-1]=floor(stack[s_pos-1]);
	      }else{
		stack[s_pos-1]=(double) block_mode[nzcr];
	      }
	      break;
	    case f_sign: 
	      if(block_ng>0) nzcr=nzcr+1;
	      if (flag==9) {
		if (stack[s_pos-1]>0) {
		  i=1;
		}else if (stack[s_pos-1]<0){
		  i=-1;
		}else{
		  i=0;
		}
		block_g[nzcr]=stack[s_pos-1];
		if(phase==1) block_mode[nzcr]=i;
	      }
	      if(phase==1||block_ng==0){
		if (stack[s_pos-1]>0) {
		  stack[s_pos-1]=1.0;
		}else if(stack[s_pos-1]<0){
		  stack[s_pos-1]=-1.0;
		}else{
		  stack[s_pos-1]=0.0;
		}
	      }else{
		stack[s_pos-1]=(double) block_mode[nzcr];
	      }
	      break;
	    case f_abs:
	      if(block_ng>0) nzcr=nzcr+1;
	      if (flag==9) {
		if (stack[s_pos-1]>0) {
		  i=1;
		}else if (stack[s_pos-1]<0){
		  i=-1;
		}else{
		  i=0;
		}
		block_g[nzcr]=stack[s_pos-1];
		if(phase==1) block_mode[nzcr]=i;
	      }
	      if(phase==1||block_ng==0){
		if (stack[s_pos-1]>0) {
		  stack[s_pos-1]=stack[s_pos-1];
		}else {
		  stack[s_pos-1]=-stack[s_pos-1];
		}
	      }else{
		stack[s_pos-1]=stack[s_pos-1]*(block_mode[nzcr]);
	      }
	      break;
	    case f_max: 
	      if(block_ng>0) nzcr=nzcr+1;
	      if (flag==9) {
		if (stack[s_pos-1]>stack[s_pos-2]) {
		  i=0;
		}else {
		  i=1;
		}
		block_g[nzcr]=stack[s_pos-1]-stack[s_pos-2];
		if(phase==1) block_mode[nzcr]=i;
	      }
	      if(phase==1||block_ng==0){
		stack[s_pos-2]=Max(stack[s_pos-2],stack[s_pos-1]);
	      }else{
		stack[s_pos-2]=stack[s_pos-1-block_mode[nzcr]];
	      }
	      s_pos--;
	      break;
	    case f_min: 
	      if(block_ng>0) nzcr=nzcr+1;
	      if (flag==9) {
		if (stack[s_pos-1]<stack[s_pos-2]) {
		  i=0;
		}else {
		  i=1;
		}
		block_g[nzcr]=stack[s_pos-1]-stack[s_pos-2];
		if(phase==1) block_mode[nzcr]=i;
	      }
	      if(phase==1||block_ng==0){
		stack[s_pos-2]=Min(stack[s_pos-2],stack[s_pos-1]);
	      }else{
		stack[s_pos-2]=stack[s_pos-1-block_mode[nzcr]];
	      }
	      s_pos--;
	      break;
	    default: 
	      ok = FALSE;
	      break;
	    }
	  if (ok) break;
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
	  if ( s_pos == BEVAL_STACK_SIZE ) 
	    {
	      scicos_set_block_error(-16);
	      return FAIL;
	    }
	  break;
	case 4:
	  /* Sciprintf("A number %f\n",constv[ bcode & 0xffff]); */
	  stack[s_pos]=constv[ bcode & 0xffff]; 
	  s_pos++;
	  if ( s_pos == BEVAL_STACK_SIZE ) 
	    {
	      scicos_set_block_error(-16);
	      return FAIL;
	    }
	  break;
	}
    }
  *res = stack[0];
  return OK;
}



