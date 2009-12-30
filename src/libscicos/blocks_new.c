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
 * Some blocks have specific authors which are named in the code. 
 * 
 *--------------------------------------------------------------------------*/

#include <stdlib.h> 
#include <math.h>
#include "nsp/machine.h"
#include "../graphics-new/new_graphics.h"
#include "nsp/graphics-old/Graphics.h" 
#include "nsp/object.h" 
#include "nsp/blas.h" 
#include "nsp/matutil.h" 

#include "scicos/scicos.h"
#include "scicos/blocks.h"

/* to be moved elsewhere XXXX  */

#if WIN32
extern double asinh(double x);
extern double acosh(double x);
extern double atanh(double x);
#endif


static void * scicos_malloc(size_t size);
static void scicos_free(void *p);



/*
 * utility to set wid as the current graphic window
 */

BCG *scicos_set_win(int wid,int *oldwid)
{
  BCG *Xgc;
  if ((Xgc = window_list_get_first_old()) != NULL)
    {
      *oldwid = Xgc->graphic_engine->xget_curwin();
      if ( *oldwid != wid) 
	{
	  Xgc->graphic_engine->xset_curwin(Max(wid,0),TRUE);
	  Xgc =  window_list_get_first_old();
	}
    }
  else 
    {
      Xgc= set_graphic_window_old(Max(wid,0));
    }
  return Xgc;
}




void scicos_absolute_value_block(scicos_block *block,int flag)
{
  int i,j;
  switch (flag) 
    {
    case 1 : 
      if( block->ng>0){
	for(i=0;i<block->insz[0];++i){
	  if (scicos_get_phase_simulation()==1) {
	    if (block->inptr[0][i]<0){
	      j=2;
	    } else{
	      j=1;
	    }
	  }else {
	    j=block->mode[i];
	  }
	  if (j==1){
	    block->outptr[0][i]=block->inptr[0][i];
	  } else{
	    block->outptr[0][i]=-block->inptr[0][i];
	  }
	}
      }else{
	for(i=0;i<block->insz[0];++i){
	  if (block->inptr[0][i]<0){
	    block->outptr[0][i]=-block->inptr[0][i];
	  }else{
	    block->outptr[0][i]=block->inptr[0][i];
	  }
	}
      }
      break;
    case 9: 
      for(i=0;i<block->insz[0];++i){
	block->g[i]=block->inptr[0][i];
	if (scicos_get_phase_simulation()==1) {
	  if(block->g[i]<0){
	    block->mode[i]=2;
	  }else{
	    block->mode[i]=1;
	  }
	}
      }
    }
}



void scicos_acos_block(scicos_block *block,int flag)
{
  int j;
  if(flag==1){
    for (j=0;j<block->insz[0];j++) {
      block->outptr[0][j]=acos(block->inptr[0][j]);
    }
  }
}

void scicos_acosh_block(scicos_block *block,int flag)
{
  int j;
  if(flag==1){
    for (j=0;j<block->insz[0];j++) {
      block->outptr[0][j]=acosh(block->inptr[0][j]);
    }
  }
}

void scicos_asin_block(scicos_block *block,int flag)
{
  int j;
  if(flag==1){
    for (j=0;j<block->insz[0];j++) {
      block->outptr[0][j]=asin(block->inptr[0][j]);
    }
  }
}

void scicos_asinh_block(scicos_block *block,int flag)
{
  int j;
  if(flag==1){
    for (j=0;j<block->insz[0];j++) {
      block->outptr[0][j]=asinh(block->inptr[0][j]);
    }
  }
}

void scicos_atan_block(scicos_block *block,int flag)
{
  int j;
  if(flag==1){
    for (j=0;j<block->insz[0];j++) {
      block->outptr[0][j]=atan(block->inptr[0][j]);
    }
  }
}

void scicos_atanh_block(scicos_block *block,int flag)
{
  int j;
  if(flag==1){
    for (j=0;j<block->insz[0];j++) {
      block->outptr[0][j]=atanh(block->inptr[0][j]);
    }
  }
}


void scicos_tanh_block(scicos_block *block,int flag)
{
  int j;
  if(flag==1){
    for (j=0;j<block->insz[0];j++) {
      block->outptr[0][j]=tanh(block->inptr[0][j]);
    }
  }
}


void scicos_tan_block(scicos_block *block,int flag)
{
  int j;
  if(flag==1){
    for (j=0;j<block->insz[0];j++) {
      block->outptr[0][j]=tan(block->inptr[0][j]);
    }
  }
}


void scicos_sin_block(scicos_block *block,int flag)
{
  int j;
  if(flag==1){
    for (j=0;j<block->insz[0];j++) {
      block->outptr[0][j]=sin(block->inptr[0][j]);
    }
  }
}

void scicos_sinh_block(scicos_block *block,int flag)
{
  int j;
  if(flag==1){
    for (j=0;j<block->insz[0];j++) {
      block->outptr[0][j]=sinh(block->inptr[0][j]);
    }
  }
}



void scicos_backlash_block(scicos_block *block,int flag)
{ 
  double* rw,t;
  if (flag == 4){/* the workspace is used to store previous values */
    if ((*block->work=	 scicos_malloc(sizeof(double)* 4))== NULL ) {
      scicos_set_block_error(-16);
      return;
    }
    rw=*block->work; 
    t=scicos_get_time();
    rw[0]=t;
    rw[1]=t;
    rw[2]=block->rpar[0];
    rw[3]=block->rpar[0];
  }else  if (flag == 5){
    scicos_free(*block->work);
  }else  if (flag == 1) {
    rw=*block->work;
    t=scicos_get_time();
    if(t>rw[1]) {
      rw[0]=rw[1];
      rw[2]=rw[3];
    }
    rw[1]=t;
    if(block->inptr[0][0]>rw[2]+block->rpar[1]/2){
      rw[3]=block->inptr[0][0]-block->rpar[1]/2;
    } else if (block->inptr[0][0]<rw[2]-block->rpar[1]/2){
      rw[3]=block->inptr[0][0]+block->rpar[1]/2;
    } else {
      rw[3]=rw[2];
    }
    block->outptr[0][0]=rw[3];
  }  else if (flag == 9) {
    rw=*block->work;
    t=scicos_get_time();
    if(t>rw[1]){
      block->g[0] = block->inptr[0][0]-block->rpar[1]/2-rw[3];
      block->g[1] = block->inptr[0][0]+block->rpar[1]/2-rw[3];
    }else{
      block->g[0] = block->inptr[0][0]-block->rpar[1]/2-rw[2];
      block->g[1] = block->inptr[0][0]+block->rpar[1]/2-rw[2];
    }
  } 
}

void scicos_cos_block(scicos_block *block,int flag)
{
  int j;
  if(flag==1){
    for (j=0;j<block->insz[0];j++) {
      block->outptr[0][j]=cos(block->inptr[0][j]);
    }
  }
}

void scicos_cosh_block(scicos_block *block,int flag)
{
  int j;
  if(flag==1){
    for (j=0;j<block->insz[0];j++) {
      block->outptr[0][j]=cosh(block->inptr[0][j]);
    }
  }
}

void scicos_deadband_block(scicos_block *block,int flag)
{/* rpar[0]:upper limit,  rpar[1]:lower limit */
  if (flag==1){
    if (scicos_get_phase_simulation()==1||block->ng==0) {
      if (*block->inptr[0]>=block->rpar[0]){
	block->outptr[0][0]=*block->inptr[0]-block->rpar[0];
      }else if (*block->inptr[0]<=block->rpar[1]){
	block->outptr[0][0]=*block->inptr[0]-block->rpar[1];
      }else {
	block->outptr[0][0]=0.0;
      }
    }else{
      if (block->mode[0]==1){
	block->outptr[0][0]=*block->inptr[0]-block->rpar[0];
      }else if(block->mode[0]==2) {
	block->outptr[0][0]=*block->inptr[0]-block->rpar[1];
      }else{
	block->outptr[0][0]=0.0;
      } 
    }
  } else if (flag==9){
    block->g[0]=*block->inptr[0]-(block->rpar[0]);
    block->g[1]=*block->inptr[0]-(block->rpar[1]);
    if (scicos_get_phase_simulation()==1) {
      if (block->g[0]>=0){
	block->mode[0]=1;
      }else if (block->g[1]<=0){
	block->mode[0]=2;
      }else {
	block->mode[0]=3;
      }
    }
  }
}


void scicos_deriv_block(scicos_block *block,int flag)
{ 
  double* rw;
  double t,dt;
  int i;
  if (flag == 4){/* the workspace is used to store previous values */
    if ((*block->work=
	 scicos_malloc(sizeof(double)*2*(1+block->insz[0])))== NULL ) {
      scicos_set_block_error(-16);
      return;
    }
    rw=*block->work; 
    t=scicos_get_time();
    rw[0]=t;
    rw[1]=t;
    for(i=0;i<block->insz[0];++i){
      rw[2+2*i]=0;
      rw[3+2*i]=0;
    }
  }else  if (flag == 5){
    scicos_free(*block->work);
  }else  if (flag == 1) {
    rw=*block->work;
    t=scicos_get_time();
    if(t>rw[1]) {
      rw[0]=rw[1];
      for(i=0;i<block->insz[0];++i)  {rw[2+2*i]=rw[3+2*i];}
    }
    rw[1]=t;
    for(i=0;i<block->insz[0];++i) {
      rw[3+2*i]=block->inptr[0][i];
    }
    dt=rw[1]-rw[0];

    if(dt!=0.0) {
      for(i=0;i<block->insz[0];++i)
	block->outptr[0][i]=(rw[3+2*i]-rw[2+2*i])/dt;
    }
  }
}
  

void scicos_extractor_block(scicos_block *block,int flag)
{
  int i,j;
  if(flag==1){
    for(i=0;i<block->nipar;++i){
      j=block->ipar[i]-1;
      if(j<0) j=0;
      if(j>=block->insz[0]) j=block->insz[0]-1;
      block->outptr[0][i]=block->inptr[0][j];
    }
  }
}


void scicos_gainblk_block(scicos_block *block,int flag)
{
  int i,un=1;
  if (block->nrpar==1){
    for (i=0;i<block->insz[0];++i){
      block->outptr[0][i]=block->rpar[0]*block->inptr[0][i];
    }
  }else{
    dmmul_scicos(block->rpar,&block->outsz[0],block->inptr[0],
		 &block->insz[0],block->outptr[0],&block->outsz[0],
		 &block->outsz[0],&block->insz[0],&un);
  }
}




void scicos_time_delay_block(scicos_block *block,int flag)
{/*  rpar[0]=delay, rpar[1]=init value, ipar[0]=buffer length */
  double* pw,del,t,td;
  int* iw;
  int i,j,k;
  if (flag == 4){/* the workspace is used to store previous values */
    if ((*block->work=
	 scicos_malloc(sizeof(int)+sizeof(double)* 
		       block->ipar[0]*(1+block->insz[0])))== NULL ) {
      scicos_set_block_error(-16);
      return;
    }
    pw=*block->work; 
    pw[0]=-block->rpar[0]*block->ipar[0];
    for(i=1;i< block->ipar[0];i++){
      pw[i]=pw[i-1]+block->rpar[0];
      for(j=1;j< block->insz[0]+1;j++){
	pw[i+block->ipar[0]*j]=block->rpar[1];
      }
    }
    iw=(int *)(pw+block->ipar[0]*(1+block->insz[0]));
    *iw=0;
  }else  if (flag == 5){
    scicos_free(*block->work);
  } else if (flag==0||flag==2) {
    if (flag==2) scicos_do_cold_restart();
    pw=*block->work; 
    iw=(int *)(pw+block->ipar[0]*(1+block->insz[0]));
    t=scicos_get_time();
    td=t-block->rpar[0];
    if(td<pw[*iw]){
      Sciprintf("delayed time=%f but last stored time=%f \r\n", td, pw[*iw]);
      Sciprintf("Consider increasing the length of buffer in delay block \r\n");
    }

    if (t>pw[(block->ipar[0]+*iw-1)%block->ipar[0]]){
      for(j=1;j< block->insz[0]+1;j++){
	pw[*iw +block->ipar[0]*j]=block->inptr[0][j-1];
      }
      pw[*iw]=t;
      /*sciprint("**time is %f. I put %f, in %d \r\n", t,block->inptr[0][0],*iw);*/
      *iw=(*iw+1)%block->ipar[0];
 
    }else{
      for(j=1;j< block->insz[0]+1;j++){
	pw[(block->ipar[0]+*iw-1)%block->ipar[0] +block->ipar[0]*j]=block->inptr[0][j-1];
      }
      pw[(block->ipar[0]+*iw-1)%block->ipar[0]]=t;
      /*sciprint("**time is %f. I put %f, in %d \r\n", t,block->inptr[0][0],*iw);*/

    }

  } else if (flag==1) {
    pw=*block->work; 
    iw=(int *) (pw+block->ipar[0]*(1+block->insz[0]));
    t=scicos_get_time();
    td=t-block->rpar[0];

    i=0;j= block->ipar[0]-1;

    while (j-i>1) {
      k=(i+j)/2;
      if (td<pw[(k+*iw)%block->ipar[0]]) {
	j=k;
      }else if (td>pw[(k+*iw)%block->ipar[0]]) {
	i=k;
      }else{
	i=k;
	j=k;
	break;
      }
    }
    i=(i+*iw)%block->ipar[0];
    j=(j+*iw)%block->ipar[0];
    del=pw[j]-pw[i];
    /*sciprint("time is %f. interpolating %d and %d, i.e. %f, %f\r\n", t,i,j,pw[i],pw[j]);*/
    if(del!=0.0){
      for (k=1;k<block->insz[0]+1;k++){
	block->outptr[0][k-1]=((pw[j]-td)*pw[i+block->ipar[0]*k] +
			       (td-pw[i])*pw[j+block->ipar[0]*k])/del;
      }
    }else{
      for (k=1;k<block->insz[0]+1;k++){
	block->outptr[0][k-1]=pw[i+block->ipar[0]*k];
      }
    }
  }
}



void scicos_variable_delay_block(scicos_block *block,int flag)
{/*  rpar[0]=max delay, rpar[1]=init value, ipar[0]=buffer length */
  double* pw,del,t,td;
  int* iw;
  int i,j,k;
  if (flag == 4){/* the workspace is used to store previous values */
    if ((*block->work=
	 scicos_malloc(sizeof(int)+sizeof(double)* 
		       block->ipar[0]*(1+block->insz[0])))== NULL ) {
      scicos_set_block_error(-16);
      return;
    }
    pw=*block->work; 
    pw[0]=-block->rpar[0]*block->ipar[0];
    for(i=1;i< block->ipar[0];i++){
      pw[i]=pw[i-1]+block->rpar[0];
      for(j=1;j< block->insz[0]+1;j++){
	pw[i+block->ipar[0]*j]=block->rpar[1];
      }
    }
    iw=(int *) (pw+block->ipar[0]*(1+block->insz[0]));
    *iw=0;
  }else  if (flag == 5){
    scicos_free(*block->work);
  } else if (flag==1) {
    if (scicos_get_phase_simulation()==1) scicos_do_cold_restart();
    pw=*block->work; 
    iw=(int *) (pw+block->ipar[0]*(1+block->insz[0]));
    t=scicos_get_time();
    del=Min(Max(0,block->inptr[1][0]),block->rpar[0]);
    td=t-del;
    if(td<pw[*iw]){
      sciprint("delayed time=%f but last stored time=%f \r\n", td, pw[*iw]);
      sciprint("Consider increasing the length of buffer in variable delay block\r\n");
    }
    if (t>pw[(block->ipar[0]+*iw-1)%block->ipar[0]]){
      for(j=1;j< block->insz[0]+1;j++){
	pw[*iw +block->ipar[0]*j]=block->inptr[0][j-1];
      }
      pw[*iw]=t;
      *iw=(*iw+1)%block->ipar[0];
    }else{
      for(j=1;j< block->insz[0]+1;j++){
	pw[(block->ipar[0]+*iw-1)%block->ipar[0] +block->ipar[0]*j]=block->inptr[0][j-1];
      }
      pw[(block->ipar[0]+*iw-1)%block->ipar[0]]=t;
    }

    i=0;j= block->ipar[0]-1;

    while (j-i>1) {
      k=(i+j)/2;
      if (td<pw[(k+*iw)%block->ipar[0]]) {
	j=k;
      }else if (td>pw[(k+*iw)%block->ipar[0]]) {
	i=k;
      }else{
	i=k;
	j=k;
	break;
      }
    }
    i=(i+*iw)%block->ipar[0];
    j=(j+*iw)%block->ipar[0];
    del=pw[j]-pw[i];
    if(del!=0.0){
      for (k=1;k<block->insz[0]+1;k++){
	block->outptr[0][k-1]=((pw[j]-td)*pw[i+block->ipar[0]*k] +
			       (td-pw[i])*pw[j+block->ipar[0]*k])/del;
      }
    }else{
      for (k=1;k<block->insz[0]+1;k++){
	block->outptr[0][k-1]=pw[i+block->ipar[0]*k];
      }
    }
  }
}


void scicos_step_func_block(scicos_block *block,int flag)
{
  int i;
  if (flag==1&&block->nevprt==1) {
    for (i=0;i<block->outsz[0];++i) {
      block->outptr[0][i]=block->rpar[block->outsz[0]+i];
    } 
  }else if (flag==4) {
    for(i=0;i<block->outsz[0];++i) {
      block->outptr[0][i]=block->rpar[i];
    }
  }
}



void scicos_signum_block(scicos_block *block,int flag)
{
  int i,j;
  if (flag==1){
    for(i=0;i<block->insz[0];++i){
      if (scicos_get_phase_simulation()==1||block->ng==0) {
	if (block->inptr[0][i]<0){
	  j=2;
	} else if (block->inptr[0][i]>0){
	  j=1;
	}else{
	  j=0;
	}
      }else {
	j=block->mode[i];
      }
      if (j==1){
	block->outptr[0][i]=1.0;
      } else if(j==2){
	block->outptr[0][i]=-1.0;
      }else{
	block->outptr[0][i]=0.0;
      }
    }
  }else if (flag==9){
    for(i=0;i<block->insz[0];++i){
      block->g[i]=block->inptr[0][i];
      if (scicos_get_phase_simulation()==1) {
	if(block->g[i]<0){
	  block->mode[i]=2;
	}else{
	  block->mode[i]=1;
	}
      }
    }
  }
}


void scicos_summation_block(scicos_block *block,int flag)
{
  int j,k;
  if(flag==1){
    if (block->nin==1){
      block->outptr[0][0]=0.0;
      for (j=0;j<block->insz[0];j++) {
	block->outptr[0][0]=block->outptr[0][0]+block->inptr[0][j];
      }
    }
    else {
      for (j=0;j<block->insz[0];j++) {
	block->outptr[0][j]=0.0;
	for (k=0;k<block->nin;k++) {
	  if(block->ipar[k]>0){
	    block->outptr[0][j]=block->outptr[0][j]+block->inptr[k][j];
	  }else{
	    block->outptr[0][j]=block->outptr[0][j]-block->inptr[k][j];
	  }
	}
      }
    }
  }
}


void scicos_switch2_block(scicos_block *block,int flag)
{
  int i=0,j,phase;
  if (flag == 1) {
    phase=scicos_get_phase_simulation();
    if (phase==1){
      i=2;
      if (*block->ipar==0){
	if (*block->inptr[1]>=*block->rpar) i=0;
      }else if (*block->ipar==1){
	if (*block->inptr[1]>*block->rpar) i=0;
      }else {
	if (*block->inptr[1]!=*block->rpar) i=0;			     
      }
    }else{
      if(block->mode[0]==1){
	i=0;
      }else if(block->mode[0]==2){
	i=2;
      }
    }
    for (j=0;j<block->insz[0];j++) {
      block->outptr[0][j]=block->inptr[i][j];
    }
  }else if(flag == 9){
    phase=scicos_get_phase_simulation();
    block->g[0]=*block->inptr[1]-(*block->rpar);
    if (phase==1){
      i=2;
      if (*block->ipar==0){
	if (block->g[0]>=0.0) i=0;
      }else if (*block->ipar==1){
	if (block->g[0]>0.0) i=0;
      }else {
	if (block->g[0]!=0.0) i=0;			     
      }
      if(i==0) {
	block->mode[0]=1;
      }else{
	block->mode[0]=2;
      }
    }
  }
}



void scicos_satur_block(scicos_block *block,int flag)
{/* rpar[0]:upper limit,  rpar[1]:lower limit */
  if (flag==1){
    if (scicos_get_phase_simulation()==1||block->ng==0) {
      if (*block->inptr[0]>=block->rpar[0]){
	block->outptr[0][0]=block->rpar[0];
      }else if (*block->inptr[0]<=block->rpar[1]){
	block->outptr[0][0]=block->rpar[1];
      }else {
	block->outptr[0][0]=block->inptr[0][0];
      }
    }else{
      if (block->mode[0]==1){
	block->outptr[0][0]=block->rpar[0];
      }else if(block->mode[0]==2) {
	block->outptr[0][0]=block->rpar[1];
      }else{
	block->outptr[0][0]=block->inptr[0][0];
      } 
    }
  } else if (flag==9){
    block->g[0]=*block->inptr[0]-(block->rpar[0]);
    block->g[1]=*block->inptr[0]-(block->rpar[1]);
    if (scicos_get_phase_simulation()==1) {
      if (block->g[0]>=0){
	block->mode[0]=1;
      }else if (block->g[1]<=0){
	block->mode[0]=2;
      }else {
	block->mode[0]=3;
      }
    }
  }
}


void scicos_logicalop_block(scicos_block *block,int flag)
{
  int i,j,k,l;
  i=block->ipar[0];
  switch (i)
    {
    case 0:
      if (block->nin==1)
	{
	  block->outptr[0][0]=1.0;
	  for (j=0;j<block->insz[0];j++) {
	    if(block->inptr[0][j]<=0)
	      {
		block->outptr[0][0]=0.0;
		break;
	      }
	  }
	}
      else {
	for (j=0;j<block->insz[0];j++) {
	  block->outptr[0][j]=1.0;
	  for (k=0;k<block->nin;k++) {
	    if(block->inptr[k][j]<=0) {
	      block->outptr[0][j]=0.0;
	      break;
	    }
	  }
	}
      }
      break;
      
    case 1:
      if (block->nin==1)
	{
	  block->outptr[0][0]=0.0;
	  for (j=0;j<block->insz[0];j++) {
	    if(block->inptr[0][j]>0) {
	      block->outptr[0][0]=1.0;
	      break;
	    }
	  }
	}
      else {
	for (j=0;j<block->insz[0];j++) {
	  block->outptr[0][j]=0.0;
	  for (k=0;k<block->nin;k++) {
	    if(block->inptr[k][j]>0) {
	      block->outptr[0][j]=1.0;
	      break;
	    }
	  }
	}
      }
      break;

    case 2:
      if (block->nin==1)
	{
	  block->outptr[0][0]=0.0;
	  for (j=0;j<block->insz[0];j++) {
	    if(block->inptr[0][j]<=0)
	      {
		block->outptr[0][0]=1.0;
		break;
	      }
	  }
	}
      else {
	for (j=0;j<block->insz[0];j++) {
	  block->outptr[0][j]=0.0;
	  for (k=0;k<block->nin;k++) {
	    if(block->inptr[k][j]<=0) {
	      block->outptr[0][j]=1.0;
	      break;
	    }
	  }
	}
      }
      break;

    case 3:
      if (block->nin==1)
	{
	  block->outptr[0][0]=1.0;
	  for (j=0;j<block->insz[0];j++) {
	    if(block->inptr[0][j]>0) {
	      block->outptr[0][0]=0.0;
	      break;
	    }
	  }
	}
      else {
	for (j=0;j<block->insz[0];j++) {
	  block->outptr[0][j]=1.0;
	  for (k=0;k<block->nin;k++) {
	    if(block->inptr[k][j]>0) {
	      block->outptr[0][j]=0.0;
	      break;
	    }
	  }
	}
      }
      break;

    case 4:
      if (block->nin==1)
	{
	  l=0;
	  for (j=0;j<block->insz[0];j++) {
	    if(block->inptr[0][j]>0) {
	      l=(l+1)%2;
	    }
	  }
	  block->outptr[0][0]=(double) l;
	}
      else {
	for (j=0;j<block->insz[0];j++) {
	  l=0;
	  for (k=0;k<block->nin;k++) {
	    if(block->inptr[k][j]>0) {
	      l=(l+1)%2;
	    }
	  }
	  block->outptr[0][j]=(double) l;
	}
      }
      break;

    case 5:
      for (j=0;j<block->insz[0];j++) {
	if(block->inptr[0][j]>0) {
	  block->outptr[0][j]=0.0;
	}
	else{
	  block->outptr[0][j]=1.0;
	}
      }
    }
}


void scicos_multiplex_block(scicos_block *block,int flag)
{
  int i,j,k;
  if (block->nin==1){
    k=0;
    for (i=0;i<block->nout;++i){
      for (j=0;j<block->outsz[i];++j){
	block->outptr[i][j]=block->inptr[0][k];
	++k;
      }
    }
  }else {
    k=0;
    for (i=0;i<block->nin;++i){
      for (j=0;j<block->insz[i];++j){
	block->outptr[0][k]=block->inptr[i][j];
	++k;
      }
    }
  }
}


void scicos_hystheresis_block(scicos_block *block,int flag)
{
  if (flag==1){
    if (scicos_get_phase_simulation()==1) {
      if (*block->inptr[0]>=block->rpar[0]){
	block->outptr[0][0]=block->rpar[2];
      }else if (*block->inptr[0]<=block->rpar[1]){
	block->outptr[0][0]=block->rpar[3];
      }
    }else{
      if (block->mode[0]<2){
	block->outptr[0][0]=block->rpar[3];
      }else{
	block->outptr[0][0]=block->rpar[2];
      }
    } 
  } else if (flag==9){
    block->g[0]=*block->inptr[0]-(block->rpar[0]);
    block->g[1]=*block->inptr[0]-(block->rpar[1]);
    if (scicos_get_phase_simulation()==1) {
      if (block->g[0]>=0){
	block->mode[0]=2;
      }else if (block->g[1]<=0){
	block->mode[0]=1;
      }
    }
  }
}



void scicos_ramp_block(scicos_block *block,int flag)
{
  double dt;
  if (flag==1){
    dt=scicos_get_time()-block->rpar[1];
    if (scicos_get_phase_simulation()==1) {
      if(dt>0) {
	block->outptr[0][0]=block->rpar[2]+block->rpar[0]*dt;
      }else{
	block->outptr[0][0]=block->rpar[2];
      }
    }else{
      if(block->mode[0]==1) {
	block->outptr[0][0]=block->rpar[2]+block->rpar[0]*dt;
      }else {
	block->outptr[0][0]=block->rpar[2];
      }
    }
  } else if (flag==9){
    block->g[0]=scicos_get_time()-(block->rpar[1]);
    if (scicos_get_phase_simulation()==1) {
      if (block->g[0]>=0){
	block->mode[0]=1;
      }else{
	block->mode[0]=2;
     }
   }
  }
}


void scicos_minmax_block(scicos_block *block,int flag)
{
  /*ipar[0]=1 -> min,  ipar[0]=2 -> max */
  int i,phase;
  double maxmin;
  phase=scicos_get_phase_simulation();
  if (flag == 1) {
    if(block->nin==1){
      if((block->ng==0)|(phase==1)){
	maxmin=block->inptr[0][0];
	for (i=1;i<block->insz[0];++i){
	  if(block->ipar[0]==1){
	    if(block->inptr[0][i]<maxmin)  maxmin=block->inptr[0][i];
	  } else {
	    if(block->inptr[0][i]>maxmin)  maxmin=block->inptr[0][i];
	  }
	}
      }else{
	maxmin=block->inptr[0][block->mode[0]-1];
      }
      block->outptr[0][0]=maxmin;

    }else if (block->nin==2){
      for (i=0;i<block->insz[0];++i){
	if((block->ng==0)|(phase==1)){
	  if(block->ipar[0]==1){
	    block->outptr[0][i]=Min(block->inptr[0][i],block->inptr[1][i]);
	  } else {
	    block->outptr[0][i]=Max(block->inptr[0][i],block->inptr[1][i]);
	  }
	}else{
	  block->outptr[0][i]=block->inptr[block->mode[0]-1][i];
	}
      }
    }
  } else if(flag == 9){
    if(block->nin==1){
      if(block->nin==1){
	if (phase==2){
	  for (i=0;i<block->insz[0];++i){
	    if(i!=block->mode[0]-1){
	      block->g[i]=block->inptr[0][i]-block->inptr[0][block->mode[0]-1];
	    }else{
	      block->g[i]=1.0;
	    }
	  }
	} else if (phase==1){
	  maxmin=block->inptr[0][0];
	  for (i=1;i<block->insz[0];++i){
	    block->mode[0]=1;
	    if(block->ipar[0]==1){
	      if(block->inptr[0][i]<maxmin) {
		maxmin=block->inptr[0][i];
		block->mode[0]=i+1;
	      }
	    } else {
	      if(block->inptr[0][i]>maxmin) {
		maxmin=block->inptr[0][i];
		block->mode[0]=i+1;
	      }
	    }
	  }
	}
      }
    }else if(block->nin==2){
      for (i=0;i<block->insz[0];++i){
	block->g[i]=block->inptr[0][i]-block->inptr[1][i];
	if(phase==1){
	  if(block->ipar[0]==1){
	    if(block->g[i]>0) {
	      block->mode[i]=2;
	    }else{
	      block->mode[i]=1;
	    }
	  }else{
	    if(block->g[i]<0) {
	      block->mode[i]=2;
	    }else{
	      block->mode[i]=1;
	    }
	  }
	}
      }
    }
  }
}


void scicos_modulo_count_block(scicos_block *block,int flag)
{
  if(flag == 1) {
    *block->outptr[0]=block->z[0];
  }else if (flag == 2){
    block->z[0]=(1+(int)block->z[0])%(block->ipar[0]);
  }
}



void scicos_mswitch_block(scicos_block *block,int flag)
{
  int i,j=0;
  i=block->ipar[1];
  if (i==0) {
    if (*block->inptr[0]>0) {
      j=(int)floor(*block->inptr[0]);
    }else{
      j=(int)ceil(*block->inptr[0]);
    }
  }else if (i==1) {
    if (*block->inptr[0]>0) {
      j=(int)floor(*block->inptr[0]+.5);
    }else{
      j=(int)ceil(*block->inptr[0]-.5);
    }
  }else if (i==2) {
    j=(int)ceil(*block->inptr[0]);
  }else if (i==3) {
    j=(int)floor(*block->inptr[0]);
  }
  j=j+1-block->ipar[0];
  j=Max(j,1);
  j=Min(j,block->nin-1);
  for (i=0;i<block->insz[j];i++) {
    block->outptr[0][i]=block->inptr[j][i];
  }
}


void scicos_product_block(scicos_block *block,int flag)
{
  int j,k;
  if(flag==1){
    if (block->nin==1){
      block->outptr[0][0]=1.0;
      for (j=0;j<block->insz[0];j++) {
	block->outptr[0][0]=block->outptr[0][0]*block->inptr[0][j];
      }
    }
    else {
      for (j=0;j<block->insz[0];j++) {
	block->outptr[0][j]=1.0;
	for (k=0;k<block->nin;k++) {
	  if(block->ipar[k]>0){
	    block->outptr[0][j]=block->outptr[0][j]*block->inptr[k][j];
	  }else{
	    if(block->inptr[k][j]==0){
	      scicos_set_block_error(-2);
	      return;
	    }else{
	      block->outptr[0][j]=block->outptr[0][j]/block->inptr[k][j];
	    }
	  }
	}
      }
    }
  }
}

/*
 * rpar[0]=rising rate limit, rpar[1]=falling rate limit 
 */

void scicos_ratelimiter_block(scicos_block *block,int flag)
{
  double *pw,rate=0.0,t;
  if (flag == 4){/* the workspace is used to store previous values */
    if ((*block->work=
	 scicos_malloc(sizeof(double)*4))== NULL ) {
      scicos_set_block_error(-16);
      return;
    }
    pw=*block->work; 
    pw[0]=0.0;
    pw[1]=0.0;
    pw[2]=0.0;
    pw[3]=0.0;
  }else  if (flag == 5){
    scicos_free(*block->work);
  } else if (flag==1) {
    if (scicos_get_phase_simulation()==1) scicos_do_cold_restart();
    pw=*block->work; 
    t=scicos_get_time();
    if(t>pw[2]){
      pw[0]=pw[2];
      pw[1]=pw[3];
      rate=(block->inptr[0][0]-pw[1])/(t-pw[0]);
    }
    else if(t<=pw[2]){
      if(t>pw[0]){
	rate=(block->inptr[0][0]-pw[1])/(t-pw[0]);
      }else{
	rate=0.0;
      }
    }
    if(rate>block->rpar[0]){
      block->outptr[0][0]=(t-pw[0])*block->rpar[0]+pw[1];
    } else if(rate<block->rpar[1]){
      block->outptr[0][0]=(t-pw[0])*block->rpar[1]+pw[1];
    }else{
      block->outptr[0][0]=block->inptr[0][0];
    }
    pw[2]=t;
    pw[3]=block->outptr[0][0];
  }
}

void scicos_integral_func_block(scicos_block *block,int flag)
{
  int i;
  if (flag==0){
    if(block->ng>0){
      for(i=0;i<block->nx;++i) {
	if(block->mode[i]==3){
	  block->xd[i]=block->inptr[0][i];
	}else{
	  block->xd[i]=0.0;
	}
      }
    }else{
      for(i=0;i<block->nx;++i) {
	block->xd[i]=block->inptr[0][i];
      }
    }
  }else if (flag==1){
    for(i=0;i<block->nx;++i) {
      block->outptr[0][i]=block->x[i];
    }
  }else if (flag==2&&block->nevprt==1){
    for(i=0;i<block->nx;++i) {
      block->x[i]=block->inptr[1][i];
    }
  } else if (flag==9){
    for(i=0;i<block->nx;++i) {
      if (block->mode[i]==3){
	block->g[i]=(block->x[i]-(block->rpar[0]))*(block->x[i]-(block->rpar[1]));
      } else {
	block->g[i]=block->inptr[0][i];
      } 
      if (scicos_get_phase_simulation()==1) {
	if (block->inptr[0][i]>=0&&block->x[i]>=block->rpar[0]){
	  block->mode[i]=1;
	}else if (block->inptr[0][i]<=0&&block->x[i]<=block->rpar[1]){
	  block->mode[i]=2;
	}else {
	  block->mode[i]=3;
	}
      }
    }
  }
}

void scicos_evtvardly_block(scicos_block *block,int flag)
{ 
  if (flag==3){
    block->evout[0]=block->inptr[0][0];
  }
}


void scicos_relationalop_block(scicos_block *block,int flag)
{
  int i;
  i=block->ipar[0];
  if(flag==1){
    if ((block->ng!=0)&(scicos_get_phase_simulation()==2))
      {
	block->outptr[0][0]=block->mode[0]-1.0;
      }
    else {
      switch (i)
	{
	case 0:
	  if(block->inptr[0][0]==block->inptr[1][0]) {
	    block->outptr[0][0]=1.0;
	  }else{
	    block->outptr[0][0]=0.0;
	  }
	  break;
	  
	case 1:
	  if(block->inptr[0][0]!=block->inptr[1][0]) {
	    block->outptr[0][0]=1.0;
	  }else{
	    block->outptr[0][0]=0.0;
	  }
	  break;
	case 2:
	  if(block->inptr[0][0]<block->inptr[1][0]) {
	    block->outptr[0][0]=1.0;
	  }else{
	    block->outptr[0][0]=0.0;
	  }
	  break;    
	case 3:
	  if(block->inptr[0][0]<=block->inptr[1][0]) {
	    block->outptr[0][0]=1.0;
	  }else{
	    block->outptr[0][0]=0.0;
	  }
	  break;   
	case 4:
	  if(block->inptr[0][0]>=block->inptr[1][0]) {
	    block->outptr[0][0]=1.0;
	  }else{
	    block->outptr[0][0]=0.0;
	  }
	  break;   
	case 5:
	  if(block->inptr[0][0]>block->inptr[1][0]) {
	    block->outptr[0][0]=1.0;
	  }else{
	    block->outptr[0][0]=0.0;
	  }
	  break;
	}
    }

  }else if(flag==9){
    block->g[0]=block->inptr[0][0]-block->inptr[1][0];
    if (scicos_get_phase_simulation()==1) {
      switch (i)
	{
	case 0:
	  if(block->inptr[0][0]==block->inptr[1][0]) {
	    block->mode[0]=(int)2.0;
	  }else{
	    block->mode[0]=(int)1.0;
	  }
	  break;
	  
	case 1:
	  if(block->inptr[0][0]!=block->inptr[1][0]) {
	    block->mode[0]=(int)2.0;
	  }else{
	    block->mode[0]=(int)1.0;
	  }
	  break;
	case 2:
	  if(block->inptr[0][0]<block->inptr[1][0]) {
	    block->mode[0]=(int)2.0;
	  }else{
	    block->mode[0]=(int)1.0;
	  }
	  break;    
	case 3:
	  if(block->inptr[0][0]<=block->inptr[1][0]) {
	    block->mode[0]=(int)2.0;
	  }else{
	    block->mode[0]=(int)1.0;
	  }
	  break;   
	case 4:
	  if(block->inptr[0][0]>=block->inptr[1][0]) {
	    block->mode[0]=(int)2.0;
	  }else{
	    block->mode[0]=(int)1.0;
	  }
	  break;   
	case 5:
	  if(block->inptr[0][0]>block->inptr[1][0]) {
	    block->mode[0]=(int)2.0;
	  }else{
	    block->mode[0]=(int)1.0;
	  }
	  break;
	}
    }
  }
}


void scicos_bounce_ball_block(scicos_block *block,int flag)
{
  int nevprt,nx,*ipar;
  int *outsz;
  double *x,*xd,*rpar;
  double *g;
  int ng;
  int *jroot;
       
  int i1;
  double d1, d2, d3;
  
  static double a, b, c;
  static int i, j, k, n;
  static double  s1, s2, s3, s4, xsi,*y1,*y2;
  
  /*     Scicos block simulator */
  /*     bouncing ball */
  /*     rpar(i): mass of ball i */
  /*     rpar(i+n): radius of ball i */
  /*     rpar(2n+1:2n+4); [xmin,xmax,ymin,ymax] */
  /*     x: [x1,x1',y1,y1',x2,x2',y2,y2',...,yn'] */
  /*     n:number of ball=ny1=ny2 */
  /*     y1: x-coord des balles */
  /*     y2: y-coord des balles */
  /*     ipar: storage de taille [nx(n-1)/2=ng]*2 */
  nevprt=block->nevprt;
  nx=block->nx;
  ipar=block->ipar;
  outsz=block->outsz;
  x=block->x;
  xd=block->xd;
  rpar=block->rpar;

  g=block->g;
  ng=block->ng;
  jroot=block->jroot;
  /* Parameter adjustments to index vectors as in Scilab (fortran)*/
  --g;
  --ipar;
  --rpar;
  --x;
  --xd;
  y1=block->outptr[0];
  y2=block->outptr[1];
  --y2;
  --y1;
  --jroot;
  
  n = outsz[0];
  if (flag == 0) {
    c = rpar[(n << 1) + 6];
    i1 = n;
    for (i = 1; i <= i1; ++i) {
      xd[((i - 1) << 2) + 1] = x[((i - 1) << 2) + 2];
      xd[((i - 1) << 2) + 3] = x[((i - 1) << 2) + 4];
      xd[((i - 1) << 2) + 2] = -c * x[((i - 1) << 2) + 2];
      xd[((i - 1) << 2) + 4] = -rpar[(n << 1) + 5] ;
    }
    
  } else if (flag == 1) {
    i1 = n;
    for (i = 1; i <= i1; ++i) {
      y1[i] = x[((i - 1) << 2) + 1];
      y2[i] = x[((i - 1) << 2) + 3];
    }
  } else if (flag == 9) {
    i1 = ng - (n << 2);
    for (k = 1; k <= i1; ++k) {
      i = ipar[((k - 1) << 1) + 1];
      j = ipar[((k - 1) << 1) + 2];
      d1 = x[((i - 1) << 2) + 1] - x[((j - 1) << 2) + 1];
      d2 = x[((i - 1) << 2) + 3] - x[((j - 1) << 2) + 3];
      d3 = rpar[i + n] + rpar[j + n];
      g[k] = d1 * d1 + d2 * d2 - d3 * d3;
    }
    k = ng - (n << 2) + 1;
    i1 = n;
    for (i = 1; i <= i1; ++i) {
      g[k] = x[((i - 1) << 2) + 3] - rpar[i + n] - rpar[(n << 1) + 3];
      ++k;
      g[k] = rpar[(n << 1) + 4] - x[((i - 1) << 2) + 3] - rpar[i + n];
      ++k;
      g[k] = x[((i - 1) << 2) + 1] - rpar[(n << 1) + 1] - rpar[i + n];
      ++k;
      g[k] = rpar[(n << 1) + 2] - rpar[i + n] - x[((i - 1) << 2) + 1];
      ++k;
    }
    
  } else if (flag == 2 && nevprt < 0) {
    i1 = ng - (n << 2);
    for (k = 1; k <= i1; ++k) {
      if (jroot[k] < 0) {
	i = ipar[((k - 1) << 1) + 1];
	j = ipar[((k - 1) << 1) + 2];
	s1 = x[((j - 1) << 2) + 1] - x[((i - 1) << 2) + 1];
	s2 = -rpar[i] * s1 / rpar[j];
	s3 = x[((j - 1) << 2) + 3] - x[((i - 1) << 2) + 3];
	s4 = -rpar[i] * s3 / rpar[j];
	a = rpar[i] * (s1 * s1 + s3 * s3) + rpar[j] * (s2 * s2 + s4 * s4);
	b = rpar[i] 
	  * (s1 * x[((i - 1) << 2) + 2] + s3 * x[((i - 1 ) << 2) + 4])
	  + rpar[j] * (s2 * x[((j - 1) << 2) + 2] + s4 * x[((j - 1) << 2) + 4]);
	xsi = -(b * 2. / a);
	x[((i - 1) << 2) + 2] += s1 * xsi;
	x[((j - 1) << 2) + 2] += s2 * xsi;
	x[((i - 1) << 2) + 4] += s3 * xsi;
	x[((j - 1) << 2) + 4] += s4 * xsi;
      }
    }
    k = ng - (n << 2) + 1;
    i1 = n;
    for (i = 1; i <= i1; ++i) {
      if (jroot[k] < 0) {
	x[((i - 1) << 2) + 4] = -x[((i - 1) << 2) + 4];
      }
      ++k;
      if (jroot[k] < 0) {
	x[((i - 1) << 2) + 4] = -x[((i - 1) << 2) + 4];
      }
      ++k;
      if (jroot[k] < 0) {
	x[((i - 1) << 2) + 2] = -x[((i - 1) << 2) + 2];
      }
      ++k;
      if (jroot[k] < 0) {
	x[((i - 1) << 2) + 2] = -x[((i - 1) << 2) + 2];
      }
      ++k;
    }
  }
} 


void scicos_bouncexy_block(scicos_block *block,int flag)
{
  char *str;
  BCG *Xgc=NULL;
  int c__1 = 1;
  int c_n1 = -1;
  int c__5 = 5;
  int c__0 = 0;
  int c__3 = 3;
  int nevprt=block->nevprt;
  double t;
  double *z__;
  int nz;
  double *rpar;
  int nrpar, *ipar, nipar;
  double *u,*y;
  int nu;
 
  static int cur = 0;
  static int c400=400;
  int i__1;
  
  static double rect[4];
  static double xmin, ymin, xmax, ymax;
  static int i__, n;
  static int on;
  static double zz[10];
  static char buf[40];
  static int wid, nax[4];
  
  /*     ipar(1) = win_num */
  /*     ipar(2) = mode : animated =0 fixed=1 */
  /*     ipar(3:3+nu-1) = colors of balls */
  
  /*     rpar(1)=xmin */
  /*     rpar(2)=xmax */
  /*     rpar(3)=ymin */
  /*     rpar(4)=ymax */
  nevprt=block->nevprt;
  nz=block->nz;
  nrpar=block->nrpar; 
  nipar=block->nipar;
  nu=block->insz[0];
  z__=block->z;
  rpar=block->rpar;
  ipar=block->ipar;
  u=block->inptr[0];
  y=block->inptr[1];
  t=scicos_get_time();

  
  /* Parameter adjustments */
  --u;
  --y;
  --ipar;
  --rpar;
  --z__;
  
  /* Function Body */
  
  if (flag == 2) {
    wid = ipar[1];
    n = nu;
    Xgc = scicos_set_win(wid,&cur);
    Xgc->graphic_engine->xset_recording(Xgc,FALSE);
    rect[0] = xmin;
    rect[1] = ymin;
    rect[2] = xmax;
    rect[3] = ymax;
    nsp_plot2d_old(Xgc,rect, &rect[1],&c__1, &c__1, &c_n1, "030", buf,0, rect, nax);
    /*     draw new point */
    i__1 = nu;
    for (i__ = 1; i__ <= i__1; ++i__) {
      z__[(i__ - 1) * 6 + 1] = u[i__] - z__[(i__ - 1) * 6 + 3] / 2;
      z__[(i__ - 1) * 6 + 2] = y[i__] + z__[(i__ - 1) * 6 + 4] / 2;
    }
    Xgc->graphic_engine->xset_pixmapclear(Xgc);
    Xgc->graphic_engine->scale->fillarcs(Xgc,&z__[1],&ipar[3],n);
    xmin = rpar[1];
    xmax = rpar[2];
    ymin = rpar[3];
    ymax = rpar[4];
    zz[0] = xmin;
    zz[1] = xmin;
    zz[2] = xmax;
    zz[3] = xmax;
    zz[4] = xmin;
    zz[5] = ymax;
    zz[6] = ymin;
    zz[7] = ymin;
    zz[8] = ymax;
    zz[9] = ymax;
    Xgc->graphic_engine->scale->drawpolylines(Xgc, zz, &zz[5], &c__1, c__1, c__5);
    Xgc->graphic_engine->xset_show(Xgc);
    Xgc->graphic_engine->xset_recording(Xgc,TRUE);
  } else if (flag == 4) {
    wid = ipar[1];
    n = nu;
    xmin = rpar[1];
    xmax = rpar[2];
    ymin = rpar[3];
    ymax = rpar[4];
    nax[0] = 2;
    nax[1] = 10;
    nax[2] = 2;
    nax[3] = 10;
    Xgc = scicos_set_win(wid,&cur);
    Xgc->graphic_engine->xset_windowdim(Xgc,c400,c400);
    Xgc = scicos_set_win(wid,&cur);
    Xgc->graphic_engine->xset_recording(Xgc,FALSE);
    on = 1;
    Xgc->graphic_engine->xset_pixmapOn(Xgc,on);
    rect[0] = xmin;
    rect[1] = ymin;
    rect[2] = xmax;
    rect[3] = ymax;
    Xgc->graphic_engine->xset_usecolor(Xgc,c__1);
    Xgc->graphic_engine->xset_alufunction1(Xgc,c__3);
    Xgc->graphic_engine->clearwindow(Xgc);
    Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
    Xgc->graphic_engine->xset_thickness(Xgc,c__1);
    Xgc->graphic_engine->xset_dash(Xgc,c__0);
    nsp_plot2d_old(Xgc,rect, &rect[1],&c__1, &c__1, &c_n1, "030", buf,0, rect, nax);
    zz[0] = xmin;
    zz[1] = xmin;
    zz[2] = xmax;
    zz[3] = xmax;
    zz[4] = xmin;
    zz[5] = ymax;
    zz[6] = ymin;
    zz[7] = ymin;
    zz[8] = ymax;
    zz[9] = ymax;
    Xgc->graphic_engine->scale->drawpolylines(Xgc,zz, &zz[5],&c__1, c__1, c__5);
    Xgc->graphic_engine->xset_show(Xgc);
    str = block->label ;
    if ( str != NULL && strlen(str) != 0 && strcmp(str," ") != 0 ) 
      Xgc->graphic_engine->setpopupname(Xgc,str);
    /* XXXXXXXXX C2F(sxevents)(); */
  }
} 


/*
 * a scope 
 */

typedef struct _cscope_ipar cscope_ipar ;
struct _cscope_ipar {
  int wid, color_flag,n,type[8],wpos[2],wdim[2];};

typedef struct _cscope_rpar cscope_rpar ;
struct _cscope_rpar {
  double dt,ymin,ymax,per;};


#ifdef NEW_GRAPHICS 

#include <nsp/object.h>
#include <nsp/figuredata.h> 
#include <nsp/figure.h>
#include <nsp/axes.h> 
#include <nsp/qcurve.h>

typedef struct _cscope_data cscope_data;

struct _cscope_data {
  int count ; 
  double tlast;
  NspFigure *Fig;
  NspList *L;
};

void scicos_cscope_block(scicos_block *block,int flag)
{
  char *str;
  BCG *Xgc;
  /* used to decode parameters by name */
  cscope_ipar *csi = (cscope_ipar*) block->ipar;
  cscope_rpar *csr = (cscope_rpar*) block->rpar;
  double t;
  int nu, cur = 0,k, wid;

  nu=Min(block->insz[0],8);
  t=scicos_get_time();

  wid = ( csi->wid == -1 ) ? 20000+ scicos_get_block_number() : csi->wid;
  
  if (flag == 2) 
    {
      cscope_data *D = (cscope_data *) (*block->work);
      k = D->count; 
      if (k > 0) {
	if (csr->dt > 0.) {
	  t = D->tlast + csr->dt;
	}
      }
      D->count++;
      D->tlast = t;
      /* nu here should be equal to csi->n */
      nsp_oscillo_add_point(D->L,t,block->inptr[0],nu);
      nsp_figure_force_redraw(D->Fig->obj);
    }
  else if (flag == 4) 
    {
      cscope_data *D;
      NspList *L;
      /* XXX :
       * buffer size for scope 
       * this should be set to the numebr of points to keep 
       * in order to cover a csr->per horizon 
       */
      int scopebs=1000;
      NspFigure *Fig =  nsp_oscillo_obj(wid,csi->n,csi->type,scopebs,TRUE,0,0,&L);
      if ( Fig == NULL ) 
	{
	  scicos_set_block_error(-16);
	  return;
	}
      if ((*block->work =  scicos_malloc(sizeof(cscope_data))) == NULL ) 
	{
	  scicos_set_block_error(-16);
	  return;
	}
      D = (cscope_data *) (*block->work);
      D->Fig = Fig;
      D->L = L;
      D->count = 0;
      D->tlast = t;
      Xgc = scicos_set_win(wid,&cur);
      if (csi->wpos[0] >= 0) {
	Xgc->graphic_engine->xset_windowpos(Xgc,csi->wpos[0],csi->wpos[1]);
      }
      if (csi->wdim[0] >= 0) {
	Xgc->graphic_engine->xset_windowdim(Xgc,csi->wdim[0],csi->wdim[1]);
      }
      str = block->label ;
      if ( str != NULL && strlen(str) != 0 && strcmp(str," ") != 0 ) 
	Xgc->graphic_engine->setpopupname(Xgc,str);
    } 
  else if (flag == 5) 
    {
      cscope_data *D = (cscope_data *) (*block->work);
      scicos_free(D);
      /* Xgc = scicos_set_win(wid,&cur); */
    }
}

#else 

void scicos_cscope_block(scicos_block *block,int flag)
{
  char *str;
  BCG *Xgc;
  /* used to decode parameters by name */
  cscope_ipar *csi = (cscope_ipar*) block->ipar;
  cscope_rpar *csr = (cscope_rpar*) block->rpar;
  double frect[4] = { 0.,0.,1.,1. };
  double t, *z__, c_b84 = 0.0;
  double tsave, rect[4];
  int nax[]={2,10,2,6};
  int c_n1 = -1, c__1 = 1;
  int nu, cur = 0, i__1,i, i__, k;
  int n1, n2,  wid;

  nu=Min(block->insz[0],8);
  t=scicos_get_time();

  wid = ( csi->wid == -1 ) ? 20000+scicos_get_block_number() : csi->wid;
  
  if (flag == 2) 
    {
      z__=*block->work; 
      --z__;
      k = (int) z__[1];
      if (k > 0) {
	n1 = (int) (z__[k + 1] / csr->per);
	if (z__[k + 1] < 0.) {
	  --n1;
	}
      } else {
	n1 = 0;
      }
    
      tsave = t;
      if (csr->dt > 0.) {
	t = z__[k + 1] + csr->dt;
      }
    
      n2 = (int) (t / csr->per);
      if (t < 0.) {
	--n2;
      }
    
      /*     add new point to the buffer */
      ++k;
      z__[k + 1] = t;
      for (i = 0; i < nu; ++i) {
	z__[csi->n + 1 + i * csi->n + k] = block->inptr[0][i];
      }
      z__[1] = (double) k;
      if (n1 == n2 && k < csi->n) {
	t = tsave;
	return ;
      }
      /*     plot 1:K points of the buffer */
      Xgc = scicos_set_win(wid,&cur);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      if (k > 0) {
	Xgc->graphic_engine->scale->xset_clipgrf(Xgc);
	for (i__ = 0; i__ < nu; ++i__) {
	  Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[csi->n + 2 + i__  *csi->n], &csi->type[i__], c__1, k);
	}
	Xgc->graphic_engine->scale->xset_unclip(Xgc);
      }
      /*     shift buffer left */
      z__[2] = z__[k + 1];
      for (i__ = 0; i__ < nu; ++i__) {
	z__[csi->n + 1 + i__ *csi->n + 1] = z__[csi->n + 1 + i__ * csi->n + k];
      }
      z__[1] = 1.;
      if (n1 != n2) {
	/*     clear window */
	Xgc->graphic_engine->clearwindow(Xgc);
	Xgc->graphic_engine->scale->xset_usecolor(Xgc,csi->color_flag);
	Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
	rect[0] = csr->per * (n1 + 1);
	rect[1] = csr->ymin;
	rect[2] = csr->per * (n1 + 2);
	rect[3] = csr->ymax;
	Xgc->graphic_engine->scale->xset_dash(Xgc,0);
	nsp_plot2d_old(Xgc,rect, &rect[1],&c__1, &c__1, &c_n1,"011","",0, rect, nax);
      }
      t = tsave;
    } 
  else if (flag == 4) 
    {
      /* the workspace is used as scope store buffer */
      if ((*block->work =  scicos_malloc(sizeof(double)*(1+csi->n*(1+nu))))== NULL ) 
	{
	  scicos_set_block_error(-16);
	  return;
	}
      z__= *block->work; 
      --z__;
      z__[1]=-1.0;
        
      n1 = (int) (t / csr->per);
      if (t <= 0.) {
	--n1;
      }
      Xgc = scicos_set_win(wid,&cur);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      if (csi->wpos[0] >= 0) {
	Xgc->graphic_engine->xset_windowpos(Xgc,csi->wpos[0],csi->wpos[1]);
      }
      if (csi->wdim[0] >= 0) {
	Xgc->graphic_engine->xset_windowdim(Xgc,csi->wdim[0],csi->wdim[1]);
      }
      rect[0] = csr->per * (n1 + 1);
      rect[1] = csr->ymin;
      rect[2] = csr->per * (n1 + 2);
      rect[3] = csr->ymax;
      Nsetscale2d_old(Xgc,frect,NULL,rect,"nn");
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,csi->color_flag);
      Xgc->graphic_engine->scale->xset_alufunction1(Xgc,3);
      Xgc->graphic_engine->clearwindow(Xgc);
      Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
      Xgc->graphic_engine->scale->xset_dash(Xgc,0);
      nsp_plot2d_old(Xgc,rect, &rect[1],&c__1, &c__1, &c_n1, "011","",0, rect, nax);
      str = block->label ;
      if ( str != NULL && strlen(str) != 0 && strcmp(str," ") != 0 ) 
	Xgc->graphic_engine->setpopupname(Xgc,str);
      z__[1] = 0.;
      z__[2] = t;
      i__1 = nu * csi->n;
      nsp_dset(&i__1, &c_b84, &z__[3], &c__1);
    } 
  else if (flag == 5) 
    {
      z__=*block->work; 
      --z__;
      k = (int) z__[1];
      if (k <= 1) {
	scicos_free(*block->work);
	return ;
      }
      Xgc = scicos_set_win(wid,&cur);
      Xgc->graphic_engine->scale->xset_clipgrf(Xgc);
      for (i__ = 0; i__ < nu; ++i__) {
	Xgc->graphic_engine->scale->drawpolylines(Xgc, &z__[2], &z__[csi->n + 2 + i__ * csi->n],&csi->type[i__], c__1, k);
      }
      Xgc->graphic_engine->scale->xset_unclip(Xgc);
      scicos_free(*block->work);
    }
}

#endif 


/* multiscope 
 * 
 * ipar = [win_num, number of subwindows (input ports),  buffer size,
 *         wpos(1),wpos(2) //  window position 
 *         wdim(1),wdim(2) // window dimension 
 *         ipar(8:7+ipar(2)) // input port sizes 
 *         ipar(8+ipar(2):7+ipar(2)+nu) // line type for ith curve 
 * rpar=[dt, per, ymin_1, ymax_1,...  ymin_k, ymax_k] 
 */

typedef struct _cmscope_ipar cmscope_ipar ;
struct _cmscope_ipar {
  int wid, nwid, n,wpos[2],wdim[2];};

typedef struct _cmscope_rpar cmscope_rpar ;
struct _cmscope_rpar {
  double dt,per,ymin,ymax;};

void scicos_cmscope_block(scicos_block *block,int flag)
{
#ifdef NEW_GRAPHICS 

#else 
  char *str;
  BCG *Xgc;
  /* used to decode parameters by name */
  cmscope_ipar *csi = (cmscope_ipar*) block->ipar;
  cmscope_rpar *csr = (cmscope_rpar*) block->rpar;
  int nax[]={2,10,2,6};
  double frect[]={ 0.0,0.0,1.0,1.0};
  int c_n1=-1, c__1 = 1,  c__0 = 0, *ipar, nipar,nu,  cur = 0;
  int i__1, kk,i,j,sum;
  int kwid, wid,  i__, k, n1,n2,it ;
  double rect[4], tsave, c_b103 = 0., t,  *z__,  *rpar;

  nu=block->insz[0];
  rpar=block->rpar;
  ipar=block->ipar;
  nipar=block->nipar;
  t=scicos_get_time();

  --ipar;
  --rpar;

  wid = ( csi->wid == -1 ) ? 20000+scicos_get_block_number() : csi->wid;


  if (flag == 2) {
    z__=*block->work; 
    --z__;
    k = (int) z__[1];
    if (k > 0) {
      n1 = (int) (z__[k + 1] / csr->per);
      if (z__[k + 1] < 0.) {
	--n1;
      }
    } else {
      n1 = 0;
    }

    tsave = t;
    if (csr->dt > 0.) {
      t = z__[k + 1] + csr->dt;
    }

    n2 = (int) (t / csr->per);
    if (t < 0.) {
      --n2;
    }
    
    /*     add new point to the buffer */
    ++k;
    z__[k + 1] = t;
    kk=0;
    for ( i=0 ; i<block->nin ; i++ ) 
      for (j = 0; j <block->insz[i] ; j++) {
	z__[csi->n + 1 + kk * csi->n + k] =block->inptr[i][j] ;
	++kk;
      }
    z__[1] = (double) k;
    if (n1 == n2 && k < csi->n) {
      t = tsave;
      return ;
    }
    /*     plot 1:K points of the buffer */
    Xgc = scicos_set_win(wid,&cur);
    Xgc->graphic_engine->scale->xset_usecolor(Xgc,TRUE);
    Xgc->graphic_engine->scale->xset_dash(Xgc,0);
    Xgc->graphic_engine->xset_recording(Xgc,TRUE);
    /*     loop on input ports */
    it=0;
    if (k > 0) {
      for (kwid = 1; kwid <= csi->nwid ; ++kwid) 
	{
	  int ilt = csi->nwid + 8 ; /* start of line type */
	  rect[0] = csr->per * n1;
	  rect[1] = rpar[(kwid << 1) + 1];
	  rect[2] = csr->per * (n1 + 1);
	  rect[3] = rpar[(kwid << 1) + 2];
	  frect[1] = (kwid - 1) * (1. / csi->nwid);
	  frect[3] = 1. / csi->nwid;
	  Nsetscale2d_old(Xgc,frect,NULL,rect,"nn");
	  Xgc->graphic_engine->scale->xset_clipgrf(Xgc);
	  /*     loop on input port elements */
	  for (i__ = 1; i__ <= ipar[kwid + 7] ; ++i__) {
	    Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[csi->n + 2 + it * csi->n], &ipar[ilt+it], c__1, k);
	    it++;
	  }
	  Xgc->graphic_engine->scale->xset_unclip(Xgc);
	}
    }
    /*     shift buffer left */
    z__[2] = z__[k + 1];
    sum=0;
    for (i=0;i<block->nin;++i){
      sum=sum+block->insz[i];
    }    
    for (i__ = 1; i__ <= sum; ++i__) {
      z__[csi->n + 1 + (i__ - 1) * csi->n + 1] = z__[csi->n + 1 + (i__ - 1) * csi->n + k];
    }
    z__[1] = 1.;
    if (n1 != n2) {
      /*     clear window */
      Xgc->graphic_engine->clearwindow(Xgc);
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,TRUE);
      Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
      Xgc->graphic_engine->scale->xset_dash(Xgc,0);
      for (kwid = 1; kwid <= csi->nwid ; ++kwid) 
	{
	  rect[0] = csr->per * (n1 + 1);
	  rect[1] = rpar[(kwid << 1) + 1];
	  rect[2] = csr->per * (n1 + 2);
	  rect[3] = rpar[(kwid << 1) + 2];
	  frect[1] = (kwid - 1) * (1. / csi->nwid);
	  frect[3] = 1. / csi->nwid;
	  Nsetscale2d_old(Xgc,frect,NULL,rect,"nn");
	  nsp_plot2d_old(Xgc,rect,&rect[1],&c__1, &c__1, &c_n1, "011","xlines",0, rect, nax);
	}
    }
    t = tsave;
  } 
  else if (flag == 4) 
    {
      sum=0;
      for ( i=0 ; i < block->nin ; ++i) sum=sum+block->insz[i];
      if ((*block->work=  scicos_malloc(sizeof(double)*(1+ipar[3]*(1+sum))))== NULL ) 
	{
	  scicos_set_block_error(-16);
	  return;
	}
      z__=*block->work; 
      --z__;
      z__[1]=-1.0;
      n1 = (int) (t / csr->per);
      if (t <= 0.) --n1;
      Xgc = scicos_set_win(wid,&cur);
      if (csi->wpos[0] >= 0) {
	Xgc->graphic_engine->xset_windowpos(Xgc,csi->wpos[0],csi->wpos[1]);
      }
      if (csi->wdim[0] >= 0) {
	Xgc->graphic_engine->xset_windowdim(Xgc,csi->wdim[0],csi->wdim[1]);
      }
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,TRUE);
      Xgc->graphic_engine->scale->xset_alufunction1(Xgc,3);
      Xgc->graphic_engine->clearwindow(Xgc);
      Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
      Xgc->graphic_engine->scale->xset_dash(Xgc,c__0); 
      str = block->label ;
      if ( str != NULL && strlen(str) != 0 && strcmp(str," ") != 0 ) 
	Xgc->graphic_engine->setpopupname(Xgc,str);
      for (kwid = 1; kwid <= csi->nwid ; ++kwid) 
	{
	  rect[0] = csr->per * (n1 + 1);
	  rect[1] = rpar[(kwid << 1) + 1];
	  rect[2] = csr->per * (n1 + 2);
	  rect[3] = rpar[(kwid << 1) + 2];
	  frect[1] = (kwid - 1) * (1. / csi->nwid);
	  frect[3] = 1. / csi->nwid;
	  Nsetscale2d_old(Xgc,frect,NULL,rect,"nn");
	  nsp_plot2d_old(Xgc,rect, &rect[1],&c__1, &c__1, &c_n1, "011","",0, rect, nax);
	}
      z__[1] = 0.;
      z__[2] = t;
      i__1 = sum * csi->n;
      nsp_dset(&i__1, &c_b103, &z__[3], &c__1);
    }
  else if (flag == 5) 
    {
      z__=*block->work; 
      --z__;
      k = (int) z__[1];
      if (k <= 1) {
	scicos_free(*block->work);
	return ;
      }
      Xgc = scicos_set_win(wid,&cur);
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,c__1);
      n1 =  (int) (z__[k + 1] / csr->per);
      /*     loop on input ports */
      it=0;
      for (kwid = 1; kwid <= csi->nwid ; ++kwid) 
	{
	  int ilt = csi->nwid + 8 ; /* start of line type */
	  rect[0] = csr->per * (n1 );
	  rect[1] = rpar[(kwid << 1) + 1];
	  rect[2] = csr->per * (n1 + 1);
	  rect[3] = rpar[(kwid << 1) + 2];
	  frect[0] = 0.;
	  frect[1] = (kwid - 1) * (1. /csi->nwid);
	  frect[2] = 1.;
	  frect[3] = 1. / csi->nwid;
	  Nsetscale2d_old(Xgc,frect,NULL,rect,"nn");
	  Xgc->graphic_engine->scale->xset_clipgrf(Xgc);
	  /*     loop on input port elements */
	  for (i__ = 1; i__ <=  ipar[kwid + 7]; ++i__) {
	    Xgc->graphic_engine->scale->drawpolylines(Xgc,&z__[2], &z__[csi->n + 2 + it * csi->n],&ipar[ilt+it], c__1,k-1);
	    it++;
	  }
	  Xgc->graphic_engine->scale->xset_unclip(Xgc);
	}
      scicos_free(*block->work);
    }
  #endif 

}
  

void scicos_scalar2vector_block(scicos_block *block,int flag)
{
  int i;
  if(flag==1){
    for(i=0;i<block->outsz[0];++i){
      block->outptr[0][i]=block->inptr[0][0];
    }
  }
}

void scicos_cstblk4_block(scicos_block *block,int flag)
{ 
  /*
   * Scicos block simulator
   * output a vector of constants out(i)=rpar(i)
   * rpar(1:nrpar) : given constants 
   */
  memcpy(block->outptr[0],block->rpar,block->nrpar*sizeof(double));    
}


void scicos_transmit_or_zero_block(scicos_block *block,int flag)
{
  int j;
  if(flag==1)
    {
      if( block->ipar[0] == 1 )
	for (j=0;j<block->insz[0];j++) 
	  {
	    block->outptr[0][j]=block->inptr[0][j];
	  }
    }
}


/* switch selected with ipar 
 *
 */

void scicos_mvswitch_block(scicos_block *block,int flag)
{
  int i,j=0;
  j=Min(Max(block->ipar[0],0),block->nin-1);
  for (i=0 ; i< block->insz[j]; i++) 
    {
      block->outptr[0][i]=block->inptr[j][i];
    }
}

/*  Copyright INRIA
 *    Scicos block simulator
 *   continuous state space linear system simulator
 *   rpar(1:nx*nx)=A
 *   rpar(nx*nx+1:nx*nx+nx*nu)=B
 *   rpar(nx*nx+nx*nu+1:nx*nx+nx*nu+nx*ny)=C
 *   rpar(nx*nx+nx*nu+nx*ny+1:nx*nx+nx*nu+nx*ny+ny*nu)=D 
 */

void scicos_csslti4_block(scicos_block *block,int flag)
{
  int un=1,lb,lc,ld;
  int nx=block->nx;
  double* x=block->x;
  double* xd=block->xd;
  double* rpar=block->rpar;
  double* y=block->outptr[0];
  double* u=block->inptr[0];
  int* outsz=block->outsz;
  int* insz=block->insz;
  
  lb=nx*nx;
  lc=lb+nx*insz[0];
  
  if (flag ==1 || flag ==6){
    /* y=c*x+d*u     */
    ld=lc+nx*outsz[0];
    if (nx==0) {
      dmmul_scicos(&rpar[ld],outsz,u,insz,y,outsz,outsz,insz,&un);
    }else{
      dmmul_scicos(&rpar[lc],outsz,x,&nx,y,outsz,outsz,&nx,&un);
      dmmul1_scicos(&rpar[ld],outsz,u,insz,y,outsz,outsz,insz,&un);
    }
  }

  else if (flag ==0){
    /* xd=a*x+b*u */
    dmmul_scicos(&rpar[0],&nx,x,&nx,xd,&nx,&nx,&nx,&un);
    dmmul1_scicos(&rpar[lb],&nx,u,insz,xd,&nx,&nx,insz,&un);
  }
}



static void * scicos_malloc(size_t size)
{
  return malloc(size);
}

static void scicos_free(void *p)
{
  free(p);
}
