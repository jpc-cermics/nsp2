/* Nsp
 * Copyright (C) 2005 Jean-Philippe Chancelier Enpc/Cermics
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
 * scicos objects used for simulation 
 *--------------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h> 

#include "nsp/machine.h"
#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"
#include "scicos/scicos.h"
#include "scicos/simul.h"

static void scicos_clear_state(scicos_state *scst);
static void scicos_clear_sim(scicos_sim *scsim);
static void scicos_clear_blocks(scicos_block *Blocks,int nblk);
static int scicos_fill_state(NspHash *State,scicos_state *scst);
static int scicos_fill_sim(NspHash *Sim,scicos_sim *scsim);

/*
 * fill a scicos_state structure 
 * with pointers from the Hash table State 
 * 
 */

static int scicos_fill_state(NspHash *State,scicos_state *scst)
{
  int i;
  void **loc= (void **) scst;
  const int nstate = 7;
  char *state[] = {"x","z","iz","tevts","evtspt","pointi","outtb"};
  if ( loc + nstate -1 != (void *) &scst->outtb )
    {
      Scierror("Error: internal error in scicos_fill_state !!\n");
      return FAIL;
    }
  scst->State = State;
  for ( i = 0 ; i < nstate ; i++ ) 
    {
      NspObject *obj;
      if ( nsp_hash_find(State,state[i],&obj) == FAIL) return FAIL;
      scst->State_elts[i]= obj;
      if ( IsMat(scst->State_elts[i]) == FALSE ) return FAIL;
      if ( ((NspMatrix *) scst->State_elts[i])->rc_type != 'r') 
	{
	  Scierror("Elements are supposed to be real matrix \n");
	  return FAIL;
	}
      /* put the pointer in the struct */
      loc[i]= (void *) ((NspMatrix *)scst->State_elts[i])->R;
    }
  /* in place conversion */
  scst->State_elts[4]=  Mat2int((NspMatrix *) scst->State_elts[4]);/* evtspt */
  scst->State_elts[5]=  Mat2int((NspMatrix *) scst->State_elts[5]);/* pointi */
  /* constants */
  scst->nevts =  ((NspMatrix *) scst->State_elts[4])->m;/* evtspt */
  scst->nout =   ((NspMatrix *) scst->State_elts[6])->m; /* outtb */

  /* extra allocations */
  if((scst->iwa=malloc(sizeof(int)*(scst->nevts)))== NULL )
    {
      Scierror("Error: running out of memory in state allocation\n");
      scicos_clear_state(scst);
      return FAIL;
    }
  return OK;
}  

/*
 * clear extra allocated variables and 
 * restore the data to their original state 
 */

static void scicos_clear_state(scicos_state *scst)
{
  FREE(scst->iwa);
  Mat2double((NspMatrix *) scst->State_elts[4]);
  Mat2double((NspMatrix *) scst->State_elts[5]);
}

/* get a copy of the state NspHash *State 
 * variable (this is useful during simulation) 
 * to debug. 
 */

NspHash *scicos_get_state_copy(scicos_state *scst)
{
  return nsp_hash_copy((NspHash *) scst->State);
}

/*
 * fill a scicos_sim structure 
 * with pointers from the Hash table Sim 
 * XXXXX   xd=&x[xptr[nblk]-1];
 * 
 */

static int scicos_fill_sim(NspHash *Sim,scicos_sim *scsim)
{
  const int convert[]={1,2,3,4,5,6,7,8,10,11,12,13,14,16,17,18,19,20,21,22,23,24,25,26,27,29,-1};
  const int nsim = 30;
  int i,count;
  void **loc= (void **) scsim;
  NspList *funs;
  Cell *cloc;

  char *sim[]={"funs","xptr","zptr","zcptr","inpptr",
	       "outptr","inplnk","outlnk","lnkptr","rpar",
	       "rpptr","ipar","ipptr","clkptr", "ordptr",
	       "execlk","ordclk","cord","oord","zord",
	       "critev","nb","ztyp","nblk","ndcblk", 
	       "subscr","funtyp","iord","labels","modptr"};

  if ( loc + nsim -1 != (void *) &scsim->modptr )
    {
      Scierror("Error: internal error in scicos_fill_state !!\n");
      return FAIL;
    }
  /* get everything as if it was Matrices */

  for ( i = 0 ; i < nsim ; i++ ) 
    {
      NspObject *obj;
      if (nsp_hash_find(Sim,sim[i],&obj) == FAIL) return FAIL;
      scsim->Sim_elts[i]= obj;
      if ( loc+i != (void *)&scsim->funs && loc+i != (void *) &scsim->labels )
	loc[i]= (void *)  ((NspMatrix *) scsim->Sim_elts[i])->R;      
    }

  scsim->Sim = Sim;
  /* convert to int in place */
  i=0;
  while (1)
    {
      int j;
      if ((j=convert[i])==-1) break;
      scsim->Sim_elts[j]= Mat2int( scsim->Sim_elts[j]); 
      i++;
    }
  /* take care of labels and funs */
  funs = scsim->funs = scsim->Sim_elts[0];
  scsim->labels = ((NspSMatrix *) scsim->Sim_elts[28])->S; /* labels */
  /* initialize to NULL in case of fail */
  scsim->mod = NULL;
  scsim->funflag = NULL;
  scsim->funptr = NULL;
  /* size of funptr */
  scsim->nblk =  *scsim->nblkptr; 
  if (( scsim->funflag = malloc(scsim->nblk*sizeof(int)))== NULL) goto fail;
  if (( scsim->funptr = malloc(scsim->nblk*sizeof(void *)))== NULL) goto fail;
  /*
   * scsim->Sim_elts[0] is a list of chars or functions 
   */
  funs = (NspList *) scsim->Sim_elts[0];
  cloc = funs->first ;
  count = 0;
  while ( cloc != NULLCELL) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  if ( count >= scsim->nblk ) 
	    {
	      Scierror("Error: funs lenght should be %d\n",scsim->nblk);
	      goto fail;
	    }
	  if ( IsString(cloc->O)) 
	    {
	      void *fptr = scicos_get_function(((NspSMatrix *) cloc->O)->S[0]);
	      if ( fptr != NULL) 
		{
		  /* a hard code function given by its adress */
		  scsim->funflag[count]= fun_pointer;
		  scsim->funptr[count]= fptr;
		}
	      else 
		{
		  /* a macros given ny its name */
		  scsim->funflag[count]= fun_macro_name;
		  scsim->funptr[count]= ((NspSMatrix *) cloc->O)->S[0];
		}
	    }
	  else if ( IsNspPList(cloc->O) )
	    {
	      /* a macro given by a pointer to its code */
	      scsim->funflag[count]= fun_macros;
	      scsim->funptr[count]=cloc->O;
	    }
	  else 
	    {
	      Scierror("Error: funs should contain strings or macros\n");
	      goto fail;
	    }
	}
      count++;
      cloc = cloc->next;
    }
  /* a set of constants */
  scsim->nlnkptr =  ((NspMatrix *) scsim->Sim_elts[8])->m; /* lnkptr */
  scsim->nordptr= ((NspMatrix *) scsim->Sim_elts[14])->mn; /* ordptr  */
  scsim->ncord = ((NspMatrix *) scsim->Sim_elts[17])->m; /* cord */
  scsim->niord = ((NspMatrix *) scsim->Sim_elts[27])->m; /* iord */
  scsim->noord = ((NspMatrix *) scsim->Sim_elts[18])->m; /* oord */
  scsim->nzord = ((NspMatrix *) scsim->Sim_elts[19])->m; /* zord */
  /* scsim->nblk =  *scsim->nblkptr; already done */
  scsim->ndcblk = *scsim->ndcblkptr;
  scsim->nsubs = ((NspMatrix *)scsim->Sim_elts[25])->m; /* subscr */

  scsim->nmod = scsim->modptr[scsim->nblk] - 1;
  scsim->nordclk=scsim->ordptr[-1+scsim->nordptr]-1;
  /*     computes number of zero crossing surfaces */
  scsim->ng = scsim->zcptr[scsim->nblk] - 1;
  /*     number of  discrete real states */
  scsim->nz = scsim->zptr[scsim->nblk] - 1;
  /*     number of continuous states */
  scsim->nx = scsim->xptr[scsim->nblk] - 1;

  scsim->debug_block=-1; /* no debug block for start */

  /* extra arguments allocated here */
  if( scsim->nmod > 0 )
    {
      if(( scsim->mod=malloc(sizeof(int)*scsim->nmod))== NULL )
	{
	  scicos_clear_sim(scsim);
	  Scierror("Error: running out of memory in block allocations\n");
	  goto fail;
	}
    }

  return OK;
 fail:
  scicos_clear_sim(scsim);
  return FAIL;
}  

/*
 * clear extra allocated variables and 
 * restore the data to their original state 
 */

static void scicos_clear_sim(scicos_sim *scsim)
{
  int i;
  const int convert[]={1,2,3,4,5,6,7,8,10,11,12,13,14,16,17,18,19,20,21,22,23,24,25,26,27,29,-1};
  FREE(scsim->funflag);
  FREE(scsim->funptr);
  FREE(scsim->mod);
  i=0;
  while (1)
    {
      int j;
      if ((j=convert[i])==-1) break;
      Mat2double( scsim->Sim_elts[j]); 
      i++;
    }
}  


/* get a copy of the state NspHash *State 
 * variable (this is useful during simulation) 
 * to debug. 
 */

NspHash *scicos_get_sim_copy(scicos_sim *scsim)
{
  return  nsp_hash_copy((NspHash *) scsim->Sim);
}


/*
 * creates and fills an array of Blocks.
 */


static void *scicos_fill_blocks(scicos_sim *scsim,scicos_state *scst)
{
  int kf,in,out,mtag;
  scicos_block *Blocks; 

  if((Blocks=calloc(scsim->nblk,sizeof(scicos_block)))== NULL )
    {
      Scierror("Error: running out of memory in block allocations\n");
      return NULL;
    }

  for ( kf = 0; kf < scsim->nblk; ++kf) 
    {
      int b_type = scsim->funtyp[kf];
      Blocks[kf].type = (b_type < 10000) ? ( b_type % 1000) : b_type % 1000 + 10000;

      if ( scsim->funflag[kf] == fun_pointer ) 
	{
	  Blocks[kf].scsptr= NULL;
	  Blocks[kf].funpt= scsim->funptr[kf];
	  Blocks[kf].scsptr_flag = scsim->funflag[kf];
	}
      else 
	{
	  /* a NspObject containing a macro or a macro name */
	  Blocks[kf].scsptr=scsim->funptr[kf];
	  Blocks[kf].scsptr_flag = scsim->funflag[kf];
	  /* the function is a macro or it is a Debug block*/
	  switch (scsim->funtyp[kf])
	    {
	    case 0:
	      Blocks[kf].funpt=scicos_sciblk;
	      break;
	    case 1:
	    case 2: 
	      Scierror("Error: block %d, type %d function not allowed for scilab blocks\n",
		       kf+1,scsim->funtyp[kf]);
	      scicos_clear_blocks(Blocks,kf+1);
	      return NULL;
	    case 3:
	      Blocks[kf].funpt=scicos_sciblk2;
	      Blocks[kf].type=2;
	      break;
	    case 5:
	      Blocks[kf].funpt=scicos_sciblk4;
	      Blocks[kf].type=4;
	      break;
	    case 99: /* debugging block */
	      Blocks[kf].funpt=scicos_sciblk4;
	      Blocks[kf].type=4;
	      scsim->debug_block=kf;
	      break;
	    case 10005:
	      Blocks[kf].funpt=scicos_sciblk4;
	      Blocks[kf].type=10004;
	      break;
	    default :
	      Scierror("Error:block %d, Undefined Function type %d\n",kf+1,scsim->funtyp[kf]);
	      scicos_clear_blocks(Blocks,kf+1);
	      return NULL;
	    }
	}
      Blocks[kf].ztyp=scsim->ztyp[kf];
      Blocks[kf].nx=scsim->xptr[kf+1]-scsim->xptr[kf];
      Blocks[kf].ng=scsim->zcptr[kf+1]-scsim->zcptr[kf];
      Blocks[kf].nz=scsim->zptr[kf+1]-scsim->zptr[kf];
      Blocks[kf].nrpar=scsim->rpptr[kf+1]-scsim->rpptr[kf];
      Blocks[kf].nipar=scsim->ipptr[kf+1]-scsim->ipptr[kf];
      Blocks[kf].nin=scsim->inpptr[kf+1]-scsim->inpptr[kf]; /* number of input ports */
      Blocks[kf].nout=scsim->outptr[kf+1]-scsim->outptr[kf];/* number of output ports */
      Blocks[kf].nevout=scsim->clkptr[kf+1] - scsim->clkptr[kf];
      Blocks[kf].label  = scsim->labels[kf];

      Blocks[kf].insz=malloc(sizeof(int)*Blocks[kf].nin); 
      mtag = ( Blocks[kf].insz == NULL);
      Blocks[kf].inptr=malloc(sizeof(double*)*Blocks[kf].nin);
      mtag = mtag || ( Blocks[kf].inptr == NULL);
      Blocks[kf].outsz=malloc(sizeof(int)*Blocks[kf].nout);
      mtag = mtag || ( Blocks[kf].outsz == NULL);
      Blocks[kf].outptr=malloc(sizeof(double*)*Blocks[kf].nout);
      mtag = mtag || ( Blocks[kf].outptr == NULL);
      Blocks[kf].evout=calloc(Blocks[kf].nevout,sizeof(double));
      mtag = mtag || ( Blocks[kf].evout == NULL);
      Blocks[kf].res=malloc(sizeof(double)*Blocks[kf].nx);
      mtag = mtag || ( Blocks[kf].res == NULL);
      Blocks[kf].jroot_init = Blocks[kf].jroot=calloc(Blocks[kf].ng,sizeof(int));
      mtag = mtag || ( Blocks[kf].jroot == NULL);

      if ( mtag ) 
	{
	  scicos_clear_blocks(Blocks,kf+1);
	  Scierror("Error: running out of memory in block allocations\n");
	  return NULL;
      }

      for(in=0;in<Blocks[kf].nin;in++) {
	int lprt=scsim->inplnk[-1+scsim->inpptr[kf]+in];
	Blocks[kf].inptr[in]=&(scst->outtb[scsim->lnkptr[-1+lprt]-1]);
	Blocks[kf].insz[in]=scsim->lnkptr[lprt]-scsim->lnkptr[-1+lprt];
      }

      for(out=0;out<Blocks[kf].nout;out++) {
	int lprt=scsim->outlnk[-1+scsim->outptr[kf]+out];
	Blocks[kf].outptr[out]=&(scst->outtb[scsim->lnkptr[-1+lprt]-1]);
	Blocks[kf].outsz[out]=scsim->lnkptr[lprt]-scsim->lnkptr[-1+lprt];
      }

      Blocks[kf].z=&(scst->z[scsim->zptr[kf]-1]);
      Blocks[kf].rpar=&(scsim->rpar[scsim->rpptr[kf]-1]);
      Blocks[kf].ipar=&(scsim->ipar[scsim->ipptr[kf]-1]);
      Blocks[kf].work= (void **)(scst->iz+kf);
      Blocks[kf].nmode=scsim->modptr[kf+1]-scsim->modptr[kf]; 
      if ( Blocks[kf].nmode!=0){
	Blocks[kf].mode=&(scsim->mod[scsim->modptr[kf]-1]);
      }
    }
  return Blocks;
}

static void scicos_clear_blocks(scicos_block *Blocks,int nblk)
{
  int kf;
  for ( kf = 0; kf < nblk; ++kf) 
    {
      FREE(Blocks[kf].insz);
      FREE(Blocks[kf].inptr);
      FREE(Blocks[kf].outsz);
      FREE(Blocks[kf].outptr);
      FREE(Blocks[kf].evout);
      FREE(Blocks[kf].res);
      FREE(Blocks[kf].jroot_init);
    }
  FREE(Blocks);
}


int scicos_fill_run(scicos_run *sr,NspHash *Sim,NspHash *State)
{
  if ( scicos_fill_state(State,&sr->state) == FAIL) return FAIL;
  if ( scicos_fill_sim(Sim,&sr->sim)==FAIL) 
    {
      scicos_clear_state(&sr->state);
      return FAIL;
    }
  if ( ( sr->Blocks=scicos_fill_blocks(&sr->sim,&sr->state)) == NULL) 
    {
      scicos_clear_state(&sr->state);
      scicos_clear_sim(&sr->sim);
      return FAIL;
    }
  sr->status = run_on ;
  return OK;
}

void scicos_clear_run(scicos_run *sr)
{
  scicos_clear_state(&sr->state);
  scicos_clear_sim(&sr->sim);
  scicos_clear_blocks(sr->Blocks,sr->sim.nblk);
  sr->status = run_off;
}
