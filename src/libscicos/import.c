#include <string.h>
#include "nsp/math.h"
#include "nsp/machine.h"
#include "nsp/object.h"

#include "scicos.h"

/* 
 * data structure selection 
 * Pointer to the beginning of the imported data 
 * nv,   size of the imported data 
 * type  type of the imported data 0:int,1:double 
 */

int  scicos_getscicosvars(int what, double **v, int *nv, int *type)
{
  int nblk;
  if ( Scicos->status == run_off ) 
    {
      *v= NULL;
      return(2); /* undefined import table scicos is not running */
    }
  nblk= Scicos->sim.nblk;
  /* imported from */
  switch (what) 
    {
    case 1 :			/* continuous state */
      *nv=(int) (Scicos->sim.xptr[nblk]-Scicos->sim.xptr[0]);
      *v=(void *)(Scicos->state.x);
      *type=1;
      break;
    case 2 :			/* continuous state splitting array*/
      *nv=(int)(nblk+1);
      *v=(void *) (Scicos->sim.xptr);
      *type=0;
      break;
    case 3 :			/* continuous state splitting array*/
      *nv=(int)(nblk+1);
      *v=(void *) (Scicos->sim.zcptr);
      *type=0;
      break;
    case 4 :			/* discrete state */
      *nv=(int)(Scicos->sim.zptr[nblk]-Scicos->sim.zptr[0]);
      *v=(void *) (Scicos->state.z);
      *type=1;
      break;
    case 5 :			/* discrete  state splitting array*/
      *nv=(int)(nblk+1);
      *v=(void *) (Scicos->sim.zptr);
      *type=0;
      break;
    case 6 :			/* rpar */
      *nv=(int)(Scicos->sim.rpptr[nblk]-Scicos->sim.rpptr[0]);
      *v=(void *) (Scicos->sim.rpar);
      *type=1;
      break;
    case 7 :			/* rpar  splitting array*/
      *nv=(int)(nblk+1);
      *v=(void *) (Scicos->sim.rpptr);
      *type=0;
      break;
    case 8 :			/* ipar */
      *nv=(int)(Scicos->sim.ipptr[nblk]-Scicos->sim.ipptr[0]);
      *v=(void *) (Scicos->sim.ipar);
      *type=0;
      break;
    case 9 :			/* ipar  splitting array*/
      *nv=(int)(nblk+1);
      *v=(void *) (Scicos->sim.ipptr);
      *type=0;
      break;
    case 10 :			/* outtb */
      *nv=(int)(Scicos->state.nout);
      *v=(void *) (Scicos->state.outtb);
      *type=1;
      break;
    case 11 :                   /* inpptr */
      *nv=(int)(nblk+1);
      *v=(void *) (Scicos->sim.inpptr); 
      *type=0;
      break;
    case 12 :                   /* outptr */
      *nv=(int)(nblk+1);
      *v=(void *) (Scicos->sim.outptr); 
      *type=0;
      break;
    case 13 :                   /* inplnk */
      *nv=(int)(Scicos->sim.inpptr[nblk]-Scicos->sim.inpptr[0]); 
      *v=(void *) (Scicos->sim.inplnk); 
      *type=0;
      break;
    case 14 :                   /* outlnk */
      *nv=(int)(Scicos->sim.outptr[nblk]-Scicos->sim.outptr[0]); 
      *v=(void *) (Scicos->sim.outlnk); 
      *type=0;
      break;
    case 15 :                   /* lnkptr */
      *nv=(int)(Scicos->sim.nlnkptr);
      *v=(void *) (Scicos->sim.lnkptr); 
      *type=0;
      break;
    }
  return(0);
}


char *scicos_getlabel(int kf)
{
  return Scicos->Blocks[kf-1].label;
}

int scicos_get_block_by_label(const char *label) 
{
  int nblk = Scicos->sim.nblk,k;
  for ( k=0 ; k < nblk ; k++) 
    {
      if ( strcmp(Scicos->Blocks[k].label,label)==0) 
	return k+1;
    }
  return 0;
}

int scicos_getscilabel(int kfun,char **label)
{
  if ( Scicos->status == run_off ) return FAIL;
  *label = Scicos->Blocks[kfun-1].label;
  return OK;
}

int scicos_getcurblock(void)
{
  return Scicos->params.curblk ;
}


void scicos_getouttb(int nsize,int *nvec, double *outtc)
{
  int i;
  if ( Scicos->state.nout == 0 ) 
    for (i=0 ; i < nsize ; i++)
      outtc[i]= 0.0;
  else 
    for (i=0 ; i < nsize ; i++)
      outtc[i]=  (double)Scicos->state.outtb[nvec[i]-1];  
}


void scicos_send_halt(void)
{
  Scicos->params.halt = 1;
}
