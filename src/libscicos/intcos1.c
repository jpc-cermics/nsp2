/*------------------------------------------------------------------
 * Copyright ENPC 2004
 * Jean-Philippe Chancelier Enpc/Cermics
 * jpc@cermics.enpc.fr 
 *------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h> 

#include "nsp/machine.h"
#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"

/* 
 * [state,t]=scicosim(state,tcur,tf,sim,'start' ,tol) 
 * 
 * sim=tlist(['scs','funs','xptr','zptr','izptr','inpptr','outptr',..
 *            'inplnk','outlnk','lnkptr','rpar','rpptr','ipar','ipptr',..
 *            'clkptr','ordptr','execlk','ordclk','cord','oord','zord',..      
 *            'critev','nb','ztyp','nblk','ndcblk','subscr','funtyp',..
 *            'iord','labels'],....) 
 *
 * state=tlist(['xcs','x','z','iz','tevts','evtspt','pointi','outtb'],..
 *               x,z,iz,tevts,evtspt,pointi,outtb)
 */

int int_scicos(Stack stack, int rhs, int opt, int lhs) 
{
  double tcur,tf;
  int i,nout;
  static char *action_name[]={ "finish","linear", "run", "start", NULL };
  const int nstate = 7, nsim = 30;
  NspHash *State, *Sim;
  NspObject * State_elts[nstate], * Sim_elts[nsim];

  const char *sim[]={"scs","funs","xptr","zptr","izptr","inpptr","outptr",
		     "inplnk","outlnk","lnkptr","rpar","rpptr","ipar","ipptr",
		     "clkptr","ordptr","execlk","ordclk","cord","oord","zord",      
		     "critev","nb","ztyp","nblk","ndcblk","subscr","funtyp",
		     "iord","labels"};
  char *state[] = {"x","z","iz","tevts","evtspt","pointi","outtb"};
  const int start=28,run=27,finish=15,linear=21;
  double simpar[7];
  CheckRhs(6,6);
  CheckLhs(1,2);
  /* first variable : the state */
  if ((State = GetHash(stack,1)) == NULLHASH) return RET_BUG;
  for ( i = 0 ; i < nstate ; i++ ) 
    {
      if ( nsp_hash_find(State,state[i],&State_elts[i]) == FAIL) return RET_BUG;
      if ( IsMat(State_elts[i]) == FAIL) return RET_BUG;
      if ( ((NspMatrix *)State_elts[i])->rc_type != 'r') 
	{
	  Scierror("Elements are supposed to be real matrix \n");
	  return RET_BUG;
	}
    }
  nout = ((NspMatrix *) State_elts[6])->m;
  State_elts[4]=(NspObject *)  Mat2int((NspMatrix *) State_elts[4]);
  /* 
     c 1e2     --   subvariable x0(state) --
     c 1e3     --   subvariable z0(state) --
     c 1e4     --   subvariable work(state) --
     c 1e5     --   subvariable tevts(state) --
     c 1e6     --   subvariable evtspt(state) --
     c 1e7     --   subvariable pointi(state) --
     c 1e8     --   subvariable outtb(state) --
  */
  if (GetScalarDouble(stack,2,&tcur) == FAIL) return RET_BUG;
  if (GetScalarDouble(stack,3,&tf) == FAIL) return RET_BUG;
  /*  checking variable sim (number 4) */
  if ((Sim = GetHash(stack,4)) == NULLHASH) return RET_BUG;
  for ( i = 0 ; i < nsim ; i++ ) 
    {
      if (nsp_hash_find(Sim,sim[i],&Sim_elts[i]) == FAIL) return NULLOBJ ;
    }

  /* 4ex */
  /* 
  c 4e2  --   subvariable funptr(sim) --
        liste 
        nblk =istk(il4e2+1)
  c 4e3     --   subvariable xptr(sim) --
  c 4e4     --   subvariable zptr(sim) --
  c 4e5     --   subvariable zcptr (sim) 
  c 4e6     --   subvariable inpptr(sim) --
        ninpptr = ->m
  c 4e7    --   subvariable outptr(sim) --
        noutptr = ->m 
  c 4e8    --   subvariable inplnk(sim) --
        ninplnk = ->m 
  c 4e9    --   subvariable outlnk(sim) --
        noutlnk = ->m
  c 4e10     --   subvariable lnkptr(sim) --
        nlnkptr = ->m
  c 4e11     --   subvariable rpar(sim) --
        nrpar = n4e11
  c 4e12       --   subvariable rpptr(sim) --
  c 4e13      --   subvariable ipar(sim) --
        nipar = n4e13
  c  4e14     --   subvariable ipptr(sim) --
  c  4e15     --   subvariable clkptr(sim) --
  c  4e16     --   subvariable ordptr(sim) --
  c       --   subvariable execlk(sim) --
  c       --   subvariable ordclk(sim) --
  c       --   subvariable cord(sim) --
  c       --   subvariable oord(sim) --
  c       --   subvariable zord(sim) --
  c       --   subvariable critev(sim) --
  c 4e23      --   subvariable nb(sim) --
        il4e23=iadr(l4+istk(il4+24)-1)
        l4e23 = sadr(il4e23+4)
        if (stk(l4e23) .ne. nblk) then
          call error(42)
          return
       endif
 c 4e24  --   subvariable ztyp(sim) --
        il4e24=iadr(l4+istk(il4+25)-1)
        n4e24 = istk(il4e24+1)
        l4e24 = sadr(il4e24+4)
        lztyp=l4e24
c      
c  4e25     --   subvariable ndblk(sim) --
        il4e25=iadr(l4+istk(il4+26)-1)
        l4e25 = sadr(il4e25+4)
        ndblk=stk(l4e25)
c      
c       --   subvariable ndcblk(sim) --
        il4e26=iadr(l4+istk(il4+27)-1)
        l4e26 = sadr(il4e26+4)
c      
c       --   subvariable subscr(sim) --
        n4e27 = istk(il4e27+1)
        l4e27 = sadr(il4e27+4)
c      
c       --   subvariable funtyp(sim) --
        n4e28 = istk(il4e28+1)
        l4e28 = sadr(il4e28+4)
c      
c       --   subvariable iord(sim) --
        n4e29 = istk(il4e29+1)
        m4e29=istk(il4e29+2)
        niord=n4e29
        l4e29 = sadr(il4e29+4)
c
c       --   subvariable labels(sim) --
        if(n4.ge.30) then
           il4e30=iadr(l4+istk(il4+31)-1)
           n4e30 = istk(il4e30+1)*istk(il4e30+2)
           labptr=il4e30+4
           llab=labptr+n4e30+1
           nlab=istk(llab-1)-1
        else
           n4e30=nblk+1
           labptr=iadr(lw)
           llab=labptr+n4e30
           err=sadr(llab+1)-lstk(bot)
           if(err.gt.0) then
              call error(17)
              return
           endif
           nlab=1
           call iset(n4e30,1,istk(labptr),1)
        endif
c       --   subvariable modptr(sim) --
        n4e31 = istk(il4e31+1)
        l4e31 = sadr(il4e31+4)
 */


  if ((rep= GetStringInArray(stack,5,action_name,1)) == -1) return RET_BUG; 
  switch (rep ) 
    {
    case 0: flag=3;break;
    case 1: flag=4;break;
    case 2: flag=2;break;
    case 3: flag=1;break;
    }
  if ((simpar = GetRealMat(stack,6)) == NULLMAT) return RET_BUG;
  /*      [atol  rtol ttol, deltat, scale, impl, hmax] */
  /*
    il6 = iadr(lstk(top-rhs+6))
    if (istk(il6) .ne. 1) then
    err = 6
    call error(53)
    return
    endif
    m6 = istk(il6+1)*istk(il6+2)
    if (m6 .ne. 5.and. m6.ne.4.and. m6.ne.6.and. m6.ne.7) then
    err = 6
    call error(89)
    return
    endif
    l6 = sadr(il6+4)
    endif
    c  
 */
  /*
        if (n1e5 .ne. n1e6) then
          call error(42)
          return
        endif
        if (nSim_elts[1] .ne. nSim_elts[2]) then
          call error(42)
          return
        endif
        if (nSim_elts[1] .ne. nSim_elts[3]) then
          call error(42)
          return
        endif
        if (nSim_elts[1] .ne. nSim_elts[4]) then
          call error(42)
          return
        endif
        if (nSim_elts[1] .ne. nSim_elts[5]) then
          call error(42)
          return
        endif
        if (nSim_elts[1] .ne. nSim_elts[10]) then
          call error(42)
          return
        endif
        if (nSim_elts[1] .ne. nSim_elts[12]) then
          call error(42)
          return
        endif
        if (nSim_elts[1] .ne. nSim_elts[13]) then
          call error(42)
          return
        endif
c        if (mSim_elts[14].ne.2.and.n4e16.ne.0) then
c          call error(42)
c          return
c        endif
c        if (mSim_elts[15].ne.2.and.n4e17.ne.0) then
c          call error(42)
c          return
c        endif
        if (mSim_elts[16].ne.2.and.n4e18.ne.0) then
          call error(42)
          return
        endif
        if (mSim_elts[17].ne.2.and.n4e19.ne.0) then
          call error(42)
          return
        endif
        if (mSim_elts[0]0.ne.2.and.nSim_elts[18].ne.0) then
          call error(42)
          return
        endif
        if (mSim_elts[0]1.ne.2.and.nSim_elts[19].ne.0) then
          call error(42)
          return
        endif
        if (mSim_elts[0]9.ne.2.and.nSim_elts[27].ne.0) then
          call error(42)
          return
        endif

        call entier(nSim_elts[1],stk(l4e3),istk(iadr(l4e3)))
        call entier(nSim_elts[2],stk(l4e4),istk(iadr(l4e4)))
        call entier(nSim_elts[3],stk(l4e5),istk(iadr(l4e5)))
        call entier(n1e6,stk(l1e6),istk(iadr(l1e6)))
        call entier(nSim_elts[4],stk(l4e6),istk(iadr(l4e6)))
        call entier(nSim_elts[5],stk(l4e7),istk(iadr(l4e7)))
        call entier(nSim_elts[6],stk(l4e8),istk(iadr(l4e8)))
        call entier(nSim_elts[7],stk(l4e9),istk(iadr(l4e9)))
        call entier(nSim_elts[8],stk(l4e10),istk(iadr(l4e10)))
        call entier(nSim_elts[10],stk(l4e12),istk(iadr(l4e12)))
        call entier(nSim_elts[11],stk(l4e13),istk(iadr(l4e13)))
        call entier(nSim_elts[12],stk(l4e14),istk(iadr(l4e14)))
        call entier(nSim_elts[13],stk(l4e15),istk(iadr(l4e15)))
        call entier(nSim_elts[14],stk(l4e16),istk(iadr(l4e16)))
        call entier(nSim_elts[1]1,stk(lSim_elts[29]),istk(iadr(l4e31)))
        call entier(nSim_elts[16]*m4e18,stk(l4e18),istk(iadr(l4e18)))
        call entier(nSim_elts[17]*m4e19,stk(l4e19),istk(iadr(l4e19)))
        call entier(nSim_elts[0]0*mSim_elts[18],stk(l4e20),istk(iadr(l4e20)))
        call entier(nSim_elts[0]1*mSim_elts[19],stk(l4e21),istk(iadr(l4e21)))
        call entier(nSim_elts[0]2,stk(lSim_elts[20]),istk(iadr(l4e22)))
        call entier(1,stk(lSim_elts[0]3),istk(iadr(lSim_elts[21])))
        call entier(nSim_elts[0]4,stk(lSim_elts[22]),istk(iadr(l4e24)))
        ilztyp=iadr(lSim_elts[0]4)
        call entier(1,stk(lSim_elts[0]5),istk(iadr(lSim_elts[23])))
        call entier(1,stk(lSim_elts[0]6),istk(iadr(lSim_elts[24])))
        call entier(nSim_elts[0]7,stk(lSim_elts[25]),istk(iadr(l4e27)))
        call entier(nSim_elts[0]8,stk(lSim_elts[26]),istk(iadr(l4e28)))
        ilfuntyp=iadr(lSim_elts[0]8)
        call entier(nSim_elts[0]9*mSim_elts[27],stk(l4e29),istk(iadr(l4e29)))
c
        if(m6.eq.4) then
           call dcopy(4,stk(l6),1,simpar,1)
           simpar(5)=  0.d0
           simpar(6)=  0.d0
           simpar(7)=  0.d0
        elseif(m6.eq.5) then
           call dcopy(5,stk(l6),1,simpar,1)
           simpar(6)=  0.d0
           simpar(7)=  0.d0
        elseif(m6.eq.6) then
           call dcopy(6,stk(l6),1,simpar,1)
           simpar(7)=  0.d0
        else
           call dcopy(7,stk(l6),1,simpar,1)
        endif
        solver=simpar(6)
c
        lfunpt=iadr(lw)
        lw=sadr(lfunpt+nblk)
c
        linpptr=iadr(lSim_elts[4])
        llnkptr=iadr(lSim_elts[8])
        iloutptr=iadr(lSim_elts[5])
c

c
c        ng=istk(iadr(lSim_elts[3])+n4e5-1)-1

        ilinp=iadr(lSim_elts[2])
        err=lw-lstk(bot)
        if (err .gt. 0) then
          call error(17)
          return
        endif
c
c     lock working area
        lstk(top+1)=lw
c     Set function table for blocks
        lf=lSim_elts[0]
        do 10 i=1,nblk
           ilf=iadr(lf)
           if(istk(ilf).eq.11.or.istk(ilf).eq.13) then
C     Block is defined by a scilab function given in the structure
              istk(lfunpt-1+i)=-lf
           elseif(istk(ilf).eq.10) then
              buf=' '
              nn=istk(ilf+5)-1
              call cvstr(nn,istk(ilf+6),buf,1)
              buf(nn+1:nn+1)=char(0)
              ifun=funnum(buf(1:nn+1))
              if (ifun.gt.0) then
C     Block is defined by a C or Fortran procedure
                 istk(lfunpt-1+i)=ifun
              else
C     Block is defined by a predefined scilab function 
                 call namstr(id,istk(ilf+6),nn,0)
                 fin=0
                 call funs(id)
                 if (fun.eq.-1.or.fun.eq.-2) then 
                    istk(lfunpt-1+i)=-lstk(fin)
                 else
                    kfun=i
                    buf='unknown block :'//buf(1:nn)
                    call error(888)
                    return
                 endif
              endif
           else
              err=4
              call error(44)
              return
           endif
           lf=lf+istk(ilSim_elts[0]+2+i)-istk(il4e2+1+i)
 10     continue
c
        if(ddt.ne.0) idb=1
c
        call scicos(stk(l1e2),istk(iadr(lSim_elts[1])),stk(l1e3),
     $       stk(l1e4),istk(ilinp),
     $       istk(iadr(lSim_elts[1]1)),
     &       istk(llab),istk(labptr),stk(l2),stk(l3),stk(l1e5),
     $       istk(iadr(l1e6)),n1e5,pointi,stk(l1e8),nout,
     $       istk(lfunpt),istk(iadr(lSim_elts[0]8)),istk(linpptr),
     $       istk(iloutptr),istk(iadr(lSim_elts[6])),istk(iadr(lSim_elts[7])),
     $       istk(llnkptr),nlnkptr,stk(lSim_elts[9]),istk(iadr(lSim_elts[10])),
     $       istk(iadr(lSim_elts[11])),istk(iadr(lSim_elts[12])),istk(iadr(lSim_elts[13])),
     $       istk(iadr(lSim_elts[14])),n4e16,
     $       istk(iadr(lSim_elts[16])),istk(iadr(lSim_elts[17])),n4e19,
     $       istk(iadr(lSim_elts[0]9)),nSim_elts[27],
     $       istk(iadr(lSim_elts[0]0)),nSim_elts[18],istk(iadr(lSim_elts[19])),n4e21,
     $       istk(iadr(lSim_elts[0]2)),nblk,istk(iadr(lSim_elts[22])),istk(iadr(lSim_elts[3])),
     $       istk(iadr(lSim_elts[0]7)),nSim_elts[25],simpar,flag,ierr)
        idb=0
        if (ierr .gt. 0 ) then
           if(ierr.eq.1) then
              buf='scheduling problem'
              kfun=0
           elseif(ierr.eq.2) then
              buf='input to zero-crossing stuck on zero'
              kfun=0
           elseif(ierr.eq.6) then
              buf='a block has been called with input out of its domain'
           elseif(ierr.eq.7) then
              buf='singularity in a block'
           elseif(ierr.eq.8) then
              buf='block produces an internal error'
           elseif(ierr.ge.1000) then
              buf='unknown or erroneous block'
           elseif(ierr.ge.100) then
              istate=-(ierr-100)
              write(buf,'(''integration problem istate='',i5)') istate
              kfun=0
           elseif(ierr.eq.3) then
              buf='event conflict'
              kfun=0
           elseif(ierr.eq.20) then
              buf='initial conditions not converging'
              kfun=0
           elseif(ierr.eq.4) then
              buf='algrebraic loop detected'
              kfun=0
           elseif(ierr.eq.5) then
              buf='cannot allocate memory'
              kfun=0
           elseif(ierr.eq.21) then
              write(buf,'(''cannot allocate memory in block='',i5)') 
     $             kfun
              kfun=0
           elseif(ierr.eq.22) then
              buf='sliding mode condition, cannot integrate'
              kfun=0
           else
              buf='scicos unexpected error,please report...'
              kfun=0
           endif
           if(.not.(err.gt.0.or.err1.gt.0)) call error(888)
           fun=0
           return
        endif
        if (err .gt. 0) return
c     
        fun=0
        top=top-rhs
c     
        if(lhs .ge. 1) then
c     
c       output variable: x0(x)
c     
c     change pointi to double
           stk(l1e7)=pointi
c     change iz to double
c           call int2db(n1e4,istk(iadr(l1e4)),-1,stk(l1e4),-1)
c     change evtspt to double
           call int2db(n1e6,istk(iadr(l1e6)),-1,stk(l1e6),-1)
           top=top+1
        endif
c     
        if(lhs .ge. 2) then
c     
c       output variable: t
c     
        top=top+1
        endif
c     
        return
      end
c
  */
}


int int_sctree(Stack stack, int rhs, int opt, int lhs) 
{
  int iok,nord,nb,i;
  NspMatrix *M[5],*ilord,*ok,*work;
  /* [ord,ok]=sctree(vec,in,depu,outptr,cmatp); */
  /* taille nb et entier lv */
  CheckRhs(5,5);
  CheckLhs(1,1);
  for ( i = 0 ; i < 5 ; i++) 
    {
      if ((M[i] = GetRealMat(stack,i+1)) == NULLMAT) return RET_BUG;
      M[i]= Mat2int(M[i]);
    }
  nb = M[0]->mn;
  if ((ilord = nsp_matrix_create(NVOID,'r',1,nb)) == NULLMAT) return RET_BUG;
  if ((ok = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return RET_BUG;
  /* which size ? FIXME */
  if ((work = nsp_matrix_create(NVOID,'r',1,nb)) == NULLMAT) return RET_BUG;
  sctree(nb,(int *)M[0]->R,(int *)M[1]->R,(int *)M[2]->R,(int *)M[3]->R,(int *)M[4]->R,
	 (int *)ilord->R,&nord,&iok,(int *)work->R);
  /* renvoyer un tableau de taille nord copie de ilord */
  ilord->convert= 'i';
  ilord = Mat2double(ilord);
  if ( nsp_matrix_resize(ilord,nord,1) == FAIL) return RET_BUG;
  ok->R[0]=iok;
  MoveObj(stack,1,(NspObject *)ilord);
  if ( lhs == 2) MoveObj(stack,2,(NspObject *)ok);
  return Max(lhs,1);
}



int int_tree2(Stack stack, int rhs, int opt, int lhs) 
{
  int nord,nmvec,iok,i;
  NspMatrix *M[4],*ipord,*ok;
  CheckRhs(4,4);
  CheckLhs(2,2);
  for ( i = 0 ; i < 4 ; i++) 
    {
      if ((M[i] = GetRealMat(stack,i+1)) == NULLMAT) return RET_BUG;
      M[i]= Mat2int(M[i]);
    }
  nmvec = M[0]->mn;
  if ((ipord = nsp_matrix_create(NVOID,'r',1,nmvec)) == NULLMAT) return RET_BUG;
  if ((ok = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return RET_BUG;
  /*
      if(.not.getrhsvar(1,'i',nvec,mvec,ipvec)) return
      if(.not.getrhsvar(2,'i',noin,moin,ipoin)) return
      if(.not.getrhsvar(3,'i',noinr,moinr,ipoinr)) return
      if(.not.getrhsvar(4,'i',ndep,mdep,ipdep)) return
      if(.not.createvar(5,'i',nvec*mvec,1,ipord)) return
      if(.not.createvar(6,'i',1,1,ipok)) return
  */

  ftree2((int *)M[0]->R,nmvec,(int *)M[3]->R,(int *)M[1]->R,(int *)M[2]->R,(int *)ipord->R,&nord,&iok);
  ipord->convert= 'i';
  ipord = Mat2double(ipord);
  if ( nsp_matrix_resize(ipord,nord,1) == FAIL) return RET_BUG;
  ok->R[0]=iok;
  MoveObj(stack,1,(NspObject *)ipord);
  if ( lhs == 2) MoveObj(stack,2,(NspObject *)ok);
  return Max(lhs,1);
}

int int_tree3(Stack stack, int rhs, int opt, int lhs) 
{
  NspMatrix *M[7],*ipord,*ok,*ipkk;
  int i,iok,nord,nb;
  CheckRhs(7,7);
  CheckLhs(2,2);
  for ( i = 0 ; i < 7 ; i++) 
    {
      if ((M[i] = GetRealMat(stack,i+1)) == NULLMAT) return RET_BUG;
      M[i]= Mat2int(M[i]);
    }
  nb = M[0]->mn;
  if ((ipord = nsp_matrix_create(NVOID,'r',1,nb)) == NULLMAT) return RET_BUG;
  if ((ok = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT) return RET_BUG;
  if ((ipkk = nsp_matrix_create(NVOID,'r',1,nb)) == NULLMAT) return RET_BUG;

  ftree3((int *)M[0]->R,M[0]->mn,(int *)M[1]->R,(int *)M[2]->R,(int *)M[3]->R,
	 (int *)M[4]->R,(int *)M[5]->R,(int *)M[6]->R,(int *)ipkk->R,
	 (int *)ipord->R,&nord,&iok);
  ipord->convert= 'i';
  ipord = Mat2double(ipord);
  if ( nsp_matrix_resize(ipord,nord,1) == FAIL) return RET_BUG;
  ok->R[0]=iok;
  MoveObj(stack,1,(NspObject *)ipord);
  if ( lhs == 2) MoveObj(stack,2,(NspObject *)ok);
  return Max(lhs,1);
}

int int_tree4(Stack stack, int rhs, int opt, int lhs) 
{
  NspMatrix *M[5],*ipr1,*ipr2;
  int i,nmd,nr;
  CheckRhs(5,5);
  CheckLhs(2,2);
  for ( i = 0 ; i < 5 ; i++) 
    {
      if ((M[i] = GetRealMat(stack,i+1)) == NULLMAT) return RET_BUG;
      M[i]= Mat2int(M[i]);
    }
  nmd = M[3]->mn;
  if ((ipr1 = nsp_matrix_create(NVOID,'r',1,nmd)) == NULLMAT) return RET_BUG;
  if ((ipr2 = nsp_matrix_create(NVOID,'r',1,nmd)) == NULLMAT) return RET_BUG;

  ftree4((int *)M[0]->R,M[3]->mn,(int *)M[4]->R,(int *)M[1]->R,(int *)M[2]->R,
	 (int *)ipr1->R,(int *)ipr2->R,&nr);
  ipr1->convert= 'i';
  ipr1 = Mat2double(ipr1);
  if ( nsp_matrix_resize(ipr1,nr,1) == FAIL) return RET_BUG;
  ipr2->convert= 'i';
  ipr2 = Mat2double(ipr2);
  if ( nsp_matrix_resize(ipr2,nr,1) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) ipr1);
  if ( lhs == 2) MoveObj(stack,2,(NspObject *)ipr2);
  return Max(lhs,1);
}


int int_scicos_debug(Stack stack, int rhs, int opt, int lhs) 
{
  /*
    FIXME 
      subroutine  scicosdebug(fname)
      common /cosdebug/ cosd
      logical getrhsvar
      int cosd
      character*(*) fname
      if(.not.getrhsvar(1,'i',n,m,i)) return
      cosd=istk(i)
      lhsvar(1)=0
      end
  */
}


int connection(int* path_out,int* path_in) 
{
  /* FIXME : call the routine 
   * under_connection 
   * function ninnout=under_connection(path_out,path_in)
   */
  int ninout; 
}

int badconnection(int* path_out,int prt_out, int nout,int* path_in,int prt_in,int nin) 
{
  /* FXME : call the routine 
   * bad_connection(path_out,prt_out,nout,path_in,prt_in,nin)
   */
  return 0;
}

int Message(char* code) 
{
  /* FIXME call x_message 
   */
  return 0;
}

