      subroutine rchek2(job, g, neq, y, yh, nyh, g0, g1, gx, jroot, irt
     $     ,IWORK, param)
clll. optimize
      external g
      integer job, neq, nyh, jroot, irt
      double precision y, yh, g0, g1, gx
      dimension neq(*), y(*), yh(nyh,*), g0(*), g1(*), gx(*), jroot(*)
      double precision param   ! use as a pointer to datas for external written in C
      integer iownd, iowns,
     1   icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,
     2   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer iownd3, iownr3, irfnd, itaskc, ngc, nge
      integer i, iflag, jflag
      double precision rownd, rowns,
     1   ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      double precision rownr3, t0, tlast, toutc
      double precision hming, t1,  x
      logical zroot, Mroot
      common /ls0001/ rownd, rowns(209),
     2   ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,
     3   iownd(14), iowns(6),
     4   icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter,
     5   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      common /lsr001/ rownr3(2), t0, tlast, toutc,
     1   iownd3(3), iownr3(2), irfnd, itaskc, ngc, nge      
      integer         iero
      common /ierode/ iero
c     ------------------ masking ----------------
      integer IWORK
      dimension IWORK(*)
c     pointers into iwork:
      parameter (leniw=18)
      integer    lmask
      data zero/0.0D0/
c     ------------------ masking ----------------

c!purpose
c this routine checks for the presence of a root in the
c vicinity of the current t, in a manner depending on the
c input flag job.  it calls subroutine roots to locate the root
c as precisely as possible.
c
c!calling sequence
c in addition to variables described previously, rchek
c uses the following for communication..
c job    = integer flag indicating type of call..
c          job = 1 means the problem is being initialized, and rchek
c                  is to look for a root at or very near the initial t.
c          job = 2 means a continuation call to the solver was just
c                  made, and rchek is to check for a root in the
c                  relevant part of the step last taken.
c          job = 3 means a successful step was just taken, and rchek
c                  is to look for a root in the interval of the step.
c g0     = array of length ng, containing the value of g at t = t0.
c          g0 is input for job .ge. 2 and on output in all cases.
c g1,gx  = arrays of length ng for work space.
c irt    = completion flag..
c          irt = 0  means no root was found.
c          irt = -1 means job = 1 and a root was found too near to t.
c          irt = 1  means a legitimate root was found (job = 2 or 3).
c                   on return, t0 is the root location, and y is the
c                   corresponding solution vector.
c          irt = 2 means a change from zero to a non-zero value has been
c          occured, so do a cold restart to reevaluate the modes
c          of if-then-else, because they have mode
c t0     = value of t at one endpoint of interval of interest.  only
c          roots beyond t0 in the direction of integration are sought.
c          t0 is input if job .ge. 2, and output in all cases.
c          t0 is updated by rchek, whether a root is found or not.
c tlast  = last value of t returned by the solver (input only).
c toutc  = copy of tout (input only).
c irfnd  = input flag showing whether the last step taken had a root.
c          irfnd = 1 if it did, = 0 if not.
c itaskc = copy of itask (input only).
c ngc    = copy of ng (input only).
c!
c
      irt = 0
c     -------------- masking obtaining the mask adresses-----------------
      lmask=iwork(leniw)-ngc
c     -------------- masking -----------------
      hming = (dabs(tn) + dabs(h))*uround*100.0d0
c
      go to (100, 200, 300), job
c
c evaluate g at initial t, and check for zero values. ------------------
 100  continue
c     -------------- masking: disabling masks in cold-major-time-step------
      do 103 i = 1,ngc
         jroot(i) = 0
 103     iwork(lmask+i)=0

      t0=tn
      call g (neq, t0, y, ngc, g0, param)
      nge = nge + 1

      do 110 i = 1,ngc
         if (dabs(g0(i)) .eq. zero) then
            iwork(lmask+i)=1
         endif
 110  continue
      return
c     -------------- masking -----------------
c
 200  continue

c     in the previous call there was not a root, so this part can be ignored.
c      if (iwork(lirfnd) .eq. 0) go to 260
       do 203 i = 1,ngc
          jroot(i) = 0
 203      iwork(lmask+i)=0
c     if a root was found on the previous step, evaluate r0 = r(t0). -------
       call intdy (t0, 0, yh, nyh, y, iflag)
       call g (neq, t0, y, ngc, g0, param)
       nge = nge + 1
       do 210 i = 1,ngc
          if (dabs(g0(i)) .eq. zero) then
             iwork(lmask+i)=1
          endif
 210   continue
c     r0 has no zero components.  proceed to check relevant interval. ------
c     260  continue 
       if (tn .eq. tlast) return
c
c     -------------- try in manor-time-steps -----
 300  continue

c set t1 to tn or toutc, whichever comes first, and get g at t1. -------
      if (itaskc.eq.2 .or. itaskc.eq.3 .or. itaskc.eq.5) go to 310
      if ((toutc - tn)*h .ge. 0.0d0) go to 310
      t1 = toutc
      if ((t1 - t0)*h .le. 0.0d0) go to 390
      call intdy (t1, 0, yh, nyh, y, iflag)
      go to 330
 310  t1 = tn
      do 320 i = 1,n
 320    y(i) = yh(i,1)
 330  call g (neq, t1, y, ngc, g1, param)
      if(iero.gt.0) return
      nge = nge + 1

c     call droots to search for root in interval from t0 to t1. -----------
      jflag = 0
      
      do 340 i = 1,ngc
         jroot(i)=iwork(lmask+i)
 340  continue
      
 350  continue
      call roots2(ngc,hming, jflag, t0, t1, g0, g1, gx, x, jroot)
      if (jflag .gt. 1) go to 360
      call intdy (x, 0, yh, nyh, y, iflag)
      call g (neq, x, y, ngc, gx, param)
      if(iero.gt.0) return
      nge = nge + 1
      go to 350
      
 360  continue
      if (jflag.eq.2) then      ! root found         
         zroot=.false.
         mroot=.false.
         do 361 i = 1,ngc          
            if(iwork(lmask+i).eq.1) then
               if(abs(g1(i)).ne. zero) then
                  jroot(i)=sign(2.0d0,g1(i))
                  mroot=.true.
               else
                  jroot(i)=0
               endif
            else
               if (abs(g1(i)) .eq. zero) then
                  jroot(i) = -sign(1.0d0,g0(i))
                  zroot=.true.
               else
                  if (sign(1.0d0,g0(i)) .ne. sign(1.0d0,g1(i))) then
                     jroot(i) = sign(1.0d0,g1(i) - g0(i))
                     zroot=.true.
                  else
                     jroot(i)=0
                  endif
               endif
            endif
 361     continue
         
         call intdy (x, 0, yh, nyh, y, iflag)

         if (zroot) then
            do 380 i = 1,ngc
               if(abs(jroot(i)).eq.2) jroot(i)=0
 380        continue  
            mroot=.false.
            irt=1
         endif
         if (mroot) then
            irt=2
         endif
      endif
      t0 = x
      call dcopy (ngc, gx, 1, g0, 1)
      return
c     
 390  continue
      return
c----------------------- end of subroutine rchek -----------------------
      end
