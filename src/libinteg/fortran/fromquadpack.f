*
*  this file contains 6 routines from the netlib quapack distribution:
*  - dqagse: routine for integration on [a,b]
*  - dqagie: routine for integration on (-oo,a], [a,+oo) or (-oo,+oo)
*  - dqelg: epsilon algorithm code used both by dqagse and dqagie
*  - dqpsrt: used both by dqagse and dqagie
*  - dqk21: base formula used by dqagse
*  - dqk15i: base formula used by dqagie
* 
*  These routines have been slightly modified by Bruno Pincon
*  for inclusion in nsp, in particular dqk21 and dqk15i
*  (see comments). Also calls to d1mach have been replaced by
*  equivalent call to the lapack dlamch and the fortran code
*  have been cleaned a little.
*  All names are prefixed by nsp to avoid possible future 
*  name clash.
*  
*  

      subroutine nspdqagse(f,a,b,epsabs,epsrel,limit,result,abserr,neval
     *   ,ier,alist,blist,rlist,elist,iord,last,vectflag,stat)
      implicit none
      integer  f,limit,neval,ier,iord(limit),last,stat
      external f
      logical vectflag
      double precision a,b,epsabs,epsrel,result,abserr,
     *  alist(limit), blist(limit),rlist(limit),elist(limit)
c***begin prologue  dqagse
c***date written   800101   (yymmdd)
c***revision date  830518   (yymmdd)
c***category no.  h2a1a1
c***keywords  automatic integrator, general-purpose,
c             (end point) singularities, extrapolation,
c             globally adaptive
c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
c
c***slightly modified by Bruno Pincon for nsp
c
c***purpose  the routine calculates an approximation result to a given
c            definite integral i = integral of f over (a,b),
c            hopefully satisfying following claim for accuracy
c            abs(i-result).le.max(epsabs,epsrel*abs(i)).
c***description
c
c        computation of a definite integral
c        standard fortran subroutine
c        double precision version
c
c        parameters
c         on entry
c
c            f      - function subprogram defining the integrand
c                     function f(x). the actual name for f needs to be
c                     declared e x t e r n a l in the driver program.
*                     f has the form of a function returning an int:
*                      stat = f(x,y,n)
*                     which must compute y(k)=f(x(k)) 1<=k<=n
*                     the returned value (stat) is used to communicate
*                     problems in the evaluation of the function by
*                     the nsp interpretor
c
c            a      - double precision
c                     lower limit of integration
c
c            b      - double precision
c                     upper limit of integration
c
c            epsabs - double precision
c                     absolute accuracy requested
c            epsrel - double precision
c                     relative accuracy requested
c                     if  epsabs.le.0
c                     and epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
c                     the routine will end with ier = 6.
c
c            limit  - integer
c                     gives an upperbound on the number of subintervals
c                     in the partition of (a,b)
c
c         on return
c            result - double precision
c                     approximation to the integral
c
c            abserr - double precision
c                     estimate of the modulus of the absolute error,
c                     which should equal or exceed abs(i-result)
c
c            neval  - integer
c                     number of integrand evaluations
c
c            ier    - integer
c                     ier = 0 normal and reliable termination of the
c                             routine. it is assumed that the requested
c                             accuracy has been achieved.
c                     ier.gt.0 abnormal termination of the routine
c                             the estimates for integral and error are
c                             less reliable. it is assumed that the
c                             requested accuracy has not been achieved.
c            error messages
c                         = 1 maximum number of subdivisions allowed
c                             has been achieved. one can allow more sub-
c                             divisions by increasing the value of limit
c                             (and taking the according dimension
c                             adjustments into account). however, if
c                             this yields no improvement it is advised
c                             to analyze the integrand in order to
c                             determine the integration difficulties. if
c                             the position of a local difficulty can be
c                             determined (e.g. singularity,
c                             discontinuity within the interval) one
c                             will probably gain from splitting up the
c                             interval at this point and calling the
c                             integrator on the subranges. if possible,
c                             an appropriate special-purpose integrator
c                             should be used, which is designed for
c                             handling the type of difficulty involved.
c                         = 2 the occurrence of roundoff error is detec-
c                             ted, which prevents the requested
c                             tolerance from being achieved.
c                             the error may be under-estimated.
c                         = 3 extremely bad integrand behaviour
c                             occurs at some points of the integration
c                             interval.
c                         = 4 the algorithm does not converge.
c                             roundoff error is detected in the
c                             extrapolation table.
c                             it is presumed that the requested
c                             tolerance cannot be achieved, and that the
c                             returned result is the best which can be
c                             obtained.
c                         = 5 the integral is probably divergent, or
c                             slowly convergent. it must be noted that
c                             divergence can occur with any other value
c                             of ier.
c                         = 6 the input is invalid, because
c                             epsabs.le.0 and
c                             epsrel.lt.max(50*rel.mach.acc.,0.5d-28).
c                             result, abserr, neval, last, rlist(1),
c                             iord(1) and elist(1) are set to zero.
c                             alist(1) and blist(1) are set to a and b
c                             respectively.
c
c            alist  - double precision
c                     vector of dimension at least limit, the first
c                      last  elements of which are the left end points
c                     of the subintervals in the partition of the
c                     given integration range (a,b)
c
c            blist  - double precision
c                     vector of dimension at least limit, the first
c                      last  elements of which are the right end points
c                     of the subintervals in the partition of the given
c                     integration range (a,b)
c
c            rlist  - double precision
c                     vector of dimension at least limit, the first
c                      last  elements of which are the integral
c                     approximations on the subintervals
c
c            elist  - double precision
c                     vector of dimension at least limit, the first
c                      last  elements of which are the moduli of the
c                     absolute error estimates on the subintervals
c
c            iord   - integer
c                     vector of dimension at least limit, the first k
c                     elements of which are pointers to the
c                     error estimates over the subintervals,
c                     such that elist(iord(1)), ..., elist(iord(k))
c                     form a decreasing sequence, with k = last
c                     if last.le.(limit/2+2), and k = limit+1-last
c                     otherwise
c
c            last   - integer
c                     number of subintervals actually produced in the
c                     subdivision process
c
c***references  (none)
c***routines called  d1mach,dqelg,dqk21,dqpsrt
c***end prologue  dqagse
c
*    local vars
      double precision abseps,area,area1,area12,area2,a1,a2,b1,b2,
     *  correc,defabs,defab1,defab2,dlamch,dres,epmach,erlarg,
     *  erlast,errbnd,errmax, error1,error2,erro12,errsum,ertest,oflow,
     *  resabs,reseps,res3la(3),rlist2(52),small,uflow
      integer id,ierro,iroff1,iroff2,iroff3,jupbnd,k,ksgn,ktmin,maxerr,
     *  nres,nrmax,numrl2
      logical extrap,noext
c
c            the dimension of rlist2 is determined by the value of
c            limexp in subroutine dqelg (rlist2 should be of dimension
c            (limexp+2) at least).
c
c            list of major variables
c            -----------------------
c
c           alist     - list of left end points of all subintervals
c                       considered up to now
c           blist     - list of right end points of all subintervals
c                       considered up to now
c           rlist(i)  - approximation to the integral over
c                       (alist(i),blist(i))
c           rlist2    - array of dimension at least limexp+2 containing
c                       the part of the epsilon table which is still
c                       needed for further computations
c           elist(i)  - error estimate applying to rlist(i)
c           maxerr    - pointer to the interval with largest error
c                       estimate
c           errmax    - elist(maxerr)
c           erlast    - error on the interval currently subdivided
c                       (before that subdivision has taken place)
c           area      - sum of the integrals over the subintervals
c           errsum    - sum of the errors over the subintervals
c           errbnd    - requested accuracy max(epsabs,epsrel*
c                       abs(result))
c           *****1    - variable for the left interval
c           *****2    - variable for the right interval
c           last      - index for subdivision
c           nres      - number of calls to the extrapolation routine
c           numrl2    - number of elements currently in rlist2. if an
c                       appropriate approximation to the compounded
c                       integral has been obtained it is put in
c                       rlist2(numrl2) after numrl2 has been increased
c                       by one.
c           small     - length of the smallest interval considered up
c                       to now, multiplied by 1.5
c           erlarg    - sum of the errors over the intervals larger
c                       than the smallest interval considered up to now
c           extrap    - logical variable denoting that the routine is
c                       attempting to perform extrapolation i.e. before
c                       subdividing the smallest interval we try to
c                       decrease the value of erlarg.
c           noext     - logical variable denoting that extrapolation
c                       is no longer allowed (true value)
c
c            machine dependent constants
c            ---------------------------
c
c           epmach is the largest relative spacing.
c           uflow is the smallest positive magnitude.
c           oflow is the largest positive magnitude.
c
c***first executable statement  dqagse
      epmach = dlamch('p')
c
c            test on validity of parameters
c            ------------------------------
      ier = 0
      neval = 0
      last = 0
      result = 0.d0
      abserr = 0.d0
      alist(1) = a
      blist(1) = b
      rlist(1) = 0.d0
      elist(1) = 0.d0
      if(epsabs.le.0.d0.and.epsrel.lt.max(50.d0*epmach,0.5d-28)) then
         ier = 6
         return
      endif
c
c           first approximation to the integral
c           -----------------------------------
c
      uflow = dlamch('u')
      oflow = dlamch('o')
      ierro = 0
      call nspdqk21(f,a,b,result,abserr,defabs,resabs,vectflag,stat)
      if ( stat .ne. 0 ) return

c
c           test on accuracy.
c
      dres = abs(result)
      errbnd = max(epsabs,epsrel*dres)
      last = 1
      rlist(1) = result
      elist(1) = abserr
      iord(1) = 1
      if(abserr.le.1.0d+02*epmach*defabs.and.abserr.gt.errbnd) ier = 2
      if(limit.eq.1) ier = 1
      if(ier.ne.0.or.(abserr.le.errbnd.and.abserr.ne.resabs).or.
     *  abserr.eq.0.0d+00) go to 140
c
c           initialization
c           --------------
c
      rlist2(1) = result
      errmax = abserr
      maxerr = 1
      area = result
      errsum = abserr
      abserr = oflow
      nrmax = 1
      nres = 0
      numrl2 = 2
      ktmin = 0
      extrap = .false.
      noext = .false.
      iroff1 = 0
      iroff2 = 0
      iroff3 = 0
      ksgn = -1
      if(dres.ge.(0.1d+01-0.5d+02*epmach)*defabs) ksgn = 1
c
c           main do-loop
c           ------------
c
      do 90 last = 2,limit
c
c           bisect the subinterval with the nrmax-th largest error
c           estimate.
c
         a1 = alist(maxerr)
         b1 = 0.5d+00*(alist(maxerr)+blist(maxerr))
         a2 = b1
         b2 = blist(maxerr)
         erlast = errmax

************************************************************************
*
*   use new nspdqk21b to integrate on the 2 subintervals improving the
*   (possible) vectorization for nsp function. (bruno on 12 july 2009)
*
         call nspdqk21b(f, a1, b2, area1, error1, defab1, 
     *                  area2, error2, defab2, vectflag, stat)
         if ( stat .ne. 0 ) return

c$$$         call nspdqk21(f,a1,b1,area1,error1,resabs,defab1,vectflag,stat)
c$$$         if ( stat .ne. 0 ) return
c$$$         call nspdqk21(f,a2,b2,area2,error2,resabs,defab2,vectflag,stat)
c$$$         if ( stat .ne. 0 ) return
*
************************************************************************
c
c           improve previous approximations to integral
c           and error and test for accuracy.
c
         area12 = area1+area2
         erro12 = error1+error2
         errsum = errsum+erro12-errmax
         area = area+area12-rlist(maxerr)
         if(defab1.eq.error1.or.defab2.eq.error2) go to 15
         if(abs(rlist(maxerr)-area12).gt.0.1d-04*abs(area12)
     *        .or.erro12.lt.0.99d+00*errmax) go to 10
         if(extrap) iroff2 = iroff2+1
         if(.not.extrap) iroff1 = iroff1+1
 10      if(last.gt.10.and.erro12.gt.errmax) iroff3 = iroff3+1
 15      rlist(maxerr) = area1
         rlist(last) = area2
         errbnd = max(epsabs,epsrel*abs(area))
c
c           test for roundoff error and eventually set error flag.
c
         if(iroff1+iroff2.ge.10.or.iroff3.ge.20) ier = 2
         if(iroff2.ge.5) ierro = 3
c
c           set error flag in the case that the number of subintervals
c           equals limit.
c
         if(last.eq.limit) ier = 1
c
c           set error flag in the case of bad integrand behaviour
c           at a point of the integration range.
c
         if( max(abs(a1),abs(b2)).le.(1.d0 + 1.d02*epmach)*
     *        (abs(a2)+1.d03*uflow)) ier = 4
c
c           append the newly-created intervals to the list.
c
         if(error2.le.error1) then
            alist(last) = a2
            blist(maxerr) = b1
            blist(last) = b2
            elist(maxerr) = error1
            elist(last) = error2
         else
            alist(maxerr) = a2
            alist(last) = a1
            blist(last) = b1
            rlist(maxerr) = area2
            rlist(last) = area1
            elist(maxerr) = error2
            elist(last) = error1
         endif
c
c           call subroutine dqpsrt to maintain the descending ordering
c           in the list of error estimates and select the subinterval
c           with nrmax-th largest error estimate (to be bisected next).
c
         call nspdqpsrt(limit,last,maxerr,errmax,elist,iord,nrmax)
c     ***jump out of do-loop
         if(errsum.le.errbnd) go to 115
c     ***jump out of do-loop
         if(ier.ne.0) go to 100
         if(last.eq.2) go to 80
         if(noext) go to 90
         erlarg = erlarg-erlast
         if(abs(b1-a1).gt.small) erlarg = erlarg+erro12
         if(extrap) go to 40
c     
c           test whether the interval to be bisected next is the
c           smallest interval.
c
         if(abs(blist(maxerr)-alist(maxerr)).gt.small) go to 90
         extrap = .true.
         nrmax = 2
 40      if(ierro.eq.3.or.erlarg.le.ertest) go to 60
c
c           the smallest interval has the largest error.
c           before bisecting decrease the sum of the errors over the
c           larger intervals (erlarg) and perform extrapolation.
c
         id = nrmax
         jupbnd = last
         if(last.gt.(2+limit/2)) jupbnd = limit+3-last
         do k = id,jupbnd
            maxerr = iord(nrmax)
            errmax = elist(maxerr)
c           ***jump out of do-loop
            if(abs(blist(maxerr)-alist(maxerr)).gt.small) go to 90
            nrmax = nrmax+1
         enddo
c
c           perform extrapolation.
c
 60      numrl2 = numrl2+1
         rlist2(numrl2) = area
         call nspdqelg(numrl2,rlist2,reseps,abseps,res3la,nres)
         ktmin = ktmin+1
         if(ktmin.gt.5.and.abserr.lt.0.1d-02*errsum) ier = 5
         if(abseps.ge.abserr) go to 70
         ktmin = 0
         abserr = abseps
         result = reseps
         correc = erlarg
         ertest = max(epsabs,epsrel*abs(reseps))
c     ***jump out of do-loop
         if(abserr.le.ertest) go to 100
c
c           prepare bisection of the smallest interval.
c
 70      if(numrl2.eq.1) noext = .true.
         if(ier.eq.5) go to 100
         maxerr = iord(1)
         errmax = elist(maxerr)
         nrmax = 1
         extrap = .false.
         small = small*0.5d+00
         erlarg = errsum
         go to 90
 80      small = abs(b-a)*0.375d+00
         erlarg = errsum
         ertest = errbnd
         rlist2(2) = area
 90   continue
c     
c           set final result and error estimate.
c           ------------------------------------
c
 100  if(abserr.eq.oflow) go to 115
      if(ier+ierro.eq.0) go to 110
      if(ierro.eq.3) abserr = abserr+correc
      if(ier.eq.0) ier = 3
      if(result.ne.0.0d+00.and.area.ne.0.0d+00) go to 105
      if(abserr.gt.errsum) go to 115
      if(area.eq.0.0d+00) go to 130
      go to 110
 105  if(abserr/abs(result).gt.errsum/abs(area)) go to 115
c
c           test on divergence.
c
 110  if(ksgn.eq.(-1).and.max(abs(result),abs(area)).le.
     *     defabs*0.1d-01) go to 130
      if(0.1d-01.gt.(result/area).or.(result/area).gt.0.1d+03
     *     .or.errsum.gt.abs(area)) ier = 6
      go to 130
c
c           compute global integral sum.
c
 115  result = 0.0d+00
      do k = 1,last
         result = result+rlist(k)
      enddo
      abserr = errsum
 130  if(ier.gt.2) ier = ier-1
 140  neval = 42*last-21
      return
      end


      subroutine nspdqagie(f,bound,inf,epsabs,epsrel,limit,result,abserr
     *   ,neval,ier,alist,blist,rlist,elist,iord,last,vectflag,stat)
      implicit none
      integer  f,inf,limit,neval,ier,iord(limit),last,stat
      external f
      logical vectflag
      double precision bound,epsabs,epsrel,result,abserr,alist(limit),
     *  blist(limit),rlist(limit),elist(limit)
c
c***begin prologue  dqagie
c***date written   800101   (yymmdd)
c***revision date  830518   (yymmdd)
c***category no.  h2a3a1,h2a4a1
c***keywords  automatic integrator, infinite intervals,
c             general-purpose, transformation, extrapolation,
c             globally adaptive
c***author  piessens,robert,appl. math & progr. div - k.u.leuven
c           de doncker,elise,appl. math & progr. div - k.u.leuven
c
c***slightly modified by Bruno Pincon for nsp
c
c***purpose  the routine calculates an approximation result to a given
c            integral   i = integral of f over (bound,+infinity)
c            or i = integral of f over (-infinity,bound)
c            or i = integral of f over (-infinity,+infinity),
c            hopefully satisfying following claim for accuracy
c            abs(i-result).le.max(epsabs,epsrel*abs(i))
c***description
c
c integration over infinite intervals
c standard fortran subroutine
c
c            f      - function subprogram defining the integrand
c                     function f(x). the actual name for f needs to be
c                     declared e x t e r n a l in the driver program.
*                     f has the form of a function returning an int:
*                      stat = f(x,y,n)
*                     which must compute y(k)=f(x(k)) 1<=k<=n
*                     the returned value (stat) is used to communicate
*                     problems in the evaluation of the function by
*                     the nsp interpretor
c
c            bound  - double precision
c                     finite bound of integration range
c                     (has no meaning if interval is doubly-infinite)
c
c            inf    - double precision
c                     indicating the kind of integration range involved
c                     inf = 1 corresponds to  (bound,+infinity),
c                     inf = -1            to  (-infinity,bound),
c                     inf = 2             to (-infinity,+infinity).
c
c            epsabs - double precision
c                     absolute accuracy requested
c            epsrel - double precision
c                     relative accuracy requested
c                     if  epsabs.le.0
c                     and epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
c                     the routine will end with ier = 6.
c
c            limit  - integer
c                     gives an upper bound on the number of subintervals
c                     in the partition of (a,b), limit.ge.1
c
c         on return
c            result - double precision
c                     approximation to the integral
c
c            abserr - double precision
c                     estimate of the modulus of the absolute error,
c                     which should equal or exceed abs(i-result)
c
c            neval  - integer
c                     number of integrand evaluations
c
c            ier    - integer
c                     ier = 0 normal and reliable termination of the
c                             routine. it is assumed that the requested
c                             accuracy has been achieved.
c                   - ier.gt.0 abnormal termination of the routine. the
c                             estimates for result and error are less
c                             reliable. it is assumed that the requested
c                             accuracy has not been achieved.
c            error messages
c                     ier = 1 maximum number of subdivisions allowed
c                             has been achieved. one can allow more
c                             subdivisions by increasing the value of
c                             limit (and taking the according dimension
c                             adjustments into account). however,if
c                             this yields no improvement it is advised
c                             to analyze the integrand in order to
c                             determine the integration difficulties.
c                             if the position of a local difficulty can
c                             be determined (e.g. singularity,
c                             discontinuity within the interval) one
c                             will probably gain from splitting up the
c                             interval at this point and calling the
c                             integrator on the subranges. if possible,
c                             an appropriate special-purpose integrator
c                             should be used, which is designed for
c                             handling the type of difficulty involved.
c                         = 2 the occurrence of roundoff error is
c                             detected, which prevents the requested
c                             tolerance from being achieved.
c                             the error may be under-estimated.
c                         = 3 extremely bad integrand behaviour occurs
c                             at some points of the integration
c                             interval.
c                         = 4 the algorithm does not converge.
c                             roundoff error is detected in the
c                             extrapolation table.
c                             it is assumed that the requested tolerance
c                             cannot be achieved, and that the returned
c                             result is the best which can be obtained.
c                         = 5 the integral is probably divergent, or
c                             slowly convergent. it must be noted that
c                             divergence can occur with any other value
c                             of ier.
c                         = 6 the input is invalid, because
c                             (epsabs.le.0 and
c                              epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
c                             result, abserr, neval, last, rlist(1),
c                             elist(1) and iord(1) are set to zero.
c                             alist(1) and blist(1) are set to 0
c                             and 1 respectively.
c
c            alist  - double precision
c                     vector of dimension at least limit, the first
c                      last  elements of which are the left
c                     end points of the subintervals in the partition
c                     of the transformed integration range (0,1).
c
c            blist  - double precision
c                     vector of dimension at least limit, the first
c                      last  elements of which are the right
c                     end points of the subintervals in the partition
c                     of the transformed integration range (0,1).
c
c            rlist  - double precision
c                     vector of dimension at least limit, the first
c                      last  elements of which are the integral
c                     approximations on the subintervals
c
c            elist  - double precision
c                     vector of dimension at least limit,  the first
c                     last elements of which are the moduli of the
c                     absolute error estimates on the subintervals
c
c            iord   - integer
c                     vector of dimension limit, the first k
c                     elements of which are pointers to the
c                     error estimates over the subintervals,
c                     such that elist(iord(1)), ..., elist(iord(k))
c                     form a decreasing sequence, with k = last
c                     if last.le.(limit/2+2), and k = limit+1-last
c                     otherwise
c
c            last   - integer
c                     number of subintervals actually produced
c                     in the subdivision process
c
c***references  (none)
c***routines called  dlamch,dqelg,dqk15i,dqpsrt
c***end prologue  dqagie
*
*    local vars
      double precision abseps,area,area1,area12,area2,a1,a2,boun,b1,b2,
     *  correc,defabs,defab1,defab2,dlamch,dres,epmach,erlarg,erlast,
     *  errbnd,errmax,error1,error2,erro12,errsum,ertest,oflow,resabs,
     *  reseps,res3la(3),rlist2(52),small,uflow
      integer id,ierro,iroff1,iroff2,iroff3,jupbnd,k,ksgn,ktmin, maxerr,
     *  nres,nrmax,numrl2
      logical extrap,noext
c
c            the dimension of rlist2 is determined by the value of
c            limexp in subroutine dqelg.
c
c
c            list of major variables
c            -----------------------
c
c           alist     - list of left end points of all subintervals
c                       considered up to now
c           blist     - list of right end points of all subintervals
c                       considered up to now
c           rlist(i)  - approximation to the integral over
c                       (alist(i),blist(i))
c           rlist2    - array of dimension at least (limexp+2),
c                       containing the part of the epsilon table
c                       wich is still needed for further computations
c           elist(i)  - error estimate applying to rlist(i)
c           maxerr    - pointer to the interval with largest error
c                       estimate
c           errmax    - elist(maxerr)
c           erlast    - error on the interval currently subdivided
c                       (before that subdivision has taken place)
c           area      - sum of the integrals over the subintervals
c           errsum    - sum of the errors over the subintervals
c           errbnd    - requested accuracy max(epsabs,epsrel*
c                       abs(result))
c           *****1    - variable for the left subinterval
c           *****2    - variable for the right subinterval
c           last      - index for subdivision
c           nres      - number of calls to the extrapolation routine
c           numrl2    - number of elements currently in rlist2. if an
c                       appropriate approximation to the compounded
c                       integral has been obtained, it is put in
c                       rlist2(numrl2) after numrl2 has been increased
c                       by one.
c           small     - length of the smallest interval considered up
c                       to now, multiplied by 1.5
c           erlarg    - sum of the errors over the intervals larger
c                       than the smallest interval considered up to now
c           extrap    - logical variable denoting that the routine
c                       is attempting to perform extrapolation. i.e.
c                       before subdividing the smallest interval we
c                       try to decrease the value of erlarg.
c           noext     - logical variable denoting that extrapolation
c                       is no longer allowed (true-value)
c
c            machine dependent constants
c            ---------------------------
c
c           epmach is the largest relative spacing.
c           uflow is the smallest positive magnitude.
c           oflow is the largest positive magnitude.
c
c***first executable statement  dqagie
       epmach = dlamch('p')
c
c           test on validity of parameters
c           -----------------------------
c
      ier = 0
      neval = 0
      last = 0
      result = 0.d0
      abserr = 0.d0
      alist(1) = 0.d0
      blist(1) = 1.d0
      rlist(1) = 0.d0
      elist(1) = 0.d0
      iord(1) = 0
      if(epsabs.le.0.d0.and.epsrel.lt.max(50.d0*epmach,0.5d-28)) then
         ier = 6
         return
      endif
c
c
c           first approximation to the integral
c           -----------------------------------
c
c           determine the interval to be mapped onto (0,1).
c           if inf = 2 the integral is computed as i = i1+i2, where
c           i1 = integral of f over (-infinity,0),
c           i2 = integral of f over (0,+infinity).
c
      boun = bound
      if(inf.eq.2) boun = 0.d0
      call nspdqk15i(f,boun,inf,0.d0,1.d0,result,abserr,defabs,resabs,
     *            vectflag,stat)
      if ( stat .ne. 0 ) return
c
c           test on accuracy
c
      last = 1
      rlist(1) = result
      elist(1) = abserr
      iord(1) = 1
      dres = abs(result)
      errbnd = max(epsabs,epsrel*dres)
      if(abserr.le.1.0d+02*epmach*defabs.and.abserr.gt.errbnd) ier = 2
      if(limit.eq.1) ier = 1
      if(ier.ne.0.or.(abserr.le.errbnd.and.abserr.ne.resabs).or.
     *  abserr.eq.0.0d+00) go to 130
c
c           initialization
c           --------------
c
      uflow = dlamch('u')
      oflow = dlamch('o')
      rlist2(1) = result
      errmax = abserr
      maxerr = 1
      area = result
      errsum = abserr
      abserr = oflow
      nrmax = 1
      nres = 0
      ktmin = 0
      numrl2 = 2
      extrap = .false.
      noext = .false.
      ierro = 0
      iroff1 = 0
      iroff2 = 0
      iroff3 = 0
      ksgn = -1
      if(dres.ge.(0.1d+01-0.5d+02*epmach)*defabs) ksgn = 1
c
c           main do-loop
c           ------------
c
      do 90 last = 2,limit
c
c           bisect the subinterval with nrmax-th largest error estimate.
c
        a1 = alist(maxerr)
        b1 = 0.5d+00*(alist(maxerr)+blist(maxerr))
        a2 = b1
        b2 = blist(maxerr)
        erlast = errmax
        call nspdqk15i(f,boun,inf,a1,b1,area1,error1,resabs,defab1,
     *                 vectflag, stat)
        if ( stat .ne. 0 ) return
        call nspdqk15i(f,boun,inf,a2,b2,area2,error2,resabs,defab2,
     *                 vectflag, stat)
        if ( stat .ne. 0 ) return
c
c           improve previous approximations to integral
c           and error and test for accuracy.
c
        area12 = area1+area2
        erro12 = error1+error2
        errsum = errsum+erro12-errmax
        area = area+area12-rlist(maxerr)
        if(defab1.eq.error1.or.defab2.eq.error2)go to 15
        if(abs(rlist(maxerr)-area12).gt.0.1d-04*abs(area12)
     *  .or.erro12.lt.0.99d+00*errmax) go to 10
        if(extrap) iroff2 = iroff2+1
        if(.not.extrap) iroff1 = iroff1+1
   10   if(last.gt.10.and.erro12.gt.errmax) iroff3 = iroff3+1
   15   rlist(maxerr) = area1
        rlist(last) = area2
        errbnd = max(epsabs,epsrel*abs(area))
c
c           test for roundoff error and eventually set error flag.
c
        if(iroff1+iroff2.ge.10.or.iroff3.ge.20) ier = 2
        if(iroff2.ge.5) ierro = 3
c
c           set error flag in the case that the number of
c           subintervals equals limit.
c
        if(last.eq.limit) ier = 1
c
c           set error flag in the case of bad integrand behaviour
c           at some points of the integration range.
c
        if(max(abs(a1),abs(b2)).le.(0.1d+01+0.1d+03*epmach)*
     *  (abs(a2)+0.1d+04*uflow)) ier = 4
c
c           append the newly-created intervals to the list.
c
        if(error2.le.error1) then
           alist(last) = a2
           blist(maxerr) = b1
           blist(last) = b2
           elist(maxerr) = error1
           elist(last) = error2
        else
           alist(maxerr) = a2
           alist(last) = a1
           blist(last) = b1
           rlist(maxerr) = area2
           rlist(last) = area1
           elist(maxerr) = error2
           elist(last) = error1
        endif
c
c           call subroutine dqpsrt to maintain the descending ordering
c           in the list of error estimates and select the subinterval
c           with nrmax-th largest error estimate (to be bisected next).
c
        call nspdqpsrt(limit,last,maxerr,errmax,elist,iord,nrmax)
        if(errsum.le.errbnd) go to 115
        if(ier.ne.0) go to 100
        if(last.eq.2) go to 80
        if(noext) go to 90
        erlarg = erlarg-erlast
        if(abs(b1-a1).gt.small) erlarg = erlarg+erro12
        if(extrap) go to 40
c
c           test whether the interval to be bisected next is the
c           smallest interval.
c
        if(abs(blist(maxerr)-alist(maxerr)).gt.small) go to 90
        extrap = .true.
        nrmax = 2
   40   if(ierro.eq.3.or.erlarg.le.ertest) go to 60
c
c           the smallest interval has the largest error.
c           before bisecting decrease the sum of the errors over the
c           larger intervals (erlarg) and perform extrapolation.
c
        id = nrmax
        jupbnd = last
        if(last.gt.(2+limit/2)) jupbnd = limit+3-last
        do k = id,jupbnd
           maxerr = iord(nrmax)
           errmax = elist(maxerr)
           if(abs(blist(maxerr)-alist(maxerr)).gt.small) go to 90
           nrmax = nrmax+1
        enddo
c
c           perform extrapolation.
c
   60   numrl2 = numrl2+1
        rlist2(numrl2) = area
        call nspdqelg(numrl2,rlist2,reseps,abseps,res3la,nres)
        ktmin = ktmin+1
        if(ktmin.gt.5.and.abserr.lt.0.1d-02*errsum) ier = 5
        if(abseps.ge.abserr) go to 70
        ktmin = 0
        abserr = abseps
        result = reseps
        correc = erlarg
        ertest = max(epsabs,epsrel*abs(reseps))
        if(abserr.le.ertest) go to 100
c
c            prepare bisection of the smallest interval.
c
   70   if(numrl2.eq.1) noext = .true.
        if(ier.eq.5) go to 100
        maxerr = iord(1)
        errmax = elist(maxerr)
        nrmax = 1
        extrap = .false.
        small = small*0.5d+00
        erlarg = errsum
        go to 90
   80   small = 0.375d+00
        erlarg = errsum
        ertest = errbnd
        rlist2(2) = area
   90 continue
c
c           set final result and error estimate.
c           ------------------------------------
c
  100 if(abserr.eq.oflow) go to 115
      if((ier+ierro).eq.0) go to 110
      if(ierro.eq.3) abserr = abserr+correc
      if(ier.eq.0) ier = 3
      if(result.ne.0.0d+00.and.area.ne.0.0d+00)go to 105
      if(abserr.gt.errsum)go to 115
      if(area.eq.0.0d+00) go to 130
      go to 110
  105 if(abserr/abs(result).gt.errsum/abs(area))go to 115
c
c           test on divergence
c
  110 if(ksgn.eq.(-1).and.max(abs(result),abs(area)).le.
     * defabs*0.1d-01) go to 130
      if(0.1d-01.gt.(result/area).or.(result/area).gt.0.1d+03.
     *or.errsum.gt.abs(area)) ier = 6
      go to 130
c
c           compute global integral sum.
c
  115 result = 0.0d+00
      do 120 k = 1,last
        result = result+rlist(k)
  120 continue
      abserr = errsum
  130 neval = 30*last-15
      if(inf.eq.2) neval = 2*neval
      if(ier.gt.2) ier=ier-1
      return
      end


      subroutine nspdqelg(n,epstab,result,abserr,res3la,nres)
      implicit none
c***begin prologue  dqelg
c***refer to  dqagie,dqagoe,dqagpe,dqagse
c***routines called  d1mach
c***revision date  830518   (yymmdd)
c***keywords  epsilon algorithm, convergence acceleration,
c             extrapolation
c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
c           de doncker,elise,appl. math & progr. div. - k.u.leuven
*
c***slightly modified by bruno pincon for nsp: transformation of the 
*           toward a cleaner fortran
*
c***purpose  the routine determines the limit of a given sequence of
c            approximations, by means of the epsilon algorithm of
c            p.wynn. an estimate of the absolute error is also given.
c            the condensed epsilon table is computed. only those
c            elements needed for the computation of the next diagonal
c            are preserved.
c***description
c
c           epsilon algorithm
c           standard fortran subroutine
c           double precision version
c
c           parameters
c              n      - integer
c                       epstab(n) contains the new element in the
c                       first column of the epsilon table.
c
c              epstab - double precision
c                       vector of dimension 52 containing the elements
c                       of the two lower diagonals of the triangular
c                       epsilon table. the elements are numbered
c                       starting at the right-hand corner of the
c                       triangle.
c
c              result - double precision
c                       resulting approximation to the integral
c
c              abserr - double precision
c                       estimate of the absolute error computed from
c                       result and the 3 previous results
c
c              res3la - double precision
c                       vector of dimension 3 containing the last 3
c                       results
c
c              nres   - integer
c                       number of calls to the routine
c                       (should be zero at first call)
c
c***end prologue  dqelg
c
      double precision abserr,delta1,delta2,delta3,dlamch,
     *  epmach,epsinf,epstab,error,err1,err2,err3,e0,e1,e1abs,e2,e3,
     *  oflow,res,result,res3la,ss,tol1,tol2,tol3
      integer i,ib,ib2,ie,indx,k1,k2,k3,limexp,n,newelm,nres,num
      dimension epstab(52),res3la(3)
c
c           list of major variables
c           -----------------------
c
c           e0     - the 4 elements on which the computation of a new
c           e1       element in the epsilon table is based
c           e2
c           e3                 e0
c                        e3    e1    new
c                              e2
c           newelm - number of elements to be computed in the new
c                    diagonal
c           error  - error = abs(e1-e0)+abs(e2-e1)+abs(new-e2)
c           result - the element in the new diagonal with least value
c                    of error
c
c           machine dependent constants
c           ---------------------------
c
c           epmach is the largest relative spacing.
c           oflow is the largest positive magnitude.
c           limexp is the maximum number of elements the epsilon
c           table can contain. if this number is reached, the upper
c           diagonal of the epsilon table is deleted.
c
c***first executable statement  dqelg
      epmach = dlamch('p')
      oflow = dlamch('o')
      nres = nres+1
      abserr = oflow
      result = epstab(n)
      if(n.lt.3) go to 100
      limexp = 50
      epstab(n+2) = epstab(n)
      newelm = (n-1)/2
      epstab(n) = oflow
      num = n
      k1 = n
      do i = 1,newelm
         k2 = k1-1
         k3 = k1-2
         res = epstab(k1+2)
         e0 = epstab(k3)
         e1 = epstab(k2)
         e2 = res
         e1abs = abs(e1)
         delta2 = e2-e1
         err2 = abs(delta2)
         tol2 = max(abs(e2),e1abs)*epmach
         delta3 = e1-e0
         err3 = abs(delta3)
         tol3 = max(e1abs,abs(e0))*epmach
         if(err2.le.tol2.and.err3.le.tol3) then
c
c           if e0, e1 and e2 are equal to within machine
c           accuracy, convergence is assumed.
c           result = e2
c           abserr = abs(e1-e0)+abs(e2-e1)
c
            result = res
            abserr = err2+err3
c           ***jump out of do-loop
            go to 100
         endif
         e3 = epstab(k1)
         epstab(k1) = e1
         delta1 = e1-e3
         err1 = abs(delta1)
         tol1 = max(e1abs,abs(e3))*epmach
c     
c           if two elements are very close to each other, omit
c           a part of the table by adjusting the value of n
c
         if(err1.le.tol1.or.err2.le.tol2.or.err3.le.tol3) then
            n = i+i-1
c           ***jump out of do-loop
            go to 50
         endif

         ss = 1.d0/delta1 + 1.d0/delta2 - 1.d0/delta3
         epsinf = abs(ss*e1)
c
c           test to detect irregular behaviour in the table, and
c           eventually omit a part of the table adjusting the value
c           of n.
c
         if(epsinf.le.0.1d-03) then
            n = i+i-1
c           ***jump out of do-loop
            go to 50
         endif
c
c           compute a new element and eventually adjust
c           the value of result.
c
         res = e1 + 1.d0/ss
         epstab(k1) = res
         k1 = k1-2
         error = err2+abs(res-e2)+err3
         if(error.le.abserr) then
            abserr = error
            result = res
         endif
      enddo
c
c           shift the table.
c
   50 if(n.eq.limexp) n = 2*(limexp/2)-1
      ib = 1
      if((num/2)*2.eq.num) ib = 2
      ie = newelm+1
      do i=1,ie
        ib2 = ib+2
        epstab(ib) = epstab(ib2)
        ib = ib2
      enddo
      if(num.ne.n) then
         indx = num-n+1
         do i = 1,n
            epstab(i)= epstab(indx)
            indx = indx+1
         enddo
      endif
      if(nres.lt.4) then
         res3la(nres) = result
         abserr = oflow
      else
c        compute error estimate
         abserr = abs(result-res3la(3))+abs(result-res3la(2))
     *           +abs(result-res3la(1))
         res3la(1) = res3la(2)
         res3la(2) = res3la(3)
         res3la(3) = result
      endif
  100 abserr = max(abserr,5.d0*epmach*abs(result))
      return
      end


      subroutine nspdqpsrt(limit,last,maxerr,ermax,elist,iord,nrmax)
      implicit none
      integer limit, last, maxerr, iord(last), nrmax
      double precision ermax, elist(last)
c***begin prologue  dqpsrt
c***refer to  dqage,dqagie,dqagpe,dqawse
c***routines called  (none)
c***revision date  810101   (yymmdd)
c***keywords  sequential sorting
c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
*
c***slightly modified by bruno pincon for nsp: transformation of the 
*           toward a cleaner fortran

c***purpose  this routine maintains the descending ordering in the
c            list of the local error estimated resulting from the
c            interval subdivision process. at each call two error
c            estimates are inserted using the sequential search
c            method, top-down for the largest error estimate and
c            bottom-up for the smallest error estimate.
c***description
c
c           ordering routine
c           standard fortran subroutine
c           double precision version
c
c           parameters (meaning at output)
c              limit  - integer
c                       maximum number of error estimates the list
c                       can contain
c
c              last   - integer
c                       number of error estimates currently in the list
c
c              maxerr - integer
c                       maxerr points to the nrmax-th largest error
c                       estimate currently in the list
c
c              ermax  - double precision
c                       nrmax-th largest error estimate
c                       ermax = elist(maxerr)
c
c              elist  - double precision
c                       vector of dimension last containing
c                       the error estimates
c
c              iord   - integer
c                       vector of dimension last, the first k elements
c                       of which contain pointers to the error
c                       estimates, such that
c                       elist(iord(1)),...,  elist(iord(k))
c                       form a decreasing sequence, with
c                       k = last if last.le.(limit/2+2), and
c                       k = limit+1-last otherwise
c
c              nrmax  - integer
c                       maxerr = iord(nrmax)
c
c***end prologue  dqpsrt
c
*  local var
      double precision errmax,errmin
      integer i,ibeg,ido,isucc,j,jbnd,jupbn,k
c
c           check whether the list contains more than
c           two error estimates.
c
c***first executable statement  dqpsrt
      if(last.le.2) then
         iord(1) = 1
         iord(2) = 2
         maxerr = iord(nrmax)
         ermax = elist(maxerr)
         return
      endif
c
c           this part of the routine is only executed if, due to a
c           difficult integrand, subdivision increased the error
c           estimate. in the normal case the insert procedure should
c           start after the nrmax-th largest error estimate.
c
      errmax = elist(maxerr)
      if(nrmax.ne.1) then
         ido = nrmax-1
         do i = 1,ido
            isucc = iord(nrmax-1)
c           ***jump out of do-loop
            if(errmax.le.elist(isucc)) go to 30
            iord(nrmax) = isucc
            nrmax = nrmax-1
         enddo
      endif
c
c           compute the number of elements in the list to be maintained
c           in descending order. this number depends on the number of
c           subdivisions still allowed.
c
   30 jupbn = last
      if(last.gt.(limit/2+2)) jupbn = limit+3-last
      errmin = elist(last)
c
c           insert errmax by traversing the list top-down,
c           starting comparison from the element elist(iord(nrmax+1)).
c
      jbnd = jupbn-1
      ibeg = nrmax+1
      if(ibeg.gt.jbnd) then
         iord(jbnd) = maxerr
         iord(jupbn) = last
         go to 90
      endif
      do i=ibeg,jbnd
         isucc = iord(i)
c        ***jump out of do-loop
         if(errmax.ge.elist(isucc)) go to 60
         iord(i-1) = isucc
      enddo
c
c           insert errmin by traversing the list bottom-up.
c
   60 iord(i-1) = maxerr
      k = jbnd
      do j=i,jbnd
         isucc = iord(k)
c        ***jump out of do-loop
         if(errmin.lt.elist(isucc)) then
            iord(k+1) = last
            goto 90
         endif
         iord(k+1) = isucc
         k = k-1
      enddo
      iord(i) = last
c
c           set maxerr and ermax.
c
   90 maxerr = iord(nrmax)
      ermax = elist(maxerr)
      return
      end



      subroutine nspdqk21(fvect, a, b, result, abserr, resabs, resasc, 
     *                    vectflag, stat)
c***begin prologue  dqk21
c***date written   800101   (yymmdd)
c***revision date  830518   (yymmdd)
c***category no.  h2a1a2
c***keywords  21-point gauss-kronrod rules
c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
c
c***modified by bruno pincon for nsp. The call to the integrand function
c            take a different form and when vectflag is TRUE only one call
c            is done over all the integration points. The last added var
c            stat lets to manage problem when the evaluation of fvect
c            by the nsp interpretor failed.
c
c***purpose  to compute i = integral of f over (a,b), with error
c                           estimate
c                       j = integral of abs(f) over (a,b)
c***description
c
c           integration rules
c           standard fortran subroutine
c           double precision version
c
c           parameters
c            on entry
c              fvect    function subprogram defining the integrand
c                       function. Have the form:
c                         stat = fvect(allx, allf,n) 
c                       and return an integer (stat) which is 0
c                       if the evaluation of fvect (generally by the 
c                       nsp interpretor) is OK. Other value imply an
c                       immediate return to the caller. allx is a 
c                       vector of length n and the function must 
c                       compute:
c                         allf(i) = f(allx(i)) for  1 <= i <= n.
c
c              a      - double precision
c                       lower limit of integration
c
c              b      - double precision
c                       upper limit of integration
*
*           vectflag  - boolean true if fvect could be called on a
*                       vector x. This is useful when the fvect is
*                       coded in nsp language to speed up the computation.
*
*
c            on return
c              result - double precision
c                       approximation to the integral i
c                       result is computed by applying the 21-point
c                       kronrod rule (resk) obtained by optimal addition
c                       of abscissae to the 10-point gauss rule (resg).
c
c              abserr - double precision
c                       estimate of the modulus of the absolute error,
c                       which should not exceed abs(i-result)
c
c              resabs - double precision
c                       approximation to the integral j
c
c              resasc - double precision
c                       approximation to the integral of abs(f-i/(b-a))
c                       over (a,b)
*
*                stat - error control for fvect evaluation. Is set
*                       by the fvect code which must return 0 if the evaluation
*                       of fvect is OK (any other values implies immediate
*                       return and stopping of the integration procedure)
c
c***references  (none)
c***routines called  dlamch
c***end prologue  dqk21
c
      implicit none
      double precision a,abserr,b,centr,dhlgth, epmach,fsum,fval1,
     *  fval2,hlgth,resabs,resasc,resg,resk,reskh,result,uflow
      double precision wg(5),wgk(11),xgk(11), dx, allx(21), allf(21)
      integer j,jg, stat
      logical vectflag
      external fvect
      integer fvect
      integer n1, n21
      double precision dlamch
c
c           the abscissae and weights are given for the interval (-1,1).
c           because of symmetry only the positive abscissae and their
c           corresponding weights are given.
c
c           xgk    - abscissae of the 21-point kronrod rule
c                    xgk(2), xgk(4), ...  abscissae of the 10-point
c                    gauss rule
c                    xgk(1), xgk(3), ...  abscissae which are optimally
c                    added to the 10-point gauss rule
c
c           wgk    - weights of the 21-point kronrod rule
c
c           wg     - weights of the 10-point gauss rule
c
c
c gauss quadrature weights and kronrod quadrature abscissae and weights
c as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
c bell labs, nov. 1981.
c
      data wg  (  1) / 0.0666713443 0868813759 3568809893 332 d0 /
      data wg  (  2) / 0.1494513491 5058059314 5776339657 697 d0 /
      data wg  (  3) / 0.2190863625 1598204399 5534934228 163 d0 /
      data wg  (  4) / 0.2692667193 0999635509 1226921569 469 d0 /
      data wg  (  5) / 0.2955242247 1475287017 3892994651 338 d0 /
c
      data xgk (  1) / 0.9956571630 2580808073 5527280689 003 d0 /
      data xgk (  2) / 0.9739065285 1717172007 7964012084 452 d0 /
      data xgk (  3) / 0.9301574913 5570822600 1207180059 508 d0 /
      data xgk (  4) / 0.8650633666 8898451073 2096688423 493 d0 /
      data xgk (  5) / 0.7808177265 8641689706 3717578345 042 d0 /
      data xgk (  6) / 0.6794095682 9902440623 4327365114 874 d0 /
      data xgk (  7) / 0.5627571346 6860468333 9000099272 694 d0 /
      data xgk (  8) / 0.4333953941 2924719079 9265943165 784 d0 /
      data xgk (  9) / 0.2943928627 0146019813 1126603103 866 d0 /
      data xgk ( 10) / 0.1488743389 8163121088 4826001129 720 d0 /
      data xgk ( 11) / 0.0000000000 0000000000 0000000000 000 d0 /
c
      data wgk (  1) / 0.0116946388 6737187427 8064396062 192 d0 /
      data wgk (  2) / 0.0325581623 0796472747 8818972459 390 d0 /
      data wgk (  3) / 0.0547558965 7435199603 1381300244 580 d0 /
      data wgk (  4) / 0.0750396748 1091995276 7043140916 190 d0 /
      data wgk (  5) / 0.0931254545 8369760553 5065465083 366 d0 /
      data wgk (  6) / 0.1093871588 0229764189 9210590325 805 d0 /
      data wgk (  7) / 0.1234919762 6206585107 7958109831 074 d0 /
      data wgk (  8) / 0.1347092173 1147332592 8054001771 707 d0 /
      data wgk (  9) / 0.1427759385 7706008079 7094273138 717 d0 /
      data wgk ( 10) / 0.1477391049 0133849137 4841515972 068 d0 /
      data wgk ( 11) / 0.1494455540 0291690566 4936468389 821 d0 /
c
c
c           list of major variables
c           -----------------------
c
c           centr  - mid point of the interval
c           hlgth  - half-length of the interval
c           fval*  - function value
c           resg   - result of the 10-point gauss formula
c           resk   - result of the 21-point kronrod formula
c           reskh  - approximation to the mean value of f over (a,b),
c                    i.e. to i/(b-a)
c
c
c           machine dependent constants
c           ---------------------------
c
c           epmach is the largest relative spacing.
c           uflow is the smallest positive magnitude.
c
c***first executable statement  dqk21
      epmach = dlamch('p')
      uflow = dlamch('u')
      n1 = 1
      n21 = 21
c
      centr = 0.5d0*(a+b)
      hlgth = 0.5d0*(b-a)
      dhlgth = abs(hlgth)
c
c           compute the 21-point kronrod approximation to
c           the integral, and estimate the absolute error.
c
      do j = 1,10
         dx = dhlgth*xgk(j)
         allx(j) = centr - dx
         allx(22-j) = centr + dx
      enddo
      allx(11) = centr

      if ( vectflag ) then  ! vector evaluation
         stat = fvect(allx, allf, n21)
         if(stat.ne.0) return
      else                  ! scalar evaluation
         do j = 1,21
            stat = fvect(allx(j), allf(j), n1)
            if(stat.ne.0) return
         enddo
      endif
            
      resg = 0.0d+00
      resk = wgk(11)*allf(11)
      resabs = abs(resk)

      jg = 1
      do j = 1,9,2
         fval1 = allf(j)
         fval2 = allf(22-j)
         resk = resk + wgk(j)*(fval1 + fval2)
         resabs = resabs + wgk(j)*(abs(fval1) + abs(fval2))
         fval1 = allf(j+1)
         fval2 = allf(21-j)
         fsum = fval1 + fval2
         resk = resk + wgk(j+1)*fsum
         resabs = resabs + wgk(j+1)*(abs(fval1) + abs(fval2))
         resg = resg + wg(jg)*fsum
         jg = jg + 1
      enddo

      reskh = resk*0.5d0
      resasc = wgk(11)*abs(allf(11)-reskh)
      do j=1,10
        resasc =resasc+wgk(j)*(abs(allf(j)-reskh)+abs(allf(22-j)-reskh))
      enddo

      result = resk*hlgth
      resabs = resabs*dhlgth
      resasc = resasc*dhlgth
      abserr = dabs((resk-resg)*hlgth)
      if (resasc.ne.0.d0.and.abserr.ne.0.d0) then
         abserr = resasc*min(1.d0,(200.d0*abserr/resasc)**1.5d0)
      endif
      if(resabs.gt.uflow/(50.d0*epmach)) then
         abserr = max((50.d0*epmach)*resabs, abserr)
      endif
      return
      end


      subroutine nspdqk21b(fvect, a, c, result1, abserr1, resasc1, 
     *                     result2, abserr2, resasc2, vectflag, stat)
*  same than nspdqk21 but operate on the 2 intervals:
*      [a,(a+b)/2] and [(a+b)/2,c] 
*  thus improving the vectorization in case it is used with a
*  (vectorized) nsp function (the nsp function could be called with 42 
*  points instead of 21 with nspdqk21)
*
*  added by Bruno Pincon
*
      implicit none
      double precision a, c, result1,abserr1, resasc1,result2, abserr2,
     *   resasc2,b,centr1,centr2, dhlgth, epmach,hlgth,resabs1,resg1,
     *   resk1,reskh1,resabs2,resg2,resk2,reskh2, uflow
      double precision wg(5),wgk(11),xgk(11), dx, allx(42), allf(42)
      integer j,jg, stat
      logical vectflag
      external fvect
      integer fvect
      integer n1, n42
      double precision dlamch
c
c           the abscissae and weights are given for the interval (-1,1).
c           because of symmetry only the positive abscissae and their
c           corresponding weights are given.
c
c           xgk    - abscissae of the 21-point kronrod rule
c                    xgk(2), xgk(4), ...  abscissae of the 10-point
c                    gauss rule
c                    xgk(1), xgk(3), ...  abscissae which are optimally
c                    added to the 10-point gauss rule
c
c           wgk    - weights of the 21-point kronrod rule
c
c           wg     - weights of the 10-point gauss rule
c
c
c gauss quadrature weights and kronrod quadrature abscissae and weights
c as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
c bell labs, nov. 1981.
c
      data wg  (  1) / 0.0666713443 0868813759 3568809893 332 d0 /
      data wg  (  2) / 0.1494513491 5058059314 5776339657 697 d0 /
      data wg  (  3) / 0.2190863625 1598204399 5534934228 163 d0 /
      data wg  (  4) / 0.2692667193 0999635509 1226921569 469 d0 /
      data wg  (  5) / 0.2955242247 1475287017 3892994651 338 d0 /
c
      data xgk (  1) / 0.9956571630 2580808073 5527280689 003 d0 /
      data xgk (  2) / 0.9739065285 1717172007 7964012084 452 d0 /
      data xgk (  3) / 0.9301574913 5570822600 1207180059 508 d0 /
      data xgk (  4) / 0.8650633666 8898451073 2096688423 493 d0 /
      data xgk (  5) / 0.7808177265 8641689706 3717578345 042 d0 /
      data xgk (  6) / 0.6794095682 9902440623 4327365114 874 d0 /
      data xgk (  7) / 0.5627571346 6860468333 9000099272 694 d0 /
      data xgk (  8) / 0.4333953941 2924719079 9265943165 784 d0 /
      data xgk (  9) / 0.2943928627 0146019813 1126603103 866 d0 /
      data xgk ( 10) / 0.1488743389 8163121088 4826001129 720 d0 /
      data xgk ( 11) / 0.0000000000 0000000000 0000000000 000 d0 /
c
      data wgk (  1) / 0.0116946388 6737187427 8064396062 192 d0 /
      data wgk (  2) / 0.0325581623 0796472747 8818972459 390 d0 /
      data wgk (  3) / 0.0547558965 7435199603 1381300244 580 d0 /
      data wgk (  4) / 0.0750396748 1091995276 7043140916 190 d0 /
      data wgk (  5) / 0.0931254545 8369760553 5065465083 366 d0 /
      data wgk (  6) / 0.1093871588 0229764189 9210590325 805 d0 /
      data wgk (  7) / 0.1234919762 6206585107 7958109831 074 d0 /
      data wgk (  8) / 0.1347092173 1147332592 8054001771 707 d0 /
      data wgk (  9) / 0.1427759385 7706008079 7094273138 717 d0 /
      data wgk ( 10) / 0.1477391049 0133849137 4841515972 068 d0 /
      data wgk ( 11) / 0.1494455540 0291690566 4936468389 821 d0 /
c
      epmach = dlamch('p')
      uflow = dlamch('u')
      n1 = 1
      n42 = 42
c
      b = 0.5d0*(a+c)
      centr1 = 0.5d0*(a+b)
      hlgth = 0.5d0*(b-a)
      dhlgth = abs(hlgth)
      centr2 = 0.5d0*(b+c)
c
c           compute the 21-point kronrod approximation to
c           the integral, and estimate the absolute error.
c
      do j = 1,10
         dx = dhlgth*xgk(j)
         allx(j)    = centr1 - dx
         allx(22-j) = centr1 + dx
         allx(j+21) = centr2 + dx
         allx(43-j) = centr2 - dx
      enddo
      allx(11) = centr1
      allx(32) = centr2

      if ( vectflag ) then  ! vector evaluation
         stat = fvect(allx, allf, n42)
         if(stat.ne.0) return
      else                  ! scalar evaluation
         do j = 1,42
            stat = fvect(allx(j), allf(j), n1)
            if(stat.ne.0) return
         enddo
      endif
            
      resg1 = 0.0d+00
      resg2 = 0.0d+00
      resk1 = wgk(11)*allf(11)
      resk2 = wgk(11)*allf(32)
      resabs1 = abs(resk1)
      resabs2 = abs(resk2)

      jg = 1
      do j = 1,9,2
         resk1 = resk1 + wgk(j)*(allf(j) + allf(22-j))
         resabs1 = resabs1 + wgk(j)*(abs(allf(j)) + abs(allf(22-j)))
         resk1 = resk1 + wgk(j+1)*(allf(j+1) + allf(21-j))
         resabs1 = resabs1 + wgk(j+1)*(abs(allf(j+1)) + abs(allf(21-j)))
         resg1 = resg1 + wg(jg)*(allf(j+1) + allf(21-j))

         resk2 = resk2 + wgk(j)*(allf(j+21) + allf(43-j))
         resabs2 = resabs2 + wgk(j)*(abs(allf(j+21)) + abs(allf(43-j)))
         resk2 = resk2 + wgk(j+1)*(allf(j+22) + allf(42-j))
         resabs2 = resabs2 +wgk(j+1)*(abs(allf(j+22)) + abs(allf(42-j)))
         resg2 = resg2 + wg(jg)*(allf(j+22) + allf(42-j))

         jg = jg + 1
      enddo

      reskh1 = resk1*0.5d0
      reskh2 = resk2*0.5d0
      resasc1 = wgk(11)*abs(allf(11)-reskh1)
      resasc2 = wgk(11)*abs(allf(32)-reskh2)
      do j=1,10
        resasc1 = resasc1 + wgk(j)*(  abs(allf(j)-reskh1) 
     *                              + abs(allf(22-j)-reskh1))
        resasc2 = resasc2 + wgk(j)*(  abs(allf(j+21)-reskh2) 
     *                              + abs(allf(43-j)-reskh2))
      enddo

      result1 = resk1*hlgth
      resabs1 = resabs1*dhlgth
      resasc1 = resasc1*dhlgth
      abserr1 = abs((resk1-resg1)*hlgth)
      if (resasc1.ne.0.d0.and.abserr1.ne.0.d0) then
         abserr1 = resasc1*min(1.d0,(200.d0*abserr1/resasc1)**1.5d0)
      endif
      if(resabs1.gt.uflow/(50.d0*epmach)) then
         abserr1 = max((50.d0*epmach)*resabs1, abserr1)
      endif

      result2 = resk2*hlgth
      resabs2 = resabs2*dhlgth
      resasc2 = resasc2*dhlgth
      abserr2 = abs((resk2-resg2)*hlgth)
      if (resasc2.ne.0.d0.and.abserr2.ne.0.d0) then
         abserr2 = resasc2*min(1.d0,(200.d0*abserr2/resasc2)**1.5d0)
      endif
      if(resabs2.gt.uflow/(50.d0*epmach)) then
         abserr2 = max((50.d0*epmach)*resabs2, abserr2)
      endif

      return
      end


      subroutine nspdqk15i(fvect,boun,inf,a,b,result,abserr,resabs,
     *                     resasc,vectflag,stat)
c***begin prologue  dqk15i
c***date written   800101   (yymmdd)
c***revision date  830518   (yymmdd)
c***category no.  h2a3a2,h2a4a2
c***keywords  15-point transformed gauss-kronrod rules
c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
c
c***modified by bruno pincon for nsp. The call to the integrand function
c            take a different form and when vectflag is TRUE only one call
c            is done over all the integration points. The last added var
c            stat lets to manage problem when the evaluation of fvect
c            by the nsp interpretor failed.
c
cc***purpose  the original (infinite integration range is mapped
c            onto the interval (0,1) and (a,b) is a part of (0,1).
c            it is the purpose to compute
c            i = integral of transformed integrand over (a,b),
c            j = integral of abs(transformed integrand) over (a,b).
c***description
c
c           integration rule
c           standard fortran subroutine
c           double precision version
c
c           parameters
c            on entry
c              fvect    function subprogram defining the integrand
c                       function. Have the form:
c                         stat = fvect(allx, allf,n) 
c                       and return an integer (stat) which is 0
c                       if the evaluation of fvect (generally by the 
c                       nsp interpretor) is OK. Other value imply an
c                       immediate return to the caller. allx is a 
c                       vector of length n and the function must 
c                       compute:
c                         allf(i) = f(allx(i)) for  1 <= i <= n.
c
c              boun   - double precision
c                       finite bound of original integration
c                       range (set to zero if inf = +2)
c
c              inf    - integer
c                       if inf = -1, the original interval is
c                                   (-infinity,bound),
c                       if inf = +1, the original interval is
c                                   (bound,+infinity),
c                       if inf = +2, the original interval is
c                                   (-infinity,+infinity) and
c                       the integral is computed as the sum of two
c                       integrals, one over (-infinity,0) and one over
c                       (0,+infinity).
c
c              a      - double precision
c                       lower limit for integration over subrange
c                       of (0,1)
c
c              b      - double precision
c                       upper limit for integration over subrange
c                       of (0,1)
*
*           vectflag  - boolean true if fvect could be called on a
*                       vector x. This is useful when the fvect is
*                       coded in nsp language to speed up the computation.
c
c
c
c            on return
c              result - double precision
c                       approximation to the integral i
c                       result is computed by applying the 15-point
c                       kronrod rule(resk) obtained by optimal addition
c                       of abscissae to the 7-point gauss rule(resg).
c
c              abserr - double precision
c                       estimate of the modulus of the absolute error,
c                       which should equal or exceed abs(i-result)
c
c              resabs - double precision
c                       approximation to the integral j
c
c              resasc - double precision
c                       approximation to the integral of
c                       abs((transformed integrand)-i/(b-a)) over (a,b)
*
*                stat - error control for fvect evaluation. Is set
*                       by the fvect code which must return 0 if the evaluation
*                       of fvect is OK (any other values implies immediate
*                       return and stopping of the integration procedure)
c
c***references  (none)
c***routines called  dlamch
c***end prologue  dqk15i
c
      implicit none
      double precision a,abserr,b,boun,centr,dinf, dlamch,epmach,
     *  fsum,hlgth, resabs,resasc,resg,resk,reskh,result,uflow
      logical vectflag
      integer inf,j,stat, nb, n1
      external fvect
      integer fvect
c
      double precision xgk(8),wgk(8),wg(8)
      double precision allx(30), allf(30), absc, absc1(7), absc2(7)
c
c           the abscissae and weights are supplied for the interval
c           (-1,1).  because of symmetry only the positive abscissae and
c           their corresponding weights are given.
c
c           xgk    - abscissae of the 15-point kronrod rule
c                    xgk(2), xgk(4), ... abscissae of the 7-point
c                    gauss rule
c                    xgk(1), xgk(3), ...  abscissae which are optimally
c                    added to the 7-point gauss rule
c
c           wgk    - weights of the 15-point kronrod rule
c
c           wg     - weights of the 7-point gauss rule, corresponding
c                    to the abscissae xgk(2), xgk(4), ...
c                    wg(1), wg(3), ... are set to zero.
c
      data wg(1) / 0.0d0 /
      data wg(2) / 0.1294849661 6886969327 0611432679 082d0 /
      data wg(3) / 0.0d0 /
      data wg(4) / 0.2797053914 8927666790 1467771423 780d0 /
      data wg(5) / 0.0d0 /
      data wg(6) / 0.3818300505 0511894495 0369775488 975d0 /
      data wg(7) / 0.0d0 /
      data wg(8) / 0.4179591836 7346938775 5102040816 327d0 /
c
      data xgk(1) / 0.9914553711 2081263920 6854697526 329d0 /
      data xgk(2) / 0.9491079123 4275852452 6189684047 851d0 /
      data xgk(3) / 0.8648644233 5976907278 9712788640 926d0 /
      data xgk(4) / 0.7415311855 9939443986 3864773280 788d0 /
      data xgk(5) / 0.5860872354 6769113029 4144838258 730d0 /
      data xgk(6) / 0.4058451513 7739716690 6606412076 961d0 /
      data xgk(7) / 0.2077849550 0789846760 0689403773 245d0 /
      data xgk(8) / 0.0000000000 0000000000 0000000000 000d0 /
c
      data wgk(1) / 0.0229353220 1052922496 3732008058 970d0 /
      data wgk(2) / 0.0630920926 2997855329 0700663189 204d0 /
      data wgk(3) / 0.1047900103 2225018383 9876322541 518d0 /
      data wgk(4) / 0.1406532597 1552591874 5189590510 238d0 /
      data wgk(5) / 0.1690047266 3926790282 6583426598 550d0 /
      data wgk(6) / 0.1903505780 6478540991 3256402421 014d0 /
      data wgk(7) / 0.2044329400 7529889241 4161999234 649d0 /
      data wgk(8) / 0.2094821410 8472782801 2999174891 714d0 /
c
c
c           list of major variables
c           -----------------------
c
c           centr  - mid point of the interval
c           hlgth  - half-length of the interval
c           absc*  - abscissa
c           tabsc* - transformed abscissa
c           fval*  - function value
c           resg   - result of the 7-point gauss formula
c           resk   - result of the 15-point kronrod formula
c           reskh  - approximation to the mean value of the transformed
c                    integrand over (a,b), i.e. to i/(b-a)
c
c           machine dependent constants
c           ---------------------------
c
c           epmach is the largest relative spacing.
c           uflow is the smallest positive magnitude.
c
c***first executable statement  dqk15i
      epmach = dlamch('p')
      uflow = dlamch('u')
      dinf = min(1,inf)
      if ( inf .eq. 2 ) then
         nb = 30
      else
         nb = 15
      endif
      n1 = 1
*
*     compute all integration points
*
      centr = 0.5d0*(a+b)
      hlgth = 0.5d0*(b-a)

      allx(8) = boun+dinf*(1.d0-centr)/centr
      do j = 1,7
         absc = hlgth*xgk(j)
         absc1(j) = centr-absc
         absc2(j) = centr+absc
         allx(j) = boun+dinf*(1.d0-absc1(j))/absc1(j)
         allx(16-j) = boun+dinf*(1.d0-absc2(j))/absc2(j)
      enddo
      if ( inf .eq. 2 ) then
         do j = 1,15
            allx(15+j) = -allx(j)
         enddo
      endif

*
*     compute f on the integration points
*
      if ( vectflag ) then
         stat = fvect(allx, allf, nb)
         if (stat .ne. 0) return
      else
         do j = 1,nb
            stat = fvect(allx(j), allf(j), n1)
            if (stat.ne.0) return
         enddo
      endif

      if ( inf .eq. 2 ) then
         do j = 1,15
            allf(j) = allf(j) + allf(15+j)
         enddo
      endif

*
*     compute the integral approximation and estimates
*
      allf(8) = (allf(8)/centr)/centr
      resg = wg(8)*allf(8)
      resk = wgk(8)*allf(8)
      resabs = abs(resk)
      do j = 1,7
         allf(j) = (allf(j)/absc1(j))/absc1(j)
         allf(16-j) = (allf(16-j)/absc2(j))/absc2(j)
         fsum = allf(j) + allf(16-j)
         resg = resg+wg(j)*fsum
         resk = resk+wgk(j)*fsum
         resabs = resabs+wgk(j)*(abs(allf(j))+abs(allf(16-j)))
      enddo

      reskh = resk*0.5d0
      resasc = wgk(8)*abs(allf(8)-reskh)
      do j=1,7
        resasc=resasc+wgk(j)*(abs(allf(j)-reskh)+abs(allf(16-j)-reskh))
      enddo

      result = resk*hlgth
      resasc = resasc*hlgth
      resabs = resabs*hlgth
      abserr = abs((resk-resg)*hlgth)
      if ( resasc.ne.0.d0 .and. abserr.ne.0.d0) then
         abserr = resasc*min(1.d0,(200.d0*abserr/resasc)**1.5d0)
      endif
      if ( resabs .gt. uflow/(50.d0*epmach) ) then
         abserr = max((epmach*50.d0)*resabs,abserr)
      endif
      return
      end
