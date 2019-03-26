function [frq,bnds,splitf]=calfrq_p(hnum,hden, fmin,fmax,varargopt)
  [frq,bnds,splitf]=calfrq_common(hnum,hden,fmin,fmax,varargopt(:));
endfunction

function [frq,bnds,splitf]=calfrq_r(h,fmin,fmax,varargopt)
  if ~varargopt.iskey['dt'] then varargopt.dt=r.dt;end
  if ~varargopt.iskey['dom'] then varargopt.dom=r.dom;end
  [frq,bnds,splitf]=calfrq_common(h.num,h.den,fmin,fmax,varargopt(:));
endfunction

function [frq,bnds,splitf]=calfrq_linearsys(sl,fmin,fmax,varargopt)
  if ~varargopt.iskey['dt'] then varargopt.dt=sl.dt;end
  if ~varargopt.iskey['dom'] then varargopt.dom=sl.dom;end
  h=ss2tf(sl);
  [frq,bnds,splitf]=calfrq_common(h.num,h.den,fmin,fmax,varargopt(:));
endfunction

function [frq,bnds,splitf]=calfrq_common(hnum,hden,fmin,fmax,dom = 'c',dt = 1)
  // frequency response discretization
  // when system is given by two polynomial matrices
  // utility function used by repfreq

  if type(hnum,'short') <> 'p' then error("Argument should be a polynomial matrix");end
  if type(hden,'short') <> 'p' then error("Argument should be a polynomial matrix");end

  function f=cont_sel(r,fmin,fmax,tol)
    f=[];
    if size(r,'*')==0 then return,end
    f=imag(r(find((abs(real(r)) <= tol*abs(r)) & (imag(r) >= 0))));
    if ~isempty(f) then
      f=f(find((f > fmin-tol) & (f < fmax+tol)));
    end
  endfunction
  
  function f=disc_sel(r,fmin,fmax,dom,tol)
    f=[];
    if size(r,'*')==0 then return,end
    f=r(find(((abs(abs(r)-ones(size(r)))) <= tol) & (imag(r) >= 0)));
    if ~isempty(f) then
      f=atan(imag(f),real(f));nf=prod(size(f));
      for k=1:nf do
        kk=int((fmax-f(k))/(2*%pi))+1;f=[f;f(1:nf)+2*%pi*kk*ones(nf,1)];end
      f=f(find((f > fmin-tol) & (f < fmax+tol)));
    end
  endfunction

  function y=freq_zzz(hnum,hden,x)
    // unused : we use the C-interfaced function
    ce_n=horner(hnum,x,vdim = %t);
    ce_d=horner(hden,x,vdim = %t);
    y=[];
    for i=1:size(ce_n,'*') do
      y.concatr[ce_n{i} ./ce_d{i}];
    end
  endfunction

  eps=1.E-14;//minimum absolute lower frequency
  k=0.001;// Minimum relative prediction error in the nyquist plan
  epss=0.002;// minimum frequency distance with a singularity
  nptmax=5000;//maximum number of discretisation points
  tol=0.01;// Tolerance for testing pure imaginary numbers
  
  [m,n]=size(hnum);

  // undefined is considered as c 
  if dom == 'u' then dom = 'c';end 
    
  if or(dom==['d','s','u']) then
    nyq_frq=1/2/dt;
    if fmax > nyq_frq then
      printf("calfrq: Frequencies beyond Nyquist frequency are ignored.\n");
      fmax=min(fmax,nyq_frq);
    end
    if fmin < -nyq_frq then
      printf("calfrq: Negative frequencies below Nyquist frequency are ignored.\n");
      fmin=max(fmin,-nyq_frq);
    end
  end
  // Use symmetry to reduce the range
  // --------------------------------
  if fmin < 0 & fmax >= 0 then
    [frq,bnds,splitf]=calfrq_common(hnum,hden,eps,-fmin,dom = dom,dt = dt)
    ns1=size(splitf,'*')-1;
    nsp=size(frq,'*');
    bnds=[bnds(1),bnds(2),-bnds(4),-bnds(3)];

    if fmax > eps then
      if fmax==-fmin then
        splitf=[1,(nsp+2)*ones(1,ns1)-splitf(1,$:-1:2), ...
                nsp*ones(size(ns1))+splitf(1,2:$)];
        bnds=[bnds(1),bnds(2),min(bnds(3),-bnds(3)),max(bnds(4),-bnds(4))];
        frq=[-frq($:-1:1),frq]
      else
        [frq2,bnds2,splitf2]=calfrq_common(hnum,hden,eps,fmax,dom = dom,dt = dt);
        ns2=size(splitf2,'*')-1
        splitf=[1,(nsp+2)*ones(1,ns1)-splitf(1,$:-1:2), ...
                nsp*ones(size(ns2))+splitf2(1,2:$)];
        bnds=[min(bnds(1),bnds2(1)),max(bnds(2),bnds2(2)),min(bnds(3),bnds2(3)), ...
              max(bnds(4),bnds2(4))];
        frq=[-frq($:-1:1),frq2]
      end
      return
    else
      frq=-frq($:-1:1);
      nsp=size(frq,'*');
      splitf=[1,(nsp+2)*ones(1,ns1)-splitf($:-1:2)]
      bnds=bnds;
      return;
    end
  elseif fmin < 0 & fmax <= 0 then
    [frq,bnds,splitf]=calfrq_common(hnum,hden,-fmax,-fmin,dom = dom,dt = dt)
    ns1=size(splitf,'*')-1;
    frq=-frq($:-1:1);
    nsp=size(frq,'*');
    splitf=[1,(nsp+2)*ones(1,ns1)-splitf($:-1:2)]
    bnds=[bnds(1),bnds(2),-bnds(4),-bnds(3)];
    return;
  elseif fmin >= fmax then
    error("calfrq: Wrong values for fmin and fmax (fmin < fmax expected).\n");
    return;
  end

  // Compute dicretisation over a given range
  // ----------------------------------------
  splitf=[]
  if fmin==0 then fmin=min(1E-14,fmax/10);end
  //
  l10=log(10)
  // Locate singularities to avoid them
  // ----------------------------------
  if dom.equal['c'] then
    //selection function for singularities in the frequency range
    c=2*%pi;f_select=cont_sel;
  else
    c=2*%pi*dt;f_select=disc_sel;
  end

  sing=[];zers=[];
  fmin=c*fmin,fmax=c*fmax;

  for i=1:m do sing.concatd[f_select(roots(hden(i)),fmin,fmax,tol)];end

  // pp=gsort(sing','g','i');npp=size(pp,'*');
  [pp,ipp]=gsort(real(sing'),'g','i');npp=size(pp,'*');
  vpp=sing';pp=vpp(ipp);

  // singularities just on the left of the range
  kinf=find(pp < fmin)
  if ~isempty(kinf) then
    fmin=fmin+tol;
    pp(kinf)=[];
  end

  // singularities just on the right of the range
  ksup=find(pp >= fmax)
  if ~isempty(ksup) then
    fmax=fmax-tol;
    pp(ksup)=[];
  end

  //check for nearly multiple singularities
  if ~isempty(pp) then
    dpp=pp(2:$)-pp(1:$-1)
    keq=find(abs(dpp) < 2*epss)
    if ~isempty(keq) then pp(keq)=[],end
  end

  if ~isempty(pp) then
    frqs=[fmin,real(matrix([(1-epss)*pp;(1+epss)*pp],2*size(pp,'*'),1)'),fmax]
  else
    frqs=[fmin,fmax]
  end
  nfrq=size(frqs,'*');

  // Evaluate bounds of nyquist plot
  //-------------------------------

  xt=[];Pas=[]
  for i=1:2:nfrq-1 do
    w=logspace(log(frqs(i))/log(10),log(frqs(i+1))/log(10),100);
    xt=[xt,w]
    Pas=[Pas,w(2)-w(1)]
  end
  if dom.equal['c'] then
    rf=freq(hnum,hden,%i*xt);
  else
    rf=freq(hnum,hden,exp(%i*xt));
  end
  //
  xmin=min(real(rf));xmax=max(real(rf));
  ymin=min(imag(rf));ymax=max(imag(rf));
  bnds=[xmin,xmax,ymin,ymax];
  dx=max([xmax-xmin,1]);dy=max([ymax-ymin,1]);

  // Compute discretization with a step adaptation method
  // ----------------------------------------------------
  frq=[];
  i=1;
  nptr=nptmax;// number of unused discretization points
  l10last=log(frqs($))/log(10);
  while i < nfrq do
    f0=frqs(i);fmax=frqs(i+1);
    while f0==fmax do
      i=i+2;
      f=frqs(i);fmax=frqs(i+1);
    end
    frq=[frq,f0];
    pas=Pas(floor(i/2)+1)
    splitf=[splitf,size(frq,'*')];

    f=min(f0+pas,fmax);

    if dom.equal['c'] then
      // continuous case
      while f0 < fmax do
        rf0=freq(hnum,hden,(%i*f0));
        rfc=freq(hnum,hden,%i*f);
        // compute prediction error
        epsd=pas/100;//epsd=1.d-8

        rfd=(freq(hnum,hden,%i*(f0+epsd))-rf0)/(epsd);
        rfp=rf0+pas*rfd;

        e=max([abs(imag(rfp-rfc))/dy;abs(real(rfp-rfc))/dx])
        if (e > k) then
          rf0=freq(hnum,hden,(%i*f0));
          rfc=freq(hnum,hden,%i*f);
          // compute prediction error
          epsd=pas/100;//epsd=1.d-8

          rfd=(freq(hnum,hden,%i*(f0+epsd))-rf0)/(epsd);
          rfp=rf0+pas*rfd;

          e=max([abs(imag(rfp-rfc))/dy;abs(real(rfp-rfc))/dx])
          // compute minmum frequency logarithmic step to ensure a maximum
          //of nptmax points to discretize
          pasmin=f0*(10^((l10last-log(f0)/log(10))/(nptr+1))-1)
          pas=pas/2
          if pas < pasmin then
            pas=pasmin
            frq=[frq,f];nptr=max([1,nptr-1])
            f0=f;f=min(f0+pas,fmax)
          else
            f=min(f0+pas,fmax)
          end
        elseif e < k/2 then
          pas=2*pas
          frq=[frq,f];nptr=max([1,nptr-1])
          f0=f;f=min(f0+pas,fmax),
        else
          frq=[frq,f];nptr=max([1,nptr-1])
          f0=f;f=min(f0+pas,fmax),
        end
      end
    else
      // discrete case
      pas=pas/dt
      while f0 < fmax do
        rf0=freq(hnum,hden,exp(%i*f0))
        rfd=dt*(freq(hnum,hden,exp(%i*(f0+pas/100)))-rf0)/(pas/100);
        rfp=rf0+pas*rfd
        rfc=freq(hnum,hden,exp(%i*f));
        e=max([abs(imag(rfp-rfc))/dy;abs(real(rfp-rfc))/dx])
        if (e > k) then
          pasmin=f0*(10^((l10last-log(f0)/log(10))/(nptr+1))-1)
          pas=pas/2
          if pas < pasmin then
            pas=pasmin
            frq=[frq,f];nptr=max([1,nptr-1])
            f0=f;f=min(f0+pas,fmax)
          else
            f=min(f0+pas,fmax)
          end
        elseif e < k/2 then
          pas=2*pas
          frq=[frq,f];nptr=max([1,nptr-1])
          f0=f;f=min(f0+pas,fmax),
        else
          frq=[frq,f];nptr=max([1,nptr-1])
          f0=f;f=min(f0+pas,fmax),
        end
      end
    end
    i=i+2
  end
  frq(size(frq,'*'))=fmax;
  frq=frq/c;
endfunction

