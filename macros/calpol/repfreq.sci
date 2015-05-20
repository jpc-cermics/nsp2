function [frq,rep,splitf]=repfreq_p(hnum,hden,varargopt)
// varargopt: frq fmin fmax step dom='c'
// Copyright CECILL INRIA (from scilab)
  
  function [frq,bnds,splitf]=calfrq_p(hnum,hden,fmin,fmax,dom='c')
  // frequency response discretization
  // when system is given by two polynomial matrices
  // utility function used by repfreq
    
    if type(hnum,'short')<>'p' then error("Argument should be a polynomial matrix");end
    if type(hden,'short')<>'p' then error("Argument should be a polynomial matrix");end

    function f=cont_sel(r,fmin,fmax,tol)
      f=[];
      if size(r,'*') ==0 then return,end;
      f=imag(r(find((abs(real(r))<=tol*abs(r))&(imag(r)>=0))));
      if ~isempty(f) then
	f=f(find((f>fmin-tol)&(f<fmax+tol)));
      end;
    endfunction

    function f=disc_sel(r,fmin,fmax,dom,tol)
      f=[];
      if size(r,'*')==0 then return,end;
      f=r(find( ((abs(abs(r)-ones(r)))<=tol)&(imag(r)>=0)));
      if ~isempty(f) then ;
	f=atan(imag(f),real(f));nf=prod(size(f));
	for k=1:nf
	  kk=int((fmax-f(k))/(2*%pi))+1;;
	  f=[f;f(1:nf)+2*%pi*kk*ones(nf,1)];;
	end;
	f=f(find((f>fmin-tol)&(f<fmax+tol)));
      end
    endfunction

    function y=freq(hnum,hden,x)
      ce_n=horner(hnum,x,vdim=%t);
      ce_d=horner(hden,x,vdim=%t);
      y=[];
      for i=1:size(ce_n,'*')
	y.concatr[ce_n{i}./ce_d{i}];
      end
    endfunction

    eps=1.d-14   //minimum absolute lower frequency
    k=0.001;     // Minimum relative prediction error in the nyquist plan
    epss=0.002   // minimum frequency distance with a singularity
    nptmax=5000  //maximum number of discretisation points
    tol=0.01     // Tolerance for testing pure imaginary numbers

    [m,n]=size(hnum);

    select dom
     case 'd' then  dom=1
     case []  then  error("Argument dom should not be an empty matrix");
     case 0   then  error("Argument dom should not be 0");
    end;

    if type(dom,'short')=="m" then
      nyq_frq=1/2/dom;
      if fmax>nyq_frq then
	printf("calfrq: Frequencies beyond Nyquist frequency are ignored.\n");
	fmax=min(fmax,nyq_frq);
      end
      if fmin<-nyq_frq then
	printf("calfrq: Negative frequencies below Nyquist frequency are ignored.\n");
	fmin=max(fmin,-nyq_frq);
      end
    end
    // Use symmetry to reduce the range
    // --------------------------------
    if fmin < 0 & fmax >=0 then
      [frq,bnds,splitf]=calfrq_p(hnum,hden,eps,-fmin)
      ns1=size(splitf,'*')-1;
      nsp=size(frq,'*');
      bnds=[bnds(1),bnds(2),-bnds(4),-bnds(3)];
      
      if fmax>eps then
	if fmax==-fmin then
	  splitf=[1, (nsp+2)*ones(1,ns1)-splitf(1,$:-1:2),nsp*ones(size(ns1))+splitf(1,2:$)];
	  bnds=[bnds(1),bnds(2),min(bnds(3),-bnds(3)),max(bnds(4),-bnds(4))];
	  frq=[-frq($:-1:1),frq]
	else
	  [frq2,bnds2,splitf2]=calfrq_p(hnum,hden,eps,fmax);
	  ns2=size(splitf2,'*')-1
	  splitf=[1, (nsp+2)*ones(1,ns1)-splitf(1,$:-1:2),nsp*ones(size(ns2))+splitf2(1,2:$)];
	  bnds=[min(bnds(1),bnds2(1)),max(bnds(2),bnds2(2)),...
		min(bnds(3),bnds2(3)),max(bnds(4),bnds2(4))];
	  frq=[-frq($:-1:1),frq2]
	end
	return
      else
	frq=-frq($:-1:1);
	nsp=size(frq,'*');
	splitf=[1, (nsp+2)*ones(1,ns1)-splitf($:-1:2)]
	bnds=bnds;
	return;
      end
    elseif fmin<0&fmax<=0 then
      [frq,bnds,splitf]=calfrq_p(hnum,hden,-fmax,-fmin)
      ns1=size(splitf,'*')-1;
      frq=-frq($:-1:1);
      nsp=size(frq,'*');
      splitf=[1, (nsp+2)*ones(1,ns1)-splitf($:-1:2)]
      bnds=[bnds(1),bnds(2),-bnds(4),-bnds(3)];
      return;
    elseif fmin >= fmax then
      error("calfrq: Wrong values for fmin and fmax (fmin < fmax expected).\n");
      return;
    end

    // Compute dicretisation over a given range
    // ----------------------------------------
    splitf=[]
    if fmin==0 then fmin=min(1d-14,fmax/10);end
    //
    l10=log(10)
    // Locate singularities to avoid them
    // ----------------------------------
    if dom.equal['c'] then
      //selection function for singularities in the frequency range
      c=2*%pi; f_select= cont_sel;
    else
      c=2*%pi*dom; f_select= disc_sel;
    end

    sing=[];zers=[];
    fmin=c*fmin,fmax=c*fmax;

    for i=1:m; sing.concatd[f_select(roots(hden(i)),fmin,fmax,tol)];end

    pp=gsort(sing','g','i');npp=size(pp,'*');

    // singularities just on the left of the range
    kinf=find(pp<fmin)
    if ~isempty(kinf) then
      fmin=fmin+tol;
      pp(kinf)=[];
    end

    // singularities just on the right of the range
    ksup=find(pp>=fmax)
    if ~isempty(ksup) then
      fmax=fmax-tol;
      pp(ksup)=[];
    end

    //check for nearly multiple singularities
    if ~isempty(pp) then
      dpp=pp(2:$)-pp(1:$-1)
      keq=find(abs(dpp)<2*epss)
      if ~isempty(keq) then pp(keq)=[],end
    end

    if ~isempty(pp) then
      frqs=[fmin,real(matrix([(1-epss)*pp;(1+epss)*pp],2*size(pp,'*'),1)'), fmax]
    else
      frqs=[fmin,fmax]
    end
    nfrq=size(frqs,'*');

    // Evaluate bounds of nyquist plot
    //-------------------------------

    xt=[];Pas=[]
    for i=1:2:nfrq-1
      w=logspace(log(frqs(i))/log(10),log(frqs(i+1))/log(10),100);
      xt=[xt,w]
      Pas=[Pas w(2)-w(1)]
    end
    if dom.equal['c'] then
      rf=freq(hnum,hden,%i*xt);
    else
      rf=freq(hnum,hden,exp(%i*xt));
    end
    //
    xmin=min(real(rf));xmax=max(real(rf));
    ymin=min(imag(rf));ymax=max(imag(rf));
    bnds=[xmin xmax ymin ymax];
    dx=max([xmax-xmin,1]);dy=max([ymax-ymin,1]);

    // Compute discretization with a step adaptation method
    // ----------------------------------------------------
    frq=[];
    i=1;
    nptr=nptmax; // number of unused discretization points
    l10last=log(frqs($))/log(10);
    while i<nfrq
      f0=frqs(i);fmax=frqs(i+1);
      while f0==fmax do
	i=i+2;
	f=frqs(i);fmax=frqs(i+1);
      end
      frq=[frq,f0];
      pas=Pas(floor(i/2)+1)
      splitf=[splitf size(frq,'*')];

      f=min(f0+pas,fmax);

      if dom.equal['c'] then
	// continuous case
	while f0<fmax
	  rf0=freq(hnum,hden,(%i*f0));
	  rfc=freq(hnum,hden,%i*f);
	  // compute prediction error
	  epsd=pas/100;//epsd=1.d-8

	  rfd=(freq(hnum,hden,%i*(f0+epsd))-rf0)/(epsd);
	  rfp=rf0+pas*rfd;

	  e=max([abs(imag(rfp-rfc))/dy;abs(real(rfp-rfc))/dx])
	  if (e>k) then rf0=freq(hnum,hden,(%i*f0));
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
	    if pas<pasmin then
	      pas=pasmin
	      frq=[frq,f];nptr=max([1,nptr-1])
	      f0=f;f=min(f0+pas,fmax)
	    else
	      f=min(f0+pas,fmax)
	    end
	  elseif e<k/2 then
	    pas=2*pas
	    frq=[frq,f];nptr=max([1,nptr-1])
	    f0=f;f=min(f0+pas,fmax),
	  else
	    frq=[frq,f];nptr=max([1,nptr-1])
	    f0=f;f=min(f0+pas,fmax),
	  end
	end
      else  // discrete case
	pas=pas/dom
	while f0<fmax
	  rf0=freq(hnum,hden,exp(%i*f0))
	  rfd=dom*(freq(hnum,hden,exp(%i*(f0+pas/100)))-rf0)/(pas/100);
	  rfp=rf0+pas*rfd
	  rfc=freq(hnum,hden,exp(%i*f));
	  e=max([abs(imag(rfp-rfc))/dy;abs(real(rfp-rfc))/dx])
	  if (e>k) then
	    pasmin=f0*(10^((l10last-log(f0)/log(10))/(nptr+1))-1)
	    pas=pas/2
	    if pas<pasmin then
	      pas=pasmin
	      frq=[frq,f];nptr=max([1,nptr-1])
	      f0=f;f=min(f0+pas,fmax)
	    else
	      f=min(f0+pas,fmax)
	    end
	  elseif e<k/2 then
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
    frq( size(frq,'*') )=fmax;
    frq=frq/c;
  endfunction
    
  function y=freq(hnum,hden,x)
    ce_n=horner(hnum,x,vdim=%t);
    ce_d=horner(hden,x,vdim=%t);
    y=[];
    for i=1:size(ce_n,'*')
      y.concatr[ce_n{i}./ce_d{i}];
    end
  endfunction

  // compute default values 
  dom = varargopt.find['dom',def='c'];
  if ~(dom.equal['c'] || dom.equal['d'] || ( type(dom,'short')=='m' && sime(dom,'*')==1)) then 
    error("dom should be ''c'' or ''d'' or a scalar");
    return 
  end
  if  dom.equal['d'] then dom=1;end 
  l10=log(10);
  step =  varargopt.find['step',def='auto']; 
  
  if dom=='c' then fmax=1.d3; else fmax=1/(2*dom),end
  fmax= varargopt.find['fmax',def=fmax];
  fmin= varargopt.find['fmin',def='sym'];
  if fmin.equal['sym'] then fmin = -fmax;end;
  // frq 
  frq=  varargopt.find['frq',def=[]];
  
  if isempty(frq) then 
    // we must compute frq 
    if step.equal['auto'] then 
      // compute the frequencies frq using auto mode 
      [frq,bnds,splitf]=calfrq_p(hnum,hden,fmin,fmax,dom=dom);
    else
      // compute the frequencies frq using step 
      splitf=1
      eps=1.e-14
      if fmin<0&fmax>=0 then
	frq=- [exp(l10*((log(eps)/l10):step:(log(-fmin)/l10))) -fmin];
	if fmax>eps then
	  frq1=[exp(l10*((log(eps)/l10):step:(log(fmax)/l10))) fmax];
	  frq=[frq($:-1:1) frq1]
	else
	  frq=frq($:-1:1);
	end
      elseif fmin<0&fmax<0 then
	frq= [exp(l10*((log(-fmax)/l10):step:(log(-fmin)/l10))) -fmin];
	frq=-frq($:-1:1);
      elseif fmin >= fmax then
	error('repfrq: fmin must be < fmax');
      else
	fmin=max(eps,fmin);
	frq=[exp(l10*((log(fmin)/l10):step:(log(fmax)/l10))) fmax];
      end
    end
  end

  // compute the responses to freq 
  // 
  if dom.equal['c'] then
    rep=freq(hnum,hden,2*%pi*%i*frq);
  else
    rep=freq(hnum,hden,exp(2*%pi*%i*dom*frq));
  end;
  // returned values 
  if nargout==1 then frq=rep,end
endfunction
