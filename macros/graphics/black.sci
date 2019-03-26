function black(varargin,varargopt)
// Black's diagram (Nichols chart) for a linear system sl.
// sl can be a continuous-time, discrete-time or sampled SIMO system
//            
//  fmin     : minimal frequency (in Hz).
//  fmax     : maximal frequency (in Hz).
//  pas      : logarithmic discretization step. (see calfrq for the
//             choice of default value).
//  comments : character strings to comment the curves.
//
//  frq      : (row)-vector of frequencies (in Hz) or (SIMO case) matrix
//             of frequencies.
//  db       : matrix of modulus (in Db). One row for each response.
//  phi      : matrix of phases (in degrees). One row for each response.
//  repf     : matrix of complex numbers. One row for each response.
//
// varargopt: frq,fmin,fmax,step,title,dom
// Copyright GPL INRIA (from scilab).
  
  if length(varargin) == 0 then 
    s=poly(0,'s')
    n=(s^2+2*0.9*10*s+100);d=(s^2+2*0.3*10.1*s+102.01);
    n1=n*(s^2+2*0.1*15.1*s+228.01); d1=d*(s^2+2*0.9*15*s+225);
    black([n;n1],[d;d1],fmin=0.01,fmax=100,title=['h1';'h'])
    return;
  end

  def_dom= "c";
  def_dt=1;
  select type(varargin(1),"short")
    case 'linearsys' then def_dom = varargin(1).dom; def_dt = varargin(1).dt
    case 'r' then def_dom = varargin(1).get_dom[]; def_dt = varargin(1).get_dt[];
  end
  // compute default values
  //-----------------------
  dom = varargopt.find['dom',def=def_dom];
  if ~or(dom==['c','d','s','u']) then 
    error("dom should be ''c'' or ''d'' or ''u''");
    return
  end
  if dom.equal['u'] then dom = 'c';end
  if dom.equal['s'] && ~varargopt.iskey['dt'] then 
    error("Error: when dom is equal to ''s'' dt must be given");
    return;
  end
  dt = varargopt.find['dt',def=def_dt];
  step =  varargopt.find['step',def='auto'];
  if dom.equal['c'] then fmax=1.d3; else fmax=1/(2*dt),end
  fmax= varargopt.find['fmax',def=fmax];
  fmin= varargopt.find['fmin',def=1.d-3];
  // title
  title =  varargopt.find['title',def=""];
  ilf=0;
  // select type of entries 
  if type(varargin(1),'short')== 'm' then 
    select length(varargin) 
     case 2 then 
      // frq, repf 
      if type(varargin(2),'short') <> 'm' then 
	error("Error: second argument should be a scalar matrix");
	return;
      end
      frq= varargin(1);
      repf= varargin(2);
      if size(frq,2)== size(frq,2) && size(frq,1) <> size(frq,1) then
	frq = ones(size(repf,1),1)*frq;
      end
      sn=size(frq);
      sd=size(repf);
      if ~sn.equal[sd] then 
	error("Error: the two first arguments should share the same size");
	return;
      end
      // compute phase and magnitude
      [phi,d]=phasemag(repf);
     case 3 then 
      // frq db phi
      frq=varargin(1);
      d=varargin(2);
      phi=varargin(3);
    else
      error("Error: two or three matrix arguments expected");
      return;
    end
    [mn,n]=size(frq);
    if mn<>1 then
      ilf=1;
    else
      ilf=0;
    end
  end
  
  if type(varargin(1),'short')== 'p' then 
    // two polynomials 
    if length(varargin) <> 2 then 
      error("Error: expecting two non optional arguments for polynomial case");
      return;
    end
    if type(varargin(2),'short')<> 'p' then 
      error("Error: second argument should be polynomial");
      return
    end
    hnum=varargin(1);
    hden=varargin(2);
    sn=size(hnum);
    sd=size(hden);
    if ~sn.equal[sd] then error("bode; The two polynomial matrices should share the same size");end
    if sn(2)<>1 then error("bode: The two polynomial matrices should be of size nx1");end
    // frq
    frq=  varargopt.find['frq',def=[]];
    if isempty(frq) then
      // compute frq
      [frq,repf,splitf]=repfreq(hnum,hden,dom=dom,dt=dt,fmin=fmin,fmax=fmax,step=step);
    end
    // compute phase and magnitude
    [phi,d]=phasemag(repf);
  end

  if type(varargin(1),'short')== 'linearsys' then 
    // a linear system 
    if length(varargin) <> 1 then
      error("Error: expecting only one non optional arguments for linear system case ");
      return;
    end
    sl=varargin(1);
    // frq
    frq=  varargopt.find['frq',def=[]];
    if isempty(frq) then
      // compute frq
      [frq,repf,splitf]=repfreq(sl,dom=dom,dt=dt,fmin=fmin,fmax=fmax,step=step);
    end
    // compute phase and magnitude
    [phi,d]=phasemag(repf);
  end
  
  if type(varargin(1),'short')== 'r' then 
    // a linear system 
    if length(varargin) <> 1 then 
      error("Error: expecting only one non optional arguments for rational case ");
      return;
    end
    H=varargin(1);
    frq=  varargopt.find['frq',def=[]];
    if isempty(frq) then
      // compute frq
      [frq,repf,splitf]=repfreq(H,dom=dom,dt=dt,fmin=fmin,fmax=fmax,step=step);
    end
    // compute phase and magnitude
    [phi,d]=phasemag(repf);
  end

  // compute frq repf splitf from fmin fmax step
  //---------------------------------------------
  // check frequencies
  if dom.equal['s'] then
    nyq_frq=1/2/dt;
    if ~isempty(find(frq > nyq_frq)) then
      printf('There are frequencies beyond Nyquist f=%f!\n',nyq_frq);
    end
  end

  // Graphics 
  if ~new_graphics() then switch_graphics();end
  [mn,n]=size(phi);
  //
  xmn=floor(min(phi)/90)*90
  xmx=ceil(max(phi)/90)*90
  ymn=min(d)
  ymx=max(d)
  rect=[ymn;xmn;ymx;xmx]
  //[xmn,xmx,npx]=graduate(-360,0)
  //[ymn,ymx,npy]=graduate(min(d),max(d))
  rect=[xmn,ymn,xmx,ymx]
  // xsetech(wrect=[0,0,1,0.5],frect=rect,fixed=%t);
  plot2d(phi',d',line_color=(1:mn),rect=rect);
  xgrid();
  kf=1
  phi1=phi+5*ones(size(phi));
  //xgeti=xget("mark");
  //xset("mark",2,xgeti(2));
  //xset("clipgrf");
  
  kk=1;p0=[phi(:,kk) d(:,kk)];ks=1;dst=0;
  dx=rect(3)-rect(1)
  dy=rect(4)-rect(2)
  dx2=dx^2;dy2=dy^2

  while kk<n
    kk=kk+1
    dst=dst+min(((phi(:,kk-1)-phi(:,kk)).^2)/dx2+((d(:,kk-1)-d(:,kk)).^2)/dy2)
    if dst>0.001 then
      if min(abs(frq(:,ks(prod(size(ks))))-frq(:,kk))./frq(:,kk))>0.2 then
	ks=[ks kk]
	dst=0
      end
    end
  end
  kf=1
  for k=1:mn,
    xnumb(phi(k,ks),d(k,ks),frq(kf,ks));
    xpoly(phi(k,ks),d(k,ks),mark=1,mark_size=8,color=-2);
    kf=kf+ilf
  end
  //xclip();
  xtitle('h(2i.pi.f) ','phase','magnitude');
  //     contour 2.3 db
  mbf=2.3;
  lmda=exp(log(10)/20*mbf);
  r=lmda/(lmda**2-1);
  npts=100;
  crcl=exp(%i*(-%pi:(2*%pi/npts):%pi));
  lgmt=log(-r*crcl+r*lmda*ones(size(crcl)));
  plot2d([180*(imag(lgmt)/%pi-ones(size(lgmt)))]',[(20/log(10)*real(lgmt))]',...
	 style=[2],leg='2.3db curve'),
  //xset("mark",xgeti(1),xgeti(2));
endfunction

