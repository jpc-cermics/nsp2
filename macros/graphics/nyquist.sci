function nyquist(varargin,varargopt)
// varargopt: frq,fmin,fmax,step,title,dom
// Nyquist plot
// Copyright CECILL INRIA (from scilab).

  if length(varargin) == 0 then 
    s=poly(0,'s');
    n=(s^2+2*0.9*10*s+100);
    d=(s^2+2*0.3*10.1*s+102.01);
    n1=n*(s^2+2*0.1*15.1*s+228.01);
    d1=d*(s^2+2*0.9*15*s+225);
    nyquist([n;n1],[d;d1],title=['h1','h2'],uc=%t);
    return;
  end

  // compute default values
  //-----------------------
  dom = varargopt.find['dom',def='c'];
  if ~or(dom==['c','d','s','u']) then 
    error("dom should be ''c'' or ''d'' or ''u''");
    return
  end
  if dom.equal['u'] then dom = 'c';end
  if dom.equal['s'] && ~varargopt.iskey['dt'] then 
    error("Error: when dom is equal to ''s'' dt must be given");
    return;
  end
  dt = varargopt.find['dt',def=1];
  step =  varargopt.find['step',def='auto'];
  if dom.equal['c'] then fmax=1.d3; else fmax=1/(2*dt),end
  fmax= varargopt.find['fmax',def=fmax];
  fmin= varargopt.find['fmin',def=1.d-3];
  // title
  title =  varargopt.find['title',def=""];
  
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
      //[phi,d]=phasemag(repf);
     case 3 then 
      // frq db phi
      frq=varargin(1);
      d=varargin(2);
      phi=varargin(3);
      repf=exp(log(10)*fmin/20 + %pi*%i/180*fmax);
    else
      error("Error: two or three matrix arguments expected");
      return;
    end
    if min(frq)<=0 then
      error('bode: requires strictly positive frequencies')
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
    // [phi,d]=phasemag(repf);
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
  // ---------
  [mn,n]=size(repf)
  repi=imag(repf);repf=real(repf)
  mnx=min(repf);
  mny=min(repi);
  mxx=max(repf);
  mxy=max(repi);
  // computing bounds of graphic window
  dx=(mxx-mnx)/30;dy=(mxy-mny)/30
  rect=[mnx-dx,mny-dy,mxx+dx,mxy+dy]

  // drawing the curves
  if ~new_graphics() then switch_graphics();end;xclear();

  // each curve consists of parts
  legends=catenate(title,sep='@');
  H=hash(leg=legends,leg_pos="urm");
  splitf($+1)=n+1;
  for ksplit=1:size(splitf,'*')-1
    sel=splitf(ksplit):splitf(ksplit+1)-1
    plot2d(repf(:,sel)',repi(:,sel)',H(:),line_color=(1:mn),rect=rect);
    H=hash(1);
  end
  xgrid();
  // setting the current mark
  kk=1;p0=[repf(:,kk),repi(:,kk)];ks=1;d=0;
  dx=rect(3)-rect(1)
  dy=rect(4)-rect(2)
  dx2=dx^2;dy2=dy^2
  // collect significant frequencies along the curve
  //-------------------------------------------------------
  Ic=min(cumsum(sqrt(((repf(:,1:$-1)-repf(:,2:$)).^2)/dx2+((repi(:,1:$-1)-repi(:,2:$)).^2)/dy2),2),'r');
  kk=1
  L=0
  DIc=0.2
  while %t
    ksup=find(Ic-L>DIc)
    if isempty(ksup) then break,end
    kk1=min(ksup);
    L=Ic(kk1);
    Ic(1:kk1)=[];
    kk=kk+kk1;
    if min(abs(frq(:,ks($))-frq(:,kk))./abs(frq(:,kk)))>0.001 then
      if min(sqrt(((repf(:,ks)-repf(:,kk)*ones(size(ks))).^2)/dx2+..
		  ((repi(:,ks)-repi(:,kk)*ones(size(ks))).^2)/dy2)) >DIc then
	ks=[ks kk]
	d=0
      end
    end
  end
  if ks($)~=n then
    if min(((repf(:,ks(1))-repf(:,n)).^2)/dx2+((repi(:,ks(1))-repi(:,n)).^2)/dy2)>0.01
      ks=[ks n]
    end
  end
  // display of parametrization (frequencies along the curve)
  //-------------------------------------------------------
  kf=1
  mrksiz=0.015*(rect(3)-rect(1))

  for k=1:mn,
    for kks=ks
      if abs(frq(kf,kks)) > 9999 | abs(frq(kf,kks)) < 0.001 then
	fmt="%e";
      else
	fmt="%g";
      end
      xstring(repf(k,kks)+mrksiz,repi(k,kks)+mrksiz,string(frq(kf,kks)),0);
    end
    s_ks = size(ks,'*');
    if s_ks > 1 then
      if ks($) < size(repf,2) then
	last=s_ks;
      else
	last=s_ks-1;
      end
      dx=repf(k,ks(1:last)+1)-repf(k,ks(1:last));
      dy=repi(k,ks(1:last)+1)-repi(k,ks(1:last));
      dd=150*sqrt((dx/(rect(3)-rect(1))).^2+(dy/(rect(4)-rect(2))).^2);
      if dd>0 then
	dx=dx./dd;dy=dy./dd;
	xarrows([repf(k,ks(1:last));repf(k,ks(1:last))+dx],..
		[repi(k,ks(1:last));repi(k,ks(1:last))+dy],arsize=mrksiz)
      end
    end
    //  xpoly(repf(k,ks),repi(k,ks),'marks',0);
    // kf=kf+ilf
  end
  // axes with 0
  //Optional unit circle
  if varargopt.iskey['uc'] then
    t= linspace(0,2*%pi,100)';
    plot2d(sin(t),cos(t),line_color=(mn+1),iso=%t,leg_pos="urm");
  end
  [v,rect]=xgetech();
  xpoly([rect(1),rect(3)],[0,0]);
  xpoly([0,0],[rect(2),rect(4)]);
  if dom.equal['c'] then
    xtitle('Nyquist plot ','Re(h(2i*pi*f))','Im(h(2i*pi*f))');
  else
    xtitle('Nyquist plot ',['Re(h(exp(';'2i*pi*f*dt)))'],'Im(h(exp(2i*pi*f*dt)))');
  end
endfunction

function nyquist_r(r,varargopt)
  if ~varargopt.iskey['dt'] then varargopt.dt= r.dt;end 
  if ~varargopt.iskey['dom'] then varargopt.dom= r.dom;end 
  nyquist(r.num,r.den, varargopt(:));
endfunction

function nyquist_linearsys(sl,varargopt)
  h=ss2tf(sl);
  if ~varargopt.iskey['dt'] then varargopt.dt= sl.dt;end 
  if ~varargopt.iskey['dom'] then varargopt.dom= sl.dom;end 
  nyquist(h.num,h.den,varargopt(:))
endfunction
