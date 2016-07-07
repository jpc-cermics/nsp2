function nyquist(hnum,hden,varargopt)
// varargopt: frq,fmin,fmax,step,title,dom
// Nyquist plot
// Copyright CECILL INRIA (from scilab).

  if nargin <= 0 then
    s=poly(0,'s');
    n=(s^2+2*0.9*10*s+100);
    d=(s^2+2*0.3*10.1*s+102.01);
    n1=n*(s^2+2*0.1*15.1*s+228.01);
    d1=d*(s^2+2*0.9*15*s+225);
    nyquist([n;n1],[d;d1],title=['h1','h2'],uc=%t);
    return;
  end

  l10=log(10);

  if type(hnum,'short')<>'p' then error("bode: Argument should be a polynomial matrix");end
  if type(hden,'short')<>'p' then error("bide: Argument should be a polynomial matrix");end
  sn=size(hnum);
  sd=size(hden);
  if ~sn.equal[sd] then error("bide; The two polynomial matrices should share the same size");end
  if sn(2)<>1 then error("bode: The two polynomial matrices should be of size nx1");end

  // compute default values
  //-----------------------
  dom = varargopt.find['dom',def='c'];
  if ~(dom.equal['c'] || dom.equal['d'] || ( type(dom,'short')=='m' && sime(dom,'*')==1)) then
    error("dom should be ''c'' or ''d'' or a scalar");
    return
  end
  if  dom.equal['d'] then dom=1;end
  step =  varargopt.find['step',def='auto'];
  if dom=='c' then fmax=1.d3; else fmax=1/(2*dom),end
  fmax= varargopt.find['fmax',def=fmax];
  fmin= varargopt.find['fmin',def='sym'];
  if fmin.equal['sym'] then fmin = -fmax;end;
  // frq
  frq=  varargopt.find['frq',def=[]];
  // title
  title =  varargopt.find['title',def=""];

  // compute frq repf splitf from fmin fmax step
  //---------------------------------------------

  if isempty(frq) then
    // compute frq
    [frq,repf,splitf]=repfreq(hnum,hden,varargopt(:));
  end

  [mn,n]=size(repf)
  //trace d
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
  // collection significant frequencies along the curve
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
  end;
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


function nyquist_r(h,varargopt)
  nyquist(h.num,h.den,varargopt(:))
endfunction
