function bode(varargin,varargopt)
// varargopt: frq,fmin,fmax,step,title,dom
// Copyright GPL INRIA (from scilab).
  
  if length(varargin) == 0 then 
    s=poly(0,'s')
    n=(s^2+2*0.9*10*s+100);d=(s^2+2*0.3*10.1*s+102.01);
    n1=n*(s^2+2*0.1*15.1*s+228.01); d1=d*(s^2+2*0.9*15*s+225);
    bode([n;n1],[d;d1],fmin=0.01,fmax=100,title=['h1';'h'])
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
      if size(frq,2) == size(repf,2) && size(frq,1) <> size(repf,1) then
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
    [phi,d]=phasemag(repf);
  end

  if type(varargin(1),'short')== 'linearsys' then 
    // a linear system 
    if length(varargin) <> 1 then 
      error("Error: expecting only one non optional arguments for linear system case ");
      return;
    end
    sl=varargin(1);
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
  
  // Graphic parts 
  if ~new_graphics() then switch_graphics();end;xclear();
  //magnitude
  rect=[min(frq),min(d),max(frq),max(d)]
  // BUG here logflag is not used when given to xsetech
  xsetech(wrect=[0,0,1,0.5],frect=rect,fixed=%t,logflag="ln");
  xgrid();
  legends=catenate(title,sep='@');
  plot2d(frq',d',rect=rect,logflag="ln",leg=legends,leg_pos="urm");
  if type(dom,'short')=='s' then
    [xx1,xx2]=xgetech();
    val= xx2([2;4])';
    // plot2d(max(frq)*[1;1],val,line_color=5,rect=rect,logflag="ln");
  end
  xtitle('Magnitude','Hz','db');
  //phase
  rect=[min(frq),min(phi),max(frq),max(phi)]
  xsetech(wrect=[0,0.5,1,0.5],frect=rect,fixed=%t,logflag="ln");
  xgrid();
  //  now the curves
  plot2d(frq',phi',rect=rect,logflag="ln") ;
  if type(dom,'short')=='m' then
    [xx1,xx2]=xgetech();
    val= xx2([2;4])';
    // plot2d1(max(frq)*[1;1],val,line_color=5,rect=rect,logflag="ln");
  end
  xtitle('Phase ',' Hz','degrees');
endfunction

