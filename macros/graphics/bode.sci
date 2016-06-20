function bode(hnum,hden,varargopt)
// varargopt: frq,fmin,fmax,step,title,dom
// Copyright CECILL INRIA (from scilab).

  if nargin <= 0 then
    s=poly(0,'s')
    n=(s^2+2*0.9*10*s+100);d=(s^2+2*0.3*10.1*s+102.01);
    n1=n*(s^2+2*0.1*15.1*s+228.01); d1=d*(s^2+2*0.9*15*s+225);
    bode([n;n1],[d;d1],fmin=0.01,fmax=100,title=['h1';'h'])
    return;
  end

  l10=log(10);
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
  fmin= varargopt.find['fmin',def=1.d-3];
  // title
  title =  varargopt.find['title',def=""];
  
  // select type of entries 
  
  if type(hnum,'short')== 'm' && type(hden,'short')== 'm' then 
    if size(hnum,2)== size(hden,2) && size(hnum,1) <> size(hden,1) then
      hnum = ones(size(hden,1),1)*hnum;
    end
    frq= hnum;
    repf= hden;
    sn=size(frq);
    sd=size(repf);
    if ~sn.equal[sd] then error("bode ww The two matrices should share the same size");end
    
  elseif  type(hnum,'short')== 'p' && type(hden,'short')== 'p' then 
    sn=size(hnum);
    sd=size(hden);
    if ~sn.equal[sd] then error("bode; The two polynomial matrices should share the same size");end
    if sn(2)<>1 then error("bode: The two polynomial matrices should be of size nx1");end
    // frq
    frq=  varargopt.find['frq',def=[]];
    if isempty(frq) then
      // compute frq
      [frq,repf,splitf]=repfreq(hnum,hden,dom=dom,fmin=fmin,fmax=fmax,step=step);
    end
  else
    error("bode: Argument should be a matrix or polynomial matrix")
  end

  // compute frq repf splitf from fmin fmax step
  //---------------------------------------------
  // check frequencies
  if type(dom,'short')=='m' then
    nyq_frq=1/2/dom;
    if ~isempty(find(frq > nyq_frq)) then
      printf('There are frequencies beyond Nyquist f=%f!\n',nyq_frq);
    end
  end

  // compute phase and magnitude
  [phi,d]=phasemag(repf);

  if ~new_graphics() then switch_graphics();end;xclear();
  [mn,n]=size(phi);
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
