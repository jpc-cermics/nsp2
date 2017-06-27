function [frq,rep,splitf]=repfreq_p(hnum,hden,varargopt)
// varargopt: frq fmin fmax step dom='c'
// Copyright CECILL INRIA (from scilab)
  
  function y=freq(hnum,hden,x)
    ce_n=horner(hnum,x,vdim=%t);
    ce_d=horner(hden,x,vdim=%t);
    y=[];
    for i=1:size(ce_n,'*')
      y.concatr[ce_n{i}./ce_d{i}];
    end
  endfunction

  // compute default values 
  l10=log(10);
  // domain 
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
  // dt 
  dt = varargopt.find['dt',def=1];
  if dom.equal['c'] then fmax=1.d3; else fmax=1/(2*dt),end
  // step 
  step =  varargopt.find['step',def='auto']; 
  // frequancies 
  fmax= varargopt.find['fmax',def=fmax];
  fmin= varargopt.find['fmin',def='sym'];
  if fmin.equal['sym'] then fmin = -fmax;end
  // frq 
  frq=  varargopt.find['frq',def=[]];
  
  splitf=[];
  if isempty(frq) then 
    // we must compute frq 
    if step.equal['auto'] then 
      // compute the frequencies frq using auto mode 
      [frq,bnds,splitf]=calfrq_p(hnum,hden,fmin,fmax,dom=dom,dt=dt);
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
    rep=freq(hnum,hden,exp(2*%pi*%i*dt*frq));
  end
  // returned values 
  if nargout==1 then frq=rep,end
endfunction

function [frq,rep,splitf]=repfreq_r(r,varargopt)
  if ~varargopt.iskey['dt'] then varargopt.dt= r.dt;end 
  if ~varargopt.iskey['dom'] then varargopt.dom= r.dom;end 
  [frq,rep,splitf]=repfreq(r.num,r.den,varargopt(:));
  if nargout==1 then frq=rep,end
endfunction

function [frq,rep,splitf]=repfreq_linearsys(sl,varargopt)
  h=ss2tf(sl);
  if ~varargopt.iskey['dt'] then varargopt.dt= sl.dt;end 
  if ~varargopt.iskey['dom'] then varargopt.dom= sl.dom;end 
  [frq,rep,splitf]=repfreq_r(h,varargopt(:))
  if nargout==1 then frq=rep,end
endfunction




