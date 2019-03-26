
function [frq,rep,splitf]=repfreq_p(hnum,hden,varargopt)
  [frq,rep,splitf]=repfreq_common(list(hnum,hden),varargopt(:));
  if nargout <=1 then frq=rep,end
endfunction

function [frq,rep,splitf]=repfreq_r(r,varargopt)
  if ~varargopt.iskey['dt'] then varargopt.dt=r.dt;end
  if ~varargopt.iskey['dom'] then varargopt.dom=r.dom;end
  [frq,rep,splitf]=repfreq_common(r,varargopt(:));
  if nargout <= 1 then frq=rep,end
endfunction

function [frq,rep,splitf]=repfreq_linearsys(sl,varargopt)
  if ~varargopt.iskey['dt'] then varargopt.dt=sl.dt;end
  if ~varargopt.iskey['dom'] then varargopt.dom=sl.dom;end
  [frq,rep,splitf]=repfreq_common(sl,varargopt(:))
  if nargout <=1 then frq=rep,end
endfunction

function [frq,rep,splitf]=repfreq_common(s_arg,varargopt)
  // varargopt: frq fmin fmax step dom='c'
  // Copyright CECILL INRIA (from scilab)

  // compute default values 
  l10=log(10);
  // domain 
  dom=varargopt.find['dom',def = 'c'];
  if ~or(dom==['c','d','s','u']) then
    error("dom should be ''c'' or ''d'' or ''u''");
    return
  end
  if dom.equal['u'] then dom='c';end
  if dom.equal['s'] && ~varargopt.iskey['dt'] then
    error("Error: when dom is equal to ''s'' dt must be given");
    return;
  end
  // dt 
  dt=varargopt.find['dt',def = 1];
  if dom.equal['c'] then fmax=1.E3;else fmax=1/(2*dt),end
  // step 
  step=varargopt.find['step',def = 'auto'];
  // frequancies 
  fmax=varargopt.find['fmax',def = fmax];
  fmin=varargopt.find['fmin',def = 'sym'];
  if fmin.equal['sym'] then fmin=-fmax;end
  // frq 
  frq=varargopt.find['frq',def = []];

  splitf=[];
  if isempty(frq) then
    // we must compute frq 
    if step.equal['auto'] then
      // compute the frequencies frq using auto mode
      if type(s_arg,'short')== 'l' then
	[frq,bnds,splitf]=calfrq_common(s_arg(1),s_arg(2),fmin,fmax,dom = dom,dt = dt);
      else
	[frq,bnds,splitf]=calfrq(s_arg,fmin,fmax,dom = dom,dt = dt);
      end
    else
      // compute the frequencies frq using step 
      splitf=1
      eps=1.E-14
      if fmin < 0 & fmax >= 0 then
        frq=-[exp(l10*((log(eps)/l10):step:(log(-fmin)/l10))),-fmin];
        if fmax > eps then
          frq1=[exp(l10*((log(eps)/l10):step:(log(fmax)/l10))),fmax];
          frq=[frq($:-1:1),frq1]
        else
          frq=frq($:-1:1);
        end
      elseif fmin < 0 & fmax < 0 then
        frq=[exp(l10*((log(-fmax)/l10):step:(log(-fmin)/l10))),-fmin];
        frq=-frq($:-1:1);
      elseif fmin >= fmax then
        error('repfrq: fmin must be < fmax');
      else
        fmin=max(eps,fmin);
        frq=[exp(l10*((log(fmin)/l10):step:(log(fmax)/l10))),fmax];
      end
    end
  end

  // compute the responses to frq 
  function y=freq_zz(hnum,hden,x)
    // function freq is C-coded
    ce_n=horner(hnum,x,vdim = %t);
    ce_d=horner(hden,x,vdim = %t);
    y=[];
    for i=1:size(ce_n,'*') do
      y.concatr[ce_n{i} ./ce_d{i}];
    end
  endfunction

  select  type(s_arg,'short')
    case 'r' then
      if dom.equal['c'] then
	rep=freq(s_arg.num,s_arg.den,2*%pi*%i*frq);
      else
	rep=freq(s_arg.num,s_arg.den,exp(2*%pi*%i*dt*frq));
      end
    case 'l' then
      if dom.equal['c'] then
	rep=freq(s_arg(1),s_arg(2),2*%pi*%i*frq);
      else
	rep=freq(s_arg(1),s_arg(2),exp(2*%pi*%i*dt*frq));
      end
    case 'linearsys' then
      [a,b,c,d]=abcd(s_arg);
      //[mn,nn]=size(b)
      // if nn <> 1 then error("Error: argument should be a simo"),end
      if dom=='c' then
	rep=freq(a,b,c,d,2*%pi*%i*frq)
      else
	rep=freq(a,b,c,d,exp(2*%pi*%i*dt*frq))
      end
  end
  // returned values 
  if nargout <= 1 then frq=rep,end
endfunction

