function [frq,rep,splitf]=repfreq_new(h,varargopt)
  // varargopt: frq fmin fmax step dom='c'
  // Copyright CECILL INRIA (from scilab)

  function y=freq_zz(hnum,hden,x)
    ce_n=horner(hnum,x,vdim = %t);
    ce_d=horner(hden,x,vdim = %t);
    y=[];
    for i=1:size(ce_n,'*') do
      y.concatr[ce_n{i} ./ce_d{i}];
    end
  endfunction

  if ~or(type(h,'short')==['linearsys','r']) then
    error("Error: argument should be a linear state space or a transfer function.\n");
  end
  if type(h,'short')=='linearsys' then
    dom=h.dom;dt=h.dt;
    h=ss2tf(h);
  else
    dom=h.dom;dt=h.dt;
  end
  if dom.equal['u'] then dom='c';end

  if ~or(dom==['c','d','s']) then
    error("dom should be ''c'' or ''d'' or ''s''");
    return;
  end
  if dom.equal['d'] then dom=1;end
  l10=log(10);
  step=varargopt.find['step',def = 'auto'];

  if dom=='c' then fmax=1.E3;else fmax=1/(2*dom),end
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
      [frq,bnds,splitf]=calfrq(h,fmin,fmax,dom = dom);
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

  // compute the responses to freq 
  if type(h,'short')=='r' then
    if size(h.num,'*') <> 1 then
      error("argument should be a siso");
      return;
    end
    if dom.equal['c'] then
      rep=freq(h.num,h.den,2*%pi*%i*frq);
    else
      rep=freq(h.num,h.den,exp(2*%pi*%i*dt*frq));
    end
  else
    // linear system 
    [a,b,c,d]=abcd(h);
    [mn,nn]=size(b)
    if nn <> 1 then error("Error: argument should be a siso"),end
    if dom=='c' then
      rep=freq(a,b,c,d,2*%pi*%i*frq)
    else
      rep=freq(a,b,c,d,exp(2*%pi*%i*dt*frq))
    end
  end
  // returned values 
  if nargout==1 then frq=rep,end
endfunction
