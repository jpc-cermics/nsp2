function sl=syslin(domain,a,b,c,d,x0)
  // Copyright INRIA
  dt=-1;
  // check domain 
  select type(domain,'short')
    case 'm' then
     //sampled system
     if isempty(domain) then
       dt=-1;domain='u';
     elseif size(domain,'*')==1 then
       dt=domain;domain='s';
     else
       error('domain (1rst argument of syslin) must be a scalar')
     end
     z='z';
    case 's' then
     if size(domain,'*') <> 1 then
       error('domain (1rst argument of syslin) must be a single string')
     end
     domain=part(domain,1)
     select domain
       case 'c' then
        z='s'
       case {'d','s'} then
        z='z'
       else
         error(domain+': unknown time domain')
     end
    else
      error('1rst argument of syslin should be a string, a scalar or a [] matrix')
  end
  //============================================================================
  if nargin==2 then
    //syslin(domaine,sys)
    if type(a,'short')=='r' then
      sl=a;
      sl.set_dom[domain];
      sl.set_dt[dt];
      sl.set_var[z];
    elseif type(a,'short')=='linearsys' then
      sl=a;
      sl.dom=domain;
      sl.dt=dt;
    else
      error('syslin: H must be a linear state space or a transfer function')
    end
    //============================================================================
  elseif nargin==3 then
    // syslin(domaine,num,den)
    num=a;den=b
    if ~(or(type(num,'short')==['m','p']) && or(type(den,'short')==['m','p'])) then
      error('syslin: N and D must be matrix of numbers or of polynomials')
    end
    if or(size(num) <> size(den)) then
      error('syslin: N and D have inconsistent dimensions')
    end
    if type(num,'short')=='p' && type(den,'short')=='p' then
      if num.get_var[] <> den.get_var[] then
        error('syslin: N and D have inconsistent formal variable names')
      end
    end
    if type(num,'short')=='m' then
      num=num*poly(1,z,'c')
    else
      num.set_var[z];
    end
    if type(den,'short')=='m' then
      den=den*poly(1,z,'c')
    else
      num.set_var[z];
    end
    sl=p2r(num,den);
    if type(domain,'short')=="m" then
      sl.set[dom = 's'];sl.set[dt = domain];
    else
      sl.set[dom = domain];end
    //============================================================================
  elseif nargin > 3 then
    // syslin(domaine,A,B,C [,D [X0]])
    if type(a,'short') <> 'm' then
      error('syslin: A must be a square matrix of numbers')
    end
    [ma,na]=size(a);
    if ma <> na then
      error('syslin: A must be a square matrix of numbers')
    end
    if type(b,'short') <> 'm' then
      error('syslin: B must be a  matrix of numbers')
    end
    [mb,nb]=size(b);
    if na <> mb && mb <> 0 then
      error('syslin: row dimension of B do not agree dimensions of A')
    end
    if type(c,'short') <> 'm' then
      error('syslin: C must be a  matrix of numbers')
    end
    [mc,nc]=size(c);
    if na <> nc & nc <> 0 then
      error('syslin: column dimension of C do not agree dimensions of A')
    end
    if nc==0 then c=zeros(0,na);end
    if nargin < 6 then
      x0=0*ones(na,1)
    else
      if type(x0,'short') <> 'm' then
        error('syslin: X0 must be a vector of numbers')
      end
      [mx,nx]=size(x0);
      if mx <> na || nx <> 1 then
        error('syslin: dimensions of X0 do not agree')
      end
    end
    if nargin < 5 then
      d=0*ones(mc,nb)
    else
      if ~or(type(d,'short')==['m','p']) then
        error('syslin: D must be a  matrix of numbers or polynomials')
      end
      [md,nd]=size(d);
      if ~isempty(c*b) then
        if mc <> md | nb <> nd then
          error('syslin: ZZcolumn dimension of D do not agree dimensions of B or C')
        end
      end
    end
    sl=linear_system(a,b,c,d,x0,dom = domain,sample = dt);
  end
endfunction
