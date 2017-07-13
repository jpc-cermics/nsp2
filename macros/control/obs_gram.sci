function go=obs_gram(a,c,domaine)
  // Copyright INRIA
  select type(a,'short')
    case 'm' then
     if nargin < 2 then error("Expecting at least two arguments");return;end
     if nargin==2 then domaine='c';end
     if domaine <> 'c' then domaine='d',end
     [m,n]=size(a)
     if m <> n then error("Error: first argument should be square");end
     [mb,nb]=size(c);if nb <> n then error("Error: arguments are incompatible");end
    case 'r' then
     sl=tf2ss(a);
     a=sl.A;c=sl.C;domaine=sl.dom;
     if domaine=='u' then
       printf('Warning: assuming time-domain is continuous\n')
       domaine='c';
     end
     [n,vn]=size(a);
    case 'linearsys' then
     a=sl.A,c=sl.C,domaine=sl.dom;
     if domaine=='u' then
       printf('Warning: assuming time-domain is continuous\n')
       domaine='c';
     end
     [n,vn]=size(a)
    else
      error('Error: expecting (A,C) pair or syslin list')
      return;
  end
  s=spec(a)
  if part(domaine,1)=='c' then
    if max(real(s)) >= 0 then error('Unstable system'),end
  else
    if max(abs(s)) >= 1 then error('Unstable system'),end
  end
  go=lyap(a,-c'*c,domaine)
endfunction
