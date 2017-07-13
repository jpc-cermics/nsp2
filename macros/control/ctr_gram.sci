function gc=ctr_gram(a,b,domaine)
  //!
  // Copyright INRIA
  select type(a,'short')
    case 'm' then
     if nargin < 2 then error("Error: expecting at least two arguments");return;end
     if nargin==2 then domaine='c';end
     if domaine <> 'c' then domaine='d',end
     [m,n]=size(a);
     if m <> n then error("Error: first argument should be square");end
     [mb,nb]=size(b);if mb <> n then error("Error: arguments are incompatible");end
    case 'r' then
     sl=tf2ss(a);
     a=sl.A;b=sl.B;domaine=sl.dom;
     if domaine=='u' then
       printf('Warning: assuming time-domain is continuous\n')
       domaine='c';
     end
     [n,vn]=size(a);
    case 'linearsys' then
     sl=a;a=sl.A,b=sl.B;domaine=sl.dom;
     if domaine=='u' then
       printf("Warning: assuming time-domain is continuous\n")
       domaine='c';
     end
     [n,vn]=size(a)
    else
      error('Error: expecting (a,b) pair or syslin state-space')
  end
  s=spec(a);
  if domaine=='c' then
    if max(real(s)) >= 0 then error('Error: Unstable system'),end
  else
    if max(abs(s)) >= 1 then error('Erro: Unstable system'),end
  end
  gc=lyap(a',-b*b',domaine)
endfunction
