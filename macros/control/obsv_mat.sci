function o=obsv_mat(a,c)
  // Copyright INRIA

  select type(a,'short')
    case 'm' then
     if nargin==1 then error('Error: 2 arguments : a,c'),end
     [m,n]=size(a)
     if m <> n then error("Error: argument must be a squared matrix");end
     [mb,nb]=size(c);
     if nb <> n then error("Error: arguments have incompatible dimensions");end
    case 'linearsys' then
     sl=a;a=sl.A;c=sl.C;[n,vn]=size(a);
    case 'r' then
     sl=tf2ss(a);a=sl.A;c=sl.C;[n,vn]=size(a);
    else
      error('Error: (a,c) pair or syslin list')
  end
  o=c;for k=1:n-1 do o=[c;o*a],end
endfunction
