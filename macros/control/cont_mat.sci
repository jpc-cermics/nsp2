function c=cont_mat(a,b)
  //c=cont_mat(a,b) or c=cont_mat(sl) is the controllability matrix.
  // of the pair a,b or of the system sl=[a,b,c,d] (syslin list)
  //                 2       n
  //i.e. c=[b, ab, ab,...; ab ]  
  //!
  // Copyright INRIA

  select type(a,'short')
    case 'm' then
     if nargin==1 then error('Error: 2 arguments expected a,b'),end
     [m,n]=size(a)
     if m <> n then error("Error: first argument should be square");end
     [mb,nb]=size(b);
     if mb <> n then error("Error: arguments have incompatible sizes");end
    case 'linearsys' then
     sl=a;a=sl.A,b=sl.B;
     [n,vn]=size(a)
    else
      error('Error: expecting a,b pair or linear system')
  end
  c=b;for k=1:n-1 do c=[b,a*c],end
endfunction
