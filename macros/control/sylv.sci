function X=sylv(A,B,C,flag)
  //  solve  A*X+X*B=C if flag=='c' or  A*X*B-X=C if flag=='d'
  if nargin <> 4 then
    error("Error: incorrect number of input arguments")
  end
  if size(A,1) <> size(A,2) then
    error("Error: argument must be a squared matrix");
  end
  if size(B,1) <> size(B,2) then
    error("Error: argument must be a squared matrix");
  end
  if size(C,1) <> size(A,1) then
    error('A and C dimensions does not agree'),
  end
  if size(C,2) <> size(B,2) then
    error('B and C dimensions does not agree'),
  end
  if flag=='c' then
    flag=[0,0,0],
  elseif flag=='d' then
    flag=[1,0,0],
  else
    error("Error: domain should be ""c"" or ""d""");
    return;
  end
  X=linmeq(1,A,B,C,flag)
endfunction
