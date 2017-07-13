function u=prbs_a(n,nc,ids)
  //  generation of pseudo random binary sequences
  //  u=[u0,u1,...,u_(n-1)];
  //  u takes values in {-1,1} and changes at most nc times its sign.
  //  ids can be used to fix the date at which u must change its sign 
  //  ids is then an integer vector with values in [1:n].
  //!
  // Copyright INRIA
  if nargin <= 2 then
    //rand('uniform');
    yy=int(min(max(n*rand(1,nc),1*ones(1,nc)),n*ones(1,nc)));
    ids=sort(yy);ids=[n,ids,1];
  else
    [n1,n2]=size(ids);
    ids=[n,min(n*ones(n1,n2),max(sort(ids),1*ones(n1,n2))),1];
  end
  u=0*ones(1,n);
  [n1,n2]=size(ids);
  val=1;
  for i=1:n2-1 do
    if ids(i) <> ids(i+1) then
      u(ids(i+1):ids(i))=val*ones(size(ids(i+1):ids(i)));val=-1*val;
    end
  end
endfunction
