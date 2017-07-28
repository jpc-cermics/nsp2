function elts=pfss(S,rmax,cord)
  //Syntax : elts=pfss(S)
  //Partial fraction decomposition of the linear system S (in state-space form):
  // elts is the list of linear systems which add up to S
  // i.e. elts=list(S1,S2,S3,...,Sn) with S1 + S2 +... +Sn = S
  // Each Si contains some poles of S according to the block-diagonalization
  // of the A matrix of S.
  // If S is given in transfer form, it is first converted into state-space
  // and each subsystem is then converted in transfer form.
  //!
  // Copyright INRIA
  flag=0;
  if type(S,'short')=='r' then flag=1;S=tf2ss(S);end
  if nargin==1 then rmax=[];cord=[];end
  if nargin==2 then
    if type(rmax,'short')=='s' then cord=rmax;end
    if type(rmax,'short')=='m' then cord=[];end
  end
  if type(S,'short') <> 'linearsys' then
    error("Error: expecting a linear system or transfer function");
  end
  [f,g,h,dd]=abcd(S);dom=S.dom;
  [n,vn]=size(f);
  if isempty(rmax) then
    [f,x,bs]=bdiag(f);
  else
    [f,x,bs]=bdiag(f,rmax);
  end
  h=h*x;g=x\g;
  k=1;ll=0;
  elts=list();
  for l=bs do
    ind=k:k+l-1;
    f1l=f(ind,ind);
    gl=g(ind,:);
    hl=h(:,ind);
    elts(ll+1)=syslin('c',f1l,gl,hl)
    ll=ll+1;k=k+l;
  end
  if nargin==2 then
    select cord
      case 'c' then
       nb=size(bs,'*');
       class=[];
       for k=1:nb do
         oneortwo=bs(k);ss=elts(k);A=ss(2);
         if oneortwo==1 then
           class=[class,real(spec(A))];end
         if oneortwo > 1 then
           class=[class,min(real(spec(A)))];end
       end
       [cl,indi]=sort(-class);
       elts1=elts;
       for k=1:size(elts) do
         elts(k)=elts1(indi(k));end
      case 'd' then
       nb=size(bs,'*');
       class=[];
       for k=1:nb do
         oneortwo=bs(k);ss=elts(k);A=ss(2);
         if oneortwo==1 then
           class=[class,abs(spec(A))];end
         if oneortwo > 1 then
           class=[class,max(abs(spec(A)))];end
       end
       [cl,indi]=sort(-class);
       elts1=elts;
       for k=1:size(elts) do
         elts(k)=elts1(indi(k));end
    end
  end
  if type(dd,'short')=='m' then
    if norm(dd,'fro') <> 0 then elts(ll+1)=dd,end
  end
  if type(dd,'short')=='p' then
    if norm(coeff(dd),1) > %eps then elts(ll+1)=dd,end
  end
  if flag==1 then
    k=size(elts);
    for kk=1:k do elts(kk)=ss2tf(elts(kk));end
  end
endfunction
