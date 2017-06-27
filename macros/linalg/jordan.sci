function [Y,blks,Dg]=jordan(A,COEFF)
// Copyright (C) 2016-2016 - F. Delebecque (Inria)
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//  
//Jordan canonical form of a structurally nilpotent matrix;
//Usage: [Y,blks]=jordan(A);   (or [Y,blks]=jordan(A-lambda*eye()));
//       [Y,blks,Dg]=jordan(A,COEFF);
//Returns in blks the size of jordan blocks.
//e.g. blks=[3 3 2 2 2 1] means that A has 
//                                       2 J-blocks of size 3
//                                       3 J-blocks of size 2
//                                       1 J-block  of size 1
//This function is a complement to Jordn: 
//it returns Y such that inv(Y)*A*Y is in JCF;
// A is assumed nilpotent. If not, inv(Y)*A*Y is block diagonal,
// its (2,2)-block containing no-zeros eigenvalues of A.
// Example:
//blks=[3 3 2 2 2 1]; nonzeros=[1;2];
//[A,Xrd]=makeJ(blks,nonzeros);
//[Y,blks1]=jordan(A);
//Aj=clean(Y\A*Y); spec(Aj)
// Caution: This function should be used with care.
//If Y is badly conditioned, one can use 
// [Y1,blks,Dg]=jordan(A,COEFF) with an appropriate normalizing
// coefficient COEFF. Then one has Y=Y1*inv(diag(Dg)); 
// and Y1 may have a better conditioning than Y.
//One has inv(Y1)*A*Y1=(1/COEFF)*JCF(A) (for the nilpotent part).
//
  
function NB_blks=froben(struc)
//NB_blks(1) = # blocks of size index(A)
//NB_blks(2) = # blocks of size index(A)-1
//
  NB_blks=[];
  if ~isempty(struc) then 
    str=[0, struc, struc($)];
  else
    str=[0 ];
  end
  for k=2:length(str)-1 do
    NB_blks(k-1)=2*str(k)-str(k-1)-str(k+1);
  end
  NB_blks=NB_blks($:-1:1)';
  //if NB_blks($)==0 then NB_blks($)=[];end
endfunction

  
  [n,na]=size(A);
  if nargin == 1 then COEFF=1;end
  if ~exists("verbose") then verbose=1;end
  blks=[];
  if norm(A,1)==0 then
    if verbose then
      printf("         %i  block of size %i\n",size(A,1),1);
    end
    [n,na]=size(A);Y=eye(n,n);Dg=ones(n,1);struc=n;blks=ones(1,n);
    return;
  end
  [X,struc,An]=Jordn(A);
  X=clean(X);
  if isempty(struc) then 
    if verbose then
      printf("         %i  block \n",0);
    end
    Y=X;Dg=ones(size(A,1),1);
    return;end
    Index=length(struc);
    slice=[0,struc];
    Y=[];old=[];Dg=[];
    NB=froben(struc);
    for kk=1:Index do
      blk_size=Index-kk+1;  //start with J-blocks of max. size
      Nb_blks=NB(kk);  //Expected number of blocks with size=kk.
      //disp('Expected: '+string(NB(kk))+' blocks of size '+string(blk_size)');
      kerplus=X(:,1:slice($-kk+1));
      kermoins=X(:,1:slice($-kk));p=(slice($-kk)+1):slice($-kk+1);
      //Slice=X(:,p);
      [w,d1,d2]=spanplus([kermoins,old],kerplus);
      d=d1-d2;
      //  if d<>Nb_blks then d1=d2;warning("warning");end   //Warning ... 
      if d<>Nb_blks then d2=d1-Nb_blks;end //Warning ... 
      W=w(:,d2+1:d1);
      // old=A*Slice;   //= X*An*X'*Slice = X*An(:,p)
      old=X*An(:,p); 
      //W orth.  each col <-> one block of size blk_size
      //size(W,2)=0 => no block of size  blk_size
      for k=1:size(W,2) do
	Wk=W(:,k);  //cyclic
	for l=(blk_size-1):-1:0 do
	  //Y=[Y,(A^l)*Wk];   //to be improved
	  nxt=(X*(An^l))*(X'*Wk);nxt=nxt*COEFF^l;
	  //[XXX,dim]=range(A,l);//XXX*nxt=[*;0];
	  Dg=[Dg;COEFF^l];
	  if norm(nxt)==0 then pause;end
	  Y=[Y,nxt];
	end
      end
      if size(W,2) <>0 then 
	if verbose then
	  printf("        %i  block(s) of size %i\n",size(W,2),blk_size);
	end
        blks=[blks,blk_size*ones(1,size(W,2))];
      end
    end
    p=size(Y,2);n=size(X,2);sz=n-p;
    if p<n then
      compl=X( :, (p+1):n );//Updt=[eye(p,p),compl(1:p,:);zeros(sz,p),compl(p+1:$,:)];
      Y=[Y,compl];Dg=[Dg;ones(size(compl,2),1)];
      //errcatch(19,'pause');
      Tmp=inv(Y)*A*Y;k=p;A11=Tmp(1:k,1:k);
      A12=Tmp(1:k,k+1:$);A22=Tmp(k+1:$,k+1:$);
      if norm(imag(A11),1)*norm(imag(A22),1)*norm(imag(A12),1)==0 then
	L=zeros(size(A11,2),size(A22,1));
	if norm(A11*L-L*A22+A12,1) > sqrt(%eps) then
	  L=sylv( real(A11), real(-A22), real(-A12),"c"); //A11*L-L*A22+A12=0;
	end
	Y(:,k+1:$)=Y(:,k+1:$)+Y(:,1:k)*L;
      else 
	// bigA11=[real(A11) -imag(A11);imag(A11) real(A11)];
	// bigA22=[real(A22) -imag(A22);imag(A22) real(A22)];
	// bigA12=[real(A12) -imag(A12);imag(A12) real(A12)];
	// bigL=sylv(bigA11, -bigA22, -bigA12,"c");
	// L=bigL(1:$/2,1:$/2)+%i*bigL($/2+1:$,1:$/2);
	L=sylv(A11,-A22,-A12);
	Y(:,k+1:$)=Y(:,k+1:$)+Y(:,1:k)*L;
      end
    else 
      return;//
    end
endfunction

function [X,struc,An]=Jordn(A,flag)
//Returns in struc the Jordan structure of A at zero.
//X is an orthogonal matrix and struc is an integer
//row vector such that struc(1)=dimension of ker(A)
//                     struc(2)=dimension of ker(A^2)
//etc
//The struc(1) first columns of X span ker(A)
//The struc(2) first columns of X span ker(A^2)
//etc
// Index of O eigenvalue = length(struc).
// The algorithm implemented transforms A into
// X'*A*X = [A0 A1 A2 ... Ai A]
//          [0  0  0  ... 0  x]
// A0 has struc(1) cols. and 0 rows. 
// A1 has struc(2)-struc(1) cols. and struc(1) rows
//  ...
// Ai has struc(i)-struc(i-1) cols. and struc(i-1) rows.
// The Ais have full column rank.
// A has n - struc(i) cols. and struc(i) cols. 
// x is non-singular and nonzero eigenvalues of A = eigenvalues of x.
//Example:
//A=makeJ([1,1,3,4,5]);
//[X,struc]=Jordn(A);
//l=length(struc);
//for k=1:l, T=A^k*X;norm(T(:,1:struc(k)),1),end

  function [U,d]=onestep(A,d)
  //Column compresses A22 in A=[A11,A12;A21,A22] with
  //[d,d]=size(A11)          A,A11 and A22 are square. 
    An=A(d+1:$,d+1:$);
    if norm(An) < 1.E-8 then
      d=size(A,1);U=eye(size(A));return;
    end
    [p,q]=size(An);
    //[Xn,dn]=colcomp(An);
    [Xn,R,JPVT,dn,SVAL]=qr(An',tol=100000*%eps);
    if SVAL(2) < sqrt(%eps)*norm(An,1) then
      [Xn,R,JPVT,dn,SVAL]=qr(An',tol=(%eps^(1/2))*norm(An,1));
    end

    Xn=Xn(:,$:-1:1);U=sysdiag(eye(d,d),Xn);
    d=d+q-dn;
    //disp(svd(An));disp(d);
  endfunction

  [n,na]=size(A);X=eye(n,n);d=0;d1=-1;
  nrm=[];
  for kmax=1:n do
    nrmtst=norm(A^kmax);
    if nrmtst<%eps then break;end
    nrm=[nrm,nrmtst];
  end

  struc=[];
  kount=0;
  while %t do
    [U,d1]=onestep(A,d);X=X*U;A=U'*A*U;A=clean(A);
    if d1==d then break;end
    struc=[struc,d1];d=d1;
    kount=kount+1;
    if kount==kmax then break;end
  end
  if nargin==2 then
    Tmp=A;k=d;A11=Tmp(1:k,1:k);A12=Tmp(1:k,k+1:$);A22=Tmp(k+1:$,k+1:$);
    //L=sylv(real(A11),real(-A22),real(-A12),"c"); //A11*L-L*A22+A12=0;
    L=sylv(A11,-A22,-A12);
    Id=eye(n,n);Jd=Id;
    X(1:size(X,1),k+1:$)=X(:,k+1:$)+X(:,1:k)*L;
    Id(:,k+1:$)=Id(:,k+1:$)+Id(:,1:k)*L;
    Jd(:,k+1:$)=Jd(:,k+1:$)-Jd(:,1:k)*L;
    A=Jd*A*Id;
  end
  An=clean(A);
endfunction

function [A,Xrd]=makeJ(blks,nonzeros)
//Utility fct: returns A, a matrix with given Jordan structure.
//The structure is given by the vector blks. The number of the
//Jordan blocks is the length of blks. The entries of blks are the
//dimensions of the Jordan blocks.
//A=makeJ([1,3 5]) returns a nilpotent matrix A (in JCF) with 
//3 blocks of respective dimensions (1,3,5).
//With blks=[1,3,5], [A,Xrd]=makeJ(blks) returns a nilpotent matrix A 
//and a nonsingular random matrix Xrd such that Xrd*A/Xrd is in JCF
//(up to rounding errors: use clean(Xrd*A/Xrd) to remove small entries).
//nonzeros is a vector of nonzero eigenvalues for A. The default
//value of nonzeros is []. 
//With blks=[1,3,5], nonzeros=[1;2], [A,Xrd]=makeJ(blks,nonzeros)
//returns  matrix A with 1+3+5 eigenvalues at zero and two nonzero 
//eigenvalues at 1 and 2.
//Note that the eigenvalues at zero cannot be computed with high accuracy:
//numerically, they are not close to zero.
//Xrd is optional. The nonzero eigenvalues have trivial (diagonal) Jordan 
//structure. Use A+lambda*eye() to shift eigenvalues.

  if nargin==1 then nonzeros=[];end
  A=coeff(randpencil([],blks,[],[]),1);
  A=[ A # diag(nonzeros)];
  if nargout==1 then return;end
  // rand('seed',0);
  Xrd=randn(A);
  A=inv(Xrd)*A*Xrd;
endfunction


if %f then 
  // unused utilities 
    
  function [partt,Ll]=parti(struc)
  //struc=Jordan structure returned by Jordn
  //partt = partition of states of a commuting matrix
  //Ll(1)(:) index corresponding to partt(1) etc

    function NB_blks=froben(struc)
    //NB_blks(1) = # blocks of size index(A)
    //NB_blks(2) = # blocks of size index(A)-1
    //
      NB_blks=[];
      if ~isempty(struc) then 
	str=[0, struc, struc($)];
      else
	str=[0 ];
      end
      for k=2:length(str)-1 do
	NB_blks(k-1)=2*str(k)-str(k-1)-str(k+1);
      end
      NB_blks=NB_blks($:-1:1)';
      //if NB_blks($)==0 then NB_blks($)=[];end
    endfunction

    
    function [Q,P,blks,NB]=struc2PQ(struc)
    //Q(1) blocks of size P(1)
    //Q(2) blocks od size P(2)
    //etc ...
    //NB(1)=nb. J-blocks of size index
    //NB(2)=nb. J-blocks of size index-1
    //...
    //NB($)=nb. J blocks of size 1

      function [q,p,struc2]=struc2pq(struc)
      //p=max index
      //q=nb. of blocks with max. index
	dd=-diff(struc($:-1:1));
	p=length(struc);
	q=dd(1);
	struc2=struc;
	for l=1:p do
	  struc2(l)=struc(l)-q*l;
	end
	for j=1:p-1 do
	  if struc2($)==struc2($-1) then
	    struc2($)=[]
	  end
	end
      endfunction
      
      if isempty(struc) then
	Q=[];P=[];blks=[];NB=[];
	return;
      end
      Q=[];P=[];
      q=1;
      while ~isempty(q) do
	[q,p,struc]=struc2pq(struc);
	Q=[Q,q];P=[P,p];
      end
      Q=[Q,struc];
      qq=[];
      for kk=1:length(P) do
	F=[];
	for f=1:Q(kk) do
	  F=[F,P(kk)];
	end
	qq=[qq,F];
      end
      blks=qq;

      NB=[];
      for k=max(P):-1:1 do
	[ppp,qqq]=find(P==k);
	nb=Q(qqq); 
	if isempty(nb) then nb=0;end
	NB=[NB,nb];
      end
    endfunction
    
    function dims=blks2dims(struc)
      [Q,P,blks,NB]=struc2PQ(struc);
      for kk=1:length(Q) do
	if Q(kk)==0 then
	  P(kk)=[];Q(kk)=[];
	end
      end

      kp=[P($),diff(P($:-1:1))];
      dims=[];
      for ij=1:length(P) do
	for k1=1:kp(ij) do
	  dims=[dims,Q];
	end
	Q($)=[];
      end
    endfunction
    
    dims=blks2dims(struc);
    Right=cumsum(dims);
    left=1;partt=list();
    for k=1:length(dims) do
      partt(k)=[left:Right(k)];
      left=Right(k)+1;
    end

    function L=tmplst(blks,dims)
      du=cumsum(duale(blks));
      Cs=cumsum(dims);
      QQ=[];
      for k=1:length(du) do
	tst=du(k)
	[pp,qq]=find(tst==Cs);
	QQ=[QQ,qq];
      end
      QQ=1+[0,QQ(1:$-1)];
      L=QQ;
    endfunction

    function Ll=INdxx(struc)
      dims=blks2dims(struc)
      [Q,P,blks,NB]=struc2PQ(struc);
      NB_blks=froben(struc);
      for jj=1:length(NB_blks) do
	if NB_blks($)==0 then NB_blks($)=[];else break;end
      end
      Ll=list();

      for kk=1:length(NB_blks) do
	if ~isempty(dims) then
	  L=tmplst(blks,dims)
	  Ll(kk)=L;
	  dims(L)=[];
	  blks(1:1:NB_blks(kk))=[];
	end
      end
      
      for kk=2:length(Ll) do
	lk=Ll(kk);lkm=Ll(kk-1);
	for jj=1:length(lk) do
	  lk(jj)=lkm(jj)+1;
	end
	Ll(kk)=lk;
      end
    endfunction
    
    Ll=INdxx(struc);
  endfunction
end 
