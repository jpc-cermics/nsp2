function F=randpencil(eps,infi,fin,eta)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - F. Delebecque (Inria)
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
//returns a random pencil with given kronecker structure 
//eps=[eps1,...,epsk]; epsilon blocks (size eps1x(eps1+1),....)
//fin=[l1,...,ln]  finite eigenvalues (assumed real)  (possibly [])
//infi=[k1,...,kp] size of J blocks, ki>=1  (infi=[] if no J blocks)
//eta=[eta1,...,etap];   eta blocks size (eta1+1)xeta1,...)
// epsi's should be >=0 
// etai's should be >=0
// infi's should be >=1
// If called with eps=[0,...,0], infi=[], fin=[], eta=[]
// randpencil returns F=[];
// this should be an empty matrix with zero rows and coldim(eps) columns
// If called with eps=[], infi=[], fin=[], eta=[0,..,0]
// randpencil returns F=[];
// this should be a empty matrix with coldim(eta) rows and 0 columns.
// (bad behavior of the empty matrix!!!!!)
//
  %s = poly(0,"s");
  %zero = poly(0,"s",roots=%f);
  if nargin <>4 then 
    error("randpencil requires 4 (possibly []) input parameters!");
  end
  select type(fin,"short")
   case "m" then 
    D=diag(fin);
    if ~isempty(D) then Fin=%s*eye(size(D))- D;else Fin=pmat_create(0,0);end
   case "p" then
    C=companion(fin);
    Fin=%s*eye(size(C)) - C;
  end
  
  function j=%jdrn(n) j=zeros(n,n);for k=1:n-1 do j(k,k+1)=1;end;endfunction;

  function Lk=%eta(k) 
    Lk=zeros(k+1,k);
    // turn Lk to polynom 
    // [me,ne]=size(Lk);Lk = ce2p(m2ce(Lk,1:me,1:ne));
    Lk = Lk + %zero;
    if k==0 then return;end
    for j=1:k do Lk(j,j)=%s;Lk(j+1,j)=-1+ %zero;end
  endfunction;
  
  function Lk=%epsilon(k) 
    Lk=zeros(k,k+1) + %zero ;
    if k==0 then return;end
    for j=1:k do Lk(j,j)=%s;Lk(j,j+1)=-1 + %zero;end
  endfunction;

  J=[];
  for kk=infi do
    J= [J # %jdrn(kk)];
  end
  if isempty(J) then Infin=pmat_create(0,0); else Infin=%s*J-eye(size(J));end


  // Eps is polynomial 
  flageps=%f;
  Eps=[] + %zero;
  seps=sort(eps);
  if ~isempty(seps) && seps(1)==0 then flageps=%t;end
  if ~flageps then
    for k=seps do
      if k==0 then [p,q]=size(Eps); Eps=[Eps,zeros(p,1) + %zero];end
      if k<>0 then Eps=[ Eps # %epsilon(k)];end
    end
  end

  // Eta is polynomial 
  flageta=%f;
  Eta=[] + %zero ;
  seta=sort(eta);
  if ~isempty(seta) && seta(1)==0 then flageta=%t;end
  if ~flageta then
    for k=seta do
      if k==0 then [p,q]=size(Eta); Eta=[Eta; zeros(1,q)+%zero];end
      if k<>0 then Eta=[Eta # %eta(k)];end
    end
  end

  // should be simplified with a new function 
  F=[ Eps # Infin # Fin # Eta];

  [p,q]=size(F);ncols=q;

  if flageps then
    F=[zeros(p,prod(size(eps))),F];
    if isempty(F) then ncols=prod(size(eps));end
  end

  if flageta then
    [p,q]=size(F);
    if ~isempty(F) then
      F=[F;zeros(prod(size(eta)),q)];
    else
      F=[F;zeros(prod(size(eta)),ncols)];
    end
  end
  // This can be uncommented for a seemingly more random pencil!
  //[p,q]=size(F);
  //rand("seed",0);
  //rand("normal")
  //Q=rand(p,p);
  //Z=rand(q,q);
  //F=Q*F*Z;
endfunction

