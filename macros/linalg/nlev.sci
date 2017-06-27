function[m,den]=nlev(a,z,rmax)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - F. Delebecque, S. Steer (Inria)
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
//[num,den]=nlev(a,z [,rmax])  calcule (z*eye-a)**(-1) par une version
//modifiee de l'algorithme de leverrier numeriquement plus stable.
//
//z     chaine de caracteres donnant le nom de la variable formelle
//rmax  parametre optionnel utilise pour bloc diagonaliser a (voir la
//      fonction bdiag)

  z=poly(0,z)
  if nargin==3 then 
    [a,x,bs]=bdiag(a,rmax),
  else 
    [a,x,bs]=bdiag(a),
  end
  [m1, n1]=size(a)
  if m1<>n1 then error("Error: argument must be a squared matrix");end
  k=1;m=[];v=0*z+ ones(1,n1);den=1;
  // bs is a row in nsp 
  for n=bs do 
    // Leverrier's algorythm
    k1=k:k-1+n;
    h=z*eye(n,n)-a(k1,k1)
    f=eye(n,n)
    for kl=1:n-1 do
      b=h*f,
      d=-sum(diag(b))/kl,
      f=b+eye(size(b))*d,
    end
    d=sum(diag(h*f))/n
    //
    den=den*d;
    l=[1:k-1,k+n:n1] ,
    if ~isempty(l) then v(l)=v(l)*d;end
    m=[m,x(:,k1)*f];
    k=k+n;
  end
  m=m*diag(v)*inv(x);
endfunction



