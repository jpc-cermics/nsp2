function r=routh_t(h,k)
  // Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
  // Copyright (C) 1987-2017 - F. Delebecque et all (INRIA)
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
  // 

  // r=routh_t(h,k) computes routh table of denominator of the
  // system described by transfer matrix SISO continue h with the
  // feedback by the gain k
  // If  k=poly(0,'k') we will have a polynomial matrix with dummy variable 
  // k, formal expression of the Routh table.
  // r=routh_t(d) computes Routh table of h :attention ! d=denom of system
  // modified 15/10/99, by Lucien.Povy@eudil.fr to get a good table

  if nargin==2 then
    if type(h,'short')<> 'r' then
      error("Error: first argument should be rational")
    end
    n=h.num;d=h.den;
    nd=max([d.degree[],n.degree[]])+1;
    cod=coeff(d,0:nd-1);//coeff du denominateur
    con=coeff(n,0:nd-1);//coeff du numerateur
    cobf=cod+k*con;//coeff de la boucle fermee
  else
    if type(h,'short')<>'p' then error('argument must be polynomial'),end
    nd=h.degree[]+1;
    cobf=coeff(h,0:nd-1)
  end

  //
  r1=cobf(nd:-2:1);
  r2=cobf(nd-1:-2:1);
  ncol=length(r1);
  if length(r2)<>ncol then r2=[r2,0],end
  r=[r1;r2]

  if ncol<2 then r=[],return,end
  if nargin==2 then
    for i=3:nd do
      r(i,1:ncol-1)=[r(i-1,1),-r(i-2,1)]*[r(i-2,2:ncol);r(i-1,2:ncol)]
    end
  else
    for i=3:nd do
      if r(i-1,1)==0 then 
	r(i,1:ncol-1)=[1.,-r(i-2,1)*%inf]*[r(i-2,2:ncol);r(i-1,2:ncol)]
      else
	r(i,1:ncol-1)=[1.,-r(i-2,1)/r(i-1,1)]*[r(i-2,2:ncol);r(i-1,2:ncol)]
      end
    end
  end
endfunction
