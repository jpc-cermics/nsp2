function [resn,g]=pfactors(pol,flag)
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

  // Given polynomial pol returns in list resn polynomials of
  // degree 1 or two which are the factors of pol.
  // One has pol= g times product of entries of resn
  // if flag=='c' unstable roots are reflected vs the imaginary axis
  // if flag=='d' unstable roots are reflected vs unit circle
  //

  w=roots(pol);
  n=size(w,'*');
  if n==0 then resn=list();g=coeff(pol);return;end
  co=coeff(pol);g=co(n+1);
  resn=list();

  if nargin==1 then flag=[];end
  if isempty(flag) then nargin=1;end
  if nargin==1 then
    kk=1;k=1;
    while %t do
      if abs(imag(w(kk)))<=%eps then
	resn(k)=poly(w(kk),pol.get_var[]);
	kk=kk+1;k=k+1;
	if kk>n then return;end
      end
      if abs(imag(w(kk)))>%eps then
	resn(k)=real(poly([w(kk),w(kk+1)],pol.get_var[]));
	kk=kk+2;k=k+1;
	if kk>n then return;end
      end
    end
  end;//nargin=1
  if nargin==2 then
    kk=1;k=1;
    if flag=='c' then
      while %t do
	if abs(imag(w(kk)))<=%eps then
	  resn(k)=poly(-abs(w(kk)),pol.get_var[]);
	  kk=kk+1;k=k+1;
	  if kk>n then return;end
	end
	if abs(imag(w(kk)))>%eps then
	  if real(w(kk))<0 then
	    resn(k)=real(poly([w(kk),w(kk+1)],pol.get_var[]));
	  else
	    resn(k)=real(poly([-w(kk),-w(kk+1)],pol.get_var[]));
	  end
	  kk=kk+2;k=k+1;
	  if kk>n then return;end
	end
      end
    end;//'c'
    if flag=='d' then
      while %t do
	wkk=w(kk);
	if abs(imag(wkk))<=%eps then
	  [themin,which]=min([abs(wkk),1/(abs(wkk))]);
	  if which==2 then g=-g*real(wkk);end
	  resn(k)=poly(sign(real(wkk))*themin,pol.get_var[]);
	  kk=kk+1;k=k+1;
	  if kk>n then return;end
	end
	if abs(imag(wkk))>%eps then
	  if abs(wkk)<1 then
	    resn(k)=real(poly([wkk,w(kk+1)],pol.get_var[]));
	  else
	    //   g=g*wkk*w(kk+1); w(kk+1)= conj(wkk)
	    g=g*abs(wkk)^2;
	    zp=[wkk,w(kk+1)];resn(k)=real(poly(ones(zp) ./zp,pol.get_var[]));
	  end
	  kk=kk+2;k=k+1;
	  if kk>n then return;end
	end
      end
    end;//'d'
  end;//nargin=2
endfunction
