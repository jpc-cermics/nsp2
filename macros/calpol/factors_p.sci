function [lnum,lden,g]=factors_r(P,flag)
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

  // Given a polynomial or rational P, returns in a polynomial 
  // matrix lnum polynomials of degree 1 or two which are the factors 
  // of numerators of P.
  // and in lden the factors of denominator of P. g is the gain.
  // if flag=='c' unstable roots are reflected vs the imaginary axis 
  // if flag=='d' unstable roots are reflected vs unit circle 
  // 

  if nargin==1 then flag='v';end
  if isempty(flag) then flag = 'v';end 
  [lnum,gn]=factors(P.num,flag);
  [lden,gd]=factors(P.den,flag);
  g=gn/gd;
  if nargout <=1 then
    // XXX dt to be added 
    num=g*prod(lnum);
    den=prod(lden);
    lnum=p2r(num,den);
  end
endfunction

function [resn,g]=factors_p(pol,flag)
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

  if nargin==1 then flag='v';end
  if isempty(flag) then flag = 'v';end 
  w=roots(pol);
  n=size(w,'*');
  if n==0 then resn=list();g=coeff(pol);return;end
  co=coeff(pol);g=co(n+1);
  resn=m2p([],var=pol.get_var[]);
  kk=1;k=1;
  if nargin==1 then flag='v';end
  select flag 
    case 'v' then 
      // void version 
      while %t do
	if abs(imag(w(kk)))<=%eps then
	  resn(k)=poly(w(kk), pol.get_var[]);
	  kk=kk+1;k=k+1;
	  if kk>n then return;end
	end
	if abs(imag(w(kk)))>%eps then 
	  resn(k)=real(poly([w(kk),w(kk+1)],pol.get_var[]));
	  kk=kk+2;k=k+1;
	  if kk>n then return;end
	end
      end
    case 'c' then 
      while %t do
	if abs(imag(w(kk)))<=%eps then
	  resn(k)=poly(-abs(w(kk)), pol.get_var[]);
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
    case 'd' then
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
	    zp=[wkk,w(kk+1)];resn(k)=real(poly(ones(zp)./zp,pol.get_var[]));
	  end
	  kk=kk+2;k=k+1;
	  if kk>n then return;end
	end
      end
  end 
endfunction
