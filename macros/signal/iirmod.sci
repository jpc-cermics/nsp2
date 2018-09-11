function [la,grad,ind]=iirmod(x,ind)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - F. Delebecque et all (INRIA)
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
  
//    p===> critere Lp
//    r=module des poles et zeros_deprecated des filtres
//theta=argument des  "    "   "    "    "
//omega=frequences ou sont donnees les specifications des filtres
//wa=fonction de ponderation pour l'amplitude

  r=x(:,1);theta=x(:,2);
  [m,n]=size(ad);if m>n,ad=ad';end
  [m,n]=size(omega);if m>n,omega=omega';end
  [m,n]=size(r);if n>m,r=r';m=n;end
  [m,n]=size(theta);if n>m,theta=theta';m=n;end
  m=m/2;
  //
  //VARIABLES LOCALES
  unr=ones_deprecated(r);unom=ones_deprecated(omega);un=unr(1:m,:).*.unom;
  n1=r.*.unom;n2=unr.*.omega;n3=theta.*.unom;n4=(unr+r.*r).*.unom;
  cos1=[];cos2=[];sin1=[];sin2=[];
  for k=(n2-n3),cos1=[cos1 cos(k)];sin1=[sin1 sin(k)];end
  for k=(n2+n3),cos2=[cos2 cos(k)];sin2=[sin2 sin(k)];end
  //
  //FORMES QUADRATIQUES
  k1=n4-2*n1.*cos1;k2=n4-2*n1.*cos2;
  //
  //AMPLITUDE
  ampl=[];k3=k1(1:m,:);k4=k1(m+1:2*m,:);k5=k2(1:m,:);k6=k2(m+1:2*m,:);
  a1=(k3.*k5)./(k4.*k6);
  for k=a1,ampl=[ampl sqrt(prod(k))];end
  //
  //GRADIENT DE L'AMPLITUDE
  grrampl=(n1-cos1)./k1+(n1-cos2)./k2;
  grrampl(m+1:2*m,:)=-grrampl(m+1:2*m,:);
  grtampl=-(n1.*sin1)./k1+(n1.*sin2)./k2;
  grtampl(m+1:2*m,:)=-grtampl(m+1:2*m,:);
  //
  //CRITERE D'ERREUR EN AMPLITUDE ET SON GRADIENT
  a=ampl-ad;a1=a.^(2*p);a1=a1.*wa;la=sum(a1);
  a1=(a1./a)*2*p;a1=a1.*ampl;
  grad=[grrampl*a1' grtampl*a1'];
endfunction
