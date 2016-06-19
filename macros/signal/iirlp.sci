function [cout,grad,ind]=iirlp(x,ind,p,flag,lambda,omega,ad,wa,td,wt)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - G. Le Vey (INRIA)
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
//optimization of IIR filters IIR with  Lp criterium for magnitude
//                                      and/or group delay
//     -cf Rabiner & Gold pp270-273-
//   p ===> critere Lp
//
//r=module des poles et zeros_deprecated des filtres
//theta=argument des  "    "   "    "    "
//omega=frequences ou sont donnees les specifications des filtres
//wa,wt=fonctions de ponderation pour l'amplitude et le
//retard de groupe ad,td=amplitudes et retard de groupe desires

  r=x(:,1);theta=x(:,2);
  [m,n]=size(ad);if m>n,ad=ad';end
  [m,n]=size(td);if m>n,td=td';end
  [m,n]=size(omega);if m>n,omega=omega';end
  [m,n]=size(r);if n>m,r=r';m=n;end
  [m,n]=size(theta);if n>m,theta=theta';m=n;end
  //
  select flag
   case 'a'
    //AMPLITUDE
    [cout,grad]=iirmod(p,r,theta,omega,wa,ad);
    //
   case 'gd'
    //RETARD DE GROUPE
    [cout,grad]=iirgroup(p,r,theta,omega,wt,td);
    //
  else
    //AMPLITUDE ET RETARD DE GROUPE
    [la,ga]=iirmod(p,r,theta,omega,wa,ad);
    [lt,gt]=iirgroup(p,r,theta,omega,wt,td);
    cout=lambda*la+(1-lambda)*lt;
    grad=lambda*ga+(1-lambda)*gt;
  end
endfunction
