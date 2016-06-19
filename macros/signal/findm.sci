function m=findm(chi)
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
  
//Search for m such that chi = %k(1-m)/%k(m)

  if chi < 1 then
    t=1;
    tn=2;
    m=0.99999;
    mn=2;
    v=16*exp(-%pi/chi);
    while abs(t-tn) > 10.*%eps
      t=tn;
      lln=log(16/(1-m));
      k1=%asn(1,1-m);
      k=%asn(1,m);
      y=(k1*lln/%pi)-k;
      mn=m;
      m=1-v*exp((-%pi*y)/k1);
      tn=m+mn;
    end
  else
    t=1;
    tn=2;
    m=0.00001;
    mn=0.1;
    v=16*exp(-%pi*chi);
    while abs(t-tn) > 10.*%eps
      t=tn;
      lln=log(16/m);
      k1=%asn(1,1-m);
      k=%asn(1,m);
      y=(k*lln/%pi)-k1;
      mn=m;
      m=v*exp((-%pi*y)/k);
      tn=m+mn;
    end
  end
endfunction
