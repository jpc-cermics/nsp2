function Sm=systmat(Sl)
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

  // System matrix of the linear system Sl (syslin list)
  // in state-space form.
  // Sm = [-sE + A   B;
  //      [    C     D]
  // To get the zeros use det or detr (for square systems)

  if type(Sl,'short')=='linearsys' then
    if Sl.dom=='d' then
      s=poly(0,'z');
    else
      s=poly(0,'s');
    end
    Sm=[-s*eye(size(Sl(2)))+Sl(2),Sl(3);Sl(4),Sl(5)];
    return
  end
  if type(Sl,'short')=='h' && Sl.iskey['type'] && Sl.type=='des' then 
    // this is a descriptor for 
    s=poly(0,'s');
    Sm=[-s*Sl.E+Sl.A,Sl.B;Sl.C,Sl.D];
    return
  end
endfunction
