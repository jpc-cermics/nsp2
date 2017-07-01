function [N,D]=lcmdiag(H,flag)
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

  // returns N and diagonal D
  // such that: 
  //  if flag='row' => H=D^(-1)*N  and D(k,k)=lcm of kth row of H('den')
  //  if flag='col' => H=N*D^(-1)  and D(k,k)=lcm of kth col of H('den')
  //  default flag = 'col'
  // 
  Num=H.num;
  Den=H.den;
  [m,n]=size(H);
  D=m2p([],var=H.get_var[]);N=D;
  if nargin==1 then flag='col';end
  select flag
    case 'row' then
      for k=1:m do
	[pgcd,fact]=lcm(Den(k,:)); 
	D=diag([diag(D);pgcd]);
	N=[N;Num(k,:).*fact];
      end
    case 'col' then
      for k=1:n do
	[pgcd,fact]=lcm(Den(:,k)); 
	D=diag([diag(D);pgcd]);
	N=[N,Num(:,k).*fact];  
      end
  end
endfunction
