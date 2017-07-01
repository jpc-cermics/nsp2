function [f,d]=invr(h,flag)
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

  //if h is a scalar, polynomial or rational fonction matrix, invr
  //computes h^(-1).
  //!
  if nargin==1 then flag='C';end
  printf("Warning: invr is deprecated, you should call inv directly\n");
  if type(h,'short')=='m' then 
    f=inv(h);
    if nargout==2 then d=1;end
    return;
  end
  if nargout <= 1 then 
    f=inv(h,flag);
  else
    [f,d]=inv(h,flag);
  end
endfunction

function [f,d]=inv_p(h,flag)
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

  if nargin==1 then flag='C';end
  [m,n]=size(h);   
  if m<>n then error("Error: matrix should be square");return;end
  ndeg=max(h.degree[]);

  if nargin == 1 && ndeg==1 then
    //  try to detect a MATRIX PENCIL
    // printf("Matrix pencil case:\n");
    E=coeff(h,1);A=-coeff(h,0);
    if norm(E-eye(size(E)),1) < 100*%eps then
      // sI -A 
      [num,den]=coff(A,h.get_var[]);f=num/den;
      return
    end
    [Bfs,Bis,chis]=glever(E,A,h.get_var[]);
    f=Bfs/chis - Bis;
    if nargout==2 then
      d=lcm(f.den);
      f=f*d;f=f.num;
    end
    return;
  end
  //     GENERAL POLYNOMIAL MATRIX 
  select flag
    case 'L' then
      f=eye(n,n);
      for k=1:n-1 do
	b=h*f,
	d=-sum(diag(b))/k
	f=b+eye(n,n)*d,
      end
      d=sum(diag(h*f))/n,
      if d.degree[]==0 then d=coeff(d),end,
      if nargout <=1 then f=f ./ d;end
      return;
    case 'C' then
      [f,d]=coffg(h);
      if d.degree[]==0 then d=coeff(d),end
      if nargout <=1 then f=f ./ d;end
      return;
    else
      error("Error: when given, flag should be ""C"" or ""L""");
      return;
  end
endfunction 

function [f,d]=inv_r(h,flag)
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
  if nargin==1 then flag='C';end
  [m,n]=size(h);
  if m<>n then error("Error: matrix should be square");return;end
  select flag
    case 'L' then
      //  Leverrier 
      f=eye(n,n);
      for k=1:n-1 do
	b=h*f,
	d=0;for l=1:n do d=d+b(l,l),end,d=-d ./ k;
	f=b+eye(n,n)*d,
      end
      b=h*f;d=0;for l=1:n do d=d+b(l,l),end;d=d ./ n,
      if nargout <=1 then f=f ./ d;end
      return;
    case 'A' then
      // lcm of all denominator entries
      denh=lcm(h.den);
      Num=h*denh;Num=Num.num;
      [N,d]=coffg(Num);
      f=N*denh; 
      if nargout <=1 then f=f/d;end
      return;
    case 'C' then
      // default method by polynomial inverse
      [Nh,Dh]=lcmdiag(h); //h=Nh*inv(Dh); Dh diagonal;
      [N,d]=coffg(Nh);
      f=Dh*N;
      if nargout <=1 then f=f/d;end
      return;
    case 'Cof' then
      // cofactors method
      [f,d]=coffg(h);
      if nargout <=1 then f= f ./ d;end
      return;
  end
endfunction
