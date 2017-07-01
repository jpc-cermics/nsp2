function [Ns,d]=coffg(Fs)
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

  // [Ns,d]=coffg(Fs) computes Fs^-1 where Fs is a polynomial
  // or rational matrix by co-factors method.
  // d = common denominator; Ns =  numerator (matrix polynomial)
  // Fs inverse = Ns/d.
  // (Be patient...results are generally reliable)

  // Author Francois Delebecque 
  // This function should not be called directly 
  // but should be accessed through inv(Fs,'C');
  // 

  [n,np]=size(Fs);
  if n<>np then error('First argument to coffg must be square!');end
  d=det(Fs); // common denominator
  // create an empty variable similar to Fs
  // should work also for standard matrices
  // XXX: need to extends zeros(0,0,like=..)
  vname='x';
  ok=execstr('vname=Fs.get_var[]',errcatch=%t);
  if ~ok then lasterror();end
  T=type(Fs);
  Ns=T.new[0,0,var=vname];
  n1=n;
  if n1 == 1 then 
    Ns= 1 ./ Fs; d= Ns.den ; Ns=Ns.num;
  else
    for kk=1:n1 do
      for l=1:n1 do
	signe=(-1)^(kk+l);
	col=[1:kk-1,kk+1:n1];row=[1:l-1,l+1:n1];
	Ns(kk,l)=-signe*det(Fs(row,col))
      end
    end
  end
  Ns=-Ns;
endfunction
