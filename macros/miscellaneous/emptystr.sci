//
// Copyright (C) 2008-2016 Jean-Philippe Chancelier
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
// A set of miscellaneous functions

function A=emptystr(a,b)
  if nargin==0 then
    A=smat_create(1,1);
  elseif nargin==1 then
    A=smat_create(size(a,1),size(a,2));
  else
    A=smat_create(a,b);
  end
endfunction

function B=string(A)
// a revoir
  if type(A,'short')=='s' then
    B=A;
  elseif type(A,'short')=='b' then
    B=string(b2m(A));
    B=strsubst(B,'1','T');
    B=strsubst(B,'0','F');
  elseif type(A,'short')=='i' then
    B=m2s(i2m(A));
  else
    B=m2s(A);// used defaut format
  end
endfunction

function y=typeof(x)
  y=type(x,'short');
  if y == 'l' then  y = x(1)(1);
  elseif y== 'i' then
    y=x.itype[];
  end
endfunction

function y=strcat(mat,sep)
  if nargin <= 1, sep ="";end
  y=catenate(mat,sep=sep);
endfunction

function y=str2code(str)
  y=ascii(str)(:)
endfunction

function y=diffobjs(A,B)
  y=~A.equal[B];
endfunction

function scicos_mputl(str,fname)
  F=fopen(fname,mode="w");
  F.put_smatrix[str];
  F.close[];
endfunction

function str=scicos_mgetl(fname)
  F=fopen(fname,mode="r");
  str=F.get_smatrix[];
  F.close[];
endfunction
