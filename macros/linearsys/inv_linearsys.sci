// Copyright  2010-2015 
// Jean-Philippe Chancelier Cermics/Enpc, François Delebeceque, Serge Steer (Inria)
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

function x=inv_linearsys(a)
  d=a(5); // in order to obtain a matrix if degree is zero
  [m,n]=size(d);
  polyn=(type(d,'short')=='p');
  constant=(type(d,'short')=='m');
  
  if constant&(m==n) then 
    minsv=min(svd(d));rcd=rcond(d);s=poly(0,'s');
  end
  if constant&(m<>n) then 
    minsv=min(svd(d));s=poly(0,'s');
  end

  if polyn then rcd=0;minsv=0;s=poly(0,d.get_var[]);end
  if m==n then 
    if rcd > 1.d-6 then
      x=invsyslin(a)
    else
      h=systmat(a);
      se=grand("getsd");
      valfa=randn(1,10)/100;
      grand('setsd',se);
      www=[];
      for k=1:10
	www=[www,rcond(horner(h,valfa(k),ttmode=%t))];end
	[w,k1]=max(www);alfa=valfa(k1);
	x=invrs(a,alfa);
    end
  elseif m<n then
    printf("Warning: non square system! --> right inverse\n')
    if minsv > 1.d-6 then
      x=invsyslin(a)
    else
      [stmp,ws]=rowregul(a,0,0);
      if min(svd(stmp(5))) > 1.d-6 then
	x=invsyslin(stmp)*ws
      else
	Error("Error: problem is singular");
      end
    end
  elseif m>n then
    printf("Warning: non square system! --> left inverse\n')
    if minsv > 1.d-6 then
      x=invsyslin(a)
    else
      [stmp,ws]=rowregul(a,0,0);
      if min(svd(stmp(5))) > 1.d-6 then
	x=invsyslin(stmp)*ws
      else
	Error("Error: problem is singular");
      end
    end
  end
endfunction
