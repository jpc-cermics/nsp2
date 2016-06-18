function y=horner_r(r,a,ttmode=%f,vdim=%t)
// Copyright  2010-2015 Jean-Philippe Chancelier 
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
// horner when first argument is rational 
  C1=horner(r.num,a,ttmode=ttmode,vdim=vdim)
  C2=horner(r.den,a,ttmode=ttmode,vdim=vdim)
  if ttmode then 
    y = C1 ./ C2 
  else
    y={};
    for i=1:size(C1,'*')
      y{i}= C1{i}./C2{i};
    end
  end
endfunction

function y=horner_p(p,x,vdim=%t,ttmode=%f)
// Copyright  2010-2015 Jean-Philippe Chancelier 
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
// generic horner for pmatrix and various x 
// when x is a matrix a hard coded function is used.
  
  function y=horner_gen_polynom(p,x)
  // generic horner for polynom p and various x 
  // just assume that x multiplied by numerical scalar
  // exists
    c = p.coeffs{1}
    d = p.degree[];
    y= c($)
    for i=d:-1:1
      y = x.*y + c(i);
    end
  endfunction
  
  function y=horner_gen_p_scalar(p,x)
  // P is a polynomial and x a scalar (m or r or p)
  // just assume that x multiplied by numerical scalar
  // exists
    c = p.coeffs;
    d = max(p.degree[]);
    y= ce2m(c,indice=d+1,noti=0)
    for i=d:-1:1
      y = x*y + ce2m(c,indice=i,noti=0)
    end
  endfunction
  
  if ttmode then 
    [mp,np]=size(p);
    [mq,nq]=size(x);
    // in ttmode P and Q should have the same size or be promoted
    if mp*np <> 1 && mq*nq <> 1 && ~(mp==mq && np==nq) then 
      error("Error: argument should be of the same size for term to term mode");
      return;
    end
    // 
    mnp =mp*np;mnq=mq*nq;
    y={}
    for i=1:max(mnp,mnq)
      y{i} = horner(p(min(i,mnp)),x(min(i,mnq)));
    end
  else
    if vdim then 
      // y has x dimension 
      y=cell(size(x,1),size(x,2))
      for i = 1: size(y,'*')
	y{i} = horner_gen_p_scalar(p,x(i));
      end
    else
      // y has the dimension of p 
      y=cell(size(p,1),size(p,2))
      for i = 1: size(y,'*')
	y{i} = horner_gen_polynom(p(i),x)
      end
    end
  end
endfunction

if %f then 
  n=10;
  x=m2p([0,1]); // x=poly(0,'x');
  x0=m2p([1]); // x0 = 0*x;
  P=rand(n,n)*x0+ rand(n,n)*x+rand(n,n)*x*x;
  Q=rand(n,n)*x0+ rand(n,n)*x+rand(n,n)*x*x;
  timer();R=horner(P,Q);timer()
end
