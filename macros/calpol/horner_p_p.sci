function R=horner_p_p(P,Q, vdim=%f, ttmode=%f)
// Copyright  2010 Jean-Philippe Chancelier 
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
// extension of horner(P,Q) when 
// Q is a polynomial matrix 
//
  [mp,np]=size(P);
  [mq,nq]=size(Q);
  if ttmode then 
    // in ttmode P and Q should have the same size 
    if mp*np <> 1 && mq*nq <> 1 && ~(mp==np && mq==nq) then 
      error("Error: argument should be of the same size for term to term mode");
      return;
    end
    D= Q.degree[];
    if max(D)==0 then 
      // Q contains just polynoms of degree 0 
      C= Q.coeffs;
      Q0= ce2m(C,noti=0);
      R=horner(P,Q0,vdim=vdim,ttmode=ttmode);
      return;
    end
    mr = max(mp,np);
    nr = max(mq,nq);
    R=cell(mr,nr);
    if mp*np == 1 then 
      for i=1:mq*nq;
	R{i}=compose(P,Q(i));
      end
    elseif mq*nq==1 then 
      for i=1:mq*nq;
	R{i}=compose(P(i),Q);
      end
    else
      for i=1:mq*nq;
	R{i}=compose(P(i),Q(i));
      end
    end      
    return;
  end
  
  // ttmode is false here 
  
  D= Q.degree[];
  if max(D)==0 then 
    // Q contains just polynoms of degree 0 
    C= Q.coeffs;
    Q0= ce2m(C,noti=0);
    R=horner(P,Q0,vdim=vdim);
    return;
  end
  if vdim then 
    // a cell dimensioned by Q 
    R=cell(mq,nq)
    for i=1:mq*nq;
      Ri= pmat_create(0,0);
      for j=1:mp*np; 
	Ri= [Ri,compose(P(i),Q(j))];
      end
      Ri.redim[mp,np];
      R{i}=Ri;
    end
  else
    // here we return a cell dimensioned by P
    R=cell(mp,np);
    for i=1:mp*np;
      Ri= pmat_create(0,0);
      for j=1:mq*nq; 
	Ri= [Ri,compose(P(i),Q(j))];
      end
      Ri.redim[mq,nq];
      R{i}=Ri;
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

  

