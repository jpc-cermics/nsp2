function [xf,yf,zf,qf] = nf3d(x,y,z,q,orient)
   //
   //  Rewritten by Bruno Pincon so as:
   //    - to be faster (about 2 times than the old version)
   //    - to have also a eventual val field q
   //      (useful when someone want to display
   //       some quantity on the surface)
   //    - and also an orientation parameter
   //
   //   j+1 +----+     orient=1 corresponds to :
   //       |    |       (i,j) -> (i,j+1) -> (i+1,j+1) -> (i+1,j) 
   //    j  +----+
   //       i   i+1    orient=2 corresponds to :
   //                    (i,j) -> (i+1,j) -> (i+1,j+1) -> (i,j+1) 
   // 
   if nargin < 5 then, orient=2, end  // orientation of the old version
   if nargin < 3 then, error("nf3d needs at least 3 arguments"), end
   [n1,n2]=size(x)
   nx = (n1-1)*(n2-1);
   v = 1:(n1*n2-n1-1); v(n1*(1:(n2-2)))=[];
   if orient == 1 then
      ind = [v; v+n1; v+(n1+1); v+1];
   else
      ind = [v; v+1; v+(n1+1); v+n1];
   end
   xf=matrix(x(ind),4,nx)
   yf=matrix(y(ind),4,nx)
   zf=matrix(z(ind),4,nx)
   if nargin > 3 then
      qf=matrix(q(ind),4,nx)
   else
      qf = []
   end
endfunction
