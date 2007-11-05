function [varargout] = ndgrid(varargin)
   //
   //  CALLING SEQUENCES
   //       [X, Y] = ndgrid(x, y)
   //    or [X, Y, Z] = ndgrid(x, y, z)   
   //    or [X, Y, Z, T] = ndgrid(x, y, z, t)
   //    etc ...
   //
   //    or if the grid is the same in all direction:
   //
   //       [X, Y] = ndgrid(x)
   //    or [X, Y, Z] = ndgrid(x)   
   //    or [X, Y, Z, T] = ndgrid(x)
   //    etc ...
   //
   //
   //  PURPOSE
   //     An utility similar to the Matlab 's one. For instance
   //     in the 3d case : 
   //
   //     [X, Y, Z] = ndgrid(x, y, z) 
   //
   //     with x, y, z three vectors of length nx, ny, nz, we have
   //     X, Y, Z which are matrices of format nx x ny x nz
   //     and, forall  i in [1,nx], j in [1, ny], k in [1, nz]
   //         X(i,j,k) = x(i)
   //         Y(i,j,k) = y(j)  
   //         Z(i,j,k) = z(k)
   //
   //     X, Y, Z are the coordinates of the grid points formed by
   //     the cartesian product of the one dimensional grids
   //     x, y and z.
   //
   //     As we don't have yet n-dim matrices in nsp we get in fact
   //     2d matrices with the second dimension length equal to the product
   //     of all the dim length from dim_2 to dim n. 
   // 
   //     for 3d mat:  nx x (ny*nz)
   //     for 4d mat:  nx x (ny*nz*nt)
   //     etc...
   //
   //  AUTHOR
   //     Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr>
   //
     if ( nargin == 1 & nargout > 1 ) then 
       // [X,Y,..] = ndgrid(x):  same grid in all direction
       nbdim = nargout; inc = 0
     elseif nargin > 1 & nargout <= nargin then
       nbdim = nargin; inc = 1
     else
       error(" ndgrid: too much output arguments")
     end
     
     if nbdim < 2 then
      error(" ndgrid must have at least 2 input or output args")
     end
     dim = zeros(1,nbdim)
     for k = 1:nargin
       if type(varargin(k),'short') ~= 'm' then  
	 error(" ndgrid : bad "+string(k)+" th arg")
       end
       dim(k) = length(varargin(k))
       varargin(k).redim[1,-1] // force row form
     end
     if ( inc == 0 ) then, dim(2:$) = dim(1), end

     // try to simulate n-dimensional matrix in nsp: the following 3
     // lines should be removed and fdim replaced by dim in the last redim
     // method: varargout(k).redim[fdim] => varargout(k).redim[dim]
     fdim = zeros(1,2)
     fdim(1) = dim(1)
     fdim(2) = prod(dim(2:$));
     
     i = 1;
     varargout = list()
     for k = 1:nbdim
       varargout(k) = repmat(varargin(i),prod(dim(1:k-1)),prod(dim(k+1:$)))
       i.add[inc]
       varargout(k).redim[fdim]
     end
endfunction
