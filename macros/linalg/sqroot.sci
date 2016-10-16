function S=sqroot(Q)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - F. Delebecque (Inria)
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
  Q1=(Q+Q')/2;
  if norm(Q1-Q,1) > 100*%eps then 
    printf("Warning: in sqroot, input is not symmetric!\n");
  end
  tt=min(spec(Q1));
  if tt <-10*%eps then 
    printf("Warning: in sqroot, input is not semi-definite positive!\n");
  end
  if norm(Q,1) < sqrt(%eps) then S=[];return;end
  [u,S,v,rk]=svd(Q);
  S=diag(S);
  S=v(:,1:rk)*sqrt(S(1:rk,1:rk));
  if norm(imag(Q1),1) <1.E-8 then S=real(S);end
endfunction
