// -*- Mode: scilab -*- 
// Copyright (C) 2009 B. Pincon Iecn/Esial
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
// Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
// 
// Tests for min, max and minmax functions on int arrays
// 

for int_type = ["int", "uint", "int8", "uint8", "int16", "uint16", "int32", ...
		"uint32", "int64", "uint64"] 
   x = m2i([ 2, 5, 0, 3, 14, 4], int_type);
   mm = m2i(0, int_type); imm = 3;
   MM = m2i(14, int_type); iMM = 5;  

   m = min(x); M = max(x);
   if ~( m.equal[mm] && M.equal[MM] ) then, pause, end

   [m,im] = min(x); [M,iM] = max(x);
   if ~( m.equal[mm] && M.equal[MM] ) then, pause, end
   if ~( im.equal[imm] && iM.equal[iMM] ) then, pause, end   
   
   [m,M] = minmax(x);
   if ~( m.equal[mm] && M.equal[MM] ) then, pause, end
  
   [m,M,im,iM] = minmax(x);
   if ~( m.equal[mm] && M.equal[MM] ) then, pause, end
   if ~( im.equal[imm] && iM.equal[iMM] ) then, pause, end   

   
   x = m2i([ 2, 5, 0,  3, 14, 4;...
	     1, 6, 9, 11,  0,15;...
	     4, 7, 3, 13,  5, 2], int_type);

   direction = 0;
   mm = m2i(0, int_type); imm = 7;
   MM = m2i(15, int_type); iMM = 17;  
   
   m = min(x,dim=direction); M = max(x,dim=direction);
   if ~( m.equal[mm] && M.equal[MM] ) then, pause, end

   [m,im] = min(x,dim=direction); [M,iM] = max(x,dim=direction);
   if ~( m.equal[mm] && M.equal[MM] ) then, pause, end
   if ~( im.equal[imm] && iM.equal[iMM] ) then, pause, end   
   
   [m,M] = minmax(x,dim=direction);
   if ~( m.equal[mm] && M.equal[MM] ) then, pause, end
  
   [m,M,im,iM] = minmax(x,dim=direction);
   if ~( m.equal[mm] && M.equal[MM] ) then, pause, end
   if ~( im.equal[imm] && iM.equal[iMM] ) then, pause, end   
   
   direction = 1;
   mm = m2i([1, 5, 0, 3, 0, 2], int_type); 
   imm = [2, 1, 1, 1, 2, 3];
   MM = m2i([4, 7, 9, 13, 14, 15], int_type); 
   iMM = [3, 3, 2, 3, 1, 2];  
   
   m = min(x,dim=direction); M = max(x,dim=direction);
   if ~( m.equal[mm] && M.equal[MM] ) then, pause, end

   [m,im] = min(x,dim=direction); [M,iM] = max(x,dim=direction);
   if ~( m.equal[mm] && M.equal[MM] ) then, pause, end
   if ~( im.equal[imm] && iM.equal[iMM] ) then, pause, end   
   
   [m,M] = minmax(x,dim=direction);
   if ~( m.equal[mm] && M.equal[MM] ) then, pause, end
  
   [m,M,im,iM] = minmax(x,dim=direction);
   if ~( m.equal[mm] && M.equal[MM] ) then, pause, end
   if ~( im.equal[imm] && iM.equal[iMM] ) then, pause, end   
   
   x = m2i([ 2, 5, 0,  3, 14, 4;...
	     1, 6, 9, 11,  0,15;...
	     4, 7, 3, 13,  5, 2], int_type);

   direction = 2;
   mm = m2i([0; 0; 2], int_type); 
   imm = [3; 5; 6];
   MM = m2i([14; 15; 13], int_type); 
   iMM = [5; 6; 4];  
   
   m = min(x,dim=direction); M = max(x,dim=direction);
   if ~( m.equal[mm] && M.equal[MM] ) then, pause, end

   [m,im] = min(x,dim=direction); [M,iM] = max(x,dim=direction);
   if ~( m.equal[mm] && M.equal[MM] ) then, pause, end
   if ~( im.equal[imm] && iM.equal[iMM] ) then, pause, end   
   
   [m,M] = minmax(x,dim=direction);
   if ~( m.equal[mm] && M.equal[MM] ) then, pause, end
  
   [m,M,im,iM] = minmax(x,dim=direction);
   if ~( m.equal[mm] && M.equal[MM] ) then, pause, end
   if ~( im.equal[imm] && iM.equal[iMM] ) then, pause, end   
 
end
