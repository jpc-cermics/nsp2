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

function [perm,L]=classmarkov(M)
// returns a permutation vector perm such that
// M(perm,perm) = [M11 0 0 0 0   0]
//                [0 M22 0 0     0]
//                [0 0 M33       0]
//                [      ...      ]
//                [0 0       Mrr 0]
//                [* *        *  Q]
// L is a cell which contains the classes descriptions 
// n=size(L,'*') is the number of classes ( n-1 reccurent and one transient)
// L{i} gives the vector of states of class i.
// 
// Each Mii is a Markov matrix of dimension size(L{i},'*') i=1,..,n-1
// Q is sub-Markov matrix of dimension tr = size(L{$},'*'); 
// perm is obtained by the concatenantion of 
// the elements of L 
// perm=[];for i=1:size(L,'*'), perm = [perm;L{i}];end
  
  L= tarjan(M);
  perm=[];for i=1:size(L,"*") do perm = [perm,L{i}];end
endfunction

