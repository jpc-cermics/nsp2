function [p] = primes_m(n)

// Copyright (C) 2007 Bruno Pinçon
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
// purpose
// -------  
// computes all the primes numbers less or equal to n using the
// Erathostenes. 1  is not considered to be a prime number.
//
  if length(n)~= 1 || ~isreal(n) then
    error("n should be a (real) scalar")
  end
  n = floor(n)
  if n < 2 then
    p = []
  elseif n == 2 then
    p = 2
  else
    if mod(n,2) == 0 then, n=n-1, end
    m = (n-1)/2;
    sieve = bmat_create(1,m)
    for i=3:2:sqrt(n)
      k = (i-1)/2
      if sieve(k) then
	sieve(k*(i+1):i:m) = %f
      end
    end
    p = [2,2*find(sieve)+1]
  end
endfunction
