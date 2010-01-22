function A = nchoosek (v, k)
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
// PURPOSE
//if v=integer, A=n!/(k! (n-k)!)
//if v=vector(1:n), A=the set of all combinations of k entries chosen in v.
//In that case, A is a matrix with length(v) rows and k columns, each row
//contains a choice of k entries in (1:n).
//FD, Inria.
  n = length(v);

  if (n == 1)
     A = round(exp(sum(log(k+1:v)) - sum(log(2:v-k))));
  elseif (k == 0)
    A = [];
  elseif (k > n)
    A = [];
  elseif (k == 1)
    A = v(:);
  elseif (k == n)
     A = v(:).';
  else
    m = round (exp(sum(log(k:n-1)) - sum(log(2:n-k))));
    A = [v(1)*ones(m,1), nchoosek(v(2:n),k-1);
	 nchoosek(v(2:n),k)];
  end
endfunction


