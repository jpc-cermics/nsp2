// -*- Mode: scilab -*- 
// tests for unique

//
// with real matrices
//
A = [1, -1, -1, 1, 2, -1, 3, 2, -1];
res = [-1, 1, 2, 3];
res_ind_first = [2, 1, 5, 7];
res_occ = [4, 2, 2, 1]; 

B = unique(A);
if ~and(B == res) then, pause; end

[B,ind] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end

[B,ind,occ] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(occ == res_occ) then, pause; end

[B,ind,occ] = unique(A,first_ind=%t);
if ~and(B == res) then, pause; end
if ~and(ind == res_ind_first) then, pause; end
if ~and(occ == res_occ) then, pause; end

// transpose (to verify that we get outputs in column form)
A.redim[-1,1]; 
res.redim[-1,1];
res_ind_first.redim[-1,1];
res_occ.redim[-1,1];

B = unique(A);
if ~and(B == res) then, pause; end

[B,ind] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end

[B,ind,occ] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(occ == res_occ) then, pause; end

[B,ind,occ] = unique(A,first_ind=%t);
if ~and(B == res) then, pause; end
if ~and(ind == res_ind_first) then, pause; end
if ~and(occ == res_occ) then, pause; end

// for A a matrix verify that we get outputs in column form too
A.redim[3,3]; 

B = unique(A);
if ~and(B == res) then, pause; end

[B,ind] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end

[B,ind,occ] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(occ == res_occ) then, pause; end

[B,ind,occ] = unique(A,first_ind=%t);
if ~and(B == res) then, pause; end
if ~and(ind == res_ind_first) then, pause; end
if ~and(occ == res_occ) then, pause; end

//
// with string matrices
//
A = [ "aaa", "foo", "bar", "foo", "a", "", "foo", "bar", "" ];
res = [ "", "a", "aaa", "bar", "foo" ];
res_ind_first = [6, 5, 1, 3, 2];
res_occ = [ 2,   1,  1, 2, 3 ];

B = unique(A);
if ~and(B == res) then, pause; end

[B,ind] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end

[B,ind,occ] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(occ == res_occ) then, pause; end

// $$$ [B,ind,occ] = unique(A,first_ind=%t);
// $$$ if ~and(B == res) then, pause; end
// $$$ if ~and(ind == res_ind_first) then, pause; end
// $$$ if ~and(occ == res_occ) then, pause; end

// transpose (to verify that we get outputs in column form)
A.redim[-1,1]; 
res.redim[-1,1];
res_ind_first.redim[-1,1];
res_occ.redim[-1,1];

B = unique(A);
if ~and(B == res) then, pause; end

[B,ind] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end

[B,ind,occ] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(occ == res_occ) then, pause; end

// $$$ [B,ind,occ] = unique(A,first_ind=%t);
// $$$ if ~and(B == res) then, pause; end
// $$$ if ~and(ind == res_ind_first) then, pause; end
// $$$ if ~and(occ == res_occ) then, pause; end

// for A a matrix verify that we get outputs in column form too
A.redim[3,3]; 

B = unique(A);
if ~and(B == res) then, pause; end

[B,ind] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end

[B,ind,occ] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(occ == res_occ) then, pause; end

// $$$ [B,ind,occ] = unique(A,first_ind=%t);
// $$$ if ~and(B == res) then, pause; end
// $$$ if ~and(ind == res_ind_first) then, pause; end
// $$$ if ~and(occ == res_occ) then, pause; end


//
// with Cells
//
A = { 1, "toto", %t, 1, "toto", list("foo","bar"), 1, %t,  list("foo","bar") };
res = { 1, "toto", %t, list("foo","bar") };
res_ind_first = [1, 2, 3, 6];
res_occ = [3, 2, 2, 2];

B = unique(A);
if ~and(B == res) then, pause; end

[B,ind] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(ind == res_ind_first) then, pause; end

[B,ind,occ] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(ind == res_ind_first) then, pause; end
if ~and(occ == res_occ) then, pause; end

// transpose (to verify that we get outputs in column form)
A.redim[-1,1]; 
res.redim[-1,1];
res_ind_first.redim[-1,1];
res_occ.redim[-1,1];

B = unique(A);
if ~and(B == res) then, pause; end

[B,ind] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(ind == res_ind_first) then, pause; end

[B,ind,occ] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(ind == res_ind_first) then, pause; end
if ~and(occ == res_occ) then, pause; end


// for A a matrix verify that we get outputs in column form too
A.redim[3,3]; 

B = unique(A);
if ~and(B == res) then, pause; end

[B,ind] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(ind == res_ind_first) then, pause; end

[B,ind,occ] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(ind == res_ind_first) then, pause; end
if ~and(occ == res_occ) then, pause; end


//
// Cells with undefined entries
//
A = { 1, "toto", %t, 1, "toto" }; A{9} = 1; // A{6},A{7},A{8} undefined
res = { 1, "toto", %t };
res_ind_first = [1, 2, 3];
res_occ = [3, 2, 1];

B = unique(A);
if ~and(B == res) then, pause; end

[B,ind] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(ind == res_ind_first) then, pause; end

[B,ind,occ] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(ind == res_ind_first) then, pause; end
if ~and(occ == res_occ) then, pause; end

// transpose (to verify that we get outputs in column form)
A.redim[-1,1]; 
res.redim[-1,1];
res_ind_first.redim[-1,1];
res_occ.redim[-1,1];

B = unique(A);
if ~and(B == res) then, pause; end

[B,ind] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(ind == res_ind_first) then, pause; end

[B,ind,occ] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(ind == res_ind_first) then, pause; end
if ~and(occ == res_occ) then, pause; end


// for A a matrix verify that we get outputs in column form too
A.redim[3,3]; 

B = unique(A);
if ~and(B == res) then, pause; end

[B,ind] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(ind == res_ind_first) then, pause; end

[B,ind,occ] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A(ind)) then, pause; end
if ~and(ind == res_ind_first) then, pause; end
if ~and(occ == res_occ) then, pause; end


//
// with List
//
A = list(1, "toto", %t, 1, "toto", {"foo","bar"}, 1, %t, {"foo","bar"});
res = list(1, "toto", %t, {"foo","bar"});
res_ind_first = [1, 2, 3, 6];
res_occ = [3, 2, 2, 2];

B = unique(A);
if ~and(B == res) then, pause; end

[B,ind] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A.sublist[ind]) then, pause; end
if ~and(ind == res_ind_first) then, pause; end

[B,ind,occ] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A.sublist[ind]) then, pause; end
if ~and(ind == res_ind_first) then, pause; end
if ~and(occ == res_occ) then, pause; end


//
// Lists with undefined entries
//
A = list(1, "toto", %t, 1, "toto" ); A(9) = 1; // A(6),A(7),A(8) undefined
res = list(1, "toto", %t);
res_ind_first = [1, 2, 3];
res_occ = [3, 2, 1];

B = unique(A);
if ~and(B == res) then, pause; end

[B,ind] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A.sublist[ind]) then, pause; end
if ~and(ind == res_ind_first) then, pause; end

[B,ind,occ] = unique(A);
if ~and(B == res) then, pause; end
if ~and(B == A.sublist[ind]) then, pause; end
if ~and(ind == res_ind_first) then, pause; end
if ~and(occ == res_occ) then, pause; end

