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
// perm=[];for i=1:size(L,'*'), perm = [perm;L{i}];end;
  
  L= tarjan(M);
  perm=[];for i=1:size(L,'*'), perm = [perm,L{i}];end;
endfunction

