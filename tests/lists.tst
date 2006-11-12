// -*- Mode: scilab -*- 

L=list(1,2,3)
L(1) = 10
L(3:5) = (4,5,6)
// Corriger le XXX de Eval
//L(:) = (....)
// plus complique 
[x,L(1:2),z]=(1,67,68,2) 
// tlist : are implemented as hash tables 
//         the type is stored as .type
L=tlist(['a','b','c','d'],20,3,4);
L.b = 78 
L('c') = 10 
// L(['c','d'])=( 10,20) 

// equal 

L=list(7,8,9);
if or(L==L<>[%t,%t,%t]) then pause;end 
if or(L==list(5,8,0)<>[%f,%t,%f]) then pause;end 
if ~L.equal[L] then pause;end
if L.equal[8] then pause;end 
if L.equal[list(5,8,0)] then pause;end 
if or(list(L,8,L)==list(L,8,L))<>[%t,%t,%t] then pause;end 

//  { "sublist", int_list_sublist },

L=list(1,2,3,4,5);
L1=L.sublist[1:4];
if or(L1<>list(1,2,3,4)) then pause;end 

//  { "add_first", int_list_add_first },

L1=L;
L1.add_first["first"];
if or(L1<>list("first",1,2,3,4,5)) then pause;end 

//  { "add_last", int_list_add_last },

L1=L;
L1.add_last["last"];
if or(L1<>list(1,2,3,4,5,"last")) then pause;end 

//  { "add", int_list_add },

L1=L;
L1.add["first",1];
if or(L1<>list("first",1,2,3,4,5)) then pause;end 

L1=L;
L1.add["xx",3];
if or(L1<>list(1,2,"xx",3,4,5)) then pause;end 
L1.add["end",length(L1)+1];
if or(L1<>list(1,2,"xx",3,4,5,"end")) then pause;end 

//  { "first", int_list_first },

if L.first[]<>1 then pause;end 

//  { "last", int_list_last },

if L.last[]<>5 then pause;end 

//  { "item", int_list_item },

if L.item[3]<>3 then pause;end 

//  { "remove_first", int_list_remove_first },

L1=L;
L1.remove_first[];
if or(L1<>list(2,3,4,5)) then pause;end 

//  { "remove_last", int_list_remove_last },

L1=L;
L1.remove_last[];
if or(L1<>list(1,2,3,4)) then pause;end 

//  { "remove", int_list_remove },

L1=L;
L1.remove[2];
if or(L1<>list(1,3,4,5)) then pause;end 

//  { "compact", int_lxcompact },

L1=L;
L1.compact[];
if or(L1<>list(1:5)) then pause;end 
L1=L;
L1.compact['c'];
if or(L1<>list(1:5)) then pause;end 
L1=L;
L1.compact['r'];
if or(L1<>list((1:5)')) then pause;end 


A=[1,2;3,4];B=A>=2;C=string(A);
L1=list(A,A,A,B,B,B,C,C,C);
L1.compact[];
if or(L1<>list([A,A,A],[B,B,B],[C,C,C])) then pause;end 
L1=list(A,A,A,B,B,B,C,C,C);
L1.compact['c'];
if or(L1<>list([A,A,A],[B,B,B],[C,C,C])) then pause;end 
L1=list(A,A,A,B,B,B,C,C,C);
L1.compact['r'];
if or(L1<>list([A;A;A],[B;B;B],[C;C;C])) then pause;end 

//  { "concat", int_lxconcat },

L1=L;
L1.concat[list(6,7,8)];
if or(L1<>list(1,2,3,4,5,6,7,8)) then pause;end 

//  { "reverse", int_lxreverse },

L1=L;
L1.reverse[];
if or(L1<>list(5,4,3,2,1)) then pause;end 

// length 

if length(list(1,8,9))<>3 then pause;end 

// cat 

L=list_concat(list(2,7),list(9,0),list(5,6));
if or(L<>list(2,7,9,0,5,6)) then pause;end 


// static int int_lxunique(Stack stack, int rhs, int opt, int lhs)

A=[1,2;3,4];B=A>=2;C=string(A);
L1=list(A,A,A,B,B,B,C,C,C);
L1=unique(L1);
if or(L1<>list(A,B,C)) then pause;end 


// static int int_lxmap(Stack stack, int rhs, int opt, int lhs)

function y=f(x);y=2*x;endfunction ;
L=list(1,2,3);
L1=map(L,f);
if or(L1<>list(2,4,6)) then pause;end 

// static int int_lxfoldr(Stack stack, int rhs, int opt, int lhs)

function z=f(x,y);z=x+y;endfunction ;
L=list(1,2,3,4);
L1=foldr(L,0,f);
if L1<>10 then pause;end 

function z=f(x,y);z=list(x,y);endfunction ;
L=list(1,2,3,4);
L1=foldr(L,0,f);
if L1<>list(1,list(2,list(3,list(4,0)))) then pause;end 

// static int int_lxfoldl

function z=f(x,y);z=x+y;endfunction ;
L=list(1,2,3,4);
v=foldl(L,f,0);
if v<>10 then pause;end 

function z=f(x,y);z=[x,y];endfunction ;
L=list(1,2,3,4);
L1=foldl(L,f,0);
if ~L1.equal[0:4] then pause;end 

// L(:) 

L=list(1,5,9);
[a,b,c]=L(:);
if c<>9 then pause;end 



// extract/insert/remove with path 

L1=list(1,list(2,list(3,4)));
if L1(list(2,2,2))<>4 then pause;end 
L1(list(2,2,2))=67;
if L1<>list(1,list(2,list(3,67))) then pause;end 
L1(list(2,2,2))=null();
if L1<>list(1,list(2,list(3))) then pause;end 

//   {"list",int_lxlist},
//   {"tlist",int_lx_tlist_as_hash},
//   {"mlist",int_lx_mlist_as_hash},
//   {"null",int_lxnull},
//   {"resize2vect_l",int_lxextractall},
//   {"extract_l",int_lxextract},
//   {"extractelts_l",int_lxextract}, 
//   {"setrowscols_l",int_lxsetrc},
//   {"sorted_list", int_lxsortedlist},
//   {"sorted_list_search", int_lxsortedsearch},
//   {"sorted_list_search_and_remove", int_lxsortedsearchandremove},




