// -*- Mode: scilab -*-
// Copyright (C) 2010 B. Pincob Iecn/Esial

// some tests for set functions (to be completed)


// union tests
// -----------

// union of elements
a = grand(1e3,1,"uin",-2e3,2e3);
b = grand(1e3,1,"uin",-2e3,2e3);
[c,ka,kb] = union(a,b);
if ~c.equal[sort([a(ka);b(kb)],dir="i")] then, pause, end

a = grand(1e3,1,"uin",-2e3,2e3);
b = grand(1e3,1,"uin",-2e3,2e3);
[c,ka,kb] = union(a,b,ind_type="int");
if ~c.equal[sort([a(ka);b(kb)],dir="i")] then, pause, end

a = m2i(a);
b = m2i(b);
[c,ka,kb] = union(a,b);
if ~c.equal[sort([a(ka);b(kb)],dir="i")] then, pause, end
[c,ka,kb] = union(a,b,ind_type="int");
if ~c.equal[sort([a(ka);b(kb)],dir="i")] then, pause, end



// setdiff tests
// -------------

// union of elements
A = [0.5, -1, 2, 1, 2, -1, 0.5, 2, -1, 2];
B = [2, 0.5];
CC = setdiff(A,B);
C = setdiff(A,B);
[C, ind] = setdiff(A,B);
if ~C.equal[CC] then, pause, end
if ~C.equal[A(ind)] then, pause, end
[C, ind] = setdiff(A,B, ind_type="int")
if ~C.equal[CC] then, pause, end
if ~C.equal[A(ind)] then, pause, end

A = ["toto", "foo", "bar", "toto", "foobar", "bar", "toto", "foo", "bar"];
B = [ "foo", "bar" ];
C = setdiff(A,B);
[C, ind] = setdiff(A,B);
if ~C.equal[A(ind)] then, pause, end



// intersection tests
// ------------------

// intersection of elements

// doubles
a = grand(1e3,1,"uin",-2e3,2e3);
b = grand(1e3,1,"uin",-2e3,2e3);

cc = intersect(a,b);
[c,ka,kb] = intersect(a,b);
if ~c.equal[cc] then, pause, end
if ~c.equal[a(ka)] then, pause, end
if ~c.equal[b(kb)] then, pause, end
[c,ka,kb] = intersect(a,b,ind_type="int");
if ~c.equal[a(ka)] then, pause, end
if ~c.equal[b(kb)] then, pause, end

// doubles with %inf, %nan  
// as all Nan are different from any other Nan
// the intersection should not have %nan
a = [a;%nan;%inf];
b = [b;-%inf;%nan];  
[d,ka,kb] = intersect(a,b);
if ~d.equal[c] then, pause, end
if ~c.equal[a(ka)] then, pause, end
if ~c.equal[b(kb)] then, pause, end
[d,ka,kb] = intersect(a,b,ind_type="int");
if ~d.equal[c] then, pause, end
if ~c.equal[a(ka)] then, pause, end
if ~c.equal[b(kb)] then, pause, end

// integers
// int32
a = m2i(grand(1e3,1,"uin",-2e3,2e3));
b = m2i(grand(1e3,1,"uin",-2e3,2e3));
cc = intersect(a,b);
[c,ka,kb] = intersect(a,b);
if ~c.equal[cc] then, pause, end
if ~c.equal[a(ka)] then, pause, end
if ~c.equal[b(kb)] then, pause, end
[c,ka,kb] = intersect(a,b,ind_type="int");
if ~c.equal[a(ka)] then, pause, end
if ~c.equal[b(kb)] then, pause, end


// intersection of rows
a = grand(1e3,4,"uin",-2,2);
b = grand(1e3,4,"uin",-2,2);
cc = intersect(a,b,which="rows");
[c,ka,kb] = intersect(a,b,which="rows");
if ~c.equal[cc] then, pause, end
if ~c.equal[a(ka,:)] then, pause, end
if ~c.equal[b(kb,:)] then, pause, end

[c,ka,kb] = intersect(a,b,which="rows",ind_type="int");
if ~c.equal[cc] then, pause, end
if ~c.equal[a(ka,:)] then, pause, end
if ~c.equal[b(kb,:)] then, pause, end

// intersection of integer rows
for int_type = ["int8","uint8","int16","uint16","int32","uint32","int64","uint64"]
   a = m2i(grand(1e3,4,"uin",-2,2),int_type);
   b = m2i(grand(1e3,4,"uin",-2,2),int_type);
   cc = intersect(a,b,which="rows");
   [c,ka,kb] = intersect(a,b,which="rows");
   if ~c.equal[cc] then, pause, end
   if ~c.equal[a(ka,:)] then, pause, end
   if ~c.equal[b(kb,:)] then, pause, end

   [c,ka,kb] = intersect(a,b,which="rows",ind_type="int");
   if ~c.equal[cc] then, pause, end
   if ~c.equal[a(ka,:)] then, pause, end
   if ~c.equal[b(kb,:)] then, pause, end
end

// intersection of columns
a = grand(4,1e3,"uin",-2,2);
b = grand(4,1e3,"uin",-2,2);
cc = intersect(a,b,which="columns");
[c,ka,kb] = intersect(a,b,which="columns");
if ~c.equal[cc] then, pause, end
if ~c.equal[a(:,ka)] then, pause, end
if ~c.equal[b(:,kb)] then, pause, end

[c,ka,kb] = intersect(a,b,which="columns",ind_type="int");
if ~c.equal[cc] then, pause, end
if ~c.equal[a(:,ka)] then, pause, end
if ~c.equal[b(:,kb)] then, pause, end

// intersection of integer columns
for int_type = ["int8","uint8","int16","uint16","int32","uint32","int64","uint64"]
   a = m2i(grand(4,1e3,"uin",-2,2),int_type);
   b = m2i(grand(4,1e3,"uin",-2,2),int_type);
   cc = intersect(a,b,which="columns");
   [c,ka,kb] = intersect(a,b,which="columns");
   if ~c.equal[cc] then, pause, end
   if ~c.equal[a(:,ka)] then, pause, end
   if ~c.equal[b(:,kb)] then, pause, end

   [c,ka,kb] = intersect(a,b,which="columns",ind_type="int");
   if ~c.equal[cc] then, pause, end
   if ~c.equal[a(:,ka)] then, pause, end
   if ~c.equal[b(:,kb)] then, pause, end
end

// intersection for string, list and arrays of cells

// strings
a = ["foo", "bar", "nsp", "tumbi", "jpc", "bp"];
b = ["foobar", "nsp", "toto", "tumbi", "jpc", "troquet", "bp"];
rc = ["bp", "jpc", "nsp", "tumbi"];
rka = [6, 5, 3, 4];
rkb = [7, 5, 2, 4];
[c,ka,kb] = intersect(a,b);
if ~c.equal[rc] then, pause, end
if ~ka.equal[rka] then, pause, end
if ~kb.equal[rkb] then, pause, end

a = list("foo", "bar", "nsp", "tumbi", 1, "jpc", "bp", 5, %t);
b = list("foobar", "nsp", "toto", "tumbi", 5, %t, "jpc", "troquet", "bp",96i);
rc = list("nsp", "tumbi", "jpc", "bp", 5, %t);
rka = [3, 4, 6, 7, 8, 9];
rkb = [2, 4, 7, 9, 5, 6];
[c,ka,kb] = intersect(a,b);
if ~c.equal[rc] then, pause, end
if ~ka.equal[rka] then, pause, end
if ~kb.equal[rkb] then, pause, end


a = {"foo", "bar", "nsp", "tumbi", 1, "jpc", "bp", 5, %t};
b = {"foobar", "nsp", "toto", "tumbi", 5, %t, "jpc", "troquet", "bp",96i};
rc = {"nsp", "tumbi", "jpc", "bp", 5, %t};
rka = [3, 4, 6, 7, 8, 9];
rkb = [2, 4, 7, 9, 5, 6];
[c,ka,kb] = intersect(a,b);
if ~c.equal[rc] then, pause, end
if ~ka.equal[rka] then, pause, end
if ~kb.equal[rkb] then, pause, end



// setxor tests
// ------------

// doubles
a = grand(1e3,1,"uin",-2e3,2e3);
b = grand(1e3,1,"uin",-2e3,2e3);

cc = setxor(a,b);
[c,ka,kb] =  setxor(a,b);
if ~c.equal[cc] then, pause, end
if ~c.equal[union(a(ka),b(kb))] then, pause, end
[c,ka,kb] = setxor(a,b,ind_type="int");
if ~c.equal[cc] then, pause, end
if ~c.equal[union(a(ka),b(kb))] then, pause, end


// doubles with %inf, %nan  
// as all Nan are different from any other Nan
a = [1;3;%nan;%inf];
b = [1;3;-%inf;%nan];  

cc = setxor(a,b);
// cc should be equal to {%nan, %inf, -%inf, %nan}
if numel(cc)~=4 then, pause, end
if or(isfinite(cc)) then, pause, end 
if sum(isnan(cc)) ~= 2 then, pause, end
if sum(isinf(cc))~= 2 then, pause, end
   
// integers
// int32
a = m2i(grand(1e3,1,"uin",-2e3,2e3));
b = m2i(grand(1e3,1,"uin",-2e3,2e3));
cc = setxor(a,b);
[c,ka,kb] =  setxor(a,b);
if ~c.equal[cc] then, pause, end
if ~c.equal[union(a(ka),b(kb))] then, pause, end
[c,ka,kb] = setxor(a,b,ind_type="int");
if ~c.equal[cc] then, pause, end
if ~c.equal[union(a(ka),b(kb))] then, pause, end


// setxor of rows
a = grand(1e3,4,"uin",-2,2);
b = grand(1e3,4,"uin",-2,2);

cc = setxor(a,b,which="rows");
[c,ka,kb] =  setxor(a,b,which="rows");
if ~c.equal[cc] then, pause, end
if ~c.equal[union(a(ka,:),b(kb,:),which="rows")] then, pause, end
[c,ka,kb] = setxor(a,b,ind_type="int",which="rows");
if ~c.equal[cc] then, pause, end
if ~c.equal[union(a(ka,:),b(kb,:),which="rows")] then, pause, end

// setxor of integer rows
for int_type = ["int8","uint8","int16","uint16","int32","uint32","int64","uint64"]
   a = m2i(grand(1e3,4,"uin",-2,2),int_type);
   b = m2i(grand(1e3,4,"uin",-2,2),int_type);
   cc = setxor(a,b,which="rows");
   [c,ka,kb] =  setxor(a,b,which="rows");
   if ~c.equal[cc] then, pause, end
   if ~c.equal[union(a(ka,:),b(kb,:),which="rows")] then, pause, end
   [c,ka,kb] = setxor(a,b,ind_type="int",which="rows");
   if ~c.equal[cc] then, pause, end
   if ~c.equal[union(a(ka,:),b(kb,:),which="rows")] then, pause, end
end

// setxor of columns
a = grand(4,1e3,"uin",-2,2);
b = grand(4,1e3,"uin",-2,2);
cc = setxor(a,b,which="columns");
[c,ka,kb] = setxor(a,b,which="columns");
if ~c.equal[cc] then, pause, end
if ~c.equal[union(a(:,ka),b(:,kb),which="col")] then, pause, end

[c,ka,kb] = setxor(a,b,which="columns",ind_type="int");
if ~c.equal[cc] then, pause, end
if ~c.equal[union(a(:,ka),b(:,kb),which="col")] then, pause, end


// setxor of integer columns
for int_type = ["int8","uint8","int16","uint16","int32","uint32","int64","uint64"]
   a = m2i(grand(4,1e3,"uin",-2,2),int_type);
   b = m2i(grand(4,1e3,"uin",-2,2),int_type);
   cc = setxor(a,b,which="columns");
   [c,ka,kb] = setxor(a,b,which="columns");
   if ~c.equal[cc] then, pause, end
   if ~c.equal[union(a(:,ka),b(:,kb),which="col")] then, pause, end

   [c,ka,kb] =  setxor(a,b,which="columns",ind_type="int");
   if ~c.equal[cc] then, pause, end
   if ~c.equal[union(a(:,ka),b(:,kb),which="col")] then, pause, end
end

// setxor for string

// strings
a = ["foo", "bar", "nsp", "tumbi", "jpc", "bp"];
b = ["foobar", "nsp", "toto", "tumbi", "jpc", "troquet", "bp"];
rc = ["bar", "foo", "foobar", "toto", "troquet"];
rka = [2, 1];
rkb = [1, 3, 6];
[c,ka,kb] = setxor(a,b);
if ~c.equal[rc] then, pause, end
if ~ka.equal[rka] then, pause, end
if ~kb.equal[rkb] then, pause, end
