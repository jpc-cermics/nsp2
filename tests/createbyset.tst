// -*- Mode: scilab -*- 

// creates a row vector 

x1(5)=78;
if type(x1,'short')<>'m' then pause;end 
if or(x1<>[0;0;0;0;78]') then pause;end 

// Question should we switch to matlab i.e 
// 
x2{2}=1:6;
if type(x2,'short')<>'ce' then pause;end 
if or(x2(2)<>{1:6}) then pause;end 

// create a hash table 
// 
x3.foo=56;
if type(x3,'short')<>'h' then pause;end 
