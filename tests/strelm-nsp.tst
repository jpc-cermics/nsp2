// -*- Mode: scilab -*- 
// Copyright INRIA


// deff('y=mymacro(x)','y=x+1');
//  [out,in,text]=m2s(mymacro);
//  if out<>'y'|in<>'x'|text<>[] then pause,end
//  mymacro=null();deff('y=mymacro(x)','y=x+1','n');
//  [out,in,text]=m2s(mymacro);
//  if out<>'y'|in<>'x'|text<>'y=x+1' then pause,end

//convstr
if tolower('ABC')<>'abc' then pause,end
if toupper('ABC')<>'ABC' then pause,end
if tolower(['ABC';'x'])<>['abc';'x'] then pause,end
if toupper(['ABC';'x'])<>['ABC';'X'] then pause,end
if toupper('')<>'' then pause,end

//concat 
if "poo"+"foo"<>"poofoo" then pause,end;
if ["poo","foo"]+["poo","foo"]<>["poopoo","foofoo"] then pause,end
if ["poo","foo"]+"poo"<>["poopoo","foopoo"] then pause,end
if "poo"+["poo","foo"]<>["poopoo","poofoo"] then pause,end

//catenate 
if catenate(string(1:5))<>"12345"  then pause,end
if catenate(string(1:5),sep="--")<>"1--2--3--4--5"  then pause,end
if catenate(string([1,2;3,4]),col="-",row="x")<>"1-2x3-4" then pause,end
if catenate(string([1,2;3,4]),col="-")<>["1-2";"3-4"] then pause,end
if catenate(string([1,2;3,4]),row="-")<>["1-3","2-4"] then pause,end

//part
if part('abc',1)<>'a' then pause,end
if part('abc',[1 1])<>'aa' then pause,end
if part('abc',[1;1])<>'aa' then pause,end
if part('abc',[])<>'' then pause,end
if part('abc',5)<>' ' then pause,end
if part('abc',5:6)<>'  ' then pause,end
if or(part(['abc';'x'],1)<>['a';'x']) then pause,end
if or(part(['abc';'x'],[1 1])<>['aa';'xx']) then pause,end
//if or(part(['abc';'x'],[1 2])<>['aa';'x ']) then pause,end

// length
if length('abd')<>3 then pause,end
if length('')<>0 then pause,end
if or(length(['abd';''])<>[3;0]) then pause,end
if or(length(string(ones(10,10)))<>1) then pause,end
if or(length(["poo",'f'])<>[3,1]) then pause,end

//m2s 
//string
// m2s
if m2s(1)<>'1.000000' then pause,end
if m2s(1.5)<>'1.500000' then pause,end
if m2s(1.5,"%5.2f")<>' 1.50' then pause,end
if or(m2s(1:3,"%.0f")<>['1','2','3']) then pause,end
if or(m2s([1;2;3],"%.0f")<>['1';'2';'3']) then pause,end

//isalnum
if or(isalnum('a8_')<>[%t,%t,%f]) then pause,end
if or(isalpha('a8_')<>[%t,%f,%f]) then pause,end
if or(isascii('a8_\734')<>[%t,%t,%t,%f]) then pause,end
if or(isdigit('a8_\034')<>[%f,%t,%f,%f]) then pause,end
if or(isgraph('a8_\034')<>[%t,%t,%t,%f]) then pause,end
//BUGif or(islower('aA8_\034')<>[%t,%f,%t,%t,%f]) then pause,end
//BUGif or(isupper('aA8_\034')<>[%f,%t,%t,%t,%f]) then pause,end
if or(ispunct('.,;\t\n aA')<>[%t,%t,%t,%f,%f,%f,%f,%f]) then pause,end
//BUGif or(isprint('.,;\t\n aA')<>[%t,%t,%f,%f,%f,%t,%t,%t]) then pause,end
if or(isspace('a\ta\na ')<>[%f,%t,%f,%t,%f,%t]) then pause,end
// {"isxdigit",int_smxisxdigit},

// capitalize 
if capitalize(['poo','foo'])<>['Poo','Foo'] then pause,end
if capitalize(['POO','FOO'])<>['Poo','Foo'] then pause,end

//  {"strstr",int_smxstrstr},
// regexp
// regsub
// grep 

// {"strindex",int_smxstrindex},
//strindex 
if or(strindex('abc,abd,aa,bxe',',')<>[4 8 11]) then pause,end
if or(strindex('abc',',')<>[]) then pause,end
if or(strindex('abc,abd,aa,bxe',',a')<>[4 8]) then pause,end
if or(strindex('abc,abd,aa,bxe','a')<>[1 5 9 10]) then pause,end
//if or(strindex(emptystr(),'a'))<>[] then pause,end

//strsubst
if strsubst('abc,abd,aa,bxe',',',';')<>'abc;abd;aa;bxe'  then pause,end
if strsubst('abc,abd,aa,bxe',',','')<>'abcabdaabxe'  then pause,end
if strsubst(',abc,abd,aa,bxe',',','')<>'abcabdaabxe'  then pause,end
if strsubst('abc',',',';')<>'abc'  then pause,end

// {"ascii",int_smxascii},

// {"split",int_smxsplit},

//   {"eq_s_s" ,  int_smxeq },
//   {"feq_s_s" ,  int_smxfeq },
//   {"fge_s_s" ,  int_smxfge },
//   {"fgt_s_s" ,  int_smxfgt },
//   {"fle_s_s" ,  int_smxfle },
//   {"flt_s_s" ,  int_smxflt },
//   {"fneq_s_s" ,  int_smxfneq },
//   {"ge_s_s" ,  int_smxge },
//   {"gt_s_s" ,  int_smxgt },
//   {"le_s_s" ,  int_smxle },
//   {"lt_s_s" ,  int_smxlt },
//   {"ne_s_s" ,  int_smxneq },
//   {"quote_s", int_smxtranspose},
//   {"strsubst",int_smxsubst},
//   {"stripblanks",int_smxstripblanks},


//sort
[s]=sort(['abc','abd','aa','bxe'],'g','i');
if or(s<>['aa','abc','abd','bxe']) then pause,end
[s,k]=sort(['abc','abd','aa','bxe'],'g','i');
if or(s<>['aa','abc','abd','bxe']) then pause,end
if or(k<>[3 1 2 4])  then pause,end
if sort('abc')<>'abc' then pause,end
//FIXME: remaining sorts ? see also gsort test 

// strcat
// emulated by macro 
if strcat(['abc','abd','aa','bxe'])<>'abcabdaabxe' then pause,end
if strcat(['abc','abd','aa','bxe'],',')<>'abc,abd,aa,bxe' then pause,end
if strcat('abc')<>'abc' then pause,end
if strcat('abc','sd')<>'abc' then pause,end

// //formal
// mode(-1)
// if addf('1','1')<>'2' then pause,end
// if addf('1','0')<>'1' then pause,end
// if addf('0','1')<>'1' then pause,end
// if addf('0','0')<>'0' then pause,end
// if addf('1','-1')<>'0' then pause,end
// if addf('-1','1')<>'0' then pause,end
// if addf('-1','0')<>'-1' then pause,end
// if addf('0','-1')<>'-1' then pause,end

// if addf('1','a')<>'a+1' then pause,end
// if addf('a','1')<>'a+1' then pause,end
// if addf('a','0')<>'a' then pause,end
// if addf('0','a')<>'a' then pause,end
// if addf('a','-1')<>'a-1' then pause,end
// if addf('-1','a')<>'a-1' then pause,end
// if addf('a','b')<>'a+b' then pause,end
// if addf('a+b','c')<>'a+b+c' then pause,end
// if addf('c','a+b')<>'c+a+b' then pause,end
// if addf('a+b','a+b')<>'a+b+a+b' then pause,end
// if addf('a+b','a-b')<>'a+a' then pause,end
// if addf('2*a+b','a-b')<>'2*a+a' then pause,end

// if mulf('1','1')<>'1' then pause,end
// if mulf('1','0')<>'0' then pause,end
// if mulf('0','1')<>'0' then pause,end
// if mulf('0','0')<>'0' then pause,end
// if mulf('1','-1')<>'-1' then pause,end
// if mulf('-1','1')<>'-1' then pause,end
// if mulf('-1','0')<>'0' then pause,end
// if mulf('0','-1')<>'0' then pause,end

// if mulf('1','a')<>'a' then pause,end
// if mulf('a','1')<>'a' then pause,end
// if mulf('a','0')<>'0' then pause,end
// if mulf('0','a')<>'0' then pause,end
// if mulf('a','-1')<>'-a' then pause,end
// if mulf('-1','a')<>'-a' then pause,end
// if mulf('a','b')<>'a*b' then pause,end
// if mulf('a+b','c')<>'(a+b)*c' then pause,end
// if mulf('c','a+b')<>'c*(a+b)' then pause,end
// if mulf('a+b','a+b')<>'(a+b)*(a+b)' then pause,end
// if mulf('2*a+b','a-b')<>'(2*a+b)*(a-b)' then pause,end

