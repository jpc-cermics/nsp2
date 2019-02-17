// -*- Mode: nsp -*- 

function ret=check_msg(code,check)
  ret = %t;
  ok=execstr(code,errcatch=%t);
  if ok then ret=%f;return;end 
  S=lasterror();
  if strstr(S(1),check)==0 then ret=%f;end 
endfunction

// test of out of range detection for Shape 
// -----------------------------------------

code = ['cdfgam(""PQ"",3,0,7)'
	'cdfgam(""PQ"",[3,7],[5 0],[7 8])'
	'cdfgam(""PQ"",[3, 7;3,7],[5, 9;5,0],[4,6;7,8])'];

check = ['Shape is out of range';
	 'Shape(2) is out of range';
	 'Shape(2,2) is out of range'];

for i=1:size(code,'*');
  if ~check_msg(code(i),check(i)) then pause;end 
end

// test of out of range detection for Rate 
// -----------------------------------------
code = ['cdfgam(""PQ"",3,4,0)';
	'cdfgam(""PQ"",[3, 1e300],[4, 1e300],[0, 1])';
	'cdfgam(""PQ"",[3, 7;3,7],[5, 9;5,7],[4,-4;7,8])'];


check = ['Rate is out of range';
	 'Rate(1) is out of range';
	 'Rate(1,2) is out of range'];

for i=1:size(code,'*');
  if ~check_msg(code(i),check(i)) then pause;end 
end

// cumgam fails (when x near a and very large (see gratio.c)
// -----------------------------------------
code = ['cdfgam(""PQ"",1e300,1e300,1)';
	'cdfgam(""PQ"",[3, 1e300],[4, 1e300],[5, 1])';
	'cdfgam(""PQ"",[3, 1e300;3, 3],[4, 1e300;4, 4],[5, 1;5, 1])'];

check=['result for given parameter';
       'result(2) for given parameter';
       'result(1,2) for given parameter'];

for i=1:size(code,'*');
  if ~check_msg(code(i),check(i)) then pause;end 
end

// P out of range
// -----------------------------------------
code = ['cdfgam(""X"",4,7,-3,0.5)';
	'cdfgam(""X"",[4,4],[7,7],[0.5,3],[0.5,0.5])';
	'cdfgam(""X"",[4,4;4,4],[7,7;7,7],[0.5,-3;0.5,0.5],[0.5,0.5;0.5,0.5])'];

check= ['Error: input parameter P is out of range, bound exceeded: 0';
	'Error: input parameter P(2) is out of range, bound exceeded: 1';
	'Error: input parameter P(1,2) is out of range, bound exceeded: 0'];

for i=1:size(code,'*');
  if ~check_msg(code(i),check(i)) then pause;end 
end

// Q out of range
// -----------------------------------------
code = ['cdfgam(""X"",4,7,0.5,-3)';
	'cdfgam(""X"",[4,4],[7,7],[0.5,0.5],[0.5,-3])';
	'cdfgam(""X"",[4,4;4,4],[7,7;7,7],[0.5,0.5;0.5,0.5],[0.5,3;0.5,0.5])'];

check=['Error: input parameter Q is out of range, bound exceeded: 0'
       'Error: input parameter Q(2) is out of range, bound exceeded: 0'
       'Error: input parameter Q(1,2) is out of range, bound exceeded: 1'];

for i=1:size(code,'*');
  if ~check_msg(code(i),check(i)) then pause;end 
end

// P + Q ~= 1
// -----------------------------------------
code = ['cdfgam(""X"",4,7,0.6,0.5)';
	'cdfgam(""X"",[4,4],[7,7],[0.5,0.6],[0.5,0.5])';
	'cdfgam(""X"",[4,4;4,4],[7,7;7,7],[0.5,0.8;0.5,0.5],[0.5,0.5;0.5,0.5])'];

check=['Error: P + Q ~= 1 ';
'Error: P(2) + Q(2) ~= 1 ';
'Error: P(1,2) + Q(1,2) ~= 1'];
for i=1:size(code,'*');
  if ~check_msg(code(i),check(i)) then pause;end 
end

// recherche du paramètre Xlam bloquée par la contrainte 1e-300
// (qu'on pourrait changer)
// -----------------------------------------
[P,Q] = cdfpoi("PQ",0,1e-301);
code = ['cdfpoi(""Xlam"",P,Q,0)';
	'cdfpoi(""Xlam"",[P,0.5],[Q,0.5],[0,0])';
	'cdfpoi(""Xlam"",[0.5,0.5;P,0.5],[0.5,0.5;Q,0.5],[4,5;0,0])'];

check=['Error: answer appears to be lower than lowest search bound 1e-300';
       'Error: answer(1) appears to be lower than lowest search bound 1e-300';
       'Error: answer(2,1) appears to be lower than lowest search bound 1e-300'];

for i=1:size(code,'*');
  if ~check_msg(code(i),check(i)) then pause;end 
end

// Xn not integer
// -----------------------------------------
code = ['cdfbin(""PQ"", 8, 9.7, 0.7, 0.3)';
	'cdfbin(""PQ"", [8,8], [89,9.7], [0.7,0.7], [0.3,0.3])';
	'cdfbin(""PQ"", [8,8;8,8], [89,9.7;89,9.7], [0.7,0.7;0.7,0.7],"+...
	" [0.3,0.3;0.3,0.3])'];

check=['Error: Xn (nb of Bernoulli trials) is not a positive integer';
 'Error: Xn(2) (nb of Bernoulli trials) is not a positive integer';
 'Error: Xn(1,2) (nb of Bernoulli trials) is not a positive integer'];

for i=1:size(code,'*');
  if ~check_msg(code(i),check(i)) then pause;end 
end
