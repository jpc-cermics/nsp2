// -*- Mode: scilab -*- 
// fonction temporaires 
// --------------------

function [z]=plus_sp_m(a,b) 
z=full(a)+b 
endfunction 

function [z]=plus_m_sp(a,b) 
z=a+full(b)
endfunction 

function [z]=minus_sp_m(a,b) 
z=full(a)-b 
endfunction 

function [z]=minus_m_sp(a,b) 
z=a-full(b)
endfunction 

function [z]=concatr_sp_m(a,b) 
z=[a,sparse(b)]
endfunction 

function [z]=concatd_sp_m(a,b) 
z=[a;sparse(b)]
endfunction 

function [z]=mult_m_sp(a,b)
// a*b : eventuellement traiter les cas particulier 
z=sparse(a)* b 
endfunction

function [z]=mult_sp_m(a,b)
// a*b : eventuellement traiter les cas particulier 
z=a * sparse(b)
endfunction

function [z]=dst_m_sp(a,b)
// a*b : eventuellement traiter les cas particulier 
z=sparse(a)* b 
endfunction

function [z]=dst_sp_m(a,b)
// a*b : eventuellement traiter les cas particulier 
z=a * sparse(b)
endfunction

// Copyright INRIA/ENPC 

ij=[1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6];
v=[1;2;3;4;5;6;7;8;9];
ij1=[1 6;1 2;6 5];
v1=[-1;-1;-1];
vc=v+%i*(20+v);
v1c=v1+%i*[0;0.3;-1.2];
zer=sparse([],[],[6,6]);

//-------------------------------------------------------------
// basic calls to sparse 
//-------------------------------------------------------------

a=sparse(ij,v,[6 6]);
b=sparse(ij1,v1,[6 6]);
ma=sparse(ij,-v,[6 6]);
mb=sparse(ij1,-v1,[6 6]);
ac=sparse(ij,vc,[6 6]);
bc=sparse(ij1,v1c,[6 6]);
mac=sparse(ij,-v,[6 6]);
mbc=sparse(ij1,-v1c,[6 6]);

//-------------------------------------------------------------
//test of sparse(ij,v [,mn]) 
//--------------------------------------------------------------

function [a]=sparse1(ij,v,m,n) 
 a=0*ones(m*n,1);
 // Il faut tester a<>[] : pas fait encore 
 if ij<>[] then a(ij(:,1)+m*(ij(:,2)-1))=v(:);end 
 a=matrix(a,m,n);
endfunction

function [T]=sptest1(ij,v,mn)
 T=or(sparse1(ij,v,mn(1,1),mn(1,2))<>full(sparse(ij,v,mn)));
endfunction

function [T]=sptest2(ij,v)
 T=or(sparse1(ij,v,maxi(ij(:,1)),maxi(ij(:,2)))<>full(sparse(ij,v)))
endfunction

//-- for real matrix

if sptest2([1 1;1 3],[1 5])   then BUG,end 
if sptest2([1 1;1 300],[1 5]) then BUG,end
if sptest2([1 1;3 1],[1 5])   then BUG,end
if sptest2([1 1;300 1],[1 5]) then BUG,end
if sptest2(ij,v) 	      then BUG,end
if sptest1(ij,v,[6,6])	      then BUG,end
if sptest1(ij,v,[8,6])	      then BUG,end
if sptest1([],[],[4,10])      then BUG,end
if sptest1([2 1;3 1;4 1;6 1],[10;11;12;13],[6,1]) then BUG,end
if sptest1([1 2;1 3;1 4;1 6],[10;11;12;13],[1,6]) then BUG,end

// -- for complex matrix
if sptest1(ij,vc,[6,6]) 	then BUG,end
if sptest1(ij,vc,[8,6]) 	then BUG,end
if sptest1(ij,vc,[6,8]) 	then BUG,end
if sptest1([2 1;3 1;4 1;6 1],[10-3*%i;11;12+5*%i;13+0.5*%i],[6,1]); then BUG,end
if sptest1([1 2;1 3;1 4;1 6],[10-3*%i;11;12+5*%i;13+0.5*%i],[1,6]); then BUG,end

//-------------------------------------------------------------
//test of sparse(a) 
//--------------------------------------------------------------

if or(full(sparse(0.3))<>0.3) then BUG,end

n=50
for i=1:10;
	a=int(10*rand(n,n,'uniform'));a(a> 7) =0;
	spa=sparse(a);
	if or(full(spa)<>a) then BUG,end
end 

for i=1:5;
	a=int(10*rand(n,n,'uniform'));a(a> 7) =0;
	// building ij,v 
	v_t=find(a<>0)'-1;
	//	v1_t=modulo(v_t,n);v2_t=((v_t-v1_t)/n)+1;v1_t=v1_t+1;
	v1_t=modulo(v_t,n)+1;
	v2_t=idiv(v_t,n)+1;
	ij_t=[v1_t,v2_t];vals=a(v_t+1);
	// using sparse(ij,vals) 
	spa1=sparse(ij_t,vals,[n,n]);
	// testing 
	if or([full(spa1)<>a]) then BUG,end
end 

//-------------------------------------------------------------
//test of spget
//--------------------------------------------------------------

sp=sparse(ij,v,[6,6]);
[ij_1,v_1]=spget(sp);[ij_2,k_2]=gsort(ij,'lr','i');
if or([ij_1<>ij_2,v_1<>v(k_2)]) then BUG,end

zer=sparse([],[],[6,6]);[ij_t,v_t]=spget(zer);
if ij_t<>[]|v_t<>[] then BUG,end

sp=sparse(ij,vc,[8,6]);
[ij_1,v_1,mn1]=spget(sp);[ij_2,k_2]=gsort(ij,'lr','i');
if or([ij_1<>ij_2,v_1<>vc(k_2)]) then BUG,end
if mn1<>[8,6] then BUG,end

n=50
for i=1:5;
	a=int(10*rand(n,n,'uniform'));a(a> 7) =0;
	spa=sparse(a);
	// building ij,v 
	v_t=find(a<>0)'-1;
	// v1_t=modulo(v_t,n);v2_t=((v_t-v1_t)/n)+1;v1_t=v1_t+1;
	v1_t=modulo(v_t,n)+1;
	v2_t=idiv(v_t,n)+1;
	ij_t=[v1_t,v2_t];vals_t=a(v_t+1);
	[ij_t,k]=gsort(ij_t,'lr','i');
	vals_t=vals_t(k);
	// using spget 
	[ij_1,vals_1]=spget(spa);
	// testing 
	if or([ij_1<>ij_t,vals_1<>vals_t]) then BUG,end
end 

//-----------------------------------------------
// addition and substraction tests
//-----------------------------------------------
// -- real real
a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
b=sparse([1 6;1 2;6 5],[-1;-1;-1],[6 6]);
v=sparse([2 1;3 1;4 1;6 1],[10;11;12;13],[6,1]);
vt=sparse([1 2;1 3;1 4;1 6],[10;11;12;13],[1,6]);
ma=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],-(1:9),[6 6]);
mb=sparse([1 6;1 2;6 5],-[-1;-1;-1],[6 6]);
zer=sparse([],[],[6,6]);
ac=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],(1:9)+%i*(21:29),[6 6]);
bc=sparse([1 6;1 2;6 5],[-1;-1;-1]+%i*[0;0.3;-1.2],[6 6]);
vc=sparse([2 1;3 1;4 1;6 1],[10-3*%i;11;12+5*%i;13+0.5*%i],[6,1]);
vct=sparse([1 2;1 3;1 4;1 6],[10-3*%i;11;12+5*%i;13+0.5*%i],[1,6]);
mac=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],-(1:9)-%i*(21:29),[6 6]);
mbc=sparse([1 6;1 2;6 5],[1;1;1]+%i*[0;-0.3;1.2],[6 6]);

if or(full(v+sparse([],[],[6,1]))<>full(v)) then BUG,end
if or(full(sparse([],[],[6,1])+v)<>full(v)) then BUG,end
if or(full(v+v)<>full(v)+full(v)) then BUG,end
if or(full(v-v)<>full(v)-full(v)) then BUG,end
if or(full(vt+sparse([],[],[1,6]))<>full(vt)) then BUG,end
if or(full(vt+vt)<>full(vt)+full(vt)) then BUG,end
if or(full(vt-vt)<>full(vt)-full(vt)) then BUG,end
if or(full(zer+zer)<>0*ones(6,6)) then BUG,end
if or(full(a+a)<>full(a)+full(a)) then BUG,end
if or(full(b+b)<>full(b)+full(b)) then BUG,end
if or(full(a+zer)<>full(a)) then BUG,end
if or(full(zer+a)<>full(a)) then BUG,end
if or(full(b+a)<>full(b)+full(a)) then BUG,end
if or(full(a+b)<>full(b)+full(a)) then BUG,end
if or(full(a+ma)<>full(a)+full(ma)) then BUG,end
if or(full(a-a)<>full(a)-full(a)) then BUG,end
if or(full(a-ma)<>full(a)-full(ma)) then BUG,end
if or(full(b-mb)<>full(b)-full(mb)) then BUG,end
if or(full(a-zer)<>full(a)) then BUG,end
if or(full(zer-a)<>-full(a)) then BUG,end
if or(full(a-mb)<>full(a)-full(mb)) then BUG,end
//  -- real complex
if or(full(sparse([],[],[6,1])+vc)<>full(vc)) then BUG,end
if or(full(v+vc)<>full(v)+full(vc)) then BUG,end
if or(full(v-vc)<>full(v)-full(vc)) then BUG,end
if or(full(vt+vct)<>full(vt)+full(vct)) then BUG,end
if or(full(vt-vct)<>full(vt)-full(vct)) then BUG,end
if or(full(a+ac)<>full(a)+full(ac)) then BUG,end
if or(full(b+bc)<>full(b)+full(bc)) then BUG,end
if or(full(a+bc)<>full(a)+full(bc)) then BUG,end
if or(full(zer+ac)<>full(zer)+full(ac)) then BUG,end
if or(full(b+ac)<>full(b)+full(ac)) then BUG,end
if or(full(a-ac)<>full(a)-full(ac)) then BUG,end
if or(full(b-bc)<>full(b)-full(bc)) then BUG,end
if or(full(a-bc)<>full(a)-full(bc)) then BUG,end
if or(full(zer-ac)<>full(zer)-full(ac)) then BUG,end
if or(full(b-ac)<>full(b)-full(ac)) then BUG,end
// -- complex real
if or(full(vc+v)<>full(vc)+full(v)) then BUG,end
if or(full(vc-v)<>full(vc)-full(v)) then BUG,end
if or(full(vct+vt)<>full(vct)+full(vt)) then BUG,end
if or(full(vct-vt)<>full(vct)-full(vt)) then BUG,end
if or(full(ac+a)<>full(ac)+full(a)) then BUG,end
if or(full(bc+b)<>full(bc)+full(b)) then BUG,end
if or(full(ac+b)<>full(ac)+full(b)) then BUG,end
if or(full(ac+zer)<>full(zer)+full(ac)) then BUG,end
if or(full(bc+a)<>full(bc)+full(a)) then BUG,end
if or(full(ac-a)<>full(ac)-full(a)) then BUG,end
if or(full(bc-b)<>full(bc)-full(b)) then BUG,end
if or(full(ac-b)<>full(ac)-full(b)) then BUG,end
if or(full(ac-zer)<>-full(zer)+full(ac)) then BUG,end
if or(full(bc-a)<>full(bc)-full(a)) then BUG,end
// -- complex complex
if or(full(vc+vc)<>full(vc)+full(vc)) then BUG,end
if or(full(vc-vc)<>full(vc)-full(vc)) then BUG,end
if or(full(vct+vct)<>full(vct)+full(vct)) then BUG,end
if or(full(vct-vct)<>full(vct)-full(vct)) then BUG,end
if or(full(ac+ac)<>full(ac)+full(ac)) then BUG,end
if or(full(bc+bc)<>full(bc)+full(bc)) then BUG,end
if or(full(ac+bc)<>full(ac)+full(bc)) then BUG,end
if or(full(bc+ac)<>full(bc)+full(ac)) then BUG,end
if or(full(ac-ac)<>full(zer)) then BUG,end
if or(full(bc-bc)<>full(zer)) then BUG,end
if or(full(ac-bc)<>full(ac)-full(bc)) then BUG,end
if or(full(bc-ac)<>full(bc)-full(ac)) then BUG,end
// sparse full and full sparse operation (soft coded apoerations)
if or(full(a+1)<>full(a)+1) then BUG,end
if or(full(1+a)<>full(a)+1) then BUG,end
if or(full(a+ones(6,6))<>full(a)+ones(6,6)) then BUG,end
if or(full(ones(6,6)+a)<>full(a)+ones(6,6)) then BUG,end
//AFAIREif or(full(a+2*eye)<>full(a)+2*eye) then BUG,end
//AFAIREif or(full(2*eye+a)<>full(a)+2*eye) then BUG,end
if or(full(a-1)<>full(a)-1) then BUG,end
if or(full(1-a)<>1-full(a)) then BUG,end
if or(full(a-ones(6,6))<>full(a)-ones(6,6)) then BUG,end
if or(full(ones(6,6)-a)<>ones(6,6)-full(a)) then BUG,end
//AFAIREif or(full(a-2*eye)<>full(a)-2*eye) then BUG,end
//AFAIREif or(full(2*eye-a)<>2*eye-full(a)) then BUG,end
if or(full(ac+1)<>full(ac)+1) then BUG,end
if or(full(1+ac)<>full(ac)+1) then BUG,end
if or(full(ac+ones(6,6))<>full(ac)+ones(6,6)) then BUG,end
if or(full(ones(6,6)+ac)<>full(ac)+ones(6,6)) then BUG,end
//AFAIREif or(full(ac+2*eye)<>full(ac)+2*eye) then BUG,end
//AFAIREif or(full(2*eye+ac)<>full(ac)+2*eye) then BUG,end
if or(full(ac-1)<>full(ac)-1) then BUG,end
if or(full(1-ac)<>1-full(ac)) then BUG,end
if or(full(ac-ones(6,6))<>full(ac)-ones(6,6)) then BUG,end
if or(full(ones(6,6)-ac)<>ones(6,6)-full(ac)) then BUG,end
//AFAIREif or(full(ac-2*eye)<>full(ac)-2*eye) then BUG,end
//AFAIREif or(full(2*eye-ac)<>2*eye-full(ac)) then BUG,end
if or(full(ac+full(bc))<>full(ac)+full(bc)) then BUG,end
if or(full(ac-full(bc))<>full(ac)-full(bc)) then BUG,end
if or(full(full(ac)+full(bc))<>full(ac)+full(bc)) then BUG,end

// MANQUE LES OPERATIONS sparse op sparse quand l'un des deux 
// sparse est scalaire AFAIRE 

//end

//if %t then
//-----------------------------------------------
// multiplication tests
//-----------------------------------------------
// real x real
// sparse scalar , scalar sparse
//AFAIREif or(full(a*2)<>full(a)*2) then BUG,end
//AFAIREif or(full(2*a)<>full(a)*2) then BUG,end
//AFAIREif  a*[]<>[] then BUG,end
//AFAIREif  []*a<>[] then BUG,end
//AFAIREc=rand(6,6);
//AFAIREif norm(a*c-full(a)*c) >100*%eps then BUG,end
//AFAIREif norm(c*a-c*full(a)) >100*%eps then BUG,end
// sparse sparse
if or(full(zer*zer)<>full(zer)) then BUG,end
if or(full(a*zer)<>full(zer)) then BUG,end
if or(full(zer*a)<>full(zer)) then BUG,end
//AFAIREif  norm(full(a*a)-full(a)*full(a),1)>100*%eps then BUG,end
//AFAIREif  norm(full(b*b)-full(b)*full(b),1)>100*%eps then BUG,end
//AFAIREif  norm(full(a*b)-full(a)*full(b),1)>100*%eps then BUG,end
//AFAIREif  norm(full(b*a)-full(b)*full(a),1)>100*%eps then BUG,end
// complex x real real x complex
// sparse scalar , scalar sparse
//AFAIREif or(full(ac*2)<>full(ac)*2) then BUG,end
//AFAIREif or(full(2*ac)<>full(ac)*2) then BUG,end
//AFAIREif norm(full((2+%i)*a)-(2+%i)*full(a),1) >100*%eps then BUG,end
//AFAIREif norm(full(a*(2+%i))-(2+%i)*full(a),1) >100*%eps then BUG,end
//AFAIREif  ac*[]<>[] then BUG,end
//AFAIREif  []*ac<>[] then BUG,end
//AFAIREc=rand(6,6);
//AFAIREcc=c+rand(6,6)*%i;
//AFAIREif norm(ac*c-full(ac)*c) >100*%eps then BUG,end
//AFAIREif norm(cc*a-cc*full(a)) >100*%eps then BUG,end
// sparse sparse
if or(full(ac*zer)<>full(zer)) then BUG,end
if or(full(zer*ac)<>full(zer)) then BUG,end
//AFAIREif  norm(full(ac*a)-full(ac)*full(a),1)>100*%eps then BUG,end
//AFAIREif  norm(full(bc*b)-full(bc)*full(b),1)>100*%eps then BUG,end
//AFAIREif  norm(full(a*bc)-full(a)*full(bc),1)>100*%eps then BUG,end
//AFAIREif  norm(full(b*ac)-full(b)*full(ac),1)>100*%eps then BUG,end
// // complex x complex
//AFAIREif norm(ac*cc-full(ac)*cc) >100*%eps then BUG,end
//AFAIREif norm(cc*ac-cc*full(ac)) >100*%eps then BUG,end
// sparse sparse
//AFAIREif  norm(full(ac*ac)-full(ac)*full(ac),1)>100*%eps then BUG,end
//AFAIREif  norm(full(bc*bc)-full(bc)*full(bc),1)>100*%eps then BUG,end
//AFAIREif  norm(full(bc*ac)-full(bc)*full(ac),1)>100*%eps then BUG,end
//----------------------------------------------------------
// element wise multiplication tests
//----------------------------------------------------------
//AFAIREif or(full(ac.*2)<>full(ac)*2) then BUG,end
//AFAIREif or(full((2).*ac)<>full(ac)*2) then BUG,end
//AFAIREif  a.*[]<>[] then BUG,end
//AFAIREif  [].*a<>[] then BUG,end
//AFAIREc=rand(6,6);
//if norm(a*c-full(a)*c) >100*%eps then BUG,end
//if norm(c*a-c*full(a)) >100*%eps then BUG,end
// sparse sparse
//AFAIREif or(full(zer.*zer)<>full(zer)) then BUG,end
//AFAIREif or(full(a.*zer)<>full(zer)) then BUG,end
//AFAIREif or(full(zer.*a)<>full(zer)) then BUG,end
//AFAIREif  norm(full(a.*a)-full(a).*full(a),1)>100*%eps then BUG,end
//AFAIREif  norm(full(b.*b)-full(b).*full(b),1)>100*%eps then BUG,end
//AFAIREif  norm(full(a.*b)-full(a).*full(b),1)>100*%eps then BUG,end
//AFAIREif  norm(full(b.*a)-full(b).*full(a),1)>100*%eps then BUG,end
// complex x real real x complex
// sparse scalar , scalar sparse
//AFAIREif norm(full((2+%i).*a)-(2+%i).*full(a),1) >100*%eps then BUG,end
//AFAIREif norm(full(a.*(2+%i))-(2+%i).*full(a),1) >100*%eps then BUG,end
//AFAIREif  ac.*[]<>[] then BUG,end
//AFAIREif  [].*ac<>[] then BUG,end
//AFAIREc=rand(6,6);
//AFAIREcc=c+rand(6,6)*%i;
//AFAIREif norm(full(ac.*c)-full(ac).*c) >100*%eps then BUG,end
//AFAIREif norm(full(cc.*a)-cc.*full(a)) >100*%eps then BUG,end
// sparse sparse
//AFAIREif or(full(ac.*zer)<>full(zer)) then BUG,end
//AFAIREif or(full(zer.*ac)<>full(zer)) then BUG,end
//AFAIREif  norm(full(ac.*a)-full(ac).*full(a),1)>100*%eps then BUG,end
//AFAIREif  norm(full(bc.*b)-full(bc).*full(b),1)>100*%eps then BUG,end
//AFAIREif  norm(full(a.*bc)-full(a).*full(bc),1)>100*%eps then BUG,end
//AFAIREif  norm(full(b.*ac)-full(b).*full(ac),1)>100*%eps then BUG,end
// // complex x complex
//AFAIREif norm(ac.*cc-full(ac).*cc) >100*%eps then BUG,end
//AFAIREif norm(cc.*ac-cc.*full(ac)) >100*%eps then BUG,end
// sparse sparse
//AFAIREif  norm(full(ac.*ac)-full(ac).*full(ac),1)>100*%eps then BUG,end
//AFAIREif  norm(full(bc.*bc)-full(bc).*full(bc),1)>100*%eps then BUG,end
//AFAIREif  norm(full(bc.*ac)-full(bc).*full(ac),1)>100*%eps then BUG,end
// ----------------------------------------------------------
// test de la transposition
//-----------------------------------------------------------
if or(full(a')<>full(a)') then BUG,end
if or(full(ac')<>full(ac)') then BUG,end
if or(full(zer')<>full(zer)' ) then BUG,end
v=sparse([2 1;3 1;4 1;6 1],[10;11;12;13],[6,1]);
if or(full(v')<>full(v)') then BUG,end
if or(full((v')')<>full(v)) then BUG,end
vc=sparse([2 1;3 1;4 1;6 1],[10-3*%i;11;12+5*%i;13+0.5*%i],[6,1]);
if or(full(vc')<>full(vc)') then BUG,end
if or(full((vc')')<>full(vc)) then BUG,end
// ----------------------------------------------------------
// test des concatenation
//-----------------------------------------------------------
if or(full([a])<>full(a)) then BUG,end
if or(full([a b])<>[full(a) full(b)]) then BUG,end
if or(full([a;b])<>[full(a);full(b)]) then BUG,end
if or(full([a,[]])<>full(a)) then BUG,end
if or(full([a;[]])<>full(a)) then BUG,end
if or(full([a zer])<>[full(a) full(zer)]) then BUG,end
if or(full([zer;b])<>[full(zer);full(b)]) then BUG,end
if or(full([ac])<>full(ac)) then BUG,end
if or(full([ac b])<>[full(ac) full(b)]) then BUG,end
if or(full([ac;b])<>[full(ac);full(b)]) then BUG,end
if or(full([ac,[]])<>full(ac)) then BUG,end
if or(full([ac;[]])<>full(ac)) then BUG,end
if or(full([a bc])<>[full(a) full(bc)]) then BUG,end
if or(full([a;bc])<>[full(a);full(bc)]) then BUG,end
if or(full([ac bc])<>[full(ac) full(bc)]) then BUG,end
if or(full([ac;bc])<>[full(ac);full(bc)]) then BUG,end
// ----------------------------------------------------------
// test des extractions
//-----------------------------------------------------------
af=full(a);
if or(full(a(1,3))<>af(1,3)) then BUG,end
if or(full(a(1,4))<>af(1,4)) then BUG,end
if or(full(a(1,:))<>af(1,:)) then BUG,end
v=sparse([2 1;3 1;4 1;6 1],[10;11;12;13],[6,1]);vf=full(v);
//AFAIREif or(full(v(:))<>vf(:)) then BUG,end
if or(full(v(3:4))<>vf(3:4)) then BUG,end
if or(full(v([1 5]))<>vf([1 5])) then BUG,end
if or(full(v([4 3]))<>vf([4 3])) then BUG,end
if or(full(v([4 4]))<>vf([4 4])) then BUG,end
if or(full(v([1 1]))<>vf([1 1])) then BUG,end
v=v';vf=vf';
//AFAIREif or(full(v(:))<>vf(:)) then BUG,end
if or(full(v(3:4))<>vf(3:4)) then BUG,end
if or(full(v([1 5]))<>vf([1 5])) then BUG,end
if or(full(v([4 3]))<>vf([4 3])) then BUG,end
if or(full(v([4 4]))<>vf([4 4])) then BUG,end
if or(full(v([1 1]))<>vf([1 1])) then BUG,end
acff=full(ac);
if or(full(ac(1,3))<>acff(1,3)) then BUG,end
if or(full(ac(1,4))<>acff(1,4)) then BUG,end
if or(full(ac(1,:))<>acff(1,:)) then BUG,end
vc=sparse([2 1;3 1;4 1;6 1],[10-3*%i;11;12+5*%i;13+0.5*%i],[6,1]);vcf=full(vc);
//AFAIREif or(full(vc(:))<>vcf(:)) then BUG,end
if or(full(vc(3:4))<>vcf(3:4)) then BUG,end
if or(full(vc([1 5]))<>vcf([1 5])) then BUG,end
if or(full(vc([4 3]))<>vcf([4 3])) then BUG,end
if or(full(vc([4 4]))<>vcf([4 4])) then BUG,end
if or(full(vc([1 1]))<>vcf([1 1])) then BUG,end
vc=vc';vcf=vcf';
//AFAIREif or(full(vc(:))<>vcf(:)) then BUG,end
if or(full(vc(3:4))<>vcf(3:4)) then BUG,end
if or(full(vc([1 5]))<>vcf([1 5])) then BUG,end
if or(full(vc([4 3]))<>vcf([4 3])) then BUG,end
if or(full(vc([4 4]))<>vcf([4 4])) then BUG,end
if or(full(vc([1 1]))<>vcf([1 1])) then BUG,end
// ----------------------------------------------------------
// test des insertions
//-----------------------------------------------------------

a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
vt=sparse([1 2;1 3;1 4;1 6],[10;11;12;13],[1,6]);
// full line insertion A(I,:) = B  Aand B sparse 
//--------------------------------
a1=a;a1(1,:)=vt;A=full(a);Vt=full(vt);A(1,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(3,:)=vt;A=full(a);Vt=full(vt);A(3,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(1,:)=vt;A=full(a);Vt=full(vt);A(1,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(3,:)=vt;A=full(a);Vt=full(vt);A(3,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(7,:)=vt;A=full(a);Vt=full(vt);A(7,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(8,:)=vt;A=full(a);Vt=full(vt);A(8,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1([1 3],:)=[vt;2*vt];A=full(a);Vt=full(vt);A([1 3],:)=[Vt;2*Vt];
if or(full(a1)<>A) then BUG,end
a1=a;a1([3 1],:)=[vt;2*vt];A=full(a);Vt=full(vt);A([3 1],:)=[Vt;2*Vt];
if or(full(a1)<>A) then BUG,end
a1=a;a1([1 3 1],:)=[vt;2*vt;3*vt];A=full(a);Vt=full(vt);A([1 3 1],:)=[Vt;2*Vt;3*Vt];
if or(full(a1)<>A) then BUG,end
//  insert zero vector
vt=sparse([],[],[1,6]);
a1=a;a1(1,:)=vt;A=full(a);Vt=full(vt);A(1,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(3,:)=vt;A=full(a);Vt=full(vt);A(3,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(1,:)=vt;A=full(a);Vt=full(vt);A(1,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(3,:)=vt;A=full(a);Vt=full(vt);A(3,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(7,:)=vt;A=full(a);Vt=full(vt);A(7,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(8,:)=vt;A=full(a);Vt=full(vt);A(8,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1([1 3],:)=[vt;2*vt];A=full(a);Vt=full(vt);A([1 3],:)=[Vt;2*Vt];
if or(full(a1)<>A) then BUG,end
a1=a;a1([3 1],:)=[vt;2*vt];A=full(a);Vt=full(vt);A([3 1],:)=[Vt;2*Vt];
if or(full(a1)<>A) then BUG,end
a1=a;a1([1 3 1],:)=[vt;2*vt;3*vt];A=full(a);Vt=full(vt);A([1 3 1],:)=[Vt;2*Vt;3*Vt];
if or(full(a1)<>A) then BUG,end
a=sparse([],[],[6,6]);
a1=a;a1(1,:)=vt;A=full(a);Vt=full(vt);A(1,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(3,:)=vt;A=full(a);Vt=full(vt);A(3,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(1,:)=vt;A=full(a);Vt=full(vt);A(1,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(3,:)=vt;A=full(a);Vt=full(vt);A(3,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(7,:)=vt;A=full(a);Vt=full(vt);A(7,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(8,:)=vt;A=full(a);Vt=full(vt);A(8,:)=Vt;
if or(full(a1)<>A) then BUG,end
b=sparse([1 1;1 3;1 6;2 1;2 2;2 4],10:15,[2,6]);
a1=a;a1([1 3],:)=b;A=full(a);B=full(b);A([1 3],:)=B;
if or(full(a1)<>A) then BUG,end
a1=a;a1([3 1],:)=b;A=full(a);B=full(b);A([3 1],:)=B;
if or(full(a1)<>A) then BUG,end
b=sparse([1 1;1 3;1 6;2 1;2 2;2 4;3 3;3 5],10:17,[3,6]);
a1=a;a1([1 3 1],:)=b;A=full(a);B=full(b);A([1 3 1],:)=B;
if or(full(a1)<>A) then BUG,end
// insert zero vector
vt=sparse([],[],[1,6]);
a1=a;a1(1,:)=vt;A=full(a);Vt=full(vt);A(1,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(3,:)=vt;A=full(a);Vt=full(vt);A(3,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(1,:)=vt;A=full(a);Vt=full(vt);A(1,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(3,:)=vt;A=full(a);Vt=full(vt);A(3,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(7,:)=vt;A=full(a);Vt=full(vt);A(7,:)=Vt;
if or(full(a1)<>A) then BUG,end
a1=a;a1(8,:)=vt;A=full(a);Vt=full(vt);A(8,:)=Vt;
if or(full(a1)<>A) then BUG,end
b=sparse([1 1;1 3;1 6;2 1;2 2;2 4],10:15,[2,6]);
a1=a;a1([1 3],:)=b;A=full(a);B=full(b);A([1 3],:)=B;
if or(full(a1)<>A) then BUG,end
a1=a;a1([3 1],:)=b;A=full(a);B=full(b);A([3 1],:)=B;
if or(full(a1)<>A) then BUG,end
b=sparse([1 1;1 3;1 6;2 1;2 2;2 4;3 3;3 5],10:17,[3,6]);
a1=a;a1([1 3 1],:)=b;A=full(a);B=full(b);A([1 3 1],:)=B;
if or(full(a1)<>A) then BUG,end
// full column insertion A(:,I)=B Aand B sparse 
//----------------------
a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
v=sparse([2 1;3 1;4 1;6 1],[10;11;12;13],[6,1]);
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,3)=v;A=full(a);V=full(v);A(:,3)=V;
if or(full(a1)<>A) then BUG,end
//
a1=a;a1(:,7)=v;A=full(a);V=full(v);A(:,7)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,8)=v;A=full(a);V=full(v);A(:,8)=V;
if or(full(a1)<>A) then BUG,end
b=sparse([1 2;2 1;3 1;3 2;4 1;6 2],10:15,[6,2]);
a1=a;a1(:,[1 3])=b;A=full(a);B=full(b);A(:,[1 3])=B;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,[3 1])=b;A=full(a);B=full(b);A(:,[3 1])=B;
if or(full(a1)<>A) then BUG,end
b=sparse([1 2;2 1;2 3;3 1;3 2;4 1;5 3;6 2],10:17,[6,3]);
a1=a;a1(:,[1 3 1])=b;A=full(a);B=full(b);A(:,[1 3 1])=B;
if or(full(a1)<>A) then BUG,end
v=sparse([],[],[6,1]);
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,3)=v;A=full(a);V=full(v);A(:,3)=V;
if or(full(a1)<>A) then BUG,end
//
a1=a;a1(:,7)=v;A=full(a);V=full(v);A(:,7)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,8)=v;A=full(a);V=full(v);A(:,8)=V;
if or(full(a1)<>A) then BUG,end
b=sparse([],[],[6,2]);
a1=a;a1(:,[1 3])=b;A=full(a);B=full(b);A(:,[1 3])=B;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,[3 1])=b;A=full(a);B=full(b);A(:,[3 1])=B;
if or(full(a1)<>A) then BUG,end
b=sparse([],[],[6,3]);
a1=a;a1(:,[1 3 1])=b;A=full(a);B=full(b);A(:,[1 3 1])=B;
if or(full(a1)<>A) then BUG,end
a=sparse([],[],[6,6]);
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,3)=v;A=full(a);V=full(v);A(:,3)=V;
if or(full(a1)<>A) then BUG,end
//
a1=a;a1(:,7)=v;A=full(a);V=full(v);A(:,7)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,8)=v;A=full(a);V=full(v);A(:,8)=V;
if or(full(a1)<>A) then BUG,end
b=sparse([1 2;2 1;3 1;3 2;4 1;6 2],10:15,[6,2]);
a1=a;a1(:,[1 3])=b;A=full(a);B=full(b);A(:,[1 3])=B;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,[3 1])=b;A=full(a);B=full(b);A(:,[3 1])=B;
if or(full(a1)<>A) then BUG,end
b=sparse([1 2;2 1;2 3;3 1;3 2;4 1;5 3;6 2],10:17,[6,3]);
a1=a;a1(:,[1 3 1])=b;A=full(a);B=full(b);A(:,[1 3 1])=B;
if or(full(a1)<>A) then BUG,end
v=sparse([],[],[6,1]);
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,3)=v;A=full(a);V=full(v);A(:,3)=V;
if or(full(a1)<>A) then BUG,end
//
a1=a;a1(:,7)=v;A=full(a);V=full(v);A(:,7)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,8)=v;A=full(a);V=full(v);A(:,8)=V;
if or(full(a1)<>A) then BUG,end
b=sparse([1 2;2 1;3 1;3 2;4 1;6 2],10:15,[6,2]);
a1=a;a1(:,[1 3])=b;A=full(a);B=full(b);A(:,[1 3])=B;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,[3 1])=b;A=full(a);B=full(b);A(:,[3 1])=B;
if or(full(a1)<>A) then BUG,end
b=sparse([1 2;2 1;2 3;3 1;3 2;4 1;5 3;6 2],10:17,[6,3]);
a1=a;a1(:,[1 3 1])=b;A=full(a);B=full(b);A(:,[1 3 1])=B;
if or(full(a1)<>A) then BUG,end
v=sparse([],[],[6,1]);
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,3)=v;A=full(a);V=full(v);A(:,3)=V;
if or(full(a1)<>A) then BUG,end
//
a1=a;a1(:,7)=v;A=full(a);V=full(v);A(:,7)=V;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,8)=v;A=full(a);V=full(v);A(:,8)=V;
if or(full(a1)<>A) then BUG,end
b=sparse([],[],[6,2]);
a1=a;a1(:,[1 3])=b;A=full(a);B=full(b);A(:,[1 3])=B;
if or(full(a1)<>A) then BUG,end
a1=a;a1(:,[3 1])=b;A=full(a);B=full(b);A(:,[3 1])=B;
if or(full(a1)<>A) then BUG,end
b=sparse([],[],[6,3]);
a1=a;a1(:,[1 3 1])=b;A=full(a);B=full(b);A(:,[1 3 1])=B;
if or(full(a1)<>A) then BUG,end
// row column insertion
a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
b=sparse([1 2;1 3;3 3],-(1:3),[3,3]);
a1=a;a1(1,1)=sparse(30);A=full(a);A(1,1)=30;
if or(full(a1)<>A) then BUG,end
a1=a;a1(1,6)=sparse(30);A=full(a);A(1,6)=30;
if or(full(a1)<>A) then BUG,end
a1=a;a1(1,8)=sparse(30);A=full(a);A(1,8)=30;
if or(full(a1)<>A) then BUG,end
a1=a;a1(1:3,1:3)=b;A=full(a);A(1:3,1:3)=full(b);
if or(full(a1)<>A) then BUG,end
a1=a;a1(1:3,6:8)=b;A=full(a);A(1:3,6:8)=full(b);
if or(full(a1)<>A) then BUG,end
a1=a;a1(6:8,1:3)=b;A=full(a);A(6:8,1:3)=full(b);
if or(full(a1)<>A) then BUG,end
a1=a;a1([3 2 1],1:3)=b;A=full(a);A([3 2 1],1:3)=full(b);
if or(full(a1)<>A) then BUG,end
a1=a;a1([1 2 1],1:3)=b;A=full(a);A([1 2 1],1:3)=full(b);
if or(full(a1)<>A) then BUG,end
a1=a;a1([3 2 1],[3 2 1])=b;A=full(a);A([3 2 1],[3 2 1])=full(b);
if or(full(a1)<>A) then BUG,end
a1=a;a1([1 2 1],[3 2 1])=b;A=full(a);A([1 2 1],[3 2 1])=full(b);
if or(full(a1)<>A) then BUG,end
a1=a;a1([1 2 1],[1 2 1])=b;A=full(a);A([1 2 1],[1 2 1])=full(b);
if or(full(a1)<>A) then BUG,end
//sparse full
a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
vt=11:16;
// full line insertion A(I,:) = B  A sparse B full 
//-----------------------------------------------
//AFAIRE a1=a;a1(1,:)=vt;A=full(a);A(1,:)=vt;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(3,:)=vt;A=full(a);A(3,:)=vt;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(1,:)=vt;A=full(a);A(1,:)=vt;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(3,:)=vt;A=full(a);A(3,:)=vt;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(7,:)=vt;A=full(a);A(7,:)=vt;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(8,:)=vt;A=full(a);A(8,:)=vt;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1([1 3],:)=[vt;2*vt];A=full(a);A([1 3],:)=[vt;2*vt];
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1([3 1],:)=[vt;2*vt];A=full(a);A([3 1],:)=[vt;2*vt];
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1([1 3 1],:)=[vt;2*vt;3*vt];A=full(a);A([1 3 1],:)=[vt;2*vt;3*vt];
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a=sparse([],[],[6,6]);
//AFAIRE a1=a;a1(1,:)=vt;A=full(a);A(1,:)=vt;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(3,:)=vt;A=full(a);A(3,:)=vt;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(1,:)=vt;A=full(a);A(1,:)=vt;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(3,:)=vt;A=full(a);A(3,:)=vt;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(7,:)=vt;A=full(a);A(7,:)=vt;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(8,:)=vt;A=full(a);A(8,:)=vt;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE b=[1:6;11:16];
//AFAIRE a1=a;a1([1 3],:)=b;A=full(a);A([1 3],:)=b;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1([3 1],:)=b;A=full(a);A([3 1],:)=b;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE b=[1:6;11:16;21:26];
//AFAIRE a1=a;a1([1 3 1],:)=b;A=full(a);A([1 3 1],:)=b;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE // full column insertion
//AFAIRE //----------------------
//AFAIRE a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
//AFAIRE v=(1:6)';
//AFAIRE a1=a;a1(:,1)=v;A=full(a);A(:,1)=v;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(:,2)=v;A=full(a);A(:,2)=v;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(:,1)=v;A=full(a);A(:,1)=v;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(:,2)=v;A=full(a);A(:,2)=v;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(:,3)=v;A=full(a);A(:,3)=v;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE //
//AFAIRE a1=a;a1(:,7)=v;A=full(a);A(:,7)=v;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(:,8)=v;A=full(a);A(:,8)=v;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE b=[(1:6)' (11:16)'];
//AFAIRE a1=a;a1(:,[1 3])=b;A=full(a);A(:,[1 3])=b;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(:,[3 1])=b;A=full(a);A(:,[3 1])=b;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE b=[(1:6)' (11:16)' (21:26)'];
//AFAIRE a1=a;a1(:,[1 3 1])=b;A=full(a);A(:,[1 3 1])=b;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE // row column insertion
//AFAIRE a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
//AFAIRE b=-[1 2 3;4 5 6;7 8 9];
//AFAIRE a1=a;a1(1,1)=30;A=full(a);A(1,1)=30;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(1,6)=30;A=full(a);A(1,6)=30;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(1,8)=30;A=full(a);A(1,8)=30;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(1:3,1:3)=b;A=full(a);A(1:3,1:3)=b;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(1:3,6:8)=b;A=full(a);A(1:3,6:8)=b;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1(6:8,1:3)=b;A=full(a);A(6:8,1:3)=b;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1([3 2 1],1:3)=b;A=full(a);A([3 2 1],1:3)=b;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1([1 2 1],1:3)=b;A=full(a);A([1 2 1],1:3)=b;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1([3 2 1],[3 2 1])=b;A=full(a);A([3 2 1],[3 2 1])=b;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1([1 2 1],[3 2 1])=b;A=full(a);A([1 2 1],[3 2 1])=b;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE a1=a;a1([1 2 1],[1 2 1])=b;A=full(a);A([1 2 1],[1 2 1])=b;
//AFAIRE if or(full(a1)<>A) then BUG,end
//AFAIRE // vector insertion
//AFAIRE v=sparse([1 1;3 1;6 1],[10 11 12],[6 1]);
//AFAIRE v1=v;v1(1)=33;V=full(v);V(1)=33;
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1(2)=33;V=full(v);V(2)=33;
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1(8)=33;V=full(v);V(8)=33;
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1([1 2 8])=[5;10;33];V=full(v);V([1 2 8])=[5;10;33];
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1(:)=[];
//AFAIRE if or(full(v1)<>[]) then BUG,end
//AFAIRE v1=v;v1(1)=sparse(33);V=full(v);V(1)=33;
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1(2)=sparse(33);V=full(v);V(2)=33;
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1(8)=sparse(33);V=full(v);V(8)=33;
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1([1 2 8])=sparse([5;10;33]);V=full(v);V([1 2 8])=[5;10;33];
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1([1 2 1])=sparse([5;10;33]);V=full(v);V([1 2 1])=[5;10;33];
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1(:)=[];
//AFAIRE if or(full(v1)<>[]) then BUG,end
//AFAIRE v1=v;v1(:)=sparse([2 1],44,[6 1]);V=full(v);V(:)=[0;44;0;0;0;0];
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v=v';
//AFAIRE v1=v;v1(1)=33;V=full(v);V(1)=33;
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1(2)=33;V=full(v);V(2)=33;
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1(8)=33;V=full(v);V(8)=33;
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1([1 2 8])=[5 10 33];V=full(v);V([1 2 8])=[5 10 33];
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1(1)=sparse(33);V=full(v);V(1)=33;
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1(2)=sparse(33);V=full(v);V(2)=33;
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1(8)=sparse(33);V=full(v);V(8)=33;
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1([1 2 8])=sparse([5 10 33]);V=full(v);V([1 2 8])=[5 10 33];
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1([1 2 1])=sparse([5 10 33]);V=full(v);V([1 2 1])=[5 10 33];
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1(:)=sparse([1 2],44,[1,6]);V=full(v);V(:)=[0 44 0 0 0 0];
//AFAIRE if or(full(v1)<>V) then BUG,end
//AFAIRE v1=v;v1(1)=[];V=full(v);V(1)=[];
//AFAIRE if or(full(v1)<>V) then BUG,end



//---Boolean sparse Matrices---
//test des comparaisons
a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
b=sparse([1 6;1 2;6 5],[10;-1;-1],[6 6]);
if full(a==a)<>full(a)==full(a) then BUG,end
if full(a<>a)<>(full(a)<>full(a)) then BUG,end
if full(a>sparse(5))<>(full(a)>5) then BUG,end
if full(sparse(5)>a)<>(5>full(a)) then BUG,end
if full(b>a)<>(full(b)>full(a))  then BUG,end
if full(a==full(a))<>full(a)==full(a) then BUG,end
if full(a<>full(a))<>(full(a)<>full(a)) then BUG,end
if full(a>5)<>(full(a)>5) then BUG,end
if full(5>a)<>(5>full(a)) then BUG,end
if full(b>full(a))<>(full(b)>full(a))  then BUG,end
if full(full(a)==a)<>full(a)==full(a) then BUG,end
if full(full(a)<>a)<>(full(a)<>full(a)) then BUG,end
if full(full(a)>sparse(5))<>(full(a)>5) then BUG,end
if full(full(b)>a)<>(full(b)>full(a))  then BUG,end
a=sparse([1 1;3 1;6 1],[10 11 12],[6 1]);
if full(a==a)<>full(a)==full(a) then BUG,end
if full(a<>a)<>(full(a)<>full(a)) then BUG,end
if full(a>sparse(5))<>(full(a)>5) then BUG,end
if full(sparse(5)>a)<>(5>full(a)) then BUG,end
if full(a==full(a))<>full(a)==full(a) then BUG,end
if full(a<>full(a))<>(full(a)<>full(a)) then BUG,end
if full(a>5)<>(full(a)>5) then BUG,end
if full(5>a)<>(5>full(a)) then BUG,end
if full(full(a)==a)<>full(a)==full(a) then BUG,end
if full(full(a)<>a)<>(full(a)<>full(a)) then BUG,end
if full(full(a)>sparse(5))<>(full(a)>5) then BUG,end
a=a';
if full(a==a)<>full(a)==full(a) then BUG,end
if full(a<>a)<>(full(a)<>full(a)) then BUG,end
if full(a>sparse(5))<>(full(a)>5) then BUG,end
if full(sparse(5)>a)<>(5>full(a)) then BUG,end
if full(a==full(a))<>full(a)==full(a) then BUG,end
if full(a<>full(a))<>(full(a)<>full(a)) then BUG,end
if full(a>5)<>(full(a)>5) then BUG,end
if full(5>a)<>(5>full(a)) then BUG,end
if full(full(a)==a)<>full(a)==full(a) then BUG,end
if full(full(a)<>a)<>(full(a)<>full(a)) then BUG,end
if full(full(a)>sparse(5))<>(full(a)>5) then BUG,end
a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
b=sparse([1 6;1 2;5 5],[10;-1;-1],[5 6]);
if a==b<>%f then BUG,end
if a<>b<>%t then BUG,end
