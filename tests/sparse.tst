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
//
if %t then
//-------------------------------------------------------------
//test of sparse(ij,v [,mn]) 
//--------------------------------------------------------------
deff('[a]=sparse1(ij,v,m,n)',['a=0*ones(m*n,1);';
		'if ij<>[] then a(ij(:,1)+m*(ij(:,2)-1))=v(:);end';
		'a=matrix(a,m,n);']);
deff('[T]=sptest1(ij,v,mn)','T=or(sparse1(ij,v,mn(1),mn(2))<>full(sparse(ij,v,mn)))')
deff('[T]=sptest2(ij,v)','T=or(sparse1(ij,v,max(ij(:,1)),max(ij(:,2)))<>full(sparse(ij,v)))')

//  -- for real matrix

if sptest2([1 1;1 3],[1 5])   then pause,end 
if sptest2([1 1;1 300],[1 5]) then pause,end
if sptest2([1 1;3 1],[1 5])   then pause,end
if sptest2([1 1;300 1],[1 5]) then pause,end
if sptest2(ij,v) 	      then pause,end
if sptest1(ij,v,[6,6])	      then pause,end
if sptest1(ij,v,[8,6])	      then pause,end
if sptest1([],[],[4,10])      then pause,end
if sptest1([2 1;3 1;4 1;6 1],[10;11;12;13],[6,1]) then pause,end
if sptest1([1 2;1 3;1 4;1 6],[10;11;12;13],[1,6]) then pause,end

//  -- for complex matrix
if sptest1(ij,vc,[6,6]) 	then pause,end
if sptest1(ij,vc,[8,6]) 	then pause,end
if sptest1(ij,vc,[6,8]) 	then pause,end
if sptest1([2 1;3 1;4 1;6 1],[10-3*%i;11;12+5*%i;13+0.5*%i],[6,1]); then pause,end
if sptest1([1 2;1 3;1 4;1 6],[10-3*%i;11;12+5*%i;13+0.5*%i],[1,6]); then pause,end

//-------------------------------------------------------------
//test of sparse(a) 
//--------------------------------------------------------------

if or(full(sparse(0.3))<>0.3) then pause,end

n=50
for i=1:10;
	a=int(10*rand(n,n,'uniform'));a(a> 7) =0;
	spa=sparse(a);
	if or(full(spa)<>a) then pause,end
end 

for i=1:5;
	a=int(10*rand(n,n,'uniform'));a(a> 7) =0;
	// building ij,v 
	v_t=find(a<>0)'-1;
	v1_t=modulo(v_t,n);v2_t=((v_t-v1_t)/n)+1;v1_t=v1_t+1;
	ij_t=[v1_t,v2_t];vals=a(v_t+1);
	// using sparse(ij,vals) 
	spa1=sparse(ij_t,vals,[n,n]);
	// testing 
	if or([full(spa1)<>a]) then pause,end
end 

//-------------------------------------------------------------
//test of spget
//--------------------------------------------------------------

sp=sparse(ij,v,[6,6]);
[ij_1,v_1]=spget(sp);[ij_2,k_2]=gsort(ij,'lr','i');
if or([ij_1<>ij_2,v_1<>v(k_2)]) then pause,end

zer=sparse([],[],[6,6]);[ij_t,v_t]=spget(zer);
if ij_t<>[]|v_t<>[] then pause,end

sp=sparse(ij,vc,[8,6]);
[ij_1,v_1,mn1]=spget(sp);[ij_2,k_2]=gsort(ij,'lr','i');
if or([ij_1<>ij_2,v_1<>vc(k_2)]) then pause,end
if mn1<>[8,6] then pause,end

n=50
for i=1:5;
	a=int(10*rand(n,n,'uniform'));a(a> 7) =0;
	spa=sparse(a);
	// building ij,v 
	v_t=find(a<>0)'-1;
	v1_t=modulo(v_t,n);v2_t=((v_t-v1_t)/n)+1;v1_t=v1_t+1;
	ij_t=[v1_t,v2_t];vals_t=a(v_t+1);
	[ij_t,k]=gsort(ij_t,'lr','i');
	vals_t=vals_t(k);
	// using spget 
	[ij_1,vals_1]=spget(spa);
	// testing 
	if or([ij_1<>ij_t,vals_1<>vals_t]) then pause,end
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

if or(full(v+sparse([],[],[6,1]))<>full(v)) then pause,end
if or(full(sparse([],[],[6,1])+v)<>full(v)) then pause,end
if or(full(v+v)<>full(v)+full(v)) then pause,end
if or(full(v-v)<>full(v)-full(v)) then pause,end
if or(full(vt+sparse([],[],[1,6]))<>full(vt)) then pause,end
if or(full(vt+vt)<>full(vt)+full(vt)) then pause,end
if or(full(vt-vt)<>full(vt)-full(vt)) then pause,end
if or(full(zer+zer)<>0*ones(6,6)) then pause,end
if or(full(a+a)<>full(a)+full(a)) then pause,end
if or(full(b+b)<>full(b)+full(b)) then pause,end
if or(full(a+zer)<>full(a)) then pause,end
if or(full(zer+a)<>full(a)) then pause,end
if or(full(b+a)<>full(b)+full(a)) then pause,end
if or(full(a+b)<>full(b)+full(a)) then pause,end
if or(full(a+ma)<>full(a)+full(ma)) then pause,end
if or(full(a-a)<>full(a)-full(a)) then pause,end
if or(full(a-ma)<>full(a)-full(ma)) then pause,end
if or(full(b-mb)<>full(b)-full(mb)) then pause,end
if or(full(a-zer)<>full(a)) then pause,end
if or(full(zer-a)<>-full(a)) then pause,end
if or(full(a-mb)<>full(a)-full(mb)) then pause,end
//  -- real complex
if or(full(sparse([],[],[6,1])+vc)<>full(vc)) then pause,end
if or(full(v+vc)<>full(v)+full(vc)) then pause,end
if or(full(v-vc)<>full(v)-full(vc)) then pause,end
if or(full(vt+vct)<>full(vt)+full(vct)) then pause,end
if or(full(vt-vct)<>full(vt)-full(vct)) then pause,end
if or(full(a+ac)<>full(a)+full(ac)) then pause,end
if or(full(b+bc)<>full(b)+full(bc)) then pause,end
if or(full(a+bc)<>full(a)+full(bc)) then pause,end
if or(full(zer+ac)<>full(zer)+full(ac)) then pause,end
if or(full(b+ac)<>full(b)+full(ac)) then pause,end
if or(full(a-ac)<>full(a)-full(ac)) then pause,end
if or(full(b-bc)<>full(b)-full(bc)) then pause,end
if or(full(a-bc)<>full(a)-full(bc)) then pause,end
if or(full(zer-ac)<>full(zer)-full(ac)) then pause,end
if or(full(b-ac)<>full(b)-full(ac)) then pause,end
// -- complex real
if or(full(vc+v)<>full(vc)+full(v)) then pause,end
if or(full(vc-v)<>full(vc)-full(v)) then pause,end
if or(full(vct+vt)<>full(vct)+full(vt)) then pause,end
if or(full(vct-vt)<>full(vct)-full(vt)) then pause,end
if or(full(ac+a)<>full(ac)+full(a)) then pause,end
if or(full(bc+b)<>full(bc)+full(b)) then pause,end
if or(full(ac+b)<>full(ac)+full(b)) then pause,end
if or(full(ac+zer)<>full(zer)+full(ac)) then pause,end
if or(full(bc+a)<>full(bc)+full(a)) then pause,end
if or(full(ac-a)<>full(ac)-full(a)) then pause,end
if or(full(bc-b)<>full(bc)-full(b)) then pause,end
if or(full(ac-b)<>full(ac)-full(b)) then pause,end
if or(full(ac-zer)<>-full(zer)+full(ac)) then pause,end
if or(full(bc-a)<>full(bc)-full(a)) then pause,end
// -- complex complex
if or(full(vc+vc)<>full(vc)+full(vc)) then pause,end
if or(full(vc-vc)<>full(vc)-full(vc)) then pause,end
if or(full(vct+vct)<>full(vct)+full(vct)) then pause,end
if or(full(vct-vct)<>full(vct)-full(vct)) then pause,end
if or(full(ac+ac)<>full(ac)+full(ac)) then pause,end
if or(full(bc+bc)<>full(bc)+full(bc)) then pause,end
if or(full(ac+bc)<>full(ac)+full(bc)) then pause,end
if or(full(bc+ac)<>full(bc)+full(ac)) then pause,end
if or(full(ac-ac)<>full(zer)) then pause,end
if or(full(bc-bc)<>full(zer)) then pause,end
if or(full(ac-bc)<>full(ac)-full(bc)) then pause,end
if or(full(bc-ac)<>full(bc)-full(ac)) then pause,end
// sparse full and full sparse operation (soft coded apoerations)
if or(full(a+1)<>full(a)+1) then pause,end
if or(full(1+a)<>full(a)+1) then pause,end
if or(full(a+ones(6,6))<>full(a)+ones(6,6)) then pause,end
if or(full(ones(6,6)+a)<>full(a)+ones(6,6)) then pause,end
if or(full(a+2*eye)<>full(a)+2*eye) then pause,end
if or(full(2*eye+a)<>full(a)+2*eye) then pause,end
if or(full(a-1)<>full(a)-1) then pause,end
if or(full(1-a)<>1-full(a)) then pause,end
if or(full(a-ones(6,6))<>full(a)-ones(6,6)) then pause,end
if or(full(ones(6,6)-a)<>ones(6,6)-full(a)) then pause,end
if or(full(a-2*eye)<>full(a)-2*eye) then pause,end
if or(full(2*eye-a)<>2*eye-full(a)) then pause,end
if or(full(ac+1)<>full(ac)+1) then pause,end
if or(full(1+ac)<>full(ac)+1) then pause,end
if or(full(ac+ones(6,6))<>full(ac)+ones(6,6)) then pause,end
if or(full(ones(6,6)+ac)<>full(ac)+ones(6,6)) then pause,end
if or(full(ac+2*eye)<>full(ac)+2*eye) then pause,end
if or(full(2*eye+ac)<>full(ac)+2*eye) then pause,end
if or(full(ac-1)<>full(ac)-1) then pause,end
if or(full(1-ac)<>1-full(ac)) then pause,end
if or(full(ac-ones(6,6))<>full(ac)-ones(6,6)) then pause,end
if or(full(ones(6,6)-ac)<>ones(6,6)-full(ac)) then pause,end
if or(full(ac-2*eye)<>full(ac)-2*eye) then pause,end
if or(full(2*eye-ac)<>2*eye-full(ac)) then pause,end
if or(full(ac+full(bc))<>full(ac)+full(bc)) then pause,end
if or(full(ac-full(bc))<>full(ac)-full(bc)) then pause,end
if or(full(full(ac)+full(bc))<>full(ac)+full(bc)) then pause,end
end
if %t then
//-----------------------------------------------
// multiplication tests
//-----------------------------------------------
// real x real
// sparse scalar , saclar sparse
if or(full(a*2)<>full(a)*2) then pause,end
if or(full(2*a)<>full(a)*2) then pause,end
if  a*[]<>[] then pause,end
if  []*a<>[] then pause,end
c=rand(6,6);
if norm(a*c-full(a)*c) >100*%eps then pause,end
if norm(c*a-c*full(a)) >100*%eps then pause,end
// sparse sparse
if or(full(zer*zer)<>full(zer)) then pause,end
if or(full(a*zer)<>full(zer)) then pause,end
if or(full(zer*a)<>full(zer)) then pause,end
if  norm(full(a*a)-full(a)*full(a),1)>100*%eps then pause,end
if  norm(full(b*b)-full(b)*full(b),1)>100*%eps then pause,end
if  norm(full(a*b)-full(a)*full(b),1)>100*%eps then pause,end
if  norm(full(b*a)-full(b)*full(a),1)>100*%eps then pause,end
// complex x real real x complex
// sparse scalar , scalar sparse
if or(full(ac*2)<>full(ac)*2) then pause,end
if or(full(2*ac)<>full(ac)*2) then pause,end
if norm(full((2+%i)*a)-(2+%i)*full(a),1) >100*%eps then pause,end
if norm(full(a*(2+%i))-(2+%i)*full(a),1) >100*%eps then pause,end
if  ac*[]<>[] then pause,end
if  []*ac<>[] then pause,end
c=rand(6,6);
cc=c+rand(6,6)*%i;
if norm(ac*c-full(ac)*c) >100*%eps then pause,end
if norm(cc*a-cc*full(a)) >100*%eps then pause,end
// sparse sparse
if or(full(ac*zer)<>full(zer)) then pause,end
if or(full(zer*ac)<>full(zer)) then pause,end
if  norm(full(ac*a)-full(ac)*full(a),1)>100*%eps then pause,end
if  norm(full(bc*b)-full(bc)*full(b),1)>100*%eps then pause,end
if  norm(full(a*bc)-full(a)*full(bc),1)>100*%eps then pause,end
if  norm(full(b*ac)-full(b)*full(ac),1)>100*%eps then pause,end
// // complex x complex
if norm(ac*cc-full(ac)*cc) >100*%eps then pause,end
if norm(cc*ac-cc*full(ac)) >100*%eps then pause,end
// sparse sparse
if  norm(full(ac*ac)-full(ac)*full(ac),1)>100*%eps then pause,end
if  norm(full(bc*bc)-full(bc)*full(bc),1)>100*%eps then pause,end
if  norm(full(bc*ac)-full(bc)*full(ac),1)>100*%eps then pause,end
//----------------------------------------------------------
// element wise multiplication tests
//----------------------------------------------------------
if or(full(ac.*2)<>full(ac)*2) then pause,end
if or(full((2).*ac)<>full(ac)*2) then pause,end
if  a.*[]<>[] then pause,end
if  [].*a<>[] then pause,end
c=rand(6,6);
//if norm(a*c-full(a)*c) >100*%eps then pause,end
//if norm(c*a-c*full(a)) >100*%eps then pause,end
// sparse sparse
if or(full(zer.*zer)<>full(zer)) then pause,end
if or(full(a.*zer)<>full(zer)) then pause,end
if or(full(zer.*a)<>full(zer)) then pause,end
if  norm(full(a.*a)-full(a).*full(a),1)>100*%eps then pause,end
if  norm(full(b.*b)-full(b).*full(b),1)>100*%eps then pause,end
if  norm(full(a.*b)-full(a).*full(b),1)>100*%eps then pause,end
if  norm(full(b.*a)-full(b).*full(a),1)>100*%eps then pause,end
// complex x real real x complex
// sparse scalar , scalar sparse
if norm(full((2+%i).*a)-(2+%i).*full(a),1) >100*%eps then pause,end
if norm(full(a.*(2+%i))-(2+%i).*full(a),1) >100*%eps then pause,end
if  ac.*[]<>[] then pause,end
if  [].*ac<>[] then pause,end
c=rand(6,6);
cc=c+rand(6,6)*%i;
if norm(full(ac.*c)-full(ac).*c) >100*%eps then pause,end
if norm(full(cc.*a)-cc.*full(a)) >100*%eps then pause,end
// sparse sparse
if or(full(ac.*zer)<>full(zer)) then pause,end
if or(full(zer.*ac)<>full(zer)) then pause,end
if  norm(full(ac.*a)-full(ac).*full(a),1)>100*%eps then pause,end
if  norm(full(bc.*b)-full(bc).*full(b),1)>100*%eps then pause,end
if  norm(full(a.*bc)-full(a).*full(bc),1)>100*%eps then pause,end
if  norm(full(b.*ac)-full(b).*full(ac),1)>100*%eps then pause,end
// // complex x complex
if norm(ac.*cc-full(ac).*cc) >100*%eps then pause,end
if norm(cc.*ac-cc.*full(ac)) >100*%eps then pause,end
// sparse sparse
if  norm(full(ac.*ac)-full(ac).*full(ac),1)>100*%eps then pause,end
if  norm(full(bc.*bc)-full(bc).*full(bc),1)>100*%eps then pause,end
if  norm(full(bc.*ac)-full(bc).*full(ac),1)>100*%eps then pause,end
// ----------------------------------------------------------
// test de la transposition
//-----------------------------------------------------------
if or(full(a')<>full(a)') then pause,end
if or(full(ac')<>full(ac)') then pause,end
if or(full(zer')<>full(zer)' ) then pause,end
v=sparse([2 1;3 1;4 1;6 1],[10;11;12;13],[6,1]);
if or(full(v')<>full(v)') then pause,end
if or(full((v')')<>full(v)) then pause,end
vc=sparse([2 1;3 1;4 1;6 1],[10-3*%i;11;12+5*%i;13+0.5*%i],[6,1]);
if or(full(vc')<>full(vc)') then pause,end
if or(full((vc')')<>full(vc)) then pause,end
// ----------------------------------------------------------
// test des concatenation
//-----------------------------------------------------------
if or(full([a])<>full(a)) then pause,end
if or(full([a b])<>[full(a) full(b)]) then pause,end
if or(full([a;b])<>[full(a);full(b)]) then pause,end
if or(full([a []])<>full(a)) then pause,end
if or(full([a;[]])<>full(a)) then pause,end
if or(full([a zer])<>[full(a) full(zer)]) then pause,end
if or(full([zer;b])<>[full(zer);full(b)]) then pause,end
if or(full([ac])<>full(ac)) then pause,end
if or(full([ac b])<>[full(ac) full(b)]) then pause,end
if or(full([ac;b])<>[full(ac);full(b)]) then pause,end
if or(full([ac []])<>full(ac)) then pause,end
if or(full([ac;[]])<>full(ac)) then pause,end
if or(full([a bc])<>[full(a) full(bc)]) then pause,end
if or(full([a;bc])<>[full(a);full(bc)]) then pause,end
if or(full([ac bc])<>[full(ac) full(bc)]) then pause,end
if or(full([ac;bc])<>[full(ac);full(bc)]) then pause,end
// ----------------------------------------------------------
// test des extractions
//-----------------------------------------------------------
af=full(a);
if or(full(a(1,3))<>af(1,3)) then pause,end
if or(full(a(1,4))<>af(1,4)) then pause,end
if or(full(a(1,:))<>af(1,:)) then pause,end
v=sparse([2 1;3 1;4 1;6 1],[10;11;12;13],[6,1]);vf=full(v);
if or(full(v(:))<>vf(:)) then pause,end
if or(full(v(3:4))<>vf(3:4)) then pause,end
if or(full(v([1 5]))<>vf([1 5])) then pause,end
if or(full(v([4 3]))<>vf([4 3])) then pause,end
if or(full(v([4 4]))<>vf([4 4])) then pause,end
if or(full(v([1 1]))<>vf([1 1])) then pause,end
v=v';vf=vf';
if or(full(v(:))<>vf(:)) then pause,end
if or(full(v(3:4))<>vf(3:4)) then pause,end
if or(full(v([1 5]))<>vf([1 5])) then pause,end
if or(full(v([4 3]))<>vf([4 3])) then pause,end
if or(full(v([4 4]))<>vf([4 4])) then pause,end
if or(full(v([1 1]))<>vf([1 1])) then pause,end
acff=full(ac);
if or(full(ac(1,3))<>acff(1,3)) then pause,end
if or(full(ac(1,4))<>acff(1,4)) then pause,end
if or(full(ac(1,:))<>acff(1,:)) then pause,end
vc=sparse([2 1;3 1;4 1;6 1],[10-3*%i;11;12+5*%i;13+0.5*%i],[6,1]);vcf=full(vc);
if or(full(vc(:))<>vcf(:)) then pause,end
if or(full(vc(3:4))<>vcf(3:4)) then pause,end
if or(full(vc([1 5]))<>vcf([1 5])) then pause,end
if or(full(vc([4 3]))<>vcf([4 3])) then pause,end
if or(full(vc([4 4]))<>vcf([4 4])) then pause,end
if or(full(vc([1 1]))<>vcf([1 1])) then pause,end
vc=vc';vcf=vcf';
if or(full(vc(:))<>vcf(:)) then pause,end
if or(full(vc(3:4))<>vcf(3:4)) then pause,end
if or(full(vc([1 5]))<>vcf([1 5])) then pause,end
if or(full(vc([4 3]))<>vcf([4 3])) then pause,end
if or(full(vc([4 4]))<>vcf([4 4])) then pause,end
if or(full(vc([1 1]))<>vcf([1 1])) then pause,end
// ----------------------------------------------------------
// test des insertions
//-----------------------------------------------------------
end
a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
vt=sparse([1 2;1 3;1 4;1 6],[10;11;12;13],[1,6]);
// full line insertion
//----------------------
a1=a;a1(1,:)=vt;A=full(a);Vt=full(vt);A(1,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(3,:)=vt;A=full(a);Vt=full(vt);A(3,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(1,:)=vt;A=full(a);Vt=full(vt);A(1,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(3,:)=vt;A=full(a);Vt=full(vt);A(3,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(7,:)=vt;A=full(a);Vt=full(vt);A(7,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(8,:)=vt;A=full(a);Vt=full(vt);A(8,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1([1 3],:)=[vt;2*vt];A=full(a);Vt=full(vt);A([1 3],:)=[Vt;2*Vt];
if or(full(a1)<>A) then pause,end
a1=a;a1([3 1],:)=[vt;2*vt];A=full(a);Vt=full(vt);A([3 1],:)=[Vt;2*Vt];
if or(full(a1)<>A) then pause,end
a1=a;a1([1 3 1],:)=[vt;2*vt;3*vt];A=full(a);Vt=full(vt);A([1 3 1],:)=[Vt;2*Vt;3*Vt];
if or(full(a1)<>A) then pause,end
//  insert zero vector
vt=sparse([],[],[1,6]);
a1=a;a1(1,:)=vt;A=full(a);Vt=full(vt);A(1,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(3,:)=vt;A=full(a);Vt=full(vt);A(3,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(1,:)=vt;A=full(a);Vt=full(vt);A(1,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(3,:)=vt;A=full(a);Vt=full(vt);A(3,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(7,:)=vt;A=full(a);Vt=full(vt);A(7,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(8,:)=vt;A=full(a);Vt=full(vt);A(8,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1([1 3],:)=[vt;2*vt];A=full(a);Vt=full(vt);A([1 3],:)=[Vt;2*Vt];
if or(full(a1)<>A) then pause,end
a1=a;a1([3 1],:)=[vt;2*vt];A=full(a);Vt=full(vt);A([3 1],:)=[Vt;2*Vt];
if or(full(a1)<>A) then pause,end
a1=a;a1([1 3 1],:)=[vt;2*vt;3*vt];A=full(a);Vt=full(vt);A([1 3 1],:)=[Vt;2*Vt;3*Vt];
if or(full(a1)<>A) then pause,end
a=sparse([],[],[6,6]);
a1=a;a1(1,:)=vt;A=full(a);Vt=full(vt);A(1,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(3,:)=vt;A=full(a);Vt=full(vt);A(3,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(1,:)=vt;A=full(a);Vt=full(vt);A(1,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(3,:)=vt;A=full(a);Vt=full(vt);A(3,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(7,:)=vt;A=full(a);Vt=full(vt);A(7,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(8,:)=vt;A=full(a);Vt=full(vt);A(8,:)=Vt;
if or(full(a1)<>A) then pause,end
b=sparse([1 1;1 3;1 6;2 1;2 2;2 4],10:15,[2,6]);
a1=a;a1([1 3],:)=b;A=full(a);B=full(b);A([1 3],:)=B;
if or(full(a1)<>A) then pause,end
a1=a;a1([3 1],:)=b;A=full(a);B=full(b);A([3 1],:)=B;
if or(full(a1)<>A) then pause,end
b=sparse([1 1;1 3;1 6;2 1;2 2;2 4;3 3;3 5],10:17,[3,6]);
a1=a;a1([1 3 1],:)=b;A=full(a);B=full(b);A([1 3 1],:)=B;
if or(full(a1)<>A) then pause,end
//  insert zero vector
vt=sparse([],[],[1,6]);
a1=a;a1(1,:)=vt;A=full(a);Vt=full(vt);A(1,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(3,:)=vt;A=full(a);Vt=full(vt);A(3,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(1,:)=vt;A=full(a);Vt=full(vt);A(1,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(3,:)=vt;A=full(a);Vt=full(vt);A(3,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(7,:)=vt;A=full(a);Vt=full(vt);A(7,:)=Vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(8,:)=vt;A=full(a);Vt=full(vt);A(8,:)=Vt;
if or(full(a1)<>A) then pause,end
b=sparse([1 1;1 3;1 6;2 1;2 2;2 4],10:15,[2,6]);
a1=a;a1([1 3],:)=b;A=full(a);B=full(b);A([1 3],:)=B;
if or(full(a1)<>A) then pause,end
a1=a;a1([3 1],:)=b;A=full(a);B=full(b);A([3 1],:)=B;
if or(full(a1)<>A) then pause,end
b=sparse([1 1;1 3;1 6;2 1;2 2;2 4;3 3;3 5],10:17,[3,6]);
a1=a;a1([1 3 1],:)=b;A=full(a);B=full(b);A([1 3 1],:)=B;
if or(full(a1)<>A) then pause,end
// full column insertion
//----------------------
a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
v=sparse([2 1;3 1;4 1;6 1],[10;11;12;13],[6,1]);
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,3)=v;A=full(a);V=full(v);A(:,3)=V;
if or(full(a1)<>A) then pause,end
//
a1=a;a1(:,7)=v;A=full(a);V=full(v);A(:,7)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,8)=v;A=full(a);V=full(v);A(:,8)=V;
if or(full(a1)<>A) then pause,end
b=sparse([1 2;2 1;3 1;3 2;4 1;6 2],10:15,[6,2]);
a1=a;a1(:,[1 3])=b;A=full(a);B=full(b);A(:,[1 3])=B;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,[3 1])=b;A=full(a);B=full(b);A(:,[3 1])=B;
if or(full(a1)<>A) then pause,end
b=sparse([1 2;2 1;2 3;3 1;3 2;4 1;5 3;6 2],10:17,[6,3]);
a1=a;a1(:,[1 3 1])=b;A=full(a);B=full(b);A(:,[1 3 1])=B;
if or(full(a1)<>A) then pause,end
v=sparse([],[],[6,1]);
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,3)=v;A=full(a);V=full(v);A(:,3)=V;
if or(full(a1)<>A) then pause,end
//
a1=a;a1(:,7)=v;A=full(a);V=full(v);A(:,7)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,8)=v;A=full(a);V=full(v);A(:,8)=V;
if or(full(a1)<>A) then pause,end
b=sparse([],[],[6,2]);
a1=a;a1(:,[1 3])=b;A=full(a);B=full(b);A(:,[1 3])=B;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,[3 1])=b;A=full(a);B=full(b);A(:,[3 1])=B;
if or(full(a1)<>A) then pause,end
b=sparse([],[],[6,3]);
a1=a;a1(:,[1 3 1])=b;A=full(a);B=full(b);A(:,[1 3 1])=B;
if or(full(a1)<>A) then pause,end
a=sparse([],[],[6,6]);
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,3)=v;A=full(a);V=full(v);A(:,3)=V;
if or(full(a1)<>A) then pause,end
//
a1=a;a1(:,7)=v;A=full(a);V=full(v);A(:,7)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,8)=v;A=full(a);V=full(v);A(:,8)=V;
if or(full(a1)<>A) then pause,end
b=sparse([1 2;2 1;3 1;3 2;4 1;6 2],10:15,[6,2]);
a1=a;a1(:,[1 3])=b;A=full(a);B=full(b);A(:,[1 3])=B;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,[3 1])=b;A=full(a);B=full(b);A(:,[3 1])=B;
if or(full(a1)<>A) then pause,end
b=sparse([1 2;2 1;2 3;3 1;3 2;4 1;5 3;6 2],10:17,[6,3]);
a1=a;a1(:,[1 3 1])=b;A=full(a);B=full(b);A(:,[1 3 1])=B;
if or(full(a1)<>A) then pause,end
v=sparse([],[],[6,1]);
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,3)=v;A=full(a);V=full(v);A(:,3)=V;
if or(full(a1)<>A) then pause,end
//
a1=a;a1(:,7)=v;A=full(a);V=full(v);A(:,7)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,8)=v;A=full(a);V=full(v);A(:,8)=V;
if or(full(a1)<>A) then pause,end
b=sparse([1 2;2 1;3 1;3 2;4 1;6 2],10:15,[6,2]);
a1=a;a1(:,[1 3])=b;A=full(a);B=full(b);A(:,[1 3])=B;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,[3 1])=b;A=full(a);B=full(b);A(:,[3 1])=B;
if or(full(a1)<>A) then pause,end
b=sparse([1 2;2 1;2 3;3 1;3 2;4 1;5 3;6 2],10:17,[6,3]);
a1=a;a1(:,[1 3 1])=b;A=full(a);B=full(b);A(:,[1 3 1])=B;
if or(full(a1)<>A) then pause,end
v=sparse([],[],[6,1]);
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,1)=v;A=full(a);V=full(v);A(:,1)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,2)=v;A=full(a);V=full(v);A(:,2)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,3)=v;A=full(a);V=full(v);A(:,3)=V;
if or(full(a1)<>A) then pause,end
//
a1=a;a1(:,7)=v;A=full(a);V=full(v);A(:,7)=V;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,8)=v;A=full(a);V=full(v);A(:,8)=V;
if or(full(a1)<>A) then pause,end
b=sparse([],[],[6,2]);
a1=a;a1(:,[1 3])=b;A=full(a);B=full(b);A(:,[1 3])=B;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,[3 1])=b;A=full(a);B=full(b);A(:,[3 1])=B;
if or(full(a1)<>A) then pause,end
b=sparse([],[],[6,3]);
a1=a;a1(:,[1 3 1])=b;A=full(a);B=full(b);A(:,[1 3 1])=B;
if or(full(a1)<>A) then pause,end
// row column insertion
a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
b=sparse([1 2;1 3;3 3],-(1:3),[3,3]);
a1=a;a1(1,1)=sparse(30);A=full(a);A(1,1)=30;
if or(full(a1)<>A) then pause,end
a1=a;a1(1,6)=sparse(30);A=full(a);A(1,6)=30;
if or(full(a1)<>A) then pause,end
a1=a;a1(1,8)=sparse(30);A=full(a);A(1,8)=30;
if or(full(a1)<>A) then pause,end
a1=a;a1(1:3,1:3)=b;A=full(a);A(1:3,1:3)=full(b);
if or(full(a1)<>A) then pause,end
a1=a;a1(1:3,6:8)=b;A=full(a);A(1:3,6:8)=full(b);
if or(full(a1)<>A) then pause,end
a1=a;a1(6:8,1:3)=b;A=full(a);A(6:8,1:3)=full(b);
if or(full(a1)<>A) then pause,end
a1=a;a1([3 2 1],1:3)=b;A=full(a);A([3 2 1],1:3)=full(b);
if or(full(a1)<>A) then pause,end
a1=a;a1([1 2 1],1:3)=b;A=full(a);A([1 2 1],1:3)=full(b);
if or(full(a1)<>A) then pause,end
a1=a;a1([3 2 1],[3 2 1])=b;A=full(a);A([3 2 1],[3 2 1])=full(b);
if or(full(a1)<>A) then pause,end
a1=a;a1([1 2 1],[3 2 1])=b;A=full(a);A([1 2 1],[3 2 1])=full(b);
if or(full(a1)<>A) then pause,end
a1=a;a1([1 2 1],[1 2 1])=b;A=full(a);A([1 2 1],[1 2 1])=full(b);
if or(full(a1)<>A) then pause,end
//sparse full
a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
vt=11:16;
// full line insertion
//----------------------
a1=a;a1(1,:)=vt;A=full(a);A(1,:)=vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(3,:)=vt;A=full(a);A(3,:)=vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(1,:)=vt;A=full(a);A(1,:)=vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(3,:)=vt;A=full(a);A(3,:)=vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(7,:)=vt;A=full(a);A(7,:)=vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(8,:)=vt;A=full(a);A(8,:)=vt;
if or(full(a1)<>A) then pause,end
a1=a;a1([1 3],:)=[vt;2*vt];A=full(a);A([1 3],:)=[vt;2*vt];
if or(full(a1)<>A) then pause,end
a1=a;a1([3 1],:)=[vt;2*vt];A=full(a);A([3 1],:)=[vt;2*vt];
if or(full(a1)<>A) then pause,end
a1=a;a1([1 3 1],:)=[vt;2*vt;3*vt];A=full(a);A([1 3 1],:)=[vt;2*vt;3*vt];
if or(full(a1)<>A) then pause,end
a=sparse([],[],[6,6]);
a1=a;a1(1,:)=vt;A=full(a);A(1,:)=vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(3,:)=vt;A=full(a);A(3,:)=vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(1,:)=vt;A=full(a);A(1,:)=vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(3,:)=vt;A=full(a);A(3,:)=vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(7,:)=vt;A=full(a);A(7,:)=vt;
if or(full(a1)<>A) then pause,end
a1=a;a1(8,:)=vt;A=full(a);A(8,:)=vt;
if or(full(a1)<>A) then pause,end
b=[1:6;11:16];
a1=a;a1([1 3],:)=b;A=full(a);A([1 3],:)=b;
if or(full(a1)<>A) then pause,end
a1=a;a1([3 1],:)=b;A=full(a);A([3 1],:)=b;
if or(full(a1)<>A) then pause,end
b=[1:6;11:16;21:26];
a1=a;a1([1 3 1],:)=b;A=full(a);A([1 3 1],:)=b;
if or(full(a1)<>A) then pause,end
// full column insertion
//----------------------
a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
v=(1:6)';
a1=a;a1(:,1)=v;A=full(a);A(:,1)=v;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,2)=v;A=full(a);A(:,2)=v;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,1)=v;A=full(a);A(:,1)=v;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,2)=v;A=full(a);A(:,2)=v;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,3)=v;A=full(a);A(:,3)=v;
if or(full(a1)<>A) then pause,end
//
a1=a;a1(:,7)=v;A=full(a);A(:,7)=v;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,8)=v;A=full(a);A(:,8)=v;
if or(full(a1)<>A) then pause,end
b=[(1:6)' (11:16)'];
a1=a;a1(:,[1 3])=b;A=full(a);A(:,[1 3])=b;
if or(full(a1)<>A) then pause,end
a1=a;a1(:,[3 1])=b;A=full(a);A(:,[3 1])=b;
if or(full(a1)<>A) then pause,end
b=[(1:6)' (11:16)' (21:26)'];
a1=a;a1(:,[1 3 1])=b;A=full(a);A(:,[1 3 1])=b;
if or(full(a1)<>A) then pause,end
//********
// row column insertion
a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
b=-[1 2 3;4 5 6;7 8 9];
a1=a;a1(1,1)=30;A=full(a);A(1,1)=30;
if or(full(a1)<>A) then pause,end
a1=a;a1(1,6)=30;A=full(a);A(1,6)=30;
if or(full(a1)<>A) then pause,end
a1=a;a1(1,8)=30;A=full(a);A(1,8)=30;
if or(full(a1)<>A) then pause,end
a1=a;a1(1:3,1:3)=b;A=full(a);A(1:3,1:3)=b;
if or(full(a1)<>A) then pause,end
a1=a;a1(1:3,6:8)=b;A=full(a);A(1:3,6:8)=b;
if or(full(a1)<>A) then pause,end
a1=a;a1(6:8,1:3)=b;A=full(a);A(6:8,1:3)=b;
if or(full(a1)<>A) then pause,end
a1=a;a1([3 2 1],1:3)=b;A=full(a);A([3 2 1],1:3)=b;
if or(full(a1)<>A) then pause,end
a1=a;a1([1 2 1],1:3)=b;A=full(a);A([1 2 1],1:3)=b;
if or(full(a1)<>A) then pause,end
a1=a;a1([3 2 1],[3 2 1])=b;A=full(a);A([3 2 1],[3 2 1])=b;
if or(full(a1)<>A) then pause,end
a1=a;a1([1 2 1],[3 2 1])=b;A=full(a);A([1 2 1],[3 2 1])=b;
if or(full(a1)<>A) then pause,end
a1=a;a1([1 2 1],[1 2 1])=b;A=full(a);A([1 2 1],[1 2 1])=b;
if or(full(a1)<>A) then pause,end
// vector insertion
v=sparse([1 1;3 1;6 1],[10 11 12],[6 1]);
v1=v;v1(1)=33;V=full(v);V(1)=33;
if or(full(v1)<>V) then pause,end
v1=v;v1(2)=33;V=full(v);V(2)=33;
if or(full(v1)<>V) then pause,end
v1=v;v1(8)=33;V=full(v);V(8)=33;
if or(full(v1)<>V) then pause,end
v1=v;v1([1 2 8])=[5;10;33];V=full(v);V([1 2 8])=[5;10;33];
if or(full(v1)<>V) then pause,end
v1=v;v1(:)=[];
if or(full(v1)<>[]) then pause,end
v1=v;v1(1)=sparse(33);V=full(v);V(1)=33;
if or(full(v1)<>V) then pause,end
v1=v;v1(2)=sparse(33);V=full(v);V(2)=33;
if or(full(v1)<>V) then pause,end
v1=v;v1(8)=sparse(33);V=full(v);V(8)=33;
if or(full(v1)<>V) then pause,end
v1=v;v1([1 2 8])=sparse([5;10;33]);V=full(v);V([1 2 8])=[5;10;33];
if or(full(v1)<>V) then pause,end
v1=v;v1([1 2 1])=sparse([5;10;33]);V=full(v);V([1 2 1])=[5;10;33];
if or(full(v1)<>V) then pause,end
v1=v;v1(:)=[];
if or(full(v1)<>[]) then pause,end
v1=v;v1(:)=sparse([2 1],44,[6 1]);V=full(v);V(:)=[0;44;0;0;0;0];
if or(full(v1)<>V) then pause,end
v=v';
v1=v;v1(1)=33;V=full(v);V(1)=33;
if or(full(v1)<>V) then pause,end
v1=v;v1(2)=33;V=full(v);V(2)=33;
if or(full(v1)<>V) then pause,end
v1=v;v1(8)=33;V=full(v);V(8)=33;
if or(full(v1)<>V) then pause,end
v1=v;v1([1 2 8])=[5 10 33];V=full(v);V([1 2 8])=[5 10 33];
if or(full(v1)<>V) then pause,end
v1=v;v1(1)=sparse(33);V=full(v);V(1)=33;
if or(full(v1)<>V) then pause,end
v1=v;v1(2)=sparse(33);V=full(v);V(2)=33;
if or(full(v1)<>V) then pause,end
v1=v;v1(8)=sparse(33);V=full(v);V(8)=33;
if or(full(v1)<>V) then pause,end
v1=v;v1([1 2 8])=sparse([5 10 33]);V=full(v);V([1 2 8])=[5 10 33];
if or(full(v1)<>V) then pause,end
v1=v;v1([1 2 1])=sparse([5 10 33]);V=full(v);V([1 2 1])=[5 10 33];
if or(full(v1)<>V) then pause,end
v1=v;v1(:)=sparse([1 2],44,[1,6]);V=full(v);V(:)=[0 44 0 0 0 0];
if or(full(v1)<>V) then pause,end
v1=v;v1(1)=[];V=full(v);V(1)=[];
if or(full(v1)<>V) then pause,end
//test des comparaisons
a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
b=sparse([1 6;1 2;6 5],[10;-1;-1],[6 6]);
if full(a==a)<>full(a)==full(a) then pause,end
if full(a<>a)<>(full(a)<>full(a)) then pause,end
if full(a>sparse(5))<>(full(a)>5) then pause,end
if full(sparse(5)>a)<>(5>full(a)) then pause,end
if full(b>a)<>(full(b)>full(a))  then pause,end
if full(a==full(a))<>full(a)==full(a) then pause,end
if full(a<>full(a))<>(full(a)<>full(a)) then pause,end
if full(a>5)<>(full(a)>5) then pause,end
if full(5>a)<>(5>full(a)) then pause,end
if full(b>full(a))<>(full(b)>full(a))  then pause,end
if full(full(a)==a)<>full(a)==full(a) then pause,end
if full(full(a)<>a)<>(full(a)<>full(a)) then pause,end
if full(full(a)>sparse(5))<>(full(a)>5) then pause,end
if full(full(b)>a)<>(full(b)>full(a))  then pause,end
a=sparse([1 1;3 1;6 1],[10 11 12],[6 1]);
if full(a==a)<>full(a)==full(a) then pause,end
if full(a<>a)<>(full(a)<>full(a)) then pause,end
if full(a>sparse(5))<>(full(a)>5) then pause,end
if full(sparse(5)>a)<>(5>full(a)) then pause,end
if full(a==full(a))<>full(a)==full(a) then pause,end
if full(a<>full(a))<>(full(a)<>full(a)) then pause,end
if full(a>5)<>(full(a)>5) then pause,end
if full(5>a)<>(5>full(a)) then pause,end
if full(full(a)==a)<>full(a)==full(a) then pause,end
if full(full(a)<>a)<>(full(a)<>full(a)) then pause,end
if full(full(a)>sparse(5))<>(full(a)>5) then pause,end
a=a';
if full(a==a)<>full(a)==full(a) then pause,end
if full(a<>a)<>(full(a)<>full(a)) then pause,end
if full(a>sparse(5))<>(full(a)>5) then pause,end
if full(sparse(5)>a)<>(5>full(a)) then pause,end
if full(a==full(a))<>full(a)==full(a) then pause,end
if full(a<>full(a))<>(full(a)<>full(a)) then pause,end
if full(a>5)<>(full(a)>5) then pause,end
if full(5>a)<>(5>full(a)) then pause,end
if full(full(a)==a)<>full(a)==full(a) then pause,end
if full(full(a)<>a)<>(full(a)<>full(a)) then pause,end
if full(full(a)>sparse(5))<>(full(a)>5) then pause,end
a=sparse([1 6;1 5;1 3;2 4;2 1;4 4;4 3;5 1;6 6],1:9,[6 6]);
b=sparse([1 6;1 2;5 5],[10;-1;-1],[5 6]);
if a==b<>%f then pause,end
if a<>b<>%t then pause,end
