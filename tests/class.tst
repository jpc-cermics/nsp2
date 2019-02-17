// -*- Mode: nsp -*- 
// an object of Classa 


a=%types.ClassA.new[cla_color=89,cla_thickness=56,cla_val=[]];

// attributes of object 

a.__attrs

a.cla_color = 32 ;
a.cla_thickness = 2;
A=testmatrix('magic',3);
a.cla_val = A;

if a.cla_color <> 32 then pause;end 
if a.cla_thickness <> 2 then pause;end
if a.cla_val <> A then pause;end

a.cla_val(3,3) = 1;
A(3,3)=1;
if a.cla_val <> A then pause;end

// general access to attribute (method set of objects)

a.set[cla_val=1:5,cla_color=56];

// a function for Classa instances 

a('cla_color') = 78  //   {"setrowscols_cla",int_set_attribute}, 

// a('cla_color')  // extractelts_cla to be done XXX (copy from hash)

clatest(a) 

// an object of Classb which inherits from Classa 

b=%types.ClassB.new[clb_color=89,clb_thickness=56,cla_color=3,clb_val=[]];

b.__attrs 

// inherited attributes 

A=testmatrix('magic',3);
b.cla_color = 78
b.cla_val = A;
b.cla_val(3,3) = 5 

A(3,3)=5;
if b.cla_color <> 78 then pause;end 
if b.cla_val <> A then pause;end

// specific attributes 

b.clb_color = 5 ;
b.clb_val = [5,6;7,8];
b.clb_val(3,3) = 100;

if b.clb_val  <> [5,6,0;7,8,0;0,0,100] then pause;end

// methods 

b.classb_color_change[89]
b.classa_color_change[890]
b.classb_color_show[] 

// functions for Classa instances 
// which can be used for instances of Classb

clatest(b);
clatest(a);

b.cla_val = 1:6;
b1 = b;
save('TMPDIR/bsave',b,D=89);
load('TMPDIR/bsave');
  
if ~b1.equal[b] then pause;end;


