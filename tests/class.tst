// an object of Classa 


a=%types.ClassA.new[cla_color=89,cla_thickness=56];

// attributes of object 

a.__attrs

a.cla_color = 32 
a.cla_thickness = 2
a.cla_val = rand(2,2)

a.cla_color
a.cla_thickness
a.cla_val 

a.cla_val(3,3) = 1;
a.cla_val 

// a function for Classa instances 

a('cla_color') = 78  //   {"setrowscols_cla",int_set_attribute}, 

test(a) 

// an object of Classb which inherits from Classa 

b=%types.ClassB.new[color=89,thickness=56];

b.__attrs 

// inherited attributes 

b.cla_color = 78 
b.cla_val = rand(2,2)
b.cla_val(3,3) = 5 

b.cla_val 
b.cla_color 

// specific attributes 

b.clb_color = 5 
b.clb_color 
b.clb_val = [5,6;7,8];
b.clb_val(3,3) = 100;

b.clb_val 

// methods 

b.classb_color_change[89]
b.classa_color_change[890]
b.classb_color_show[] 

// functions for Classa instances 

test(b) 
test(a) 
