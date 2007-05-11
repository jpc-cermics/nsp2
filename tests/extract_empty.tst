x=1:5;
// special case for 0x1 !
if x(ones(0,1)) <> zeros(1,0) then pause;end 
if x(ones(1,0)) <> zeros(1,0);then pause;end 
if x(5:0) <> zeros(1,0);then pause;end 

if x(ones(0,7)) <> zeros(0,7) then pause;end 
if x(ones(7,0)) <> zeros(7,0);then pause;end 

if x(ones(0,0)) <> zeros(0,0);then pause;end 

x=(1:5)';
// special case for 0x1 !
if x(ones(0,1)) <> zeros(0,1) then pause;end 
if x(ones(1,0)) <> zeros(0,1);then pause;end 
if x(5:0) <> zeros(0,1);then pause;end 

if x(ones(0,7)) <> zeros(0,7);then pause;end 
if x(ones(7,0)) <> zeros(7,0);then pause;end 

if x(ones(0,0)) <> zeros(0,0);then pause;end 

x=rand(4,4);
// here 0x1 is not a special case 
if x(ones(0,1)) <> zeros(0,1) then pause;end 
if x(ones(1,0)) <> zeros(1,0);then pause;end 

if x(ones(0,7)) <> zeros(0,7);then pause;end 
if x(ones(7,0)) <> zeros(7,0);then pause;end 

if x(ones(0,0)) <> zeros(0,0);then pause;end 
x=zeros(0,4);
// here 0x1 is not a special case 
if x(ones(0,1)) <> zeros(0,1) then pause;end 
if x(ones(1,0)) <> zeros(1,0);then pause;end 

if x(ones(0,7)) <> zeros(0,7);then pause;end 
if x(ones(7,0)) <> zeros(7,0);then pause;end 
if x(ones(0,0)) <> zeros(0,0);then pause;end 



