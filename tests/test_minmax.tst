// -*- Mode: scilab -*- 
// min max with %nan 

n=10;
x=grand(1,'prm',(1:10)');
iM=find(x==10);
im=find(x==1);
v1 = [%nan,x',x'];
v2 = [x',%nan,x'];
w = [v1 ; v2];

[M1, iM1] = max(v1);
[M2, iM2] = max(v2);

if M1 <> 10 then pause;end
if M2 <> 10 then pause;end
if iM1 <> iM+1 then pause;end
if iM2 <> iM then pause;end

[m1, im1] = min(v1);
[m2, im2] = min(v2);

if m1 <> 1 then pause;end
if m2 <> 1 then pause;end
if im1 <> im+1 then pause;end
if im2 <> im then pause;end

[mc,imc] = min(w,"c");
[mr,imr] = min(w,"r");

if mc<>[1;1] then pause;end 
if imc<>[im+1;im] then pause;end 

[Mc,iMc] = max(w,"c");
[Mr,iMr] = max(w,"r");

if Mc<>[10;10] then pause;end 
if iMc<>[iM+1;iM] then pause;end 
