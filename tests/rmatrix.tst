// -*- Mode: nsp -*- 

function assert_checkequal(a,b)
  if ~a.equal[b] then pause;end
endfunction

s=poly(0,'s');

// p2r 
//-----

n= 1+s;
d = 1+2*s^2;
r = p2r( n,d);

if ~n.equal[r.num] then pause;end 
if ~d.equal[r.den] then pause;end 
if ~ r.dom.equal['u'] then pause;end 

r1= n/d;
if ~ r1.equal[r] then pause;end 

// simp

n= 1+s;
d = (1+s)*(1-s);
r = p2r( n,d);

if ~n.equal[r.num] then pause;end 
if ~d.equal[r.den] then pause;end 

r = p2r(n,d,simp=%t)
if norm(coeff(r.num - (-1+0*s))) > 10*%eps then pause;end
if norm(coeff(r.den - (-1+s))) > 10*%eps then pause;end
    
r1 = n/d;
if ~r1.equal[r] then pause;end 

// [,]

r = [1/(1+s),s/(1+s^2)];
if ~r.num.equal[[1,s]] then pause;end 
if ~r.den.equal[[1+s,1+s^2]] then pause;end 

r = [1/(1+s),1+s^3];
if ~r.num.equal[[1,1+s^3]] then pause;end 
if ~r.den.equal[[1+s,1]] then pause;end 

r = [1/(1+s),78];
if ~r.num.equal[[1,78+0*s]] then pause;end 
if ~r.den.equal[[1+s,1]] then pause;end 

r = [7,1/(1+s)];
if ~r.num.equal[[7,1+0*s]] then pause;end 
if ~r.den.equal[[1,1+s]] then pause;end 

// [;]

r = [1/(1+s);s/(1+s^2)];
if ~r.num.equal[[1;s]] then pause;end 
if ~r.den.equal[[1+s;1+s^2]] then pause;end 

r = [1/(1+s);1+s^3];
if ~r.num.equal[[1;1+s^3]] then pause;end 
if ~r.den.equal[[1+s;1]] then pause;end 

r = [1/(1+s);78];
if ~r.num.equal[[1;78+0*s]] then pause;end 
if ~r.den.equal[[1+s;1]] then pause;end 

r = [7;1/(1+s)];
if ~r.num.equal[[7;1+0*s]] then pause;end 
if ~r.den.equal[[1;1+s]] then pause;end 

// [ # ] 

if %f then 

r = [1/(1+s) # s/(1+s^2)];
if ~r.num.equal[[1 # s]] then pause;end 
if ~r.den.equal[[1+s # 1+s^2]] then pause;end 

r = [1/(1+s) # 1+s^3];
if ~r.num.equal[[1 # 1+s^3]] then pause;end 
if ~r.den.equal[[1+s # 1]] then pause;end 

r = [1/(1+s) # 78];
if ~r.num.equal[[1 # 78+0*s]] then pause;end 
if ~r.den.equal[[1+s # 1]] then pause;end 

r = [7 # 1/(1+s)];
if ~r.num.equal[[7 # 1+0*s]] then pause;end 
if ~r.den.equal[[1 # 1+s]] then pause;end 

end

// extract 

R= [1/(1+s),(1+s^2)/s,6;1+1/s,8,1+s^4];

if ~R(1).equal[1/(1+s)] then pause;end 
if ~R(1,1).equal[1/(1+s)] then pause;end 
if ~R(3).equal[(1+s^2)/s] then pause;end 
if ~R(:,1).equal[[1/(1+s);(1+s)/s]] then pause;end 
if ~R(1,:).equal[[1/(1+s),(1+s^2)/s,6]] then pause;end 

R(1,2)= 1/s;
if ~ R.equal[[1/(1+s),1/s,6;1+1/s,8,1+s^4]]; then pause;end
R(1,:)= [1/s,6,s]; 
if ~ R.equal[[1/s,6,s;1+1/s,8,1+s^4]]; then pause;end

// + 

r = [1/(1+s);s/(1+s^2)];
R = r+(1/s)
if ~R.equal[[(1+2*s)/(s+s^2);(1+2*s^2)/(s+s^3)]] then pause;end

// - 

r = [1/(1+s);s/(1+s^2)];
R = r - (1/s)
if ~R.equal[[(-1)/(s+s^2);(-1)/(s+s^3)]] then pause;end

// *

r = [1/(1+s);s/(1+s^2)];
R = r *(1/s)
if ~R.equal[[(1)/(s+s^2);(1)/(1+s^2)]] then pause;end

r = [1/(1+s),s/(1+s^2)]
R = r*[(1+s);2/s]
if ~R.equal[[1+(2/(1+s^2))]] then pause;end 

r = [1/(1+s),s/(1+s^2)]
R = r *[(1+s);2/s]
if ~R.equal[[1+(2/(1+s^2))]] then pause;end 

// / 

r = [1/(1+s),s/(1+s^2)]
R = r / (s);
if ~R.equal[[1/(s+s^2),1/(1+s^2)]] then pause;end 

r = [1/(1+s),s/(1+s^2)]
R = r ./ (s);
if ~R.equal[[1/(s+s^2),1/(1+s^2)]] then pause;end 

// 
