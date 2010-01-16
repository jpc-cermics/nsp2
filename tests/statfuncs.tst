// -*- Mode: scilab -*- 
//
//  some tests for basic statistic functions (Bruno 16 Jan 2010)
//  (mean, var, std, median) 
//
//
//////////////////////////////////////////////////////////////////////////
// 1/ test with a simple example : values are 1,2,...,n
//    so  mean is (n+1)/2
//    and var is n(n+1)/12 (unbiased) or (n-1)(n+1)/12 (biased)
//////////////////////////////////////////////////////////////////////////
n = 13;
x = 1:n;
emean = (n+1)/2;
md = x(7);
evar1 =  n*(n+1)/12;
sd1 = sqrt(evar1);
evar2 =  (n-1)*(n+1)/12;
sd2 = sqrt(evar2);
if  abs(mean(x) - emean)/emean > 2*%eps then, pause, end
if  ~md.equal[median(x)] then, pause, end
if  abs(var(x) - evar1)/evar1 > 2*%eps then, pause, end
if  abs(std(x) - sd1)/sd1 > 2*%eps then, pause, end
if  abs(var(x,unbiased=%f) - evar2)/evar2 > 2*%eps then, pause, end
if  abs(std(x,unbiased=%f) - sd2)/sd2 > 2*%eps then, pause, end

// verify weighted option with 1/n weights
wgtx = ones(size(x))/n;
if  abs(mean(x,weights=wgtx) - emean)/emean > 2*%eps then, pause, end
if  abs(var(x,weights=wgtx) - evar1)/evar1 > 2*%eps then, pause, end
if  abs(std(x,weights=wgtx) - sd1)/sd1 > 2*%eps then, pause, end
if  abs(var(x,weights=wgtx,unbiased=%f) - evar2)/evar2 > 2*%eps then, pause, end
if  abs(std(x,weights=wgtx,unbiased=%f) - sd2)/sd2 > 2*%eps then, pause, end

// verify skip_nan option by adding %nan at the ends of x
y = [%nan,%nan,x,%nan,%nan];
wgty = [1,1,wgtx,1,1];

if  abs(mean(y,skip_nan=%t) - emean)/emean > 2*%eps then, pause, end
if  ~md.equal[median(y,skip_nan=%t)] then, pause, end
if  abs(var(y,skip_nan=%t) - evar1)/evar1 > 2*%eps then, pause, end
if  abs(std(y,skip_nan=%t) - sd1)/sd1 > 2*%eps then, pause, end
if  abs(var(y,skip_nan=%t,unbiased=%f) - evar2)/evar2 > 2*%eps then, pause, end
if  abs(std(y,skip_nan=%t,unbiased=%f) - sd2)/sd2 > 2*%eps then, pause, end
if  abs(mean(y,weights=wgty,skip_nan=%t) - emean)/emean > 2*%eps then, pause, end
if  abs(var(y,weights=wgty,skip_nan=%t) - evar1)/evar1 > 2*%eps then, pause, end
if  abs(std(y,weights=wgty,skip_nan=%t) - sd1)/sd1 > 2*%eps then, pause, end
if  abs(var(y,weights=wgty,unbiased=%f,skip_nan=%t) - evar2)/evar2 > 2*%eps then, pause, end
if  abs(std(y,weights=wgty,unbiased=%f,skip_nan=%t) - sd2)/sd2 > 2*%eps then, pause, end

// verify trim option of mean function by adding values at both end 
// that will be discarded (so results are unchanged)
z = [x,-2,n+7];
trim_par = 1/(n+2);
if  abs(mean(z,trim=trim_par) - emean)/emean > 2*%eps then, pause, end
z = [x,0,-3,n+1,n+9];
trim_par = 2/(n+4);
if  abs(mean(z,trim=trim_par) - emean)/emean > 2*%eps then, pause, end
// verify trim + skip_nan
z = [%nan,z,%nan,%nan];
if  abs(mean(z,trim=trim_par,skip_nan=%t) - emean)/emean > 2*%eps then, pause, end


//////////////////////////////////////////////////////////////////////////
// 2/ add 2 others simple examples to the first to test with dim=1 and 2
//    2,4,...,2n  so mean = n+1, var = n(n+1)/3 (unbiased) or (n-1)(n+1)/3 (biased) 
//    3,6,...,3n  so  mean is 3*(n+1)/2, var = 3*n(n+1)/4 (unbiased) or 3*(n-1)(n+1)/4 (biased) 
//////////////////////////////////////////////////////////////////////////
// test dim = 2
x = [x;2*x;3*x];
emean = (n+1)*[0.5; 1 ; 1.5];
md = x(:,7);
evar1 =  n*(n+1)*[1/12; 1/3; 3/4];
sd1 = sqrt(evar1);
evar2 =  (n-1)*(n+1)*[1/12; 1/3; 3/4];
sd2 = sqrt(evar2);

if  max(abs(mean(x,dim=2) - emean)./emean) > 2*%eps then, pause, end
if  ~md.equal[median(x,dim=2)] then, pause, end
if  max(abs(var(x,dim=2) - evar1)./evar1) > 2*%eps then, pause, end
if  max(abs(std(x,dim=2) - sd1)./sd1) > 2*%eps then, pause, end
if  max(abs(var(x,dim=2,unbiased=%f) - evar2)./evar2) > 2*%eps then, pause, end
if  max(abs(std(x,dim=2,unbiased=%f) - sd2)./sd2) > 2*%eps then, pause, end

// verify weighted option with 1/n weights
wgtx = ones(size(x))/n;
if  max(abs(mean(x,dim=2,weights=wgtx) - emean)./emean) > 4*%eps then, pause, end
if  max(abs(var(x,dim=2,weights=wgtx) - evar1)./evar1) > 4*%eps then, pause, end
if  max(abs(std(x,dim=2,weights=wgtx) - sd1)./sd1) > 4*%eps then, pause, end
if  max(abs(var(x,dim=2,unbiased=%f,weights=wgtx) - evar2)./evar2) > 4*%eps then, pause, end
if  max(abs(std(x,dim=2,unbiased=%f,weights=wgtx) - sd2)./sd2) > 4*%eps then, pause, end

// verify skip_nan option by adding %nan
col_nan = %nan*ones(3,1);
col_ones = ones(3,1);
y = [col_nan,x(:,1:4),col_nan,col_nan,x(:,5:$),col_nan];
wgty = [col_ones,wgtx(:,1:4),col_ones,col_ones,wgtx(:,5:$),col_ones];
if  max(abs(mean(y,dim=2,skip_nan=%t) - emean)./emean) > 2*%eps then, pause, end
if  ~md.equal[median(y,dim=2,skip_nan=%t)] then, pause, end
if  max(abs(var(y,dim=2,skip_nan=%t) - evar1)./evar1) > 2*%eps then, pause, end
if  max(abs(std(y,dim=2,skip_nan=%t) - sd1)./sd1) > 2*%eps then, pause, end
if  max(abs(var(y,dim=2,unbiased=%f,skip_nan=%t) - evar2)./evar2) > 2*%eps then, pause, end
if  max(abs(std(y,dim=2,unbiased=%f,skip_nan=%t) - sd2)./sd2) > 2*%eps then, pause, end
if  max(abs(mean(y,dim=2,weights=wgty,skip_nan=%t) - emean)./emean) > 4*%eps then, pause, end
if  max(abs(var(y,dim=2,weights=wgty,skip_nan=%t) - evar1)./evar1) > 4*%eps then, pause, end
if  max(abs(std(y,dim=2,weights=wgty,skip_nan=%t) - sd1)./sd1) > 4*%eps then, pause, end
if  max(abs(var(y,dim=2,unbiased=%f,weights=wgty,skip_nan=%t) - evar2)./evar2) > 4*%eps then, pause, end
if  max(abs(std(y,dim=2,unbiased=%f,weights=wgty,skip_nan=%t) - sd2)./sd2) > 4*%eps then, pause, end

// verify trim option of mean function by adding values at both end 
// that will be discarded
z = [x(:,1:4),-2*ones(3,1),x(:,5:$),4*(n+7)*ones(3,1)];
trim_par = 1/(n+2);
if  max(abs(mean(z,dim=2,trim=trim_par) - emean)./emean) > 2*%eps then, pause, end
z = [-5*ones(3,2),5*(n+9)*ones(3,2),z];
trim_par = 3/(n+6);
if  max(abs(mean(z,dim=2,trim=trim_par) - emean)./emean) > 2*%eps then, pause, end
// verify trim + skip_nan
zz = [col_nan,z];
if  max(abs(mean(z,2,trim=trim_par,skip_nan=%t) - emean)./emean) > 2*%eps then, pause, end

// same tests with dim=1
x = x';
emean = emean';
md = md';
evar1 = evar1';
sd1 = sd1';
evar2 = evar2';
sd2 = sd2';
wgtx = wgtx';
y = y';
wgty = wgty';
z = z'; zz = zz';

if  max(abs(mean(x,dim=1) - emean)./emean) > 2*%eps then, pause, end
if  ~md.equal[median(x,dim=1)] then, pause, end
if  max(abs(var(x,dim=1) - evar1)./evar1) > 2*%eps then, pause, end
if  max(abs(std(x,dim=1) - sd1)./sd1) > 2*%eps then, pause, end
if  max(abs(var(x,1,unbiased=%f) - evar2)./evar2) > 2*%eps then, pause, end
if  max(abs(std(x,1,unbiased=%f) - sd2)./sd2) > 2*%eps then, pause, end
if  max(abs(mean(x,dim=1,weights=wgtx) - emean)./emean) > 4*%eps then, pause, end
if  max(abs(var(x,1,weights=wgtx) - evar1)./evar1) > 4*%eps then, pause, end
if  max(abs(std(x,1,weights=wgtx) - sd1)./sd1) > 4*%eps then, pause, end
if  max(abs(var(x,dim=1,unbiased=%f,weights=wgtx) - evar2)./evar2) > 4*%eps then, pause, end
if  max(abs(std(x,dim=1,unbiased=%f,weights=wgtx) - sd2)./sd2) > 4*%eps then, pause, end

if  max(abs(mean(y,dim=1,skip_nan=%t) - emean)./emean) > 2*%eps then, pause, end
if  ~md.equal[median(y,dim=1,skip_nan=%t)] then, pause, end
if  max(abs(var(y,1,skip_nan=%t) - evar1)./evar1) > 2*%eps then, pause, end
if  max(abs(std(y,1,skip_nan=%t) - sd1)./sd1) > 2*%eps then, pause, end
if  max(abs(var(y,dim=1,unbiased=%f,skip_nan=%t) - evar2)./evar2) > 2*%eps then, pause, end
if  max(abs(std(y,dim=1,unbiased=%f,skip_nan=%t) - sd2)./sd2) > 2*%eps then, pause, end
if  max(abs(mean(y,dim=1,weights=wgty,skip_nan=%t) - emean)./emean) > 4*%eps then, pause, end
if  max(abs(var(y,dim=1,weights=wgty,skip_nan=%t) - evar1)./evar1) > 4*%eps then, pause, end
if  max(abs(std(y,dim=1,weights=wgty,skip_nan=%t) - sd1)./sd1) > 4*%eps then, pause, end
if  max(abs(var(y,dim=1,unbiased=%f,weights=wgty,skip_nan=%t) - evar2)./evar2) > 4*%eps then, pause, end
if  max(abs(std(y,dim=1,unbiased=%f,weights=wgty,skip_nan=%t) - sd2)./sd2) > 4*%eps then, pause, end
if  max(abs(mean(z,1,trim=trim_par) - emean)./emean) > 2*%eps then, pause, end
if  max(abs(mean(z,dim=1,trim=trim_par,skip_nan=%t) - emean)./emean) > 2*%eps then, pause, end


//////////////////////////////////////////////////////////////////////////
// 3/ test  weighted option in general (using binomial distributions)
//////////////////////////////////////////////////////////////////////////
n = 18;
m = 12;
p = linspace(0.1,0.9,m);
x = (0:n)'*ones(1,m);
wgt = zeros(n+1,m);
for k=1:m
   wgt(:,k) = pdf("bin",(0:n)',n,p(k));
end

// test with dim=1
emean = n*p;
evar = n*p.*(1-p);
sd = sqrt(evar);
if  max(abs(mean(x,dim=1,weights=wgt) - emean)./emean) > 10*%eps then, pause, end
if  max(abs(var(x,dim=1,unbiased=%f,weights=wgt) - evar)./evar) > 20*%eps then, pause, end
if  max(abs(std(x,dim=1,unbiased=%f,weights=wgt) - sd)./sd) > 20*%eps then, pause, end

// verify skip_nan option adding some %nan
row_nan = %nan*zeros(1,m);
y = [row_nan; x(1:5,:); row_nan; row_nan; x(6:$,:)];
row_ones = ones(1,m);
wgty = [row_ones; wgt(1:5,:); row_ones; row_ones;wgt(6:$,:)];
if  max(abs(mean(y,dim=1,weights=wgty,skip_nan=%t) - emean)./emean) > 10*%eps then, pause, end
if  max(abs(var(y,dim=1,unbiased=%f,weights=wgty,skip_nan=%t) - evar)./evar) > 20*%eps then, pause, end
if  max(abs(std(y,dim=1,unbiased=%f,weights=wgty,skip_nan=%t) - sd)./sd) > 20*%eps then, pause, end

// test with dim=2
emean.redim[-1,1];
evar.redim[-1,1];
sd.redim[-1,1];
x = x';
wgt = wgt';
if  max(abs(mean(x,dim=2,weights=wgt) - emean)./emean) > 10*%eps then, pause, end
if  max(abs(var(x,dim=2,unbiased=%f,weights=wgt) - evar)./evar) > 20*%eps then, pause, end
if  max(abs(std(x,dim=2,unbiased=%f,weights=wgt) - sd)./sd) > 20*%eps then, pause, end

// verify skip_nan option adding some %nan
y = y';
wgty = wgty';
if  max(abs(mean(y,dim=2,weights=wgty,skip_nan=%t) - emean)./emean) > 10*%eps then, pause, end
if  max(abs(var(y,dim=2,unbiased=%f,weights=wgty,skip_nan=%t) - evar)./evar) > 20*%eps then, pause, end
if  max(abs(std(y,dim=2,unbiased=%f,weights=wgty,skip_nan=%t) - sd)./sd) > 20*%eps then, pause, end

