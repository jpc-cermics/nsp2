// -*- Mode: scilab -*- 
// tests for cdfxxx, cdf, icdf and dist_stat functions 
// Bruno Pincon, sept 2010

/////////////////////////////////////////////////////////
// tests for F distribution
/////////////////////////////////////////////////////////

ae = 3; be = 2;
xe =[ 0.0014393908669709057;
      0.006734006734006777;
      0.03245012703014272;
      0.18307027430256984;
      0.43864205811833523;
      1.1349429226128769;
      3.1533745288163186;
      9.161790168179738;
      99.16620137447201;
      999.1666203472842;
      9999.166662037871 ];
// pe computed using wolfram-alpha web calculator
pe =[ 0.00009999999999999993;
      0.0010000000000000095;
      0.009999999999999966;
      0.100000000000000022;
      0.24999999999999996;
      0.500000000000000013;
      0.750000000000000004;
      0.90000000000000003;
      0.990000000000000044;
      0.9990000000000000763;
      0.99990000000000001065];

v = ones(size(xe));
p = cdf("f",xe,ae,be);
pp = cdff("PQ",xe,ae*v,be*v);
if ~p.equal[pp] then, pause, end
erp = max( abs(p-pe)./pe );
if erp > 1e-15 then, pause, end
x = icdf("f",pe,ae,be);
xx = cdff("F",ae*v,be*v,pe,1-pe);
if ~x.equal[xx] then, pause, end
erx = max( abs(x-xe)./xe );
if erx > 1e-13 then, pause, end
a = cdff("Dfn",be*v,pe,1-pe,xe);
era = max( abs(a-ae)/ae );
if era > 1e-9 then, pause, end  // 2 last cases are badly conditionned so 1e-9
b = cdff("Dfd",pe,1-pe,xe,ae*v);
erb = max( abs(b-be)/be );
if erb > 1e-13 then, pause, end

// verify extreme value
[p,q] = cdff("PQ",0,ae,be);
if ~ (p.equal[0] && q.equal[1]) then, pause, end
[p,q] = cdff("PQ",%inf,ae,be);
if ~ (p.equal[1] && q.equal[0]) then, pause, end
x = cdff("F", ae, be, 0, 1);
if ~x.equal[0] then, pause, end
x = cdff("F", ae, be, 1, 0);
if ~x.equal[%inf] then, pause, end



ae = 332; be = 5;
xe = [ 0.1876507842744393; 
       0.23736470790102512;
       0.32544293001889024;
       0.5362795010681327 ;
       0.7505322715770671 ;
       1.146702555761148  ;
       1.870342232243431  ;
       3.111470441493004  ;
       9.053567303453834  ;
       23.88518499113799  ;
       61.11087818490155  ];
// pe computed using wolfram-alpha web calculator
pe = [ 0.000100000000000037357;
       0.001000000000000068765;
       0.0100000000000011215; 
       0.100000000000010857;
       0.24999999999991882;
       0.49999999999994867;
       0.74999999999997315;
       0.899999999999989792;
       0.9899999999999988035;
       0.99899999999999988074;
       0.999900000000000000905];

me = 5/3;
sde = me*sqrt(335/166);
[m,sd] = dist_stat("f",ae,be);
erm = abs(m-me)./me; ersd = abs(sd-sde)/sde;
if erm > 2e-16 then, pause, end
if ersd > 2e-16 then, pause, end

v = ones(size(xe));
p = cdf("f",xe,ae,be);
pp = cdff("PQ",xe,ae*v,be*v);
if ~p.equal[pp] then, pause, end
erp = max( abs(p-pe)./pe );
if erp > 1e-15 then, pause, end
x = icdf("f",pe,ae,be);
xx = cdff("F",ae*v,be*v,pe,1-pe);
if ~x.equal[xx] then, pause, end
erx = max( abs(x-xe)./xe );
if erx > 1e-13 then, pause, end
a = cdff("Dfn",be*v,pe,1-pe,xe);  // one a is another solution of the equation (we test this here after)
era =  abs(a-ae)/ae;
ind = find(era > 1e-9); // detect other solution
[pp,qq] = cdff("PQ",xe(ind),a(ind),be*ones(size((ind))));  // verify error on p for the other solution
era(ind) = abs(pp-pe(ind));
era = max( era );
if era > 1e-11 then, pause, end
b = cdff("Dfd",pe,1-pe,xe,ae*v);
erb = max( abs(b-be)/be );
if erb > 2e-14 then, pause, end

// verify extreme value
[p,q] = cdff("PQ",0,ae,be);
if ~ (p.equal[0] && q.equal[1]) then, pause, end
[p,q] = cdff("PQ",%inf,ae,be);
if ~ (p.equal[1] && q.equal[0]) then, pause, end
x = cdff("F", ae, be, 0, 1);
if ~x.equal[0] then, pause, end
x = cdff("F", ae, be, 1, 0);
if ~x.equal[%inf] then, pause, end




/////////////////////////////////////////////////////////
// tests for Student T distribution
/////////////////////////////////////////////////////////

dfe = 5;
xe = [0.0;
      0.7266868438004231;
      1.4758840488244815;
      3.3649299989072188;
      5.893429531356009;
      9.677566300882816];
// pe computed using wolfram-alpha web calculator
pe = [0.5;
      0.75000000000000013;
      0.9000000000000000541;
      0.9900000000000000023;
      0.9989999999999999991;
      0.9999000000000000111];


me = 0;
sde = sqrt(5/3);
[m,sd] = dist_stat("t",dfe);
if m ~= me then, pause, end
ersd = abs(sd-sde)/sde;
if ersd > 2e-16 then, pause, end

// test symmetry
x = linspace(-10,10,26);
[p,q] = cdf("t",x,dfe);
[qq,pp] = cdf("t",-x,dfe);
if ~(p.equal[pp] && q.equal[qq]) then, pause, end

// usual tests
v = ones(size(xe));
p = cdf("t",xe,dfe);
pp = cdft("PQ",xe,dfe*v);
if ~p.equal[pp] then, pause, end
erp = max( abs(p-pe)./pe );
if erp > 1e-15 then, pause, end
x = icdf("t",pe,dfe);
xx = cdft("T",dfe*v,pe,1-pe);
if ~x.equal[xx] then, pause, end
erx = max( abs(x-xe)./(abs(xe)+1e-6));
if erx > 1e-14 then, pause, end
df = cdft("Df",pe,1-pe,xe);
erdf =  max(abs(df-dfe)/dfe);
if erdf > 1e-14 then, pause, end


// verify extreme value
[p,q] = cdft("PQ",-%inf,dfe);
if ~ (p.equal[0] && q.equal[1]) then, pause, end
[p,q] = cdft("PQ",%inf,dfe);
if ~ (p.equal[1] && q.equal[0]) then, pause, end

x = cdft("T", dfe, 0, 1);
if ~x.equal[-%inf] then, pause, end
x = cdft("T", dfe, 1, 0);
if ~x.equal[%inf] then, pause, end



/////////////////////////////////////////////////////////
// tests for normal distribution
/////////////////////////////////////////////////////////
me = 0.625;
sde = 0.75;
xe = [0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2];
// pe computed using pari-gp software
pe = [0.3085375387259868963622953894, 0.4338161673890963463825581651, 0.5661838326109036536174418350,...
      0.6914624612740131036377046106, 0.7976716190363569746314664003, 0.8783274954256187431795040554,...
      0.9331927987311419339955059590, 0.9666234924151827560954251535];

[m,sd] = dist_stat("nor",me,sde);
if ~(m == me && sd == sde) then, pause, end

// test symmetry
sxe = 2*me - xe;
[p,q] = cdf("nor",xe,me,sde);
[qq,pp] = cdf("nor",sxe,me,sde);
if ~(p.equal[pp] && q.equal[qq]) then, pause, end

// usual tests
v = ones(size(xe));
pp = cdfnor("PQ",xe,me*v,sde*v);
if ~p.equal[pp] then, pause, end
erp = max( abs(p-pe)./pe );
if erp > 1e-15 then, pause, end
x = icdf("nor",pe,me,sde);
xx = cdfnor("X",me*v,sde*v,pe,1-pe);
if ~x.equal[xx] then, pause, end
erx = max( abs(x-xe)./xe );
if erx > 1e-15 then, pause, end
m = cdfnor("Mean",sde*v,pe,1-pe,xe);
erm =  max(abs(m-me)/me);
if erm > 1e-15 then, pause, end
sd = cdfnor("Std",pe,1-pe,xe,me*v);
erb = max( abs(sd-sde)/sde );
if erb > 1e-15 then, pause, end

// verify extreme value
[p,q] = cdfnor("PQ",-%inf,me,sde);
if ~ (p.equal[0] && q.equal[1]) then, pause, end
[p,q] = cdff("PQ",%inf,me,sde);
if ~ (p.equal[1] && q.equal[0]) then, pause, end
x = cdfnor("X", me, sde, 0, 1);
if ~x.equal[-%inf] then, pause, end
x = cdfnor("X", me, sde, 1, 0);
if ~x.equal[%inf] then, pause, end


/////////////////////////////////////////////////////////
// tests for Poisson distribution
/////////////////////////////////////////////////////////

lambdae = 10;
xe = 0:20;
// pe and qe computed with pari-gp
pe = [0.00004539992976248485153559151556, 0.0004993992273873333668915066712, ...
      0.002769395715511575943671082449, 0.01033605067592571786626966838,...
      0.02925268807696107267276613320, 0.06708596287903178228575906284,...
      0.1301414208824829649740806122, 0.2202206466016989402431113972,...
      0.3328196787507189093293998781, 0.4579297144718522083141648572,...
      0.5830397501929855072989298360, 0.6967761463031066881941707260,...
      0.7915564763948743389402048009, 0.8644644226193109933602310124,...
      0.9165415270653371750888211636, 0.9512595966960212962412145976,...
      0.9729583902151988719614604938, 0.9857223864029503870910169034,...
      0.9928134953961456732741037976, 0.9965456580241431923178337419,...
      0.9984117393381419518396987141];
qe = [0.99995460007023751515, 0.99950060077261266663, 0.99723060428448842406,...
      0.98966394932407428213, 0.97074731192303892733, 0.93291403712096821771,...
      0.86985857911751703503, 0.77977935339830105976, 0.66718032124928109067,...
      0.54207028552814779169, 0.41696024980701449270, 0.30322385369689331181,...
      0.20844352360512566106, 0.13553557738068900664, 0.083458472934662824911,...
      0.048740403303978703759, 0.027041609784801128039, 0.014277613597049612909,...
      0.0071865046038543267259, 0.0034543419758568076822, 0.0015882606618580481603];

me = lambdae; sde = sqrt(lambdae);
[m,sd] = dist_stat("poi",lambdae);
if ~(m == me && sd == sde) then, pause, end

v = ones(size(xe));
[p,q] = cdf("poi",xe,lambdae);
[pp,qq] = cdfpoi("PQ",xe,lambdae*v);
if ~(p.equal[pp] && q.equal[qq]) then, pause, end
erp = max( abs(p-pe)./pe );
erq =  max( abs(q-qe)./qe );
if erq > 1e-14 then, pause, end
x = cdfpoi("S",lambdae*v,pe,qe);
erx = max( abs(x-xe)./(xe+1e-6) );
if erx > 1e-14 then, pause, end
// a test for icdf...
xx = icdf("poi",p,lambdae,Q=q);
if ~xx.equal[xe] then, pause, end

lambda = cdfpoi("Xlam",pe,1-pe,xe);
erl =  max(abs(lambda-lambdae)/lambdae);
if erl > 1e-14 then, pause, end

// verify extreme value
[p,q] = cdfpoi("PQ",%inf,lambdae);
if ~ (p.equal[1] && q.equal[0]) then, pause, end
x = cdfpoi("S", lambdae, 0, 1);
if ~x.equal[0] then, pause, end
x = cdfpoi("S", lambdae, 1, 0);
if ~x.equal[%inf] then, pause, end




/////////////////////////////////////////////////////////
// tests for Non central Student T distribution
/////////////////////////////////////////////////////////

dfe = 27;
pnonce = 45;

xe = [ 29.1516022086057625;
       31.1658463768033300;
       33.9447244835852544;
       38.4743055911762255;
       41.5866711100078490;
       45.5597696443552707;
       50.1907655307224303;
       55.0462588648301363;
       65.3666211312566077];

// pe computed using wolfram-alpha web calculator
pe = [0.0000999999995950049108;
      0.000999999999595949478;
      0.00999999999959357259;
      0.0999999999996300955;
      0.2499999999996927339;
      0.499999999999796052;
      0.749999999999898612;
      0.899999999999959173;
      0.989999999999995517];

// me and sde computed using wolfram-alpha web calculator
me = 46.300171811169412087351862665217543591249;
sde = 6.661388012733784806884035370386105039214;

[m,sd] = dist_stat("nt",dfe,pnonce);
erm = abs(m-me)/me; ersd = abs(sd-sde)/sde;
if erm > 1e-15 then, pause, end
if ersd > 1e-13 then, pause, end

// usual tests
v = ones(size(xe));
p = cdf("nt",xe,dfe,pnonce);
pp = cdftnc("PQ",xe,dfe*v,pnonce*v);
if ~p.equal[pp] then, pause, end
erp = max( abs(p-pe)./pe );
if erp > 1e-08 then, pause, end   // max is 4.049946429789740e-09

x = icdf("nt",pe,dfe,pnonce);
xx = cdftnc("T",dfe*v,pnonce*v,pe,1-pe);
if ~x.equal[xx] then, pause, end
erx = max( abs(x-xe)./(abs(xe)+1e-6));
if erx > 2e-10 then, pause, end

df = cdftnc("Df",pnonce*v,pe,1-pe,xe);
erdf =  max(abs(df-dfe)/dfe);
if erdf > 1e-9 then, pause, end

pnonc = cdftnc("Pnonc",pe,1-pe,xe,dfe*v);
erpn =  max(abs(pnonc-pnonce)/pnonce);
if erpn > 1e-10 then, pause, end

// verify extreme value
[p,q] = cdftnc("PQ",-%inf,dfe,pnonce);
if ~ (p.equal[0] && q.equal[1]) then, pause, end
[p,q] = cdftnc("PQ",%inf,dfe,pnonce);
if ~ (p.equal[1] && q.equal[0]) then, pause, end

x = cdftnc("T", dfe, pnonce, 0, 1);
if ~x.equal[-%inf] then, pause, end
x = cdftnc("T", dfe, pnonce, 1, 0);
if ~x.equal[%inf] then, pause, end

