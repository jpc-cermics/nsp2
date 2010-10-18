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

// verify special values
[p,q] = cdff("PQ",%nan,ae,be);
if ~(isnan(p) && isnan(q)) then, pause, end
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

// verify special values
[p,q] = cdff("PQ",%nan,ae,be);
if ~(isnan(p) && isnan(q)) then, pause, end
[p,q] = cdff("PQ",0,ae,be);
if ~ (p.equal[0] && q.equal[1]) then, pause, end
[p,q] = cdff("PQ",%inf,ae,be);
if ~ (p.equal[1] && q.equal[0]) then, pause, end
x = cdff("F", ae, be, 0, 1);
if ~x.equal[0] then, pause, end
x = cdff("F", ae, be, 1, 0);
if ~x.equal[%inf] then, pause, end



/////////////////////////////////////////////////////////
// tests for non central F distribution
/////////////////////////////////////////////////////////
nu1e = 3; nu2e = 6; lambdae = 34;

xe = [1.1651;
      1.8410;
      3.0354;
      5.8191;
      8.5438;      
      13.3724;
      21.7037;
      35.0580;
      91.4176;
      211.5908;
      470.0584];

// pe computed using Maple
pe = [0.00010000054183185959737;
      0.00099991428978974905606;
      0.0099995581578520163523;
      0.10000172571747423957;
      0.24999843178998739825;
      0.49999928033489761216;
      0.74999939928727204875;
      0.90000022844640534378;
      0.99000000291434569247;
      0.99900000011914222427;
      0.99990000002634554381];

// qe computed using Maple
qe = [0.999899999458168140319367589244;
      0.999000085710210250979957912247;
      0.990000441842147983689690359337;
      0.899998274282525760410809097531;
      0.750001568210012601649856235525;
      0.500000719665102387865327949496;
      0.250000600712727951227621955323;
      0.099999771553594656241825513942;
      0.009999997085654307482586083897;
      0.000999999880857775734412133257;
      0.000099999973654456185816611128];
    
// me, sde computed using Maple      
me = 18.5;
sde = 20.328551350256121976;

[m,sd] = dist_stat("nf",nu1e,nu2e,lambdae);
erm = abs(m-me)./me; ersd = abs(sd-sde)/sde;
if erm > 2e-16 then, pause, end
if ersd > 2e-16 then, pause, end

v = ones(size(xe));
[p,q] = cdf("nf",xe,nu1e,nu2e,lambdae);
[pp,qq] = cdffnc("PQ",xe,nu1e*v,nu2e*v,lambdae*v);
if ~p.equal[pp] || ~q.equal[qq] then, pause, end
erp = max( abs(p-pe)./pe );
erq = max( abs(q-qe)./qe );
if erp > 5e-15 then, pause, end
if erq > 5e-15 then, pause, end
x = icdf("nf",pe,nu1e,nu2e,lambdae,Q=qe);
xx = cdffnc("F",nu1e*v,nu2e*v,lambdae*v,pe,qe);
if ~x.equal[xx] then, pause, end
erx = max( abs(x-xe)./xe );
if erx > 5e-15 then, pause, end
nu1 = cdffnc("Dfn",nu2e*v,lambdae*v,pe,qe,xe);
ernu1 =  max(abs(nu1-nu1e)/nu1e);
if ernu1 > 5e-15 then, pause, end
nu2 = cdffnc("Dfd",lambdae*v,pe,qe,xe,nu1e*v);
ernu2 = max( abs(nu2-nu2e)/nu2e );
if ernu2 > 5e-15 then, pause, end
lambda = cdffnc("Pnonc",pe,qe,xe,nu1e*v,nu2e*v);
erl = max( abs(lambda-lambdae)/lambdae );
if erl > 2e-12 then, pause, end

// verify special values
[p,q] = cdffnc("PQ",%nan,nu1e,nu2e,lambdae);
if ~(isnan(p) && isnan(q)) then, pause, end
[p,q] = cdffnc("PQ",0,nu1e,nu2e,lambdae);
if ~ (p.equal[0] && q.equal[1]) then, pause, end
[p,q] = cdffnc("PQ",%inf,nu1e,nu2e,lambdae);
if ~ (p.equal[1] && q.equal[0]) then, pause, end 
x = cdffnc("F", nu1e, nu2e, lambdae, 0, 1); 
if ~x.equal[0] then, pause, end
x = cdffnc("F", nu1e, nu2e, lambdae, 1, 0); 
if ~x.equal[%inf] then, pause, end



// 
nu1e=56; nu2e=7; lambdae=123;
 
xe = [   0.3990;
         0.4418;
         0.4627;
         0.5187;
         0.6633;
         0.7768;
         0.8380;
         1.0255;
         1.1356;
         1.5237;
         1.8013;
         2.4294;
         3.5038;
         5.2768;
         7.9773;
        10.4598;
        18.3749;
        23.0410;
        38.1530;
        47.1177;
        76.2270;
        93.5160;
       291.4893;
       355.9132;
       565.2558];

// pe computed using Maple (Digits=20)
pe = [ 9.9801455386391872330e-8;
       5.0013065469748517264e-7;
       9.9939270369695251752e-7;
       0.0000049975400588107515396;
       0.000099946118415779266158;
       0.00050012486762930724613;
       0.00099960898485605688261;
       0.0049984777839524740955;
       0.010000255233109816017;
       0.050000302886552127947;
       0.10000772726234428612;
       0.25000859152265820791;
       0.50000651592038201852;
       0.74999724695177234416;
       0.89999980261771792754;
       0.94999941224224022427;
       0.99000003173048867618;
       0.99500002713997911784;
       0.99900000224154178328;
       0.99949999840350334544;
       0.99989999982912149180;
       0.99994999995438619251;
       0.99999900000026715824;
       0.99999950000003750363;
       0.99999989999995162389];

// qe computed using Maple (Digits=30)
qe = [0.999999900198544613608127669624;
      0.999999499869345302514827363240;
      0.999999000607296303047482475437;
      0.999995002459941189248460449163;
      0.999900053881584220733841802807;
      0.999499875132370692753874558711;
      0.999000391015143943117388412502;
      0.995001522216047525904510643132;
      0.989999744766890183982906334732;
      0.949999697113447872053033212069;
      0.899992272737655713883739362924;
      0.749991408477341792087719353907;
      0.499993484079617981482374696524;
      0.250002753048227655843314098036;
      0.100000197382282072457691080316;
      0.050000587757759775727334266354;
      0.009999968269511323819732361263;
      0.004999972860020882161274400279;
      0.000999997758458216724914364431;
      0.000500001596496654561908913956;
      0.000100000170878508202286026404;
      0.000050000045613807492448752157;
      9.99999732841763282525479e-7;
      4.99999962496366875085295e-7;
      1.00000048376105570025441e-7];
    
// computed using Maple (Digits=20)
me = 4.4750000000000000000; 
sde = 3.7389280995137273879;

[m,sd] = dist_stat("nf",nu1e,nu2e,lambdae);
erm = abs(m-me)./me; ersd = abs(sd-sde)/sde;
if erm > 2e-16 then, pause, end
if ersd > 2e-16 then, pause, end

v = ones(size(xe));
[p,q] = cdf("nf",xe,nu1e,nu2e,lambdae);
[pp,qq] = cdffnc("PQ",xe,nu1e*v,nu2e*v,lambdae*v);
if ~p.equal[pp] || ~q.equal[qq] then, pause, end
erp = max( abs(p-pe)./pe );
erq = max( abs(q-qe)./qe );
if erp > 1e-14 then, pause, end
if erq > 2e-14 then, pause, end
x = icdf("nf",pe,nu1e,nu2e,lambdae,Q=qe);
xx = cdffnc("F",nu1e*v,nu2e*v,lambdae*v,pe,qe);
if ~x.equal[xx] then, pause, end
erx = max( abs(x-xe)./xe );
if erx > 1e-14 then, pause, end
nu1 = cdffnc("Dfn",nu2e*v,lambdae*v,pe,qe,xe);
ernu1 =  max(abs(nu1-nu1e)/nu1e);
if ernu1 > 1e-14 then, pause, end
nu2 = cdffnc("Dfd",lambdae*v,pe,qe,xe,nu1e*v);
ernu2 = max( abs(nu2-nu2e)/nu2e );
if ernu2 > 1e-13 then, pause, end
lambda = cdffnc("Pnonc",pe,qe,xe,nu1e*v,nu2e*v);
erl = max( abs(lambda-lambdae)/lambdae );
if erl > 1e-9 then, pause, end

// verify special values
[p,q] = cdffnc("PQ",%nan,nu1e,nu2e,lambdae);
if ~(isnan(p) && isnan(q)) then, pause, end
[p,q] = cdffnc("PQ",0,nu1e,nu2e,lambdae);
if ~ (p.equal[0] && q.equal[1]) then, pause, end
[p,q] = cdffnc("PQ",%inf,nu1e,nu2e,lambdae);
if ~ (p.equal[1] && q.equal[0]) then, pause, end 
x = cdffnc("F", nu1e, nu2e, lambdae, 0, 1); 
if ~x.equal[0] then, pause, end
x = cdffnc("F", nu1e, nu2e, lambdae, 1, 0); 
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


// verify special values
[p,q] = cdft("PQ",%nan,dfe);
if ~(isnan(p) && isnan(q)) then, pause, end
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

// verify special values
[p,q] = cdfnor("PQ",%nan,me,sde);
if ~(isnan(p) && isnan(q)) then, pause, end
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

// verify special values
[p,q] = cdfpoi("PQ",%nan,lambdae);
if ~(isnan(p) && isnan(q)) then, pause, end
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

// pe computed using Maple
pe = [0.000099999999594986546240;
      0.00099999999959594695746;
      0.0099999999995934392610;
      0.099999999999630130485;
      0.24999999999969304242;
      0.49999999999979688394;
      0.74999999999989862944;
      0.89999999999995863801;
      0.98999999999999597511];

// me and sde computed using Maple
me = 46.300171811169412089;
sde = 6.6613880127337848116;

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

// verify special values
[p,q] = cdftnc("PQ",%nan,dfe,pnonce);
if ~(isnan(p) && isnan(q)) then, pause, end
[p,q] = cdftnc("PQ",-%inf,dfe,pnonce);
if ~ (p.equal[0] && q.equal[1]) then, pause, end
[p,q] = cdftnc("PQ",%inf,dfe,pnonce);
if ~ (p.equal[1] && q.equal[0]) then, pause, end

x = cdftnc("T", dfe, pnonce, 0, 1);
if ~x.equal[-%inf] then, pause, end
x = cdftnc("T", dfe, pnonce, 1, 0);
if ~x.equal[%inf] then, pause, end

//////////////////////////
// pe values computed using Maple
dfe = 7;
pnonce = 75;
xe = [29.2493;
      30.4882;
      31.0763;
      32.5970;
      33.3308;
      35.2681;
      36.2255;
      38.8331;
      40.1702;
      44.0098;
      46.1140;
      52.8684;
      57.2110;
      65.9899;
      78.7686;
      117.9237;
      178.3409;
      256.6190;
      362.4879;
      507.7959;
      708.4979;
      986.5372];
                              
pe = [9.9996109330776735181e-8;  
      4.9999000252556015733e-7;
      9.9996736778565225059e-7 ; 
      0.0000050002229277854506845;
      0.0000099995874784089372765;
      0.000049998132509900910249;
      0.000099996960260647410767;
      0.00050001232012242964714;
      0.0010000176568062961334;
      0.0050000810566692108897;
      0.010000062923852941918;
      0.050000287599388245389;
      0.10000037779563279342;
      0.25000020062720916830;
      0.49999913872043005991;
      0.90000013692779479446;
      0.99000001414221882558;
      0.99900000009706683499;
      0.99990000006146782047;
      0.99998999999640738359;
      0.99999900000026562696;
      0.99999989999999306446];
      
me = 84.440164912895042512;
sde = 27.317733242036919736;

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
if erp > 5e-06 then, pause, end

x = icdf("nt",pe,dfe,pnonce);
xx = cdftnc("T",dfe*v,pnonce*v,pe,1-pe);
if ~x.equal[xx] then, pause, end
erx = max( abs(x-xe)./(abs(xe)+1e-6));
if erx > 1e-7 then, pause, end

df = cdftnc("Df",pnonce*v,pe,1-pe,xe);
erdf =  max(abs(df-dfe)/dfe);
if erdf > 1e-6 then, pause, end

pnonc = cdftnc("Pnonc",pe,1-pe,xe,dfe*v);
erpn =  max(abs(pnonc-pnonce)/pnonce);
if erpn > 1e-6 then, pause, end

// verify special values
[p,q] = cdftnc("PQ",%nan,dfe,pnonce);
if ~(isnan(p) && isnan(q)) then, pause, end
[p,q] = cdftnc("PQ",-%inf,dfe,pnonce);
if ~ (p.equal[0] && q.equal[1]) then, pause, end
[p,q] = cdftnc("PQ",%inf,dfe,pnonce);
if ~ (p.equal[1] && q.equal[0]) then, pause, end

x = cdftnc("T", dfe, pnonce, 0, 1);
if ~x.equal[-%inf] then, pause, end
x = cdftnc("T", dfe, pnonce, 1, 0);
if ~x.equal[%inf] then, pause, end






/////////////////////////////////////////////////////////
// tests for Chi2 distribution
/////////////////////////////////////////////////////////

nue = 120;
me = nue;
sde = sqrt(2*nue);

xe = [70.7281048145734133;
      77.7551404253091079;
      86.9232796553538947;
      100.6236313238458138;
      109.2196641157778885;
      119.3339957817558314;
      130.0545935689990529;
      140.2325689567917948;
      158.9501658973062206;
      173.6174364561603340;
      186.3259854458229654 ]

// pe computed using pari-gp
pe = [0.000100000000000000093;
      0.000999999999999998722;
      0.00999999999999999842;
      0.0999999999999999957;
      0.249999999999999999;
      0.499999999999999758;
      0.749999999999999869;
      0.900000000000000071;
      0.989999999999999988;
      0.999000000000000031;
      0.999900000000000011];

[m,sd] = dist_stat("chi",nue);
erm = abs(m-me)/me; ersd = abs(sd-sde)/sde;
if erm > 1e-16 then, pause, end
if ersd > 1e-16 then, pause, end

// usual tests
v = ones(size(xe));
p = cdf("chi",xe,nue);
pp = cdfchi("PQ",xe,nue*v);
if ~p.equal[pp] then, pause, end
erp = max( abs(p-pe)./pe );
if erp > 4e-15 then, pause, end

x = icdf("chi",pe,nue);
xx = cdfchi("X",nue*v,pe,1-pe);
if ~x.equal[xx] then, pause, end
erx = max( abs(x-xe)./(abs(xe)+1e-6));
if erx > 2e-15 then, pause, end

nu = cdfchi("Df",pe,1-pe,xe);
ernu =  max(abs(nu-nue)/nue);
if ernu > 4e-15 then, pause, end

// verify special values
[p,q] = cdfchi("PQ",%nan,nue);
if ~(isnan(p) && isnan(q)) then, pause, end
[p,q] = cdfchi("PQ",-%inf,nue);
if ~ (p.equal[0] && q.equal[1]) then, pause, end
[p,q] = cdfchi("PQ",%inf,nue);
if ~ (p.equal[1] && q.equal[0]) then, pause, end

x = cdfchi("X", nue, 0, 1);
if ~x.equal[0] then, pause, end
x = cdfchi("X", nue, 1, 0);
if ~x.equal[%inf] then, pause, end


/////////////////////////////////////////////////////////
// tests for Non central Chi2 distribution
/////////////////////////////////////////////////////////

nue = 70;
lambdae = 56;

me = 126;
sde = 2*sqrt(91);


xe = [   66.1489524252418022;
         74.5069462816052663;
         85.5261440109413797;
        102.1702072508162757;
        112.6919073952190473;
        125.1277668761197077;
        138.3577139037065535;
        150.9512945466967437;
        174.1649991964322908;
        192.3820748917787853;
        208.1733961233065600 ];

// pe computed using pari-gp
pe = [0.000100000000000002419;
      0.000999999999999987420;
      0.00999999999999981349;
      0.100000000000002034;
      0.250000000000000586;
      0.500000000000000690;
      0.749999999999999432;
      0.899999999999999889;
      0.989999999999999945;
      0.998999999999999998;
      0.999900000000000010];

[m,sd] = dist_stat("nch",nue,lambdae);
erm = abs(m-me)/me; ersd = abs(sd-sde)/sde;
if erm > 1e-16 then, pause, end
if ersd > 1e-16 then, pause, end

// usual tests
v = ones(size(xe));
p = cdf("nch",xe,nue,lambdae);
pp = cdfchn("PQ",xe,nue*v,lambdae*v);
if ~p.equal[pp] then, pause, end
erp = max( abs(p-pe)./pe );
if erp > 4e-15 then, pause, end

x = icdf("nch",pe,nue,lambdae);
xx = cdfchn("X",nue*v,lambdae*v,pe,1-pe);
if ~x.equal[xx] then, pause, end
erx = max( abs(x-xe)./(abs(xe)+1e-6));
if erx > 2e-15 then, pause, end

nu = cdfchn("Df",lambdae*v,pe,1-pe,xe);
ernu =  max(abs(nu-nue)/nue);
if ernu > 2e-15 then, pause, end

lambda = cdfchn("Pnonc",pe,1-pe,xe,nue*v);
erpn =  max(abs(lambda-lambdae)/lambdae);
if erpn > 1e-15 then, pause, end

// verify special values
[p,q] = cdfchn("PQ",%nan,nue,lambdae);
if ~(isnan(p) && isnan(q)) then, pause, end
[p,q] = cdfchn("PQ",-%inf,nue,lambdae);
if ~ (p.equal[0] && q.equal[1]) then, pause, end
[p,q] = cdfchn("PQ",%inf,nue,lambdae);
if ~ (p.equal[1] && q.equal[0]) then, pause, end

x = cdfchn("X", nue, lambdae, 0, 1);
if ~x.equal[0] then, pause, end
x = cdfchn("X", nue, lambdae, 1, 0);
if ~x.equal[%inf] then, pause, end



/////////////////////////////////////////////////////////
// tests for gamma distribution
/////////////////////////////////////////////////////////

ae = 9;
be = 3;

me = 3;
sde = 1;

xe = [0.5925275476044988;
      0.8174748014545915;
      1.1691518168620967;
      1.8108226860848100;
      2.2792150583997159;
      2.8896503947901238;
      3.6008149659546906;
      4.3315705137728688;
      5.8008842891175121;
      7.0520660552799939;
      8.1982324119944714];

// pe computed using pari-gp
pe = [0.00009999999999999985459899033541;
      0.0009999999999999988053036093811;
      0.009999999999999997530552926664;
      0.09999999999999992948214190407;
      0.2500000000000002015233835064;
      0.4999999999999998977461755023;
      0.7499999999999989828246852614;
      0.9000000000000000286438503030;
      0.9899999999999999992380234867;
      0.9989999999999999991545865709;
      0.9999000000000000109395012255];

[m,sd] = dist_stat("gam",ae,be);
erm = abs(m-me)/me; ersd = abs(sd-sde)/sde;
if erm > 1e-16 then, pause, end
if ersd > 1e-16 then, pause, end

// usual tests
v = ones(size(xe));
p = cdf("gam",xe,ae,be);
pp = cdfgam("PQ",xe,ae*v,be*v);
if ~p.equal[pp] then, pause, end
erp = max( abs(p-pe)./pe );
if erp > 1e-15 then, pause, end

x = icdf("gam",pe,ae,be);
xx = cdfgam("X",ae*v,be*v,pe,1-pe);
if ~x.equal[xx] then, pause, end
erx = max( abs(x-xe)./(abs(xe)+1e-6));
if erx > 2e-15 then, pause, end

a = cdfgam("Shape",be*v,pe,1-pe,xe);
era =  max(abs(a-ae)/ae);
if era > 2e-15 then, pause, end

b = cdfgam("Rate",pe,1-pe,xe,ae*v);
erb =  max(abs(b-be)/be);
if erb > 1e-15 then, pause, end

// verify special values
[p,q] = cdfgam("PQ",%nan,ae,be);
if ~(isnan(p) && isnan(q)) then, pause, end
[p,q] = cdfgam("PQ",-%inf,ae,be);
if ~ (p.equal[0] && q.equal[1]) then, pause, end
[p,q] = cdfgam("PQ",%inf,ae,be);
if ~ (p.equal[1] && q.equal[0]) then, pause, end

x = cdfgam("X", ae, be, 0, 1);
if ~x.equal[0] then, pause, end
x = cdfgam("X", ae, be, 1, 0);
if ~x.equal[%inf] then, pause, end


/////////////////////////////////////////////////////////
// tests for beta distribution
/////////////////////////////////////////////////////////

ae = 3;
be = 89;

me = 3/92;
sde = 0.018417315549325240399681975137769141607692599874;

xe = [0.0009570936718572668;
      0.0021148892705262021;
      0.0048334485237759000;
      0.0121711382874959088;
      0.0190103272382486504;
      0.0292767061547196498;
      0.0426283106842147985;
      0.0574276183818364094;
      0.0891812263448390485;
      0.1173131614184024230;
      0.1434015292172172717];

// pe computed using wolfram-alpha
pe = [0.000100000000000001372;
      0.001000000000000015092;
      0.01000000000000012744;
      0.09999999999999903347;
      0.2500000000000022625;
      0.4999999999999970126;
      0.7500000000000677608;
      0.89999999999996934669;
      0.990000000000000022441;
      0.99899999999999962216975;
      0.9999000000000000501193];

[m,sd] = dist_stat("bet",ae,be);
erm = abs(m-me)/me; ersd = abs(sd-sde)/sde;
if erm > 1e-16 then, pause, end
if ersd > 2e-16 then, pause, end

// usual tests
v = ones(size(xe));
p = cdf("bet",xe,ae,be);
pp = cdfbet("PQ",xe,1-xe,ae*v,be*v);
if ~p.equal[pp] then, pause, end
erp = max( abs(p-pe)./pe );
if erp > 1e-14 then, pause, end

x = icdf("bet",pe,ae,be);
xx = cdfbet("XY",ae*v,be*v,pe,1-pe);
if ~x.equal[xx] then, pause, end
erx = max( abs(x-xe)./(abs(xe)+1e-6));
if erx > 1e-13 then, pause, end

a = cdfbet("A",be*v,pe,1-pe,xe,1-xe);
era =  max(abs(a-ae)/ae);
if era > 1e-13 then, pause, end

b = cdfbet("B",pe,1-pe,xe,1-xe,ae*v);
erb =  max(abs(b-be)/be);
if erb > 1e-13 then, pause, end

// verify special values
[p,q] = cdfbet("PQ",%nan,%nan,ae,be);
if ~(isnan(p) && isnan(q)) then, pause, end
[p,q] = cdfbet("PQ",-%inf,%inf,ae,be);
if ~ (p.equal[0] && q.equal[1]) then, pause, end
[p,q] = cdfbet("PQ",0,1,ae,be);
if ~ (p.equal[0] && q.equal[1]) then, pause, end
[p,q] = cdfbet("PQ",%inf,-%inf,ae,be);
if ~ (p.equal[1] && q.equal[0]) then, pause, end
[p,q] = cdfbet("PQ",1,0,ae,be);
if ~ (p.equal[1] && q.equal[0]) then, pause, end

[x,y] = cdfbet("XY", ae, be, 0, 1);
if ~(x.equal[0] && y.equal[1]) then, pause, end
[x,y] = cdfbet("XY", ae, be, 1, 0);
if ~(x.equal[1] && y.equal[0]) then, pause, end

// a test which fails before cdfbet improvement about computing x or y
[x,y]=cdfbet("XY",1e18,3,0.4,0.6);
[p,q] = cdfbet("PQ",x,y,1e18,3);
if abs(p-0.4)/0.4 > 1e-14 then, pause, end



