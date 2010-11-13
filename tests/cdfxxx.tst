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

// pe and qe computed using Maple (Digits=30)
pe = [0.0000999999999999999337805059354793;
      0.00100000000000000948098250000001;
      0.00999999999999996648061629900047;
      0.100000000000000022206919859078;
      0.249999999999999964397015164199;
      0.500000000000000013352188734267;
      0.750000000000000003623925216819;
      0.900000000000000031137075181069;
      0.990000000000000044168765732696;
      0.999000000000000076305242148685;
      0.999900000000000010654587624996];

qe = [0.999900000000000000066219494065;
      0.998999999999999990519017500000;
      0.990000000000000033519383701000;
      0.899999999999999977793080140922;
      0.750000000000000035602984835801;
      0.499999999999999986647811265733;
      0.249999999999999996376074783181;
      0.099999999999999968862924818931;
      0.009999999999999955831234267304;
      0.000999999999999923694757851315;
      0.000099999999999989345412375004];

v = ones(size(xe));
[p,q] = cdf("f",xe,ae,be);
[pp,qq] = cdff("PQ",xe,ae*v,be*v);
if ~p.equal[pp] || ~q.equal[qq] then, pause, end
erp = max( abs(p-pe)./pe );
erq = max( abs(q-qe)./qe );
if erp > 1e-15 then, pause, end
if erq > 1e-15 then, pause, end
x = icdf("f",pe,ae,be,Q=qe);
xx = cdff("F",ae*v,be*v,pe,qe);
if ~x.equal[xx] then, pause, end
erx = max( abs(x-xe)./xe );
if erx > 3e-15 then, pause, end
a = cdff("Dfn",be*v,pe,qe,xe);
era = max( abs(a-ae)/ae );
if era > 2e-11 then, pause, end
b = cdff("Dfd",pe,qe,xe,ae*v);
erb = max( abs(b-be)/be );
if erb > 5e-15 then, pause, end

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

/////////////////////////////

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
// pe and qe computed using Maple (Digits=30)
pe = [0.000100000000000037356859514025778;
      0.00100000000000006876550136910717;
      0.0100000000000011215019054042710;
      0.100000000000010857552034551343;
      0.249999999999918815933500998595;
      0.499999999999948672390308797696;
      0.749999999999973149932264147060;
      0.899999999999989792175442413569;
      0.989999999999998803515368741224;
      0.998999999999999880738609669402;
      0.999900000000000000905006799924];

qe = [0.999899999999999962643140485974;
      0.998999999999999931234498630893;
      0.989999999999998878498094595729;
      0.899999999999989142447965448657;
      0.750000000000081184066499001405;
      0.500000000000051327609691202304;
      0.250000000000026850067735852940;
      0.100000000000010207824557586431;
      0.010000000000001196484631258776;
      0.001000000000000119261390330598;
      0.000099999999999999094993200076];

me = 5/3;
sde = me*sqrt(335/166);
[m,sd] = dist_stat("f",ae,be);
erm = abs(m-me)./me; ersd = abs(sd-sde)/sde;
if erm > 2e-16 then, pause, end
if ersd > 2e-16 then, pause, end

v = ones(size(xe));
[p,q] = cdf("f",xe,ae,be);
[pp,qq] = cdff("PQ",xe,ae*v,be*v);
if ~p.equal[pp] || ~q.equal[qq] then, pause, end
erp = max( abs(p-pe)./pe );
erq = max( abs(p-pe)./pe );
if erp > 1e-15 then, pause, end
if erq > 1e-15 then, pause, end
x = icdf("f",pe,ae,be,Q=qe);
xx = cdff("F",ae*v,be*v,pe,qe);
if ~x.equal[xx] then, pause, end
erx = max( abs(x-xe)./xe );
if erx > 3e-15 then, pause, end
a = cdff("Dfn",be*v,pe,qe,xe);  // one a is another solution of the equation (we test this here after)
era =  abs(a-ae)/ae;
ind = find(era > 1e-9); // detect other solution
[pp,qq] = cdff("PQ",xe(ind),a(ind),be*ones(size((ind))));  // verify error on p for the other solution
era(ind) = abs(pp-pe(ind));
era = max( era );
if era > 1e-12 then, pause, end
b = cdff("Dfd",pe,qe,xe,ae*v);
erb = max( abs(b-be)/be );
if erb > 1e-15 then, pause, end

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
if erl > 5e-15 then, pause, end

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
if erl > 1e-14 then, pause, end

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




/////////////////////////////////////////////////////////
// tests for binomial distribution
/////////////////////////////////////////////////////////

Ne = 8;
pre = 0.5;
qre = 0.5;

me = 4;
sde = sqrt(2);
[m,sd] = dist_stat("bin",Ne,pre);
if ~(m == me && sd == sde) then, pause, end

xe = (0:7)';
pe = [0.003906250;
      0.035156250;
      0.14453125;
      0.36328125;
      0.63671875;
      0.85546875;
      0.96484375;
      0.99609375];

qe =[0.99609375;
     0.96484375;
     0.85546875;
     0.63671875;
     0.36328125;
     0.14453125;
     0.03515625;
     0.00390625];		       

v = ones(size(xe));
[p,q] = cdf("bin",xe,Ne,pre);
[pp,qq] = cdfbin("PQ",xe,Ne*v,pre*v,qre*v);
if ~(p.equal[pp] && q.equal[qq]) then, pause, end
erp = max( abs(p-pe)./pe );
erq =  max( abs(q-qe)./qe );
if erp > 1e-14 || erq > 1e-14 then, pause, end
x = cdfbin("S",Ne*v,pre*v,qre*v,pe,qe);
erx = max( abs(x-xe)./(xe+1e-6) );
if erx > 1e-14 then, pause, end
// a test for icdf...
xx = icdf("bin",p,Ne,pre,Q=q);
if ~xx.equal[xe] then, pause, end

N = cdfbin("Xn",pre*v,qre*v,pe,qe,xe);
erN =  max(abs(N-Ne)/Ne);
if erN > 1e-14 then, pause, end

pr = cdfbin("PrOmpr",pe,qe,xe,Ne*v);
erp =  max(abs(pr-pre)/pre);
if erp > 1e-15 then, pause, end

// verify special values
[p,q] = cdfbin("PQ",%nan,Ne,pre,qre);
if ~(isnan(p) && isnan(q)) then, pause, end
[p,q] = cdfbin("PQ",%inf,Ne,pre,qre);
if ~ (p.equal[1] && q.equal[0]) then, pause, end
x = cdfbin("S", Ne, pre,qre, 0, 1);
if ~x.equal[0] then, pause, end
x = cdfbin("S", Ne, pre,qre, 1, 0);
if ~x.equal[Ne] then, pause, end


/////////////////////////////////////////////
Ne = 536;
pre = 0.01;
qre = 0.99;

me = 5.36;
sde = sqrt(5.36*0.99);
[m,sd] = dist_stat("bin",Ne,pre);
if ~(m == me && sd == sde) then, pause, end

xe = [0:20]';
pe = [0.0045757712397081284048;
      0.029349643810249106435;
      0.096289147978124981414;
      0.21664502415875029198;
      0.37863917063418789434;
      0.55274197048253699425;
      0.70837932186212179568;
      0.82740933085372344757;
      0.90691296059685131859;
      0.95402622266685301994;
      0.97910570661724786500;
      0.99121939675306759732;
      0.99657266890904853964;
      0.99875224514038809968;
      0.99957469706606673597;
      0.99986380137933558994;
      0.99995889187631354002;
      0.99998827218435188466;
      0.99999682907541355743;
      0.99999918551856610740;
      0.99999980081205593989];

qe = [0.99542422876029187160;
      0.97065035618975089357;
      0.90371085202187501859;
      0.78335497584124970802;
      0.62136082936581210566;
      0.44725802951746300575;
      0.29162067813787820432;
      0.17259066914627655243;
      0.093087039403148681407;
      0.045973777333146980061;
      0.020894293382752135001;
      0.0087806032469324026761;
      0.0034273310909514603608;
      0.0012477548596119003195;
      0.00042530293393326402688;
      0.00013619862066441005734;
      0.000041108123686459982769;
      0.000011727815648115337029;
      0.0000031709245864425698357;
      0.00000081448143389259992278;
      0.00000019918794406010777884];

v = ones(size(xe));
[p,q] = cdf("bin",xe,Ne,pre);
[pp,qq] = cdfbin("PQ",xe,Ne*v,pre*v,qre*v);
if ~(p.equal[pp] && q.equal[qq]) then, pause, end
erp = max( abs(p-pe)./pe );
erq = max( abs(q-qe)./qe );
if erp > 1e-14 ||  erq > 1e-14 then, pause, end
x = cdfbin("S",Ne*v,pre*v,qre*v,pe,qe);
erx = max( abs(x-xe)./(xe+1e-6) );  // x(1) not accurate. This is due to evaluation of cumbin
                                    // (cumbin calls cumbet) for s near 0 (one parameter of the beta
                                    // is 1 + s so this is not well conditionned for s < epsm)
if erx > 1e-9 then, pause, end

// a test for icdf...
xx = icdf("bin",p,Ne,pre,Q=q);
if ~xx.equal[xe] then, pause, end

N = cdfbin("Xn",pre*v,qre*v,pe,qe,xe);
erN =  max(abs(N-Ne)/Ne);
if erN > 1e-14 then, pause, end

pr = cdfbin("PrOmpr",pe,qe,xe,Ne*v);
erp =  max(abs(pr-pre)/pre);
if erp > 1e-12 then, pause, end

// verify special values
[p,q] = cdfbin("PQ",%nan,Ne,pre,qre);
if ~(isnan(p) && isnan(q)) then, pause, end
[p,q] = cdfbin("PQ",%inf,Ne,pre,qre);
if ~ (p.equal[1] && q.equal[0]) then, pause, end
x = cdfbin("S", Ne, pre,qre, 0, 1);
if ~x.equal[0] then, pause, end
x = cdfbin("S", Ne, pre,qre, 1, 0);
if ~x.equal[Ne] then, pause, end

/////////////////////////////////////////////
Ne = 321;
pre = 0.9;
qre = 0.1;

me = 288.9;
sde = sqrt(28.89);
[m,sd] = dist_stat("bin",Ne,pre);
if ~(abs(m - me)/me <= 2*%eps && abs(sd - sde)/sde <= 2*%eps) then, pause, end

xe = [0; 50; 100; 150; 175; 200; 225; 240; 250; 260; 265;
      270; 275; 280; 285; 287; 288; 289; 290; 292; 295; 300; 305; 310; 315; 320];

pe =  [1.0000000000000000000e-321; 6.5083205422645359224e-215; 4.0318956556729814777e-141; 
       1.4519404758656870831e-83; 5.8209996235426349890e-60; 9.1732452512905724320e-40; 
       3.7048966634154258164e-23; 4.4352160407343684605e-15; 1.4529234277622591707e-10; 
       0.00000081815278095305200984; 0.000030055381495567516456; 0.00066306702539178785468; 
       0.0085483416434786790044; 0.062777042072038058980; 0.25867625434535255484; 
       0.38839967991060633995; 0.46052062508314932368; 0.53463799780372117893; 
       0.60824421622966840069; 0.74406905072516894767; 0.89306050460617850209; 
       0.98837438693100647005; 0.99962756276666586587; 0.99999773819378560716; 
       0.99999999889174036302; 0.99999999999999794957];

qe = [1.0000000000000000000; 1.0000000000000000000; 1.0000000000000000000; 
      1.0000000000000000000; 1.0000000000000000000; 1.0000000000000000000; 
      1.0000000000000000000; 0.99999999999999556478; 0.99999999985470765722; 
      0.99999918184721904695; 0.99996994461850443248; 0.99933693297460821215; 
      0.99145165835652132100; 0.93722295792796194102; 0.74132374565464744516; 
      0.61160032008939366005; 0.53947937491685067632; 0.46536200219627882107; 
      0.39175578377033159931; 0.25593094927483105233; 0.10693949539382149791; 
      0.011625613068993529953; 0.00037243723333413412562; 0.0000022618062143928353918; 
      0.0000000011082596369756712815; 2.0504327506461018404e-15];

v = ones(size(xe));
// don't test if cdf("bin"... and cdfbin output the same values
// because this test case needs the option ompr= which is not implemented
// in cdf("bin"... (this is because fl(1 - fl(0.9)) is not equal to
// fl(0.1) so the ompr computed in cdf("bin" using 1-0.9 is not exactly
// equal to the ompr provided to cdfbin).

[p,q] = cdfbin("PQ",xe,Ne*v,pre*v,qre*v);
erp = max( abs(p-pe)./pe );
erq = max( abs(q-qe)./qe );
if erp > 1e-13 ||  erq > 1e-13 then, pause, end
x = cdfbin("S",Ne*v,pre*v,qre*v,pe,qe);
erx = max( abs(x-xe)./(xe+1e-6) );
if erx > 1e-14 then, pause, end

N = cdfbin("Xn",pre*v,qre*v,pe,qe,xe);
erN =  max(abs(N-Ne)/Ne);
if erN > 1e-13 then, pause, end

pr = cdfbin("PrOmpr",pe,qe,xe,Ne*v);
erp =  max(abs(pr-pre)/pre);
if erp > 1e-15 then, pause, end

// verify special values
[p,q] = cdfbin("PQ",%nan,Ne,pre,qre);
if ~(isnan(p) && isnan(q)) then, pause, end
[p,q] = cdfbin("PQ",%inf,Ne,pre,qre);
if ~ (p.equal[1] && q.equal[0]) then, pause, end
x = cdfbin("S", Ne, pre,qre, 0, 1);
if ~x.equal[0] then, pause, end
x = cdfbin("S", Ne, pre,qre, 1, 0);
if ~x.equal[Ne] then, pause, end



/////////////////////////////////////////////////////////
// tests for negative binomial distribution
/////////////////////////////////////////////////////////

re = 18;
pre = 0.5;
qre = 0.5;

me = re*qre/pre;
sde = sqrt(re*qre)/pre;
[m,sd] = dist_stat("nbn",re,pre);
if ~(m == me && sd == sde) then, pause, end

// pe and qe computed with pari-gp
xe = (0:2:52)';
pe = [0.0000038146972656250000000; 0.00020122528076171875000; 0.0021717548370361328125; 
      0.011327922344207763672; 0.037759348750114440918; 0.092466671019792556763; 
      0.18079730402678251266; 0.29830744792707264423; 0.43208312022034078836; 
      0.56603029978577978909; 0.68644871492142556235; 0.78520474607830692548; 
      0.86002188073553043068; 0.91291442636463671079; 0.94809732224794629474; 
      0.97026831237311483846; 0.98358043121786575824; 0.99123338235045754807; 
      0.99546332983020641150; 0.99771923362679588998; 0.99888372419002853866; 
      0.99946711711042828235; 0.99975145420680948329; 0.99988655880855835970; 
      0.99994926143841600570; 0.99997773275614253208; 0.99999040015379983929];


qe =[0.99999618530273437500; 0.99979877471923828125; 0.99782824516296386719; 
     0.98867207765579223633; 0.96224065124988555908; 0.90753332898020744324; 
     0.81920269597321748734; 0.70169255207292735577; 0.56791687977965921164; 
     0.43396970021422021091; 0.31355128507857443765; 0.21479525392169307452; 
     0.13997811926446956932; 0.087085573635363289213; 0.051902677752053705262; 
     0.029731687626885161535; 0.016419568782134241758; 0.0087666176495424519288; 
     0.0045366701697935885029; 0.0022807663732041100202; 0.0011162758099714613358; 
     0.00053288288957171765087; 0.00024854579319051671275; 0.00011344119144164030487;
     0.000050738561583994301434; 0.000022267243857467918975; 0.0000095998462001607141891];

v = ones(size(xe));
[p,q] = cdf("nbn",xe,re,pre);
[pp,qq] = cdfnbn("PQ",xe,re*v,pre*v,qre*v);
if ~(p.equal[pp] && q.equal[qq]) then, pause, end
erp = max( abs(p-pe)./pe );
erq =  max( abs(q-qe)./qe );
if erp > 1e-14 || erq > 1e-14 then, pause, end
x = cdfnbn("S",re*v,pre*v,qre*v,pe,qe);
erx = max( abs(x-xe)./(xe+1e-6) );
if erx > 1e-14 then, pause, end
// a test for icdf...
xx = icdf("nbn",p,re,pre,Q=q);
if ~xx.equal[xe] then, pause, end

r = cdfnbn("Xn",pre*v,qre*v,pe,qe,xe);
err =  max(abs(r-re)/re);
if err > 1e-14 then, pause, end

pr = cdfnbn("PrOmpr",pe,qe,xe,re*v);
erp =  max(abs(pr-pre)/pre);
if erp > 1e-15 then, pause, end

// verify special values
[p,q] = cdfnbn("PQ",%nan,re,pre,qre);
if ~(isnan(p) && isnan(q)) then, pause, end
[p,q] = cdfnbn("PQ",%inf,re,pre,qre);
if ~ (p.equal[1] && q.equal[0]) then, pause, end
x = cdfnbn("S", re, pre,qre, 0, 1);
if ~x.equal[0] then, pause, end
x = cdfnbn("S", re, pre,qre, 1, 0);
if ~x.equal[%inf] then, pause, end

/////////////////////////////////////////////////////////

re = 10;
pre = 1/2^5
qre = 1 - pre;

me = re*qre/pre;
sde = sqrt(re*qre)/pre;
[m,sd] = dist_stat("nbn",re,pre);
if ~(m == me && sd == sde) then, pause, end

// pe and qe computed with pari-gp
xe = (0:30:900)';
pe = [8.8817841970012523234e-16; 0.00000031814398149804616464; 0.000063373530037514412497; 
      0.0011866886497253352769; 0.0079102064520729237857; 0.029555776968738159403; 
      0.076449369054385103013; 0.15373999867736089120; 0.25809435076609119812; 
      0.37915491441249172863; 0.50381837453064821138; 0.62043809837419169193; 
      0.72124655551612268495; 0.80281273168353602276; 0.86520717699531204387; 
      0.91068807465122323402; 0.94247960669320091637; 0.96390050286194950954; 
      0.97787260411598936829; 0.98672647096284427163; 0.99219359671474432344; 
      0.99549161959737026818; 0.99743954831605920941; 0.99856815960183475050; 
      0.99921068141788634680; 0.99957062504970522758; 0.99976929861792366844; 
      0.99987746613760718812; 0.99993561520229717712; 0.99996650883495890505; 
      0.99998274287769331695];


qe = [0.99999999999999911182; 0.99999968185601850195; 0.99993662646996248559; 
      0.99881331135027466472; 0.99208979354792707621; 0.97044422303126184060; 
      0.92355063094561489699; 0.84626000132263910880; 0.74190564923390880188; 
      0.62084508558750827137; 0.49618162546935178862; 0.37956190162580830807; 
      0.27875344448387731505; 0.19718726831646397724; 0.13479282300468795613; 
      0.089311925348776765982; 0.057520393306799083628; 0.036099497138050490459; 
      0.022127395884010631710; 0.013273529037155728367; 0.0078064032852556765637; 
      0.0045083804026297318212; 0.0025604516839407905856; 0.0014318403981652494966;
      0.00078931858211365319863; 0.00042937495029477241755; 0.00023070138207633156072; 
      0.00012253386239281188212; 0.000064384797702822881737; 0.000033491165041094947095; 
      0.000017257122306683050504];


v = ones(size(xe));
[p,q] = cdf("nbn",xe,re,pre);
[pp,qq] = cdfnbn("PQ",xe,re*v,pre*v,qre*v);
if ~(p.equal[pp] && q.equal[qq]) then, pause, end
erp = max( abs(p-pe)./pe );
erq =  max( abs(q-qe)./qe );
if erp > 1e-14 || erq > 1e-14 then, pause, end
x = cdfnbn("S",re*v,pre*v,qre*v,pe,qe);
erx = max( abs(x-xe)./(xe+1e-6) );
if erx > 1e-9 then, pause, end  // 0 is not well recover
// a test for icdf...
xx = icdf("nbn",p,re,pre,Q=q);
if ~xx.equal[xe] then, pause, end

r = cdfnbn("Xn",pre*v,qre*v,pe,qe,xe);
err =  max(abs(r-re)/re);
if err > 1e-14 then, pause, end

pr = cdfnbn("PrOmpr",pe,qe,xe,re*v);
erp =  max(abs(pr-pre)/pre);
if erp > 1e-15 then, pause, end

// verify special values
[p,q] = cdfnbn("PQ",%nan,re,pre,qre);
if ~(isnan(p) && isnan(q)) then, pause, end
[p,q] = cdfnbn("PQ",%inf,re,pre,qre);
if ~ (p.equal[1] && q.equal[0]) then, pause, end
x = cdfnbn("S", re, pre,qre, 0, 1);
if ~x.equal[0] then, pause, end
x = cdfnbn("S", re, pre,qre, 1, 0);
if ~x.equal[%inf] then, pause, end

