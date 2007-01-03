M=premia_create()
// Choose a model 
models=premia_get_models();
// n=x_choose(models,['Choose a model']);

M.set_model[1];
M.get_model[]
B=M.get_model_values[];

// change the model variables 

b=list(list("Current Date",0),...
       list("Spot",100),...
       list("Trend",0),...
       list("Volatility",0.2),...
       list("Annual Dividend Rate",0),...
       list("Annual Interest Rate",10));

if or(B<>b) then pause;end 

b=list(list("Current Date",1),...
       list("Spot",156),...
       list("Trend",0.3),...
       list("Volatility",0.98),...
       list("Annual Dividend Rate",0.123),...
       list("Annual Interest Rate",15));

M.set_model_values[b];
 
B=M.get_model_values[];

if or(B<>b) then pause;end 




M.set_option[0,0]
M.set_method[0];

[a,b]=M.get_option_values[];
[a,b]=M.get_model_values[];


xclear();

res=[];
vspot=80:1:120;
for spot=vspot
  b(2)(2)=spot;
  M.set_model_values[b];
  M.compute[]
  [a,lres]=M.get_method_res_values[];
  res=[res,lres(1)(2)];
end

plot(vspot,res);

