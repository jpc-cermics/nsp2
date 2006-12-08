M=premiapb_create()
M.set_model[0]
M.set_option[0,0]
M.set_method[45];

[a,b]=M.get_option_vars[];
[a,b]=M.get_model_vars[];

b=list(list("Current Date",0),...
       list("Spot",100),...
       list("Trend",0),...
       list("Volatility",0.2),...
       list("Annual Dividend Rate",0),...
       list("Annual Interest Rate",10));

xclear();

res=[];
vspot=80:1:120;
for spot=vspot
  b(2)(2)=spot;
  M.set_model_values[b];
  M.compute[]
  [a,lres]=M.get_method_res_vars[];
  res=[res,lres(1)(2)];
end

plot(vspot,res);

