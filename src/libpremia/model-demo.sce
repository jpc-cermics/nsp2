M=premia_create()
// Choose a model 
models=premia_get_models();
n=x_choose(models,['Choose a model']);
M.set_model[n];
M.get_model[]

exec(getenv('NSP')+'/src/libpremia/macros.sci');

premia_model_values(M) 


  



