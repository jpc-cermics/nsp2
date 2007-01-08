
exec(getenv('NSP')+'/src/libpremia/macros.sci');

M=premia_create()
// Choose a model 
models=premia_get_models();
n=x_choose(models,['Choose a model']);
M.set_model[n];
M.get_model[]

premia_model_values(M) 

[family,option]=select_premia_option(n);

M.set_option[family,option];

M.get_option[];

premia_option_values(M) 

meths=premia_get_methods(family,option,n);
meth=x_choose(meths,['Choose a method']);

// if meth<>0 then 



M.set_method[meth]

M.get_method[]

premia_method_values(M);

M.compute[];

L=M.get_method_results[];

premia_method_results(M);



