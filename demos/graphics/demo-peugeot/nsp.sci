A=glob(getenv('SCI')+'/macros/xdess/*.sci');
for i=1:size(A,'*'); exec(A(i));end

A=glob(getenv('SCI')+'/macros/scicos/00util.sci');
for i=1:size(A,'*'); exec(A(i));end

xclear();
exec('interpolation_code.sci');xset('wshow');xclick()
xclear()
exec('incertitude_code.sci');xset('wshow');xclick()
xclear()
exec('icm_code.sci');xset('wshow');xclick()
xclear()
exec('seuil_code.sci');xset('wshow');xclick()
xclear()
exec('efficacite_code.sci');xset('wshow');xclick()
xclear()
exec('evolution_ratio_code.sci');xset('wshow');xclick()
xclear()
exec('capabilite_code.sci');xset('wshow');xclick()
xclear()
exec('diag_effets.sci');xset('wshow');xclick()
xclear()
exec('nuages_code.sci');xset('wshow');xclick()
xclear()
exec('plusieurs.sci');xset('wshow');xclick()
xclear()
exec('pareto_code.sci');xset('wshow');xclick()
xclear()
exec('isovar_code.sci');xset('wshow');xclick()

