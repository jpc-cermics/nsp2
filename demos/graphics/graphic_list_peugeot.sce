// FIXME : this is a temporary load 
// of all the graphics macros 

NSP=getenv('SCI');
if %t then 
  A=glob(NSP+'/macros/xdess/*.sci');
  for i=1:size(A,1),exec(A(i,1));end;
  A=glob(NSP+'/macros/scicos/00util.sci');
  exec(A)
end 
exec(NSP+'/demos/graphics/main_graphic.sci');

// A set of demos 
//-------------------------

// utility function to build a list for demos 

function L=build_demo_list(str,n)
  L = list() 
  for i=1:n
    name=sprintf("%s_%d",str,i); 
    execstr(sprintf("info=%s_%d_info;",str,i)); 
    L(i) = list(info,"not-used",name);
  end 
endfunction

// demo for 2d plots 
// -----------------------

demo_2d_1_info='interpolation';

function y=demo_2d_1()
  exec(NSP+'/demos/graphics/demo-peugeot/interpolation_code.sci');
endfunction

demo_2d_2_info='ncertitude'

function y=demo_2d_2()
exec(NSP+'/demos/graphics/demo-peugeot/incertitude_code.sci');
endfunction

demo_2d_3_info='icm';

function y=demo_2d_3(info=%f)
exec(NSP+'/demos/graphics/demo-peugeot/icm_code.sci');
endfunction

demo_2d_4_info='seuil'

function y=demo_2d_4(info=%f)
exec(NSP+'/demos/graphics/demo-peugeot/seuil_code.sci');
endfunction

demo_2d_5_info='efficcacit'

function y=demo_2d_5(info=%f)
exec(NSP+'/demos/graphics/demo-peugeot/efficacite_code.sci');
endfunction

demo_2d_6_info='evolution ratio';

function y=demo_2d_6(info=%f)
  exec(NSP+'/demos/graphics/demo-peugeot/evolution_ratio_code.sci');
endfunction

demo_2d_7_info='capabilite';

function y=demo_2d_7(info=%f)
 exec(NSP+'/demos/graphics/demo-peugeot/capabilite_code.sci');
endfunction

demo_2d_8_info='diag_effets';

function y=demo_2d_8(info=%f)
  exec(NSP+'/demos/graphics/demo-peugeot/diag_effets.sci');
endfunction

demo_2d_9_info='nuages';

function y=demo_2d_9(info=%f)
  exec(NSP+'/demos/graphics/demo-peugeot/nuages_code.sci');
endfunction

demo_2d_10_info='plusieurs';

function y=demo_2d_10(info=%f)
  exec(NSP+'/demos/graphics/demo-peugeot/plusieurs.sci');
endfunction

demo_2d_11_info='pareto';

function y=demo_2d_11(info=%f)
  exec(NSP+'/demos/graphics/demo-peugeot/pareto_code.sci');
endfunction

demo_2d_12_info='isovar';

function y=demo_2d_12(info=%f)
  exec(NSP+'/demos/graphics/demo-peugeot/isovar_code.sci');
endfunction


// organize the previous list for graphic demo widget 

graphic_test_2d = build_demo_list("demo_2d",12);

graphic_demos_all = list( list("Peugeot", "", "", graphic_test_2d  ));
  
graphics_demo_in_gtk(graphic_demos_all)
  

