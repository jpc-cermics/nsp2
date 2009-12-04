//---------------------------------------------------
// Copyright (C) 2004 Jean-Philippe Chancelier Cermics/Enpc 
// jpc@cermics.enpc.fr 
// NSP  graphic demo 
//--------------------------------------------------- 

exec('NSP/demos/graphics/main_graphic.sci');

// A set of demos 
// add libdemo in the function search list 

// utility function to build a list for demos 

function L=build_demo_list(str,n)
  L = list() 
  for i=1:n
    name=sprintf("%s_%d",str,i); 
    execstr(sprintf("info_d=""%s_%d_info"";",str,i)); 
    L(i) = list(info_d,"not-used",name);
  end 
endfunction

exec('NSP/tests/tobedone/3dbruno-new/examples.sce');
exec('NSP/tests/tobedone/3dbruno-new/libbruno.sce');

// organize the previous list for graphic demo widget 
graphic_test_2d = build_demo_list("demo_3dlib",15);

graphic_demos_all=list(list("3d bruno", "", "", graphic_test_2d  ));
opengl=%t; 
graphics_demo_in_gtk(graphic_demos_all,opengl);

