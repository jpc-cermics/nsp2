//---------------------------------------------------
// Copyright (C) 2004 Jean-Philippe Chancelier Cermics/Enpc 
// jpc@cermics.enpc.fr 
// NSP  3d graphic demo in Opengl 
// 
// FIXME: this is to be changed since xsetech is not supported in OpenGL 
//--------------------------------------------------- 

exec('NSP/demos/graphics/main_graphic.sci');

// A set of demos 
// add libdemo in the function search list 

add_lib('NSP/demos/graphics/libdemo');

// utility function to build a list for demos 

function L=build_demo_list(str,n)
  L = list() 
  for i=1:n
    name=sprintf("%s_%d",str,i); 
    execstr(sprintf("info_d=%s_%d_info;",str,i)); 
    L(i) = list(info_d,"not-used",name);
  end 
endfunction

// a sublist for 3d plots 
// -----------------------

demo_3d_1_info="param3d";
demo_3d_2_info="param3d";
demo_3d_3_info="plot3d";
demo_3d_4_info="plot3d1";
demo_3d_5_info="plot3d with function";
demo_3d_6_info="multiple colormaps";
demo_3d_7_info="interpolated shading";
demo_3d_8_info="genfact and interp. shad.";
demo_3d_9_info="genfact and interp. shad.";
demo_3d_10_info="Tree";
demo_3d_11_info="parametric surface nf3d";
demo_3d_12_info="Cube";
demo_3d_13_info="Tube";
demo_3d_14_info="Shell";

// organize the previous list for graphic demo widget 
graphic_test_3d = build_demo_list("demo_3d",14);

graphic_demos_all = list( list("OpenGl 3D curves and surfaces",  "", "", graphic_test_3d ));
			  
graphics_demo_in_gtk(graphic_demos_all,%t);

