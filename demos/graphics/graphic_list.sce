//---------------------------------------------------
// Copyright (C) 2004 Jean-Philippe Chancelier Cermics/Enpc 
// jpc@cermics.enpc.fr 
// NSP  graphic demo 
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

// demo for 2d plots 
// -----------------------

demo_2d_1_info='plot2d';
demo_2d_2_info='plot2d log scale';
demo_2d_3_info='plot2d3';
demo_2d_4_info='plot2d superpose';
demo_2d_5_info='plot2d with function';
demo_2d_6_info='histplot';

// organize the previous list for graphic demo widget 
graphic_test_2d = build_demo_list("demo_2d",6);

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

// ----------------------------
// organize the previous list  for graphic demo widget 

graphic_test_prim = list() 
for i=1:3
  graphic_test_prim(i) = list(sprintf("test%d",i), "not-used",sprintf("demo_prim_%d",i));
end 

//-------------------------------
// animations 
//-------------------------------

// organize the previous list for graphic demo widget 
graphic_test_anim = list() 
for i=1:7
  name=sprintf("demo_anim_%d",i); 
  test_info=sprintf("test %d",i); 
  //execstr('test_info='+name+'(info=%t);');
  graphic_test_anim(i) = list(test_info, "not-used",name);
end 

// ------------------------------
// Matplot Matplot1 fec grayplot 
// ------------------------------

demo_contour_1_info="grayplot";
demo_contour_2_info="fec";
demo_contour_3_info="grayplot+contour";
demo_contour_4_info="contour";
demo_contour_5_info="contourf";
demo_contour_6_info="contourf";

// organize the previous list for graphic demo widget 
graphic_test_contour = list() 
for i=1:6
  name=sprintf("demo_contour_%d",i); 
  execstr(sprintf("info_d=%s_info;",name)); 
  graphic_test_contour(i) = list(info_d, "not-used",name);
end 

// organize the previous list 
//------------------------------

graphic_demos_all = list( list("primitives", "", "", graphic_test_prim  ), 
                          list("2D curves", "", "", graphic_test_2d  ), 
			  list("2D contours", "", "", graphic_test_contour  ), 
                          list("3D curves and surfaces",  "", "", graphic_test_3d ),
			  list("Animations",  "", "", graphic_test_anim ));
			  

graphics_demo_in_gtk(graphic_demos_all,opengl);

