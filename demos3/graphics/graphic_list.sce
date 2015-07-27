//---------------------------------------------------
// Copyright (C) 2004-2015 Jean-Philippe Chancelier Cermics/Enpc
// jpc@cermics.enpc.fr
// NSP  graphic demo
//---------------------------------------------------

// gtk-widget for demos

exec('NSP/demos3/graphics/main_graphic.sci');

// demos are coded in libdemo
add_lib('NSP/demos3/graphics/libdemo');

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
demo_2d_2_info='plot2d modes';
demo_2d_3_info='plot2d log scales';
demo_2d_4_info='stem';
demo_2d_5_info='plot2d with function';
demo_2d_6_info='histplot';
demo_2d_7_info='marks';
demo_2d_8_info='hinton';
demo_2d_9_info='xarcs';
demo_2d_10_info='vector field';
demo_2d_11_info='scatter plot';
demo_2d_12_info='histplot';
demo_2d_13_info='table';

// organize the previous list for graphic demo widget
graphic_test_2d = build_demo_list("demo_2d",13);

// a sublist for 3d plots
// -----------------------

demo_3d_1_info="param3d";
demo_3d_2_info="plot3d and plot3d1";
demo_3d_3_info="plot3d with function";
demo_3d_4_info="multiple colormaps";
demo_3d_5_info="interpolated shading";
demo_3d_6_info="genfact and interp. shad.";
demo_3d_7_info="Tree";
demo_3d_8_info="parametric surface nf3d";
demo_3d_9_info="Cube";
demo_3d_10_info="Tube";
demo_3d_11_info="Shell";
demo_3d_12_info="plot3d and contours";
demo_3d_13_info="some surfaces";
demo_3d_14_info="tobedone";
demo_3d_15_info="tobedone";

graphic_test_3d = build_demo_list("demo_3d",13);

// ----------------------------
// organize the previous list  for graphic demo widget

graphic_test_prim = list()
for i=1:4
  graphic_test_prim(i) = list(sprintf("test%d",i), "not-used",sprintf("demo_prim_%d",i));
end

//-------------------------------
// animations
//-------------------------------

demo_anim_1_info="Clock";
demo_anim_2_info="Cycloid";
demo_anim_3_info="Hypotrochoid";
demo_anim_4_info="Hypotrochoid variation";
demo_anim_5_info="Matplot";
demo_anim_6_info="plot3d1";
demo_anim_7_info="plot3d1";
demo_anim_8_info="champ1";
demo_anim_9_info="param3d1";
demo_anim_10_info="contours";
demo_anim_11_info="Hinton";
demo_anim_12_info="Vector field and ode";
demo_anim_13_info="lorenz";
demo_anim_14_info="contours";

graphic_test_anim =build_demo_list("demo_anim",14);

// ------------------------------
// contours: Matplot Matplot1 fec grayplot
// ------------------------------

demo_contour_1_info="grayplot";
demo_contour_2_info="fec";
demo_contour_3_info="grayplot+contour";
demo_contour_4_info="contour";
demo_contour_5_info="contourf";
demo_contour_6_info="contourf";

graphic_test_contour = build_demo_list("demo_contour",6);

// organize the previous list
//------------------------------

graphic_demos_all = list( list("primitives", "", "", graphic_test_prim  ),
                          list("2D curves", "", "", graphic_test_2d  ),
			  list("2D contours", "", "", graphic_test_contour  ),
                          list("3D curves and surfaces",  "", "", graphic_test_3d ),
			  list("Animations",  "", "", graphic_test_anim ));

graphics_demo_in_gtk(graphic_demos_all,opengl);
