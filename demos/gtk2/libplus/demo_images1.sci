
function demo_images1() 
  window = gtkwindow_new()
  //window.connect[ "delete_event", demo_delete];
  window.set_title["Images"];
  //window.connect[  "destroy",  gtk_widget_destroyed, &window]
  //window.connect[  "destroy",	cleanup_callback, NULL]
  window.set_border_width[  8]
  vbox = gtkvbox_new(homogeneous=%f,spacing=8);
  vbox.set_border_width[  8]
  window.add[  vbox]

  function add_image(image,title,vbox)
    label = gtklabel_new();
    label.set_markup["<u>"+title+"</u>"];
    vbox.pack_start[ label,expand=%f,fill=%f,padding=0]
    frame = gtkframe_new();
    frame.set_shadow_type[GTK.SHADOW_IN];
    align = gtkalignment_new(xalign=0.5,yalign=0.5,xscale=0,yscale=0);
    align.add[  frame]
    vbox.pack_start[ align,expand=%f,fill=%f,padding=0]
    frame.add[image]
  endfunction
  
  function image=demo_image_from_pixmap() 
// create a pixmap from a file 
// then an image 
  visual = gdkvisual_new() 
  // colormap 
  cmap= gdkcolormap_new(visual,%t)
  //cmap.alloc_color[rand(30,3)*56]
  //cmap.alloc_color[["red","green","blue"]];
  None = none_create();
  // gdkpixmap_new(darea.window,67,67)
  // gdkpixmap_new(None,67,67,depth=78)

  // colormap or drawable , transparent_color or none, file or data 
  [pixmap,mask]= gdk_pixmap_create_from_xpm(cmap,None,"nsp.gif");

  image = gtkimage_new("pixmap",pixmap,mask);
  endfunction 

  add_image(demo_image_from_pixmap(),"file ->pixmap",vbox)
  
  function image=demo_image_from_pixmap_data() 
    xpm_pix = [ "     9     9        2            1";
		". c #000000";
		"# c #f8fcf8";
		".........";
		".#######.";
		".#######.";
		".#######.";
		".#.....#.";
		".#######.";
		".#######.";
		".#######.";
		"........."]
    visual = gdkvisual_new() 
    // colormap 
    cmap= gdkcolormap_new(visual,%t)
    [pixmap,mask]= gdk_pixmap_create_from_xpm(cmap,none_create(),xpm_pix);
    image = gtkimage_new("pixmap",pixmap,mask);
  endfunction
  
  add_image(demo_image_from_pixmap_data(),"data ->pixmap ",vbox)

// FIXME: need more examples with create from data 
//        and examples with bitmap_create  

   function image=demo_image_from_pixbuf() 
     pixbuf= gdk_pixbuf_new_from_file('shell.xpm')
     image = gtkimage_new("pixbuf",pixbuf);
   endfunction 
   add_image(demo_image_from_pixbuf(),"file ->pixbuf ->image",vbox)

   function image=demo_image_from_pixbuf_pixmap() 
     pixbuf= gdk_pixbuf_new_from_file('tree.gif')
     pixbuf.render_pixmap_and_mask[] // alpha_thresold]
     [pixmap,mask]=pixbuf.render_pixmap_and_mask[]
     image = gtkimage_new("pixmap",pixmap,mask);
   endfunction 
   add_image(demo_image_from_pixbuf_pixmap(),"file ->pixbuf -> pixmap ->image",vbox)
   
   function image=demo_image_from_file() 
     image= gtkimage_new('file',"gtk-logo-rgb.gif")
   endfunction 
     
   add_image(demo_image_from_file(),"file -> image",vbox)
   
   window.show_all[]
endfunction
