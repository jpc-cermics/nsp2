
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
  
  function image=demo_image_from_pixmap(window) 
// create a pixmap from a file, then an image 
// get a proper visual  visual = gdkvisual_new() 
// is not good because it can give a visual with a depth 
// diff to the depth of our widgets. 
  visual = window.get_visual[];
  // colormap 
  cmap= gdkcolormap_new(visual,%t)
  //cmap.alloc_color[rand(30,3)*56]
  //cmap.alloc_color[["red","green","blue"]];
  None = none_create();
  // gdkpixmap_new(darea.window,67,67)
  // gdkpixmap_new(None,67,67,depth=78)
  // colormap or drawable , transparent_color or none, file or data 
  nsp_logo = getenv('NSP')+'/demos/gtk2/libplus/nsp.png';
  // The depth used in gdk_pixmap_create_from_xpm depends on the 
  // depth given by a drawable or the depth given by the colormap
  // in both cases we have to check that the depth is the same 
  // as the depth required by the destination widget.
  [pixmap,mask]= gdk_pixmap_create_from_xpm(cmap,None,nsp_logo);
  image = gtkimage_new("pixmap",pixmap,mask);
  endfunction 

  add_image(demo_image_from_pixmap(window),"file.gif ->pixmap",vbox)
  
  function image=demo_image_from_pixmap_data(window) 
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
    visual = window.get_visual[];
    // colormap 
    cmap= gdkcolormap_new(visual,%t)
    [pixmap,mask]= gdk_pixmap_create_from_xpm(cmap,none_create(),xpm_pix);
    image = gtkimage_new("pixmap",pixmap,mask);
  endfunction
  
  add_image(demo_image_from_pixmap_data(window),"data ->pixmap ",vbox)

// FIXME: need more examples with create from data 
//        and examples with bitmap_create  

   function image=demo_image_from_pixbuf() 
     shell = getenv('NSP')+'/demos/gtk2/libplus/shell.xpm';
     pixbuf= gdk_pixbuf_new_from_file(shell)
     image = gtkimage_new("pixbuf",pixbuf);
   endfunction 
   
   add_image(demo_image_from_pixbuf(),"file.xpm ->pixbuf ->image",vbox)

   function image=demo_image_from_pixbuf_pixmap() 
     tree = getenv('NSP')+'/demos/gtk2/libplus/tree.gif';
     pixbuf= gdk_pixbuf_new_from_file(tree)
     // pixbuf.render_pixmap_and_mask[] // alpha_thresold]
     [pixmap,mask]=pixbuf.render_pixmap_and_mask[]
     image = gtkimage_new("pixmap",pixmap,mask);
   endfunction 
  
   add_image(demo_image_from_pixbuf_pixmap(),"file.gif ->pixbuf -> pixmap ->image",vbox)
   
   function image=demo_image_from_file() 
     gtk_logo = getenv('NSP')+'/demos/gtk2/libplus/gtk-logo-rgb.gif";
     image= gtkimage_new('file',gtk_logo);
   endfunction 
     
   add_image(demo_image_from_file(),"file.gif -> image",vbox)
   
   window.show_all[]
endfunction
