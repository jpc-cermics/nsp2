function demo_add_image(image,ok,title,vbox)
  label = gtklabel_new();
  label.set_markup["<u>"+title+"</u>"];
  vbox.pack_start[ label,expand=%f,fill=%f,padding=0]
  if ok then 
    frame = gtkframe_new();
    frame.set_shadow_type[GTK.SHADOW_IN];
    align = gtkalignment_new(xalign=0.5,yalign=0.5,xscale=0,yscale=0);
    align.add[  frame]
    vbox.pack_start[ align,expand=%f,fill=%f,padding=0]
    frame.add[image]
  else
    label = gtklabel_new();
    label.set_markup["<u>"+'failed to create image'+"</u>"];
    vbox.pack_start[ label,expand=%f,fill=%f,padding=0]
  end
endfunction

function demo_images1() 
  // 
  // Create an image from images stored in file 
  // 
  window = gtkwindow_new()
  //window.connect[ "delete_event", demo_delete];
  window.set_title[" file -> images "];
  //window.connect[  "destroy",  gtk_widget_destroyed, &window]
  //window.connect[  "destroy",	cleanup_callback, NULL]
  window.set_border_width[  8]
  vbox = gtkvbox_new(homogeneous=%f,spacing=8);
  vbox.set_border_width[  8]
  window.add[  vbox]
  
  function [image,ok]=demo_image_from_file(fname) 
    ok=execstr('image= gtkimage_new('"file'',fname)',errcatch=%t);
  endfunction 
  
  fname = getenv('NSP')+'/demos/gtk2/libplus/gtk-logo-rgb.gif";
  [image,ok]=demo_image_from_file(fname)
  demo_add_image(image,ok,"gtkimage_new(''file'',.) with gif file",vbox)

  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.png";
  [image,ok]=demo_image_from_file(fname)
  demo_add_image(image,ok,"gtkimage_new(''file'',.) with png file",vbox)

  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.xpm";
  [image,ok]=demo_image_from_file(fname)
  demo_add_image(image,ok,"gtkimage_new(''file'',.) with xpm file",vbox)

  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.svg";
  [image,ok]=demo_image_from_file(fname)
  demo_add_image(image,ok,"gtkimage_new(''file'',.) with svg image",vbox)

  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.jpg";
  [image,ok]=demo_image_from_file(fname)
  demo_add_image(image,ok,"gtkimage_new(''file'',.) with jpg image",vbox)
  
  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.xxx";
  [image,ok]=demo_image_from_file(fname)
  demo_add_image(image,ok,"gtkimage_new(''file'',.) with unknown extension)",vbox)

  window.show_all[]
endfunction

function demo_images2() 
  // 
  // Create an image from images stored in file 
  // By first reading a pixbuf.
  // 
  window = gtkwindow_new()
  //window.connect[ "delete_event", demo_delete];
  window.set_title["file -> pixbuf -> image"];
  //window.connect[  "destroy",  gtk_widget_destroyed, &window]
  //window.connect[  "destroy",	cleanup_callback, NULL]
  window.set_border_width[  8]
  vbox = gtkvbox_new(homogeneous=%f,spacing=8);
  vbox.set_border_width[  8]
  window.add[  vbox]
  
  function [image,ok]=demo_image_from_pixbuf(fname) 
    image=[];
    ok = execstr('pixbuf= gdk_pixbuf_new_from_file(fname)',errcatch=%t);
    if ~ok then return;end 
    ok = execstr('image = gtkimage_new('"pixbuf"',pixbuf);',errcatch=%t);
  endfunction 
  
  fname = getenv('NSP')+'/demos/gtk2/libplus/gtk-logo-rgb.gif";
  [image,ok]=demo_image_from_pixbuf(fname)
  demo_add_image(image,ok,"gtkimage_new(''pixbuf'',.) with gif file",vbox)

  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.png";
  [image,ok]=demo_image_from_pixbuf(fname)
  demo_add_image(image,ok,"gtkimage_new(''pixbuf'',.) with png file",vbox)

  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.xpm";
  [image,ok]=demo_image_from_pixbuf(fname)
  demo_add_image(image,ok,"gtkimage_new(''pixbuf'',.) with xpm file",vbox)

  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.svg";
  [image,ok]=demo_image_from_pixbuf(fname)
  demo_add_image(image,ok,"gtkimage_new(''pixbuf'',.) with svg image",vbox)

  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.xxx";
  [image,ok]=demo_image_from_pixbuf(fname)
  demo_add_image(image,ok,"gtkimage_new(''pixbuf'',.) with unknown extension)",vbox)

  window.show_all[]
endfunction

function demo_images3() 
  // 
  // Create an image from images stored in file 
  // By first reading a pixbuf.
  // 
  window = gtkwindow_new()
  //window.connect[ "delete_event", demo_delete];
  window.set_title["file -> pixbuf -> pixmap -> images"];
  //window.connect[  "destroy",  gtk_widget_destroyed, &window]
  //window.connect[  "destroy",	cleanup_callback, NULL]
  window.set_border_width[  8]
  vbox = gtkvbox_new(homogeneous=%f,spacing=8);
  vbox.set_border_width[  8]
  window.add[  vbox]
    
  function [image,ok]=demo_image_from_pixbuf_pixmap(fname) 
    image=[];
    ok = execstr('pixbuf= gdk_pixbuf_new_from_file(fname)',errcatch=%t);
    if ~ok then return;end 
    // pixbuf.render_pixmap_and_mask[] // alpha_thresold]
    ok = execstr('[pixmap,mask]=pixbuf.render_pixmap_and_mask[]',errcatch=%t)
    if ~ok then return;end 
    ok = execstr('image = gtkimage_new('"pixmap"',pixmap,mask);',errcatch=%t)
  endfunction 
      
  fname = getenv('NSP')+'/demos/gtk2/libplus/gtk-logo-rgb.gif";
  [image,ok]=demo_image_from_pixbuf_pixmap(fname)
  demo_add_image(image,ok,"gtkimage_new(''pixmap'',.) with gif file",vbox)

  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.png";
  [image,ok]=demo_image_from_pixbuf_pixmap(fname)
  demo_add_image(image,ok,"gtkimage_new(''pixmap'',.) with png file",vbox)

  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.xpm";
  [image,ok]=demo_image_from_pixbuf_pixmap(fname)
  demo_add_image(image,ok,"gtkimage_new(''pixmap'',.) with xpm file",vbox)

  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.svg";
  [image,ok]=demo_image_from_pixbuf_pixmap(fname)
  demo_add_image(image,ok,"gtkimage_new(''pixmap'',.) with svg image",vbox)

  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.xxx";
  [image,ok]=demo_image_from_pixbuf_pixmap(fname)
  demo_add_image(image,ok,"gtkimage_new(''pixmap'',.) with unknown extension)",vbox)

  window.show_all[]
endfunction

function demo_images4() 
  // 
  // Create an image from images stored in file 
  // By first reading a pixmap.
  // 
  window = gtkwindow_new()
  //window.connect[ "delete_event", demo_delete];
  window.set_title["file -> pixmap -> images"];
  //window.connect[  "destroy",  gtk_widget_destroyed, &window]
  //window.connect[  "destroy",	cleanup_callback, NULL]
  window.set_border_width[  8]
  vbox = gtkvbox_new(homogeneous=%f,spacing=8);
  vbox.set_border_width[  8]
  window.add[  vbox]
  
  function  [image,ok]=demo_image_from_pixmap(window,fname) 
  // create a pixmap from a file, then an image 
  // get a proper visual  visual = gdkvisual_new() 
  // is not good because it can give a visual with a depth 
  // diff to the depth of our widgets. 
    image=[];
    visual = window.get_visual[];
    // colormap 
    cmap= gdkcolormap_new(visual,%t)
    //cmap.alloc_color[rand(30,3)*56]
    //cmap.alloc_color[["red","green","blue"]];
    None = none_create();
    // gdkpixmap_new(darea.window,67,67)
    // gdkpixmap_new(None,67,67,depth=78)
    // colormap or drawable , transparent_color or none, file or data 
    // The depth used in gdk_pixmap_create_from_xpm depends on the 
    // depth given by a drawable or the depth given by the colormap
    // in both cases we have to check that the depth is the same 
    // as the depth required by the destination widget.
    ok=execstr('[pixmap,mask]= gdk_pixmap_create_from_xpm(cmap,None,fname);',errcatch=%t);
    if ~ok then return;end 
    ok = execstr('image = gtkimage_new('"pixmap"',pixmap,mask);',errcatch=%t);
  endfunction 
      
  fname = getenv('NSP')+'/demos/gtk2/libplus/gtk-logo-rgb.gif";
  [image,ok]=demo_image_from_pixmap(window,fname)
  demo_add_image(image,ok,"gtkimage_new(''pixmap'',.) with gif file",vbox)

  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.png";
  [image,ok]=demo_image_from_pixmap(window,fname)
  demo_add_image(image,ok,"gtkimage_new(''pixmap'',.) with png file",vbox)

  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.xpm";
  [image,ok]=demo_image_from_pixmap(window,fname)
  demo_add_image(image,ok,"gtkimage_new(''pixmap'',.) with xpm file",vbox)

  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.svg";
  [image,ok]=demo_image_from_pixmap(window,fname)
  demo_add_image(image,ok,"gtkimage_new(''pixmap'',.) with svg image",vbox)

  fname = getenv('NSP')+'/demos/gtk2/libplus/nsp.xxx";
  [image,ok]=demo_image_from_pixmap(window,fname)
  demo_add_image(image,ok,"gtkimage_new(''pixmap'',.) with unknown extension)",vbox)

  window.show_all[]
endfunction


function demo_images5() 
  window = gtkwindow_new()
  //window.connect[ "delete_event", demo_delete];
  window.set_title["Images"];
  //window.connect[  "destroy",  gtk_widget_destroyed, &window]
  //window.connect[  "destroy",	cleanup_callback, NULL]
  window.set_border_width[  8]
  vbox = gtkvbox_new(homogeneous=%f,spacing=8);
  vbox.set_border_width[  8]
  window.add[  vbox]
  
  
  [image,ok]=demo_image_from_pixmap(window);
  demo_add_image(image,ok,"file.xpm ->pixmap",vbox)
  
  function [image,ok]=demo_image_from_pixmap_data(window) 
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
    ok=execstr('[pixmap,mask]= gdk_pixmap_create_from_xpm(cmap,none_create(),xpm_pix);',...
	       errcatch=%t);
    if ok then 
      image = gtkimage_new("pixmap",pixmap,mask);
    else
      image=[];
    end
  endfunction
  
  [image,ok]=demo_image_from_pixmap_data(window)
  demo_add_image(image,ok,"data ->pixmap ",vbox)

  window.show_all[]
endfunction

