
function toggle_sensitivity_callback(togglebutton,args)
  container= args(1);
  ch = container.get_children[];
  flag= ~ togglebutton.get_active[];
  for child=ch do 
    child.set_sensitive[flag];
  end
  togglebutton.set_sensitive[%t]
endfunction

function demo_images() 
  // 
  // Create an image from images stored in file 
  // 
  window = gtkwindow_new()
  //window.connect[ "delete_event", demo_delete];
  window.set_title[" images "];
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

  fname = getenv('NSP')+'/demos/gtk2/libplus/floppybuddy.gif';
  [image,ok]=demo_image_from_file(fname)
  demo_add_image(image,ok,"gtkimage_new(''file'',.) with animated gif file",vbox)

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
  
  // Sensitivity control 
  button = gtktogglebutton_new(mnemonic="_Insensitive");
  vbox.pack_start[ button,expand=%f,fill=%f,padding=0]
  button.connect["toggled",  toggle_sensitivity_callback,list(vbox)]
  window.show_all[]
endfunction
