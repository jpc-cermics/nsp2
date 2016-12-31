// Images
//
// GtkImage is used to display an image; the image can be in a number of formats.
// Typically, you load an image into a GdkPixbuf, then display the pixbuf.
//
// This demo code shows some of the more obscure cases, in the simple
// case a call to gtk_image_new_from_file() is all you need.
//
// If you want to put image data in your program as a C variable,
// use the make-inline-pixbuf program that comes with GTK+.
// This way you won't need to depend on loading external files, your
// application binary can be self-contained.
//

function window = demo_images(do_widget)
// Create an image from images stored in file
  
  function demo_add_image(image,ok,title,vbox)
    label = gtk_label_new();
    label.set_markup["<u>"+title+"</u>"];
    vbox.pack_start[ label,expand=%f,fill=%f,padding=0]
    if ok then
      frame = gtk_frame_new();
      frame.set_shadow_type[GTK.SHADOW_IN];
      vbox.pack_start[frame,expand=%f,fill=%f,padding=0]
      frame.add[image]
    else
      label = gtk_label_new();
      label.set_markup["<u>"+'failed to create image'+"</u>"];
      vbox.pack_start[ label,expand=%f,fill=%f,padding=0]
    end
  endfunction

  window = gtk_window_new()
  window.set_title[" images "];
  window.set_border_width[  8]
  vbox = gtk_box_new("vertical",spacing=8);
  vbox.set_border_width[  8]
  window.add[  vbox]

  function [image,ok]=demo_image_from_file(fname)
    ok=execstr('image= gtk_image_new('"file'',fname)',errcatch=%t);
  endfunction

  image = gtk_image_new_from_icon_name("gtk3-demo", GTK.ICON_SIZE_DIALOG);
  demo_add_image(image,%t,"gtk_image_new_from_icon_name",vbox)
  
  fname = getenv('NSP')+'/demos3/gtk3/libbase/demo_images/gtk-logo-rgb.gif";
  [image,ok]=demo_image_from_file(fname)
  demo_add_image(image,ok,"gtk_image_new(''file'',.) with gif file",vbox)

  fname = getenv('NSP')+'/demos3/gtk3/libbase/demo_images/floppybuddy.gif';
  [image,ok]=demo_image_from_file(fname)
  demo_add_image(image,ok,"gtk_image_new(''file'',.) with animated gif file",vbox)

  gicon = g_themed_icon_new_with_default_fallbacks ("battery-caution-charging-symbolic");
  image = gtk_image_new_from_gicon (gicon, GTK.ICON_SIZE_DIALOG);
  demo_add_image(image,%t,"gtk_image_new_from_gicon",vbox)
  
  // Sensitivity control
  button = gtk_toggle_button_new(mnemonic="_Insensitive");
  vbox.pack_start[ button,expand=%f,fill=%f,padding=0]
  
  function toggle_sensitivity_callback(togglebutton,args)
    container= args(1);
    ch = container.get_children[];
    flag= ~ togglebutton.get_active[];
    for child=ch do
      child.set_sensitive[flag];
    end
    togglebutton.set_sensitive[%t]
  endfunction
  
  button.connect["toggled",  toggle_sensitivity_callback,list(vbox)]
  window.show_all[]
endfunction

