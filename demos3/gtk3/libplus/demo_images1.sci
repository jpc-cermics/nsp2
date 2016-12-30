function demo_add_image(image,ok,title,vbox)
  label = gtklabel_new();
  label.set_markup["<u>"+title+"</u>"];
  vbox.pack_start[ label,expand=%f,fill=%f,padding=0]
  if ok then
    frame = gtkframe_new();
    frame.set_shadow_type[GTK.SHADOW_IN];
    vbox.pack_start[frame,expand=%f,fill=%f,padding=0]
    frame.add[image]
  else
    label = gtklabel_new();
    label.set_markup["<u>"+'failed to create image'+"</u>"];
    vbox.pack_start[ label,expand=%f,fill=%f,padding=0]
  end
endfunction

function demo_images1()
  // Create an image from images stored in file
  // file -> gtkimage
  window = gtkwindow_new()
  window.set_title[" file -> images "];
  window.set_border_width[  8]
  vbox = gtkbox_new("vertical",spacing=8);
  vbox.set_border_width[  8]
  window.add[  vbox]

  function [image,ok]=demo_image_from_file(fname)
    ok=execstr('image= gtkimage_new('"file'',fname)',errcatch=%t);
  endfunction

  fname = getenv('NSP')+'/demos3/gtk3/libbase/demo_images/gtk-logo-rgb.gif";
  [image,ok]=demo_image_from_file(fname)
  demo_add_image(image,ok,"gtkimage_new(''file'',.) with gif file",vbox)

  fname = getenv('NSP')+'/demos3/gtk3/libplus/nsp.png";
  [image,ok]=demo_image_from_file(fname)
  demo_add_image(image,ok,"gtkimage_new(''file'',.) with png file",vbox)

  fname = getenv('NSP')+'/demos3/gtk3/libplus/nsp.xpm";
  [image,ok]=demo_image_from_file(fname)
  demo_add_image(image,ok,"gtkimage_new(''file'',.) with xpm file",vbox)

  fname = getenv('NSP')+'/demos3/gtk3/libplus/nsp.svg";
  [image,ok]=demo_image_from_file(fname)
  demo_add_image(image,ok,"gtkimage_new(''file'',.) with svg image",vbox)

  fname = getenv('NSP')+'/demos3/gtk3/libplus/nsp.jpg";
  [image,ok]=demo_image_from_file(fname)
  demo_add_image(image,ok,"gtkimage_new(''file'',.) with jpg image",vbox)

  fname = getenv('NSP')+'/demos3/gtk3/libplus/nsp.xxx";
  [image,ok]=demo_image_from_file(fname)
  demo_add_image(image,ok,"gtkimage_new(''file'',.) with unknown extension)",vbox)

  window.show_all[]
endfunction

function demo_images2()
// Create an image from images stored in file
// By first reading a pixbuf.
// file -> gdkpixbuf -> gtkimage
  window = gtkwindow_new()
  window.set_title["file -> pixbuf -> image"];
  window.set_border_width[  8]
  vbox = gtkbox_new("vertical",spacing=8);
  vbox.set_border_width[  8]
  window.add[  vbox]

  function [image,ok]=demo_image_from_pixbuf(fname)
    image=[];
    ok = execstr('pixbuf= gdk_pixbuf_new_from_file(fname)',errcatch=%t);
    if ~ok then return;end
    ok = execstr('image = gtkimage_new('"pixbuf"',pixbuf);',errcatch=%t);
  endfunction

  fname = getenv('NSP')+'/demos3/gtk3/libbase/demo_images/gtk-logo-rgb.gif";
  [image,ok]=demo_image_from_pixbuf(fname)
  demo_add_image(image,ok,"gtkimage_new(''pixbuf'',.) with gif file",vbox)

  fname = getenv('NSP')+'/demos3/gtk3/libplus/nsp.png";
  [image,ok]=demo_image_from_pixbuf(fname)
  demo_add_image(image,ok,"gtkimage_new(''pixbuf'',.) with png file",vbox)

  fname = getenv('NSP')+'/demos3/gtk3/libplus/nsp.xpm";
  [image,ok]=demo_image_from_pixbuf(fname)
  demo_add_image(image,ok,"gtkimage_new(''pixbuf'',.) with xpm file",vbox)

  fname = getenv('NSP')+'/demos3/gtk3/libplus/nsp.svg";
  [image,ok]=demo_image_from_pixbuf(fname)
  demo_add_image(image,ok,"gtkimage_new(''pixbuf'',.) with svg image",vbox)

  fname = getenv('NSP')+'/demos3/gtk3/libplus/nsp.xxx";
  [image,ok]=demo_image_from_pixbuf(fname)
  demo_add_image(image,ok,"gtkimage_new(''pixbuf'',.) with unknown extension)",vbox)

  window.show_all[]
endfunction

function demo_images3()
// image from inline xpm
  window = gtkwindow_new()
  window.set_title["Images"];
  window.set_border_width[  8]
  vbox = gtkbox_new("vertical",spacing=8);
  vbox.set_border_width[  8]
  window.add[  vbox]

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
	      "........."];

  ok=execstr('pixbuf= gdk_pixbuf_new_from_xpm_data(xpm_pix);',errcatch=%t);
  if ok then
    image = gtkimage_new("pixbuf",pixbuf);
  else
    image=[];
  end
  demo_add_image(image,ok,"data -> pixbuf ",vbox)
  window.show_all[]
endfunction
