// Header Bar
//
// GtkHeaderBar is a container that is suitable for implementing
// window titlebars. One of its features is that it can position
// a title (and optional subtitle) centered with regard to the
// full width, regardless of variable-width content at the left
// or right.
//
// It is commonly used with gtk_window_set_titlebar()
//

function window= demo_headerbar (do_widget)
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_screen [ do_widget.get_screen []];
  //window.connect [ "destroy", gtk_widget_destroyed, &window];
  window.set_default_size[600, 400];
  
  header = gtk_header_bar_new ();
  header.set_show_close_button[%t];
  header.set_title [ "Welcome to Facebook - Log in, sign up or learn more"];
  header.set_has_subtitle[%f];
  
  button = gtk_button_new ();
  icon = g_themed_icon_new ("mail-send-receive-symbolic");
  image = gtk_image_new_from_gicon (icon, GTK.ICON_SIZE_BUTTON);
  button.add[image];
  header.pack_end[ button];
  
  box = gtk_box_new (GTK.ORIENTATION_HORIZONTAL,spacing= 0);
  stc = box.get_style_context [];
  stc.add_class["linked"];
  button = gtk_button_new ();
  button.add [ gtk_image_new_from_icon_name ("pan-start-symbolic", GTK.ICON_SIZE_BUTTON)];
  box.add[button];
  button = gtk_button_new ();
  button.add [ gtk_image_new_from_icon_name ("pan-end-symbolic", GTK.ICON_SIZE_BUTTON)];
  box.add[button];

  header.pack_start[box];
  window.set_titlebar[header];
  window.add[gtk_text_view_new ()];
  window.show_all[];
endfunction

