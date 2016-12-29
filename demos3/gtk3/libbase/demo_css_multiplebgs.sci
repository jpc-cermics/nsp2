//  Theming/Multiple Backgrounds
//
// Gtk themes are written using CSS. Every widget is build of multiple items
// that you can style very similarly to a regular website.


function demo_css_multiplebgs (do_widget)

  function y=drawing_area_draw (widget,cr)
    context = widget.get_style_context [];
    gtk_render_background (context, cr,...
			   0, 0, widget.get_allocated_width [],...
			   widget.get_allocated_height []);
    gtk_render_frame (context, cr,...
		      0, 0,...
		      widget.get_allocated_width [],...
		      widget.get_allocated_height []);
    y= %f;
  endfunction 
  
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_title [ "Multiple Backgrounds"];
  //window.set_transient_for[do_widget];
  window.set_default_size[400, 300];
  // window.connect [ "destroy", gtk_widget_destroyed, &window];

  container = gtk_overlay_new ();
  container.add_events[ior([GDK.ENTER_NOTIFY_MASK , GDK.LEAVE_NOTIFY_MASK , GDK.POINTER_MOTION_MASK])];
  window.add[container];
  
  child = gtk_drawing_area_new ();
  child.set_name [ "canvas"];
  //child.connect [ "draw", drawing_area_draw];
  container.add[child];

  child = gtk_button_new ();
  child.add_events[ ior([GDK.ENTER_NOTIFY_MASK , GDK.LEAVE_NOTIFY_MASK , GDK.POINTER_MOTION_MASK])];
  container.add_overlay[ child];
  child.set_name [ "bricks-button"];
  child.set_halign[GTK.ALIGN_CENTER];
  child.set_valign[GTK.ALIGN_CENTER];
  child.set_size_request[250, 84];

  paned = gtk_paned_new (GTK.ORIENTATION_VERTICAL);
  container.add_overlay[paned];

  //  Need a filler so we get a handle  
  child = gtk_box_new (GTK.ORIENTATION_VERTICAL, spacing=0);
  paned.add[child];

  text = gtk_text_buffer_new ();
  text.create_tag[ "warning", underline= PANGO.UNDERLINE_SINGLE];
  text.create_tag[ "error",   underline= PANGO.UNDERLINE_ERROR];

  provider = gtk_css_provider_new ();
  
  container = gtk_scrolled_window_new ();
  paned.add[container];
  child = gtk_text_view_new_with_buffer (text);
  container.add[child];
  text.connect [ "changed", css_text_changed, provider];

  S=getfile(getenv('NSP')+"/demos3/gtk3/libbase/demo_css_multiplebgs/css_multiplebgs.css");
  S=strsubst(S,'NSP',getenv('NSP'));
  text.set_text [ catenate(S,sep='\n')];
  
  //provider.load_from_resource["/css_multiplebgs/css_multiplebgs.css"];
  provider.connect ["parsing-error", show_parsing_error, child.get_buffer []];
  apply_css (window, list(provider));
  window.show_all[];
endfunction 
