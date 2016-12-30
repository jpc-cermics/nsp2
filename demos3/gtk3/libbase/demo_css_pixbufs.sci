//  Theming/Animated Backgrounds
//
// This demo is done in honour of the Pixbufs demo further down.
// It is done exclusively with CSS as the background of the window.
  
function window=demo_css_pixbufs(do_widget)

  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_title [ "Animated Backgrounds"];
  window.set_transient_for[do_widget];
  window.set_default_size[400, 300];
  // window.connect [ "destroy", gtk_widget_destroyed, &window];

  paned = gtk_paned_new (GTK.ORIENTATION_VERTICAL);
  window.add[paned];

  //  Need a filler so we get a handle  
  child = gtk_box_new (GTK.ORIENTATION_VERTICAL, spacing= 0);
  paned.add[child];

  text = gtk_text_buffer_new ();
  text.create_tag[ "warning", underline= PANGO.UNDERLINE_SINGLE];
  text.create_tag[ "error",   underline= PANGO.UNDERLINE_ERROR];
  
  provider = gtk_css_provider_new ();

  container = gtk_scrolled_window_new (NULL, NULL);
  paned.add[container];
  child = gtk_text_view_new_with_buffer (text);
  container.add[child];
  text.connect [ "changed", css_text_changed, provider];

  S=getfile("css_pixbufs/gtk.css");
  
  // improve set_text to work 
  text.set_text [ catenate(S,sep='\n')];
  
  provider.connect ["parsing-error", css_show_parsing_error, child.get_buffer []];
  css_apply_css (window, provider);
  window.show_all[];
endfunction

