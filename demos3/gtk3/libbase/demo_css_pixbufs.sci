//  Theming/Animated Backgrounds
//
// This demo is done in honour of the Pixbufs demo further down.
// It is done exclusively with CSS as the background of the window.
  
function window=demo_css_pixbufs(do_widget)
  
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_title [ "Animated Backgrounds"];
  if nargin >=1 then 
    window.set_transient_for[do_widget];
  end
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
  text.connect [ "changed", css_text_changed, provider];
  provider.connect ["parsing-error", css_show_parsing_error, text];
  
  container = gtk_scrolled_window_new ();
  paned.add[container];
  child = gtk_text_view_new_with_buffer (text);
  container.add[child];
  
  S=getfile(getenv("NSP")+"/demos3/gtk3/libbase/demo_css_pixbufs/css_pixbufs.css");
  S=strsubst(S,'NSP',getenv('NSP'));
  text.set_text[catenate(S,sep='\n')];
  
  provider.load_from_data[catenate(S,sep='\n'),-1];
  
  css_apply_css (window, list(provider));
  window.show_all[];
endfunction

