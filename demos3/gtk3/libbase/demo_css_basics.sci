//  Theming/CSS Basics
//
// Gtk themes are written using CSS. Every widget is build of multiple items
// that you can style very similarly to a regular website.
//

function window= demo_css_basics (do_widget)

  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_title [ "CSS Basics"];
  //window.set_transient_for[do_widget];
  window.set_default_size[400, 300];
  // window.connect [ "destroy", gtk_widget_destroyed, &window];

  text = gtk_text_buffer_new ();
  text.create_tag[ "warning", underline= PANGO.UNDERLINE_SINGLE];
  text.create_tag[ "error",   underline= PANGO.UNDERLINE_ERROR];

  provider = gtk_css_provider_new ();

  container = gtk_scrolled_window_new ();
  window.add[container];
  child = gtk_text_view_new_with_buffer (text);
  container.add[child];
  text.connect [ "changed", css_text_changed, provider];

    // The -gtk-icon-shadow should be un commented depending on gtk version
  S=getfile(getenv('NSP')+"/demos3/gtk3/libbase/demo_css_basics/css_basics.css");
  S=strsubst(S,'NSP',getenv('NSP'));
  text.set_text [ catenate(S,sep='\n')];
  
  provider.connect [ "parsing-error", css_show_parsing_error, child.get_buffer []];

  css_apply_css (window, list(provider));
  window.show_all[];
  
endfunction
