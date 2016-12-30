//  Theming/Shadows
//
// This demo shows how to use CSS shadows.
  
function css_show_parsing_error (provider, section, error, buffer)
  start = buffer.get_iter_at_line_index[ section.get_start_line [],...
		    section.get_start_position []];
  bend = buffer.get_iter_at_line_index [ section.get_end_line [], ...
		    section.get_end_position []];
  
  if error.matches["gtk-css-provider-error-quark", ...
		   GTK.CSS_PROVIDER_ERROR_DEPRECATED] then
    tag_name = "warning";
  else
    tag_name = "error";
  end
  buffer.apply_tag_by_name[tag_name,start,bend];
endfunction

function css_text_changed (buffer, provider)
  start = buffer.get_start_iter[];
  bend = buffer.get_end_iter [];
  buffer.remove_all_tags[start, bend];
  text = buffer.get_text [ start, bend,include_hidden_chars= %f];
  ok=execstr('provider.load_from_data[text, -1];',errcatch=%t);
  if ~ok then 
    // error is underlines by  show_parsing_error
    lasterror(); // printf(catenate(lasterror()));
  else
    gtk_style_context_reset_widgets (gdk_screen_get_default ());
  end
endfunction

function css_apply_css (widget, L)
  provider=L(1);
  context = widget.get_style_context[];
  context.add_provider[ provider, i2m(intmax(1u))];
  if is(widget,%types.GtkContainer) then 
    widget.forall[ css_apply_css, L];
  end
endfunction 

function window=demo_css_shadows (do_widget)

  function toolbar=create_toolbar (void)

    toolbar = gtk_toolbar_new ();
    toolbar.set_valign[GTK.ALIGN_CENTER];
    
    item = gtk_tool_button_new ();
    item.set_icon_name [ "go-next"];
    toolbar.insert[ item, -1];

    item = gtk_tool_button_new ();
    item.set_icon_name [ "go-previous"];
    toolbar.insert[ item, -1];
    
    item = gtk_tool_button_new ();//NULL, "Hello World");
    item.set_is_important[%t];
    toolbar.insert[ item, -1];
  endfunction 


  
  
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_title [ "Shadows"];
  //window.set_transient_for[do_widget];
  window.set_default_size[400, 300];
  //window.connect [ "destroy", gtk_widget_destroyed, &window);

  paned = gtk_paned_new (GTK.ORIENTATION_VERTICAL);
  window.add[paned];

  child = create_toolbar ();
  paned.add[child];

  text = gtk_text_buffer_new ();
  text.create_tag[ "warning", underline= PANGO.UNDERLINE_SINGLE];
  text.create_tag[ "error",   underline= PANGO.UNDERLINE_ERROR];
  
  provider = gtk_css_provider_new ();
  
  container = gtk_scrolled_window_new ();
  paned.add[container];
  child = gtk_text_view_new_with_buffer (text);
  container.add[child];
  text.connect[ "changed", css_text_changed, provider];
  
  // The -gtk-icon-shadow should be un commented depending on gtk version
  S=getfile(getenv('NSP')+"/demos3/gtk3/libbase/demo_css_shadows/css_shadows.css");
  S=strsubst(S,'NSP',getenv('NSP'));
  text.set_text [ catenate(S,sep='\n')];
  
  provider.connect [ "parsing-error", css_show_parsing_error, child.get_buffer []];

  css_apply_css (window, list(provider));
  window.show_all[];
endfunction 

