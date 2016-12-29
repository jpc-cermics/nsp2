//  Theming/CSS Blend Modes
//
// You can blend multiple backgrounds using the CSS blend modes available.

function window = demo_css_blendmodes (do_widget)
// Il faut etre en 3.20 

  
  function update_css_for_blend_mode (provider,blend_mode)
  // bytes = g_resources_lookup_data ("/css_blendmodes/css_blendmodes.css", 0, NULL);
  // css = g_strdup_printf ((gchar*) bytes.get_data[NULL],
  //                        blend_mode,
  //                        blend_mode,
  //                        blend_mode);
  // gtk_css_provider_load_from_data (provider, css, -1, NULL);
  // g_bytes_unref (bytes);
  // g_free (css);
  endfunction 

  function row_activated (listbox, row, provider)
    blend_mode = blend_modes(row.get_index [],2);
    update_css_for_blend_mode (provider, blend_mode);
  endfunction

  function setup_listbox (builder, provider)

    listbox = gtk_list_box_new ();
    container = builder.get_object["scrolledwindow"];
    container.add[ listbox];
    listbox.connect [ "row-activated", row_activated, provider];

    // These are the available blend modes.
    blend_modes =[ "Color", "color" ;
		   "Color (burn)", "color-burn" ;
		   "Color (dodge)", "color-dodge" ;
		   "Darken", "darken" ;
		   "Difference", "difference" ;
		   "Exclusion", "exclusion" ;
		   "Hard Light", "hard-light" ;
		   "Hue", "hue" ;
		   "Lighten", "lighten" ;
		   "Luminosity", "luminosity" ;
		   "Multiply", "multiply" ;
		   "Normal", "normal" ;
		   "Overlay", "overlay" ;
		   "Saturate", "saturate" ;
		   "Screen", "screen" ;
		   "Soft Light", "soft-light" ];

    //  Add a row for each blend mode available  
    for i=1:size(blend_modes,1) 
      row = gtk_list_box_row_new ();
      label = gtk_label_new(str=blend_modes(i,1));
      label.set_xalign[ 0.0];
      row.add[label];
      listbox.add[row];
      //  The first selected row is "normal"  
      if blend_modes(i,2) == "normal" then 
	normal_row = row;
      end
    end
    //  Select the "normal" row  
    listbox.select_row[ normal_row];
    // XXX g_signal_emit_by_name (normal_row, "activate");
    normal_row.grab_focus[];
  endfunction 
  
  builder = gtk_builder_new_from_file (getenv("NSP")+"/demos3/gtk3/libbase/demo_css_blendmodes.ui");
  window = builder.get_object["window"];
  
  //window.set_transient_for[do_widget];
  // window.connect [ "destroy", gtk_widget_destroyed, &window];
  
  //  Setup the CSS provider for window  
  provider = gtk_css_provider_new ();
  
  gtk_style_context_add_provider_for_screen (gdk_screen_get_default (),...
					     provider,...
					     GTK.STYLE_PROVIDER_PRIORITY_APPLICATION);
  
  setup_listbox (builder, provider);
  window.show_all[];
endfunction 

