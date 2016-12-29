// Info Bars
//
// Info bar widgets are used to report important messages to the user.

function demo_infobar(do_widget)

  function on_bar_response(info_bar, response_id, user_data)
    if response_id == GTK.RESPONSE_CLOSE then 
      info_bar.hide[];
      return;
    end
    window = info_bar.get_toplevel[];
    
    dialog = gtk_message_dialog_new (parent=window,...
				     flags=ior(GTK.DIALOG_MODAL, GTK.DIALOG_DESTROY_WITH_PARENT),...
				     type=GTK.MESSAGE_INFO,...
				     buttons=GTK.BUTTONS_OK,...
				     message="You clicked a button on an info bar");
    // dialog.format_secondary_text[ "Your response has id %d", response_id];
    // g_signal_connect_swapped (dialog,  "response",  G_CALLBACK (gtk_widget_destroy), dialog);
    dialog.run[];
    dialog.destroy[];
  endfunction
  
  actions = gtk_box_new (GTK.ORIENTATION_HORIZONTAL, spacing = 0);
  
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  //  window.set_screen[do_widget.get_screen[]];
  window.set_title["Info Bars"];
  //g_signal_connect (window, "destroy", G_CALLBACK (gtk_widget_destroyed), &window);
  window.set_border_width[8];

  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL,spacing= 0);
  window.add[vbox];

  bars_type = ["INFO", "WARNING", "QUESTION", "ERROR", "OTHER"];
  
  // A set of info bars 
  for i = 1:size(bars_type,'*')
    bar = gtk_info_bar_new ();
    vbox.pack_start[bar, expand= %f,fill= %f,padding=0];
    execstr( sprintf("mtype= GTK.MESSAGE_%s",bars_type(i)));
    bar.set_message_type[ mtype ];
    if bars_type(i) == "QUESTION" then 
      bar.add_button["_OK", GTK.RESPONSE_OK];
      bar.set_show_close_button[%t];
      bar.connect[ "response", on_bar_response, window];
    end
    label = gtk_label_new (str="This is an info bar with message type GTK.MESSAGE_"+bars_type(i));
    label.set_line_wrap[%t];
    label.set_xalign[0];
    area= bar.get_content_area[];
    area.pack_start[label,  expand= %f,fill= %f,padding=0];

    button = gtk_toggle_button_new(label=bars_type(i));
    button.bind_property["active", bar, "visible", "BIDIRECTIONAL"];
    actions.add[button];
  end
    
  // now a frame 
  frame = gtk_frame_new (label="Info bars");
  vbox.pack_start[frame,  expand= %f,fill= %f,padding=8];
  
  vbox2 = gtk_box_new (GTK.ORIENTATION_VERTICAL, spacing=8);
  vbox2.set_border_width[8];
  frame.add[vbox2];
  
  // Standard message dialog 
  label = gtk_label_new (str="An example of different info bars");
  vbox2.pack_start[label,  expand= %f,fill= %f,padding=0];
  
  actions.show_all[];
  vbox2.pack_start[actions,  expand= %f,fill= %f,padding=0];
  
  window.show_all[];
endfunction 
  
