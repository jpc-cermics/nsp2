// flipping

function demo_flipping ()

  function flipping_toggled_cb (widget, data)
    if  widget.get_active[] then
      gtk_widget_set_default_direction[GTK.TEXT_DIR_RTL];
    else
      gtk_widget_set_default_direction[GTK.TEXT_DIR_LTR]
    end
  endfunction

  function set_direction_recurse (widget, data)
    widget.set_direction[data(1)]
    if is(widget,%types.GtkContainer) then
      widget.foreach[ set_direction_recurse,data]
    end
  endfunction

  function frame= create_forward_back (title, text_dir)
    frame = gtk_frame_new(label=title);
    bbox = gtk_button_box_new("horizontal");
    back_button = gtk_button_new(icon_name="go-previous");
    forward_button = gtk_button_new(icon_name="document-save");
    bbox.set_border_width[  5]
    frame.add[  bbox]
    bbox.add[  back_button]
    bbox.add[  forward_button]
    set_direction_recurse(frame,list(text_dir));
  endfunction

  dialog = gtk_dialog_new ();
  //     dialog.connect[  "destroy",hide]
  dialog.set_title[  "Bidirectional Flipping"]

  check_button = gtk_check_button_new(label="Right-to-left global direction");
  dialog_vbox = dialog.get_content_area[];
  dialog_vbox.pack_start[ check_button,expand=%t,fill=%t,padding=0];
  dialog_vbox.pack_start[ create_forward_back ("Default", GTK.TEXT_DIR_NONE)];
  dialog_vbox.pack_start[ create_forward_back ("Left-to-Right", GTK.TEXT_DIR_LTR)];
  dialog_vbox.pack_start[ create_forward_back ("Right-to-Left", GTK.TEXT_DIR_RTL)];

  if gtk_widget_get_default_direction () == GTK.TEXT_DIR_RTL then
    check_button.set_active[  %t]
  end
  check_button.connect[  "toggled",	flipping_toggled_cb, list(%f)]
  check_button.set_border_width[  10]

  dialog.add_button["Close",10];
  dialog.show_all[];
  response = dialog.run[]
  if response == 10 // Close button
    dialog.destroy[];
  end
endfunction
