// Drag and drop
//
// note that these two functions don't use any global variables
// to communicate.  In fact, try openning two copies of
// testgtk.sci (or the C version) and drag between them.

function demo_dnd()

  function dnd_drag_data_get(w, context, selection_data, info, time)
    dnd_string = "Bill Gates demands royalties for\n" +
    "your use of his innovation."
    // methode set
    selection_data.set[selection_data.get_target[], 8,dnd_string]
  endfunction

  function dnd_drag_data_received(w, context, x, y, data, info, time)

    function []=dnd_message_box(title, message, button)
      dialog = gtk_dialog_new()
      dialog.set_title[title];
      dialog.connect[ "delete_event", demo_delete];
      hbox = gtk_box_new("horizontal",spacing=5);
      hbox.set_border_width[5]
      window_vbox = dialog.get_content_area[];
      window_vbox.pack_start[hbox]
      // hbox.show[]
      label = gtk_label_new(str=message)
      hbox.pack_start[label]
      // label.show[]
      dialog.add_button[button,10];
      dialog.show_all[];
      response = dialog.run[]
      if response == 10 // Close button
	dialog.destroy[];
      end
    endfunction

    if data.get_format[] == 8
      target=data.get_target[];
      str = data.get_text[];
      msg = sprintf("Drop data of type %s was:\n\n%s",target.get_name[],str);
      dnd_message_box("Drop", msg,"Continue with life in\n" +
      "spite of this oppression")
    end
  endfunction

  targets = list(list('text/plain',GTK.TARGET_SAME_APP, 0))
  win = gtk_window_new()
  win.connect["delete_event", demo_delete];
  win.set_title["Drag -N- Drop"];
  box1 = gtk_box_new("vertical",spacing=0)
  win.add[box1]
  box1.show[]
  box2=	gtk_box_new("horizontal",spacing=5)
  box2.set_border_width[10]
  box1.pack_start[box2]
  box2.show[]
  frame = gtk_frame_new(label="Drag")
  box2.pack_start[frame]
  frame.show[]
  box3 = gtk_box_new("vertical",spacing=5)
  box3.set_border_width[5]
  frame.add[box3]
  box3.show[]
  button = gtk_button_new(label="Drag me!")
  box3.pack_start[button]
  button.show[]
  button.connect['drag_data_get', dnd_drag_data_get];
  gtk_drag_source_set(button,ior(GDK.BUTTON1_MASK,GDK.BUTTON3_MASK),targets, GDK.ACTION_COPY);
  frame = gtk_frame_new(label="Drop")
  box2.pack_start[frame]
  frame.show[]
  box3 = gtk_box_new("vertical",spacing=5)
  box3.set_border_width[5]
  frame.add[box3]
  box3.show[]
  button = gtk_button_new(label="To")
  box3.pack_start[button]
  button.show[]
  button.realize[]
  button.connect['drag_data_received', dnd_drag_data_received];
  gtk_drag_dest_set(button,GTK.DEST_DEFAULT_ALL,targets,GDK.ACTION_COPY);
  separator = gtk_separator_new("horizontal")
  box1.pack_start[separator,expand= %f,fill=%t,padding=0]
  separator.show[]
  box2 = gtk_box_new("vertical",spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand= %f,fill=%t,padding=0]
  box2.show[]
  button = gtk_button_new(label="close")
  button.connect["clicked", button_destroy_win,list(win) ];
  box2.pack_start[button]
  //button.set_flags[GTK.CAN_DEFAULT]
  button.set_can_default[%t]
  button.grab_default[]
  button.show[]
  win.show[];
  // gtk_main()
endfunction
