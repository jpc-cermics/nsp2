// Clipboard
//
// GtkClipboard is used for clipboard handling. This demo shows how to
// copy and paste text to and from the clipboard.
//
// It also shows how to transfer images via the clipboard or via
// drag-and-drop, and how to make clipboard contents persist after
// the application exits. Clipboard persistence requires a clipboard
// manager to run.

function pixbuf = demo_clipboard_get_pixbuf (image)
  
  select image.get_storage_type[]
   case GTK.IMAGE_PIXBUF;  pixbuf = image.get_pixbuf[];
   case GTK.IMAGE_ICON_NAME;
    [icon_name,size] = image.get_icon_name[];
    icon_theme = gtk_icon_theme_get_for_screen (image.get_screen[]);
    pixbuf = icon_theme.load_icon[icon_name,size,...
		    GTK.ICON_LOOKUP_GENERIC_FALLBACK];
  else
    printf("Image storage type %d not handled\n",image.get_storage_type[]);
    pixbuf = [];
  end
endfunction 

function window=demo_clipboard (do_widget)

  function y=button_press (widget, button, data)

  // if button.button <> GDK_BUTTON_SECONDARY then y=%f;return; end
    
    menu = gtk_menu_new ();
    item = gtk_menu_item_new(mnemonic="_Copy");
    
    function copy_image (item, data)
      GDK_SELECTION_CLIPBOARD=69 
      clipboard = gtk_clipboard_get(selection=GDK_SELECTION_CLIPBOARD);
      pixbuf = demo_clipboard_get_pixbuf(data);
      clipboard.set_image[pixbuf];
    endfunction 
    
    item.connect[ "activate", copy_image, data];
    item.show[];
    menu.append[item];

    item = gtk_menu_item_new(mnemonic ="_Paste");
    
    function paste_image (item, data)
      GDK_SELECTION_CLIPBOARD=69 
      clipboard = gtk_clipboard_get (selection=GDK_SELECTION_CLIPBOARD);
      pixbuf = gtk_clipboard_wait_for_image (clipboard);
      if pixbuf then 
	data.set_from_pixbuf[pixbuf];
      end
    endfunction 
    
    item.connect[ "activate", paste_image, data];
    item.show[];
    menu.append[item];
    // menu.popup[NULL, NULL, NULL, NULL, 3, button->time];
    y= %t;
  endfunction 
  
  function drag_begin (widget, context, data)
    pixbuf = demo_clipboard_get_pixbuf (data);
    context.set_icon_pixbuf[ pixbuf, -2, -2];
  endfunction 

  function drag_data_get (widget,context, selection_data, info,time,data)
    pixbuf = demo_clipboard_get_pixbuf (data);
    selection_data.set_pixbuf[pixbuf];
  endfunction 

  function drag_data_received (widget, context, x, y,selection_data, info, time, data)
    if selection_data.get_length [] > 0 then 
      pixbuf = selection_data.get_pixbuf [];
      data.set_from_pixbuf[pixbuf];
    end
  endfunction 
  
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  //window.set_screen[ do_widget.get_screen []];
  window.set_title[ "Clipboard"];
  // window.connect[ "destroy",gtk_widget_destroyed, &window];
  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL, spacing= 0);
  vbox.set_border_width[8];
  window.add[vbox];
  label = gtk_label_new(str="""Copy"" will copy the text\nin the entry to the clipboard");
  vbox.pack_start[label,expand=%f,fill=%f,padding=0 ];

  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL,spacing= 4);
  hbox.set_border_width[8];
  vbox.pack_start[hbox,expand=%f,fill=%f,padding=0];

  //  Create the first entry  
  entry = gtk_entry_new ();
  hbox.pack_start[entry,expand=%t,fill=%t,padding=0];

  //  Create the button  
  button = gtk_button_new(mnemonic="_Copy");
  hbox.pack_start[button,expand=%f,fill=%f,padding=0];
  
  function copy_button_clicked (button, user_data)
    GDK_SELECTION_CLIPBOARD=69 
    entry = user_data;
    //  Get the clipboard object  
    clipboard = entry.get_clipboard[GDK_SELECTION_CLIPBOARD];
    //  Set clipboard text  
    clipboard.set_text[ entry.get_text []];
  endfunction

  
  button.connect[ "clicked", copy_button_clicked, entry ];

  label = gtk_label_new(str="""Paste"" will paste the text from the clipboard to the entry");
  vbox.pack_start[label,expand=%f,fill=%f,padding=0];

  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL,spacing= 4);
  hbox.set_border_width[8];
  vbox.pack_start[hbox,expand=%f,fill=%f,padding=0];

  //  Create the second entry  
  entry = gtk_entry_new ();
  hbox.pack_start[entry,expand=%t,fill=%t,padding=0];

  //  Create the button  
  button = gtk_button_new(mnemonic="_Paste");
  hbox.pack_start[button,expand=%f,fill=%f,padding=0];
  
  function paste_button_clicked (button, user_data)

    function paste_received (clipboard, text, user_data) 
      entry = user_data(1);
      //  Set the entry text  
      entry.set_text[text];
    endfunction
    
    GDK_SELECTION_CLIPBOARD=69 
    entry = user_data;
    //  Get the clipboard object  
    clipboard = entry.get_clipboard[GDK_SELECTION_CLIPBOARD];
    //  Request the contents of the clipboard, contents_received will be
    //  called when we do get the contents.
    clipboard.request_text[paste_received, list(entry)];
  endfunction
  
  button.connect[ "clicked", paste_button_clicked, entry];

  label = gtk_label_new(str="Images can be transferred via the clipboard, too");
  vbox.pack_start[label,expand=%f,fill=%f,padding=0];

  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL,spacing= 4);
  hbox.set_border_width[8];
  vbox.pack_start[hbox, expand=%f,fill=%f,padding=0];

  //  Create the first image  
  image = gtk_image_new_from_icon_name ("dialog-warning", GTK.ICON_SIZE_BUTTON);
  ebox = gtk_event_box_new ();
  ebox.add[image];
  hbox.add[ebox];

  //  make ebox a drag source  
  targets = list();
  gtk_drag_source_set (ebox, GDK.BUTTON1_MASK, targets, GDK.ACTION_COPY);
  gtk_drag_source_add_image_targets (ebox);
  ebox.connect[ "drag-begin", drag_begin, image];
  ebox.connect[ "drag-data-get", drag_data_get, image];

  //  accept drops on ebox  
  gtk_drag_dest_set (ebox, GTK.DEST_DEFAULT_ALL, targets, GDK.ACTION_COPY);
  gtk_drag_dest_add_image_targets (ebox);
  ebox.connect[ "drag-data-received", drag_data_received, image];

  //  context menu on ebox  
  ebox.connect[ "button-press-event", button_press, image];

  //  Create the second image  
  image = gtk_image_new_from_icon_name ("process-stop", GTK.ICON_SIZE_BUTTON);
  ebox = gtk_event_box_new ();
  ebox.add[image];
  hbox.add[ebox];
  
  //  make ebox a drag source  
  gtk_drag_source_set (ebox, GDK.BUTTON1_MASK, targets, GDK.ACTION_COPY);
  gtk_drag_source_add_image_targets (ebox);
  ebox.connect[ "drag-begin", drag_begin, image];
  ebox.connect[ "drag-data-get",drag_data_get, image];

  //  accept drops on ebox  
  gtk_drag_dest_set (ebox, GTK.DEST_DEFAULT_ALL, targets, GDK.ACTION_COPY);
  gtk_drag_dest_add_image_targets (ebox);
  ebox.connect[ "drag-data-received", drag_data_received, image];

  //  context menu on ebox  
  ebox.connect[ "button-press-event", button_press, image];

  //  tell the clipboard manager to make the data persistent  
  GDK_SELECTION_CLIPBOARD=69 
  clipboard = gtk_clipboard_get (selection=GDK_SELECTION_CLIPBOARD);
  clipboard.set_can_store[list()];
  window.show_all[];
endfunction 

