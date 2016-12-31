// Dialog and Message Boxes
//
// Dialog widgets are used to pop up a transient window for user feedback.

function window=demo_dialog(do_widget)

  function message_dialog_clicked (button, data)
    i= button.get_data['i']
    mes = sprintf("This message box has been popped up the following\nnumber of times:\n\n %d",i);
    dialog = gtk_message_dialog_new (parent= data(1),
    flags= ior(GTK.DIALOG_MODAL,GTK.DIALOG_DESTROY_WITH_PARENT),
    type= GTK.MESSAGE_INFO,
    buttons= GTK.BUTTONS_OK,
    message = mes);
    dialog.run[];
    dialog.destroy[];
    button.set_data[i=i+1]
  endfunction

  function interactive_dialog_clicked (button,data)
    entry1 = data(2);
    entry2 = data(3);
    dialog = gtk_dialog_new(title= "Interactive Dialog",parent=data(1),
    flags = ior(GTK.DIALOG_MODAL,GTK.DIALOG_DESTROY_WITH_PARENT),
    buttons = ["gtk-ok","_Non-stock Button"]);
    // default return values for buttons(i) is i
    // a rajouter : GTK.RESPONSE_OK, "_Non-stock
    // Button", GTK.RESPONSE_CANCEL,
    ok_rep = 1;

    hbox = gtk_box_new("horizontal",spacing=8);
    hbox.set_border_width[  8]
    vbox = dialog.get_content_area[];
    vbox.pack_start[ hbox,expand=%f,fill=%f,padding=0];
    // GTK.STOCK_DIALOG_QUESTION
    // XXX stock = gtk_image_new("stock","gtk-dialog-question" , GTK.ICON_SIZE_DIALOG);
    //hbox.pack_start[ stock,expand=%f,fill=%f,padding=0]

    table = gtk_grid_new();
    table.set_row_spacing[  4]
    table.set_column_spacing[  4]
    hbox.pack_start[ table,expand=%t,fill=%t,padding=0]
    label = gtk_label_new(mnemonic="_Entry 1");
    table.attach[ label, 0, 0,1, 1]
    local_entry1 = gtk_entry_new ();
    local_entry1.set_text[  entry1.get_text[]]
    table.attach[ local_entry1, 1, 0, 1, 1]
    label.set_mnemonic_widget[ local_entry1]
    label = gtk_label_new(mnemonic="E_ntry 2");
    table.attach[ label,  0, 1, 1, 1]

    local_entry2 = gtk_entry_new ();
    local_entry2.set_text[  entry2.get_text[]]
    table.attach[ local_entry2, 1, 1,1,1]
    label.set_mnemonic_widget[  local_entry2]

    hbox.show_all[];
    response = dialog.run[];

    if response == ok_rep; // GTK.RESPONSE_OK
      entry1.set_text[  local_entry1.get_text[]]
      entry2.set_text[  local_entry2.get_text[]]
    end
    dialog.destroy[];
  endfunction
  
  window = gtk_window_new ();//GTK.WINDOW_TOPLEVEL);
  window.set_title[  "Dialogs"]

  // window.connect[  "destroy", hide];
  window.set_border_width[  8]

  frame = gtk_frame_new(label="Dialogs");
  window.add[  frame]

  vbox = gtk_box_new("vertical",spacing=8);
  vbox.set_border_width[  8]
  frame.add[  vbox]

  // Standard message dialog
  hbox = gtk_box_new("horizontal",spacing=8);
  vbox.pack_start[ hbox,expand=%f,fill=%f,padding=0]
  button = gtk_button_new(mnemonic="_Message Dialog");
  button.set_data[i=1]
  button.connect[  "clicked", message_dialog_clicked,list(window)]
  hbox.pack_start[ button,expand=%f,fill=%f,padding=0]

  vbox.pack_start[  gtk_separator_new("horizontal"),expand=%f,fill=%f,padding=0];

  // Interactive dialog
  hbox = gtk_box_new("horizontal",spacing=8);
  vbox.pack_start[ hbox,expand=%f,fill=%f,padding=0]
  vbox2 = gtk_box_new("vertical",spacing=0);

  button = gtk_button_new(mnemonic="_Interactive Dialog");
  hbox.pack_start[ vbox2,expand=%f,fill=%f,padding=0]
  vbox2.pack_start[ button,expand=%f,fill=%f,padding=0]

  table = gtk_grid_new();
  table.set_row_spacing[  4]
  table.set_column_spacing[  4]
  hbox.pack_start[ table,expand=%f,fill=%f,padding=0]

  label = gtk_label_new(mnemonic="_Entry 1");
  table.attach[ label, 0,0, 1,1]
  entry1 = gtk_entry_new ();
  table.attach[  entry1, 1, 0,1, 1]
  label.set_mnemonic_widget[  entry1]

  label = gtk_label_new(mnemonic="E_ntry 2");
  table.attach[  label, 0, 1, 1, 1]

  entry2 = gtk_entry_new ();
  table.attach[  entry2, 1, 1,1,1]
  label.set_mnemonic_widget[  entry2]
  button.connect[  "clicked", interactive_dialog_clicked,list(window,entry1,entry2)];

  window.show_all[];
endfunction
