// Dialog and Message Boxes
// Dialog widgets are used to pop up a transient window for user feedback.

function message_dialog_clicked (button, data)
  i= button.get_data['i']
  mes = sprintf("This message box has been popped up the following\nnumber of times:\n\n %d",i);
  dialog = gtkmessagedialog_new (parent= data(1), 
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
  dialog = gtkdialog_new(title= "Interactive Dialog",parent=data(1),
			 flags = ior(GTK.DIALOG_MODAL,GTK.DIALOG_DESTROY_WITH_PARENT),
			 buttons = ["gtk-ok","_Non-stock Button"]);
  // default return values for buttons(i) is i 
  // a rajouter : GTK.RESPONSE_OK, "_Non-stock
  // Button", GTK.RESPONSE_CANCEL,
  ok_rep = 1;
  
  hbox = gtkhbox_new(homogeneous=%f,spacing=8);
  hbox.set_border_width[  8]
  dialog.vbox.pack_start[ hbox,expand=%f,fill=%f,padding=0];
  // GTK.STOCK_DIALOG_QUESTION
  stock = gtkimage_new("stock","gtk-dialog-question" , GTK.ICON_SIZE_DIALOG);
  hbox.pack_start[ stock,expand=%f,fill=%f,padding=0]

  table = gtktable_new(rows=2,columns=2,homogeneous=%f);
  table.set_row_spacings[  4]
  table.set_col_spacings[  4]
  hbox.pack_start[ table,expand=%t,fill=%t,padding=0]
  label = gtklabel_new(mnemonic="_Entry 1");
  table.attach_defaults[ label,  0, 1, 0, 1]
  local_entry1 = gtkentry_new ();
  local_entry1.set_text[  entry1.get_text[]]
  table.attach_defaults[  local_entry1, 1, 2, 0, 1]
  label.set_mnemonic_widget[ local_entry1]

  label = gtklabel_new(mnemonic="E_ntry 2");
  table.attach_defaults[ label,  0, 1, 1, 2]

  local_entry2 = gtkentry_new ();
  local_entry2.set_text[  entry2.get_text[]]
  table.attach_defaults[  local_entry2, 1, 2, 1, 2]
  label.set_mnemonic_widget[  local_entry2]
  
  hbox.show_all[];
  response = dialog.run[];

  if response == ok_rep; // GTK.RESPONSE_OK 
    entry1.set_text[  local_entry1.get_text[]]
    entry2.set_text[  local_entry2.get_text[]]
  end
  dialog.destroy[];
endfunction 

function demo_dialogs () 
  window = gtkwindow_new ();//GTK.WINDOW_TOPLEVEL);
  window.set_title[  "Dialogs"]

  // window.connect[  "destroy", hide];
  window.set_border_width[  8]

  frame = gtkframe_new(label="Dialogs");
  window.add[  frame]

  vbox = gtkvbox_new(homogeneous=%f,spacing=8);
  vbox.set_border_width[  8]
  frame.add[  vbox]

  // Standard message dialog 
  hbox = gtkhbox_new(homogeneous=%f,spacing=8);
  vbox.pack_start[ hbox,expand=%f,fill=%f,padding=0]
  button = gtkbutton_new(mnemonic="_Message Dialog");
  button.set_data[i=1]
  button.connect[  "clicked", message_dialog_clicked,list(window)]
  hbox.pack_start[ button,expand=%f,fill=%f,padding=0]

  vbox.pack_start[  gtkhseparator_new (),expand=%f,fill=%f,padding=0];

  // Interactive dialog
  hbox = gtkhbox_new(homogeneous=%f,spacing=8);
  vbox.pack_start[ hbox,expand=%f,fill=%f,padding=0]
  vbox2 = gtkvbox_new(homogeneous=%f,spacing=0);

  button = gtkbutton_new(mnemonic="_Interactive Dialog");
  hbox.pack_start[ vbox2,expand=%f,fill=%f,padding=0]
  vbox2.pack_start[ button,expand=%f,fill=%f,padding=0]

  table = gtktable_new(rows=2,columns=2,homogeneous=%f);
  table.set_row_spacings[  4]
  table.set_col_spacings[  4]
  hbox.pack_start[ table,expand=%f,fill=%f,padding=0]

  label = gtklabel_new(mnemonic="_Entry 1");
  table.attach_defaults[  label, 0, 1, 0, 1]
  entry1 = gtkentry_new ();
  table.attach_defaults[  entry1, 1, 2, 0, 1]
  label.set_mnemonic_widget[  entry1]

  label = gtklabel_new(mnemonic="E_ntry 2");
  
  table.attach_defaults[  label, 0, 1, 1, 2]

  entry2 = gtkentry_new ();
  table.attach_defaults[  entry2, 1, 2, 1, 2]
  label.set_mnemonic_widget[  entry2]

  button.connect[  "clicked", interactive_dialog_clicked,list(window,entry1,entry2)]
  
  window.show_all[];
endfunction 
	






