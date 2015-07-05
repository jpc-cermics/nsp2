// GtkScrolledWindow OK
//----------------------------------------------------

function demo_scrolled_windows()
  dialog = gtkdialog_new()
  dialog.connect["delete_event", demo_delete];
  dialog.set_title["dialog"];
  scrolled_window= gtkscrolledwindow_new()
  scrolled_window.set_border_width[10]
  scrolled_window.set_policy[GTK.POLICY_AUTOMATIC, GTK.POLICY_AUTOMATIC]
  window_vbox = dialog.get_content_area[];
  window_vbox.pack_start[scrolled_window]
  scrolled_window.show[]
  table = gtk_grid_new();
  //table.set_row_spacings[10]
  //table.set_col_spacings[10]
  scrolled_window.add[table];
  table.show[]
  for i = 0:19
    for j = 0:19
      button = gtktogglebutton_new(label=sprintf("button (%d,%d)",i,j));
      table.attach[button,i,j,1,1];
      button.show[]
    end
  end
  dialog.add_button["Close",1];
  dialog.show_all[];
  response = dialog.run[]
  dialog.destroy[];
endfunction
