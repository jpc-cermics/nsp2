// Pickers
//
// These widgets are mainly intended for use in preference dialogs.
// They allow to select colors, fonts, files, directories and applications.

function demo_pickers()
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  // window.set_screen[do_widget.get_screen[]];
  window.set_title["Pickers"];
  // window.connect[ "destroy", gtk_widget_destroyed];
  window.set_border_width [ 10];

  table = gtk_grid_new ();
  table.set_row_spacing [ 3];
  table.set_column_spacing [ 10];
  window.add [ table];
  table.set_border_width [10];

  label = gtk_label_new(str="Color:");
  label.set_halign [ GTK.ALIGN_START];
  label.set_valign [ GTK.ALIGN_CENTER];
  label.set_hexpand [ %t];
  picker = gtk_color_button_new ();
  table.attach [ label, 0, 0, 1, 1];
  table.attach [ picker, 1, 0, 1, 1];

  label = gtk_label_new (str="Font:");
  label.set_halign [ GTK.ALIGN_START];
  label.set_valign [ GTK.ALIGN_CENTER];
  label.set_hexpand [ %t];
  picker = gtk_font_button_new ();
  table.attach [ label, 0, 1, 1, 1];
  table.attach [ picker, 1, 1, 1, 1];

  label = gtk_label_new (str="File:");
  label.set_halign [ GTK.ALIGN_START];
  label.set_valign [ GTK.ALIGN_CENTER];
  label.set_hexpand [ %t];
  picker = gtk_file_chooser_button_new ("Pick a File", GTK.FILE_CHOOSER_ACTION_OPEN);
  table.attach [ label, 0, 2, 1, 1];
  table.attach [ picker, 1, 2, 1, 1];

  label = gtk_label_new (str="Folder:");
  label.set_halign [ GTK.ALIGN_START];
  label.set_valign [ GTK.ALIGN_CENTER];
  picker = gtk_file_chooser_button_new ("Pick a Folder",GTK.FILE_CHOOSER_ACTION_SELECT_FOLDER);
  table.attach [ label, 0, 3, 1, 1];
  table.attach [ picker, 1, 3, 1, 1];

  label = gtk_label_new (str="Mail:");
  label.set_halign [ GTK.ALIGN_START];
  label.set_valign [ GTK.ALIGN_CENTER];
  label.set_hexpand [%t];
  picker = gtk_app_chooser_button_new ("x-scheme-handler/mailto");
  picker.set_show_dialog_item[%t];
  table.attach [ label, 0, 4, 1, 1];
  table.attach [ picker, 1, 4, 1, 1];

  window.show_all[];
endfunction
