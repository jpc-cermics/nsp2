
function demo_color_picker ()
  window = gtk_window_new(type=GTK.WINDOW_TOPLEVEL);
  // window.set_screen[widget.get_screen[]];
  // window.connect["destroy",gtk_widget_destroyed,list(window)];
  window.set_title["GtkColorButton"];
  window.set_border_width[0];
  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL,spacing= 8);
  hbox.set_border_width[8];
  window.add[hbox];
  label = gtk_label_new (str="Pick a color");
  hbox.add[label];
  picker = gtk_color_button_new ();
  picker.set_use_alpha[%t];
  hbox.add[picker];
  window.show_all[];
endfunction
