// Overlay/Interactive Overlay
//
// Shows widgets in static positions over a main widget.
//
// The overlayed widgets can be interactive controls such
// as the entry in this example, or just decorative, like
// the big blue label.

function window = demo_overlay (do_widget)

  function do_number (button, entry)
    entry.set_text[ button.get_label []];
  endfunction
  
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_default_size[500, 510];
  window.set_title[ "Interactive Overlay"];
  
  overlay = gtk_overlay_new ();
  grid = gtk_grid_new ();
  overlay.add[grid];

  entry = gtk_entry_new ();
  for  j = 0:5
    for i = 0:5
      text = sprintf("%d", 5*j + i);
      button = gtk_button_new(label=text);
      button.set_hexpand[%t];
      button.set_vexpand[%t];
      button.connect [ "clicked", do_number, entry];
      grid.attach[button, i, j, 1, 1];
    end
  end
  
  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL, spacing=10);
  overlay.add_overlay[vbox];
  overlay.set_overlay_pass_through[vbox, %t];
  vbox.set_halign[GTK.ALIGN_CENTER];
  vbox.set_valign[GTK.ALIGN_CENTER];

  label = gtk_label_new(str="<span foreground=''blue'' weight=''ultrabold'' font=''40''>Numbers</span>");
  label.set_use_markup[%t];
  vbox.pack_start[label,expand=%f,fill=%f,padding= 8];

  entry.set_placeholder_text[ "Your Lucky Number"];
  vbox.pack_start[entry,expand=%f,fill=%f,padding= 8];
  
  window.add[overlay];
  // window.connect[ "destroy", gtk_widget_destroyed, &window];
  overlay.show_all[];
  window.show[];
endfunction

