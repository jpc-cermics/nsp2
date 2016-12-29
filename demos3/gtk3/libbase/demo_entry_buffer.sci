// Entry/Entry Buffer
//
// GtkEntryBuffer provides the text content in a GtkEntry.
// Applications can provide their own buffer implementation,
// e.g. to provide secure handling for passwords in memory.

function window=demo_entry_buffer (do_widget) 

  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  if nargin >= 1 then   window.set_screen[ do_widget.get_screen []];end
  window.set_title[ "Entry Buffer"];
  window.set_resizable[%f];
  //window.connect[ "destroy", gtk_widget_destroyed, &window];

  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL, spacing=5);
  window.add[vbox];
  vbox.set_border_width[5];

  label = gtk_label_new();
  label.set_markup[
      "Entries share a buffer. Typing in one is reflected in the other."];
  vbox.pack_start[label, expand=%f, fill=%f,padding=0];

  //  Create a buffer  
  buffer = gtk_entry_buffer_new ("",0);

  //  Create our first entry  
  entry = gtk_entry_new_with_buffer (buffer);
  vbox.pack_start[entry, expand=%f, fill=%f,padding=0];

  //  Create the second entry  
  entry = gtk_entry_new_with_buffer (buffer);
  entry.set_visibility[%f];
  vbox.pack_start[entry, expand=%f, fill=%f,padding=0];

  window.show_all[];
endfunction
