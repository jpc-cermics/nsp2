//  List Box
//
// GtkListBox allows lists with complicated layouts, using
// regular widgets supporting sorting and filtering.
//

function window = demo_listbox (do_widget)

  function y = gtk_message_row_sort (a, b, data)
  // return b->priv->message->time - a->priv->message->time;
  endfunction

  function row_activated (listbox, row)
  // gtk_message_row_expand (row);
  endfunction
  
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  if nargin >=1 then 
    window.set_screen[do_widget.get_screen []];
  end
  window.set_title[ "List Box"];
  window.set_default_size[ 400, 600];
  // window.connect[ "destroy", gtk_widget_destroyed, &window];

  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL,spacing= 12);
  window.add[vbox];
  label = gtk_label_new(str="Messages from Gtk+ and friends");
  vbox.pack_start[label, expand=%f, fill=%f, padding= 0];
  scrolled = gtk_scrolled_window_new ();
  scrolled.set_policy[GTK.POLICY_NEVER, GTK.POLICY_AUTOMATIC];
  vbox.pack_start[scrolled, expand=%t, fill=%t, padding=0];
  listbox = gtk_list_box_new ();
  scrolled.add[listbox];

  // listbox.set_sort_func[gtk_message_row_sort, listbox];
  listbox.set_activate_on_single_click[%f];
  listbox.connect[ "row-activated", row_activated];

  vbox.show_all[];
      
  for i=1:100 
    message = gtk_label_new(str=sprintf("Message %d",i));
    message.show[];
    listbox.add[message];
  end
  window.show[];
endfunction 

