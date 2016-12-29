// Entry/Delayed Search Entry
//
// GtkSearchEntry sets up GtkEntries ready for search. Search entries
// have their "changed" signal delayed and should be used
// when the searched operation is slow such as loads of entries
// to search, or online searches.

function window=demo_search_entry2 (do_widget)

  
  function search_changed_cb (entry, result_label)
    text = entry.get_text [];
    printf("search changed: %s\n", text);
    result_label.set_text[ text];
  endfunction

  function changed_cb (editable)
    text = editable.get_text [];
    printf ("changed: %s\n", text);
  endfunction

  function y= window_key_press_event_cb (widget,event,bar)
    y = bar.handle_event[event];
  endfunction

  function search_changed (entry, label)
    label.set_text[ "search-changed"];
  endfunction

  function next_match (entry, label)
    label.set_text[ "next-match"];
  endfunction

  function previous_match (entry, label)
    label.set_text[ "previous-match"];
  endfunction

  function stop_search (entry, label)
    label.set_text[ "stop-search"];
  endfunction
  
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_title[ "Delayed Search Entry"];
  
  if nargin >= 1 then 
    window.set_screen[ do_widget.get_screen []];
    window.set_transient_for[do_widget];
  end
  
  window.set_resizable[%t];
  window.set_size_request[ 200, -1];

  // window.connect[ "destroy", gtk_widget_destroyed, window];

  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL,spacing= 0);
  window.add[vbox];
  vbox.set_border_width[0];

  entry = gtk_search_entry_new ();
  container = gtk_box_new (GTK.ORIENTATION_HORIZONTAL,spacing= 10);
  container.set_halign[GTK.ALIGN_CENTER];
  container.pack_start[entry, expand=%f, fill=%f,padding=0];
  searchbar = gtk_search_bar_new ();
  searchbar.connect_entry[entry];
  searchbar.set_show_close_button[%f];
  searchbar.add[container];
  vbox.pack_start[searchbar, expand=%f, fill=%f,padding=0];

  //  Hook the search bar to key presses  
  window.connect[ "key-press-event",
		  window_key_press_event_cb, searchbar];

  //  Help  
  label = gtk_label_new(str="Start Typing to search");
  vbox.pack_start[label, expand=%t, fill= %t, padding=0];

  //  Toggle button  
  button = gtk_toggle_button_new(label="Search");
  button.bind_property[ "active", searchbar,...
		    "search-mode-enabled","BIDIRECTIONAL"];
  vbox.pack_start[button, expand=%t, fill=%t,padding=0];

  //  Result  
  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL,spacing= 10);
  vbox.pack_start[hbox, expand=%t, fill=%t,padding=0];
  hbox.set_border_width[0];

  label = gtk_label_new(str="Result:");
  label.set_xalign[ 0.0];
  label.set_margin_start[6];
  hbox.pack_start[label,expand= %t, fill=%t,padding=0];

  label = gtk_label_new(str="");
  hbox.pack_start[label,expand= %t, fill=%t,padding=0];

  entry.connect[ "search-changed",
		 search_changed_cb, label];
  entry.connect[ "changed",
		 changed_cb, label];

  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL,spacing= 10);
  vbox.pack_start[hbox,expand= %t, fill=%t,padding=0];
  hbox.set_border_width[0];

  label = gtk_label_new(str="Signal:");
  label.set_xalign[ 0.0];
  label.set_margin_start[6];
  hbox.pack_start[label,expand= %t, fill=%t,padding=0];

  label = gtk_label_new(str="");
  hbox.pack_start[label,expand= %t, fill=%t,padding=0];

  entry.connect[ "search-changed",
		 search_changed, label];
  entry.connect[ "next-match",
		 next_match, label];
  entry.connect[ "previous-match",
		 previous_match, label];
  entry.connect[ "stop-search",
		 stop_search, label];

  window.show_all[];
endfunction
