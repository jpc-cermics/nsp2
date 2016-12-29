// Entry/Search Entry
//
// GtkEntry allows to display icons and progress information.
// This demo shows how to use these features in a search entry.

//static guint search_progress_id = 0;
//static guint finish_search_id = 0;

function show_find_button (notebook)
  notebook.set_current_page[0];
endfunction

function show_cancel_button (void)
  notebook.set_current_page[1];
endfunction

function y=search_progress (data)
  gtk_entry_progress_pulse (data);
  y= G_SOURCE_CONTINUE;
endfunction

function search_progress_done (entry)
  entry.set_progress_fraction[ 0.0];
endfunction

function y = finish_search (button,notebook)
  show_find_button (notebook);
  g_source_remove (search_progress_id);
  search_progress_id = 0;
  y = G_SOURCE_REMOVE;
endfunction

function y = start_search_feedback (data)
  search_progress_id = g_timeout_add_full (G_PRIORITY_DEFAULT, 100,search_progress, data,search_progress_done);
  y = G_SOURCE_REMOVE;
endfunction

function start_search (button, entry)
  show_cancel_button ();
  search_progress_id = g_timeout_add_seconds (1, start_search_feedback, entry);
  finish_search_id = g_timeout_add_seconds (15, finish_search, button);
endfunction

function stop_search (button, data)
  g_source_remove (finish_search_id);
  finish_search (button);
endfunction

function clear_entry (entry)
  entry.set_text[ ""];
endfunction

function search_by_name (item,  entry)
  if %f then
    entry.set_icon_tooltip_text[
	GTK.ENTRY_ICON_PRIMARY,
	"Search by name\n" + 
	"Click here to change the search type"];
  end
  entry.set_placeholder_text[ "name"];
endfunction

function search_by_description (item,  entry)
  if %f then 
    entry.set_icon_tooltip_text[
	GTK.ENTRY_ICON_PRIMARY,
	"Search by description\n" + 
	"Click here to change the search type"];
  end
  entry.set_placeholder_text[ "description"];
endfunction

function search_by_file (item,   entry)
  if %f then 
    entry.set_icon_tooltip_text[
	GTK.ENTRY_ICON_PRIMARY,
	"Search by file name\n" + 
	"Click here to change the search type"];
  end
  entry.set_placeholder_text[ "file name"];
endfunction

function menu=create_search_menu (entry)
  menu = gtk_menu_new ();

  item = gtk_menu_item_new(mnemonic="Search by _name");
  item.connect[ "activate", search_by_name, entry];
  menu.append[ item];

  item = gtk_menu_item_new(mnemonic="Search by _description");
  item.connect[ "activate", search_by_description, entry];
  menu.append[ item];

  item = gtk_menu_item_new(mnemonic="Search by _file name");
  item.connect[ "activate", search_by_file, entry];
  menu.append[ item];

  menu.show_all[];
endfunction

function icon_press_cb (entry, position, event, data)
  if (position == GTK.ENTRY_ICON_PRIMARY)
    // gtk_menu_popup (menu, NULL, NULL, NULL, NULL, event.button, event.time);
  end
endfunction

function activate_cb (entry, button)
  if (search_progress_id <> 0)
    return;
  end
  start_search (button, entry);
endfunction

function search_entry_destroyed (widget)
  if (finish_search_id <> 0)
    g_source_remove (finish_search_id);
    finish_search_id = 0;
  end

  if (search_progress_id <> 0)
    g_source_remove (search_progress_id);
    search_progress_id = 0;
  end
  // window = NULL;
endfunction

function entry_populate_popup (entry,  menu, user_data)
  has_text = entry.get_text_length [] > 0;
  
  item = gtk_separator_menu_item_new ();
  item.show[];
  menu.append[ item];
  
  item = gtk_menu_item_new(mnemonic="C_lear");
  item.show[];
  g_signal_connect_swapped (item, "activate",
  clear_entry, entry);
  menu.append[ item];
  item.set_sensitive[has_text];
  
  search_menu = create_search_menu (entry);
  item = gtk_menu_item_new_with_label ("Search by");
  item.show[];
  item.set_submenu[search_menu];
  menu.append[ item];
endfunction

function window=demo_search_entry (do_widget)
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  if nargin >= 1 then   window.set_screen[ do_widget.get_screen []];end

  window.set_title[ "Search Entry"];
  window.set_resizable[%f];
  // window.connect[ "destroy", search_entry_destroyed, &window];

  vbox = gtk_box_new (GTK.ORIENTATION_VERTICAL,spacing= 5);
  window.add[vbox];
  vbox.set_border_width[5];
  
  label = gtk_label_new();
  label.set_markup[ "Search entry demo"];
  vbox.pack_start[label,expand= %f, fill=%f,padding=0];

  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL,spacing= 10);
  vbox.pack_start[hbox,expand= %t, fill=%t,padding=0];
  hbox.set_border_width[0];

  //  Create our entry  
  entry = gtk_search_entry_new ();
  hbox.pack_start[entry,expand= %f, fill=%f,padding=0];

  //  Create the find and cancel buttons  
  notebook = gtk_notebook_new ();
  notebook.set_show_tabs[%f];
  notebook.set_show_border[%f];
  hbox.pack_start[notebook,expand= %f, fill=%f,padding=0];

  find_button = gtk_button_new(label="Find");
  find_button.connect[ "clicked", start_search, entry];
  notebook.append_page[find_button];
  find_button.show[];

  cancel_button = gtk_button_new(label="Cancel");
  cancel_button.connect[ "clicked",stop_search];
  notebook.append_page[cancel_button];
  cancel_button.show[];

  //  Set up the search icon  
  search_by_name ("", entry);

  //  Set up the clear icon  
  entry.connect[ "icon-press", icon_press_cb];
  entry.connect[ "activate", activate_cb];

  //  Create the menu  
  menu = create_search_menu (entry);
  menu.attach_to_widget[entry];

  //  add accessible alternatives for icon functionality  
  entry.set_property["populate-all" , %t]
  entry.connect[ "populate-popup", entry_populate_popup];
  window.show_all[];
endfunction
