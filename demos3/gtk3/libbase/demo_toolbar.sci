// Toolbar
// ---------------------------------

function [tree_view,list_store]=create_items_list()
  list_store = gtk_list_store_new (2, GTK_TYPE_TOOL_ITEM, G_TYPE_STRING);
  tree_view = gtk_tree_view_new_with_model (list_store);
  tree_view.insert_column_with_attributes[-1, "Tool Item", gtk_cell_renderer_text_new (),"text",1,NULL];
  cell = gtk_cell_renderer_toggle_new ();
  cell.connect["toggled",visibile_toggled,list(list_store)];

  gtk_tree_view_insert_column_with_data_func (GTK_TREE_VIEW (tree_view),
					      -1, "Visible",
					      cell,
					      set_visible_func, NULL, NULL);

  cell = gtk_cell_renderer_toggle_new ();
  cell.connect[ "toggled", expand_toggled, list(list_store)];
  gtk_tree_view_insert_column_with_data_func (GTK_TREE_VIEW (tree_view),
					      -1, "Expand",
					      cell,
					      set_expand_func, NULL, NULL);

  cell = gtk_cell_renderer_toggle_new ();
  cell.connect["toggled", homogeneous_toggled, list(list_store)];

  gtk_tree_view_insert_column_with_data_func (GTK_TREE_VIEW (tree_view),
					      -1, "Homogeneous",
					      cell,
					      set_homogeneous_func, NULL,NULL);

  cell = gtk_cell_renderer_toggle_new ();
  cell.connect["toggled", important_toggled, list(list_store)];

  gtk_tree_view_insert_column_with_data_func (GTK_TREE_VIEW (tree_view),
					      -1, "Important",
					      cell,
					      set_important_func, NULL, ...
						  NULL);
endfunction

function demo_toolbar()

  function set_toolbar_horizontal(_b,args)
    args(1).set_orientation[GTK.ORIENTATION_HORIZONTAL]
  endfunction
  function set_toolbar_vertical(_b,args)
    args(1).set_orientation[GTK.ORIENTATION_VERTICAL]
  endfunction
  function set_toolbar_icons(_b,args)
    args(1).set_style[GTK.TOOLBAR_ICONS]
  endfunction
  function set_toolbar_text(_b,args)
    args(1).set_style[GTK.TOOLBAR_TEXT]
  endfunction
  function set_toolbar_both(_b,args)
    args(1).set_style[GTK.TOOLBAR_BOTH]
  endfunction
  function set_toolbar_small_space(_b,args)
    args(1).insert_space[5]
  endfunction
  function set_toolbar_big_space(_b,args)
    args(1).insert_space[10]
  endfunction
  function set_toolbar_enable(_b,args)
    args(1).set_tooltips[%t]
  endfunction
  function set_toolbar_disable(_b,args)
    args(1).set_tooltips[%f]
  endfunction

  window = gtk_window_new();
  // window.connect["destroy", G_CALLBACK(gtk_main_quit), NULL];
  grid = gtk_grid_new ();
  window.add[grid];

  toolbar = gtk_toolbar_new ();
  grid.attach[toolbar, 0, 0, 2, 1];
  hbox1 = gtk_box_new (GTK.ORIENTATION_HORIZONTAL,spacing= 3);
  hbox1.set_border_width[5];
  hbox1.set_vexpand[%t];
  grid.attach[ hbox1, 1, 1, 1, 1];

  hbox2 = gtk_box_new (GTK.ORIENTATION_HORIZONTAL,spacing= 2);
  hbox2.set_border_width[5];
  hbox2.set_vexpand[%t];
  grid.attach[ hbox2, 1, 2, 1, 1];

  checkbox = gtk_check_button_new(mnemonic="_Vertical");
  hbox1.pack_start[checkbox,expand= %f,fill=%f,padding=0];
  // checkbox.connect["toggled",change_orientation, list(toolbar)];

  checkbox = gtk_check_button_new(mnemonic="_Show Arrow");
  checkbox.set_active[%t];
  hbox1.pack_start[ checkbox, expand= %f,fill=%f,padding=0];
  // checkbox.connect[ "toggled",change_show_arrow, list(toolbar)];

  checkbox = gtk_check_button_new(mnemonic="_Set Toolbar Style:");
  // checkbox.connect[ "toggled", set_toolbar_style_toggled, list(toolbar)];
  hbox1.pack_start[ checkbox,expand= %f,fill=%f,padding=0];

  option_menu = gtk_combo_box_text_new ();
  option_menu.set_sensitive[%f];
  checkbox.set_data[option_menu= option_menu];

  toolbar_styles = 'S'+string(1:5);

  for i=1:size(toolbar_styles,'*')
    option_menu.append_text[toolbar_styles(i)];
  end
  option_menu.set_active[toolbar.get_style[]];
  hbox2.pack_start[ option_menu,expand= %f,fill=%f,padding=0];
  // option_menu.connect["changed",change_toolbar_style, list(toolbar)];

  checkbox = gtk_check_button_new(mnemonic="_Set Icon Size:");
  // checkbox.connect[ "toggled", set_icon_size_toggled, list(toolbar)];
  hbox2.pack_start[ checkbox,expand= %f,fill=%f,padding=0];

  option_menu = gtk_combo_box_text_new ();
  checkbox.set_data[ option_menu=option_menu];
  option_menu.set_sensitive[%f];
  option_menu.append_text["small toolbar"];
  option_menu.append_text["large toolbar"];

  hbox2.pack_start[ option_menu,expand= %f,fill=%f,padding=0];
  // option_menu.connect[ "changed",icon_size_history_changed, list(toolbar)];

  scrolled_window = gtk_scrolled_window_new();//(NULL, NULL);
  scrolled_window.set_policy[GTK.POLICY_AUTOMATIC, GTK.POLICY_AUTOMATIC];
  scrolled_window.set_hexpand[%t];
  scrolled_window.set_vexpand[%t];
  grid.attach[ scrolled_window, 1, 3, 1, 1];

  // store = create_items_list (&treeview);
  // scrolled_window.add[treeview];

  item = gtk_tool_button_new();// (NULL, NULL);
  item.set_icon_name["document-new"];
  item.set_label["Custom label"];
  // add_item_to_list (store, item, "New");
  toolbar.insert[item,-1];
  // gdk_threads_add_timeout (3000, (GSourceFunc) timeout_cb, item);
  item.set_expand[%t];

  menu = gtk_menu_new ();
  for i = 0:20;
    text = sprintf("Menuitem %d", i);
    menuitem = gtk_menu_item_new(label=text);
    menuitem.show[];
    menu.append[menuitem];
  end

  item = gtk_menu_tool_button_new();//(NULL, NULL);
  item.set_icon_name[ "document-open"];
  item.set_label["Open"];
  item.set_menu[menu];
  //add_item_to_list (store, item, "Open");
  toolbar.insert[ item, -1];
  // gdk_threads_add_timeout (3000, (GSourceFunc) timeout_cb1, item);

  menu = gtk_menu_new ();
  for i = 0:20
    text = sprintf ("A%d", i);
    menuitem = gtk_menu_item_new(label=text);
    menuitem.show[];
    menu.append[ menuitem];
  end

  item = gtk_menu_tool_button_new();// (NULL, NULL);
  item.set_icon_name[ "go-previous"];
  item.set_label[ "Back"];
  item.set_menu[menu];
  // add_item_to_list (store, item, "BackWithHistory"];
  toolbar.insert[ item, -1];

  item = gtk_separator_tool_item_new ();
  // add_item_to_list (store, item, "-----");
  toolbar.insert[ item, -1];

  image = gtk_image_new_from_icon_name ("dialog-warning", GTK.ICON_SIZE_DIALOG);
  item = gtk_tool_item_new ();
  image.show[]
  item.add[image];
  // add_item_to_list (store, item, "(Custom Item)"];
  toolbar.insert[ item, -1];

  item = gtk_tool_button_new();//(NULL, NULL);
  item.set_icon_name[ "go-previous"];
  item.set_label[ "Back"];
  // add_item_to_list (store, item, "Back"];
  toolbar.insert[ item, -1];

  item = gtk_separator_tool_item_new ();
  // add_item_to_list (store, item, "-----");
  toolbar.insert[ item, -1];

  item = gtk_tool_button_new();// (NULL, NULL);
  item.set_icon_name[ "go-next"];
  item.set_label[ "Forward"];
  // add_item_to_list (store, item, "Forward"];
  toolbar.insert[ item, -1];

  item = gtk_toggle_tool_button_new ();
  item.set_label[ "Bold"];
  item.set_icon_name[ "format-text-bold"];
  // item.connect["toggled", bold_toggled];
  // add_item_to_list (store, item, "Bold"];
  toolbar.insert[ item, -1];
  item.set_sensitive[%f];

  item = gtk_separator_tool_item_new ();
  // add_item_to_list (store, item, "-----");
  toolbar.insert[ item, -1];
  item.set_expand[%t];
  item.set_draw[%f];

  item = gtk_radio_tool_button_new();// (NULL);
  item.set_label[ "Left"];
  item.set_icon_name[ "format-justify-left"];
  group = item.get_group[];

  // add_item_to_list (store, item, "Left"];
  toolbar.insert[ item, -1];

  // take care that group here is only given by an other
  // item
  item = gtk_radio_tool_button_new (group=item);
  item.set_label[ "Center"];
  item.set_icon_name[ "format-justify-center"];
  group = item.get_group[];
  // add_item_to_list (store, item, "Center"];
  toolbar.insert[ item, -1];

  item = gtk_radio_tool_button_new (group=item);
  item.set_label[ "Right"];
  item.set_icon_name[ "format-justify-right"];
  // add_item_to_list (store, item, "Right"];
  toolbar.insert[ item, -1];
  icon_widget=gtk_image_new_from_file (getenv("NSP")+"/demos3/gtk3/libbase/apple-red.png");
  item = gtk_tool_button_new (icon_widget=icon_widget,label= "_Apple");
  // add_item_to_list (store, item, "Apple"];
  toolbar.insert[ item, -1];
  item.set_use_underline[%t];

  // gicon = g_content_type_get_icon ("video/ogg");
  // image = gtk_image_new_from_gicon (gicon, GTK.ICON_SIZE_LARGE_TOOLBAR);
  // item = gtk_tool_button_new (icon_widget=image,label= "Video");
  // // add_item_to_list (store, item, "Video"];
  // toolbar.insert[ item, -1];

  image = gtk_image_new_from_icon_name ("utilities-terminal", GTK.ICON_SIZE_LARGE_TOOLBAR);
  item = gtk_tool_button_new (icon_widget=image,label= "Terminal");
  // add_item_to_list (store, item, "Terminal");
  toolbar.insert[ item, -1];

  image = gtk_spinner_new ();
  image.start[];
  item = gtk_tool_button_new (icon_widget=image,label= "Spinner");
  // add_item_to_list (store, item, "Spinner");
  toolbar.insert[ item, -1];

  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL, spacing=5);
  hbox.set_border_width[5];
  hbox.set_hexpand[%t];
  grid.attach[ hbox, 1, 4, 1, 1];

  button = gtk_button_new(label="Drag me to the toolbar");
  hbox.pack_start[ button,expand= %f,fill=%f,padding=0];

  label = gtk_label_new (str="Drop index:");
  hbox.pack_start[ label,expand= %f,fill=%f,padding=0];

  label = gtk_label_new ();
  hbox.pack_start[ label,expand= %f,fill=%f,padding=0];

  checkbox = gtk_check_button_new(mnemonic="_Right to left");
  if gtk_widget_get_default_direction () == GTK.TEXT_DIR_RTL then
    checkbox.set_active[%t];
  else
    checkbox.set_active[%f];
  end
  // checkbox.connect[ "toggled", rtl_toggled];

  hbox.pack_end[checkbox, expand= %f,fill=%f,padding=0];

  // gtk_drag_source_set (button, GDK_BUTTON1_MASK,
  // 		       target_table, G_N_ELEMENTS (target_table),
  // 		       GDK_ACTION_MOVE);
  // gtk_drag_dest_set (toolbar, GTK.DEST_DEFAULT_DROP,
  // 		     target_table, G_N_ELEMENTS (target_table),
  // 		     GDK_ACTION_MOVE);

  // toolbar.connect["drag_motion", toolbar_drag_motion];
  // toolbar.connect["drag_leave",toolbar_drag_leave];
  // toolbar.connect["drag_drop",toolbar_drag_drop];

  window.show_all[];
  // window.connect["delete_event",gtk_main_quit];
  // toolbar.connect["popup_context_menu", popup_context_menu];
endfunction
