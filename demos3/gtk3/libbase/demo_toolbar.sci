// Toolbar

function demo_toolbar()

  function [treeview,list_store]=create_items_list()

    function item_toggled( cell, path, data)
    // called when a toogle button is activated in the treeview
      path = gtk_tree_path_new(path);
      model=data(2);
      iter = model.get_iter[path]
      item = model.get_value[iter,0];
      str=sprintf("rep=item.get_%s[]",data(1));
      execstr(str);
      rep=~rep;
      str=sprintf("item.set_%s[rep]",data(1));
      execstr(str);
      cell.set_active[rep];
      model.row_changed[path,iter];
    endfunction

    function set_render_func(tree_column, cell, model, iter, data)
    // item is the tool widget.
    // we get the visibility status of the item and set the cell value
    // accordingly
      item=model.get_value[iter,0]
      str=sprintf("rep=item.get_%s[]",data(1));
      execstr(str);
      cell.set_active[rep];
    endfunction

    // the model contains a tool_item and a string
    item = gtk_tool_item_new();
    fls = list(list(item),"name")
    list_store = gtk_list_store_new(fls,%f);
    treeview = gtk_tree_view_new_with_model (list_store);

    col_id=1;
    renderer = gtk_cell_renderer_text_new ();
    //renderer.connect[  "edited",  cell_edited,list(model)]
    renderer.set_data[column=col_id ];
    attrs= hash_create(text= col_id, editable= col_id);
    col= gtk_tree_view_column_new(title="ToolItem",renderer=renderer,attrs=attrs);
    treeview.append_column[col];

    for str=["visible","expand","homogeneous","is_important"]
      renderer = gtk_cell_renderer_toggle_new ();
      renderer.connect["toggled", item_toggled , list(str,list_store)];
      treeview.insert_column_with_data_func[ -1, capitalize(str), renderer, set_render_func,list(str)];
    end

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

  //---- vertical

  function change_orientation ( button, data)
    toolbar=data(1);
    grid = toolbar.get_parent[];
    if button.get_active[] then
      orientation = GTK.ORIENTATION_VERTICAL;
    else
      orientation = GTK.ORIENTATION_HORIZONTAL;
    end;
    grid.remove[toolbar];
    toolbar.set_orientation[orientation];
    if orientation == GTK.ORIENTATION_HORIZONTAL then
      toolbar.set_hexpand[%t];
      toolbar.set_vexpand[%f];
      grid.attach[toolbar, 0, 0, 2, 1];
    else
      toolbar.set_hexpand[%f];
      toolbar.set_vexpand[%t];
      grid.attach[toolbar, 0, 0, 1, 5];
    end
  endfunction

  checkbox = gtk_check_button_new(mnemonic="_Vertical");
  hbox1.pack_start[checkbox,expand= %f,fill=%f,padding=0];
  checkbox.connect["toggled",change_orientation, list(toolbar)];

  //---- show arrow
  function change_show_arrow (button, data)
    toolbar = data(1)
    toolbar.set_show_arrow[button.get_active[]];
  endfunction

  checkbox = gtk_check_button_new(mnemonic="_Show Arrow");
  checkbox.set_active[%t];
  checkbox.connect[ "toggled",change_show_arrow, list(toolbar)];
  hbox1.pack_start[ checkbox, expand= %f,fill=%f,padding=0];

  //---- toolbar style

  function set_toolbar_style_toggled (button, data)
    toolbar = data(1);
    option_menu = button.get_data["option_menu"];
    if button.get_active[] then
      style = option_menu.get_active[];
      toolbar.set_style[style];
      option_menu.set_sensitive[%t];
    else
      toolbar.unset_style[];
      option_menu.set_sensitive[%f];
    end
  endfunction

  checkbox = gtk_check_button_new(mnemonic="_Set Toolbar Style:");
  checkbox.connect[ "toggled", set_toolbar_style_toggled, list(toolbar)];
  hbox1.pack_start[ checkbox,expand= %f,fill=%f,padding=0];

  //---- the toolbar_style options

  option_menu = gtk_combo_box_text_new ();
  option_menu.set_sensitive[%f];
  checkbox.set_data[option_menu= option_menu];
  toolbar_styles =["icons","text","both (vertical)","both (horizontal)"];
  for i=1:size(toolbar_styles,'*')
    option_menu.append_text[toolbar_styles(i)];
  end
  option_menu.set_active[toolbar.get_style[]];

  function change_toolbar_style (option_menu, data)
    toolbar = data(1);
    style = option_menu.get_active[];
    toolbar.set_style[style];
  endfunction

  option_menu.connect["changed",change_toolbar_style, list(toolbar)];
  hbox2.pack_start[ option_menu,expand= %f,fill=%f,padding=0];

  //---- icon-size

  function set_icon_size_toggled (button, data)
    toolbar = data(1);
    option_menu = button.get_data["option_menu"];
    if button.get_active[] then
      if option_menu.get_active[] == 0 then
        icon_size = GTK.ICON_SIZE_SMALL_TOOLBAR;
      else
        icon_size = GTK.ICON_SIZE_LARGE_TOOLBAR;
      end
      toolbar.set_icon_size[icon_size];
      option_menu.set_sensitive[%t];
    else
      toolbar.unset_icon_size[];
      option_menu.set_sensitive[%f];
    end
  endfunction

  checkbox = gtk_check_button_new(mnemonic="_Set Icon Size:");
  checkbox.connect[ "toggled", set_icon_size_toggled, list(toolbar)];
  hbox2.pack_start[ checkbox,expand= %f,fill=%f,padding=0];

  option_menu = gtk_combo_box_text_new ();
  checkbox.set_data[ option_menu=option_menu];
  option_menu.set_sensitive[%f];
  option_menu.append_text["small toolbar"];
  option_menu.append_text["large toolbar"];

  function icon_size_history_changed (menu, data)
    toolbar = data(1);
    if menu.get_active[] == 0 then
      icon_size = GTK.ICON_SIZE_SMALL_TOOLBAR;
    else
      icon_size = GTK.ICON_SIZE_LARGE_TOOLBAR;
    end
    toolbar.set_icon_size[ icon_size];
  endfunction

  hbox2.pack_start[ option_menu,expand= %f,fill=%f,padding=0];
  option_menu.connect[ "changed",icon_size_history_changed, list(toolbar)];

  //--- a scrolled window

  scrolled_window = gtk_scrolled_window_new();//(NULL, NULL);
  scrolled_window.set_policy[GTK.POLICY_AUTOMATIC, GTK.POLICY_AUTOMATIC];
  scrolled_window.set_hexpand[%t];
  scrolled_window.set_vexpand[%t];
  grid.attach[ scrolled_window, 1, 3, 1, 1];

  [treeview,store]=create_items_list();
  scrolled_window.add[treeview];

  //--- document-new

  item = gtk_tool_button_new();
  item.set_icon_name["document-new"];
  item.set_label["Custom label"];
  store.append[list(list(item),"New")];
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

  //--- document-open

  item = gtk_menu_tool_button_new();//(NULL, NULL);
  item.set_icon_name[ "document-open"];
  item.set_label["Open"];
  item.set_menu[menu];
  store.append[list(list(item),"Open")];
  toolbar.insert[ item, -1];
  // gdk_threads_add_timeout (3000, (GSourceFunc) timeout_cb1, item);

  menu = gtk_menu_new ();
  for i = 0:20
    text = sprintf ("A%d", i);
    menuitem = gtk_menu_item_new(label=text);
    menuitem.show[];
    menu.append[ menuitem];
  end

  //--- go-previous

  item = gtk_menu_tool_button_new();// (NULL, NULL);
  item.set_icon_name[ "go-previous"];
  item.set_label[ "Back"];
  item.set_menu[menu];
  store.append[list(list(item), "BackWithHistory")];
  toolbar.insert[ item, -1];

  //--- separator

  item = gtk_separator_tool_item_new ();
  store.append[list(list(item), "-----")];
  toolbar.insert[ item, -1];

  //--- image "dialog-warning"

  image = gtk_image_new_from_icon_name ("dialog-warning", GTK.ICON_SIZE_DIALOG);
  item = gtk_tool_item_new ();
  image.show[]
  item.add[image];
  store.append[list(list(item), "(Custom Item)")];
  toolbar.insert[ item, -1];

  //--- go-previous

  item = gtk_tool_button_new();//(NULL, NULL);
  item.set_icon_name[ "go-previous"];
  item.set_label[ "Back"];
  store.append[list(list(item), "Back")];
  toolbar.insert[ item, -1];

  //--- separator

  item = gtk_separator_tool_item_new ();
  store.append[list(list(item), "-----")];
  toolbar.insert[ item, -1];

  //--- go-next

  item = gtk_tool_button_new();// (NULL, NULL);
  item.set_icon_name[ "go-next"];
  item.set_label[ "Forward"];
  store.append[list(list(item), "Forward")];
  toolbar.insert[ item, -1];

  //--- toggle_tool

  item = gtk_toggle_tool_button_new ();
  item.set_label[ "Bold"];
  item.set_icon_name[ "format-text-bold"];
  // item.connect["toggled", bold_toggled];
  store.append[list(list(item), "Bold")];
  toolbar.insert[ item, -1];
  item.set_sensitive[%f];

  item = gtk_separator_tool_item_new ();
  store.append[list(list(item), "-----")];
  toolbar.insert[ item, -1];
  item.set_expand[%t];
  item.set_draw[%f];

  //---- radio tool

  item = gtk_radio_tool_button_new();// (NULL);
  item.set_label[ "Left"];
  item.set_icon_name[ "format-justify-left"];
  group = item.get_group[];

  store.append[list(list(item), "Left")];
  toolbar.insert[ item, -1];

  // take care that group here is only given by an other item
  item = gtk_radio_tool_button_new (group=item);
  item.set_label[ "Center"];
  item.set_icon_name[ "format-justify-center"];
  group = item.get_group[];
  store.append[list(list(item), "Center")];
  toolbar.insert[ item, -1];

  item = gtk_radio_tool_button_new (group=item);
  item.set_label[ "Right"];
  item.set_icon_name[ "format-justify-right"];
  store.append[list(list(item), "Right")];
  toolbar.insert[ item, -1];

  //---- Apple

  icon_widget=gtk_image_new_from_file (getenv("NSP")+"/demos3/gtk3/libbase/apple-red.png");
  item = gtk_tool_button_new (icon_widget=icon_widget,label= "_Apple");
  store.append[list(list(item), "Apple")];
  toolbar.insert[ item, -1];
  item.set_use_underline[%t];

  //----- Gicon
  // gicon = g_content_type_get_icon ("video/ogg");
  // image = gtk_image_new_from_gicon (gicon, GTK.ICON_SIZE_LARGE_TOOLBAR);
  // item = gtk_tool_button_new (icon_widget=image,label= "Video");
  // store.append[list(list(item), "Video")];
  // toolbar.insert[ item, -1];

  //---- utilities-terminal
  image = gtk_image_new_from_icon_name ("utilities-terminal", GTK.ICON_SIZE_LARGE_TOOLBAR);
  item = gtk_tool_button_new (icon_widget=image,label= "Terminal");
  store.append[list(list(item), "Terminal")];
  toolbar.insert[ item, -1];

  //---- spinner
  image = gtk_spinner_new ();
  image.start[];
  item = gtk_tool_button_new (icon_widget=image,label= "Spinner");
  store.append[list(list(item), "Spinner")];
  toolbar.insert[ item, -1];

  //---- last row

  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL, spacing=5);
  hbox.set_border_width[5];
  hbox.set_hexpand[%t];
  grid.attach[ hbox, 1, 4, 1, 1];

  //---- The Drag button

  button = gtk_button_new(label="Drag me to the toolbar");
  hbox.pack_start[ button,expand= %f,fill=%f,padding=0];

  label = gtk_label_new (str="Drop index:");
  hbox.pack_start[ label,expand= %f,fill=%f,padding=0];

  label = gtk_label_new ();
  hbox.pack_start[ label,expand= %f,fill=%f,padding=0];

  targets= list(list("application/x-toolbar-item", 0, 0));

  gtk_drag_source_set(button,GDK.BUTTON1_MASK,targets, GDK.ACTION_MOVE);
  gtk_drag_dest_set(button,GTK.DEST_DEFAULT_DROP,targets, GDK.ACTION_MOVE);

  //---- the rtl

  function rtl_toggled (check)
    if check.get_active[] then
      gtk_widget_set_default_direction (GTK.TEXT_DIR_RTL);
    else
      gtk_widget_set_default_direction (GTK.TEXT_DIR_LTR);
    end
  endfunction

  checkbox = gtk_check_button_new(mnemonic="_Right to left");
  if gtk_widget_get_default_direction () == GTK.TEXT_DIR_RTL then
    checkbox.set_active[%t];
  else
    checkbox.set_active[%f];
  end
  checkbox.connect[ "toggled", rtl_toggled];

  hbox.pack_end[checkbox, expand= %f,fill=%f,padding=0];

  // drag callbacks

  function y=toolbar_drag_drop (widget, context, x, y, time, label)
    pause xxx;
    buf=sprintf("%d", widget.get_drop_index[x,y]);
    label.set_label[buf];
    y=%t
  endfunction

  function y=toolbar_drag_motion (toolbar,context, x, y, time,data)
    pause xxx;
    drag_item = gtk_tool_button_new (label="A quite long button");
    gdk_drag_status (context, GDK.ACTION_MOVE, time);
    index = toolbar.get_drop_index[x,y];
    toolbar.set_drop_highlight_item[drag_item,index];
    y=%t
  endfunction

  function toolbar_drag_leave (toolbar,context, x, y, time,data)
    pause xxx;
    toolbar.set_drop_highlight_item[];
  endfunction

  toolbar.connect["drag_motion", toolbar_drag_motion];
  toolbar.connect["drag_leave",toolbar_drag_leave];
  toolbar.connect["drag_drop",toolbar_drag_drop,list(label)];

  window.show_all[];
  // window.connect["delete_event",gtk_main_quit];
  // toolbar.connect["popup_context_menu", popup_context_menu];
endfunction
