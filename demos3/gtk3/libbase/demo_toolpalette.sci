// Tool Palette
//
// A tool palette widget shows groups of toolbar items as a grid of icons
// or a list of names.

////***************************** 
//  ====== Canvas drawing ======  
// ****************************** 

function [ok,item] = canvas_item_new (widget, button, x, y)
  icon_name = button.get_icon_name [];
  icon_theme = gtk_icon_theme_get_for_screen (widget.get_screen []);
  width = gtk_icon_size_lookup (GTK.ICON_SIZE_DIALOG);
  flag = GTK.ICON_LOOKUP_GENERIC_FALLBACK;
  ok = execstr('pixbuf = icon_theme.load_icon[icon_name, width(1),flag]',...
	       errcatch=%t);
  if ok then item = hash(pixbuf=pixbuf,x=x,y=y); 
  else
    lasterror();
    item=[];
  end
endfunction 

function canvas_item_draw (item, cr, preview)

  cx = item.pixbuf.get_width[];
  cy = item.pixbuf.get_height[];
  
  gdk_cairo_set_source_pixbuf (cr, item.pixbuf, item.x - cx * 0.5, item.y - cy * 0.5);

  if (preview) then 
    cairo_paint_with_alpha (cr, 0.6);
  else
    cairo_paint (cr);
  end
endfunction

function y= canvas_draw (widget, cr)

  cairo_set_source_rgb (cr, 1, 1, 1);
  cairo_paint (cr);

  if widget.check_data['canvas_items'] then 
    canvas_items= widget.get_data['canvas_items'];
    for i=1:size(canvas_items,'*')
      canvas_item_draw (canvas_items(i), cr, %f);
    end
  end
  
  if widget.check_data['drop_item'] then 
    drop_item = widget.get_data['drop_item'];
    if type(drop_item,'short')<>'m' then 
      canvas_item_draw (drop_item, cr, %t);
    end
  end
  y= %t;
endfunction

// *************************** 
//  ====== Palette DnD ======  
// *************************** 

function palette_drop_item (drag_item,drop_group, x, y)
  drag_group = drag_item.get_parent [];
  drop_item = drop_group.get_drop_item[x, y];
  drop_position = -1;

  drop_position = drop_group.get_item_position[drop_item];
    
  if ~drag_group.equal[drop_group] then 
    drag_group.child_get[ drag_item, "homogeneous"]
    drag_group.child_get[ drag_item, "expand"];
    drag_group.child_get[ drag_item, "fill"];
    drag_group.child_get[ drag_item, "new-row"];
    drag_group.remove[drag_item];
    drop_group.insert[drag_item, drop_position];
    drop_group.child_set[ drag_item,homogeneous= homogeneous, expand= expand, fill= fill,...
			     new_row= new_row];
  else
    drop_group.set_item_position[drag_item, drop_position];
  end
endfunction 

function palette_drop_group (palette, drag_group, drop_group)
  drop_position = -1;

  if (drop_group)
    drop_position = palette.get_group_position[drop_group];
  end
  palette.set_group_position[drag_group, drop_position];
endfunction 

function palette_drag_data_received (widget, context, x, y, selection, info, time, data)
  drag_palette = gtk_drag_get_source_widget(context);
  while %t 
    if is(drag_palette, %types.GtkToolPalette) then break;end
    drag_palette = drag_palette.get_parent [];
  end
  
  drag_item = drag_palette.get_drag_item[ selection];
  // it should be widget here ? 
  drop_group = drag_palette.get_drop_group[x, y];
  
  if is(drag_item,%types.GtkToolItemGroup) then 
    palette_drop_group (drag_palette, drag_item, drop_group);
  elseif is(drag_item,%types.GtkToolItem) then 
    allocation = drop_group.get_allocation[];
    palette_drop_item (drag_item, drop_group, x - allocation.x, y - allocation.y);
  end
endfunction

// ****************************** 
//  ====== Passive Canvas ======  
// ****************************** 

function passive_canvas_drag_data_received (widget, context, x, y, selection, info, time, data)
//  find the tool button, which is the source of this DnD operation  
  palette = gtk_drag_get_source_widget(context);

  while %t 
    if is(palette, %types.GtkToolPalette) then break;end
    palette = palette.get_parent [];
  end
  
  tool_item = palette.get_drag_item[ selection];
  //  append a new canvas item when a tool button was found  
  [ok,canvas_item] = canvas_item_new (widget, tool_item, x, y);
  if ~ok then return;end
  if ~widget.check_data['canvas_items'] then 
    canvas_items=list();
  else
    canvas_items= widget.get_data['canvas_items'];
  end
  canvas_items($+1) = canvas_item;
  widget.set_data[canvas_items=canvas_items]
  widget.queue_draw[];
endfunction

// ********************************** 
//  ====== Interactive Canvas ======  
// ********************************** 

function y = interactive_canvas_drag_motion (widget, context, x, y, time, data)

// printf("In interactive_canvas drag motion \n");
  have_drop_item = %f 
  if widget.check_data['drop_item'] then 
    drop_item = widget.get_data['drop_item'];
    have_drop_item = type(drop_item,'short')<>'m';
  end
  if have_drop_item then 
    //  already have a drop indicator - just update position  
    drop_item.x = x;
    drop_item.y = y;
    widget.set_data[drop_item=drop_item];
    widget.queue_draw[];
    gdk_drag_status (context, GDK.ACTION_COPY, time);
  else
    //  request DnD data for creating a drop indicator  
    ok = execstr(' target = gtk_drag_dest_find_target (widget, context,list());',errcatch=%t);
    if ~ok then lasterror(); y=%f; return ;end 
    widget.set_data[drag_data_requested_for_drop=%f];
    gtk_drag_get_data(widget,context, target, time= time);
  end
  y=%t
endfunction
  
function interactive_canvas_drag_data_received (widget, context, x, y, ...
						selection, info, time, data)
  //  find the tool button which is the source of this DnD operation  
  
  palette = gtk_drag_get_source_widget(context);
  
  while %t 
    if is(palette, %types.GtkToolPalette) then break;end
    palette = palette.get_parent [];
  end

  tool_item = palette.get_drag_item[selection];
  
  //  create a canvas item when a tool button was found  
  [ok,item] = canvas_item_new (widget, tool_item, x, y);
  if ~ok then return;end
  //  Either create a new item or just create a preview item, 
  //  depending on why the drag data was requested.  
  if widget.check_data["drag_data_requested_for_drop"] then 
    drag_data_requested_for_drop = widget.get_data["drag_data_requested_for_drop"];
  else
    drag_data_requested_for_drop = %f;
  end
  
  if drag_data_requested_for_drop then 
    
    if ~widget.check_data['canvas_items'] then 
      canvas_items=list();
    else
      canvas_items= widget.get_data['canvas_items'];
    end
    canvas_items($+1) = item;
    widget.set_data[drop_item=[]];
    widget.set_data[canvas_items=canvas_items]
    gtk_drag_finish (context, %t, %f, time);
  else
    widget.set_data[drop_item = item];
    gdk_drag_status (context, GDK.ACTION_COPY, time);
  end
  widget.queue_draw[];
endfunction 

function y=interactive_canvas_drag_drop (widget, context, x, y, time,data)

  ok = execstr(' target = gtk_drag_dest_find_target (widget, context,list());',errcatch=%t);
  if ~ok then lasterror(); y=%f;return;end
  widget.set_data[drag_data_requested_for_drop = %t];
  gtk_drag_get_data(widget,context, target, time= time);
  y=%f
endfunction
  
function interactive_canvas_drag_leave (data)
  widget = data;
  if widget.check_data['drop_item'] then 
    widget.set_data[drop_item= []];
    widget.queue_draw[];
  end
endfunction

function on_combo_orientation_changed (combo_box, user_data)
  palette = user_data;
  model = combo_box.get_model [];
  val = 0;
  sw = palette.get_parent [];
  iter = combo_box.get_active_iter[];
  // if (~combo_box.get_active_iter[iter]) then return; end 
    
  val = model.get_value[iter, 1];
  palette.set_orientation[val];

  if (val == GTK.ORIENTATION_HORIZONTAL)
    sw.set_policy[GTK.POLICY_AUTOMATIC, GTK.POLICY_NEVER];
  else
    sw.set_policy[GTK.POLICY_NEVER, GTK.POLICY_AUTOMATIC];
  end
endfunction 
  
function on_combo_style_changed (combo_box,  user_data)
  palette = user_data;
  model = combo_box.get_model [];
  val = 0;
  iter = combo_box.get_active_iter[];
  // if ~iter then return;end
  
  val = model.get_value[iter,1];
  
  if (val == -1) then 
    gtk_tool_palette_unset_style (palette);
  else
    palette.set_style[val];
  end
endfunction 
  

function window = demo_toolpalette (do_widget)

  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  if nargin >= 1 then 
    window.set_screen[ do_widget.get_screen []];
  end
  window.set_title[ "Tool Palette"];
  window.set_default_size[200, 600];

  // window.connect[ "destroy", gtk_widget_destroyed, &window);
  window.set_border_width[8];

  //  Add widgets to control the ToolPalette appearance:  
  box = gtk_box_new (GTK.ORIENTATION_VERTICAL, spacing=6);
  window.add[box];

  //  Orientation combo box:  
  orientation_model = gtk_list_store_new (list("",1),%f);
  iter = orientation_model.append[];
  orientation_model.set[iter, 0, "Horizontal"];
  orientation_model.set[iter,1, GTK.ORIENTATION_HORIZONTAL];
  iter = orientation_model.append[];
  orientation_model.set[iter, 0, "Vertical"];
  orientation_model.set[iter, 1, GTK.ORIENTATION_VERTICAL];
  combo_orientation = gtk_combo_box_new(model =orientation_model);
  
  cell_renderer = gtk_cell_renderer_text_new ();
  combo_orientation.pack_start[cell_renderer, expand=%t];
  combo_orientation.set_attributes[cell_renderer, hash(text= 0)];
  combo_orientation.set_active_iter[ iter];
  box.pack_start[combo_orientation,expand=%f,fill=%f,padding=0]

  //  Style combo box:  
  style_model = gtk_list_store_new (list("",1),%f);
  iter = style_model.append[];
  style_model.set[iter, 0, "Text"];
  style_model.set[iter, 1, GTK.TOOLBAR_TEXT];
                          			  
  iter = style_model.append[];
  style_model.set[iter, 0, "Both"];
  style_model.set[iter, 1, GTK.TOOLBAR_BOTH];

  iter = style_model.append[];
  style_model.set[iter, 0, "Both: Horizontal"];
  style_model.set[iter,1, GTK.TOOLBAR_BOTH_HORIZ];

  iter = style_model.append[];
  style_model.set[iter, 0, "Icons"];
  style_model.set[iter, 1, GTK.TOOLBAR_ICONS];

  iter = style_model.append[];
  style_model.set[iter, 0, "Default"]; 
  style_model.set[iter, 1, -GTK.TOOLBAR_ICONS];

  combo_style = gtk_combo_box_new(model=style_model);
  cell_renderer = gtk_cell_renderer_text_new ();
  combo_style.pack_start[ cell_renderer, expand=%t];
  combo_style.set_attributes[cell_renderer, hash(text= 0)];
      
  combo_style.set_active_iter[ iter];
  box.pack_start[combo_style, expand=%f,fill=%f,padding=0]

  //  Add hbox  
  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL, spacing= 5);
  box.pack_start[hbox, expand=%t,fill=%t,padding=0];

  //  Add and fill the ToolPalette:  
  palette = gtk_tool_palette_new ();
  //palette.set_icon_size[6];

  load_icon_items (palette);
  load_toggle_items (palette);
  load_special_items (palette);

  palette_scroller = gtk_scrolled_window_new ();
  palette_scroller.set_policy[ GTK.POLICY_NEVER, GTK.POLICY_AUTOMATIC];
  palette_scroller.set_border_width[6];
  palette_scroller.set_hexpand[%t];

  palette_scroller.add[palette];
  hbox.add[palette_scroller];

  box.show_all[];

  //  Connect signals:  
  combo_orientation.connect[ "changed", on_combo_orientation_changed, palette];
  combo_style.connect[ "changed", on_combo_style_changed, palette];

  //  Keep the widgets in sync:  
  on_combo_orientation_changed (combo_orientation, palette);

  //  ===== notebook =====  
      
  notebook = gtk_notebook_new ();
  notebook.set_border_width[6];
  hbox.pack_end[notebook, expand=%f,fill=%f,padding=0];

  //  ===== DnD for tool items =====  

  palette.connect[ "drag-data-received", palette_drag_data_received];

  palette.add_drag_dest[palette, GTK.DEST_DEFAULT_ALL, ... 
		    ior([ GTK.TOOL_PALETTE_DRAG_ITEMS, GTK.TOOL_PALETTE_DRAG_GROUPS]),...
		    GDK.ACTION_MOVE];

  //  ===== passive DnD dest =====  

  contents = gtk_drawing_area_new ();
  contents.set_app_paintable[%t];
  contents.connect["draw", canvas_draw];
  contents.connect["drag-data-received", passive_canvas_drag_data_received];
  
  palette.add_drag_dest[ contents, GTK.DEST_DEFAULT_ALL, GTK.TOOL_PALETTE_DRAG_ITEMS, GDK.ACTION_COPY];

  contents_scroller = gtk_scrolled_window_new ();
  contents_scroller.set_policy[ GTK.POLICY_AUTOMATIC, GTK.POLICY_ALWAYS];
  contents_scroller.add[contents];
  contents_scroller.set_border_width[6];

  notebook.append_page[ contents_scroller, gtk_label_new(str="Passive DnD Mode")];

  //  ===== interactive DnD dest =====  

  contents = gtk_drawing_area_new ();
  contents.set_app_paintable[%t];
  
  contents.connect["draw", canvas_draw]
  contents.connect["drag-motion", interactive_canvas_drag_motion ]
  contents.connect["drag-data-received", interactive_canvas_drag_data_received ]
  contents.connect["drag-leave", interactive_canvas_drag_leave, contents]
  contents.connect["drag-drop", interactive_canvas_drag_drop]
    
  palette.add_drag_dest[ contents, GTK.DEST_DEFAULT_HIGHLIGHT, GTK.TOOL_PALETTE_DRAG_ITEMS,GDK.ACTION_COPY];

  contents_scroller = gtk_scrolled_window_new ();
  contents_scroller.set_policy[ GTK.POLICY_AUTOMATIC, GTK.POLICY_ALWAYS];
  contents_scroller.add[contents];
  contents_scroller.set_border_width[6];

  notebook.append_page[ contents_scroller, gtk_label_new(str="Interactive DnD Mode")];
  window.show_all[];
endfunction 

function load_icon_items (palette)
  
  icon_theme = gtk_icon_theme_get_for_screen (palette.get_screen []);
  contexts = icon_theme.list_contexts[];
  for i=1:length(contexts)
    
    context = contexts(i);
    group = gtk_tool_item_group_new (context);
    palette.add[group];
    
    if context ==  "Animations" then continue;end 
    // printf ("Got context ''%s''\n", context);
    icon_names = icon_theme.list_icons[context];
    // list to string matrix 
    icon_names.compact[];
    icon_names = sort(icon_names(1));
    count = 0;
    for ll=1:size(icon_names,'*')
      id = icon_names(ll)
      if id == "emblem-desktop" then continue;end 
      if strstr(id, "-symbolic")<>0 then continue;end 
      item = gtk_tool_button_new ()
      item.set_icon_name[id];
      item.set_tooltip_text[id];
      group.insert[item,-1];
      count = count+1;
      if count >= 10 then break;end
    end
  end
endfunction 

function load_toggle_items (palette)
  // return;
  group = gtk_tool_item_group_new ("Radio Item");
  palette.add[group];
  for i = 1:10 
    if i== 1 then
      item = gtk_radio_tool_button_new ()
      first=item;
    else
      item = gtk_radio_tool_button_new (group=first);//toggle_group(1));
    end
    item.set_label[sprintf("#%d", i)];
    group.insert[ item, -1];
    toggle_group = item.get_group [];
  end
endfunction

function load_special_items (palette)

  function item = create_entry_item (text)
  entry = gtk_entry_new ();
  entry.set_text[text];
  entry.set_width_chars[5];
  item = gtk_tool_item_new ();
  item.add[entry];
  endfunction 
  
  group = gtk_tool_item_group_new ("ti");
  label_button = gtk_button_new(label="Advanced Features");
  label_button.show[];
  group.set_label_widget [label_button];
  palette.add[group];
  
  item = create_entry_item ("homogeneous=%f");
  group.insert[item, -1];
  group.child_set[ item, homogeneous= %f];
  
  item = create_entry_item ("homogeneous=%f, expand=%t");
  group.insert[item, -1];
  group.child_set[ item, homogeneous= %f, expand= %t];

  item = create_entry_item ("homogeneous=%f, expand=%t, fill=%f");
  group.insert[item, -1];
  group.child_set[ item, homogeneous= %f, expand= %t, fill= %f];

  item = create_entry_item ("homogeneous=%f, expand=%t, new-row=%t");
  group.insert[item, -1];
  group.child_set[ item, homogeneous= %f, expand= %t, new_row= %t];

  item = gtk_tool_button_new ()
  item.set_icon_name [ "go-up"];
  item.set_tooltip_text [ "Show on vertical palettes only"];
  group.insert[item, -1];
  item.set_visible_horizontal[%f];

  item = gtk_tool_button_new ()
  item.set_icon_name [ "go-next"];
  item.set_tooltip_text [ "Show on horizontal palettes only"];
  group.insert[item, -1];
  item.set_visible_vertical[%f];

  item = gtk_tool_button_new ()
  item.set_icon_name [ "edit-delete"];
  item.set_tooltip_text [ "Do not show at all"];
  group.insert[item, -1];
  item.set_no_show_all[%t];

  item = gtk_tool_button_new ()
  item.set_icon_name [ "view-fullscreen"];
  item.set_tooltip_text [ "Expanded this item"];
  group.insert[item, -1];
  group.child_set[ item, homogeneous= %f,expand= %t];

  item = gtk_tool_button_new ()
  item.set_icon_name [ "help-browser"];
  item.set_tooltip_text [ "A regular item"];
  group.insert[item, -1];
endfunction

