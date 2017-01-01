// Popovers
//
// A bubble-like window containing contextual information or options.
// GtkPopovers can be attached to any widget, and will be displayed
// within the same window, but on top of all its content.

function toggle_changed_cb (button, popover)
  popover.set_visible[button.get_active []];
endfunction 


function popover= create_popover (parent, child, pos)
  popover = gtk_popover_new (parent);
  popover.set_position[pos];
  popover.add[child];
  popover.set_border_width[6];
  child.show[];
endfunction 

function popover=create_complex_popover (parent, pos)

  builder = gtk_builder_new();
  builder.add_from_file[(getenv("NSP")+"/demos3/gtk3/libbase/demo_popover.ui")];
  // builder.connect_signals[H,args];
  
  window = builder.get_object["window"];
  content = window.get_child [];
  parent1 = content.get_parent[];
  parent1.remove[content];
  window.destroy[];
  
  popover = create_popover (parent, content, GTK.POS_BOTTOM);
  popover.set_size_request[ 200, -1];
  popover.set_vexpand[%t];

  popover.set_margin_start[10];
  popover.set_margin_end[10];
  popover.set_margin_bottom[10];
endfunction 

function entry_size_allocate_cb (entry, allocation, user_data)
  popover = user_data;
  if popover.is_visible[] then 
    popover_pos = entry.get_data["popover-icon-pos"];
    rect= entry.get_icon_area[ popover_pos];
    popover.set_pointing_to[rect];
  end
endfunction 

function entry_icon_press_cb (entry, icon_pos,event, user_data)
  popover = user_data;
  rect= entry.get_icon_area[ icon_pos]
  popover.set_pointing_to[rect];
  popover.show[];
  entry.set_data[ "popover-icon-pos",icon_pos];
endfunction 

function day_selected_cb (calendar, user_data)

  event = gtk_get_current_event ();
  if (event.type <> GDK.BUTTON_PRESS) then return;end
  window = event.get_window[];
  xy=event.get_coords[]
  xyn= window.coords_to_parent[xy(1),xy(2)];
  
  allocation=calendar.get_allocation[];
  rect= [xyn(1) - allocation.x, xyn(2) - allocation.y,1,1];
  popover = create_popover (calendar, gtk_entry_new (), GTK.POS_BOTTOM);
  popover.set_pointing_to[rect];
  popover.show[];
endfunction 

function window = demo_popover (do_widget)
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  box = gtk_box_new (GTK.ORIENTATION_VERTICAL, spacing= 24);
  box.set_border_width[24];
  window.add[box];
  // window.connect[ "destroy",gtk_widget_destroyed, &window];

  widget = gtk_toggle_button_new(label="Button");
  popover = create_popover (widget,...
			    gtk_label_new(str="This popover does not grab input"),...
			    GTK.POS_TOP);
  popover.set_modal[%f];
  widget.connect[ "toggled", toggle_changed_cb, popover];
  box.add[widget];
  widget = gtk_entry_new ();
  popover = create_complex_popover (widget, GTK.POS_TOP);
  widget.set_icon_from_icon_name[GTK.ENTRY_ICON_PRIMARY, "edit-find"];
  widget.set_icon_from_icon_name[GTK.ENTRY_ICON_SECONDARY, "edit-clear"];
  widget.connect[ "icon-press", entry_icon_press_cb, popover];
  widget.connect[ "size-allocate", entry_size_allocate_cb, popover];
  box.add[widget];

  widget = gtk_calendar_new ();
  widget.connect[ "day-selected", day_selected_cb];
  box.add[widget];
  window.show_all[];
endfunction

