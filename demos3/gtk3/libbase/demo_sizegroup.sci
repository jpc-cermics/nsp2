// Size Groups
//
// GtkSizeGroup provides a mechanism for grouping a number of
// widgets together so they all request the same amount of space.
// This is typically useful when you want a column of widgets to
// have the same size, but you can't use a GtkTable widget.
//
// Note that size groups only affect the amount of space requested,
// not the size that the widgets finally receive. If you want the
// widgets in a GtkSizeGroup to actually be the same size, you need
// to pack them in such a way that they get the size they request
// and not more. For example, if you are packing your widgets
// into a table, you would not include the GTK.FILL flag.

function demo_sizegroup ()
  
// Convenience function to create an option menu holding a number of strings
  function [option_menu]=create_option_menu (strings)
  // gtkoptionmenu_new deprecated and replaced by gtk_combo_box_text_new
    if %t then 
      // shortest way 
      option_menu = gtk_combo_box_new(text=strings); // with_entry=%t);
      option_menu.set_active[1];
    else
      // another way 
      option_menu = gtk_combo_box_text_new ();
      for i = 1:size(strings,'*')
	option_menu.append_text[strings(i)];
      end
      option_menu.set_active[1];
    end
  endfunction

  function add_row (table, row,size_group,size_group_label,label_text,options)
    label = gtk_label_new(mnemonic=label_text);
    table.attach[  label,0, row, 1,1];//  xoptions=ior(GTK.EXPAND,GTK.FILL)];
    option_menu = create_option_menu (options);
    label.set_mnemonic_widget[ option_menu]
    size_group.add_widget[ option_menu];
    size_group_label.add_widget[label];
    table.attach[  option_menu,1, row, 1,1];
  endfunction
  
  
  color_options = [ "Red", "Green", "Blue"];
  dash_options = ["Solid", "Dashed", "Dotted"];
end_options = ["Square", "Round", "Arrow"];

// XXXX a rechanger GTK.RESPONSE_NONE,
//// ,buttons= GTK.STOCK_CLOSE);
window = gtk_dialog_new(title = "GtkSizeGroup")
window.set_resizable[%f]

// XXX window.connect[  "response",gtk_widget_destroy, NULL]
// window.connect[  "destroy",gtk_widget_destroyed, &window]

vbox = gtk_box_new("vertical",spacing=5);
window_vbox = window.get_content_area[];
window_vbox.pack_start[ vbox,expand=%t,fill=%t,padding=0];
vbox.set_border_width[  5]

size_group = gtk_size_group_new (GTK.SIZE_GROUP_HORIZONTAL);
size_group_labels = gtk_size_group_new (GTK.SIZE_GROUP_HORIZONTAL);

// Create one frame holding color options

frame = gtk_frame_new(label="Color Options");
vbox.pack_start[ frame,expand=%t,fill=%t,padding=0]

// table = gtktable_new(rows=2,columns=2,homogeneous=%f);
table = gtk_grid_new();// rows=2,columns=2,homogeneous=%f);
table.set_row_homogeneous[%f];
table.set_column_homogeneous[%f];
table.set_column_spacing[5];
table.set_border_width[  5]
frame.add[  table]

add_row (table, 0, size_group, size_group_labels, "_Foreground", color_options);
add_row (table, 1, size_group, size_group_labels, "_Background", color_options);

// And another frame holding line style options

frame = gtk_frame_new(label="Line Options");
vbox.pack_start[ frame,expand=%f,fill=%f,padding=0]

table = gtk_grid_new();
table.set_row_homogeneous[%f];
table.set_column_homogeneous[%f];
table.set_column_spacing[5];
table.set_border_width[  5]
frame.add[  table]

add_row (table, 0, size_group, size_group_labels, "_Dashing", dash_options);
add_row (table, 1, size_group, size_group_labels, "_Line ends", end_options);

//  And a check button to turn grouping on and off */
check_button = gtk_check_button_new(mnemonic="_Enable grouping");
vbox.pack_start[ check_button,expand=%f,fill=%f,padding=0]

check_button.set_active[  %t]


function toggle_grouping (check_button, args)
// GTK.SIZE_GROUP_NONE is not generally useful, but is useful
// here to show the effect of GTK.SIZE_GROUP_HORIZONTAL by
// contrast.
  if check_button.get_active[]
    new_mode = GTK.SIZE_GROUP_HORIZONTAL;
  else
    new_mode = GTK.SIZE_GROUP_NONE;
  end
  args(1).set_mode[new_mode];
endfunction

check_button.connect[  "toggled",toggle_grouping,list( size_group)]
window.show_all[];
endfunction
