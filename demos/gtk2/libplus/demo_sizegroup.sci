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

// Convenience function to create an option menu holding a number of strings

function [option_menu]=create_option_menu (strings)
  menu = gtkmenu_new ();
  for str=strings 
    menu_item = gtkmenuitem_new(label=str);
    menu_item.show[];
    menu.append[  menu_item]
  end
  option_menu = gtkoptionmenu_new ();
  option_menu.set_menu[menu];
endfunction

function add_row (table, row,size_group,label_text,options)
  label = gtklabel_new(mnemonic=label_text);
  label.set_alignment[  0, 1]
  table.attach[  label,0, 1, row, row + 1,  xoptions=ior(GTK.EXPAND,GTK.FILL),yoptions= 0,xpadding=0,ypadding= 0]
  option_menu = create_option_menu (options);
  label.set_mnemonic_widget[ option_menu]
  size_group.add_widget[ option_menu];
  table.attach[  option_menu,1, 2,row, row + 1, xoptions=0,yoptions=0,xpadding=0,ypadding=0]
endfunction

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

function demo_sizegroup ()
  color_options = [ "Red", "Green", "Blue"];
  dash_options = ["Solid", "Dashed", "Dotted"];
  end_options = ["Square", "Round", "Arrow"];
    
  // XXXX a rechanger GTK.RESPONSE_NONE,
  //// ,buttons= GTK.STOCK_CLOSE);
  window = gtkdialog_new(title = "GtkSizeGroup")
  window.set_resizable[%f]
  
  // XXX window.connect[  "response",gtk_widget_destroy, NULL]
  // window.connect[  "destroy",gtk_widget_destroyed, &window]

  vbox = gtkvbox_new(homogeneous=%f,spacing=5);
  window.vbox.pack_start[ vbox,expand=%t,fill=%t,padding=0];
  vbox.set_border_width[  5]

  size_group = gtksizegroup_new (GTK.SIZE_GROUP_HORIZONTAL);
      
  // Create one frame holding color options

  frame = gtkframe_new(label="Color Options");
  vbox.pack_start[ frame,expand=%t,fill=%t,padding=0]

  table = gtktable_new(rows=2,columns=2,homogeneous=%f);
  table.set_border_width[  5]
  table.set_row_spacings[  5]
  table.set_col_spacings[  10]
  frame.add[  table]

  add_row (table, 0, size_group, "_Foreground", color_options);
  add_row (table, 1, size_group, "_Background", color_options);

  // And another frame holding line style options
  
  frame = gtkframe_new(label="Line Options");
  vbox.pack_start[ frame,expand=%f,fill=%f,padding=0]
  
  table = gtktable_new(rows=2,columns=2,homogeneous=%f);
  table.set_border_width[  5]
  table.set_row_spacings[  5]
  table.set_col_spacings[  10]
  frame.add[  table]

  add_row (table, 0, size_group, "_Dashing", dash_options);
  add_row (table, 1, size_group, "_Line ends", end_options);

  //  And a check button to turn grouping on and off */
  check_button = gtkcheckbutton_new(mnemonic="_Enable grouping");
  vbox.pack_start[ check_button,expand=%f,fill=%f,padding=0]
      
  check_button.set_active[  %t]
  check_button.connect[  "toggled",toggle_grouping,list( size_group)]
  window.show_all[];
endfunction

