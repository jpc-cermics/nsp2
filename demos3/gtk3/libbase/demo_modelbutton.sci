//  Model Button
//
// GtkModelButton is a button widget that is designed to be used with
// a GAction as model. The button will adjust its appearance according
// to the kind of action it is connected to.
//
// It is also possible to use GtkModelButton without a GAction. In this
// case, you should set the "role" attribute yourself, and connect to the
// "clicked" signal as you would for any other button.
//
// A common use of GtkModelButton is to implement menu-like content
// in popovers.
  

function window=demo_modelbutton (do_widget)

  function tool_clicked (button,user_data)
  //use get_property for g_object_get (button, "active", &active, NULL);
    active = button.get_property["active"];  
    button.set_property[ "active", ~active];
  endfunction 
  
  fname = getenv('NSP')+"/demos3/gtk3/libbase/demo_modelbutton.ui"
  builder = gtk_builder_new_from_file(fname);

  if %t then 
    // 
    // gtk_builder_add_callback_symbol (builder, "tool_clicked", tool_clicked);
    // gtk_builder_connect_signals (builder, NULL);
    H=hash(tool_clicked=tool_clicked);
    // builder.connect_signal(H [,extra_args]);
    // extra_args are extra_arguments passed to handler
    builder.connect_signals[H]; 
  else
    // connect signals explicitely 
    popover= builder.get_object[ "thing_c"]
    childs = popover.get_children[];
    box = childs(1);
    childs = box.get_children[];
    for i =1:size(childs) 
      mb = childs(i);
      mb.connect["clicked",tool_clicked];
    end
  end
    
  // main window 
  window = builder.get_object[ "window1"];
  if nargin >= 1 then window.set_screen[ do_widget.get_screen []];end
  
  // an action group 
  win_entries = { "color", "", "s", "''red''"
		  "chocolate", "", "", "true"
		  "vanilla", "", "", "false"
		  "sprinkles", "", "", ""};
  // we build actions from win_entries 
  // this step could be remove by improving the 
  // add_action_entries method 
  
  actions=m2s([]);
  for i=1:size(win_entries,1)
    actions(i,1:3)=[win_entries{i,1},win_entries{i,3},win_entries{i,4}];
  end
  
  action_group = g_simple_action_group_new ();
  action_group.add_action_entries [actions];
  window.insert_action_group["win", action_group];
  window.show_all[];
endfunction

